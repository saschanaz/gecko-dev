/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99: */

// Copyright 2012 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "irregexp/RegExpParser.h"

#include "mozilla/ArrayUtils.h"
#include "mozilla/Move.h"

#include "frontend/TokenStream.h"
#include "gc/GC.h"
#include "irregexp/RegExpCharacters.h"
#include "irregexp/RegExpEngine.h"
#include "util/StringBuffer.h"
#include "vm/ErrorReporting.h"

using namespace js;
using namespace js::irregexp;

using mozilla::Move;
using mozilla::PointerRangeSize;

using irregexp::Zone;

inline bool
IsInRange(int value, int lower_limit, int higher_limit)
{
    MOZ_ASSERT(lower_limit <= higher_limit);
    return static_cast<unsigned int>(value - lower_limit) <=
           static_cast<unsigned int>(higher_limit - lower_limit);
}

inline bool
IsDecimalDigit(uc32 c)
{
    // ECMA-262, 3rd, 7.8.3 (p 16)
    return IsInRange(c, '0', '9');
}

// Returns the value (0 .. 15) of a hexadecimal character c.
// If c is not a legal hexadecimal character, returns a value < 0.
inline int
HexValue(uint32_t c)
{
    c -= '0';
    if (static_cast<unsigned>(c) <= 9) return c;
    c = (c | 0x20) - ('a' - '0');  // detect 0x11..0x16 and 0x31..0x36.
    if (static_cast<unsigned>(c) <= 5) return c + 10;
    return -1;
}


// ----------------------------------------------------------------------------
// RegExpParser

template <typename CharT>
RegExpParser<CharT>::RegExpParser(frontend::TokenStreamAnyChars& ts, Zone* zone,
                                  const CharT* chars, const CharT* end, bool multiline,
                                  bool unicode, bool ignore_case)
  : ts(ts),
    zone_(zone),
    captures_(NULL),
    start_(chars),
    next_pos_(start_),
    end_(end),
    current_(kEndMarker),
    ignore_case_(ignore_case),
    multiline_(multiline),
    unicode_(unicode),
    captures_started_(0),
    capture_count_(0),
    has_more_(true),
    simple_(false),
    contains_anchor_(false),
    is_scanned_for_captures_(false)
{
    Advance();
}

template <typename CharT>
template <bool update_position>
inline uc32 RegExpParser<CharT>::ReadNext() {
    const CharT* position = next_pos_;
    uc32 c0 = *position;
    position++;
    // Read the whole surrogate pair in case of unicode flag, if possible.
    if (unicode() && position < end_ &&
        unicode::IsLeadSurrogate(static_cast<uc16>(c0))) {
        uc16 c1 = *position;
        if (unicode::IsTrailSurrogate(c1)) {
            c0 = unicode::UTF16Decode(static_cast<uc16>(c0), c1);
            position++;
        }
    }
    if (update_position) next_pos_ = position;
    return c0;
}

template <typename CharT>
uc32 RegExpParser<CharT>::Next() {
    if (has_next()) {
        return ReadNext<false>();
    } else {
        return kEndMarker;
    }
}

template <typename CharT>
void RegExpParser<CharT>::Advance() {
    if (has_next()) {
        current_ = ReadNext<true>();
    } else {
        current_ = kEndMarker;
        // Advance so that position() points to 1-after-the-last-character. This is
        // important so that Reset() to this position works correctly.
        next_pos_ = end_ + 1;
        has_more_ = false;
    }
}

template <typename CharT>
void RegExpParser<CharT>::Reset(const CharT* pos) {
    next_pos_ = pos;
    has_more_ = (pos < end_);
    Advance();
}

template <typename CharT>
void RegExpParser<CharT>::Advance(int dist) {
    next_pos_ += dist - 1;
    Advance();
}

template <typename CharT>
bool RegExpParser<CharT>::simple() { return simple_; }

template <typename CharT>
bool RegExpParser<CharT>::IsSyntaxCharacterOrSlash(uc32 c) {
  switch (c) {
    case '^':
    case '$':
    case '\\':
    case '.':
    case '*':
    case '+':
    case '?':
    case '(':
    case ')':
    case '[':
    case ']':
    case '{':
    case '}':
    case '|':
    case '/':
      return true;
    default:
      break;
  }
  return false;
}

template <typename CharT>
void
RegExpParser<CharT>::SyntaxError(unsigned errorNumber, ...)
{
    ErrorMetadata err;

    ts.fillExcludingContext(&err, ts.currentToken().pos.begin);

    // For most error reporting, the line of context derives from the token
    // stream.  So when location information doesn't come from the token
    // stream, we can't give a line of context.  But here the "line of context"
    // can be (and is) derived from the pattern text, so we can provide it no
    // matter if the location is derived from the caller.
    size_t offset = PointerRangeSize(start_, next_pos_ - 1);
    size_t end = PointerRangeSize(start_, end_);

    const CharT* windowStart = (offset > ErrorMetadata::lineOfContextRadius)
                               ? start_ + (offset - ErrorMetadata::lineOfContextRadius)
                               : start_;

    const CharT* windowEnd = (end - offset > ErrorMetadata::lineOfContextRadius)
                             ? start_ + offset + ErrorMetadata::lineOfContextRadius
                             : end_;

    size_t windowLength = PointerRangeSize(windowStart, windowEnd);
    MOZ_ASSERT(windowLength <= ErrorMetadata::lineOfContextRadius * 2);

    // Create the windowed string, not including the potential line
    // terminator.
    StringBuffer windowBuf(ts.context());
    if (!windowBuf.append(windowStart, windowEnd))
        return;

    // The line of context must be null-terminated, and StringBuffer doesn't
    // make that happen unless we force it to.
    if (!windowBuf.append('\0'))
        return;

    err.lineOfContext.reset(windowBuf.stealChars());
    if (!err.lineOfContext)
        return;

    err.lineLength = windowLength;
    err.tokenOffset = offset - (windowStart - start_);

    va_list args;
    va_start(args, errorNumber);

    ReportCompileError(ts.context(), Move(err), nullptr, JSREPORT_ERROR, errorNumber, args);

    va_end(args);
}

template <typename CharT>
RegExpTree*
RegExpParser<CharT>::ReportError(unsigned errorNumber, const char* param /* = nullptr */)
{
    gc::AutoSuppressGC suppressGC(ts.context());
    SyntaxError(errorNumber, param);
    return nullptr;
}

// Pattern ::
//   Disjunction
template <typename CharT>
RegExpTree* RegExpParser<CharT>::ParsePattern() {
    RegExpTree* result = ParseDisjunction();
    MOZ_ASSERT_IF(result, !has_more());
    return result;
}

// Disjunction ::
//   Alternative
//   Alternative | Disjunction
// Alternative ::
//   [empty]
//   Term Alternative
// Term ::
//   Assertion
//   Atom
//   Atom Quantifier
template <typename CharT>
RegExpTree* RegExpParser<CharT>::ParseDisjunction() {
    // Used to store current state while parsing subexpressions.
    RegExpParserState initial_state(NULL, INITIAL, RegExpLookaround::LOOKAHEAD, 0,
                                    ignore_case(), unicode(), zone());
    RegExpParserState* state = &initial_state;
    // Cache the builder in a local variable for quick access.
    RegExpBuilder* builder = initial_state.builder();
    while (true) {
        switch (current()) {
          case kEndMarker:
            if (state->IsSubexpression()) {
                // Inside a parenthesized group when hitting end of input.
                return ReportError(JSMSG_MISSING_PAREN);
            }
            DCHECK_EQ(INITIAL, state->group_type());
            // Parsing completed successfully.
            return builder->ToRegExp();
          case ')': {
            if (!state->IsSubexpression()) {
                return ReportError(JSMSG_UNMATCHED_RIGHT_PAREN);
            }
            DCHECK_NE(INITIAL, state->group_type());

            Advance();
            // End disjunction parsing and convert builder content to new single
            // regexp atom.
            RegExpTree* body = builder->ToRegExp();

            int end_capture_index = captures_started();

            int capture_index = state->capture_index();
            SubexpressionType group_type = state->group_type();

            // Build result of subexpression.
            if (group_type == CAPTURE) {
                RegExpCapture* capture = GetCapture(capture_index);
                capture->set_body(body);
                body = capture;
            } else if (group_type == GROUPING) {
                body = zone_->newInfallible<RegExpGroup>(body);
            } else {
                DCHECK(group_type == POSITIVE_LOOKAROUND ||
                       group_type == NEGATIVE_LOOKAROUND);
                bool is_positive = (group_type == POSITIVE_LOOKAROUND);
                body = zone_->newInfallible<RegExpLookaround>(
                    body, is_positive, end_capture_index - capture_index,
                    capture_index, state->lookaround_type());
            }

            // Restore previous state.
            state = state->previous_state();
            builder = state->builder();

            builder->AddAtom(body);
            // For compatibility with JSC and ES3, we allow quantifiers after
            // lookaheads, and break in all cases.
            break;
          }
          case '|': {
            Advance();
            builder->NewAlternative();
            continue;
          }
          case '*':
          case '+':
          case '?':
            return ReportError(JSMSG_NOTHING_TO_REPEAT);
          case '^': {
            Advance();
            if (multiline()) {
                builder->AddAssertion(
                    zone_->newInfallible<RegExpAssertion>(RegExpAssertion::START_OF_LINE));
            } else {
                builder->AddAssertion(
                    zone_->newInfallible<RegExpAssertion>(RegExpAssertion::START_OF_INPUT));
                set_contains_anchor();
            }
            continue;
          }
          case '$': {
            Advance();
            RegExpAssertion::AssertionType assertion_type =
                multiline() ? RegExpAssertion::END_OF_LINE
                            : RegExpAssertion::END_OF_INPUT;
            builder->AddAssertion(zone_->newInfallible<RegExpAssertion>(assertion_type));
            continue;
          }
          case '.': {
            Advance();
            CharacterRangeVector* ranges =
                zone_->newInfallible<CharacterRangeVector>(*zone());

            // Everything except \x0a, \x0d, \u2028 and \u2029
            CharacterRange::AddClassEscape('.', ranges, false, zone());

            RegExpCharacterClass* cc = zone_->newInfallible<RegExpCharacterClass>(ranges);
            builder->AddCharacterClass(cc);
            break;
          }
          case '(': {
            SubexpressionType subexpr_type = CAPTURE;
            RegExpLookaround::Type lookaround_type = state->lookaround_type();
            Advance();
            if (current() == '?') {
                switch (Next()) {
                  case ':':
                    subexpr_type = GROUPING;
                    Advance(2);
                    break;
                  case '=':
                    lookaround_type = RegExpLookaround::LOOKAHEAD;
                    subexpr_type = POSITIVE_LOOKAROUND;
                    Advance(2);
                    break;
                  case '!':
                    lookaround_type = RegExpLookaround::LOOKAHEAD;
                    subexpr_type = NEGATIVE_LOOKAROUND;
                    Advance(2);
                    break;
                  default:
                    return ReportError(JSMSG_INVALID_GROUP);
                }
            }

            if (subexpr_type == CAPTURE) {
                if (captures_started_ >= kMaxCaptures) {
                    return ReportError(JSMSG_TOO_MANY_PARENS);
                }
                captures_started_++;
            }

            // Store current state and begin new disjunction parsing.
            state = zone_->newInfallible<RegExpParserState>(
                state, subexpr_type, lookaround_type, captures_started_,
                ignore_case(), unicode(), zone());
            builder = state->builder();
            continue;
          }
          case '[': {
            RegExpTree* cc = ParseCharacterClass();
            if (!cc)
                return nullptr;
            builder->AddCharacterClass(cc->AsCharacterClass());
            break;
          }
            // Atom ::
            //   \ AtomEscape
          case '\\':
            switch (Next()) {
              case kEndMarker:
                return ReportError(JSMSG_ESCAPE_AT_END_OF_REGEXP);
              case 'b':
                Advance(2);
                builder->AddAssertion(
                    zone_->newInfallible<RegExpAssertion>(RegExpAssertion::BOUNDARY));
                continue;
              case 'B':
                Advance(2);
                builder->AddAssertion(
                    zone_->newInfallible<RegExpAssertion>(RegExpAssertion::NON_BOUNDARY));
                continue;
                // AtomEscape ::
                //   CharacterClassEscape
                //
                // CharacterClassEscape :: one of
                //   d D s S w W
              case 'd':
              case 'D':
              case 's':
              case 'S':
              case 'w':
              case 'W': {
                uc32 c = Next();
                Advance(2);
                CharacterRangeVector* ranges =
                    zone_->newInfallible<CharacterRangeVector>(*zone());
                CharacterRange::AddClassEscape(c, ranges,
                                               unicode() && ignore_case(), zone());
                RegExpCharacterClass* cc =
                    zone_->newInfallible<RegExpCharacterClass>(ranges);
                builder->AddCharacterClass(cc);
                break;
              }
              case '1':
              case '2':
              case '3':
              case '4':
              case '5':
              case '6':
              case '7':
              case '8':
              case '9': {
                int index = 0;
                bool is_backref = ParseBackReferenceIndex(&index);
                if (is_backref) {
                    if (state->IsInsideCaptureGroup(index)) {
                        // The back reference is inside the capture group it refers to.
                        // Nothing can possibly have been captured yet, so we use empty
                        // instead. This ensures that, when checking a back reference,
                        // the capture registers of the referenced capture are either
                        // both set or both cleared.
                        builder->AddEmpty();
                    } else {
                        RegExpCapture* capture = GetCapture(index);
                        RegExpTree* atom = zone_->newInfallible<RegExpBackReference>(capture);
                        builder->AddAtom(atom);
                    }
                    break;
                }
                // With /u, no identity escapes except for syntax characters
                // are allowed. Otherwise, all identity escapes are allowed.
                if (unicode()) {
                    return ReportError(JSMSG_BACK_REF_OUT_OF_RANGE);
                }
                uc32 first_digit = Next();
                if (first_digit == '8' || first_digit == '9') {
                    builder->AddCharacter(first_digit);
                    Advance(2);
                    break;
                }
                MOZ_FALLTHROUGH;
              }
              case '0': {
                Advance();
                if (unicode() && Next() >= '0' && Next() <= '9') {
                    // With /u, decimal escape with leading 0 are not parsed as octal.
                    return ReportError(JSMSG_INVALID_DECIMAL_ESCAPE);
                }
                uc32 octal = ParseOctalLiteral();
                builder->AddCharacter(octal);
                break;
              }
                // ControlEscape :: one of
                //   f n r t v
              case 'f':
                Advance(2);
                builder->AddCharacter('\f');
                break;
              case 'n':
                Advance(2);
                builder->AddCharacter('\n');
                break;
              case 'r':
                Advance(2);
                builder->AddCharacter('\r');
                break;
              case 't':
                Advance(2);
                builder->AddCharacter('\t');
                break;
              case 'v':
                Advance(2);
                builder->AddCharacter('\v');
                break;
              case 'c': {
                Advance();
                uc32 controlLetter = Next();
                // Special case if it is an ASCII letter.
                // Convert lower case letters to uppercase.
                uc32 letter = controlLetter & ~('a' ^ 'A');
                if (letter < 'A' || 'Z' < letter) {
                    // controlLetter is not in range 'A'-'Z' or 'a'-'z'.
                    // Read the backslash as a literal character instead of as
                    // starting an escape.
                    // ES#prod-annexB-ExtendedPatternCharacter
                    if (unicode()) {
                        // With /u, invalid escapes are not treated as identity escapes.
                        return ReportError(JSMSG_INVALID_IDENTITY_ESCAPE);
                    }
                    builder->AddCharacter('\\');
                } else {
                    Advance(2);
                    builder->AddCharacter(controlLetter & 0x1f);
                }
                break;
              }
              case 'x': {
                Advance(2);
                uc32 value;
                if (ParseHexEscape(2, &value)) {
                    builder->AddCharacter(value);
                } else if (!unicode()) {
                    builder->AddCharacter('x');
                } else {
                    // With /u, invalid escapes are not treated as identity escapes.
                    return ReportError(JSMSG_INVALID_IDENTITY_ESCAPE);
                }
                break;
              }
              case 'u': {
                Advance(2);
                uc32 value;
                bool parsed;
                if (!ParseUnicodeEscape(&value, &parsed))
                    return nullptr;
                if (parsed) {
                    builder->AddEscapedUnicodeCharacter(value);
                } else if (!unicode()) {
                    builder->AddCharacter('u');
                } else {
                    // With /u, invalid escapes are not treated as identity escapes.
                    return ReportError(JSMSG_INVALID_UNICODE_ESCAPE);
                }
                break;
              }
              default:
                Advance();
                // With /u, no identity escapes except for syntax characters
                // are allowed. Otherwise, all identity escapes are allowed.
                if (!unicode() || IsSyntaxCharacterOrSlash(current())) {
                    builder->AddCharacter(current());
                    Advance();
                } else {
                    return ReportError(JSMSG_INVALID_IDENTITY_ESCAPE);
                }
                break;
            }
            break;
          case '{': {
            int dummy;
            bool parsed = ParseIntervalQuantifier(&dummy, &dummy);
            if (parsed) return ReportError(JSMSG_NOTHING_TO_REPEAT);
            MOZ_FALLTHROUGH;
          }
          case '}':
          case ']':
            if (unicode()) {
                return ReportError(current() == ']'
                                   ? JSMSG_RAW_BRACKET_IN_REGEP
                                   : JSMSG_RAW_BRACE_IN_REGEP);
            }
            MOZ_FALLTHROUGH;
          default:
            builder->AddUnicodeCharacter(current());
            Advance();
            break;
        }  // end switch(current())

        int min;
        int max;
        switch (current()) {
            // QuantifierPrefix ::
            //   *
            //   +
            //   ?
            //   {
          case '*':
            min = 0;
            max = RegExpTree::kInfinity;
            Advance();
            break;
          case '+':
            min = 1;
            max = RegExpTree::kInfinity;
            Advance();
            break;
          case '?':
            min = 0;
            max = 1;
            Advance();
            break;
          case '{':
            if (ParseIntervalQuantifier(&min, &max)) {
                if (max < min) {
                    return ReportError(
                        JSMSG_NUMBERS_OUT_OF_ORDER);
                }
                break;
            } else if (unicode()) {
                // With /u, incomplete quantifiers are not allowed.
                return ReportError(JSMSG_RAW_BRACE_IN_REGEP);
            }
            continue;
          default:
            continue;
        }
        RegExpQuantifier::QuantifierType quantifier_type = RegExpQuantifier::GREEDY;
        if (current() == '?') {
            quantifier_type = RegExpQuantifier::NON_GREEDY;
            Advance();
        }
        if (!builder->AddQuantifierToAtom(min, max, quantifier_type)) {
            return ReportError(JSMSG_NOTHING_TO_REPEAT);
        }
    }
}

#ifdef DEBUG
// Currently only used in an DCHECK.
static bool IsSpecialClassEscape(uc32 c) {
  switch (c) {
    case 'd':
    case 'D':
    case 's':
    case 'S':
    case 'w':
    case 'W':
      return true;
    default:
      return false;
  }
}
#endif

// In order to know whether an escape is a backreference or not we have to scan
// the entire regexp and find the number of capturing parentheses.  However we
// don't want to scan the regexp twice unless it is necessary.  This mini-parser
// is called when needed.  It can see the difference between capturing and
// noncapturing parentheses and can skip character classes and backslash-escaped
// characters.
template <typename CharT>
void RegExpParser<CharT>::ScanForCaptures() {
    DCHECK(!is_scanned_for_captures_);
    const CharT* saved_position = position();
    // Start with captures started previous to current position
    int capture_count = captures_started();
    // Add count of captures after this position.
    int n;
    while ((n = current()) != kEndMarker) {
        Advance();
        switch (n) {
          case '\\':
            Advance();
            break;
          case '[': {
            int c;
            while ((c = current()) != kEndMarker) {
                Advance();
                if (c == '\\') {
                    Advance();
                } else {
                    if (c == ']') break;
                }
            }
            break;
          }
          case '(':
            if (current() == '?') {
                break;
            }
            capture_count++;
            break;
        }
    }
    capture_count_ = capture_count;
    is_scanned_for_captures_ = true;
    Reset(saved_position);
}

template <typename CharT>
bool RegExpParser<CharT>::ParseBackReferenceIndex(int* index_out) {
    DCHECK_EQ('\\', current());
    DCHECK('1' <= Next() && Next() <= '9');

    // Try to parse a decimal literal that is no greater than the total number
    // of left capturing parentheses in the input.
    const CharT* start = position();
    int value = Next() - '0';
    Advance(2);
    while (true) {
        uc32 c = current();
        if (IsDecimalDigit(c)) {
            value = 10 * value + (c - '0');
            if (value > kMaxCaptures) {
                Reset(start);
                return false;
            }
            Advance();
        } else {
            break;
        }
    }
    if (value > captures_started()) {
        if (!is_scanned_for_captures_) ScanForCaptures();
        if (value > capture_count_) {
            Reset(start);
            return false;
        }
    }
    *index_out = value;
    return true;
}

template <typename CharT>
RegExpCapture* RegExpParser<CharT>::GetCapture(int index) {
    // The index for the capture groups are one-based. Its index in the list is
    // zero-based.
    int know_captures =
        is_scanned_for_captures_ ? capture_count_ : captures_started_;
    DCHECK(index <= know_captures);
    if (captures_ == NULL) {
        captures_ = zone_->newInfallible<RegExpCaptureVector>(*zone());
        captures_->reserve(know_captures);
    }
    while (captures_->length() < know_captures) {
        captures_->append(zone_->newInfallible<RegExpCapture>(captures_->length() + 1));
    }
    return captures_->at(index - 1);
}

template <typename CharT>
bool RegExpParser<CharT>::RegExpParserState::IsInsideCaptureGroup(int index) {
    for (RegExpParserState* s = this; s != NULL; s = s->previous_state()) {
        if (s->group_type() != CAPTURE) continue;
        // Return true if we found the matching capture index.
        if (index == s->capture_index()) return true;
        // Abort if index is larger than what has been parsed up till this state.
        if (index > s->capture_index()) return false;
    }
    return false;
}

// QuantifierPrefix ::
//   { DecimalDigits }
//   { DecimalDigits , }
//   { DecimalDigits , DecimalDigits }
//
// Returns true if parsing succeeds, and set the min_out and max_out
// values. Values are truncated to RegExpTree::kInfinity if they overflow.
template <typename CharT>
bool RegExpParser<CharT>::ParseIntervalQuantifier(int* min_out, int* max_out) {
    DCHECK_EQ(current(), '{');
    const CharT* start = position();
    Advance();
    int min = 0;
    if (!IsDecimalDigit(current())) {
        Reset(start);
        return false;
    }
    while (IsDecimalDigit(current())) {
        int next = current() - '0';
        if (min > (RegExpTree::kInfinity - next) / 10) {
            // Overflow. Skip past remaining decimal digits and return -1.
            do {
                Advance();
            } while (IsDecimalDigit(current()));
            min = RegExpTree::kInfinity;
            break;
        }
        min = 10 * min + next;
        Advance();
    }
    int max = 0;
    if (current() == '}') {
        max = min;
        Advance();
    } else if (current() == ',') {
        Advance();
        if (current() == '}') {
            max = RegExpTree::kInfinity;
            Advance();
        } else {
            while (IsDecimalDigit(current())) {
                int next = current() - '0';
                if (max > (RegExpTree::kInfinity - next) / 10) {
                    do {
                        Advance();
                    } while (IsDecimalDigit(current()));
                    max = RegExpTree::kInfinity;
                    break;
                }
                max = 10 * max + next;
                Advance();
            }
            if (current() != '}') {
                Reset(start);
                return false;
            }
            Advance();
        }
    } else {
        Reset(start);
        return false;
    }
    *min_out = min;
    *max_out = max;
    return true;
}

template <typename CharT>
uc32 RegExpParser<CharT>::ParseOctalLiteral() {
    DCHECK(('0' <= current() && current() <= '7') || current() == kEndMarker);
    // For compatibility with some other browsers (not all), we parse
    // up to three octal digits with a value below 256.
    // ES#prod-annexB-LegacyOctalEscapeSequence
    uc32 value = current() - '0';
    Advance();
    if ('0' <= current() && current() <= '7') {
        value = value * 8 + current() - '0';
        Advance();
        if (value < 32 && '0' <= current() && current() <= '7') {
            value = value * 8 + current() - '0';
            Advance();
        }
    }
    return value;
}

template <typename CharT>
bool RegExpParser<CharT>::ParseHexEscape(int length, uc32* value) {
    const CharT* start = position();
    uc32 val = 0;
    for (int i = 0; i < length; ++i) {
        uc32 c = current();
        int d = HexValue(c);
        if (d < 0) {
            Reset(start);
            return false;
        }
        val = val * 16 + d;
        Advance();
    }
    *value = val;
    return true;
}

// This parses RegExpUnicodeEscapeSequence as described in ECMA262.
template <typename CharT>
bool RegExpParser<CharT>::ParseUnicodeEscape(uc32* value, bool* parsed) {
    // Accept both \uxxxx and \u{xxxxxx} (if harmony unicode escapes are
    // allowed). In the latter case, the number of hex digits between { } is
    // arbitrary. \ and u have already been read.
    if (current() == '{' && unicode()) {
        Advance();
        if (ParseUnlimitedLengthHexNumber(0x10ffff, value)) {
            if (current() == '}') {
                Advance();
                *parsed = true;
                return true;
            }
            ReportError(JSMSG_INVALID_UNICODE_ESCAPE);
            return false;
        }
        // Error already reported in ParseUnlimitedLengthHexNumber.
        return false;
    }
    // \u but no {, or \u{...} escapes not allowed.
    bool result = ParseHexEscape(4, value);
    if (result && unicode() && unicode::IsLeadSurrogate(*value) &&
        current() == '\\') {
        // Attempt to read trail surrogate.
        const CharT* start = position();
        if (Next() == 'u') {
            Advance(2);
            uc32 trail;
            if (ParseHexEscape(4, &trail) &&
                unicode::IsTrailSurrogate(trail)) {
                *value = unicode::UTF16Decode(static_cast<uc16>(*value),
                                              static_cast<uc16>(trail));
                *parsed = true;
                return true;
            }
        }
        Reset(start);
    }
    *parsed = result;
    return true;
}

template <typename CharT>
bool RegExpParser<CharT>::ParseUnlimitedLengthHexNumber(int max_value, uc32* value) {
    uc32 x = 0;
    int d = HexValue(current());
    if (d < 0) {
        ReportError(JSMSG_INVALID_UNICODE_ESCAPE);
        return false;
    }
    while (d >= 0) {
        x = x * 16 + d;
        if (x > max_value) {
            ReportError(JSMSG_UNICODE_OVERFLOW, "regular expression");
            return false;
        }
        Advance();
        d = HexValue(current());
    }
    *value = x;
    return true;
}

template <typename CharT>
bool RegExpParser<CharT>::ParseClassCharacterEscape(uc32* code) {
    DCHECK(current() == '\\');
    DCHECK(has_next() && !IsSpecialClassEscape(Next()));
    Advance();
    switch (current()) {
      case 'b':
        Advance();
        *code = '\b';
        return true;
      // ControlEscape :: one of
      //   f n r t v
      case 'f':
        Advance();
        *code = '\f';
        return true;
      case 'n':
        Advance();
        *code = '\n';
        return true;
      case 'r':
        Advance();
        *code = '\r';
        return true;
      case 't':
        Advance();
        *code = '\t';
        return true;
      case 'v':
        Advance();
        *code = '\v';
        return true;
      case 'c': {
        uc32 controlLetter = Next();
        uc32 letter = controlLetter & ~('A' ^ 'a');
        // Inside a character class, we also accept digits and underscore as
        // control characters, unless with /u. See Annex B:
        // ES#prod-annexB-ClassControlLetter
        if (letter >= 'A' && letter <= 'Z') {
            Advance(2);
            // Control letters mapped to ASCII control characters in the range
            // 0x00-0x1f.
            *code = controlLetter & 0x1f;
            return true;
        }
        if (unicode()) {
            // With /u, invalid escapes are not treated as identity escapes.
            ReportError(JSMSG_INVALID_IDENTITY_ESCAPE);
            return false;
        }
        if ((controlLetter >= '0' && controlLetter <= '9') ||
            controlLetter == '_') {
            Advance(2);
            *code = controlLetter & 0x1f;
            return true;
        }
        // We match JSC in reading the backslash as a literal
        // character instead of as starting an escape.
        // TODO(v8:6201): Not yet covered by the spec.
        *code = '\\';
        return true;
      }
      case '0':
        // With /u, \0 is interpreted as NUL if not followed by another digit.
        if (unicode() && !(Next() >= '0' && Next() <= '9')) {
            Advance();
            *code = 0;
            return true;
        }
        MOZ_FALLTHROUGH;
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
        // For compatibility, we interpret a decimal escape that isn't
        // a back reference (and therefore either \0 or not valid according
        // to the specification) as a 1..3 digit octal character code.
        // ES#prod-annexB-LegacyOctalEscapeSequence
        if (unicode()) {
            // With /u, decimal escape is not interpreted as octal character code.
            ReportError(current() == '0'
                        ? JSMSG_INVALID_DECIMAL_ESCAPE
                        : JSMSG_INVALID_IDENTITY_ESCAPE);
            return false;
        }
        *code = ParseOctalLiteral();
        return true;
      case 'x': {
        Advance();
        uc32 value;
        if (ParseHexEscape(2, &value)) {
            *code = value;
            return true;
        }
        if (unicode()) {
            // With /u, invalid escapes are not treated as identity escapes.
            ReportError(JSMSG_INVALID_IDENTITY_ESCAPE);
            return false;
        }
        // If \x is not followed by a two-digit hexadecimal, treat it
        // as an identity escape.
        *code = 'x';
        return true;
      }
      case 'u': {
        Advance();
        uc32 value;
        bool parsed;
        if (!ParseUnicodeEscape(&value, &parsed))
            return false;
        if (parsed) {
            *code = value;
            return true;
        }
        if (unicode()) {
            // With /u, invalid escapes are not treated as identity escapes.
            ReportError(JSMSG_INVALID_UNICODE_ESCAPE);
            return false;
        }
        // If \u is not followed by a four-digit hexadecimal or braced hexadecimal, treat it
        // as an identity escape.
        *code = 'u';
        return true;
      }
      default: {
        uc32 result = current();
        // With /u, no identity escapes except for syntax characters and '-' are
        // allowed. Otherwise, all identity escapes are allowed.
        if (!unicode() || IsSyntaxCharacterOrSlash(result) || result == '-') {
            Advance();
            *code = result;
            return true;
        }
        ReportError(JSMSG_INVALID_IDENTITY_ESCAPE);
        return false;
      }
    }
    return true;
}

template <typename CharT>
bool RegExpParser<CharT>::ParseClassEscape(CharacterRangeVector* ranges,
                                           Zone* zone,
                                           bool add_unicode_case_equivalents,
                                           uc32* char_out, bool* is_class_escape) {
    uc32 current_char = current();
    if (current_char == '\\') {
        switch (Next()) {
          case 'w':
          case 'W':
          case 'd':
          case 'D':
          case 's':
          case 'S': {
            CharacterRange::AddClassEscape(static_cast<char>(Next()), ranges,
                                           add_unicode_case_equivalents, zone);
            Advance(2);
            *is_class_escape = true;
            return true;
          }
          case kEndMarker:
            return ReportError(JSMSG_ESCAPE_AT_END_OF_REGEXP);
          default:
            break;
        }
        if (!ParseClassCharacterEscape(char_out))
            return false;
        *is_class_escape = false;
        return true;
    } else {
        Advance();
        *char_out = current_char;
        *is_class_escape = false;
        return true;
    }
}

template <typename CharT>
RegExpTree* RegExpParser<CharT>::ParseCharacterClass() {
    DCHECK_EQ(current(), '[');
    Advance();
    bool is_negated = false;
    if (current() == '^') {
        is_negated = true;
        Advance();
    }
    CharacterRangeVector* ranges = zone_->newInfallible<CharacterRangeVector>(*zone());
    bool add_unicode_case_equivalents = unicode() && ignore_case();
    while (has_more() && current() != ']') {
        uc32 char_1 = 0, char_2 = 0;
        bool is_class_1, is_class_2;
        if (!ParseClassEscape(ranges, zone(), add_unicode_case_equivalents, &char_1,
                              &is_class_1))
            return nullptr; {
        }
        if (current() == '-') {
            Advance();
            if (current() == kEndMarker) {
                // If we reach the end we break out of the loop and let the
                // following code report an error.
                break;
            } else if (current() == ']') {
                if (!is_class_1) ranges->append(CharacterRange::Singleton(char_1));
                ranges->append(CharacterRange::Singleton('-'));
                break;
            }
            if (!ParseClassEscape(ranges, zone(), add_unicode_case_equivalents, &char_2,
                                  &is_class_2))
                return nullptr; {
            }
            if (is_class_1 || is_class_2) {
                // Either end is an escaped character class. Treat the '-' verbatim.
                if (unicode()) {
                    // ES2015 21.2.2.15.1 step 1.
                    return ReportError(JSMSG_RANGE_WITH_CLASS_ESCAPE);
                }
                if (!is_class_1) ranges->append(CharacterRange::Singleton(char_1));
                ranges->append(CharacterRange::Singleton('-'));
                if (!is_class_2) ranges->append(CharacterRange::Singleton(char_2));
                continue;
            }
            // ES2015 21.2.2.15.1 step 6.
            if (char_1 > char_2) {
                return ReportError(JSMSG_BAD_CLASS_RANGE);
            }
            ranges->append(CharacterRange::Range(char_1, char_2));
        } else {
            if (!is_class_1) ranges->append(CharacterRange::Singleton(char_1));
        }
    }
    if (!has_more()) {
        return ReportError(JSMSG_UNTERM_CLASS);
    }
    Advance();
    if (ranges->length() == 0) {
        ranges->append(CharacterRange::Everything());
        is_negated = !is_negated;
    }
    RegExpCharacterClass::Flags flags;
    if (is_negated) flags = RegExpCharacterClass::NEGATED;
    return zone_->newInfallible<RegExpCharacterClass>(ranges, flags);
}

// ----------------------------------------------------------------------------
// RegExpBuilder

#ifdef DEBUG
#define LAST(x) last_added_ = x;
#else
#define LAST(x)
#endif

RegExpBuilder::RegExpBuilder(Zone* zone, bool ignore_case, bool unicode)
  : zone_(zone),
    pending_empty_(false),
    ignore_case_(ignore_case),
    unicode_(unicode),
    characters_(NULL),
    pending_surrogate_(kNoPendingSurrogate),
    terms_(),
    alternatives_()
#ifdef DEBUG
    ,
    last_added_(ADD_NONE)
#endif
{
}

void RegExpBuilder::AddLeadSurrogate(uc16 lead_surrogate) {
    DCHECK(unicode::IsLeadSurrogate(lead_surrogate));
    FlushPendingSurrogate();
    // Hold onto the lead surrogate, waiting for a trail surrogate to follow.
    pending_surrogate_ = lead_surrogate;
}

void RegExpBuilder::AddTrailSurrogate(uc16 trail_surrogate) {
    DCHECK(unicode::IsTrailSurrogate(trail_surrogate));
    if (pending_surrogate_ != kNoPendingSurrogate) {
        uc16 lead_surrogate = pending_surrogate_;
        pending_surrogate_ = kNoPendingSurrogate;
        DCHECK(unicode::IsLeadSurrogate(lead_surrogate));
        uc32 combined =
            unicode::UTF16Decode(lead_surrogate, trail_surrogate);
        if (NeedsDesugaringForIgnoreCase(combined)) {
            AddCharacterClassForDesugaring(combined);
        } else {
            CharacterVector* surrogate_pair = zone()->newInfallible<CharacterVector>(*zone());
            surrogate_pair->append(lead_surrogate);
            surrogate_pair->append(trail_surrogate);
            RegExpAtom* atom =
                zone()->newInfallible<RegExpAtom>(surrogate_pair);
            AddAtom(atom);
        }
    } else {
        pending_surrogate_ = trail_surrogate;
        FlushPendingSurrogate();
    }
}

void RegExpBuilder::FlushPendingSurrogate() {
    if (pending_surrogate_ != kNoPendingSurrogate) {
        DCHECK(unicode());
        uc32 c = pending_surrogate_;
        pending_surrogate_ = kNoPendingSurrogate;
        AddCharacterClassForDesugaring(c);
    }
}

void RegExpBuilder::FlushCharacters() {
    FlushPendingSurrogate();
    pending_empty_ = false;
    if (characters_ != NULL) {
        RegExpTree* atom = zone()->newInfallible<RegExpAtom>(characters_);
        characters_ = NULL;
        text_.Add(atom, zone());
        LAST(ADD_ATOM);
    }
}

void RegExpBuilder::FlushText() {
    FlushCharacters();
    int num_text = text_.length();
    if (num_text == 0) {
        return;
    } else if (num_text == 1) {
        terms_.Add(text_.last(), zone());
    } else {
        RegExpText* text = zone()->newInfallible<RegExpText>(zone());
        for (int i = 0; i < num_text; i++) text_.Get(i)->AppendToText(text, zone());
        terms_.Add(text, zone());
    }
    text_.Clear();
}

void RegExpBuilder::AddCharacter(uc16 c) {
    FlushPendingSurrogate();
    pending_empty_ = false;
    if (NeedsDesugaringForIgnoreCase(c)) {
        AddCharacterClassForDesugaring(c);
    } else {
        if (characters_ == NULL) {
            characters_ = zone()->newInfallible<CharacterVector>(*zone());
        }
        characters_->append(c);
        LAST(ADD_CHAR);
    }
}

void RegExpBuilder::AddUnicodeCharacter(uc32 c) {
    if (c > static_cast<uc32>(unicode::UTF16Max)) {
        DCHECK(unicode());
        AddLeadSurrogate(unicode::LeadSurrogate(c));
        AddTrailSurrogate(unicode::TrailSurrogate(c));
    } else if (unicode() && unicode::IsLeadSurrogate(c)) {
        AddLeadSurrogate(c);
    } else if (unicode() && unicode::IsTrailSurrogate(c)) {
        AddTrailSurrogate(c);
    } else {
        AddCharacter(static_cast<uc16>(c));
    }
}

void RegExpBuilder::AddEscapedUnicodeCharacter(uc32 character) {
    // A lead or trail surrogate parsed via escape sequence will not
    // pair up with any preceding lead or following trail surrogate.
    FlushPendingSurrogate();
    AddUnicodeCharacter(character);
    FlushPendingSurrogate();
}

void RegExpBuilder::AddEmpty() { pending_empty_ = true; }

void RegExpBuilder::AddCharacterClass(RegExpCharacterClass* cc) {
    if (NeedsDesugaringForUnicode(cc)) {
        // With /u, character class needs to be desugared, so it
        // must be a standalone term instead of being part of a RegExpText.
        AddTerm(cc);
    } else {
        AddAtom(cc);
    }
}

void RegExpBuilder::AddCharacterClassForDesugaring(uc32 c) {
    AddTerm(zone_->newInfallible<RegExpCharacterClass>(
        CharacterRange::List(zone(), CharacterRange::Singleton(c))));
}

void RegExpBuilder::AddAtom(RegExpTree* term) {
    if (term->IsEmpty()) {
        AddEmpty();
        return;
    }
    if (term->IsTextElement()) {
        FlushCharacters();
        text_.Add(term, zone());
    } else {
        FlushText();
        terms_.Add(term, zone());
    }
    LAST(ADD_ATOM);
}

void RegExpBuilder::AddTerm(RegExpTree* term) {
    FlushText();
    terms_.Add(term, zone());
    LAST(ADD_ATOM);
}

void RegExpBuilder::AddAssertion(RegExpTree* assert) {
    FlushText();
    if (terms_.length() > 0 && terms_.last()->IsAssertion()) {
        // Omit repeated assertions of the same type.
        RegExpAssertion* last = terms_.last()->AsAssertion();
        RegExpAssertion* next = assert->AsAssertion();
        if (last->assertion_type() == next->assertion_type()) return;
    }
    terms_.Add(assert, zone());
    LAST(ADD_ASSERT);
}

void RegExpBuilder::NewAlternative() { FlushTerms(); }

void RegExpBuilder::FlushTerms() {
    FlushText();
    int num_terms = terms_.length();
    RegExpTree* alternative;
    if (num_terms == 0) {
        alternative = RegExpEmpty::GetInstance();
    } else if (num_terms == 1) {
        alternative = terms_.last();
    } else {
        alternative = zone()->newInfallible<RegExpAlternative>(terms_.GetList(zone()));
    }
    alternatives_.Add(alternative, zone());
    terms_.Clear();
    LAST(ADD_NONE);
}

bool RegExpBuilder::NeedsDesugaringForUnicode(RegExpCharacterClass* cc) {
    if (!unicode()) return false;
    // TODO(yangguo): we could be smarter than this. Case-insensitivity does not
    // necessarily mean that we need to desugar. It's probably nicer to have a
    // separate pass to figure out unicode desugarings.
    if (ignore_case()) return true;
    CharacterRangeVector* ranges = cc->ranges(zone());
    CharacterRange::Canonicalize(ranges);
    for (int i = ranges->length() - 1; i >= 0; i--) {
        uc32 from = ranges->at(i).from();
        uc32 to = ranges->at(i).to();
        // Check for non-BMP characters.
        if (to >= unicode::NonBMPMin) return true;
        // Check for lone surrogates.
        if (from <= unicode::TrailSurrogateMax && to >= unicode::LeadSurrogateMin) return true;
    }
    return false;
}


bool RegExpBuilder::NeedsDesugaringForIgnoreCase(uc32 c) {
    if (unicode() && ignore_case()) {
        return unicode::HasCaseFold(c);
    }
    return false;
}

RegExpTree* RegExpBuilder::ToRegExp() {
    FlushTerms();
    int num_alternatives = alternatives_.length();
    if (num_alternatives == 0) return RegExpEmpty::GetInstance();
    if (num_alternatives == 1) return alternatives_.last();
    return zone()->newInfallible<RegExpDisjunction>(alternatives_.GetList(zone()));
}

bool RegExpBuilder::AddQuantifierToAtom(
        int min, int max, RegExpQuantifier::QuantifierType quantifier_type) {
    FlushPendingSurrogate();
    if (pending_empty_) {
        pending_empty_ = false;
        return true;
    }
    RegExpTree* atom;
    if (characters_ != NULL) {
        DCHECK(last_added_ == ADD_CHAR);
        // Last atom was character.
        CharacterVector* char_vector = characters_;
        int num_chars = char_vector->length();
        if (num_chars > 1) {
            CharacterVector* prefix = zone()->newInfallible<CharacterVector>(*zone());
            prefix->append(char_vector->begin(), num_chars - 1);
            text_.Add(zone()->newInfallible<RegExpAtom>(prefix), zone());
            char_vector = zone()->newInfallible<CharacterVector>(*zone());
            char_vector->append((*characters_)[num_chars - 1]);
        }
        characters_ = NULL;
        atom = zone()->newInfallible<RegExpAtom>(char_vector);
        FlushText();
    } else if (text_.length() > 0) {
        DCHECK(last_added_ == ADD_ATOM);
        atom = text_.RemoveLast();
        FlushText();
    } else if (terms_.length() > 0) {
        DCHECK(last_added_ == ADD_ATOM);
        atom = terms_.RemoveLast();
        // With /u, lookarounds are not quantifiable.
        if (unicode() && atom->IsLookaround()) return false;
        if (atom->max_match() == 0) {
            // Guaranteed to only match an empty string.
            LAST(ADD_TERM);
            if (min == 0) {
                return true;
            }
            terms_.Add(atom, zone());
            return true;
        }
    } else {
        // Only call immediately after adding an atom or character!
        MOZ_CRASH("Bad call");
    }
    terms_.Add(zone()->newInfallible<RegExpQuantifier>(min, max, quantifier_type, atom),
               zone());
    LAST(ADD_TERM);
    return true;
}

#undef LAST

template class irregexp::RegExpParser<Latin1Char>;
template class irregexp::RegExpParser<char16_t>;

template <typename CharT>
static bool
ParsePattern(frontend::TokenStreamAnyChars& ts, LifoAlloc& alloc,
             const CharT* chars, size_t length,
             bool multiline, bool match_only, bool unicode, bool ignore_case,
             bool global, bool sticky, RegExpCompileData* data)
{
    // We shouldn't strip pattern for exec, or test with global/sticky,
    // to reflect correct match position and lastIndex.
    if (match_only && !global && !sticky) {
        // Try to strip a leading '.*' from the RegExp, but only if it is not
        // followed by a '?' (which will affect how the .* is parsed). This
        // pattern will affect the captures produced by the RegExp, but not
        // whether there is a match or not.
        if (length >= 3 && chars[0] == '.' && chars[1] == '*' && chars[2] != '?') {
            chars += 2;
            length -= 2;
        }

        // Try to strip a trailing '.*' from the RegExp, which as above will
        // affect the captures but not whether there is a match. Only do this
        // when there are no other meta characters in the RegExp, so that we
        // are sure this will not affect how the RegExp is parsed.
        if (length >= 3 && !HasRegExpMetaChars(chars, length - 2) &&
            chars[length - 2] == '.' && chars[length - 1] == '*')
        {
            length -= 2;
        }
    }

    RegExpParser<CharT> parser(ts, &alloc, chars, chars + length, multiline, unicode, ignore_case);
    data->tree = parser.ParsePattern();
    if (!data->tree)
        return false;

    data->simple = parser.simple();
    data->contains_anchor = parser.contains_anchor();
    data->capture_count = parser.captures_started();
    return true;
}

bool
irregexp::ParsePattern(frontend::TokenStreamAnyChars& ts, LifoAlloc& alloc, JSAtom* str,
                       bool multiline, bool match_only, bool unicode, bool ignore_case,
                       bool global, bool sticky, RegExpCompileData* data)
{
    JS::AutoCheckCannotGC nogc;
    return str->hasLatin1Chars()
           ? ::ParsePattern(ts, alloc, str->latin1Chars(nogc), str->length(),
                            multiline, match_only, unicode, ignore_case, global, sticky, data)
           : ::ParsePattern(ts, alloc, str->twoByteChars(nogc), str->length(),
                            multiline, match_only, unicode, ignore_case, global, sticky, data);
}

template <typename CharT>
static bool
ParsePatternSyntax(frontend::TokenStreamAnyChars& ts, LifoAlloc& alloc,
                   const CharT* chars, size_t length, bool unicode)
{
    LifoAllocScope scope(&alloc);

    RegExpParser<CharT> parser(ts, &alloc, chars, chars + length, false, unicode, false);
    return parser.ParsePattern() != nullptr;
}

bool
irregexp::ParsePatternSyntax(frontend::TokenStreamAnyChars& ts, LifoAlloc& alloc, JSAtom* str,
                             bool unicode)
{
    JS::AutoCheckCannotGC nogc;
    return str->hasLatin1Chars()
           ? ::ParsePatternSyntax(ts, alloc, str->latin1Chars(nogc), str->length(), unicode)
           : ::ParsePatternSyntax(ts, alloc, str->twoByteChars(nogc), str->length(), unicode);
}

bool
irregexp::ParsePatternSyntax(frontend::TokenStreamAnyChars& ts, LifoAlloc& alloc,
                             const mozilla::Range<const char16_t> chars, bool unicode)
{
    return ::ParsePatternSyntax(ts, alloc, chars.begin().get(), chars.length(), unicode);
}
