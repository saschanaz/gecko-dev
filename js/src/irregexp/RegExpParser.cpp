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
IsDecimalDigit(widechar c)
{
    // ECMA-262, 3rd, 7.8.3 (p 16)
    return IsInRange(c, '0', '9');
}

// ----------------------------------------------------------------------------
// SpiderMonkey implementation for Unicode RegExps

static inline RegExpTree*
RangeAtom(LifoAlloc* alloc, char16_t from, char16_t to)
{
    CharacterRangeVector* ranges = alloc->newInfallible<CharacterRangeVector>(*alloc);
    ranges->append(CharacterRange::Range(from, to));
    return alloc->newInfallible<RegExpCharacterClass>(ranges);
}

static inline RegExpTree*
NegativeLookahead(LifoAlloc* alloc, char16_t from, char16_t to)
{
    return alloc->newInfallible<RegExpLookaround>(RangeAtom(alloc, from, to), false, 0, 0,
                                                  RegExpLookaround::LOOKAHEAD);
}

class WideCharRange
{
  public:
    WideCharRange()
      : from_(0), to_(0)
    {}

    WideCharRange(widechar from, widechar to)
      : from_(from), to_(to)
    {}

    static inline WideCharRange Singleton(widechar value) {
        return WideCharRange(value, value);
    }
    static inline WideCharRange Range(widechar from, widechar to) {
        MOZ_ASSERT(from <= to);
        return WideCharRange(from, to);
    }

    bool Contains(widechar i) const { return from_ <= i && i <= to_; }
    widechar from() const { return from_; }
    widechar to() const { return to_; }

  private:
    widechar from_;
    widechar to_;
};

typedef InfallibleVector<WideCharRange, 1> WideCharRangeVector;

static inline CharacterRange
LeadSurrogateRange()
{
    return CharacterRange::Range(unicode::LeadSurrogateMin, unicode::LeadSurrogateMax);
}

static inline CharacterRange
TrailSurrogateRange()
{
    return CharacterRange::Range(unicode::TrailSurrogateMin, unicode::TrailSurrogateMax);
}

static inline WideCharRange
NonBMPRange()
{
    return WideCharRange::Range(unicode::NonBMPMin, unicode::NonBMPMax);
}

static const char16_t kNoCharClass = 0;

// Adds a character or pre-defined character class to character ranges.
// If char_class is not kInvalidClass, it's interpreted as a class
// escape (i.e., 's' means whitespace, from '\s').
static inline void
AddCharOrEscape(LifoAlloc* alloc,
                CharacterRangeVector* ranges,
                char16_t char_class,
                widechar c)
{
    if (char_class != kNoCharClass)
        CharacterRange::AddClassEscape(char_class, ranges, alloc);
    else
        ranges->append(CharacterRange::Singleton(c));
}

static inline void
AddCharOrEscapeUnicode(LifoAlloc* alloc,
                       CharacterRangeVector* ranges,
                       CharacterRangeVector* lead_ranges,
                       CharacterRangeVector* trail_ranges,
                       WideCharRangeVector* wide_ranges,
                       char16_t char_class,
                       widechar c,
                       bool ignore_case)
{
    if (char_class != kNoCharClass) {
        CharacterRange::AddClassEscapeUnicode(char_class, ranges, ignore_case, alloc);
        switch (char_class) {
          case 'S':
          case 'W':
          case 'D':
            lead_ranges->append(LeadSurrogateRange());
            trail_ranges->append(TrailSurrogateRange());
            wide_ranges->append(NonBMPRange());
            break;
          case '.':
            MOZ_CRASH("Bad char_class!");
        }
        return;
    }

    if (unicode::IsLeadSurrogate(c))
        lead_ranges->append(CharacterRange::Singleton(c));
    else if (unicode::IsTrailSurrogate(c))
        trail_ranges->append(CharacterRange::Singleton(c));
    else if (c >= unicode::NonBMPMin)
        wide_ranges->append(WideCharRange::Singleton(c));
    else
        ranges->append(CharacterRange::Singleton(c));
}

static inline void
AddUnicodeRange(LifoAlloc* alloc,
                CharacterRangeVector* ranges,
                CharacterRangeVector* lead_ranges,
                CharacterRangeVector* trail_ranges,
                WideCharRangeVector* wide_ranges,
                widechar first,
                widechar next)
{
    MOZ_ASSERT(first <= next);
    if (first < unicode::LeadSurrogateMin) {
        if (next < unicode::LeadSurrogateMin) {
            ranges->append(CharacterRange::Range(first, next));
            return;
        }
        ranges->append(CharacterRange::Range(first, unicode::LeadSurrogateMin - 1));
        first = unicode::LeadSurrogateMin;
    }
    if (first <= unicode::LeadSurrogateMax) {
        if (next <= unicode::LeadSurrogateMax) {
            lead_ranges->append(CharacterRange::Range(first, next));
            return;
        }
        lead_ranges->append(CharacterRange::Range(first, unicode::LeadSurrogateMax));
        first = unicode::LeadSurrogateMax + 1;
    }
    MOZ_ASSERT(unicode::LeadSurrogateMax + 1 == unicode::TrailSurrogateMin);
    if (first <= unicode::TrailSurrogateMax) {
        if (next <= unicode::TrailSurrogateMax) {
            trail_ranges->append(CharacterRange::Range(first, next));
            return;
        }
        trail_ranges->append(CharacterRange::Range(first, unicode::TrailSurrogateMax));
        first = unicode::TrailSurrogateMax + 1;
    }
    if (first <= unicode::UTF16Max) {
        if (next <= unicode::UTF16Max) {
            ranges->append(CharacterRange::Range(first, next));
            return;
        }
        ranges->append(CharacterRange::Range(first, unicode::UTF16Max));
        first = unicode::NonBMPMin;
    }
    MOZ_ASSERT(unicode::UTF16Max + 1 == unicode::NonBMPMin);
    wide_ranges->append(WideCharRange::Range(first, next));
}

// Negate a vector of ranges by subtracting its ranges from a range
// encompassing the full range of possible values.
template <typename RangeType>
static inline void
NegateUnicodeRanges(LifoAlloc* alloc, InfallibleVector<RangeType, 1>** ranges,
                    RangeType full_range)
{
    typedef InfallibleVector<RangeType, 1> RangeVector;
    RangeVector* tmp_ranges = alloc->newInfallible<RangeVector>(*alloc);
    tmp_ranges->append(full_range);
    RangeVector* result_ranges = alloc->newInfallible<RangeVector>(*alloc);

    // Perform the following calculation:
    //   result_ranges = tmp_ranges - ranges
    // with the following steps:
    //   result_ranges = tmp_ranges - ranges[0]
    //   SWAP(result_ranges, tmp_ranges)
    //   result_ranges = tmp_ranges - ranges[1]
    //   SWAP(result_ranges, tmp_ranges)
    //   ...
    //   result_ranges = tmp_ranges - ranges[N-1]
    //   SWAP(result_ranges, tmp_ranges)
    // The last SWAP is just for simplicity of the loop.
    for (int i = 0; i < (*ranges)->length(); i++) {
        result_ranges->clear();

        const RangeType& range = (**ranges)[i];
        for (int j = 0; j < tmp_ranges->length(); j++) {
            const RangeType& tmpRange = (*tmp_ranges)[j];
            auto from1 = tmpRange.from();
            auto to1 = tmpRange.to();
            auto from2 = range.from();
            auto to2 = range.to();

            if (from1 < from2) {
                if (to1 < from2) {
                    result_ranges->append(tmpRange);
                } else if (to1 <= to2) {
                    result_ranges->append(RangeType::Range(from1, from2 - 1));
                } else {
                    result_ranges->append(RangeType::Range(from1, from2 - 1));
                    result_ranges->append(RangeType::Range(to2 + 1, to1));
                }
            } else if (from1 <= to2) {
                if (to1 > to2)
                    result_ranges->append(RangeType::Range(to2 + 1, to1));
            } else {
                result_ranges->append(tmpRange);
            }
        }

        auto tmp = tmp_ranges;
        tmp_ranges = result_ranges;
        result_ranges = tmp;
    }

    // After the loop, result is pointed at by tmp_ranges, instead of
    // result_ranges.
    *ranges = tmp_ranges;
}

static bool
WideCharRangesContain(WideCharRangeVector* wide_ranges, widechar c)
{
    for (int i = 0; i < wide_ranges->length(); i++) {
        const WideCharRange& range = (*wide_ranges)[i];
        if (range.Contains(c))
            return true;
    }
    return false;
}

static void
CalculateCaseInsensitiveRanges(LifoAlloc* alloc, widechar from, widechar to, int32_t diff,
                               WideCharRangeVector* wide_ranges,
                               WideCharRangeVector** tmp_wide_ranges)
{
    widechar contains_from = 0;
    widechar contains_to = 0;
    for (widechar c = from; c <= to; c++) {
        if (WideCharRangesContain(wide_ranges, c) &&
            !WideCharRangesContain(wide_ranges, c + diff))
        {
            if (contains_from == 0)
                contains_from = c;
            contains_to = c;
        } else if (contains_from != 0) {
            if (!*tmp_wide_ranges)
                *tmp_wide_ranges = alloc->newInfallible<WideCharRangeVector>(*alloc);

            (*tmp_wide_ranges)->append(WideCharRange::Range(contains_from + diff,
                                                            contains_to + diff));
            contains_from = 0;
        }
    }

    if (contains_from != 0) {
        if (!*tmp_wide_ranges)
            *tmp_wide_ranges = alloc->newInfallible<WideCharRangeVector>(*alloc);

        (*tmp_wide_ranges)->append(WideCharRange::Range(contains_from + diff,
                                                        contains_to + diff));
    }
}

static RegExpTree*
UnicodeRangesAtom(LifoAlloc* alloc,
                  CharacterRangeVector* ranges,
                  CharacterRangeVector* lead_ranges,
                  CharacterRangeVector* trail_ranges,
                  WideCharRangeVector* wide_ranges,
                  bool is_negated,
                  bool ignore_case)
{
    // Calculate case folding for non-BMP first and negate the range if needed.
    if (ignore_case) {
        WideCharRangeVector* tmp_wide_ranges = nullptr;
#define CALL_CALC(FROM, TO, LEAD, TRAIL_FROM, TRAIL_TO, DIFF) \
        CalculateCaseInsensitiveRanges(alloc, FROM, TO, DIFF, wide_ranges, &tmp_wide_ranges);
        FOR_EACH_NON_BMP_CASE_FOLDING(CALL_CALC)
        FOR_EACH_NON_BMP_REV_CASE_FOLDING(CALL_CALC)
#undef CALL_CALC

        if (tmp_wide_ranges) {
            for (int i = 0; i < tmp_wide_ranges->length(); i++)
                wide_ranges->append((*tmp_wide_ranges)[i]);
        }
    }

    if (is_negated) {
        NegateUnicodeRanges(alloc, &lead_ranges, LeadSurrogateRange());
        NegateUnicodeRanges(alloc, &trail_ranges, TrailSurrogateRange());
        NegateUnicodeRanges(alloc, &wide_ranges, NonBMPRange());
    }

    bool unicode = true;
    RegExpBuilder* builder = alloc->newInfallible<RegExpBuilder>(alloc, ignore_case, unicode);

    bool added = false;

    if (is_negated) {
        ranges->append(LeadSurrogateRange());
        ranges->append(TrailSurrogateRange());
    }
    if (ranges->length() > 0) {
        RegExpCharacterClass::Flags flags;
        if (is_negated) flags = RegExpCharacterClass::NEGATED;
        builder->AddAtom(alloc->newInfallible<RegExpCharacterClass>(ranges, flags));
        added = true;
    }

    if (lead_ranges->length() > 0) {
        if (added)
            builder->NewAlternative();
        builder->AddAtom(alloc->newInfallible<RegExpCharacterClass>(lead_ranges));
        builder->AddAtom(NegativeLookahead(alloc, unicode::TrailSurrogateMin,
                                           unicode::TrailSurrogateMax));
        added = true;
    }

    if (trail_ranges->length() > 0) {
        if (added)
            builder->NewAlternative();
        builder->AddAssertion(alloc->newInfallible<RegExpAssertion>(
            RegExpAssertion::NOT_AFTER_LEAD_SURROGATE));
        builder->AddAtom(alloc->newInfallible<RegExpCharacterClass>(trail_ranges));
        added = true;
    }

    for (int i = 0; i < wide_ranges->length(); i++) {
        if (added)
            builder->NewAlternative();

        const WideCharRange& range = (*wide_ranges)[i];
        widechar from = range.from();
        widechar to = range.to();
        char16_t from_lead, from_trail;
        char16_t to_lead, to_trail;

        unicode::UTF16Encode(from, &from_lead, &from_trail);
        if (from == to) {
            builder->AddCharacter(from_lead);
            builder->AddCharacter(from_trail);
        } else {
            unicode::UTF16Encode(to, &to_lead, &to_trail);
            if (from_lead == to_lead) {
                MOZ_ASSERT(from_trail != to_trail);
                builder->AddCharacter(from_lead);
                builder->AddAtom(RangeAtom(alloc, from_trail, to_trail));
            } else if (from_trail == unicode::TrailSurrogateMin &&
                       to_trail == unicode::TrailSurrogateMax)
            {
                builder->AddAtom(RangeAtom(alloc, from_lead, to_lead));
                builder->AddAtom(RangeAtom(alloc, unicode::TrailSurrogateMin,
                                           unicode::TrailSurrogateMax));
            } else if (from_lead + 1 == to_lead) {
                builder->AddCharacter(from_lead);
                builder->AddAtom(RangeAtom(alloc, from_trail, unicode::TrailSurrogateMax));

                builder->NewAlternative();

                builder->AddCharacter(to_lead);
                builder->AddAtom(RangeAtom(alloc, unicode::TrailSurrogateMin, to_trail));
            } else if (from_lead + 2 == to_lead) {
                builder->AddCharacter(from_lead);
                builder->AddAtom(RangeAtom(alloc, from_trail, unicode::TrailSurrogateMax));

                builder->NewAlternative();

                builder->AddCharacter(from_lead + 1);
                builder->AddAtom(RangeAtom(alloc, unicode::TrailSurrogateMin,
                                           unicode::TrailSurrogateMax));

                builder->NewAlternative();

                builder->AddCharacter(to_lead);
                builder->AddAtom(RangeAtom(alloc, unicode::TrailSurrogateMin, to_trail));
            } else {
                builder->AddCharacter(from_lead);
                builder->AddAtom(RangeAtom(alloc, from_trail, unicode::TrailSurrogateMax));

                builder->NewAlternative();

                builder->AddAtom(RangeAtom(alloc, from_lead + 1, to_lead - 1));
                builder->AddAtom(RangeAtom(alloc, unicode::TrailSurrogateMin,
                                           unicode::TrailSurrogateMax));

                builder->NewAlternative();

                builder->AddCharacter(to_lead);
                builder->AddAtom(RangeAtom(alloc, unicode::TrailSurrogateMin, to_trail));
            }
        }
        added = true;
    }

    return builder->ToRegExp();
}

template <typename CharT>
bool
RegExpParser<CharT>::ParseClassAtom(char16_t* char_class, widechar* value)
{
    MOZ_ASSERT(*char_class == kNoCharClass);
    widechar first = current();
    if (first == '\\') {
        switch (Next()) {
          case 'w': case 'W': case 'd': case 'D': case 's': case 'S': {
            *char_class = Next();
            Advance(2);
            return true;
          }
          case kEndMarker:
            return ReportError(JSMSG_ESCAPE_AT_END_OF_REGEXP);
          default:
            if (!ParseClassCharacterEscape(value))
                return false;
            return true;
        }
    } else {
        if (unicode_) {
            char16_t lead, trail;
            if (ParseRawSurrogatePair(&lead, &trail)) {
                *value = unicode::UTF16Decode(lead, trail);
                return true;
            }
        }
        Advance();
        *value = first;
        return true;
    }
}

static inline RegExpTree*
CaseFoldingSurrogatePairAtom(LifoAlloc* alloc, char16_t lead, char16_t trail, int32_t diff)
{
    bool ignore_case = true;
    bool unicode = true;
    RegExpBuilder* builder = alloc->newInfallible<RegExpBuilder>(alloc, ignore_case, unicode);

    builder->AddCharacter(lead);
    CharacterRangeVector* ranges = alloc->newInfallible<CharacterRangeVector>(*alloc);
    ranges->append(CharacterRange::Range(trail, trail));
    ranges->append(CharacterRange::Range(trail + diff, trail + diff));
    builder->AddAtom(alloc->newInfallible<RegExpCharacterClass>(ranges));

    return builder->ToRegExp();
}

static inline RegExpTree*
SurrogatePairAtom(LifoAlloc* alloc, char16_t lead, char16_t trail, bool ignore_case)
{
    if (ignore_case) {
#define CALL_ATOM(FROM, TO, LEAD, TRAIL_FROM, TRAIL_TO, DIFF) \
        if (lead == LEAD &&trail >= TRAIL_FROM && trail <= TRAIL_TO) \
            return CaseFoldingSurrogatePairAtom(alloc, lead, trail, DIFF);
        FOR_EACH_NON_BMP_CASE_FOLDING(CALL_ATOM)
        FOR_EACH_NON_BMP_REV_CASE_FOLDING(CALL_ATOM)
#undef CALL_ATOM
    }

    bool unicode = true;
    RegExpBuilder* builder = alloc->newInfallible<RegExpBuilder>(alloc, ignore_case, unicode);
    builder->AddCharacter(lead);
    builder->AddCharacter(trail);
    return builder->ToRegExp();
}

static inline RegExpTree*
LeadSurrogateAtom(LifoAlloc* alloc, char16_t value, bool ignore_case)
{
    bool unicode = true;
    RegExpBuilder* builder = alloc->newInfallible<RegExpBuilder>(alloc, ignore_case, unicode);
    builder->AddCharacter(value);
    builder->AddAtom(NegativeLookahead(alloc, unicode::TrailSurrogateMin,
                                       unicode::TrailSurrogateMax));
    return builder->ToRegExp();
}

static inline RegExpTree*
TrailSurrogateAtom(LifoAlloc* alloc, char16_t value, bool ignore_case)
{
    bool unicode = true;
    RegExpBuilder* builder = alloc->newInfallible<RegExpBuilder>(alloc, ignore_case, unicode);
    builder->AddAssertion(alloc->newInfallible<RegExpAssertion>(
        RegExpAssertion::NOT_AFTER_LEAD_SURROGATE));
    builder->AddCharacter(value);
    return builder->ToRegExp();
}

static inline RegExpTree*
UnicodeEverythingAtom(LifoAlloc* alloc, bool ignore_case)
{
    bool unicode = true;
    RegExpBuilder* builder = alloc->newInfallible<RegExpBuilder>(alloc, ignore_case, unicode);

    // everything except \x0a, \x0d, \u2028 and \u2029

    CharacterRangeVector* ranges = alloc->newInfallible<CharacterRangeVector>(*alloc);
    AddClassNegated(kLineTerminatorAndSurrogateRanges,
                    kLineTerminatorAndSurrogateRangeCount,
                    ranges);
    builder->AddAtom(alloc->newInfallible<RegExpCharacterClass>(ranges));

    builder->NewAlternative();

    builder->AddAtom(RangeAtom(alloc, unicode::LeadSurrogateMin, unicode::LeadSurrogateMax));
    builder->AddAtom(NegativeLookahead(alloc, unicode::TrailSurrogateMin,
                                       unicode::TrailSurrogateMax));

    builder->NewAlternative();

    builder->AddAssertion(alloc->newInfallible<RegExpAssertion>(
        RegExpAssertion::NOT_AFTER_LEAD_SURROGATE));
    builder->AddAtom(RangeAtom(alloc, unicode::TrailSurrogateMin, unicode::TrailSurrogateMax));

    builder->NewAlternative();

    builder->AddAtom(RangeAtom(alloc, unicode::LeadSurrogateMin, unicode::LeadSurrogateMax));
    builder->AddAtom(RangeAtom(alloc, unicode::TrailSurrogateMin, unicode::TrailSurrogateMax));

    return builder->ToRegExp();
}

RegExpTree*
UnicodeCharacterClassEscapeAtom(LifoAlloc* alloc, char16_t char_class, bool ignore_case)
{
    CharacterRangeVector* ranges = alloc->newInfallible<CharacterRangeVector>(*alloc);
    CharacterRangeVector* lead_ranges = alloc->newInfallible<CharacterRangeVector>(*alloc);
    CharacterRangeVector* trail_ranges = alloc->newInfallible<CharacterRangeVector>(*alloc);
    WideCharRangeVector* wide_ranges = alloc->newInfallible<WideCharRangeVector>(*alloc);
    AddCharOrEscapeUnicode(alloc, ranges, lead_ranges, trail_ranges, wide_ranges, char_class, 0,
                           ignore_case);

    return UnicodeRangesAtom(alloc, ranges, lead_ranges, trail_ranges, wide_ranges, false, false);
}

static inline RegExpTree*
UnicodeBackReferenceAtom(LifoAlloc* alloc, RegExpTree* atom, bool ignore_case)
{
    // If a back reference has a standalone lead surrogate as its last
    // character, then that lead surrogate shouldn't match lead surrogates that
    // are paired with a corresponding trail surrogate.
    bool unicode = true;
    RegExpBuilder* builder = alloc->newInfallible<RegExpBuilder>(alloc, ignore_case, unicode);

    builder->AddAtom(atom);
    builder->AddAssertion(alloc->newInfallible<RegExpAssertion>(
        RegExpAssertion::NOT_IN_SURROGATE_PAIR));

    return builder->ToRegExp();
}

// ----------------------------------------------------------------------------
// RegExpParser

template <typename CharT>
RegExpParser<CharT>::RegExpParser(frontend::TokenStreamAnyChars& ts, Zone* zone,
                                  const CharT* chars, const CharT* end, bool multiline,
                                  bool unicode, bool ignore_case)
  : ts(ts),
    zone_(zone),
    captures_(nullptr),
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
void
RegExpParser<CharT>::Advance()
{
    if (next_pos_ < end_) {
        current_ = *next_pos_;
        next_pos_++;
    } else {
        current_ = kEndMarker;
        next_pos_ = end_ + 1;
        has_more_ = false;
    }
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

            if (unicode_ && (group_type == POSITIVE_LOOKAROUND || group_type == NEGATIVE_LOOKAROUND))
                continue;
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
            // Everything except \x0a, \x0d, \u2028 and \u2029
            if (unicode_) {
                builder->AddAtom(UnicodeEverythingAtom(zone(), ignore_case()));
                break;
            }
            CharacterRangeVector* ranges = zone_->newInfallible<CharacterRangeVector>(*zone());
            CharacterRange::AddClassEscape('.', ranges, zone());
            RegExpTree* atom = zone_->newInfallible<RegExpCharacterClass>(ranges);
            builder->AddAtom(atom);
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
            RegExpTree* atom = ParseCharacterClass();
            if (!atom)
                return nullptr;
            builder->AddAtom(atom);
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
              case 'D': case 'S': case 'W':
                if (unicode_) {
                    Advance();
                    builder->AddAtom(UnicodeCharacterClassEscapeAtom(zone(), current(),
                                                                     ignore_case_));
                    Advance();
                    break;
                }
                MOZ_FALLTHROUGH;
              case 'd': case 's': case 'w': {
                widechar c = Next();
                Advance(2);
                CharacterRangeVector* ranges =
                    zone_->newInfallible<CharacterRangeVector>(*zone());
                if (unicode_)
                    CharacterRange::AddClassEscapeUnicode(c, ranges, ignore_case_, zone());
                else
                    CharacterRange::AddClassEscape(c, ranges, zone());
                RegExpTree* atom = zone_->newInfallible<RegExpCharacterClass>(ranges);
                builder->AddAtom(atom);
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
                        if (unicode_)
                            builder->AddAtom(UnicodeBackReferenceAtom(zone(), atom, ignore_case()));
                        else
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
                widechar value;
                if (unicode_) {
                    if (current() == '{') {
                        if (!ParseBracedHexEscape(&value))
                            return nullptr;
                        if (unicode::IsLeadSurrogate(value)) {
                            builder->AddAtom(LeadSurrogateAtom(zone(), value, ignore_case()));
                        } else if (unicode::IsTrailSurrogate(value)) {
                            builder->AddAtom(TrailSurrogateAtom(zone(), value, ignore_case()));
                        } else if (value >= unicode::NonBMPMin) {
                            char16_t lead, trail;
                            unicode::UTF16Encode(value, &lead, &trail);
                            builder->AddAtom(SurrogatePairAtom(zone(), lead, trail,
                                                               ignore_case_));
                        } else {
                            builder->AddCharacter(value);
                        }
                    } else if (ParseHexEscape(4, &value)) {
                        if (unicode::IsLeadSurrogate(value)) {
                            widechar trail;
                            if (ParseTrailSurrogate(&trail)) {
                                builder->AddAtom(SurrogatePairAtom(zone(), value, trail,
                                                                   ignore_case_));
                            } else {
                                builder->AddAtom(LeadSurrogateAtom(zone(), value, ignore_case()));
                            }
                        } else if (unicode::IsTrailSurrogate(value)) {
                            builder->AddAtom(TrailSurrogateAtom(zone(), value, ignore_case()));
                        } else {
                            builder->AddCharacter(value);
                        }
                    } else {
                        return ReportError(JSMSG_INVALID_UNICODE_ESCAPE);
                    }
                    break;
                }
                if (ParseHexEscape(4, &value)) {
                    builder->AddCharacter(value);
                } else {
                    builder->AddCharacter('u');
                }
                break;
              }
              default:
                // Identity escape.
                if (unicode_ && !IsSyntaxCharacterOrSlash(Next()))
                    return ReportError(JSMSG_INVALID_IDENTITY_ESCAPE);
                builder->AddCharacter(Next());
                Advance(2);
                break;
            }
            break;
          case '{': {
            if (unicode_)
                return ReportError(JSMSG_RAW_BRACE_IN_REGEP);
            int dummy;
            bool parsed = ParseIntervalQuantifier(&dummy, &dummy);
            if (parsed) return ReportError(JSMSG_NOTHING_TO_REPEAT);
            MOZ_FALLTHROUGH;
          }
          default:
            if (unicode_) {
                char16_t lead, trail;
                if (ParseRawSurrogatePair(&lead, &trail)) {
                    builder->AddAtom(SurrogatePairAtom(zone(), lead, trail, ignore_case_));
                } else {
                    widechar c = current();
                    if (unicode::IsLeadSurrogate(c))
                        builder->AddAtom(LeadSurrogateAtom(zone(), c, ignore_case()));
                    else if (unicode::IsTrailSurrogate(c))
                        builder->AddAtom(TrailSurrogateAtom(zone(), c, ignore_case()));
                    else if (c == ']')
                        return ReportError(JSMSG_RAW_BRACKET_IN_REGEP);
                    else if (c == '}')
                        return ReportError(JSMSG_RAW_BRACE_IN_REGEP);
                    else
                        builder->AddCharacter(c);
                    Advance();
                }
                break;
            }
            builder->AddCharacter(current());
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
        builder->AddQuantifierToAtom(min, max, quantifier_type);
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
    DCHECK('0' <= current() && current() <= '7');
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
        widechar value;
        if (unicode_) {
            if (current() == '{') {
                if (!ParseBracedHexEscape(&value))
                    return false;
                *code = value;
                return true;
            }
            if (ParseHexEscape(4, &value)) {
                if (unicode::IsLeadSurrogate(value)) {
                    widechar trail;
                    if (ParseTrailSurrogate(&trail)) {
                        *code = unicode::UTF16Decode(value, trail);
                        return true;
                    }
                }
                *code = value;
                return true;
            }
            ReportError(JSMSG_INVALID_UNICODE_ESCAPE);
            return false;
        }
        if (ParseHexEscape(4, &value)) {
            *code = value;
            return true;
        }
        // If \u is not followed by a four-digit or braced hexadecimal, treat it
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
RegExpTree*
RegExpParser<CharT>::ParseCharacterClass()
{
    DCHECK_EQ(current(), '[');
    Advance();
    bool is_negated = false;
    if (current() == '^') {
        is_negated = true;
        Advance();
    }
    CharacterRangeVector* ranges = zone_->newInfallible<CharacterRangeVector>(*zone());
    CharacterRangeVector* lead_ranges = nullptr;
    CharacterRangeVector* trail_ranges = nullptr;
    WideCharRangeVector* wide_ranges = nullptr;

    if (unicode_) {
        lead_ranges = zone_->newInfallible<CharacterRangeVector>(*zone());
        trail_ranges = zone_->newInfallible<CharacterRangeVector>(*zone());
        wide_ranges = zone_->newInfallible<WideCharRangeVector>(*zone());
    }

    while (has_more() && current() != ']') {
        char16_t char_class = kNoCharClass;
        widechar first = 0;
        if (!ParseClassAtom(&char_class, &first))
            return nullptr;
        if (current() == '-') {
            Advance();
            if (current() == kEndMarker) {
                // If we reach the end we break out of the loop and let the
                // following code report an error.
                break;
            } else if (current() == ']') {
                if (unicode_) {
                    AddCharOrEscapeUnicode(zone(), ranges, lead_ranges, trail_ranges, wide_ranges,
                                           char_class, first, ignore_case_);
                } else {
                    AddCharOrEscape(zone(), ranges, char_class, first);
                }
                ranges->append(CharacterRange::Singleton('-'));
                break;
            }
            char16_t char_class_2 = kNoCharClass;
            widechar next = 0;
            if (!ParseClassAtom(&char_class_2, &next))
                return nullptr;
            if (char_class != kNoCharClass || char_class_2 != kNoCharClass) {
                if (unicode_)
                    return ReportError(JSMSG_RANGE_WITH_CLASS_ESCAPE);

                // Either end is an escaped character class. Treat the '-' verbatim.
                AddCharOrEscape(zone(), ranges, char_class, first);
                ranges->append(CharacterRange::Singleton('-'));
                AddCharOrEscape(zone(), ranges, char_class_2, next);
                continue;
            }
            if (first > next)
                return ReportError(JSMSG_BAD_CLASS_RANGE);
            if (unicode_)
                AddUnicodeRange(zone(), ranges, lead_ranges, trail_ranges,wide_ranges, first, next);
            else
                ranges->append(CharacterRange::Range(first, next));
        } else {
            if (unicode_) {
                AddCharOrEscapeUnicode(zone(), ranges, lead_ranges, trail_ranges, wide_ranges,
                                       char_class, first, ignore_case_);
            } else {
                AddCharOrEscape(zone(), ranges, char_class, first);
            }
        }
    }
    if (!has_more())
        return ReportError(JSMSG_UNTERM_CLASS);
    Advance();
    if (!unicode_) {
        if (ranges->length() == 0) {
            ranges->append(CharacterRange::Everything());
            is_negated = !is_negated;
        }
        RegExpCharacterClass::Flags flags;
        if (is_negated) flags = RegExpCharacterClass::NEGATED;
        return zone_->newInfallible<RegExpCharacterClass>(ranges, flags);
    }

    if (!is_negated && ranges->length() == 0 && lead_ranges->length() == 0 &&
        trail_ranges->length() == 0 && wide_ranges->length() == 0)
    {
        ranges->append(CharacterRange::Everything());
        return zone_->newInfallible<RegExpCharacterClass>(ranges, RegExpCharacterClass::NEGATED);
    }

    return UnicodeRangesAtom(zone(), ranges, lead_ranges, trail_ranges, wide_ranges, is_negated,
                             ignore_case_);
}

template <typename CharT>
bool
RegExpParser<CharT>::ParseBracedHexEscape(widechar* value)
{
    MOZ_ASSERT(current() == '{');
    Advance();

    bool first = true;
    uint32_t code = 0;
    while (true) {
        widechar c = current();
        if (c == kEndMarker) {
            ReportError(JSMSG_INVALID_UNICODE_ESCAPE);
            return false;
        }
        if (c == '}') {
            if (first) {
                ReportError(JSMSG_INVALID_UNICODE_ESCAPE);
                return false;
            }
            Advance();
            break;
        }

        int d = HexValue(c);
        if (d < 0) {
            ReportError(JSMSG_INVALID_UNICODE_ESCAPE);
            return false;
        }
        code = (code << 4) | d;
        if (code > unicode::NonBMPMax) {
            ReportError(JSMSG_UNICODE_OVERFLOW, "regular expression");
            return false;
        }
        Advance();
        first = false;
    }

    *value = code;
    return true;
}

template <typename CharT>
bool
RegExpParser<CharT>::ParseTrailSurrogate(widechar* value)
{
    if (current() != '\\')
        return false;

    const CharT* start = position();
    Advance();
    if (current() != 'u') {
        Reset(start);
        return false;
    }
    Advance();
    if (!ParseHexEscape(4, value)) {
        Reset(start);
        return false;
    }
    if (!unicode::IsTrailSurrogate(*value)) {
        Reset(start);
        return false;
    }
    return true;
}

template <typename CharT>
bool
RegExpParser<CharT>::ParseRawSurrogatePair(char16_t* lead, char16_t* trail)
{
    widechar c1 = current();
    if (!unicode::IsLeadSurrogate(c1))
        return false;

    const CharT* start = position();
    Advance();
    widechar c2 = current();
    if (!unicode::IsTrailSurrogate(c2)) {
        Reset(start);
        return false;
    }
    Advance();
    *lead = c1;
    *trail = c2;
    return true;
}

// ----------------------------------------------------------------------------
// RegExpBuilder

RegExpBuilder::RegExpBuilder(Zone* zone, bool ignore_case, bool unicode)
  : zone_(zone),
    pending_empty_(false),
    ignore_case_(ignore_case),
    unicode_(unicode),
    characters_(NULL)
#ifdef DEBUG
  , last_added_(ADD_NONE)
#endif
{}

void RegExpBuilder::FlushCharacters() {
    pending_empty_ = false;
    if (characters_ != NULL) {
        RegExpTree* atom = zone()->newInfallible<RegExpAtom>(characters_);
        characters_ = NULL;
        text_.Add(atom, zone());
#ifdef DEBUG
        last_added_ = ADD_ATOM;
#endif
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
    pending_empty_ = false;
    if (characters_ == NULL) {
        characters_ = zone()->newInfallible<CharacterVector>(*zone());
    }
    characters_->append(c);
#ifdef DEBUG
    last_added_ = ADD_CHAR;
#endif
}

void RegExpBuilder::AddEmpty() { pending_empty_ = true; }

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
#ifdef DEBUG
    last_added_ = ADD_ATOM;
#endif
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
#ifdef DEBUG
    last_added_ = ADD_ASSERT;
#endif
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
#ifdef DEBUG
    last_added_ = ADD_NONE;
#endif
}

RegExpTree* RegExpBuilder::ToRegExp() {
    FlushTerms();
    int num_alternatives = alternatives_.length();
    if (num_alternatives == 0) return RegExpEmpty::GetInstance();
    if (num_alternatives == 1) return alternatives_.last();
    return zone()->newInfallible<RegExpDisjunction>(alternatives_.GetList(zone()));
}

bool RegExpBuilder::AddQuantifierToAtom(int min, int max,
        RegExpQuantifier::QuantifierType quantifier_type) {
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
        if (atom->max_match() == 0) {
            // Guaranteed to only match an empty string.
#ifdef DEBUG
            last_added_ = ADD_TERM;
#endif
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
#ifdef DEBUG
    last_added_ = ADD_TERM;
#endif
    return true;
}

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
