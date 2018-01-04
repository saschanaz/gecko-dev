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

#ifndef V8_PARSER_H_
#define V8_PARSER_H_

#include "mozilla/Range.h"

#include <stdarg.h>

#include "irregexp/RegExpAST.h"

namespace js {

namespace frontend {
    class TokenStreamAnyChars;
}

namespace irregexp {

struct RegExpCompileData;

extern bool
ParsePattern(frontend::TokenStreamAnyChars& ts, LifoAlloc& alloc, JSAtom* str,
             bool multiline, bool match_only, bool unicode, bool ignore_case,
             bool global, bool sticky, RegExpCompileData* data);

extern bool
ParsePatternSyntax(frontend::TokenStreamAnyChars& ts, LifoAlloc& alloc, JSAtom* str,
                   bool unicode);

extern bool
ParsePatternSyntax(frontend::TokenStreamAnyChars& ts, LifoAlloc& alloc,
                   const mozilla::Range<const char16_t> chars, bool unicode);

// A BufferedVector is an automatically growing list, just like (and backed
// by) a Vector, that is optimized for the case of adding and removing
// a single element. The last element added is stored outside the backing list,
// and if no more than one element is ever added, the ZoneList isn't even
// allocated.
// Elements must not be nullptr pointers.
template <typename T, int initial_size>
class BufferedVector {
  public:
    typedef InfallibleVector<T*, 1> VectorType;

    BufferedVector() : list_(nullptr), last_(nullptr) {}

    // Adds element at end of list. This element is buffered and can
    // be read using last() or removed using RemoveLast until a new Add or until
    // RemoveLast or GetList has been called.
    void Add(T* value, Zone* zone) {
        if (last_ != nullptr) {
            if (list_ == nullptr) {
                list_ = zone->newInfallible<VectorType>(*zone);
                list_->reserve(initial_size);
            }
            list_->append(last_);
        }
        last_ = value;
    }

    T* last() {
        DCHECK(last_ != nullptr);
        return last_;
    }

    T* RemoveLast() {
        DCHECK(last_ != nullptr);
        T* result = last_;
        if ((list_ != nullptr) && (list_->length() > 0))
            last_ = list_->popCopy();
        else
            last_ = nullptr;
        return result;
    }

    T* Get(int i) {
        DCHECK((0 <= i) && (i < length()));
        if (list_ == nullptr) {
            DCHECK_EQ(0, i);
            return last_;
        } else {
            if (i == list_->length()) {
                DCHECK(last_ != nullptr);
                return last_;
            } else {
                return list_->at(i);
            }
        }
    }

    void Clear() {
        list_ = nullptr;
        last_ = nullptr;
    }

    int length() {
        int length = (list_ == nullptr) ? 0 : list_->length();
        return length + ((last_ == nullptr) ? 0 : 1);
    }

    VectorType* GetList(Zone* zone) {
        if (list_ == nullptr) {
            list_ = zone->newInfallible<VectorType>(*zone);
        }
        if (last_ != nullptr) {
            list_->append(last_);
            last_ = nullptr;
        }
        return list_;
    }

  private:
    VectorType* list_;
    T* last_;
};


// Accumulates RegExp atoms and assertions into lists of terms and alternatives.
class RegExpBuilder {
  public:
    RegExpBuilder(Zone* zone, JSRegExp::Flags flags);
    void AddCharacter(uc16 character);
    void AddUnicodeCharacter(uc32 character);
    void AddEscapedUnicodeCharacter(uc32 character);
    // "Adds" an empty expression. Does nothing except consume a
    // following quantifier
    void AddEmpty();
    void AddCharacterClass(RegExpCharacterClass* cc);
    void AddCharacterClassForDesugaring(uc32 c);
    void AddAtom(RegExpTree* tree);
    void AddTerm(RegExpTree* tree);
    void AddAssertion(RegExpTree* tree);
    void NewAlternative();  // '|'
    bool AddQuantifierToAtom(int min, int max,
                             RegExpQuantifier::QuantifierType type);
    void FlushText();
    RegExpTree* ToRegExp();
    JSRegExp::Flags flags() const { return flags_; }
    void set_flags(JSRegExp::Flags flags) { flags_ = flags; }

    bool ignore_case() const { return flags_.contains(JSRegExp::kIgnoreCase); }
    bool multiline() const { return flags_.contains(JSRegExp::kMultiline); }
    bool dotall() const { return flags_.contains(JSRegExp::kDotAll); }

  private:
    static const uc16 kNoPendingSurrogate = 0;
    void AddLeadSurrogate(uc16 lead_surrogate);
    void AddTrailSurrogate(uc16 trail_surrogate);
    void FlushPendingSurrogate();
    void FlushCharacters();
    void FlushTerms();
    bool NeedsDesugaringForUnicode(RegExpCharacterClass* cc);
    bool NeedsDesugaringForIgnoreCase(uc32 c);
    Zone* zone() const { return zone_; }
    bool unicode() const { return flags_.contains(JSRegExp::kUnicode); }

    Zone* zone_;
    bool pending_empty_;
    JSRegExp::Flags flags_;
    CharacterVector* characters_;
    uc16 pending_surrogate_;
    BufferedVector<RegExpTree, 2> terms_;
    BufferedVector<RegExpTree, 2> text_;
    BufferedVector<RegExpTree, 2> alternatives_;

#ifdef DEBUG
    enum { ADD_NONE, ADD_CHAR, ADD_TERM, ADD_ASSERT, ADD_ATOM } last_added_;
#endif
};

template <typename CharT>
class RegExpParser {
  public:
    RegExpParser(frontend::TokenStreamAnyChars& ts, Zone* zone,
                 const CharT* chars, const CharT* end,
                 JSRegExp::Flags flags);

    RegExpTree* ParsePattern();
    RegExpTree* ParseDisjunction();
    RegExpTree* ParseGroup();

    // Parses a {...,...} quantifier and stores the range in the given
    // out parameters.
    bool ParseIntervalQuantifier(int* min_out, int* max_out);

    // Tries to parse the input as a single escaped character.  If successful
    // it stores the result in the output parameter and returns true.
    // Otherwise it throws an error and returns false.  The character must not
    // be 'b' or 'B' since they are usually handled specially.
    bool ParseClassCharacterEscape(uc32* code);

    // Checks whether the following is a length-digit hexadecimal number,
    // and sets the value if it is.
    bool ParseHexEscape(int length, uc32* value);
    bool ParseUnicodeEscape(uc32* value, bool* parsed);
    bool ParseUnlimitedLengthHexNumber(int max_value, uc32* value);
    RegExpTree* ParseCharacterClass(const RegExpBuilder* state);

    uc32 ParseOctalLiteral();

    // Tries to parse the input as a back reference.  If successful it
    // stores the result in the output parameter and returns true.  If
    // it fails it will push back the characters read so the same characters
    // can be reparsed.
    bool ParseBackReferenceIndex(int* index_out);

    // Parse inside a class. Either add escaped class to the range, or return
    // false and pass parsed single character through |char_out|.
    bool ParseClassEscape(CharacterRangeVector* ranges, Zone* zone,
                          bool add_unicode_case_equivalents, uc32* char_out,
                          bool* is_class_escape);

    char ParseClassEscape();

  private:
    void SyntaxError(unsigned errorNumber, ...);

  public:
    RegExpTree* ReportError(unsigned errorNumber, const char* param = nullptr);

    void Advance();
    void Advance(int dist);
    void Reset(const CharT* pos);

    // Reports whether the pattern might be used as a literal search string.
    // Only use if the result of the parse is a single atom node.
    bool simple();
    bool contains_anchor() { return contains_anchor_; }
    void set_contains_anchor() { contains_anchor_ = true; }
    int captures_started() { return captures_started_; }
    const CharT* position() { return next_pos_ - 1; }
    // The Unicode flag can't be changed using in-regexp syntax, so it's OK to
    // just read the initial flag value here.
    bool unicode() const { return top_level_flags_.contains(JSRegExp::kUnicode); }

    static bool IsSyntaxCharacterOrSlash(uc32 c);

    static const int kMaxCaptures = 1 << 16;
    static const uc32 kEndMarker = (1 << 21);

  private:
    enum SubexpressionType {
        INITIAL,
        CAPTURE,  // All positive values represent captures.
        POSITIVE_LOOKAROUND,
        NEGATIVE_LOOKAROUND,
        GROUPING
    };

    class RegExpParserState {
      public:
        // Push a state on the stack.
        RegExpParserState(RegExpParserState* previous_state,
                          SubexpressionType group_type,
                          RegExpLookaround::Type lookaround_type,
                          int disjunction_capture_index,
                          JSRegExp::Flags flags, Zone* zone)
            : previous_state_(previous_state),
              builder_(zone->newInfallible<RegExpBuilder>(zone, flags)),
              group_type_(group_type),
              lookaround_type_(lookaround_type),
              disjunction_capture_index_(disjunction_capture_index)
        {}
        // Parser state of containing expression, if any.
        RegExpParserState* previous_state() const { return previous_state_; }
        bool IsSubexpression() { return previous_state_ != nullptr; }
        // RegExpBuilder building this regexp's AST.
        RegExpBuilder* builder() const { return builder_; }
        // Type of regexp being parsed (parenthesized group or entire regexp).
        SubexpressionType group_type() const { return group_type_; }
        // Lookahead or Lookbehind.
        RegExpLookaround::Type lookaround_type() const { return lookaround_type_; }
        // Index in captures array of first capture in this sub-expression, if any.
        // Also the capture index of this sub-expression itself, if group_type
        // is CAPTURE.
        int capture_index() const { return disjunction_capture_index_; }

        // Check whether the parser is inside a capture group with the given index.
        bool IsInsideCaptureGroup(int index);

      private:
        // Linked list implementation of stack of states.
        RegExpParserState* const previous_state_;
        // Builder for the stored disjunction.
        RegExpBuilder* const builder_;
        // Stored disjunction type (capture, look-ahead or grouping), if any.
        const SubexpressionType group_type_;
        // Stored read direction.
        const RegExpLookaround::Type lookaround_type_;
        // Stored disjunction's capture index (if any).
        const int disjunction_capture_index_;
    };

    // Return the 1-indexed RegExpCapture object, allocate if necessary.
    RegExpCapture* GetCapture(int index);

    RegExpParserState* ParseOpenParenthesis(RegExpParserState* state);

    Zone* zone() const { return zone_; }

    uc32 current() { return current_; }
    bool has_more() { return has_more_; }
    bool has_next() { return next_pos_ < end_; }
    uc32 Next();
    template <bool update_position>
    uc32 ReadNext();
    void ScanForCaptures();

    frontend::TokenStreamAnyChars& ts;
    Zone* zone_;
    RegExpCaptureVector* captures_;
    const CharT* const start_;
    const CharT* next_pos_;
    const CharT* end_;
    uc32 current_;
    // These are the flags specified outside the regexp syntax ie after the
    // terminating '/' or in the second argument to the constructor.  The current
    // flags are stored on the RegExpBuilder.
    JSRegExp::Flags top_level_flags_;
    int captures_started_;
    int capture_count_;  // Only valid after we have scanned for captures.
    bool has_more_;
    bool simple_;
    bool contains_anchor_;
    bool is_scanned_for_captures_;
};

} } // namespace js::irregexp

#endif // V8_PARSER_H_
