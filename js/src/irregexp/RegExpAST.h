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

#ifndef V8_REGEXP_AST_H_
#define V8_REGEXP_AST_H_

#include "ds/LifoAlloc.h"
#include "js/Vector.h"

// Prevent msvc build failures as indicated in bug 1205328
#ifdef min
# undef min
#endif
#ifdef max
# undef max
#endif

namespace js {
namespace irregexp {

#define FOR_EACH_REG_EXP_TREE_TYPE(VISIT)                            \
  VISIT(Disjunction)                                                 \
  VISIT(Alternative)                                                 \
  VISIT(Assertion)                                                   \
  VISIT(CharacterClass)                                              \
  VISIT(Atom)                                                        \
  VISIT(Quantifier)                                                  \
  VISIT(Capture)                                                     \
  VISIT(Lookahead)                                                   \
  VISIT(BackReference)                                               \
  VISIT(Empty)                                                       \
  VISIT(Text)

#define FORWARD_DECLARE(Name) class RegExp##Name;
FOR_EACH_REG_EXP_TREE_TYPE(FORWARD_DECLARE)
#undef FORWARD_DECLARE

class RegExpCompiler;
class RegExpNode;
class RegExpTree;

class RegExpVisitor
{
  public:
    virtual ~RegExpVisitor() { }
#define MAKE_CASE(Name)                                         \
    virtual void* Visit##Name(RegExp##Name*, void* data) = 0;
    FOR_EACH_REG_EXP_TREE_TYPE(MAKE_CASE)
#undef MAKE_CASE
};

// InfallibleVector is like Vector, but all its methods are infallible (they
// crash on OOM). We use this class instead of Vector to avoid a ton of
// MOZ_MUST_USE warnings in irregexp code (imported from V8).
template<typename T, size_t N>
class InfallibleVector
{
    Vector<T, N, LifoAllocPolicy<Infallible>> vector_;

    InfallibleVector(const InfallibleVector&) = delete;
    void operator=(const InfallibleVector&) = delete;

  public:
    explicit InfallibleVector(const LifoAllocPolicy<Infallible>& alloc) : vector_(alloc) {}

    void append(const T& t) { MOZ_ALWAYS_TRUE(vector_.append(t)); }
    void append(const T* begin, size_t length) { MOZ_ALWAYS_TRUE(vector_.append(begin, length)); }

    void clear() { vector_.clear(); }
    void popBack() { vector_.popBack(); }
    void reserve(size_t n) { MOZ_ALWAYS_TRUE(vector_.reserve(n)); }

    size_t length() const { return vector_.length(); }
    T popCopy() { return vector_.popCopy(); }

    T* begin() { return vector_.begin(); }
    const T* begin() const { return vector_.begin(); }

    T& operator[](size_t index) { return vector_[index]; }
    const T& operator[](size_t index) const { return vector_[index]; }

    InfallibleVector& operator=(InfallibleVector&& rhs) { vector_ = Move(rhs.vector_); return *this; }
};

// A simple closed interval.
class Interval
{
  public:
    Interval() : from_(kNone), to_(kNone) { }

    Interval(int from, int to) : from_(from), to_(to) { }

    Interval Union(Interval that) {
        if (that.from_ == kNone)
            return *this;
        else if (from_ == kNone)
            return that;
        else
            return Interval(Min(from_, that.from_), Max(to_, that.to_));
    }

    bool Contains(int value) {
        return (from_ <= value) && (value <= to_);
    }

    bool is_empty() { return from_ == kNone; }

    int from() const { return from_; }
    int to() const { return to_; }

    static Interval Empty() { return Interval(); }
    static const int kNone = -1;

  private:
    int from_;
    int to_;
};

class CharacterRange;
typedef InfallibleVector<CharacterRange, 1> CharacterRangeVector;

// Represents code units in the range from from_ to to_, both ends are
// inclusive.
class CharacterRange
{
  public:
    CharacterRange()
      : from_(0), to_(0)
    {}

    CharacterRange(char16_t from, char16_t to)
      : from_(from), to_(to)
    {}

    static void AddClassEscape(LifoAlloc* alloc, char16_t type, CharacterRangeVector* ranges);
    static void AddClassEscapeUnicode(LifoAlloc* alloc, char16_t type,
                                      CharacterRangeVector* ranges, bool ignoreCase);

    static inline CharacterRange Singleton(char16_t value) {
        return CharacterRange(value, value);
    }
    static inline CharacterRange Range(char16_t from, char16_t to) {
        MOZ_ASSERT(from <= to);
        return CharacterRange(from, to);
    }
    static inline CharacterRange Everything() {
        return CharacterRange(0, 0xFFFF);
    }
    bool Contains(char16_t i) { return from_ <= i && i <= to_; }
    char16_t from() const { return from_; }
    void set_from(char16_t value) { from_ = value; }
    char16_t to() const { return to_; }
    void set_to(char16_t value) { to_ = value; }
    bool is_valid() { return from_ <= to_; }
    bool IsEverything(char16_t max) { return from_ == 0 && to_ >= max; }
    bool IsSingleton() { return (from_ == to_); }
    void AddCaseEquivalents(bool is_latin1, bool unicode, CharacterRangeVector* ranges);

    static void Split(const LifoAlloc* alloc,
                      CharacterRangeVector base,
                      const Vector<int>& overlay,
                      CharacterRangeVector* included,
                      CharacterRangeVector* excluded);

    // Whether a range list is in canonical form: Ranges ordered by from value,
    // and ranges non-overlapping and non-adjacent.
    static bool IsCanonical(const CharacterRangeVector& ranges);

    // Convert range list to canonical form. The characters covered by the ranges
    // will still be the same, but no character is in more than one range, and
    // adjacent ranges are merged. The resulting list may be shorter than the
    // original, but cannot be longer.
    static void Canonicalize(CharacterRangeVector& ranges);

    // Negate the contents of a character range in canonical form.
    static void Negate(const LifoAlloc* alloc,
                       CharacterRangeVector src,
                       CharacterRangeVector* dst);

    static const int kStartMarker = (1 << 24);
    static const int kPayloadMask = (1 << 24) - 1;

  private:
    char16_t from_;
    char16_t to_;
};

class CharacterSet
{
  public:
    explicit CharacterSet(char16_t standard_set_type)
      : ranges_(nullptr),
        standard_set_type_(standard_set_type)
    {}
    explicit CharacterSet(CharacterRangeVector* ranges)
      : ranges_(ranges),
        standard_set_type_(0)
    {}

    CharacterRangeVector& ranges(LifoAlloc* alloc);
    char16_t standard_set_type() { return standard_set_type_; }
    void set_standard_set_type(char16_t special_set_type) {
        standard_set_type_ = special_set_type;
    }
    bool is_standard() { return standard_set_type_ != 0; }
    void Canonicalize();

  private:
    CharacterRangeVector* ranges_;

    // If non-zero, the value represents a standard set (e.g., all whitespace
    // characters) without having to expand the ranges.
    char16_t standard_set_type_;
};

class TextElement
{
  public:
    enum TextType {
        ATOM,
        CHAR_CLASS
    };

    static TextElement Atom(RegExpAtom* atom);
    static TextElement CharClass(RegExpCharacterClass* char_class);

    int cp_offset() const { return cp_offset_; }
    void set_cp_offset(int cp_offset) { cp_offset_ = cp_offset; }
    int length() const;

    TextType text_type() const { return text_type_; }

    RegExpTree* tree() const { return tree_; }

    RegExpAtom* atom() const {
        MOZ_ASSERT(text_type() == ATOM);
        return reinterpret_cast<RegExpAtom*>(tree());
    }

    RegExpCharacterClass* char_class() const {
        MOZ_ASSERT(text_type() == CHAR_CLASS);
        return reinterpret_cast<RegExpCharacterClass*>(tree());
    }

  private:
    TextElement(TextType text_type, RegExpTree* tree)
      : cp_offset_(-1), text_type_(text_type), tree_(tree)
    {}

    int cp_offset_;
    TextType text_type_;
    RegExpTree* tree_;
};

typedef InfallibleVector<TextElement, 1> TextElementVector;

class RegExpTree
{
  public:
    static const int kInfinity = INT32_MAX;
    virtual ~RegExpTree() {}
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success) = 0;
    virtual bool IsTextElement() { return false; }
    virtual bool IsAnchoredAtStart() { return false; }
    virtual bool IsAnchoredAtEnd() { return false; }
    virtual int min_match() = 0;
    virtual int max_match() = 0;
    // Returns the interval of registers used for captures within this
    // expression.
    virtual Interval CaptureRegisters() { return Interval::Empty(); }
    virtual void AppendToText(RegExpText* text) {
        MOZ_CRASH("Bad call");
    }
#define MAKE_ASTYPE(Name)                                               \
    virtual RegExp##Name* As##Name();                                   \
    virtual bool Is##Name();
    FOR_EACH_REG_EXP_TREE_TYPE(MAKE_ASTYPE)
#undef MAKE_ASTYPE
};

typedef InfallibleVector<RegExpTree*, 1> RegExpTreeVector;

class RegExpDisjunction : public RegExpTree
{
  public:
    explicit RegExpDisjunction(RegExpTreeVector* alternatives);
    virtual void* Accept(RegExpVisitor* visitor, void* data);
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success);
    virtual RegExpDisjunction* AsDisjunction();
    virtual Interval CaptureRegisters();
    virtual bool IsDisjunction();
    virtual bool IsAnchoredAtStart();
    virtual bool IsAnchoredAtEnd();
    virtual int min_match() { return min_match_; }
    virtual int max_match() { return max_match_; }

    const RegExpTreeVector& alternatives() { return *alternatives_; }

  private:
    RegExpTreeVector* alternatives_;
    int min_match_;
    int max_match_;
};

class RegExpAlternative : public RegExpTree
{
  public:
    explicit RegExpAlternative(RegExpTreeVector* nodes);
    virtual void* Accept(RegExpVisitor* visitor, void* data);
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success);
    virtual RegExpAlternative* AsAlternative();
    virtual Interval CaptureRegisters();
    virtual bool IsAlternative();
    virtual bool IsAnchoredAtStart();
    virtual bool IsAnchoredAtEnd();
    virtual int min_match() { return min_match_; }
    virtual int max_match() { return max_match_; }

    const RegExpTreeVector& nodes() { return *nodes_; }

  private:
    RegExpTreeVector* nodes_;
    int min_match_;
    int max_match_;
};

class RegExpAssertion : public RegExpTree {
 public:
  enum AssertionType {
    START_OF_LINE,
    START_OF_INPUT,
    END_OF_LINE,
    END_OF_INPUT,
    BOUNDARY,
    NON_BOUNDARY,
    NOT_AFTER_LEAD_SURROGATE,
    NOT_IN_SURROGATE_PAIR
  };
  explicit RegExpAssertion(AssertionType type) : assertion_type_(type) { }
  virtual void* Accept(RegExpVisitor* visitor, void* data);
  virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                             RegExpNode* on_success);
  virtual RegExpAssertion* AsAssertion();
  virtual bool IsAssertion();
  virtual bool IsAnchoredAtStart();
  virtual bool IsAnchoredAtEnd();
  virtual int min_match() { return 0; }
  virtual int max_match() { return 0; }
  AssertionType assertion_type() { return assertion_type_; }
 private:
  AssertionType assertion_type_;
};

class RegExpCharacterClass : public RegExpTree
{
  public:
    RegExpCharacterClass(CharacterRangeVector* ranges, bool is_negated)
      : set_(ranges),
        is_negated_(is_negated)
    {}

    explicit RegExpCharacterClass(char16_t type)
      : set_(type),
        is_negated_(false)
    {}

    virtual void* Accept(RegExpVisitor* visitor, void* data);
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success);
    virtual RegExpCharacterClass* AsCharacterClass();
    virtual bool IsCharacterClass();
    virtual bool IsTextElement() { return true; }
    virtual int min_match() { return 1; }
    virtual int max_match() { return 1; }
    virtual void AppendToText(RegExpText* text);

    CharacterSet character_set() { return set_; }

    // TODO(lrn): Remove need for complex version if is_standard that
    // recognizes a mangled standard set and just do { return set_.is_special(); }
    bool is_standard(LifoAlloc* alloc);

    // Returns a value representing the standard character set if is_standard()
    // returns true.
    // Currently used values are:
    // s : unicode whitespace
    // S : unicode non-whitespace
    // w : ASCII word character (digit, letter, underscore)
    // W : non-ASCII word character
    // d : ASCII digit
    // D : non-ASCII digit
    // . : non-unicode non-newline
    // * : All characters
    char16_t standard_type() { return set_.standard_set_type(); }

    CharacterRangeVector& ranges(LifoAlloc* alloc) { return set_.ranges(alloc); }
    bool is_negated() { return is_negated_; }

  private:
    CharacterSet set_;
    bool is_negated_;
};

typedef InfallibleVector<char16_t, 10> CharacterVector;

class RegExpAtom : public RegExpTree
{
  public:
    explicit RegExpAtom(CharacterVector* data)
      : data_(data)
    {}

    virtual void* Accept(RegExpVisitor* visitor, void* data);
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success);
    virtual RegExpAtom* AsAtom();
    virtual bool IsAtom();
    virtual bool IsTextElement() { return true; }
    virtual int min_match() { return data_->length(); }
    virtual int max_match() { return data_->length(); }
    virtual void AppendToText(RegExpText* text);

    const CharacterVector& data() { return *data_; }
    int length() { return data_->length(); }

  private:
    CharacterVector* data_;
};

class RegExpText : public RegExpTree
{
  public:
    explicit RegExpText(LifoAlloc* alloc)
      : elements_(*alloc), length_(0)
    {}

    virtual void* Accept(RegExpVisitor* visitor, void* data);
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success);
    virtual RegExpText* AsText();
    virtual bool IsText();
    virtual bool IsTextElement() { return true; }
    virtual int min_match() { return length_; }
    virtual int max_match() { return length_; }
    virtual void AppendToText(RegExpText* text);

    void AddElement(TextElement elm)  {
        elements_.append(elm);
        length_ += elm.length();
    }
    const TextElementVector& elements() { return elements_; }

  private:
    TextElementVector elements_;
    int length_;
};

class RegExpQuantifier : public RegExpTree
{
  public:
    enum QuantifierType { GREEDY, NON_GREEDY, POSSESSIVE };
    RegExpQuantifier(int min, int max, QuantifierType type, RegExpTree* body)
      : body_(body),
        min_(min),
        max_(max),
        min_match_(min * body->min_match()),
        quantifier_type_(type)
    {
        if (max > 0 && body->max_match() > kInfinity / max) {
            max_match_ = kInfinity;
        } else {
            max_match_ = max * body->max_match();
        }
    }

    virtual void* Accept(RegExpVisitor* visitor, void* data);
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success);
    static RegExpNode* ToNode(int min,
                              int max,
                              bool is_greedy,
                              RegExpTree* body,
                              RegExpCompiler* compiler,
                              RegExpNode* on_success,
                              bool not_at_start = false);
    virtual RegExpQuantifier* AsQuantifier();
    virtual Interval CaptureRegisters();
    virtual bool IsQuantifier();
    virtual int min_match() { return min_match_; }
    virtual int max_match() { return max_match_; }
    int min() { return min_; }
    int max() { return max_; }
    bool is_possessive() { return quantifier_type_ == POSSESSIVE; }
    bool is_non_greedy() { return quantifier_type_ == NON_GREEDY; }
    bool is_greedy() { return quantifier_type_ == GREEDY; }
    RegExpTree* body() { return body_; }

  private:
    RegExpTree* body_;
    int min_;
    int max_;
    int min_match_;
    int max_match_;
    QuantifierType quantifier_type_;
};

class RegExpCapture : public RegExpTree
{
  public:
    explicit RegExpCapture(RegExpTree* body, int index)
      : body_(body), index_(index)
    {}

    virtual void* Accept(RegExpVisitor* visitor, void* data);
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success);
    static RegExpNode* ToNode(RegExpTree* body,
                              int index,
                              RegExpCompiler* compiler,
                              RegExpNode* on_success);
    virtual RegExpCapture* AsCapture();
    virtual bool IsAnchoredAtStart();
    virtual bool IsAnchoredAtEnd();
    virtual Interval CaptureRegisters();
    virtual bool IsCapture();
    virtual int min_match() { return body_->min_match(); }
    virtual int max_match() { return body_->max_match(); }
    RegExpTree* body() { return body_; }
    int index() { return index_; }
    static int StartRegister(int index) { return index * 2; }
    static int EndRegister(int index) { return index * 2 + 1; }

  private:
    RegExpTree* body_;
    int index_;
};

class RegExpLookahead : public RegExpTree
{
  public:
    RegExpLookahead(RegExpTree* body,
                    bool is_positive,
                    int capture_count,
                    int capture_from)
      : body_(body),
        is_positive_(is_positive),
        capture_count_(capture_count),
        capture_from_(capture_from)
    {}

    virtual void* Accept(RegExpVisitor* visitor, void* data);
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success);
    virtual RegExpLookahead* AsLookahead();
    virtual Interval CaptureRegisters();
    virtual bool IsLookahead();
    virtual bool IsAnchoredAtStart();
    virtual int min_match() { return 0; }
    virtual int max_match() { return 0; }
    RegExpTree* body() { return body_; }
    bool is_positive() { return is_positive_; }
    int capture_count() { return capture_count_; }
    int capture_from() { return capture_from_; }

  private:
    RegExpTree* body_;
    bool is_positive_;
    int capture_count_;
    int capture_from_;
};

typedef InfallibleVector<RegExpCapture*, 1> RegExpCaptureVector;

class RegExpBackReference : public RegExpTree
{
  public:
    explicit RegExpBackReference(RegExpCapture* capture)
      : capture_(capture)
    {}

    virtual void* Accept(RegExpVisitor* visitor, void* data);
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success);
    virtual RegExpBackReference* AsBackReference();
    virtual bool IsBackReference();
    virtual int min_match() { return 0; }
    virtual int max_match() { return capture_->max_match(); }
    int index() { return capture_->index(); }
    RegExpCapture* capture() { return capture_; }
  private:
    RegExpCapture* capture_;
};

class RegExpEmpty : public RegExpTree
{
  public:
    RegExpEmpty()
    {}

    virtual void* Accept(RegExpVisitor* visitor, void* data);
    virtual RegExpNode* ToNode(RegExpCompiler* compiler,
                               RegExpNode* on_success);
    virtual RegExpEmpty* AsEmpty();
    virtual bool IsEmpty();
    virtual int min_match() { return 0; }
    virtual int max_match() { return 0; }
    static RegExpEmpty* GetInstance() {
        static RegExpEmpty instance;
        return &instance;
    }
};

} } // namespace js::irregexp

#endif  // V8_REGEXP_AST_H_
