/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99: */

// Copyright 2009 the V8 project authors. All rights reserved.
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

#ifndef V8_REGEXP_TYPES_H_
#define V8_REGEXP_TYPES_H_

#include "jstypes.h"

#include "ds/LifoAlloc.h"
#include "js/Vector.h"

#include "vm/Unicode.h"

namespace js {

namespace jit {
    class JitCode;
}

namespace irregexp {

#define DCHECK(...) MOZ_ASSERT(__VA_ARGS__)
#define DCHECK_EQ(val1, val2) MOZ_ASSERT((val1) == (val2))
#define DCHECK_NE(val1, val2) MOZ_ASSERT((val1) != (val2))
#define DCHECK_LT(val1, val2) MOZ_ASSERT((val1) <  (val2))
#define DCHECK_LE(val1, val2) MOZ_ASSERT((val1) <= (val2))
#define DCHECK_GT(val1, val2) MOZ_ASSERT((val1) >  (val2))
#define DCHECK_GE(val1, val2) MOZ_ASSERT((val1) >= (val2))
#define DCHECK_IMPLIES(...) MOZ_ASSERT_IF(__VA_ARGS__)
#define DCHECK_NULL(expr) MOZ_ASSERT((expr) == nullptr)
#define DCHECK_NOT_NULL(expr) MOZ_ASSERT((expr) != nullptr)

#define DISALLOW_IMPLICIT_CONSTRUCTORS(ClassName) \
    ClassName() = default; \
    ClassName(const ClassName&) = default; \
    ClassName& operator=(const ClassName&) = delete;

using byte = uint8_t;
using uc16 = uint16_t;
using uc32 = int32_t;

constexpr int32_t kMinInt = INT32_MIN;
constexpr int32_t kMaxInt = INT32_MAX;

namespace String {
    constexpr int32_t kMaxOneByteCharCode = 0xff;
    constexpr uint32_t kMaxOneByteCharCodeU = 0xff;
    constexpr int32_t kMaxUtf16CodeUnit = 0xffff;
    constexpr int32_t kMaxCodePoint = 0x10ffff;
}

namespace unibrow {
    using uchar = char16_t;

    namespace Ecma262UnCanonicalize {
        constexpr size_t kMaxWidth = 4;
    }

    namespace Utf16 {
        static const uint32_t kMaxNonSurrogateCharCode = 0xffff;

        static constexpr auto& IsLeadSurrogate = unicode::IsLeadSurrogate;
        static constexpr auto& IsTrailSurrogate = unicode::IsTrailSurrogate;
        static constexpr auto& LeadSurrogate = unicode::LeadSurrogate;
        static constexpr auto& TrailSurrogate = unicode::TrailSurrogate;
        static constexpr auto& CombineSurrogatePair = unicode::UTF16Decode;
    }
}

using Zone = LifoAlloc;

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

    int length() const { return vector_.length(); }
    T popCopy() { return vector_.popCopy(); }

    T* begin() { return vector_.begin(); }
    const T* begin() const { return vector_.begin(); }

    T& operator[](size_t index) { return vector_[index]; }
    const T& operator[](size_t index) const { return vector_[index]; }

    // Alternative accessors to match V8's Vector API.
    T& at(size_t index) { return vector_[index]; }
    const T& at(size_t index) const { return vector_[index]; }

    T& last() {
        MOZ_ASSERT(length() > 0);
        return at(length() - 1);
    }
    const T& last() const {
        MOZ_ASSERT(length() > 0);
        return at(length() - 1);
    }

    bool contains(const T& value) const {
        for (size_t i = 0, len = static_cast<size_t>(length()); i < len; i++) {
            if (at(i) == value)
                return true;
        }
        return false;
    }

    InfallibleVector& operator=(InfallibleVector&& rhs) { vector_ = Move(rhs.vector_); return *this; }
};

// TODO(anba): Maybe move to different file?
struct RegExpCode
{
    jit::JitCode* jitCode;
    uint8_t* byteCode;

    RegExpCode()
      : jitCode(nullptr), byteCode(nullptr)
    {}

    bool empty() {
        return !jitCode && !byteCode;
    }

    void destroy() {
        js_free(byteCode);
    }
};

}}  // namespace js::irregexp

#endif  // V8_REGEXP_TYPES_H_
