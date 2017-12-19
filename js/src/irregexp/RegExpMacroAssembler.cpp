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

#include "irregexp/RegExpMacroAssembler.h"

using namespace js;
using namespace js::irregexp;

using jit::Label;

template <typename CharT>
int
irregexp::CaseInsensitiveCompareStrings(const CharT* substring1, const CharT* substring2,
                                        size_t byteLength)
{
    AutoUnsafeCallWithABI unsafe;

    MOZ_ASSERT(byteLength % sizeof(CharT) == 0);
    size_t length = byteLength / sizeof(CharT);

    for (size_t i = 0; i < length; i++) {
        char16_t c1 = substring1[i];
        char16_t c2 = substring2[i];
        if (c1 != c2) {
            c1 = unicode::ToLowerCase(c1);
            c2 = unicode::ToLowerCase(c2);
            if (c1 != c2)
                return 0;
        }
    }

    return 1;
}

template int
irregexp::CaseInsensitiveCompareStrings(const Latin1Char* substring1, const Latin1Char* substring2,
                                        size_t byteLength);

template int
irregexp::CaseInsensitiveCompareStrings(const char16_t* substring1, const char16_t* substring2,
                                        size_t byteLength);

template <typename CharT>
int
irregexp::CaseInsensitiveCompareUCStrings(const CharT* substring1, const CharT* substring2,
                                          size_t byteLength)
{
    AutoUnsafeCallWithABI unsafe;

    MOZ_ASSERT(byteLength % sizeof(CharT) == 0);
    size_t length = byteLength / sizeof(CharT);

    for (size_t i = 0; i < length; i++) {
        char16_t c1 = substring1[i];
        char16_t c2 = substring2[i];
        if (c1 != c2) {
            c1 = unicode::FoldCase(c1);
            c2 = unicode::FoldCase(c2);
            if (c1 != c2)
                return 0;
        }
    }

    return 1;
}

template int
irregexp::CaseInsensitiveCompareUCStrings(const Latin1Char* substring1,
                                          const Latin1Char* substring2,
                                          size_t byteLength);

template int
irregexp::CaseInsensitiveCompareUCStrings(const char16_t* substring1,
                                          const char16_t* substring2,
                                          size_t byteLength);

void RegExpMacroAssembler::CheckNotInSurrogatePair(int cp_offset,
                                                   Label* on_failure) {
    Label ok;
    // Check that current character is not a trail surrogate.
    LoadCurrentCharacter(cp_offset, &ok);
    CheckCharacterNotInRange(kTrailSurrogateStart, kTrailSurrogateEnd, &ok);
    // Check that previous character is not a lead surrogate.
    LoadCurrentCharacter(cp_offset - 1, &ok);
    CheckCharacterInRange(kLeadSurrogateStart, kLeadSurrogateEnd, on_failure);
    Bind(&ok);
}

void RegExpMacroAssembler::CheckPosition(int cp_offset,
                                         Label* on_outside_input) {
    LoadCurrentCharacter(cp_offset, on_outside_input, true);
}

bool RegExpMacroAssembler::CheckSpecialCharacterClass(uc16 type,
                                                      Label* on_no_match) {
    return false;
}
