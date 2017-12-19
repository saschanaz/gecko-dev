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

#include "irregexp/InterpretedRegExpMacroAssembler-inl.h"

#include "irregexp/RegExpBytecode.h"

using namespace js;
using namespace js::irregexp;
using namespace js::jit;

using jit::Label;

static const int32_t INVALID_OFFSET = -1;

InterpretedRegExpMacroAssembler::InterpretedRegExpMacroAssembler(JSContext* cx, LifoAlloc* alloc,
                                                                 size_t numSavedRegisters)
  : RegExpMacroAssembler(cx, *alloc, numSavedRegisters),
    buffer_(nullptr),
    length_(0),
    pc_(0),
    advance_current_start_(0),
    advance_current_offset_(0),
    advance_current_end_(kInvalidPC)
{
    // The first int32 word is the number of registers.
    Emit32(0);
}

InterpretedRegExpMacroAssembler::~InterpretedRegExpMacroAssembler() {
    js_free(buffer_);
}

void InterpretedRegExpMacroAssembler::Bind(Label* l) {
    advance_current_end_ = kInvalidPC;
    DCHECK(!l->bound());
    if (l->used()) {
        int pos = l->offset();
        MOZ_ASSERT(pos >= 0);
        do {
            int fixup = pos;
            pos = *reinterpret_cast<int32_t*>(buffer_ + fixup);
            *reinterpret_cast<uint32_t*>(buffer_ + fixup) = pc_;
        } while (pos != INVALID_OFFSET);
    }
    l->bind(pc_);
}

void InterpretedRegExpMacroAssembler::EmitOrLink(Label* l) {
    if (l == NULL) l = &backtrack_;
    if (l->bound()) {
        Emit32(l->offset());
    } else {
        int pos = INVALID_OFFSET;
        if (l->used()) {
            pos = l->offset();
        }
        l->use(pc_);
        Emit32(pos);
    }
}

void InterpretedRegExpMacroAssembler::PopRegister(int register_index) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_POP_REGISTER, register_index);
}

void InterpretedRegExpMacroAssembler::PushRegister(
        int register_index,
        StackCheckFlag check_stack_limit) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_PUSH_REGISTER, register_index);
}

void InterpretedRegExpMacroAssembler::WriteCurrentPositionToRegister(
        int register_index, int cp_offset) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_SET_REGISTER_TO_CP, register_index);
    Emit32(cp_offset);  // Current position offset.
}

void InterpretedRegExpMacroAssembler::ClearRegisters(int reg_from, int reg_to) {
    DCHECK(reg_from <= reg_to);
    for (int reg = reg_from; reg <= reg_to; reg++) {
        SetRegister(reg, -1);
    }
}

void InterpretedRegExpMacroAssembler::ReadCurrentPositionFromRegister(
        int register_index) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_SET_CP_TO_REGISTER, register_index);
}

void InterpretedRegExpMacroAssembler::WriteBacktrackStackPointerToRegister(
        int register_index) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_SET_REGISTER_TO_SP, register_index);
}

void InterpretedRegExpMacroAssembler::ReadBacktrackStackPointerFromRegister(
        int register_index) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_SET_SP_TO_REGISTER, register_index);
}

void InterpretedRegExpMacroAssembler::SetCurrentPositionFromEnd(int by) {
    DCHECK(by >= 0 && by < (1 << 24));
    Emit(BC_SET_CURRENT_POSITION_FROM_END, by);
}

void InterpretedRegExpMacroAssembler::SetRegister(int register_index, int to) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_SET_REGISTER, register_index);
    Emit32(to);
}

void InterpretedRegExpMacroAssembler::AdvanceRegister(int register_index, int by) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_ADVANCE_REGISTER, register_index);
    Emit32(by);
}

void InterpretedRegExpMacroAssembler::PopCurrentPosition() {
    Emit(BC_POP_CP, 0);
}

void InterpretedRegExpMacroAssembler::PushCurrentPosition() {
    Emit(BC_PUSH_CP, 0);
}

void InterpretedRegExpMacroAssembler::Backtrack() {
    Emit(BC_POP_BT, 0);
}

void InterpretedRegExpMacroAssembler::JumpOrBacktrack(Label* l) {
    if (advance_current_end_ == pc_) {
        // Combine advance current and goto.
        pc_ = advance_current_start_;
        Emit(BC_ADVANCE_CP_AND_GOTO, advance_current_offset_);
        EmitOrLink(l);
        advance_current_end_ = kInvalidPC;
    } else {
        // Regular goto.
        Emit(BC_GOTO, 0);
        EmitOrLink(l);
    }
}

void InterpretedRegExpMacroAssembler::PushBacktrack(Label* l) {
    Emit(BC_PUSH_BT, 0);
    EmitOrLink(l);
}

bool InterpretedRegExpMacroAssembler::Succeed() {
    Emit(BC_SUCCEED, 0);
    return false;  // Restart matching for global regexp not supported.
}

void InterpretedRegExpMacroAssembler::Fail() {
    Emit(BC_FAIL, 0);
}

void InterpretedRegExpMacroAssembler::AdvanceCurrentPosition(int by) {
    DCHECK(by >= kMinCPOffset);
    DCHECK(by <= kMaxCPOffset);
    advance_current_start_ = pc_;
    advance_current_offset_ = by;
    Emit(BC_ADVANCE_CP, by);
    advance_current_end_ = pc_;
}

void InterpretedRegExpMacroAssembler::CheckGreedyLoop(
        Label* on_tos_equals_current_position) {
    Emit(BC_CHECK_GREEDY, 0);
    EmitOrLink(on_tos_equals_current_position);
}

void InterpretedRegExpMacroAssembler::LoadCurrentCharacter(int cp_offset,
                                                           Label* on_failure,
                                                           bool check_bounds,
                                                           int characters) {
    DCHECK(cp_offset >= kMinCPOffset);
    DCHECK(cp_offset <= kMaxCPOffset);
    int bytecode;
    if (check_bounds) {
        if (characters == 4) {
            bytecode = BC_LOAD_4_CURRENT_CHARS;
        } else if (characters == 2) {
            bytecode = BC_LOAD_2_CURRENT_CHARS;
        } else {
            DCHECK(characters == 1);
            bytecode = BC_LOAD_CURRENT_CHAR;
        }
    } else {
        if (characters == 4) {
            bytecode = BC_LOAD_4_CURRENT_CHARS_UNCHECKED;
        } else if (characters == 2) {
            bytecode = BC_LOAD_2_CURRENT_CHARS_UNCHECKED;
        } else {
            DCHECK(characters == 1);
            bytecode = BC_LOAD_CURRENT_CHAR_UNCHECKED;
        }
    }
    Emit(bytecode, cp_offset);
    if (check_bounds) EmitOrLink(on_failure);
}

void InterpretedRegExpMacroAssembler::CheckCharacterLT(uc16 limit,
                                                       Label* on_less) {
    Emit(BC_CHECK_LT, limit);
    EmitOrLink(on_less);
}

void InterpretedRegExpMacroAssembler::CheckCharacterGT(uc16 limit,
                                                       Label* on_greater) {
    Emit(BC_CHECK_GT, limit);
    EmitOrLink(on_greater);
}

void InterpretedRegExpMacroAssembler::CheckCharacter(uint32_t c, Label* on_equal) {
    if (c > MAX_FIRST_ARG) {
        Emit(BC_CHECK_4_CHARS, 0);
        Emit32(c);
    } else {
        Emit(BC_CHECK_CHAR, c);
    }
    EmitOrLink(on_equal);
}

void InterpretedRegExpMacroAssembler::CheckAtStart(Label* on_at_start) {
    Emit(BC_CHECK_AT_START, 0);
    EmitOrLink(on_at_start);
}

void InterpretedRegExpMacroAssembler::CheckNotAtStart(int cp_offset,
                                                      Label* on_not_at_start) {
    Emit(BC_CHECK_NOT_AT_START, cp_offset);
    EmitOrLink(on_not_at_start);
}

void InterpretedRegExpMacroAssembler::CheckNotCharacter(uint32_t c,
                                                        Label* on_not_equal) {
    if (c > MAX_FIRST_ARG) {
        Emit(BC_CHECK_NOT_4_CHARS, 0);
        Emit32(c);
    } else {
        Emit(BC_CHECK_NOT_CHAR, c);
    }
    EmitOrLink(on_not_equal);
}

void InterpretedRegExpMacroAssembler::CheckCharacterAfterAnd(
        uint32_t c,
        uint32_t mask,
        Label* on_equal) {
    if (c > MAX_FIRST_ARG) {
        Emit(BC_AND_CHECK_4_CHARS, 0);
        Emit32(c);
    } else {
        Emit(BC_AND_CHECK_CHAR, c);
    }
    Emit32(mask);
    EmitOrLink(on_equal);
}

void InterpretedRegExpMacroAssembler::CheckNotCharacterAfterAnd(
        uint32_t c,
        uint32_t mask,
        Label* on_not_equal) {
    if (c > MAX_FIRST_ARG) {
        Emit(BC_AND_CHECK_NOT_4_CHARS, 0);
        Emit32(c);
    } else {
        Emit(BC_AND_CHECK_NOT_CHAR, c);
    }
    Emit32(mask);
    EmitOrLink(on_not_equal);
}

void InterpretedRegExpMacroAssembler::CheckNotCharacterAfterMinusAnd(
        uc16 c,
        uc16 minus,
        uc16 mask,
        Label* on_not_equal) {
    Emit(BC_MINUS_AND_CHECK_NOT_CHAR, c);
    Emit16(minus);
    Emit16(mask);
    EmitOrLink(on_not_equal);
}

void InterpretedRegExpMacroAssembler::CheckCharacterInRange(
        uc16 from,
        uc16 to,
        Label* on_in_range) {
    Emit(BC_CHECK_CHAR_IN_RANGE, 0);
    Emit16(from);
    Emit16(to);
    EmitOrLink(on_in_range);
}

void InterpretedRegExpMacroAssembler::CheckCharacterNotInRange(
        uc16 from,
        uc16 to,
        Label* on_not_in_range) {
    Emit(BC_CHECK_CHAR_NOT_IN_RANGE, 0);
    Emit16(from);
    Emit16(to);
    EmitOrLink(on_not_in_range);
}

void InterpretedRegExpMacroAssembler::CheckBitInTable(
        RegExpShared::JitCodeTable table, Label* on_bit_set) {
    static const int kBitsPerByte = 8;

    Emit(BC_CHECK_BIT_IN_TABLE, 0);
    EmitOrLink(on_bit_set);
    for (int i = 0; i < kTableSize; i += kBitsPerByte) {
        int byte = 0;
        for (int j = 0; j < kBitsPerByte; j++) {
            if (table[i + j] != 0) byte |= 1 << j;
        }
        Emit8(byte);
    }
}

void InterpretedRegExpMacroAssembler::CheckNotBackReference(int start_reg,
                                                            bool read_backward,
                                                            Label* on_not_equal) {
    DCHECK(start_reg >= 0);
    DCHECK(start_reg <= kMaxRegister);
    Emit(read_backward ? BC_CHECK_NOT_BACK_REF_BACKWARD : BC_CHECK_NOT_BACK_REF,
         start_reg);
    EmitOrLink(on_not_equal);
}

void InterpretedRegExpMacroAssembler::CheckNotBackReferenceIgnoreCase(
        int start_reg, bool read_backward, bool unicode, Label* on_not_equal) {
    DCHECK(start_reg >= 0);
    DCHECK(start_reg <= kMaxRegister);
    Emit(read_backward ? (unicode ? BC_CHECK_NOT_BACK_REF_NO_CASE_UNICODE_BACKWARD
                                  : BC_CHECK_NOT_BACK_REF_NO_CASE_BACKWARD)
                       : (unicode ? BC_CHECK_NOT_BACK_REF_NO_CASE_UNICODE
                                  : BC_CHECK_NOT_BACK_REF_NO_CASE),
         start_reg);
    EmitOrLink(on_not_equal);
}

void InterpretedRegExpMacroAssembler::IfRegisterLT(int register_index,
                                                   int comparand,
                                                   Label* on_less_than) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_CHECK_REGISTER_LT, register_index);
    Emit32(comparand);
    EmitOrLink(on_less_than);
}

void InterpretedRegExpMacroAssembler::IfRegisterGE(int register_index,
                                                   int comparand,
                                                   Label* on_greater_or_equal) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_CHECK_REGISTER_GE, register_index);
    Emit32(comparand);
    EmitOrLink(on_greater_or_equal);
}

void InterpretedRegExpMacroAssembler::IfRegisterEqPos(int register_index,
                                                      Label* on_eq) {
    DCHECK(register_index >= 0);
    DCHECK(register_index <= kMaxRegister);
    checkRegister(register_index);
    Emit(BC_CHECK_REGISTER_EQ_POS, register_index);
    EmitOrLink(on_eq);
}

RegExpCode InterpretedRegExpMacroAssembler::GenerateCode(JSContext* cx, bool match_only) {
    Bind(&backtrack_);
    Emit(BC_POP_BT, 0);

    // Update the number of registers.
    *(int32_t*)buffer_ = num_registers_;

    RegExpCode res;
    res.byteCode = buffer_;
    buffer_ = nullptr;
    return res;
}

void InterpretedRegExpMacroAssembler::BindBacktrack(Label* label) {
    Bind(label);
}

void InterpretedRegExpMacroAssembler::Expand() {
    AutoEnterOOMUnsafeRegion oomUnsafe;

    int newLength = Max(100, length_ * 2);
    if (newLength < length_ + 4)
        oomUnsafe.crash("InterpretedRegExpMacroAssembler::Expand");

    buffer_ = (uint8_t*) js_realloc(buffer_, newLength);
    if (!buffer_)
        oomUnsafe.crash("InterpretedRegExpMacroAssembler::Expand");
    length_ = newLength;
}
