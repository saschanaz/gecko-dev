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

#ifndef V8_INTERPRETED_REGEXP_MACRO_ASSEMBLER_H_
#define V8_INTERPRETED_REGEXP_MACRO_ASSEMBLER_H_

#include "irregexp/RegExpMacroAssembler.h"

namespace js {
namespace irregexp {

class MOZ_STACK_CLASS InterpretedRegExpMacroAssembler final : public RegExpMacroAssembler
{
  public:
    InterpretedRegExpMacroAssembler(JSContext* cx, LifoAlloc* alloc, size_t numSavedRegisters);
    ~InterpretedRegExpMacroAssembler();

    // Inherited virtual methods.
    RegExpCode GenerateCode(JSContext* cx, bool match_only);
    void AdvanceCurrentPosition(int by);
    void AdvanceRegister(int reg, int by);
    void Backtrack();
    void Bind(jit::Label* label);
    void CheckAtStart(jit::Label* on_at_start);
    void CheckCharacter(unsigned c, jit::Label* on_equal);
    void CheckCharacterAfterAnd(unsigned c, unsigned and_with, jit::Label* on_equal);
    void CheckCharacterGT(char16_t limit, jit::Label* on_greater);
    void CheckCharacterLT(char16_t limit, jit::Label* on_less);
    void CheckGreedyLoop(jit::Label* on_tos_equals_current_position);
    void CheckNotAtStart(jit::Label* on_not_at_start);
    void CheckNotBackReference(int start_reg, jit::Label* on_no_match);
    void CheckNotBackReferenceIgnoreCase(int start_reg, jit::Label* on_no_match, bool unicode);
    void CheckNotCharacter(unsigned c, jit::Label* on_not_equal);
    void CheckNotCharacterAfterAnd(unsigned c, unsigned and_with, jit::Label* on_not_equal);
    void CheckNotCharacterAfterMinusAnd(char16_t c, char16_t minus, char16_t and_with,
                                        jit::Label* on_not_equal);
    void CheckCharacterInRange(char16_t from, char16_t to,
                               jit::Label* on_in_range);
    void CheckCharacterNotInRange(char16_t from, char16_t to,
                                  jit::Label* on_not_in_range);
    void CheckBitInTable(RegExpShared::JitCodeTable table, jit::Label* on_bit_set);
    void JumpOrBacktrack(jit::Label* to);
    void Fail();
    void IfRegisterGE(int reg, int comparand, jit::Label* if_ge);
    void IfRegisterLT(int reg, int comparand, jit::Label* if_lt);
    void IfRegisterEqPos(int reg, jit::Label* if_eq);
    void LoadCurrentCharacter(int cp_offset, jit::Label* on_end_of_input,
                              bool check_bounds = true, int characters = 1);
    void PopCurrentPosition();
    void PopRegister(int register_index);
    void PushCurrentPosition();
    void PushRegister(int register_index, StackCheckFlag check_stack_limit);
    void ReadCurrentPositionFromRegister(int reg);
    void ReadBacktrackStackPointerFromRegister(int reg);
    void SetCurrentPositionFromEnd(int by);
    void SetRegister(int register_index, int to);
    bool Succeed();
    void WriteCurrentPositionToRegister(int reg, int cp_offset);
    void ClearRegisters(int reg_from, int reg_to);
    void WriteBacktrackStackPointerToRegister(int reg);
    void PushBacktrack(jit::Label* label);
    void BindBacktrack(jit::Label* label);

    // The byte-code interpreter checks on each push anyway.
    int stack_limit_slack() { return 1; }

  private:
    void Expand();

    // Code and bitmap emission.
    void EmitOrLink(jit::Label* label);
    void Emit32(uint32_t x);
    void Emit16(uint32_t x);
    void Emit8(uint32_t x);
    void Emit(uint32_t bc, uint32_t arg);

    jit::Label backtrack_;

    // The program counter.
    int pc_;

    int advance_current_start_;
    int advance_current_offset_;
    int advance_current_end_;

    static const int kInvalidPC = -1;

    uint8_t* buffer_;
    int length_;
};

} }  // namespace js::irregexp

#endif  // V8_INTERPRETED_REGEXP_MACRO_ASSEMBLER_H_
