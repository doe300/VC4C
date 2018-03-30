/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef LOADINSTRUCTION_H
#define LOADINSTRUCTION_H

#include "Instruction.h"
namespace vc4c
{
    namespace qpu_asm
    {
        class LoadInstruction : public Instruction
        {
        public:
            explicit LoadInstruction(uint64_t code) : Instruction(code) {}
            LoadInstruction(Pack pack, ConditionCode condAdd, ConditionCode condMul, SetFlag sf, WriteSwap ws,
                Address addOut, Address mulOut, uint32_t value);
            LoadInstruction(Pack pack, ConditionCode condAdd, ConditionCode condMul, SetFlag sf, WriteSwap ws,
                Address addOut, Address mulOut, int16_t value0, int16_t value1);
            LoadInstruction(Pack pack, ConditionCode condAdd, ConditionCode condMul, SetFlag sf, WriteSwap ws,
                Address addOut, Address mulOut, uint16_t value0, uint16_t value1);
            ~LoadInstruction() override = default;

            std::string toASMString(bool addComments) const override;
            bool isValidInstruction() const override;

            BITFIELD_ENTRY(Pack, Pack, 52, Quintuple)
            BITFIELD_ENTRY(AddCondition, ConditionCode, 49, Triple)
            BITFIELD_ENTRY(MulCondition, ConditionCode, 46, Triple)
            BITFIELD_ENTRY(SetFlag, SetFlag, 45, Bit)

            BITFIELD_ENTRY(ImmediateInt, uint32_t, 0, Int)
            BITFIELD_ENTRY(ImmediateShort0, uint16_t, 16, Short)
            BITFIELD_ENTRY(ImmediateShort1, uint16_t, 0, Short)
            BITFIELD_ENTRY(ImmediateSignedShort0, int16_t, 16, SignedShort)
            BITFIELD_ENTRY(ImmediateSignedShort1, int16_t, 0, SignedShort)
        };
    } // namespace qpu_asm
} // namespace vc4c

#endif /* LOADINSTRUCTION_H */
