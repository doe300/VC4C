/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INSTRUCTION_HELPER_H
#define INSTRUCTION_HELPER_H

#include "../InstructionWalker.h"

namespace vc4c
{
    namespace intermediate
    {
        /*
         * After this function returns, dest will contain the positive value of src (either src or it's two's
         * compliment) and writeIsNegative will return whether the src was negative (-1 if negative, 0 otherwise)
         */
        NODISCARD InstructionWalker insertMakePositive(
            InstructionWalker it, Method& method, const Value& src, Value& dest, Value& writeIsNegative);
        /*
         * Restores the original sign to the value in src and writes into dest according to the value of sign.
         * Sign is -1 to restore a negative value and 0 to restore a positive value.
         *
         * NOTE: src is required to be unsigned!
         */
        NODISCARD InstructionWalker insertRestoreSign(
            InstructionWalker it, Method& method, const Value& src, Value& dest, const Value& sign);

        NODISCARD InstructionWalker insertCalculateIndices(InstructionWalker it, Method& method, const Value& container,
            const Value& dest, const std::vector<Value>& indices, bool firstIndexIsElement = false);

        NODISCARD InstructionWalker insertByteSwap(
            InstructionWalker it, Method& method, const Value& src, const Value& dest);

        /**
         * For a given value, finds the value (possible the input) that is the original source of a move-chain.
         *
         * Walks a chain of move-instructions backwards as long as the moves are unconditional simple moves without
         * side-effects.
         *
         * @return the local at the beginning of the move-chain
         */
        Value getSourceValue(Value value);

        /**
         * For a given instruction, finds the instruction (possible the input) that is the original source of a
         * move-chain.
         *
         * Walks a chain of move-instructions backwards as long as the moves are unconditional simple moves without
         * side-effects.
         *
         * If the input local of the move-chain is written by only one instruction, this instruction is returned.
         * Otherwise, the first move in the chain is returned.
         *
         * @return the instruction at the beginning of the move-chain
         */
        const IntermediateInstruction* getSourceInstruction(const IntermediateInstruction* inst);

        /**
         * For a given local, return all locals which are in the same equivalence class as the given input.
         *
         * This function defines the equivalence class as such:
         * - Locals L and K are equivalent, if L is the result of an unconditional simple move from K.
         * - Due to the transitive property, all locals in a unconditional and simple move-chain are in one
         *   class.
         * - Due to the symmetric property, the resulting class is the same for any given input local part of this
         *   class.
         *
         * See https://en.wikipedia.org/wiki/Equivalence_class
         */
        FastSet<const Local*> getEquivalenceClass(const Local* local);

        /**
         * Inserts a tight loop into the given method at the given position
         *
         * The input instruction walker will be set to the first instruction (the label) in the block FOLLOWING the
         * loop. The output basic block is the inserted block and can be used to insert code into the loop itself.
         *
         * NOTE: The inserted loop will be a while(conditionValue) loop, so the condition variable needs to be
         * initialized before the loop starts.
         *
         * NOTE: The loop is repeated as long as the conditionValue matches the repeatCondition. Normal branch condition
         * behavior applies, so only the first element of the conditionValue is actually checked!
         */
        NODISCARD BasicBlock& insertLoop(Method& method, InstructionWalker& it, const Value& conditionValue,
            ConditionCode repeatCondition, const std::string& label = "");
    } // namespace intermediate
} // namespace vc4c

#endif /* INSTRUCTION_HELPER_H */
