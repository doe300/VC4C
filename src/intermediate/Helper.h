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
         * Walks a chain of move-instructions (or identity operations) backwards as long as the moves are unconditional
         * simple moves without side-effects.
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
         * NOTE: The loop is repeated as long as the conditionValue is true. Normal branch condition
         * behavior applies, so only the first element of the conditionValue is actually checked!
         */
        NODISCARD BasicBlock& insertLoop(
            Method& method, InstructionWalker& it, const Value& conditionValue, const std::string& label = "");

        /**
         * Inserts the instructions required to set the flags to indicate branching on the given elements of the given
         * condition value.
         *
         * @param conditionValue the variable to set the branch flags on. This needs to be a boolean (also vector) type
         * or any type where a non-zero value (at the positions indicated by the conditional elements mask) indicates
         * taking the branch and a zero-value indicates not taking the branch.
         * @param conditionalElements the optional mask of SIMD elements to be considered for branching.
         * @param branchOnAllElements if the mask is not a single SIMD element, this indicates whether the branch has to
         * be taken if ALL (true) or ANY (false) of the selected elements have the correct flags set.
         */
        NODISCARD std::pair<InstructionWalker, BranchCond> insertBranchCondition(Method& method, InstructionWalker it,
            const Value& conditionValue, std::bitset<NATIVE_VECTOR_SIZE> conditionalElements = 0x1,
            bool branchOnAllElements = false);

        /**
         * Attempts to determine the original boolean value as well as the SIMD element mask used to set the flags for
         * the successive branch instructions.
         *
         * In other words: tries to recreate the original conditionValue and conditionalElements passed into
         * #insertBranchCondition(...) which created the given instruction.
         */
        std::pair<Optional<Value>, std::bitset<NATIVE_VECTOR_SIZE>> getBranchCondition(const ExtendedInstruction* inst);

        /**
         * Redirects all EXPLICIT branches previously targeting the old target to now jump to the new target.
         */
        void redirectAllBranches(BasicBlock& oldTarget, BasicBlock& newTarget);

        /**
         * Checks whether:
         * - the pack-mode of the previous instruction is set, since in that case, the register-file A MUST be used, so
         * it cannot be read in the next instruction
         * - the unpack-mode of this instruction is set, since in that case, the register-file A MUST be used, so it
         * cannot be written to in the previous instruction
         * - also vector-rotations MUST be on accumulator, but the input MUST NOT be written in the previous instruction
         *
         * NOTE: This function does not check whether the instructions are actually neighboring!
         *
         * @return whether they has to be another instruction between the two given instructions, i.e. due to
         * read-after-write of locals
         */
        bool needsDelay(
            const IntermediateInstruction* firstInst, const IntermediateInstruction* secondInst, const Local* local);

        /**
         * Runs the checks of the #needsDelay(() function above with additional checks enabled:
         * - local is used afterwards (and not just in the next few instructions)
         *
         * NOTE: This function does not check whether the instructions are actually neighboring!
         *
         * @return whether they has to be another instruction between the two given instructions, i.e. due to
         * read-after-write of locals
         */
        bool needsDelay(InstructionWalker firstIt, InstructionWalker secondIt, const Local* local,
            std::size_t accumulatorThreshold);

        /**
         * Generalized version of above check running the #needsDelay() check for all locals written in the first and
         * used in the second instruction
         *
         * @return whether they has to be another instruction between the two given instructions, i.e. due to
         * read-after-write of locals
         */
        bool needsDelay(const IntermediateInstruction* firstInst, const IntermediateInstruction* secondInst);
    } // namespace intermediate
} // namespace vc4c

#endif /* INSTRUCTION_HELPER_H */
