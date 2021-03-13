/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef COMBINER_H
#define COMBINER_H

#include "../performance.h"

#include <functional>
#include <vector>

namespace vc4c
{
    class Method;
    class Module;
    class InstructionWalker;
    struct Configuration;
    class Local;
    struct Register;

    namespace intermediate
    {
        struct MoveOperation;
        struct Operation;
    } // namespace intermediate

    namespace optimizations
    {
        /*
         * Combine successive branches to the same label into a single branch.
         * Also eliminates branches to the label directly following the branch and replaces them with automatic
         * fall-through.
         *
         * Example:
         *   label: %91
         *   br %103
         *   label: %92
         *   br %103
         *   label: %93
         *   br %103
         *   label: %94
         *   br %103
         *   label: %95
         *   br %103
         *   [...]
         *   br %105
         *   label %105
         *
         * is converted into:
         *   label: %91
         *   label: %92
         *   label: %93
         *   label: %94
         *   label: %95
         *   br %103
         *   [...]
         *   label %105
         */
        bool simplifyBranches(const Module& module, Method& method, const Configuration& config);

        /**
         * Additional data to influence the merge condition checking and relay intermediate information between the
         * single conditions
         */
        struct MergeConditionData
        {
            /**
             * The optional label-to-register map. If this is set, the merge conditions are checked in "peephole mode",
             * modifying some checks from a more generic version to check the exact registers locals are mapped to,
             * allowing for more accurate results.
             *
             * This value has to be set before the merge condition checks are started.
             */
            const FastMap<const Local*, Register>* registerMap = nullptr;
            /**
             * One of the instructions is a simple move of zero and can be rewritten by converting to an xor of the one
             * of the (literal) inputs of the other instruction.
             *
             * This value is set during the merge condition checks.
             */
            intermediate::MoveOperation* rewriteSimpleMoveOfZero = nullptr;

            bool isPeepholeRun() const noexcept;
        };

        using MergeCondition = std::function<bool(intermediate::Operation*, intermediate::Operation*,
            intermediate::MoveOperation*, intermediate::MoveOperation*, MergeConditionData&)>;

        // The global merge conditions
        extern const std::vector<MergeCondition> MERGE_CONDITIONS;

        /**
         * Utility function to do the actual combination of operations
         *
         * On success, the CombinedInstruction is inserted into the first parameter it while the second parameter nextIt
         * is erased, making it inaccessible for further processing.
         *
         * NOTE: This function does NOT check for combinability, this has to be done before!
         */
        bool combineOperationsInner(InstructionWalker it, InstructionWalker nextIt);

        /*
         * Combine ALU-instructions which (can) use different ALUs into a single instruction accessing both ALUs.
         * There are two types of instruction-pairs which can be combined:
         * - The instructions do not depend on one another, their relative order unimportant (e.g. output of first
         * instruction is not read in second one)
         * - Both instructions write to the same output but with inverted conditions (see example)
         *
         * Example (source taken from #combineSelectionWithZero):
         *   %5 = %11 (ifz)
         *   %5 = xor %11, %11 (ifzc)
         *
         * is converted to:
         *   %5 = xor %11, %11 (ifzc) and %5 = v8min %11, %11 (ifz)
         *
         * NOTE: As of this point, the instruction-type CombinedInstruction can occur within a basic block!
         * Also, only moves and ALU instructions are combined at the moment
         */
        bool combineOperations(const Module& module, Method& method, const Configuration& config);

        /*
         * Combines the loading of the same constant value (e.g. literal or constant register) within a small range in a
         * single basic block
         *
         * Example:
         *   %3 = loadi 123456
         *   ...
         *   %7 = loadi 123456
         *   %8 = mul24 %3, %4
         *   %9 = add %7, %5
         *
         * is converted to:
         *   %3 = loadi 123456
         *   ...
         *   %8 = mul24 %3, %4
         *   %9 = add %3, %5
         *
         * Also:
         *   %5 = qpu_num
         *   %6 = add %5, %4
         *   ...
         *   %7 = qpu_num
         *   %8 = and %7, %6
         *
         * is converted to:
         *   %5 = qpu_num
         *   %6 = add %5, %4
         *   ...
         *   %8 = and %5, %6
         */
        bool combineLoadingConstants(const Module& module, Method& method, const Configuration& config);

        /*
         * Prepares selections (successive writes to same value with inverted conditions) which write to a local, have
         * no side-effects and one of the sources is zero for combination, by rewriting the zero-write to xor-ing the
         * other value
         *
         * Example:
         *   %5 = %11 (ifz)
         *   %5 = 0 (ifzc)
         *
         * is converted to:
         *   %5 = %11 (ifz)
         *   %5 = xor %11, %11 (ifzc)
         */
        InstructionWalker combineSelectionWithZero(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);

        /*
         * Combines vector several consecutive rotations with the same data
         *
         * Example:
         *   %4 = %3 << 2
         *   %5 = %4 << 6
         *
         * is converted to:
         *   %5 = %3 << 8
         *
         * NOTE: This optimization currently only works for constant rotation offsets.
         */
        bool combineVectorRotations(const Module& module, Method& method, const Configuration& config);

        /*
         * Combines arithmetic operations if the result of the first operation is used as the second operation and the
         * operations allow combining (e.g. no side-effects).
         *
         * Also, the combining is only done if an instruction can be saved (e.g. intermediate result has single usage)
         *
         * Example:
         *   %a = add %b, 3
         *   [...]
         *   %c = add %a, 4
         *
         * becomes:
         *   %c = add %b, 7
         *
         * Also:
         *   %a = shl %b, 4
         *   [...]
         *   %c = shl %a, 3
         *
         * becomes:
         *   %c = shl %b, 7
         *
         */
        InstructionWalker combineArithmeticOperations(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);

        // TODO documentation, TODO move somewhere else?!
        bool cacheWorkGroupDMAAccess(const Module& module, Method& method, const Configuration& config);
    } // namespace optimizations
} // namespace vc4c
#endif /* COMBINER_H */
