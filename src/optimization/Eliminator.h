/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef ELIMINATOR_H
#define ELIMINATOR_H

namespace vc4c
{
    class Method;
    class Module;
    class InstructionWalker;
    struct Configuration;

    namespace optimizations
    {
        /*
         * Eliminates operations without side-effects, whose output is never read.
         * When an instruction is removed, the previous instruction is re-checked since it could have become obsolete
         * just now.
         */
        bool eliminateDeadCode(const Module& module, Method& method, const Configuration& config);

        /*
         * Simplifies instructions which have no semantical meaning (e.g. addition with 0, xor with 0, and with all bits
         * set, etc.)
         *
         * More accurately, replaces all ALU-instructions without side-effects and where the operation itself assigns
         * the output to one of the operands.
         *
         * Example:
         *   %3 = add %2, 0
         *   %4 = max %2, %2
         *
         * is replaced with:
         *   %3 = %2
         *   %4 = %2
         */
        InstructionWalker simplifyOperation(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);
        /*
         * Replaces operations with their result if it can be determined at compile-time (e.g. operation with only
         * constant operands)
         *
         * Example:
         *   %3 = add 4, 7
         *
         * is replaced with:
         *   %3 = 11
         */
        InstructionWalker foldConstants(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);

        /*
         * Eliminates return-instructions by replacing them with a jump to the end-of-kernel label
         *
         * Example:
         *   ret
         *   ...
         *
         * is converted into:
         *   br %end_of_function
         *   ...
         *   label: %end_of_function
         *
         * NOTE: Return-instructions in inlined functions are already replaced with branches (and moves for returned
         * values) at this point. Also, this optimization-step is required for the compilation to work correctly
         */
        InstructionWalker eliminateReturn(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);

        /*
         * Eliminates various types of redundant moves
         *
         * Example:
         *   %add = fadd %tmp.1, %tmp.3
         *   [...]
         *   %x = mov %add
         *
         * becomes:
         *   [...]
         *   %x = fadd %tmp.1, %tmp.3
         *
         * Also:
         *   %add = fadd %tmp.1, %tmp.3
         *   [...]
         *   vpm = mov %add
         *
         * becomes:
         *   [...]
         *   vpm = fadd %tmp.1, %tmp.3
         *
         * And:
         *   %in = move unif
         *   %x = add %in, %y
         *
         * becomes:
         *   %x = add unif, %y
         *
         * And:
         *  %in = move %in
         *
         * remove it
         */
        bool eliminateRedundantMoves(const Module& module, Method& method, const Configuration& config);

        /*
         * Transform bit operations where some calculations are redundant
         *
         * Example:
         *  %1 = and %2, %3
         *  %4 = and %1, %2
         *
         * becomes:
         *  %1 = and %2, %3
         *  %4 = %1
         *
         * Also:
         *  %1 = and %2, %3
         *  %4 = or %1, %2
         *
         * becomes:
         *  %1 = and %2, %3
         *  %4 = %2
         *
         * And:
         *  %1 = or %2, %3
         *  %4 = and %1, %2
         *
         * becomes:
         *  %1 = or %2, %3
         *  %4 = %2
         *
         * And:
         *  %1 = or %2, %3
         *  %4 = or %1, %2
         *
         * becomes:
         *  %1 = or %2, %3
         *  %4 = %1
         *
         * And:
         *  %y = asr %x, const1
         *  %z = and %y, const2
         *
         * becomes, if const2 <= 2^const1:
         *  %y = shr %x, const1
         *  %z = and %y, const2
         *
         * And:
         *  %b = shl %a, sameConst
         *  %c = shr %b, sameConst
         *
         * becomes:
         *  %c = and %a, 2^sameConst
         */
        bool eliminateRedundantBitOp(const Module& module, Method& method, const Configuration& config);

        /*
         * Propagate source value of move operation in a basic block.
         *
         * Example:
         *  %a = %b
         *  [...]
         *  %x = add %a, %y
         *  [...]
         *  %a = xxx
         *  [...]
         *  %x = add %a, %y
         *
         * becomes:
         *  %a = %b
         *  [...]
         *  %x = add %b, %y
         *  [...]
         *  %a = xxx
         *  [...]
         *  %x = add %a, %y => this `a` cannot be replaced
         */
        bool propagateMoves(const Module& module, Method& method, const Configuration& config);

        /*
         * Common Subexpression Elimination (CSE)
         *
         * Iterates over each basic block and looks for instructions calculating the same value and combines them, if
         * possible.
         *
         * Example:
         *   %a = add %b, %c
         *   [...]
         *   %d = add %b, %c
         *
         * becomes:
         *   %a = add %b, %c
         *   [...]
         *   %d = %a
         *
         */
        bool eliminateCommonSubexpressions(const Module& module, Method& method, const Configuration& config);

        /*
         * Replaces calls to the SFU registers with constant input to a move of the result
         *
         * Example:
         *   sfu_recip = 2.0
         *   nop (sfu)
         *   nop (sfu)
         *   %a = r4
         *
         * becomes:
         *   %a = 0.5
         *
         */
        InstructionWalker rewriteConstantSFUCall(
            const Module& module, Method& method, InstructionWalker it, const Configuration& config);
    } // namespace optimizations
} // namespace vc4c
#endif /* ELIMINATOR_H */
