/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef ELIMINATOR_H
#define ELIMINATOR_H

#include "config.h"

namespace vc4c
{
    class Method;
    class Module;
    class InstructionWalker;

    namespace optimizations
    {
        /*
         * Eliminates operations without side-effects, whose output is never read.
         * When an instruction is removed, the previous instruction is re-checked since it could have become obsolete
         * just now.
         */
        void eliminateDeadStore(const Module& module, Method& method, const Configuration& config);
        void eliminatePhiNodes(const Module& module, Method& method, const Configuration& config);

        /*
         * Eliminates instructions which have no semantical meaning (e.g. addition with 0, xor with 0, and with all bits
         * set, etc.)
         *
         * More accurately, replaces all ALU-instructions without side-effects and where at least one operand is the
         * corresponding identity.
         *
         * Example:
         *   %3 = add %2, 0
         *
         * is replaced with:
         *   %3 = %2
         */
        bool eliminateUselessInstruction(
            const Module& module, Method& method, InstructionWalker& it, const Configuration& config);
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
        bool calculateConstantInstruction(
            const Module& module, Method& method, InstructionWalker& it, const Configuration& config);
        /*
         * Eliminates branches to the label directly following the branch
         *
         * Example:
         *   ...
         *   br %103
         *   label: %103
         *
         * The branch is replaced with an automatic fall-through:
         *   ...
         *   label: %103
         */
        InstructionWalker eliminateUselessBranch(
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
         * Transform bit ("and" and "or") operations
         *
         * 	and v1, v2, v3 => and v1, v2, v4
         *	and v4, v1, v2    mov v4, v1
         *
         *	and v1, v2, v3 => and v1, v2, v3
         *	or  v4, v1, v2    mov v4, v2
         *
         * or  v1, v2, v3 => or  v1, v2, v4
         * and v4, v1, v2    mov v4, v2
         *
         * or  v1, v2, v3 => or  v1, v2, v3
         * or  v4, v1, v2    mov v4, v1
         */
        bool eliminateRedundantBitOp(const Module& module, Method& method, const Configuration& config);

        /*
         * Translform operations (and, or, max, min, v8max, v8min with 1st arg == 2nd arg) which are equal to move.
         */
        bool translateToMove(const Module& module, Method& method, const Configuration& config);

        /*
         * Propagate source value of move operation in a basic block.
         *
         * Works as follows:
         *
         * mov a, b
         * ...
         * iadd x, a, y => replace a to b
         * ...
         * iadd a, ...
         * ...
         * iadd x, a, y => this `a` cannot be replaced
         */
        bool propagateMoves(const Module& module, Method& method, const Configuration& config);
    } // namespace optimizations
} // namespace vc4c
#endif /* ELIMINATOR_H */
