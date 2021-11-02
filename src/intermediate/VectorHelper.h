#pragma once
/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INSTRUCTION_VECTOR_HELPER_H
#define INSTRUCTION_VECTOR_HELPER_H

#include "../InstructionWalker.h"

namespace vc4c
{
    namespace intermediate
    {
        enum class Direction : unsigned char
        {
            UP,
            DOWN
        };
        NODISCARD InstructionWalker insertVectorRotation(InstructionWalker it, const Value& src, const Value& offset,
            const Value& dest, Direction direction = Direction::UP, bool isSingleElementMoveFromToZero = false);

        NODISCARD InstructionWalker insertReplication(
            InstructionWalker it, const Value& src, const Value& dest, bool useDestination = true);

        NODISCARD InstructionWalker insertVectorExtraction(
            InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& dest);
        NODISCARD InstructionWalker insertVectorInsertion(
            InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& value);
        NODISCARD InstructionWalker insertVectorShuffle(InstructionWalker it, Method& method, const Value& destination,
            const Value& source0, const Value& source1, const Value& mask);

        NODISCARD InstructionWalker insertVectorConcatenation(
            InstructionWalker it, Method& method, const Value& source0, const Value& source1, const Value& dest);

        /**
         * Assembles a vector into the given output variable by inserting the given scalar values at their positions.
         *
         * Any SIMD vector entry not explicitly set will be considered UNDEFINED and its value depend on the
         * instructions used to assemble the vector.
         *
         * E.g. if the elements vector has 5 entries, elements[0] will be inserted into SIMD element 0, elements[1] into
         * SIMD element 1, ... and elements[4] into SIMD element 4.
         *
         * Applies some further optimizations to generate a small number of instructions, where applicable. E.g.
         * - If all elements are the same, the first element is replicated across all elements
         * - If an element is a literal value, the vector rotation is skipped
         */
        NODISCARD InstructionWalker insertAssembleVector(
            InstructionWalker it, Method& method, const Value& dest, const std::vector<Value>& elements);
        NODISCARD InstructionWalker insertAssembleVector(
            InstructionWalker it, Method& method, const Value& dest, DataType inputType, const SIMDVector& elements);

        /**
         * Inserts operations to fold the given input vector to the given output value using the given binary operation
         *
         * The folding is done by:
         * 1. splitting the input vector into N parts with output vector size
         * 2. fold the N parts by repetitive applying the given folding operation
         * 3. store the result in the output value
         *
         * If the output vector size is larger or equals than the input vector size, a simple move will be inserted.
         */
        NODISCARD InstructionWalker insertFoldVector(InstructionWalker it, Method& method, const Value& dest,
            const Value& src, OpCode foldingOp, InstructionDecorations decorations = InstructionDecorations::NONE);
        NODISCARD InstructionWalker insertFoldVector(InstructionWalker it, Method& method, const Value& dest,
            const Value& src, OpCode foldingOp,
            FastSet<const intermediate::IntermediateInstruction*>& addedInstructions,
            InstructionDecorations decorations = InstructionDecorations::NONE);

    } // namespace intermediate
} // namespace vc4c

#endif /* INSTRUCTION_VECTOR_HELPER_H */
