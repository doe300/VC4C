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
            const Value& dest, Direction direction = Direction::UP);

        NODISCARD InstructionWalker insertReplication(
            InstructionWalker it, const Value& src, const Value& dest, bool useDestionation = true);

        NODISCARD InstructionWalker insertVectorExtraction(
            InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& dest);
        NODISCARD InstructionWalker insertVectorInsertion(
            InstructionWalker it, Method& method, const Value& container, const Value& index, const Value& value);
        NODISCARD InstructionWalker insertVectorShuffle(InstructionWalker it, Method& method, const Value& destination,
            const Value& source0, const Value& source1, const Value& mask);

        NODISCARD InstructionWalker insertVectorConcatenation(
            InstructionWalker it, Method& method, const Value& source0, const Value& source1, const Value& dest);

        /**
         * Types of input sources for generating vector elements
         *
         * The source types are sorted by the number of instructions they require.
         */
        enum class SourceType
        {
            // For undefined values, the source is of no importance,
            ANY,
            // Read the elem_num register
            ELEMENT_NUMBER,
            // Load the given constant value
            CONSTANT,
            // Load the given unsigned value [0,3]
            LOAD_UNSIGNED_MASK,
            // Load the given signed value [-2,1]
            LOAD_SIGNED_MASK,
            // Replicate (elem_num/4) per quad to get the quad number (0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)
            REPLICATE_QUAD_NUMBER
        };

        /**
         * Types of modification to apply to the vector element source
         *
         * The modification types are sorted by the number of instructions they require.
         */
        enum class ModificationType
        {
            // Do not modify the source
            NONE,
            // Multiply (mul24) the given constant factor
            MULTIPLY,
            // Add the given constant offset
            ADD,
            // Rotate by the given constant factor
            ROTATE
        };

        /**
         * Information to assemble part of a vector (e.g. for assembling constant vectors or for shuffling).
         */
        struct ElementSource
        {
            SourceType sourceType;
            // The additional source value, e.g. constant value, load value
            Optional<Literal> sourceValue;
            ModificationType modificationType = ModificationType::NONE;
            // The additional modification value, e.g. addition offset, multiplication factor or rotation offset
            Optional<Literal> modificationValue = {};
            // TODO modification value could also be elem_num!

            bool operator<(const ElementSource& other) const noexcept;
            bool operator==(const ElementSource& other) const noexcept;

            bool isCompatible(const ElementSource& other) const noexcept;
            std::string to_string() const;
        };

        Optional<std::vector<ElementSource>> checkVectorCanBeAssembled(DataType type, SIMDVector vector);
        NODISCARD InstructionWalker insertAssembleVector(
            InstructionWalker it, Method& method, const Value& dest, std::vector<ElementSource>&& sources);

    } // namespace intermediate
} // namespace vc4c

#endif /* INSTRUCTION_VECTOR_HELPER_H */
