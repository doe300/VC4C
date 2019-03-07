/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_CONCEPTS_H
#define VC4C_CONCEPTS_H

#include "InstructionWalker.h"
#include "Module.h"
#include "asm/ALUInstruction.h"
#include "asm/BranchInstruction.h"
#include "asm/LoadInstruction.h"
#include "asm/SemaphoreInstruction.h"
#include "performance.h"

#include <type_traits>

namespace vc4c
{
    template <typename T>
    struct assert_copyable
    {
        constexpr static bool value = std::is_copy_constructible<T>::value && std::is_copy_assignable<T>::value;
    };

    template <typename T>
    struct assert_moveable
    {
        constexpr static bool value = std::is_move_constructible<T>::value && std::is_move_assignable<T>::value;
    };

    template <typename T>
    struct assert_assignable
    {
        constexpr static bool value = assert_copyable<T>::value && assert_moveable<T>::value;
    };

    template <typename T>
    struct assert_not_copyable
    {
        constexpr static bool value = !std::is_copy_constructible<T>::value && !std::is_copy_assignable<T>::value;
    };

    template <typename T>
    struct assert_not_moveable
    {
        constexpr static bool value = !std::is_move_constructible<T>::value && !std::is_move_assignable<T>::value;
    };

    template <typename T>
    struct assert_not_assignable
    {
        constexpr static bool value = assert_not_copyable<T>::value && assert_not_moveable<T>::value;
    };

    template <typename T>
    struct assert_comparable
    {
        constexpr static bool value = std::is_same<bool, decltype(std::declval<T>() < std::declval<T>())>::value &&
            std::is_same<bool, decltype(std::declval<T>() == std::declval<T>())>::value;
    };

    template <typename T>
    struct assert_hashable
    {
        constexpr static bool value =
            std::is_same<size_t, decltype(std::declval<std::hash<T>>().operator()(std::declval<T>()))>::value;
    };

    template <typename T>
    struct assert_stringifyable
    {
        constexpr static bool value = std::is_same<std::string, decltype(std::declval<T>().to_string())>::value;
    };

    template <typename T>
    struct assert_trivial
    {
        constexpr static bool value = std::is_literal_type<T>::value && std::is_trivially_copy_assignable<T>::value &&
            std::is_trivially_copy_constructible<T>::value && std::is_trivially_copyable<T>::value &&
            std::is_trivially_destructible<T>::value && std::is_trivially_move_assignable<T>::value &&
            std::is_trivially_move_constructible<T>::value;
    };

    /*
     * DataType
     *
     * (most of this concepts are required by Value)
     */
    static_assert(assert_assignable<DataType>::value, "DataType is not assignable!");
    static_assert(std::is_destructible<DataType>::value, "DataType is not destructible!");
    static_assert(assert_comparable<DataType>::value, "DataType is not comparable!");
    static_assert(assert_hashable<DataType>::value, "DataType is not hashable!");
    static_assert(assert_stringifyable<DataType>::value, "DataType is not stringify-able!");
    static_assert(sizeof(DataType) == sizeof(void*), "DataType is unnecessary big!");
    static_assert(assert_trivial<DataType>::value, "DataType is not trivial");

    /*
     * Value types
     *
     * (most of this concepts are required by Value)
     */
    static_assert(!std::is_default_constructible<Register>::value, "Register is default constructible!");
    static_assert(assert_assignable<Register>::value, "Register is not assignable!");
    static_assert(assert_comparable<Register>::value, "Register is not comparable");
    static_assert(assert_hashable<Register>::value, "Register is not hashable!");
    static_assert(assert_stringifyable<Register>::value, "Register is not stringify-able!");
    static_assert(assert_trivial<Register>::value, "Register is not trivial");
    static_assert(sizeof(Register) == 2 * sizeof(uint8_t), "");

    static_assert(!std::is_default_constructible<Literal>::value, "Literal is default constructible!");
    static_assert(assert_assignable<Literal>::value, "Literal is not assignable!");
    static_assert(assert_comparable<Literal>::value, "Literal is not comparable");
    static_assert(assert_stringifyable<Literal>::value, "Literal is not stringify-able!");
    static_assert(assert_trivial<Literal>::value, "Literal is not trivial");
    static_assert(sizeof(Literal) <= 2 * sizeof(uint32_t), "");
    static_assert(sizeof(Optional<Literal>) == sizeof(Literal), "");

    static_assert(!std::is_default_constructible<SmallImmediate>::value, "SmallImmediate is default constructible!");
    static_assert(assert_assignable<SmallImmediate>::value, "SmallImmediate is not assignable!");
    static_assert(assert_comparable<SmallImmediate>::value, "SmallImmediate is not comparable");
    static_assert(assert_stringifyable<SmallImmediate>::value, "SmallImmediate is not stringify-able!");
    static_assert(assert_trivial<SmallImmediate>::value, "SmallImmediate is not literal");
    static_assert(sizeof(SmallImmediate) == sizeof(uint8_t), "");
    static_assert(sizeof(Optional<SmallImmediate>) == sizeof(SmallImmediate), "");

    static_assert(assert_assignable<CompoundConstant>::value, "ContainerValue is not assignable!");
    static_assert(std::is_destructible<CompoundConstant>::value, "ContainerValue is not destructible!");
    static_assert(assert_stringifyable<CompoundConstant>::value, "SIMDVector is not stringify-able!");

    static_assert(std::is_default_constructible<SIMDVector>::value, "SIMDVector is not default constructible!");
    static_assert(assert_assignable<SIMDVector>::value, "SIMDVector is not assignable!");
    static_assert(std::is_destructible<SIMDVector>::value, "SIMDVector is not destructible!");
    static_assert(assert_hashable<SIMDVector>::value, "SIMDVector is not hashable!");

    static_assert(!std::is_default_constructible<Value>::value, "Value is default constructible!");
    static_assert(assert_assignable<Value>::value, "Value is not assignable!");
    static_assert(std::is_destructible<Value>::value, "Value is not destructible!");
    // XXX static_assert(assert_comparable<Value>::value, "Value is not comparable!");
    static_assert(assert_hashable<Value>::value, "Value is not hashable!");
    static_assert(assert_stringifyable<Value>::value, "Value is not stringify-able!");

    static_assert(assert_not_copyable<Local>::value, "Local is copyable!");
    static_assert(std::is_move_constructible<Local>::value, "Local is not move constructible!");
    static_assert(std::is_destructible<Local>::value, "Local is not destructible!");
    static_assert(assert_stringifyable<Local>::value, "Local is not stringify-able!");
    static_assert(std::has_virtual_destructor<Local>::value, "Local has no virtual destructor!");

    /*
     * Method/Module types
     *
     * (cannot be moved/copied, since it would invalidate references/pointers)
     */
    static_assert(assert_not_assignable<BasicBlock>::value, "BasicBlock is assignable!");
    static_assert(std::is_destructible<BasicBlock>::value, "BasicBlock is not destructible!");

    static_assert(assert_not_assignable<Method>::value, "Method is assignable!");
    static_assert(std::is_destructible<Method>::value, "Method is not destructible!");

    static_assert(assert_not_assignable<Module>::value, "Module is assignable!");
    static_assert(std::is_destructible<Module>::value, "Module is not destructible!");

    static_assert(
        std::is_default_constructible<InstructionWalker>::value, "InstructionWalker is not default constructible!");
    static_assert(assert_assignable<InstructionWalker>::value, "InstructionWalker is not assignable!");
    static_assert(std::is_destructible<InstructionWalker>::value, "InstructionWalker is not destructible!");
    static_assert(assert_hashable<InstructionWalker>::value, "InstructionWalker is not hashable!");
    static_assert(sizeof(Optional<InstructionWalker>) == sizeof(InstructionWalker), "");

    /*
     * Intermediate instructions
     *
     * (live only on the heap, copying/moving them would invalidate the pointers e.g. for local-uses)
     */
    static_assert(
        assert_not_assignable<intermediate::IntermediateInstruction>::value, "IntermediateInstruction is assignable!");
    static_assert(std::is_destructible<intermediate::IntermediateInstruction>::value,
        "IntermediateInstruction is not destructible!");
    static_assert(assert_stringifyable<intermediate::IntermediateInstruction>::value,
        "IntermediateInstruction is not stringify-able!");
    static_assert(std::has_virtual_destructor<intermediate::IntermediateInstruction>::value,
        "IntermediateInstruction has no virtual destructor!");

    /*
     * Backend instructions and types
     *
     * (Try to keep fully assignable, if possible)
     */
    static_assert(assert_assignable<ConditionCode>::value, "ConditionCode is not assignable!");
    static_assert(assert_comparable<ConditionCode>::value, "ConditionCode is not comparable");
    static_assert(assert_stringifyable<ConditionCode>::value, "ConditionCode is not stringify-able!");
    static_assert(assert_trivial<ConditionCode>::value, "ConditionCode is not trivial");
    static_assert(sizeof(ConditionCode) == sizeof(uint8_t), "");

    static_assert(assert_assignable<Signaling>::value, "Signaling is not assignable!");
    static_assert(assert_comparable<Signaling>::value, "Signaling is not comparable");
    static_assert(assert_stringifyable<Signaling>::value, "Signaling is not stringify-able!");
    static_assert(assert_trivial<Signaling>::value, "Signaling is not trivial");
    static_assert(sizeof(Signaling) == sizeof(uint8_t), "");

    static_assert(assert_assignable<Unpack>::value, "Unpack is not assignable!");
    static_assert(assert_comparable<Unpack>::value, "Unpack is not comparable");
    static_assert(assert_stringifyable<Unpack>::value, "Unpack is not stringify-able!");
    static_assert(assert_trivial<Unpack>::value, "Unpack is not trivial");
    static_assert(sizeof(Unpack) == sizeof(uint8_t), "");

    static_assert(assert_assignable<Pack>::value, "Pack is not assignable!");
    static_assert(assert_comparable<Pack>::value, "Pack is not comparable");
    static_assert(assert_stringifyable<Pack>::value, "Pack is not stringify-able!");
    static_assert(assert_trivial<Pack>::value, "Pack is not trivial");
    static_assert(sizeof(Pack) == sizeof(uint8_t), "");

    static_assert(assert_assignable<SetFlag>::value, "SetFlag is not assignable!");
    static_assert(assert_comparable<SetFlag>::value, "SetFlag is not comparable");
    static_assert(assert_trivial<SetFlag>::value, "SetFlag is not trivial");
    static_assert(sizeof(SetFlag) == sizeof(uint8_t), "");

    static_assert(assert_assignable<OpCode>::value, "OpCode is not assignable!");
    static_assert(assert_comparable<OpCode>::value, "OpCode is not comparable");
    static_assert(assert_trivial<OpCode>::value, "OpCode is not trivial");

    static_assert(std::is_trivially_destructible<qpu_asm::Instruction>::value,
        "Assembler instructions are not trivially destructible!");
    static_assert(std::is_trivially_destructible<qpu_asm::ALUInstruction>::value,
        "Assembler instructions are not trivially destructible!");
    static_assert(std::is_trivially_destructible<qpu_asm::BranchInstruction>::value,
        "Assembler instructions are not trivially destructible!");
    static_assert(std::is_trivially_destructible<qpu_asm::LoadInstruction>::value,
        "Assembler instructions are not trivially destructible!");
    static_assert(std::is_trivially_destructible<qpu_asm::SemaphoreInstruction>::value,
        "Assembler instructions are not trivially destructible!");
    static_assert(sizeof(qpu_asm::Instruction) == sizeof(uint64_t), "Assembler instructions contain additional data!");
    static_assert(
        sizeof(qpu_asm::ALUInstruction) == sizeof(uint64_t), "Assembler instructions contain additional data!");
    static_assert(
        sizeof(qpu_asm::BranchInstruction) == sizeof(uint64_t), "Assembler instructions contain additional data!");
    static_assert(
        sizeof(qpu_asm::LoadInstruction) == sizeof(uint64_t), "Assembler instructions contain additional data!");
    static_assert(
        sizeof(qpu_asm::SemaphoreInstruction) == sizeof(uint64_t), "Assembler instructions contain additional data!");

    /*
     * Some helper types
     *
     * (Try to keep fully assignable, if possible)
     */
    static_assert(std::is_default_constructible<Optional<unsigned>>::value, "Optional is not default constructible!");
    static_assert(assert_assignable<Optional<unsigned>>::value, "Optional is not assignable!");
    static_assert(assert_copyable<Optional<unsigned>>::value, "Optional is not copyable!");
    static_assert(assert_moveable<Optional<unsigned>>::value, "Optional is not moveable!");
    static_assert(std::is_destructible<Optional<unsigned>>::value, "Optional is not destructible!");

    static_assert(assert_not_copyable<TemporaryFile>::value, "TemporaryFile is copyable!");

    static_assert(std::is_default_constructible<Bitfield<uint64_t>>::value, "Bitfield is not default constructible!");
    static_assert(assert_assignable<Bitfield<uint64_t>>::value, "Bitfield is not assignable!");
    static_assert(assert_comparable<Bitfield<uint64_t>>::value, "Bitfield is not comparable");
    static_assert(assert_trivial<Bitfield<uint64_t>>::value, "Bitfield is not trivial");

    static_assert(assert_assignable<InstructionPart>::value, "InstructionPart is not assignable!");
    static_assert(assert_comparable<InstructionPart>::value, "InstructionPart is not comparable");
    static_assert(assert_trivial<InstructionPart>::value, "InstructionPart is not trivial");
    static_assert(sizeof(InstructionPart) == sizeof(uint8_t), "");
} // namespace vc4c

#endif /* VC4C_CONCEPTS_H */
