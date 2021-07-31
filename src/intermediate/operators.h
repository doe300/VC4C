/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_INSTRUCTION_OPERATORS_H
#define VC4C_INSTRUCTION_OPERATORS_H

#include "../InstructionWalker.h"
#include "../Method.h"
#include "../asm/OpCodes.h"
#include "../helper.h"
#include "IntermediateInstruction.h"

#include <bitset>
#include <cmath>

namespace vc4c
{
    namespace operators
    {
        inline constexpr Literal operator""_lit(unsigned long long int num)
        {
            if(num > std::numeric_limits<uint32_t>::max())
                throw CompilationError(CompilationStep::GENERAL, "Literal exceeds integer size", std::to_string(num));
            return Literal(static_cast<uint32_t>(num));
        }

        inline constexpr Literal operator""_lit(long double num)
        {
            if(num > static_cast<long double>(std::numeric_limits<float>::max()))
                throw CompilationError(CompilationStep::GENERAL, "Literal exceeds float size", std::to_string(num));
            if(num < static_cast<long double>(std::numeric_limits<float>::lowest()))
                throw CompilationError(CompilationStep::GENERAL, "Literal exceeds float size", std::to_string(num));
            if(static_cast<long double>(static_cast<float>(num)) != num)
                throw CompilationError(
                    CompilationStep::GENERAL, "Literal cannot be represented exactly as float", std::to_string(num));
            return Literal(static_cast<float>(num));
        }

        inline Value operator""_val(unsigned long long int num)
        {
            if(num > std::numeric_limits<uint32_t>::max())
                throw CompilationError(
                    CompilationStep::GENERAL, "Value literal exceeds integer size", std::to_string(num));
            auto type = num <= std::numeric_limits<uint8_t>::max() ?
                TYPE_INT8 :
                (num <= std::numeric_limits<uint16_t>::max() ? TYPE_INT16 : TYPE_INT32);
            return Value(Literal(static_cast<uint32_t>(num)), type);
        }

        inline Value operator""_val(long double num)
        {
            if(num > static_cast<long double>(std::numeric_limits<float>::max()))
                throw CompilationError(
                    CompilationStep::GENERAL, "Value literal exceeds float size", std::to_string(num));
            if(num < static_cast<long double>(std::numeric_limits<float>::lowest()))
                throw CompilationError(
                    CompilationStep::GENERAL, "Value literal exceeds float size", std::to_string(num));
            if(static_cast<long double>(static_cast<float>(num)) != num)
                throw CompilationError(CompilationStep::GENERAL, "Value literal cannot be represented exactly as float",
                    std::to_string(num));
            return Value(Literal(static_cast<float>(num)), TYPE_FLOAT);
        }

        struct OperationWrapper : private NonCopyable
        {
            OpCode op;
            Value arg0;
            Optional<Value> arg1 = NO_VALUE;

            OperationWrapper(OpCode code, const Value& arg0) : op(code), arg0(arg0) {}
            OperationWrapper(OpCode code, const Value& arg0, const Value& arg1) : op(code), arg0(arg0), arg1(arg1) {}

            Signaling signal = SIGNAL_NONE;
            Unpack unpackMode = UNPACK_NOP;
            Pack packMode = PACK_NOP;
            ConditionCode conditional = COND_ALWAYS;
            SetFlag setFlags = SetFlag::DONT_SET;
            intermediate::InstructionDecorations decoration = intermediate::InstructionDecorations::NONE;

            NODISCARD OperationWrapper operator,(Signaling sig) &&
            {
                signal = sig;
                return std::move(*this);
            }

            NODISCARD OperationWrapper operator,(Unpack unpack) &&
            {
                unpackMode = unpack;
                return std::move(*this);
            }

            NODISCARD OperationWrapper operator,(Pack pack) &&
            {
                packMode = pack;
                return std::move(*this);
            }

            NODISCARD OperationWrapper operator,(ConditionCode cond) &&
            {
                conditional = cond;
                return std::move(*this);
            }

            NODISCARD OperationWrapper operator,(SetFlag flags) &&
            {
                setFlags = flags;
                return std::move(*this);
            }

            NODISCARD OperationWrapper operator,(intermediate::InstructionDecorations deco) &&
            {
                decoration = add_flag(decoration, deco);
                return std::move(*this);
            }

            NODISCARD std::unique_ptr<intermediate::IntermediateInstruction> toInstruction(const Value& result)
            {
                std::unique_ptr<intermediate::UnpackingInstruction> res = nullptr;
                if(op == OP_V8MIN)
                    res = std::make_unique<intermediate::MoveOperation>(result, arg0);
                else if(op.numOperands == 1)
                    res = std::make_unique<intermediate::Operation>(op, result, arg0);
                else
                    res = std::make_unique<intermediate::Operation>(op, result, arg0, arg1.value());
                res->setUnpackMode(unpackMode)
                    .setPackMode(packMode)
                    .setCondition(conditional)
                    .setSetFlags(setFlags)
                    .setSignaling(signal)
                    .addDecorations(decoration);
                return res;
            }
        };

        struct LoadWrapper : private NonCopyable
        {
            intermediate::LoadType type;
            Literal value;

            LoadWrapper(Literal value) : type(intermediate::LoadType::REPLICATE_INT32), value(value) {}
            LoadWrapper(uint32_t mask, intermediate::LoadType type) : type(type), value(mask) {}

            Signaling signal = SIGNAL_NONE;
            Pack packMode = PACK_NOP;
            ConditionCode conditional = COND_ALWAYS;
            SetFlag setFlags = SetFlag::DONT_SET;
            intermediate::InstructionDecorations decoration = intermediate::InstructionDecorations::NONE;

            NODISCARD LoadWrapper operator,(Signaling sig) &&
            {
                signal = sig;
                return std::move(*this);
            }

            NODISCARD LoadWrapper operator,(Pack pack) &&
            {
                packMode = pack;
                return std::move(*this);
            }

            NODISCARD LoadWrapper operator,(ConditionCode cond) &&
            {
                conditional = cond;
                return std::move(*this);
            }

            NODISCARD LoadWrapper operator,(SetFlag flags) &&
            {
                setFlags = flags;
                return std::move(*this);
            }

            NODISCARD LoadWrapper operator,(intermediate::InstructionDecorations deco) &&
            {
                decoration = add_flag(decoration, deco);
                return std::move(*this);
            }

            NODISCARD std::unique_ptr<intermediate::IntermediateInstruction> toInstruction(const Value& result)
            {
                std::unique_ptr<intermediate::LoadImmediate> res = nullptr;
                if(type == intermediate::LoadType::REPLICATE_INT32)
                    res = std::make_unique<intermediate::LoadImmediate>(result, value);
                else
                    res = std::make_unique<intermediate::LoadImmediate>(result, value.unsignedInt(), type);
                res->setPackMode(packMode)
                    .setCondition(conditional)
                    .setSetFlags(setFlags)
                    .setSignaling(signal)
                    .addDecorations(decoration);
                return res;
            }
        };

        struct ComparisonWrapper : private NonCopyable
        {
            ConditionCode code;
            Value arg0;
            Value arg1;
            using FuncType = FunctionPointer<void(
                InstructionWalker&, const Value&, const Value&, const Value&, intermediate::InstructionDecorations)>;
            FuncType func;
            intermediate::InstructionDecorations decoration = intermediate::InstructionDecorations::NONE;

            ComparisonWrapper(ConditionCode code, const Value& arg0, const Value& arg1, const FuncType& func) :
                code(code), arg0(arg0), arg1(arg1), func(func)
            {
            }

            NODISCARD inline ConditionCode operator()(InstructionWalker& it, const Value& out) const
            {
                func(it, out, arg0, arg1, decoration);
                return code;
            }

            NODISCARD ComparisonWrapper operator,(intermediate::InstructionDecorations deco) &&
            {
                decoration = add_flag(decoration, deco);
                return std::move(*this);
            }
        };

        struct comp_signed
        {
        };
        struct comp_unsigned
        {
        };
        struct comp_float
        {
        };

        template <typename T>
        struct as_type
        {
            const Value& val;
        };

        using as_unsigned = as_type<comp_unsigned>;
        using as_signed = as_type<comp_signed>;
        using as_float = as_type<comp_float>;

        NODISCARD inline OperationWrapper operator-(as_float&& arg)
        {
            return OperationWrapper{OP_FSUB, FLOAT_ZERO, arg.val};
        }
        NODISCARD inline OperationWrapper operator-(as_signed&& arg)
        {
            return OperationWrapper{OP_SUB, INT_ZERO, arg.val};
        }

        NODISCARD inline OperationWrapper operator+(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType() != arg2.type.isFloatingType())
                throw CompilationError(CompilationStep::GENERAL, "Cannot add floating-point and integer values");
            if(arg1.type.isFloatingType() && arg2.type.isFloatingType())
                return OperationWrapper{OP_FADD, arg1, arg2};
            return OperationWrapper{OP_ADD, arg1, arg2};
        }

        NODISCARD inline OperationWrapper operator+(as_float&& arg1, as_float&& arg2)
        {
            return OperationWrapper{OP_FADD, arg1.val, arg2.val};
        }

        NODISCARD inline OperationWrapper operator+(as_signed&& arg1, as_signed&& arg2)
        {
            return OperationWrapper{OP_ADD, arg1.val, arg2.val};
        }

        NODISCARD inline OperationWrapper operator-(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType() != arg2.type.isFloatingType())
                throw CompilationError(CompilationStep::GENERAL, "Cannot subtract floating-point and integer values");
            if(arg1.type.isFloatingType() && arg2.type.isFloatingType())
                return OperationWrapper{OP_FSUB, arg1, arg2};
            return OperationWrapper{OP_SUB, arg1, arg2};
        }

        NODISCARD inline OperationWrapper operator-(as_float&& arg1, as_float&& arg2)
        {
            return OperationWrapper{OP_FSUB, arg1.val, arg2.val};
        }

        NODISCARD inline OperationWrapper operator-(as_signed&& arg1, as_signed&& arg2)
        {
            return OperationWrapper{OP_SUB, arg1.val, arg2.val};
        }

        NODISCARD inline OperationWrapper operator*(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType() && arg2.type.isFloatingType())
                return OperationWrapper{OP_FMUL, arg1, arg2};
            if(arg1.type.isIntegralType() && arg1.type.getScalarBitCount() <= 24 && arg2.type.isIntegralType() &&
                arg2.type.getScalarBitCount() <= 24)
                return OperationWrapper{OP_MUL24, arg1, arg2};
            throw CompilationError(CompilationStep::GENERAL, "Invalid operand types for multiplication");
        }

        NODISCARD inline OperationWrapper operator*(as_float&& arg1, as_float&& arg2)
        {
            return OperationWrapper{OP_FMUL, arg1.val, arg2.val};
        }

        NODISCARD inline OperationWrapper operator*(const Value& arg1, const Literal& arg2)
        {
            if(!arg1.type.isIntegralType())
                throw CompilationError(
                    CompilationStep::GENERAL, "Invalid operand type for multiplication", arg1.to_string());
            if(!isPowerTwo(arg2.unsignedInt()))
                throw CompilationError(CompilationStep::GENERAL,
                    "Can only insert multiplication by constant powers of two", arg2.to_string());
            return OperationWrapper{
                OP_SHL, arg1, Value(Literal(static_cast<int32_t>(std::log2(arg2.unsignedInt()))), TYPE_INT8)};
        }

        NODISCARD inline OperationWrapper operator/(const Value& arg1, const Literal& arg2)
        {
            if(!arg1.type.isIntegralType())
                throw CompilationError(CompilationStep::GENERAL, "Invalid operand type for division", arg1.to_string());
            if(!isPowerTwo(arg2.unsignedInt()))
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only insert division by constant powers of two", arg2.to_string());
            return OperationWrapper{
                OP_SHR, arg1, Value(Literal(static_cast<int32_t>(std::log2(arg2.unsignedInt()))), TYPE_INT8)};
        }

        NODISCARD inline OperationWrapper operator%(const Value& arg1, const Literal& arg2)
        {
            if(!arg1.type.isIntegralType())
                throw CompilationError(CompilationStep::GENERAL, "Invalid operand type for division", arg1.to_string());
            if(!isPowerTwo(arg2.unsignedInt()))
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only insert division by constant powers of two", arg2.to_string());
            return OperationWrapper{OP_AND, arg1, Value(Literal(arg2.unsignedInt() - 1), TYPE_INT8)};
        }

        NODISCARD inline OperationWrapper operator!(const Value& arg)
        {
            if(!arg.type.isIntegralType() || arg.type.getElementType() != TYPE_BOOL)
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only logically invert boolean values", arg.to_string());
            return OperationWrapper{OP_XOR, arg, BOOL_TRUE};
        }

        NODISCARD inline OperationWrapper operator&&(const Value& arg1, const Value& arg2)
        {
            if(!arg1.type.isIntegralType() || arg1.type.getElementType() != TYPE_BOOL)
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only logically AND boolean values", arg1.to_string());
            if(!arg2.type.isIntegralType() || arg2.type.getElementType() != TYPE_BOOL)
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only logically AND boolean values", arg2.to_string());
            return OperationWrapper{OP_AND, arg1, arg2};
        }

        NODISCARD inline OperationWrapper operator||(const Value& arg1, const Value& arg2)
        {
            if(!arg1.type.isIntegralType() || arg1.type.getElementType() != TYPE_BOOL)
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only logically OR boolean values", arg1.to_string());
            if(!arg2.type.isIntegralType() || arg2.type.getElementType() != TYPE_BOOL)
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only logically OR boolean values", arg2.to_string());
            return OperationWrapper{OP_OR, arg1, arg2};
        }

        NODISCARD inline OperationWrapper operator~(const Value& arg)
        {
            return OperationWrapper{OP_NOT, arg};
        }

        NODISCARD inline OperationWrapper operator&(const Value& arg1, const Value& arg2)
        {
            return OperationWrapper{OP_AND, arg1, arg2};
        }

        NODISCARD inline OperationWrapper operator|(const Value& arg1, const Value& arg2)
        {
            return OperationWrapper{OP_OR, arg1, arg2};
        }

        NODISCARD inline OperationWrapper operator^(const Value& arg1, const Value& arg2)
        {
            return OperationWrapper{OP_XOR, arg1, arg2};
        }

        NODISCARD inline OperationWrapper operator<<(const Value& arg1, const Value& arg2)
        {
            return OperationWrapper{OP_SHL, arg1, arg2};
        }

        // signed right shift, does arithmetic shift
        NODISCARD inline OperationWrapper operator>>(as_signed&& arg1, const Value& arg2)
        {
            return OperationWrapper{OP_ASR, arg1.val, arg2};
        }

        // unsigned right shift, does bit-shift
        NODISCARD inline OperationWrapper operator>>(as_unsigned&& arg1, const Value& arg2)
        {
            return OperationWrapper{OP_SHR, arg1.val, arg2};
        }

        NODISCARD inline OperationWrapper operator,(const Value& src, Signaling sig)
        {
            return OperationWrapper{OP_V8MIN, src}, sig;
        }

        NODISCARD inline OperationWrapper operator,(const Value& src, Unpack unpack)
        {
            return OperationWrapper{OP_V8MIN, src}, unpack;
        }

        NODISCARD inline OperationWrapper operator,(const Value& src, Pack pack)
        {
            return OperationWrapper{OP_V8MIN, src}, pack;
        }

        NODISCARD inline OperationWrapper operator,(const Value& src, ConditionCode cond)
        {
            return OperationWrapper{OP_V8MIN, src}, cond;
        }

        NODISCARD inline OperationWrapper operator,(const Value& src, SetFlag flags)
        {
            return OperationWrapper{OP_V8MIN, src}, flags;
        }

        NODISCARD inline OperationWrapper operator,(const Value& src, intermediate::InstructionDecorations deco)
        {
            return OperationWrapper{OP_V8MIN, src}, deco;
        }

        NODISCARD inline OperationWrapper max(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType() && arg2.type.isFloatingType())
                return OperationWrapper{OP_FMAX, arg1, arg2};
            if(arg1.type.isIntegralType() && arg2.type.isIntegralType())
                return OperationWrapper{OP_MAX, arg1, arg2};
            throw CompilationError(CompilationStep::GENERAL, "Invalid operand types for max operator");
        }

        NODISCARD inline OperationWrapper max(as_float&& arg1, as_float&& arg2)
        {
            return OperationWrapper{OP_FMAX, arg1.val, arg2.val};
        }

        NODISCARD inline OperationWrapper max(as_signed&& arg1, as_signed&& arg2)
        {
            return OperationWrapper{OP_MAX, arg1.val, arg2.val};
        }

        NODISCARD inline OperationWrapper min(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType() && arg2.type.isFloatingType())
                return OperationWrapper{OP_FMIN, arg1, arg2};
            if(arg1.type.isIntegralType() && arg2.type.isIntegralType())
                return OperationWrapper{OP_MIN, arg1, arg2};
            throw CompilationError(CompilationStep::GENERAL, "Invalid operand types for min operator");
        }

        NODISCARD inline OperationWrapper min(as_float&& arg1, as_float&& arg2)
        {
            return OperationWrapper{OP_FMIN, arg1.val, arg2.val};
        }

        NODISCARD inline OperationWrapper min(as_signed&& arg1, as_signed&& arg2)
        {
            return OperationWrapper{OP_MIN, arg1.val, arg2.val};
        }

        NODISCARD inline OperationWrapper mul24(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType())
                throw CompilationError(
                    CompilationStep::GENERAL, "Cannot apply mul24 operator to floating point values", arg1.to_string());
            if(arg2.type.isFloatingType())
                throw CompilationError(
                    CompilationStep::GENERAL, "Cannot apply mul24 operator to floating point values", arg2.to_string());
            return OperationWrapper{OP_MUL24, arg1, arg2};
        }

        NODISCARD inline OperationWrapper itof(const Value& arg)
        {
            return OperationWrapper{OP_ITOF, arg};
        }

        NODISCARD inline OperationWrapper ftoi(const Value& arg)
        {
            return OperationWrapper{OP_FTOI, arg};
        }

        NODISCARD inline OperationWrapper clz(const Value& arg)
        {
            return (OperationWrapper{OP_CLZ, arg}, intermediate::InstructionDecorations::UNSIGNED_RESULT);
        }

        NODISCARD inline LoadWrapper load(Literal value)
        {
            return LoadWrapper{value};
        }

        NODISCARD inline LoadWrapper load(uint32_t mask, intermediate::LoadType type)
        {
            return LoadWrapper{mask, type};
        }

        struct AssignmentWrapper : private NonCopyable
        {
            InstructionWalker& it;
            const Value& result;

            AssignmentWrapper(InstructionWalker& it, const Value& out) : it(it), result(out) {}

            void operator=(OperationWrapper&& op) &&
            {
                it.emplace(op.toInstruction(result));
                it.nextInBlock();
            }

            void operator=(LoadWrapper&& op) &&
            {
                it.emplace(op.toInstruction(result));
                it.nextInBlock();
            }

            void operator=(const Value& src) &&
            {
                it.emplace(std::make_unique<intermediate::MoveOperation>(result, src));
                it.nextInBlock();
            }

            void operator=(Value&& src) &&
            {
                it.emplace(std::make_unique<intermediate::MoveOperation>(Value(result), std::move(src)));
                it.nextInBlock();
            }

            NODISCARD ConditionCode operator=(ComparisonWrapper&& op) &&
            {
                return op(it, result);
            }
        };

        struct ValueWrapper : private NonCopyable
        {
            Method& method;
            InstructionWalker& it;
            DataType type;
            std::string name;

            ValueWrapper(Method& method, InstructionWalker& it, DataType type, std::string&& name) :
                method(method), it(it), type(type), name(std::move(name))
            {
            }

            NODISCARD Value operator=(OperationWrapper&& op) &&
            {
                if(type.isUnknown())
                {
                    // auto-deduce type
                    type = op.op.returnsFloat ? TYPE_FLOAT : TYPE_INT32;
                    auto arg0Mask = op.unpackMode(op.arg0.getReadMask(), op.op.acceptsFloat);
                    auto arg1Mask = op.arg1 ? op.unpackMode(op.arg1->getReadMask(), op.op.acceptsFloat) : BITMASK_NONE;
                    auto mask = op.packMode(op.op(arg0Mask, arg1Mask).resultMask);
                    if(mask.getUpperHalfWord() == 0)
                        type = op.op.returnsFloat ? TYPE_HALF : TYPE_INT16;
                    if(mask.getByte3() == 0 && mask.getByte2() == 0 && mask.getByte1() == 0)
                        type = TYPE_INT8;
                    auto secondWidth = op.arg1 ? op.arg1->type.getVectorWidth() : uint8_t{0};
                    type = type.toVectorType(std::max(op.arg0.type.getVectorWidth(), secondWidth));
                }
                if(op.setFlags != SetFlag::SET_FLAGS && !op.signal.hasSideEffects() &&
                    (!op.arg0.checkRegister() || !op.arg0.reg().hasSideEffectsOnRead()) &&
                    (!op.arg1 || !op.arg1->checkRegister() || !op.arg1->reg().hasSideEffectsOnRead()) &&
                    /* to not override either-or where either is dynamic and or is constant */
                    op.conditional == COND_ALWAYS &&
                    /* XXX this is not necessary, but we don't known which inputs to unpack */
                    !op.unpackMode.hasEffect())
                {
                    auto precalc = op.op(op.arg0, op.arg1);
                    if(precalc.first)
                    {
                        auto tmp = op.packMode(precalc.first.value(), precalc.second).value();
                        // so the result type matches the expected type
                        tmp.type = type;
                        return tmp;
                    }
                }
                auto result = method.addNewLocal(type, name);
                it.emplace(op.toInstruction(result));
                // we need to set pointer to next, so it is consistent with behavior if pre-calculation succeeded
                it.nextInBlock();
                return result;
            }

            NODISCARD Value operator=(const Value& src) &&
            {
                auto result = method.addNewLocal(type, name);
                it.emplace(std::make_unique<intermediate::MoveOperation>(result, src));
                it.nextInBlock();
                return result;
            }

            NODISCARD Value operator=(Value&& src) &&
            {
                auto result = method.addNewLocal(type, name);
                it.emplace(std::make_unique<intermediate::MoveOperation>(Value(result), std::move(src)));
                it.nextInBlock();
                return result;
            }
        };

        /**
         * Inserts an instruction that assigns the output of the given calculation to the given Value.
         *
         * NOTE: The InstructionWalker is automatically incremented
         */
        NODISCARD inline AssignmentWrapper assign(InstructionWalker& it, const Value& out)
        {
            return AssignmentWrapper{it, out};
        }

        /**
         * Inserts an instruction that assigns the output of the given calculation to the Value returned.
         * If the operation can be calculated on compile-time, the direct result will be returned and no instruction
         * will be inserted.
         *
         * NOTE: The InstructionWalker is automatically incremented (iff an instruction is generated)
         */
        NODISCARD inline ValueWrapper assign(InstructionWalker& it, DataType type, std::string&& name = "")
        {
            return ValueWrapper{it.getBasicBlock()->getMethod(), it, type, std::move(name)};
        }

        /**
         * Inserts an instruction that assigns the output of the given calculation to the Value returned.
         * If the operation can be calculated on compile-time, the direct result will be returned and no instruction
         * will be inserted.
         *
         * NOTE: The InstructionWalker is automatically incremented (iff an instruction is generated)
         */
        NODISCARD inline ValueWrapper assign(InstructionWalker& it, std::string&& name = "")
        {
            return ValueWrapper{it.getBasicBlock()->getMethod(), it, TYPE_UNKNOWN, std::move(name)};
        }

        /**
         * Inserts an instruction that assigns the output of the given calculation to the given Value.
         *
         * NOTE: The InstructionWalker is automatically incremented
         */
        NODISCARD inline AssignmentWrapper assignNop(InstructionWalker& it)
        {
            return AssignmentWrapper{it, NOP_REGISTER};
        }

        /**
         * Inserts a nop instruction optionally triggering the given signal
         *
         * NOTE: The InstructionWalker is automatically incremented
         */
        inline void nop(InstructionWalker& it, intermediate::DelayType type, Signaling signal = SIGNAL_NONE)
        {
            it.emplace(std::make_unique<intermediate::Nop>(type, signal));
            it.nextInBlock();
        }

        template <typename T>
        NODISCARD inline ComparisonWrapper operator==(as_type<T>&& a, as_type<T>&& b)
        {
            // a == b <=> a xor b == 0 [<=> a - b == 0]
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                if(arg1.hasLiteral(Literal(0u)))
                    // special case for a == 0
                    // does not save instructions, but does not force value a to be on register-file A (since B is
                    // reserved for literal 0)
                    assign(it, out) = (arg0, SetFlag::SET_FLAGS, deco);
                else if(arg0.hasLiteral(Literal(0u)))
                    assign(it, out) = (arg1, SetFlag::SET_FLAGS, deco);
                else
                    assign(it, out) = (arg0 ^ arg1, SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_ZERO_SET, a.val, b.val, func};
        }

        template <typename T>
        NODISCARD inline ComparisonWrapper operator!=(as_type<T>&& a, as_type<T>&& b)
        {
            // a != b <=> a xor b != 0
            auto tmp = (std::forward<as_type<T>>(a) == std::forward<as_type<T>>(b));
            return ComparisonWrapper{tmp.code.invert(), a.val, b.val, ComparisonWrapper::FuncType{tmp.func}};
        }

        NODISCARD inline ComparisonWrapper operator>(as_signed&& a, as_signed&& b)
        {
            // min/max(a, b) set flag carry if a > b
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                if(!arg0.type.isIntegralType() || !arg1.type.isIntegralType())
                    throw CompilationError(CompilationStep::GENERAL, "Can only compare integer values");
                assign(it, out) = (max(arg0, arg1), SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_CARRY_SET, a.val, b.val, func};
        }

        NODISCARD inline ComparisonWrapper operator>(as_float&& a, as_float&& b)
        {
            // fmin/fmax(a, b) set flag carry if a > b
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                if(!arg0.type.isFloatingType() || !arg1.type.isFloatingType())
                    throw CompilationError(CompilationStep::GENERAL, "Can only compare float values");
                assign(it, out) = (max(arg0, arg1), SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_CARRY_SET, a.val, b.val, func};
        }

        NODISCARD inline ComparisonWrapper operator>=(as_signed&& a, as_signed&& b)
        {
            // min/max(a, b) set flag carry if a > b => min/max(b, a) set carry if a < b
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                if(!arg0.type.isIntegralType() || !arg1.type.isIntegralType())
                    throw CompilationError(CompilationStep::GENERAL, "Can only compare integer values");
                assign(it, out) = (max(arg1, arg0), SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_CARRY_CLEAR, a.val, b.val, func};
        }

        NODISCARD inline ComparisonWrapper operator>=(as_float&& a, as_float&& b)
        {
            // fmin/fmax(a, b) set flag carry if a > b => fmin/fmax(b, a) set carry if a < b
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                if(!arg0.type.isFloatingType() || !arg1.type.isFloatingType())
                    throw CompilationError(CompilationStep::GENERAL, "Can only compare float values");
                assign(it, out) = (max(arg1, arg0), SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_CARRY_CLEAR, a.val, b.val, func};
        }

        NODISCARD inline ComparisonWrapper operator<(as_signed&& a, as_signed&& b)
        {
            // min/max(a, b) set flag carry if a > b => min/max(b, a) set carry if a < b
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                if(!arg0.type.isIntegralType() || !arg1.type.isIntegralType())
                    throw CompilationError(CompilationStep::GENERAL, "Can only compare integer values");
                assign(it, out) = (max(arg1, arg0), SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_CARRY_SET, a.val, b.val, func};
        }

        NODISCARD inline ComparisonWrapper operator<(as_float&& a, as_float&& b)
        {
            // fmin/fmax(a, b) set flag carry if a > b => fmin/fmax(b, a) set carry if a < b
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                if(!arg0.type.isFloatingType() || !arg1.type.isFloatingType())
                    throw CompilationError(CompilationStep::GENERAL, "Can only compare float values");
                assign(it, out) = (max(arg1, arg0), SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_CARRY_SET, a.val, b.val, func};
        }

        NODISCARD inline ComparisonWrapper operator<=(as_signed&& a, as_signed&& b)
        {
            // min/max(a, b) set flag carry if a > b
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                if(!arg0.type.isIntegralType() || !arg1.type.isIntegralType())
                    throw CompilationError(CompilationStep::GENERAL, "Can only compare integer values");
                assign(it, out) = (max(arg0, arg1), SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_CARRY_CLEAR, a.val, b.val, func};
        }

        NODISCARD inline ComparisonWrapper operator<=(as_float&& a, as_float&& b)
        {
            // fmin/fmax(a, b) set flag carry if a > b
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                if(!arg0.type.isFloatingType() || !arg1.type.isFloatingType())
                    throw CompilationError(CompilationStep::GENERAL, "Can only compare float values");
                assign(it, out) = (max(arg0, arg1), SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_CARRY_CLEAR, a.val, b.val, func};
        }

        NODISCARD inline ComparisonWrapper isnan(as_float&& val)
        {
            // VideoCore IV considers NaN > Inf for min/fmax/fminabs/fmaxabs
            // isnan(a) <=> fmaxabs(a, Inf) -> sets carry
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                it.emplace(std::make_unique<intermediate::Operation>(OP_FMAXABS, out, arg0, FLOAT_INF))
                    .setSetFlags(SetFlag::SET_FLAGS)
                    .addDecorations(deco);
                it.nextInBlock();
            };
            return ComparisonWrapper{COND_CARRY_SET, val.val, UNDEFINED_VALUE, func};
        }

        NODISCARD inline ComparisonWrapper isnaninf(as_float&& val)
        {
            // VideoCore IV considers NaN > Inf for min/fmax/fminabs/fmaxabs
            // isinf(a) || isnan(a) <=> fmaxabs(a, highest-float) -> sets carry
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                it.emplace(std::make_unique<intermediate::Operation>(
                               OP_FMAXABS, out, arg0, Value(Literal(0x7F7FFFFFu), TYPE_FLOAT)))
                    .setSetFlags(SetFlag::SET_FLAGS)
                    .addDecorations(deco);
                it.nextInBlock();
            };
            return ComparisonWrapper{COND_CARRY_SET, val.val, UNDEFINED_VALUE, func};
        }

        NODISCARD inline ComparisonWrapper isinf(as_float&& val)
        {
            // VideoCore IV considers NaN > Inf for min/fmax/fminabs/fmaxabs
            // isinf(a) <=> a == Inf || a == -Inf <=> (a & 0x7FFFFFFF) == Inf
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                auto tmp = assign(it, TYPE_INT32.toVectorType(arg0.type.getVectorWidth())) =
                    (arg0 & Value(Literal(0x7FFFFFFFu), TYPE_INT32));
                assign(it, out) = (tmp ^ FLOAT_INF, SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_ZERO_SET, val.val, UNDEFINED_VALUE, func};
        }

        NODISCARD inline ComparisonWrapper iszero(as_float&& val)
        {
            // IEEE 754 considers -0 and 0 to be equal according to the "usual numeric comparisons", see
            // https://en.wikipedia.org/wiki/Signed_zero#Comparisons
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                // check all bits zero except the sign, which we don't care about to also allow for -0
                assign(it, out) = (arg0 & Value(Literal(0x7FFFFFFFu), TYPE_INT32), SetFlag::SET_FLAGS, deco);
            };
            return ComparisonWrapper{COND_ZERO_SET, val.val, UNDEFINED_VALUE, func};
        }

        NODISCARD inline ComparisonWrapper selectSIMDElement(uint8_t index)
        {
            return as_unsigned{ELEMENT_NUMBER_REGISTER} == as_unsigned{Value(SmallImmediate(index), TYPE_INT8)};
        }

        NODISCARD inline ComparisonWrapper selectSIMDElement(const Value& index)
        {
            return as_unsigned{ELEMENT_NUMBER_REGISTER} == as_unsigned{index};
        }

        NODISCARD inline ComparisonWrapper selectSIMDElements(std::bitset<NATIVE_VECTOR_SIZE> indices)
        {
            static const auto func = [](InstructionWalker& it, const Value& out, const Value& arg0, const Value& arg1,
                                         intermediate::InstructionDecorations deco) {
                it.emplace(
                      std::make_unique<intermediate::LoadImmediate>(out, arg0.getLiteralValue().value().unsignedInt(),
                          intermediate::LoadType::PER_ELEMENT_UNSIGNED))
                    .setSetFlags(SetFlag::SET_FLAGS)
                    .addDecorations(deco);
                it.nextInBlock();
            };
            return ComparisonWrapper{COND_ZERO_CLEAR,
                Value(Literal(static_cast<uint32_t>(indices.to_ulong())), TYPE_INT8), UNDEFINED_VALUE, func};
        }
    } // namespace operators
} // namespace vc4c

#endif /* VC4C_INSTRUCTION_OPERATORS_H */
