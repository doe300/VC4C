/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_INSTRUCTION_OPERATORS_H
#define VC4C_INSTRUCTION_OPERATORS_H

#include "../asm/OpCodes.h"
#include "../helper.h"
#include "IntermediateInstruction.h"

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
            const OpCode op;
            const Value arg0;
            const Optional<Value> arg1 = NO_VALUE;

            OperationWrapper(OpCode code, const Value& arg0) : op(code), arg0(arg0) {}
            OperationWrapper(OpCode code, const Value& arg0, const Value& arg1) : op(code), arg0(arg0), arg1(arg1) {}

            Signaling signal = SIGNAL_NONE;
            Unpack unpackMode = UNPACK_NOP;
            Pack packMode = PACK_NOP;
            ConditionCode conditional = COND_ALWAYS;
            SetFlag setFlags = SetFlag::DONT_SET;
            intermediate::InstructionDecorations decoration = intermediate::InstructionDecorations::NONE;

            OperationWrapper operator,(Signaling sig) &&
            {
                signal = sig;
                return std::move(*this);
            }

            OperationWrapper operator,(Unpack unpack) &&
            {
                unpackMode = unpack;
                return std::move(*this);
            }

            OperationWrapper operator,(Pack pack) &&
            {
                packMode = pack;
                return std::move(*this);
            }

            OperationWrapper operator,(ConditionCode cond) &&
            {
                conditional = cond;
                return std::move(*this);
            }

            OperationWrapper operator,(SetFlag flags) &&
            {
                setFlags = flags;
                return std::move(*this);
            }

            OperationWrapper operator,(intermediate::InstructionDecorations deco) &&
            {
                decoration = add_flag(decoration, deco);
                return std::move(*this);
            }

            intermediate::IntermediateInstruction* toInstruction(const Value& result)
            {
                intermediate::IntermediateInstruction* res = nullptr;
                if(op == OP_V8MIN)
                    res = new intermediate::MoveOperation(result, arg0);
                else if(op.numOperands == 1)
                    res = new intermediate::Operation(op, result, arg0);
                else
                    res = new intermediate::Operation(op, result, arg0, arg1.value());
                return res->setSignaling(signal)
                    ->setUnpackMode(unpackMode)
                    ->setPackMode(packMode)
                    ->setCondition(conditional)
                    ->setSetFlags(setFlags)
                    ->addDecorations(decoration);
            }
        };

        inline OperationWrapper operator-(const Value& arg)
        {
            if(arg.type.isFloatingType())
                return OperationWrapper{OP_FSUB, FLOAT_ZERO, arg};
            return OperationWrapper{OP_SUB, INT_ZERO, arg};
        }

        inline OperationWrapper operator+(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType() != arg2.type.isFloatingType())
                throw CompilationError(CompilationStep::GENERAL, "Cannot add floating-point and integer values");
            if(arg1.type.isFloatingType() && arg2.type.isFloatingType())
                return OperationWrapper{OP_FADD, arg1, arg2};
            return OperationWrapper{OP_ADD, arg1, arg2};
        }

        inline OperationWrapper operator-(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType() != arg2.type.isFloatingType())
                throw CompilationError(CompilationStep::GENERAL, "Cannot subtract floating-point and integer values");
            if(arg1.type.isFloatingType() && arg2.type.isFloatingType())
                return OperationWrapper{OP_FSUB, arg1, arg2};
            return OperationWrapper{OP_SUB, arg1, arg2};
        }

        inline OperationWrapper operator*(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType() && arg2.type.isFloatingType())
                return OperationWrapper{OP_FMUL, arg1, arg2};
            if(arg1.type.isIntegralType() && arg1.type.getScalarBitCount() <= 24 && arg2.type.isIntegralType() &&
                arg2.type.getScalarBitCount() <= 24)
                return OperationWrapper{OP_MUL24, arg1, arg2};
            throw CompilationError(CompilationStep::GENERAL, "Invalid operand types for multiplication");
        }

        inline OperationWrapper operator/(const Value& arg1, const Literal& arg2)
        {
            if(!arg1.type.isIntegralType())
                throw CompilationError(CompilationStep::GENERAL, "Invalid operand type for division", arg1.to_string());
            if(!isPowerTwo(arg2.unsignedInt()))
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only insert division by constant powers of two", arg2.to_string());
            return OperationWrapper{
                OP_SHR, arg1, Value(Literal(static_cast<int32_t>(std::log2(arg2.unsignedInt()))), TYPE_INT8)};
        }

        inline OperationWrapper operator%(const Value& arg1, const Literal& arg2)
        {
            if(!arg1.type.isIntegralType())
                throw CompilationError(CompilationStep::GENERAL, "Invalid operand type for division", arg1.to_string());
            if(!isPowerTwo(arg2.unsignedInt()))
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only insert division by constant powers of two", arg2.to_string());
            return OperationWrapper{OP_AND, arg1, Value(Literal(arg2.unsignedInt() - 1), TYPE_INT8)};
        }

        inline OperationWrapper operator!(const Value& arg)
        {
            if(!arg.type.isIntegralType() || arg.type.getElementType() != TYPE_BOOL)
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only logically invert boolean values", arg.to_string());
            return OperationWrapper{OP_XOR, arg, BOOL_TRUE};
        }

        inline OperationWrapper operator&&(const Value& arg1, const Value& arg2)
        {
            if(!arg1.type.isIntegralType() || arg1.type.getElementType() != TYPE_BOOL)
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only logically AND boolean values", arg1.to_string());
            if(!arg2.type.isIntegralType() || arg2.type.getElementType() != TYPE_BOOL)
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only logically AND boolean values", arg2.to_string());
            return OperationWrapper{OP_AND, arg1, arg2};
        }

        inline OperationWrapper operator||(const Value& arg1, const Value& arg2)
        {
            if(!arg1.type.isIntegralType() || arg1.type.getElementType() != TYPE_BOOL)
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only logically OR boolean values", arg1.to_string());
            if(!arg2.type.isIntegralType() || arg2.type.getElementType() != TYPE_BOOL)
                throw CompilationError(
                    CompilationStep::GENERAL, "Can only logically OR boolean values", arg2.to_string());
            return OperationWrapper{OP_OR, arg1, arg2};
        }

        inline OperationWrapper operator~(const Value& arg)
        {
            return OperationWrapper{OP_NOT, arg};
        }

        inline OperationWrapper operator&(const Value& arg1, const Value& arg2)
        {
            return OperationWrapper{OP_AND, arg1, arg2};
        }

        inline OperationWrapper operator|(const Value& arg1, const Value& arg2)
        {
            return OperationWrapper{OP_OR, arg1, arg2};
        }

        inline OperationWrapper operator^(const Value& arg1, const Value& arg2)
        {
            return OperationWrapper{OP_XOR, arg1, arg2};
        }

        inline OperationWrapper operator<<(const Value& arg1, const Value& arg2)
        {
            return OperationWrapper{OP_SHL, arg1, arg2};
        }

        inline OperationWrapper operator>>(const Value& arg1, const Value& arg2)
        {
            // TODO could also be ASR for signed numbers!
            return OperationWrapper{OP_SHR, arg1, arg2};
        }

        inline OperationWrapper operator,(const Value& src, Signaling sig)
        {
            return OperationWrapper{OP_V8MIN, src}, sig;
        }

        inline OperationWrapper operator,(const Value& src, Unpack unpack)
        {
            return OperationWrapper{OP_V8MIN, src}, unpack;
        }

        inline OperationWrapper operator,(const Value& src, Pack pack)
        {
            return OperationWrapper{OP_V8MIN, src}, pack;
        }

        inline OperationWrapper operator,(const Value& src, ConditionCode cond)
        {
            return OperationWrapper{OP_V8MIN, src}, cond;
        }

        inline OperationWrapper operator,(const Value& src, SetFlag flags)
        {
            return OperationWrapper{OP_V8MIN, src}, flags;
        }

        inline OperationWrapper operator,(const Value& src, intermediate::InstructionDecorations deco)
        {
            return OperationWrapper{OP_V8MIN, src}, deco;
        }

        // TODO equality operators? Generates multiple instructions

        inline OperationWrapper max(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType() && arg2.type.isFloatingType())
                return OperationWrapper{OP_FMAX, arg1, arg2};
            if(arg1.type.isIntegralType() && arg2.type.isIntegralType())
                return OperationWrapper{OP_MAX, arg1, arg2};
            throw CompilationError(CompilationStep::GENERAL, "Invalid operand types for max operator");
        }

        inline OperationWrapper min(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType() && arg2.type.isFloatingType())
                return OperationWrapper{OP_FMIN, arg1, arg2};
            if(arg1.type.isIntegralType() && arg2.type.isIntegralType())
                return OperationWrapper{OP_MIN, arg1, arg2};
            throw CompilationError(CompilationStep::GENERAL, "Invalid operand types for min operator");
        }

        inline OperationWrapper mul24(const Value& arg1, const Value& arg2)
        {
            if(arg1.type.isFloatingType())
                throw CompilationError(
                    CompilationStep::GENERAL, "Cannot apply mul24 operator to floating point values", arg1.to_string());
            if(arg2.type.isFloatingType())
                throw CompilationError(
                    CompilationStep::GENERAL, "Cannot apply mul24 operator to floating point values", arg2.to_string());
            return OperationWrapper{OP_MUL24, arg1, arg2};
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

            void operator=(const Value& src) &&
            {
                it.emplace(new intermediate::MoveOperation(result, src));
                it.nextInBlock();
            }
        };

        struct ValueWrapper : private NonCopyable
        {
            Method& method;
            InstructionWalker& it;
            DataType type;
            std::string name;

            ValueWrapper(Method& method, InstructionWalker& it, const DataType& type, std::string&& name) :
                method(method), it(it), type(type), name(std::forward<std::string>(name))
            {
            }

            Value operator=(OperationWrapper&& op) &&
            {
                if(op.setFlags != SetFlag::SET_FLAGS && !op.signal.hasSideEffects() &&
                    (!op.arg0.hasRegister() || !op.arg0.reg().hasSideEffectsOnRead()) &&
                    (!op.arg1 || !op.arg1->hasRegister() || !op.arg1->reg().hasSideEffectsOnRead()) &&
                    /* to not override either-or where either is dynamic and or is constant */
                    op.conditional == COND_ALWAYS &&
                    /* XXX this is not necessary, but we don't known which inputs to unpack */
                    op.unpackMode == UNPACK_NOP)
                {
                    auto precalc = op.op(op.arg0, op.arg1);
                    if(precalc)
                        return op.packMode.pack(precalc.value()).value();
                }
                auto result = method.addNewLocal(type, name);
                it.emplace(op.toInstruction(result));
                // we need to set pointer to next, so it is consistent with behavior if pre-calculation succeeded
                it.nextInBlock();
                return result;
            }

            Value operator=(const Value& src) &&
            {
                auto result = method.addNewLocal(type, name);
                it.emplace(new intermediate::MoveOperation(result, src));
                it.nextInBlock();
                return result;
            }
        };

        inline AssignmentWrapper assign(InstructionWalker& it, const Value& out)
        {
            // TODO document that iterator is incremented (opposite to everywhere else)!
            // TODO document this version always inserts, beneath version precalculates
            return AssignmentWrapper{it, out};
        }

        inline ValueWrapper assign(InstructionWalker& it, const DataType& type, std::string&& name = "")
        {
            // TODO document that iterator is incremented (opposite to everywhere else)!
            return ValueWrapper{it.getBasicBlock()->getMethod(), it, type, std::forward<std::string>(name)};
        }

        inline void nop(InstructionWalker& it, intermediate::DelayType type, Signaling signal = SIGNAL_NONE)
        {
            it.emplace(new intermediate::Nop(type));
            it->signal = signal;
            it.nextInBlock();
        }
    }
}

#endif /* VC4C_INSTRUCTION_OPERATORS_H */
