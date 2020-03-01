/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LongOperations.h"

#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/TypeConversions.h"
#include "../intermediate/operators.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::operators;

static void lowerFlags(Method& method, InstructionWalker it, const MultiRegisterData& output, FlagBehavior flagBehavior)
{
    auto flagType = TYPE_INT32.toVectorType(output.lower->type.getVectorWidth());
    auto flagIt = it.copy().nextInBlock();
    // Need to combine the flags into single instruction -> for all possible combinations, find instruction
    // which produces those flags behavior...
    if(has_flag(flagBehavior,
           add_flag(FlagBehavior::ZERO_ALL_ZEROS, FlagBehavior::NEGATIVE_MSB_SET, FlagBehavior::CARRY_NEVER)))
    {
        /*
         * This is e.g. the behavior of AND, OR, XOR, etc.
         *
         * - OR'ing upper and lower parts together gives the zero-check
         * - for negative check, need to only consider high bit of upper part
         * => compress lower part to never set the high bit
         */
        auto tmp0 = assign(flagIt, flagType) = as_unsigned{output.lower->createReference()} >> 1_val;
        // since we shift the last bit of the lower part out, need to separately add it again back in
        auto tmp1 = assign(flagIt, flagType) = output.lower->createReference() & 1_val;
        auto tmp2 = assign(flagIt, flagType) = tmp0 | tmp1;
        assignNop(flagIt) = (tmp2 | output.upper->createReference(), SetFlag::SET_FLAGS);
    }
    else
        throw CompilationError(CompilationStep::NORMALIZER, "Unhandled flag behavior for 64-bit operation");
}

static void lowerLongOperation(
    Method& method, InstructionWalker it, intermediate::Operation& op, const Configuration& config)
{
    auto outLocal = op.checkOutputLocal();
    auto out = Local::getLocalData<MultiRegisterData>(outLocal);
    if(!out && op.getOutput() != NOP_REGISTER)
        throw CompilationError(CompilationStep::NORMALIZER,
            "Can only calculate with 64-bit values into long locals or NOP register", op.to_string());
    Value in0Low = op.getFirstArg();
    Value in0Up = INT_ZERO;
    if(auto data = Local::getLocalData<MultiRegisterData>(in0Low.checkLocal()))
    {
        in0Low = data->lower->createReference();
        in0Up = data->upper->createReference();
    }
    auto in1Low = op.getSecondArg();
    Optional<Value> in1Up{};
    if(auto data = Local::getLocalData<MultiRegisterData>(in1Low & &Value::checkLocal))
    {
        in1Low = data->lower->createReference();
        in1Up = data->upper->createReference();
    }
    else if(in1Low & &Value::getLiteralValue)
    {
        if(in1Low->getLiteralValue()->type == LiteralType::LONG_LEADING_ONES)
            in1Up = INT_MINUS_ONE;
        else
            in1Up = INT_ZERO;
    }
    else
    {
        // TODO for the upper part for e.g. literals/vectors, how to know whether it should zero-extended or sign-extend
        // the lower part? or store 64-bit literal?? Would increase Literal size by 1/3 for rare uses!
        in1Up = INT_ZERO;
    }

    if(in0Low.type.getScalarBitCount() > 32)
        // For literal/vector operands, just truncate the type (e.g. i64 32 to i32 32)
        // This saves us from needing to convert the writer(s) of the literal/vector
        in0Low.type = TYPE_INT32.toVectorType(in0Low.type.getVectorWidth());

    if(in1Low && in1Low->type.getScalarBitCount() > 32)
        // For literal/vector operands, just truncate the type (e.g. i64 32 to i32 32)
        // This saves us from needing to convert the writer(s) of the literal/vector
        in1Low->type = TYPE_INT32.toVectorType(in1Low->type.getVectorWidth());

    if(!outLocal && !out)
    {
        // If the output is NOP, we still need an output value to be able to calculate the flags
        outLocal = method.addNewLocal(TYPE_INT64.toVectorType(op.getFirstArg().type.getVectorWidth())).checkLocal();
        out = Local::getLocalData<MultiRegisterData>(outLocal);
    }

    if(op.op == OP_ADD)
    {
        /*
         * 64-bit add is:
         * %out.lower = %a.lower + %b.lower (set carry flag)
         * %tmp = %a.upper + %b.upper
         * %out.upper = %tmp + carry bit
         */
        assign(it, out->lower->createReference()) = (in0Low + in1Low.value(), SetFlag::SET_FLAGS, op.decoration);
        assign(it, out->upper->createReference()) = (in0Up + in1Up.value(), op.decoration);
        op.setOutput(out->upper->createReference());
        op.setArgument(0, out->upper->createReference());
        op.setArgument(1, INT_ONE);
        op.setCondition(COND_CARRY_SET);
    }
    else if(op.op == OP_SUB)
    {
        // %a - %b => %a + -%b
        // build two's complement by inverting all bits and adding one
        auto tmp = assign(it, op.getSecondArg()->type) = (~op.getSecondArg().value(), op.conditional, op.decoration);
        auto firstIt = it.copy().previousInBlock();
        tmp = assign(it, op.getSecondArg()->type) = (tmp + 1_val, op.conditional, op.decoration);
        auto secondIt = it.copy().previousInBlock();
        op.op = OP_ADD;
        op.setArgument(1, tmp);
        // do the actual lowering for the newly added bitwise NOT and ADD operations
        if(auto firstOp = firstIt.get<intermediate::Operation>())
            lowerLongOperation(method, firstIt, *firstOp, config);
        if(auto secondOp = secondIt.get<intermediate::Operation>())
            lowerLongOperation(method, secondIt, *secondOp, config);
        lowerLongOperation(method, it, op, config);
    }
    else if(op.op == OP_NOT)
    {
        assign(it, out->lower->createReference()) = (~in0Low, op.conditional, op.decoration);
        op.setOutput(out->upper->createReference());
        op.setArgument(0, in0Up);
    }
    else if(op.op == OP_AND || op.op == OP_OR || op.op == OP_XOR)
    {
        it.emplace(new intermediate::Operation(op.op, out->lower->createReference(), in0Low, in1Low.value()));
        it->copyExtrasFrom(&op);
        it.nextInBlock();
        op.setOutput(out->upper->createReference());
        op.setArgument(0, in0Up);
        op.setArgument(1, in1Up.value());
        // TODO this is wrong when flags are set!
    }
    else if(op.op == OP_SHR || op.op == OP_ASR)
    {
        auto offsetType = TYPE_INT8.toVectorType(outLocal->type.getVectorWidth());
        it.emplace(new intermediate::Operation(OP_MIN, NOP_REGISTER, 32_val, in1Low.value()));
        it->setSetFlags(SetFlag::SET_FLAGS);
        it.nextInBlock();
        auto cond = COND_CARRY_CLEAR;
        auto zeroCond = COND_ZERO_SET;
        /*
         * For offset >= 32 we can simplify:
         * %out.lower = %a.upper >> (offset - 32)
         * %out.upper = 0 (or sign-extension)
         *
         * Since offset is between 32 and 63, the effective offset fits into the lower 5 bits actually used by the QPU!
         */
        auto offset = assign(it, offsetType) = (in1Low.value() - 32_val, cond);
        if(op.op == OP_ASR)
        {
            assign(it, out->lower->createReference()) = (as_signed{in0Up} >> offset, cond);
            // fill up with sign, all zeroes of high bit was zero, all ones otherwise
            assign(it, out->upper->createReference()) = (as_signed{in0Up} >> 31_val, cond);
        }
        else
        {
            assign(it, out->lower->createReference()) = (as_unsigned{in0Up} >> offset, cond);
            assign(it, out->upper->createReference()) = (INT_ZERO, cond);
        }

        /*
         * For offset < 32 we need to shift both parts:
         * %low = %a.lower >> offset
         * %high = %a.upper << (32 - offset)
         * %out.lower = %low | %high
         * %out.upper = %a.upper >> offset
         *
         * Since we have offset < 32 it also fits into the lower 5 bits actually used by the QPU!
         */
        cond = cond.invert();
        auto tmpLow = assign(it, out->lower->type, "%long_shift") = (as_unsigned{in0Low} >> in1Low.value(), cond);
        auto tmpOffset = assign(it, offsetType, "%long_shift") = (32_val - in1Low.value(), cond);
        auto tmpUp = assign(it, out->upper->type, "%long_shift") = (in0Up << tmpOffset, cond);
        // special handling required since for offset of zero, we need to skip OR'ing the upper part to the lower part
        assign(it, tmpUp) = (INT_ZERO, zeroCond);
        assign(it, out->lower->createReference()) = (tmpLow | tmpUp, cond);
        op.setOutput(out->upper->createReference());
        op.setArgument(0, in0Up);
        op.setArgument(1, in1Low.value());
        op.setCondition(cond);
    }
    else if(op.op == OP_SHL)
    {
        auto offsetType = TYPE_INT8.toVectorType(outLocal->type.getVectorWidth());
        it.emplace(new intermediate::Operation(OP_MIN, NOP_REGISTER, 32_val, in1Low.value()));
        it->setSetFlags(SetFlag::SET_FLAGS);
        it.nextInBlock();
        auto cond = COND_CARRY_CLEAR;
        auto zeroCond = COND_ZERO_SET;
        /*
         * For offset >= 32 we can simplify:
         * %out.lower = 0
         * %out.upper = %a.lower << (offset - 32)
         *
         * Since offset is between 32 and 63, the effective offset fits into the lower 5 bits actually used by the QPU!
         */
        auto offset = assign(it, offsetType, "%long_shift") = (in1Low.value() - 32_val, cond);
        assign(it, out->lower->createReference()) = (INT_ZERO, cond);
        assign(it, out->upper->createReference()) = (in0Low << offset, cond);
        /*
         * For offset < 32 we need to shift both parts:
         * %out.lower = %a.lower << offset
         * %low = %a.lower >> (32 - offset)
         * %high = %a.upper << offset
         * %out.upper = %low | %high
         *
         * Since we have offset < 32 it also fits into the lower 5 bits actually used by the QPU!
         */
        cond = cond.invert();
        assign(it, out->lower->createReference()) = (in0Low << in1Low.value(), cond);
        auto tmpOffset = assign(it, offsetType, "%long_shift") = (32_val - in1Low.value(), cond);
        auto tmpLow = assign(it, out->lower->type, "%long_shift") = (as_unsigned{in0Low} >> tmpOffset, cond);
        // special handling required since for offset of zero, we need to skip OR'ing the lower part to the upper part
        assign(it, tmpLow) = (INT_ZERO, zeroCond);
        auto tmpUp = assign(it, out->upper->type, "%long_shift") = (in0Up << in1Low.value(), cond);
        op.setOutput(out->upper->createReference());
        op.op = OP_OR;
        op.setArgument(0, tmpLow);
        op.setArgument(1, tmpUp);
        op.setCondition(cond);
    }
    else if(op.op == OP_MAX)
    {
        /*
         * %a.upper > %b.upper:
         *   %out.upper = %a.upper
         *   %out.lower = %a.lower
         * %a.upper < %b.upper:
         *   %out.upper = %b.upper
         *   %out.lower = %b.lower
         * %a.upper == %b.upper:
         *   %out.upper = %a.upper
         *   %out.lower = %a.upper < 0 ? min(%a.lower, %b.lower) : max(%a.lower, %b.lower)
         *
         * Since we cannot combine flags, we calculate in reverse order:
         */
        auto upperEqualNegLower = assign(it, out->lower->type, "%long_max") =
            min(as_signed{in0Low}, as_signed{in1Low.value()});
        auto upperEqualPosLower = assign(it, out->lower->type, "%long_max") =
            max(as_signed{in0Low}, as_signed{in1Low.value()});
        auto negativeCond = assignNop(it) = as_signed{in0Up} < as_signed{INT_ZERO};
        auto upperEqualLower = assign(it, out->lower->type, "%long_max") = (upperEqualNegLower, negativeCond);
        assign(it, upperEqualLower) = (upperEqualPosLower, negativeCond.invert());

        auto equalCond = assignNop(it) = as_signed{in0Up} == as_signed{in1Up.value()};
        auto upperEqualOrLessLower = assign(it, out->lower->type, "%long_max") = (upperEqualLower, equalCond);
        assign(it, upperEqualOrLessLower) = (in1Low.value(), equalCond.invert());

        it.emplace(new intermediate::Operation(OP_MAX, out->upper->createReference(), in0Up, in1Up.value()));
        it->setSetFlags(SetFlag::SET_FLAGS);
        it.nextInBlock();

        assign(it, out->lower->createReference()) = (in0Low, COND_CARRY_SET);

        op.op = OP_OR;
        op.setOutput(out->lower->createReference());
        op.setArgument(0, upperEqualOrLessLower);
        op.setArgument(1, upperEqualOrLessLower);
        op.setCondition(COND_CARRY_CLEAR);
    }
    else if(op.op == OP_MIN)
    {
        /*
         * %a.upper < %b.upper:
         *   %out.upper = %a.upper
         *   %out.lower = %a.lower
         * %a.upper > %b.upper:
         *   %out.upper = %b.upper
         *   %out.lower = %b.lower
         * %a.upper == %b.upper:
         *   %out.upper = %a.upper
         *   %out.lower = %a.upper < 0 ? max(%a.lower, %b.lower) : min(%a.lower, %b.lower)
         *
         * Since we cannot combine flags, we calculate in reverse order:
         */
        auto upperEqualNegLower = assign(it, out->lower->type, "%long_min") =
            max(as_signed{in0Low}, as_signed{in1Low.value()});
        auto upperEqualPosLower = assign(it, out->lower->type, "%long_min") =
            min(as_signed{in0Low}, as_signed{in1Low.value()});
        auto negativeCond = assignNop(it) = as_signed{in0Up} < as_signed{INT_ZERO};
        auto upperEqualLower = assign(it, out->lower->type, "%long_min") = (upperEqualNegLower, negativeCond);
        assign(it, upperEqualLower) = (upperEqualPosLower, negativeCond.invert());

        auto equalCond = assignNop(it) = as_signed{in0Up} == as_signed{in1Up.value()};
        auto upperEqualOrGreaterLower = assign(it, out->lower->type, "%long_min") = (upperEqualLower, equalCond);
        assign(it, upperEqualOrGreaterLower) = (in1Low.value(), equalCond.invert());

        // min(a, b) sets carry flag is a > b, so we invert operands to set carry if %a < %b
        it.emplace(new intermediate::Operation(OP_MIN, out->upper->createReference(), in1Up.value(), in0Up));
        it->setSetFlags(SetFlag::SET_FLAGS);
        it.nextInBlock();

        assign(it, out->lower->createReference()) = (in0Low, COND_CARRY_SET);

        op.op = OP_OR;
        op.setOutput(out->lower->createReference());
        op.setArgument(0, upperEqualOrGreaterLower);
        op.setArgument(1, upperEqualOrGreaterLower);
        op.setCondition(COND_CARRY_CLEAR);
    }

    if(op.doesSetFlag())
    {
        op.setSetFlags(SetFlag::DONT_SET);
        lowerFlags(method, it, *out, op.op.flagBehavior);
    }
}

void normalization::lowerLongOperation(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(!it.has())
        return;
    bool writesLongLocal = false;
    bool readsLongLocal = false;
    it->forUsedLocals([&](const Local* loc, LocalUse::Type type, auto& inst) {
        if(loc->get<MultiRegisterData>())
        {
            writesLongLocal |= has_flag(type, LocalUse::Type::WRITER);
            readsLongLocal |= has_flag(type, LocalUse::Type::READER);
        }
    });
    if(!writesLongLocal && !readsLongLocal)
        return;

    if(auto op = it.get<intermediate::Operation>())
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Lowering 64-bit operation: " << op->to_string() << logging::endl);
        ::lowerLongOperation(method, it, *op, config);
    }
    else if(auto move = it.get<intermediate::MoveOperation>())
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Lowering 64-bit move/rotation: " << move->to_string() << logging::endl);
        auto outLocal = move->checkOutputLocal();
        auto out = Local::getLocalData<MultiRegisterData>(outLocal);
        auto src = Local::getLocalData<MultiRegisterData>(move->getSource().checkLocal());
        if(!out && move->getOutput() != NOP_REGISTER)
            throw CompilationError(CompilationStep::NORMALIZER,
                "Can only move/rotate 64-bit values into long locals or NOP register", move->to_string());

        if(!outLocal && !out)
        {
            // If the output is NOP, we still need an output value to be able to calculate the flags
            outLocal =
                method.addNewLocal(TYPE_INT64.toVectorType(move->getSource().type.getVectorWidth())).checkLocal();
            out = Local::getLocalData<MultiRegisterData>(outLocal);
        }

        if(!move->hasSideEffects() && it.get<intermediate::VectorRotation>() && src)
        {
            auto rot = it.get<intermediate::VectorRotation>();
            // split into 2 rotations
            it.emplace(new intermediate::VectorRotation(
                out->lower->createReference(), src->lower->createReference(), rot->getOffset(), rot->type));
            it->copyExtrasFrom(move);
            it.nextInBlock();
            move->setOutput(out->upper->createReference());
            move->setSource(src->upper->createReference());
        }
        else if(!move->hasOtherSideEffects(intermediate::SideEffectType::FLAGS) &&
            (move->getSource().type.getScalarBitCount() <= 32 || move->getSource().getConstantValue()))
        {
            assign(it, out->lower->createReference()) = move->getSource();
            move->setOutput(out->upper->createReference());
            move->setSource(Value(0_lit, move->getSource().type));
        }
        else if(!move->hasOtherSideEffects(intermediate::SideEffectType::FLAGS) && src)
        {
            // simple copy move -> copy the parts
            it.emplace(new intermediate::MoveOperation(out->lower->createReference(), src->lower->createReference()));
            it->copyExtrasFrom(move);
            it.nextInBlock();
            move->setOutput(out->upper->createReference());
            move->setSource(src->upper->createReference());
        }

        if(move->doesSetFlag())
        {
            move->setSetFlags(SetFlag::DONT_SET);
            lowerFlags(method, it, *out, OP_OR.flagBehavior);
        }
    }
    else if(auto call = it.get<intermediate::MethodCall>())
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Lowering 64-bit intrinsic function: " << call->to_string() << logging::endl);
        auto outLoc = call->checkOutputLocal();
        auto out = Local::getLocalData<MultiRegisterData>(outLoc);
        auto src = Local::getLocalData<MultiRegisterData>(call->assertArgument(0).checkLocal());

        if(out && call->methodName.find("vc4cl_int_to_long") != std::string::npos)
        {
            it = intermediate::insertSignExtension(it, method, call->assertArgument(0), outLoc->createReference(), true,
                call->conditional, call->setFlags);
            it.erase();
        }
        else if(out && call->methodName.find("vc4cl_int_to_ulong") != std::string::npos)
        {
            // see "zext" above
            it = intermediate::insertZeroExtension(it, method, call->assertArgument(0), outLoc->createReference(), true,
                call->conditional, call->setFlags);
            it.erase();
        }
        else if(src && call->methodName.find("vc4cl_long_to_int") != std::string::npos)
        {
            // TODO correct for signed??
            assign(it, call->getOutput().value()) =
                (src->lower->createReference(), call->conditional, call->decoration);
            it.erase();
        }
        else if(out && src && call->methodName.find("vc4cl_bitcast_long") != std::string::npos)
        {
            // simple copy move -> copy the parts
            it.emplace(new intermediate::MoveOperation(out->lower->createReference(), src->lower->createReference()));
            it->copyExtrasFrom(call);
            it.nextInBlock();
            it.emplace(new intermediate::MoveOperation(out->upper->createReference(), src->upper->createReference()));
            it->copyExtrasFrom(call);
            it.nextInBlock();
            it.erase();
        }
        else if(out && src && call->methodName.find("vc4cl_bitcast_ulong") != std::string::npos)
        {
            // simple copy move -> copy the parts
            it.emplace(new intermediate::MoveOperation(out->lower->createReference(), src->lower->createReference()));
            it->copyExtrasFrom(call);
            it->addDecorations(intermediate::InstructionDecorations::UNSIGNED_RESULT);
            it.nextInBlock();
            it.emplace(new intermediate::MoveOperation(out->upper->createReference(), src->upper->createReference()));
            it->copyExtrasFrom(call);
            it->addDecorations(intermediate::InstructionDecorations::UNSIGNED_RESULT);
            it.nextInBlock();
            it.erase();
        }
    }
    else if(auto load = it.get<intermediate::LoadImmediate>())
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Lowering 64-bit load: " << load->to_string() << logging::endl);
        auto out = Local::getLocalData<MultiRegisterData>(load->checkOutputLocal());
        if(!out)
            throw CompilationError(
                CompilationStep::NORMALIZER, "Can only load 64-bit values into long locals", load->to_string());

        load->setOutput(out->lower->createReference());
        if(load->getImmediate().type == LiteralType::LONG_LEADING_ONES)
            assign(it, out->upper->createReference()) = INT_MINUS_ONE;
        else
            assign(it, out->upper->createReference()) = INT_ZERO;

        if(load->doesSetFlag())
        {
            load->setSetFlags(SetFlag::DONT_SET);
            lowerFlags(method, it, *out, OP_OR.flagBehavior);
        }
    }
}
