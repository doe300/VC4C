/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LongOperations.h"

#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/TypeConversions.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "../intrinsics/Operators.h"
#include "log.h"

#include <cmath>

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::operators;

// see VC4CLStdLib (_intrinsics.h)
static constexpr unsigned char VC4CL_UNSIGNED{1};

std::pair<Value, Value> normalization::getLowerAndUpperWords(const Value& longValue)
{
    Value low = longValue;
    Value up = INT_ZERO;
    if(auto data = Local::getLocalData<MultiRegisterData>(longValue.checkLocal()))
    {
        low = data->lower->createReference();
        up = data->upper->createReference();
    }
    else if(auto lit = longValue.getLiteralValue())
    {
        low = Value(*lit, longValue.type);
        up = lit->type == LiteralType::LONG_LEADING_ONES ? INT_MINUS_ONE : INT_ZERO;
    }
    // TODO for the upper part for e.g. literals/vectors, how to know whether it should zero-extended or sign-extend
    // the lower part? or store 64-bit literal?? Would increase Literal size by 1/3 for rare uses!

    if(low.type.getScalarBitCount() > 32)
        low.type = TYPE_INT32.toVectorType(longValue.type.getVectorWidth());
    if(up.type.getScalarBitCount() > 32)
        up.type = TYPE_INT32.toVectorType(longValue.type.getVectorWidth());

    return std::make_pair(low, up);
}

static void lowerFlags(Method& method, InstructionWalker it, const MultiRegisterData& output, FlagBehavior flagBehavior,
    bool isFloatOperation, const Optional<Value>& setCarryFlag = {})
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
    else if(has_flag(flagBehavior, add_flag(FlagBehavior::ZERO_ALL_ZEROS, FlagBehavior::NEGATIVE_MSB_SET)) &&
        setCarryFlag)
    {
        /*
         * This is e.g. the behavior of (F)MIN, (F)MAX, (F)ADD, etc.
         * - OR'ing upper and lower parts together gives the zero-check
         * - for negative check, need to only consider high bit of upper part
         * => compress lower part to never set the high bit
         * - since we take the carry flag from outside, we don't care in here which behavior actually sets the carry
         *   flag and can apply this block for all operations matching the zero and negative flags
         */
        auto tmp0 = assign(flagIt, flagType) = as_unsigned{output.lower->createReference()} >> 1_val;
        // since we shift the last bit of the lower part out, need to separately add it again back in
        auto tmp1 = assign(flagIt, flagType) = output.lower->createReference() & 1_val;
        auto tmp2 = assign(flagIt, flagType) = tmp0 | tmp1;
        auto tmp3 = assign(flagIt, flagType) = tmp2 | output.upper->createReference();
        /*
         * tmp = upper | compact(lower) // as calculated above
         * tmp = tmp & 1 ? tmp | 2 : tmp // copy last bit to 2nd last
         * tmp = tmp & ~1 // mask of last bit, since we use it to set carry flag
         * tmp = tmp | "carry set" (single bit)
         * - = tmp >>> 1 (asr), setf
         *
         * flags  |   tmp    | operation
         * zcnccc | >0, %2=0 | asr >> 1
         * zcnscc | <0, %2=0 | asr >> 1
         * zcnccs | >0, %2=1 | asr >> 1
         * zcnscs | <0, %2=1 | asr >> 1
         * zsnccc | 0, %2=0  | asr >> 1
         * zsnscc |  <N/A>   | -
         * zsnccs | 1, %2=1  | asr >> 1
         * zsnscs |  <N/A>   | -
         */
        assignNop(flagIt) = (tmp3 & 1_val, SetFlag::SET_FLAGS);
        assign(flagIt, tmp3) = (tmp3 | 2_val, COND_ZERO_CLEAR);
        // mask off the least significant bit, to not set wrong carry flags
        static_assert(static_cast<int32_t>(uint32_t{0xFFFFFFFE}) == -2, "");
        auto tmp4 = assign(flagIt, flagType) = tmp3 & Value(SmallImmediate::fromInteger(-2).value(), TYPE_INT32);
        auto tmp5 = assign(flagIt, flagType) = tmp4 | *setCarryFlag;
        assignNop(flagIt) = (as_signed{tmp5} >> 1_val, SetFlag::SET_FLAGS);
    }
    else
        throw CompilationError(
            CompilationStep::NORMALIZER, "Unhandled flag behavior for 64-bit operation", it->to_string());
}

static InstructionWalker findReplicationRead(InstructionWalker it)
{
    FastSet<const Local*> writtenLocals;

    for(it.nextInBlock(); !it.isEndOfBlock(); it.nextInBlock())
    {
        if(it.has() &&
            (it->readsRegister(REG_REPLICATE_ALL) || it->readsRegister(REG_REPLICATE_QUAD) ||
                it->readsRegister(REG_ACC5)))
        {
            it->forUsedLocals([&](const Local* loc, LocalUse::Type type, const auto&) {
                if(writtenLocals.find(loc) != writtenLocals.end())
                    throw CompilationError(CompilationStep::NORMALIZER,
                        "Cannot rewrite 64-bit replication if replication output is written in between",
                        it->to_string());
            });
            break;
        }
        if(it.has())
            it->forUsedLocals([&](const Local* loc, LocalUse::Type type, const auto&) { writtenLocals.emplace(loc); });
    }
    return it;
}

NODISCARD static InstructionWalker lowerLongReplication(
    Method& method, InstructionWalker it, const MultiRegisterData* input)
{
    auto readerIt = findReplicationRead(it);
    if(readerIt.isEndOfBlock())
        throw CompilationError(CompilationStep::NORMALIZER,
            "Cannot lower 64-bit replication without replication register reader", it->to_string());

    Value outLow = UNDEFINED_VALUE;
    Value outUp = UNDEFINED_VALUE;
    std::tie(outLow, outUp) = getLowerAndUpperWords(readerIt->getOutput().value());

    // need to split the replication and replicate lower and upper part separately
    it.nextInBlock();
    it = intermediate::insertReplication(it, input->lower->createReference(), outLow);
    it = intermediate::insertReplication(it, input->upper->createReference(), outUp);

    // remove original value extraction
    readerIt.erase();
    return it;
}

static void lowerLongOperation(
    Method& method, InstructionWalker it, intermediate::Operation& op, const Configuration& config)
{
    auto outLocal = op.checkOutputLocal();
    auto out = Local::getLocalData<MultiRegisterData>(outLocal);
    Value in0Low = UNDEFINED_VALUE;
    Value in0Up = UNDEFINED_VALUE;
    std::tie(in0Low, in0Up) = getLowerAndUpperWords(op.getFirstArg());
    auto in1Low = op.getSecondArg();
    Optional<Value> in1Up = INT_ZERO;
    if(auto arg = op.getSecondArg())
        std::tie(in1Low, in1Up) = getLowerAndUpperWords(op.assertArgument(1));

    auto originalOut = op.getOutput();
    // whether we created a new output local and need to map it to the actual output after the lowering
    bool isDummyOutput = false;
    if(!out)
    {
        // If the output is not a 64-bit local, we still need a 64-bit output value to be able to calculate the flags
        outLocal = method.addNewLocal(TYPE_INT64.toVectorType(op.getFirstArg().type.getVectorWidth())).checkLocal();
        out = Local::getLocalData<MultiRegisterData>(outLocal);
        isDummyOutput = true;
    }

    // we need to get the flag behavior before possibly rewriting the operation
    auto flagBehavior = op.op.flagBehavior;
    bool returnsFloat = op.op.returnsFloat;
    Optional<Value> setCarryFlag = NO_VALUE;
    bool retainSetFlag = false;

    if(op.op == OP_ADD)
    {
        /*
         * 64-bit add is:
         * %out.lower = %a.lower + %b.lower (set carry flag)
         * %tmp = %a.upper + %b.upper
         * %out.upper = %tmp + carry bit
         */
        if(op.doesSetFlag())
            setCarryFlag = assign(it, TYPE_BOOL) = BOOL_FALSE;
        assign(it, out->upper->createReference()) = (in0Up + in1Up.value(), op.decoration, op.getFlags());
        if(setCarryFlag)
            assign(it, *setCarryFlag) = (BOOL_TRUE, COND_CARRY_SET);
        assign(it, out->lower->createReference()) = (in0Low + in1Low.value(), SetFlag::SET_FLAGS, op.decoration);
        op.setOutput(out->upper->createReference());
        op.setArgument(0, out->upper->createReference());
        op.setArgument(1, INT_ONE);
        op.setCondition(COND_CARRY_SET);
        if(setCarryFlag)
        {
            // For flags set by conditional instructions, the flags are only updated where the operation is actually
            // executed, which we can use here.
            // NOTE: This carry is set by the above conditional instruction and is thus not the same carry condition as
            // above!
            retainSetFlag = true;
            assign(it, *setCarryFlag) = (BOOL_TRUE, COND_CARRY_SET);
        }
    }
    else if(op.op == OP_SUB)
    {
        // %a - %b => %a + -%b
        // build two's complement by inverting all bits and adding one
        auto tmp = assign(it, op.getSecondArg()->type) = (~op.getSecondArg().value(), op.getCondition(), op.decoration);
        auto firstIt = it.copy().previousInBlock();
        tmp = assign(it, op.getSecondArg()->type) = (tmp + 1_val, op.getCondition(), op.decoration);
        auto secondIt = it.copy().previousInBlock();
        op.op = OP_ADD;
        op.setArgument(1, tmp);
        // do the actual lowering for the newly added bitwise NOT and ADD operations
        if(auto firstOp = firstIt.get<intermediate::Operation>())
            lowerLongOperation(method, firstIt, *firstOp, config);
        if(auto secondOp = secondIt.get<intermediate::Operation>())
            lowerLongOperation(method, secondIt, *secondOp, config);
        lowerLongOperation(method, it, op, config);
        // break here to not handle flags again, they are already handled by the lowered add
        return;
    }
    else if(op.op == OP_NOT)
    {
        assign(it, out->lower->createReference()) = (~in0Low, op.getCondition(), op.decoration);
        op.setOutput(out->upper->createReference());
        op.setArgument(0, in0Up);
    }
    else if(op.op == OP_AND || op.op == OP_OR || op.op == OP_XOR)
    {
        it.emplace(intermediate::createWithExtras<intermediate::Operation>(
            op, op.op, out->lower->createReference(), in0Low, in1Low.value()));
        it.nextInBlock();
        op.setOutput(out->upper->createReference());
        op.setArgument(0, in0Up);
        op.setArgument(1, in1Up.value());
        // TODO this is wrong when flags are set!
    }
    else if(op.op == OP_SHR || op.op == OP_ASR)
    {
        auto offsetType = TYPE_INT8.toVectorType(outLocal->type.getVectorWidth());
        assignNop(it) = (min(as_signed{32_val}, as_signed{in1Low.value()}), SetFlag::SET_FLAGS);
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
        assignNop(it) = (min(as_signed{32_val}, as_signed{in1Low.value()}), SetFlag::SET_FLAGS);
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
        auto negativeCond = assignNop(it) = isnegative(as_signed{in0Up});
        auto upperEqualLower = assign(it, out->lower->type, "%long_max") = (upperEqualNegLower, negativeCond);
        assign(it, upperEqualLower) = (upperEqualPosLower, negativeCond.invert());

        auto equalCond = assignNop(it) = as_signed{in0Up} == as_signed{in1Up.value()};
        auto upperEqualOrLessLower = assign(it, out->lower->type, "%long_max") = (upperEqualLower, equalCond);
        assign(it, upperEqualOrLessLower) = (in1Low.value(), equalCond.invert());

        assign(it, out->upper->createReference()) =
            (max(as_signed{in0Up}, as_signed{in1Up.value()}), SetFlag::SET_FLAGS);
        assign(it, out->lower->createReference()) = (in0Low, COND_CARRY_SET);

        op.op = OP_OR;
        op.setOutput(out->lower->createReference());
        op.setArgument(0, upperEqualOrLessLower);
        op.setArgument(1, upperEqualOrLessLower);
        op.setCondition(COND_CARRY_CLEAR);

        // carry is first > second <=> result.upper != second.upper || result.lower != second.lower
        it.nextInBlock();
        setCarryFlag = assign(it, TYPE_BOOL) = BOOL_FALSE;
        auto cond = assignNop(it) = as_unsigned{out->upper->createReference()} != as_unsigned{*in1Up};
        assign(it, *setCarryFlag) = (BOOL_TRUE, cond);
        cond = assignNop(it) = as_unsigned{out->lower->createReference()} != as_unsigned{*in1Low};
        assign(it, *setCarryFlag) = (BOOL_TRUE, cond);
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
        auto negativeCond = assignNop(it) = isnegative(as_signed{in0Up});
        auto upperEqualLower = assign(it, out->lower->type, "%long_min") = (upperEqualNegLower, negativeCond);
        assign(it, upperEqualLower) = (upperEqualPosLower, negativeCond.invert());

        auto equalCond = assignNop(it) = as_signed{in0Up} == as_signed{in1Up.value()};
        auto upperEqualOrGreaterLower = assign(it, out->lower->type, "%long_min") = (upperEqualLower, equalCond);
        assign(it, upperEqualOrGreaterLower) = (in1Low.value(), equalCond.invert());

        // min(a, b) sets carry flag is a > b, so we invert operands to set carry if %a < %b
        assign(it, out->upper->createReference()) =
            (min(as_signed{in1Up.value()}, as_signed{in0Up}), SetFlag::SET_FLAGS);
        assign(it, out->lower->createReference()) = (in0Low, COND_CARRY_SET);

        op.op = OP_OR;
        op.setOutput(out->lower->createReference());
        op.setArgument(0, upperEqualOrGreaterLower);
        op.setArgument(1, upperEqualOrGreaterLower);
        op.setCondition(COND_CARRY_CLEAR);

        // carry is first > second <=> result.upper != first.upper || result.lower != first.lower
        it.nextInBlock();
        setCarryFlag = assign(it, TYPE_BOOL) = BOOL_FALSE;
        auto cond = assignNop(it) = as_unsigned{out->upper->createReference()} != as_unsigned{in0Up};
        assign(it, *setCarryFlag) = (BOOL_TRUE, cond);
        cond = assignNop(it) = as_unsigned{out->lower->createReference()} != as_unsigned{in0Low};
        assign(it, *setCarryFlag) = (BOOL_TRUE, cond);
    }
    else
        throw CompilationError(CompilationStep::NORMALIZER, "Unsupported operation on 64-bit integers", op.to_string());

    if(isDummyOutput &&
        (originalOut->hasRegister(REG_REPLICATE_ALL) || originalOut->hasRegister(REG_REPLICATE_QUAD) ||
            originalOut->hasRegister(REG_ACC5)))
    {
        it = lowerLongReplication(method, it, out);
    }
    else if(isDummyOutput && originalOut && originalOut != NOP_REGISTER)
    {
        // the original output is not a multi-register value (checked above), so simply truncate the lower word into the
        // actual output
        assign(it, *originalOut) = out->lower->createReference();
    }

    if(op.doesSetFlag())
    {
        if(!retainSetFlag)
            op.setSetFlags(SetFlag::DONT_SET);
        lowerFlags(method, it, *out, flagBehavior, returnsFloat, setCarryFlag);
    }
}

void normalization::lowerLongOperation(
    Module& module, Method& method, InstructionWalker it, const Configuration& config)
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

        auto originalOut = move->getOutput();
        // whether we created a new output local and need to map it to the actual output after the lowering
        bool isDummyOutput = false;
        if(!out)
        {
            // If the output is not a 64-bit local, we still need a 64-bit output value to be able to calculate the
            // flags
            outLocal =
                method.addNewLocal(TYPE_INT64.toVectorType(move->getSource().type.getVectorWidth())).checkLocal();
            out = Local::getLocalData<MultiRegisterData>(outLocal);
            isDummyOutput = true;
        }

        if(!move->hasSideEffects() && it->getVectorRotation() && src)
        {
            auto rot = it->getVectorRotation();
            // split into 2 rotations
            it.emplace(intermediate::createWithExtras<intermediate::VectorRotation>(
                *move, out->lower->createReference(), src->lower->createReference(), rot->offset, rot->type));
            it.nextInBlock();
            move->setOutput(out->upper->createReference());
            move->setSource(src->upper->createReference());
        }
        else if(!move->hasOtherSideEffects(intermediate::SideEffectType::FLAGS) &&
            (move->getSource().type.getScalarBitCount() <= 32 || move->getSource().getConstantValue()))
        {
            assign(it, out->lower->createReference()) = (move->getSource(), move->getCondition());
            move->setOutput(out->upper->createReference());
            move->setSource(Value(0_lit, move->getSource().type));
        }
        else if(!move->hasOtherSideEffects(intermediate::SideEffectType::FLAGS) && src)
        {
            // simple copy move -> copy the parts
            it.emplace(intermediate::createWithExtras<intermediate::MoveOperation>(
                *move, out->lower->createReference(), src->lower->createReference()));
            it.nextInBlock();
            move->setOutput(out->upper->createReference());
            move->setSource(src->upper->createReference());
        }

        if(isDummyOutput &&
            (originalOut->hasRegister(REG_REPLICATE_ALL) || originalOut->hasRegister(REG_REPLICATE_QUAD) ||
                originalOut->hasRegister(REG_ACC5)))
        {
            it = lowerLongReplication(method, it, out);
        }
        else if(isDummyOutput && originalOut && originalOut != NOP_REGISTER)
        {
            // the original output is not a multi-register value (checked above), so simply truncate the lower word into
            // the actual output
            assign(it, *originalOut) = out->lower->createReference();
        }

        if(move->doesSetFlag())
        {
            move->setSetFlags(SetFlag::DONT_SET);
            lowerFlags(method, it, *out, OP_OR.flagBehavior, false);
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
            it =
                intermediate::insertSignExtension(it, method, call->assertArgument(0), outLoc->createReference(), true);
            it.erase();
        }
        else if(out && call->methodName.find("vc4cl_int_to_ulong") != std::string::npos)
        {
            // see "zext" above
            it =
                intermediate::insertZeroExtension(it, method, call->assertArgument(0), outLoc->createReference(), true);
            it.erase();
        }
        else if(src && call->methodName.find("vc4cl_long_to_int_sat") != std::string::npos)
        {
            // saturate the low part if the high part is not zero (for unsigned) or 0/-1 (for signed)
            bool isUnsigned = call->getArgument(1) &&
                (call->assertArgument(1).getConstantValue() & &Value::getLiteralValue & &Literal::unsignedInt) ==
                    VC4CL_UNSIGNED;
            auto outValue = call->getOutput().value();
            if(isUnsigned)
            {
                // for unsigned, any non-zero upper part means overflow, so saturate
                assign(it, outValue) = src->lower->createReference();
                auto cond = assignNop(it) = isnonzero(as_unsigned{src->upper->createReference()});
                assign(it, outValue) = (0xFFFFFFFF_val, cond);
            }
            else
            {
                // for signed, the lower part overflows if the upper part is neither all zeroes nor all ones
                // assume that the high bit of the upper part is correctly signed (i.e. 1 for negative and 0 for
                // positive values)
                auto resultNegative = assign(it, outValue.type) = as_signed{src->upper->createReference()} >> 31_val;
                // normal case, no overflow
                assign(it, outValue) = src->lower->createReference();
                // if there are bits in the upper word which differ from the high bit, we have an overflow
                auto cond = assignNop(it) = as_unsigned{src->upper->createReference()} != as_unsigned{resultNegative};
                // if the result is supposed to be negative, all ones ^ 0x7FFFFFFF = 0x80000000 (= INT_MIN)
                // if the result is supposed to be positive, all zeroes ^ 0x7FFFFFFF = 0x7FFFFFFF (= INT_MAX)
                assign(it, outValue) = (resultNegative ^ 0x7FFFFFFF_val, cond);
            }
            it.erase();
        }
        else if(src && call->methodName.find("vc4cl_long_to_int") != std::string::npos)
        {
            // TODO correct for signed??
            assign(it, call->getOutput().value()) = (src->lower->createReference(), call->decoration);
            it.erase();
        }
        else if(src && call->methodName.find("vc4cl_ulong_to_float") != std::string::npos)
        {
            it = intermediate::insertUnsignedToFloatConversion(it, method, call->assertArgument(0), *call->getOutput());
            it.erase();
        }
        else if(src && call->methodName.find("vc4cl_long_to_float") != std::string::npos)
        {
            it = intermediate::insertSignedToFloatConversion(it, method, call->assertArgument(0), *call->getOutput());
            it.erase();
        }
        else if(out && src && call->methodName.find("vc4cl_bitcast_long") != std::string::npos)
        {
            // simple copy move -> copy the parts
            it.emplace(intermediate::createWithExtras<intermediate::MoveOperation>(
                *call, out->lower->createReference(), src->lower->createReference()));
            it.nextInBlock();
            it.emplace(intermediate::createWithExtras<intermediate::MoveOperation>(
                *call, out->upper->createReference(), src->upper->createReference()));
            it.nextInBlock();
            it.erase();
        }
        else if(out && src && call->methodName.find("vc4cl_bitcast_ulong") != std::string::npos)
        {
            // simple copy move -> copy the parts
            it.emplace(intermediate::createWithExtras<intermediate::MoveOperation>(
                *call, out->lower->createReference(), src->lower->createReference()));
            it->addDecorations(intermediate::InstructionDecorations::UNSIGNED_RESULT);
            it.nextInBlock();
            it.emplace(intermediate::createWithExtras<intermediate::MoveOperation>(
                *call, out->upper->createReference(), src->upper->createReference()));
            it->addDecorations(intermediate::InstructionDecorations::UNSIGNED_RESULT);
            it.nextInBlock();
            it.erase();
        }
        else if(out && call->methodName.find("vc4cl_mul_full") != std::string::npos)
        {
            auto outLow = out->lower->createReference();
            auto outUp = out->upper->createReference();
            call->setOutput(outUp);
            it = intrinsics::intrinsifyIntegerToLongMultiplication(method, it, call, outLow);
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
            lowerFlags(method, it, *out, OP_OR.flagBehavior, false);
        }
    }
}

InstructionWalker normalization::insertLongLoad(InstructionWalker it, Method& method, const Local& dest, uint64_t value)
{
    auto multiRegisters = Local::getLocalData<MultiRegisterData>(&dest);

    if(!multiRegisters)
        throw CompilationError(CompilationStep::LLVM_2_IR, "Cannot load 64-bit constant given value", dest.to_string());

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating loading of scalar 64-bit constant " << value << " into " << dest.to_string()
            << logging::endl);

    Literal lower(static_cast<uint32_t>(static_cast<uint64_t>(value) & 0x00000000FFFFFFFF));
    Literal upper(static_cast<uint32_t>((static_cast<uint64_t>(value) & 0xFFFFFFFF00000000) >> 32));
    it.emplace(std::make_unique<intermediate::LoadImmediate>(multiRegisters->lower->createReference(), lower));
    it.nextInBlock();
    it.emplace(std::make_unique<intermediate::LoadImmediate>(multiRegisters->upper->createReference(), upper));
    it.nextInBlock();
    return it;
}
