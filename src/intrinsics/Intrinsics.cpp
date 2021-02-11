/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Intrinsics.h"

#include "../SIMDVector.h"
#include "../analysis/ValueRange.h"
#include "../intermediate/Helper.h"
#include "../intermediate/TypeConversions.h"
#include "../intermediate/VectorHelper.h"
#include "../intermediate/operators.h"
#include "../periphery/SFU.h"
#include "../periphery/TMU.h"
#include "../periphery/VPM.h"

#include "Comparisons.h"
#include "Images.h"
#include "Operators.h"
#include "WorkItems.h"
#include "log.h"

#include <climits>
#include <cmath>
#include <cstdbool>
#include <map>
#include <vector>

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::intrinsics;
using namespace vc4c::operators;

// TODO need to add sign extension to some more intrinsics?

// The function to apply for pre-calculation
using UnaryInstruction = std::function<Optional<Value>(const Value&)>;
static const UnaryInstruction NO_OP = [](const Value& val) { return NO_VALUE; };
// The function to apply for pre-calculation
using BinaryInstruction = std::function<Optional<Value>(const Value&, const Value&)>;
static const BinaryInstruction NO_OP2 = [](const Value& val0, const Value& val1) { return NO_VALUE; };

// see VC4CLStdLib (_intrinsics.h)
static constexpr unsigned char VC4CL_UNSIGNED{1};

using IntrinsicFunction = std::function<InstructionWalker(Method&, InstructionWalker, const MethodCall*)>;
// NOTE: copying the captures is on purpose, since the sources do not exist anymore!

static IntrinsicFunction intrinsifyUnaryALUInstruction(const std::string& opCode, const bool useSignFlag = false,
    Pack packMode = PACK_NOP, Unpack unpackMode = UNPACK_NOP, bool setFlags = false)
{
    return [opCode, useSignFlag, packMode, unpackMode, setFlags](
               Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker {
        bool isUnsigned = callSite->getArgument(1) && callSite->assertArgument(1).getLiteralValue() &&
            callSite->assertArgument(1).getLiteralValue()->unsignedInt() == VC4CL_UNSIGNED;

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying unary '" << callSite->to_string() << "' to operation " << opCode << logging::endl);
        if(opCode == "mov")
            it.reset((new MoveOperation(callSite->getOutput().value(), callSite->assertArgument(0)))
                         ->copyExtrasFrom(callSite));
        else
            it.reset(
                (new Operation(OpCode::toOpCode(opCode), callSite->getOutput().value(), callSite->assertArgument(0)))
                    ->copyExtrasFrom(callSite));
        // XXX pack modes do not write all bytes, need to zero/sign extend before? Would only be necessary if we
        // actually use the other bits.
        if(packMode.hasEffect())
            it.get<ExtendedInstruction>()->setPackMode(packMode);
        if(unpackMode.hasEffect())
            it.get<UnpackingInstruction>()->setUnpackMode(unpackMode);
        if(setFlags)
            it.get<ExtendedInstruction>()->setSetFlags(SetFlag::SET_FLAGS);

        if(useSignFlag && isUnsigned)
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);

        return it;
    };
}

static IntrinsicFunction intrinsifyBinaryALUInstruction(const std::string& opCode, const bool useSignFlag = false,
    Pack packMode = PACK_NOP, Unpack unpackMode = UNPACK_NOP, bool setFlags = false)
{
    return [opCode, useSignFlag, packMode, unpackMode, setFlags](
               Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker {
        bool isUnsigned = callSite->getArgument(2) && callSite->assertArgument(2).getLiteralValue() &&
            callSite->assertArgument(2).getLiteralValue()->unsignedInt() == VC4CL_UNSIGNED;

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying binary '" << callSite->to_string() << "' to operation " << opCode << logging::endl);
        it.reset((new Operation(OpCode::toOpCode(opCode), callSite->getOutput().value(), callSite->assertArgument(0),
                      callSite->assertArgument(1)))
                     ->copyExtrasFrom(callSite));
        if(packMode.hasEffect())
            it.get<ExtendedInstruction>()->setPackMode(packMode);
        if(unpackMode.hasEffect())
            it.get<UnpackingInstruction>()->setUnpackMode(unpackMode);
        if(setFlags)
            it.get<ExtendedInstruction>()->setSetFlags(SetFlag::SET_FLAGS);

        if(useSignFlag && isUnsigned)
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);

        return it;
    };
}

static IntrinsicFunction intrinsifySFUInstruction(const Register sfuRegister)
{
    return [sfuRegister](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying unary '" << callSite->to_string() << "' to SFU call" << logging::endl);
        it = periphery::insertSFUCall(sfuRegister, it, callSite->assertArgument(0));
        it.reset((new MoveOperation(callSite->getOutput().value(), Value(REG_SFU_OUT, callSite->getReturnType())))
                     ->copyExtrasFrom(callSite));
        return it;
    };
}

static IntrinsicFunction intrinsifyValueRead(const Value& val)
{
    return [val](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying method-call '" << callSite->to_string() << "' to value read" << logging::endl);
        it.reset((new MoveOperation(callSite->getOutput().value(), val))->copyExtrasFrom(callSite));
        return it;
    };
}

static IntrinsicFunction intrinsifySemaphoreAccess(bool increment)
{
    return [increment](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker {
        if(!callSite->assertArgument(0).getLiteralValue())
            throw CompilationError(CompilationStep::NORMALIZER, "Semaphore-number needs to be a compile-time constant",
                callSite->to_string());
        if(callSite->assertArgument(0).getLiteralValue()->signedInt() < 0 ||
            callSite->assertArgument(0).getLiteralValue()->signedInt() >= 16)
            throw CompilationError(
                CompilationStep::NORMALIZER, "Semaphore-number needs to be between 0 and 15", callSite->to_string());

        if(increment)
        {
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Intrinsifying semaphore increment with instruction" << logging::endl);
            it.reset((new SemaphoreAdjustment(
                          static_cast<Semaphore>(callSite->assertArgument(0).getLiteralValue()->unsignedInt()), true))
                         ->copyExtrasFrom(callSite));
        }
        else
        {
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Intrinsifying semaphore decrement with instruction" << logging::endl);
            it.reset((new SemaphoreAdjustment(
                          static_cast<Semaphore>(callSite->assertArgument(0).getLiteralValue()->unsignedInt()), false))
                         ->copyExtrasFrom(callSite));
        }
        return it;
    };
}

static IntrinsicFunction intrinsifyMutexAccess(bool lock)
{
    return [lock](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker {
        if(lock)
        {
            CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying mutex lock with instruction" << logging::endl);
            it.reset((new MutexLock(MutexAccess::LOCK))->copyExtrasFrom(callSite));
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying mutex unlock with instruction" << logging::endl);
            it.reset((new MutexLock(MutexAccess::RELEASE))->copyExtrasFrom(callSite));
        }
        return it;
    };
}

enum class MemoryAccess : unsigned char
{
    READ,
    WRITE,
    COPY,
    PREFETCH
};

static IntrinsicFunction intrinsifyMemoryAccess(MemoryAccess access, bool setMutex)
{
    return [access, setMutex](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker {
        switch(access)
        {
        case MemoryAccess::READ:
        {
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Intrinsifying memory read " << callSite->to_string() << logging::endl);
            // This needs to be access via VPM for atomic-instructions to work correctly!!
            it.emplace(new MemoryInstruction(MemoryOperation::READ, Value(callSite->getOutput().value()),
                Value(callSite->assertArgument(0)), Value(INT_ONE), setMutex));
            it.nextInBlock();
            break;
        }
        case MemoryAccess::WRITE:
        {
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Intrinsifying memory write " << callSite->to_string() << logging::endl);
            it.emplace(new MemoryInstruction(MemoryOperation::WRITE, Value(callSite->assertArgument(0)),
                Value(callSite->assertArgument(1)), Value(INT_ONE), setMutex));
            it.nextInBlock();
            break;
        }
        case MemoryAccess::COPY:
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying ternary '" << callSite->to_string() << "' to DMA copy operation "
                    << logging::endl);
            it.emplace(new MemoryInstruction(MemoryOperation::COPY, Value(callSite->assertArgument(0)),
                Value(callSite->assertArgument(1)), callSite->getArgument(2).value_or(INT_ONE), setMutex));
            it.nextInBlock();
            break;
        }
        case MemoryAccess::PREFETCH:
        {
            // TODO could be used to load into VPM and then use the cache for further reads
            // for now, simply discard
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Discarding unsupported DMA pre-fetch: " << callSite->to_string() << logging::endl);
            break;
        }
        }

        it.erase();
        // so next instruction is not skipped
        it.previousInBlock();

        return it;
    };
}

static IntrinsicFunction intrinsifyVectorRotation()
{
    return [](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Intrinsifying vector rotation " << callSite->to_string() << logging::endl);
        it = insertVectorRotation(
            it, callSite->assertArgument(0), callSite->assertArgument(1), callSite->getOutput().value(), Direction::UP);
        it.erase();
        // so next instruction is not skipped
        it.previousInBlock();

        return it;
    };
}

static IntrinsicFunction intrinsifyCheckNaN(bool checkInfinite)
{
    return [=](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying floating point check: " << callSite->to_string() << logging::endl);
        ConditionCode cond = COND_ALWAYS;
        // Return -1/1 (for vector/scalar) if value is Inf/NaN, 0 otherwise
        if(checkInfinite)
            cond = assignNop(it) = isnaninf(as_float{callSite->assertArgument(0)});
        else
            cond = assignNop(it) = isnan(as_float{callSite->assertArgument(0)});

        if(callSite->assertArgument(0).type.isScalarType())
            assign(it, callSite->getOutput().value()) = (INT_ONE, cond);
        else
            assign(it, callSite->getOutput().value()) = (INT_MINUS_ONE, cond);
        assign(it, callSite->getOutput().value()) = (INT_ZERO, cond.invert());

        it.erase();
        // so next instruction is not skipped
        it.previousInBlock();

        return it;
    };
}

static InstructionWalker intrinsifyIntegerMultiplicationHighPart(
    Method& method, InstructionWalker it, const MethodCall* callSite)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Intrinsifying full 32-bit to 64-bit multiplication taking the high part: " << callSite->to_string()
            << logging::endl);
    return intrinsifyIntegerToLongMultiplication(method, it, it.get<MethodCall>());
}

static InstructionWalker intrinsifySaturatedSubtraction(
    Method& method, InstructionWalker it, const MethodCall* callSite)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Intrinsifying 32-bit integer saturated subtaction: " << callSite->to_string() << logging::endl);
    auto out = callSite->getOutput().value();

    auto normalValue = assign(it, out.type) =
        as_signed{callSite->assertArgument(0)} - as_signed{callSite->assertArgument(1)};

    /*
     * 1.op 2.op | res+ res- | examples
     *  +    +  ->  ok   ok  | INT_MAX - 0 = INT_MAX, 0 - INT_MAX = -INT_MAX
     *  +    -  ->  ok  sat+ | INT_MAX - INT_MIN = sat+, 0 - INT_MIN = sat+, INT_MAX - -1 = sat+
     *  -    +  -> sat-  ok  | -1 - INT_MAX = INT_MIN, INT_MIN - INT_MAX = sat-, INT_MIN - 1 = sat-
     *  -    -  ->  ok   ok  | INT_MIN - INT_MIN = 0, INT_MIN - -1 = INT_MIN+1, -1 - INT_MIN = INT_MAX
     *
     * => Overflow iff: sign(1.op) != sign(2.op) && sign(2.op) == sign(res)
     */

    auto firstSign = assign(it, out.type) = as_signed{callSite->assertArgument(0)} >> 31_val;
    auto secondSign = assign(it, out.type) = as_signed{callSite->assertArgument(1)} >> 31_val;
    auto resultSign = assign(it, out.type) = as_signed{normalValue} >> 31_val;

    auto differentSigns = assign(it, out.type) = firstSign ^ secondSign;
    auto resultHasSecondSign = assignNop(it) = as_unsigned{secondSign} == as_unsigned{resultSign};
    auto saturationValue = assign(it, out.type) = resultSign ^ 0x80000000_val;
    assign(it, saturationValue) = (0_val, resultHasSecondSign.invert());
    saturationValue = assign(it, out.type) = (saturationValue & differentSigns, SetFlag::SET_FLAGS);

    assign(it, out) = normalValue;
    assign(it, out) = (saturationValue, COND_ZERO_CLEAR);
    it.erase();
    // so next instruction is not skipped
    it.previousInBlock();
    return it;
}

static ConditionCode toCondition(int c)
{
    // for mapping, see _flags.h in VC4CLStdLib
    switch(c)
    {
    case 'z':
        return COND_ZERO_SET;
    case 'Z':
        return COND_ZERO_CLEAR;
    case 'n':
        return COND_NEGATIVE_SET;
    case 'N':
        return COND_NEGATIVE_CLEAR;
    case 'c':
        return COND_CARRY_SET;
    case 'C':
        return COND_CARRY_CLEAR;
    }
    throw CompilationError(CompilationStep::NORMALIZER, "Unknown key to map to condition code", std::to_string(c));
}

static InstructionWalker intrinsifyFlagCondition(Method& method, InstructionWalker it, const MethodCall* callSite)
{
    auto cond = toCondition(callSite->assertArgument(2).getLiteralValue()->signedInt());
    assign(it, callSite->getOutput().value()) = (callSite->assertArgument(0), cond);
    assign(it, callSite->getOutput().value()) = (callSite->assertArgument(1), cond.invert());
    it.erase();
    // so next instruction is not skipped
    it.previousInBlock();
    return it;
}

static InstructionWalker intrinsifyPopcount(Method& method, InstructionWalker it, const MethodCall* callSite)
{
    // This is a generalized implementation parameterized on the type size, adapted from
    // https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
    // Alternative implementation (esp. for 64-bit) can be found here:
    // https://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation
    auto& arg = callSite->assertArgument(0);
    auto typeWidth = arg.type.getScalarBitCount();

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Intrinsifying " << static_cast<unsigned>(typeWidth) << "-bit popcount: " << callSite->to_string()
            << logging::endl);

    if(auto argParts = Local::getLocalData<MultiRegisterData>(arg.checkLocal()))
    {
        auto output = callSite->getOutput().value();
        auto resultParts = Local::getLocalData<MultiRegisterData>(callSite->checkOutputLocal());
        // insert dummy instruction to be replaced
        auto lowerResult = method.addNewLocal(resultParts ? resultParts->lower->type : output.type, "%popcount");
        it.emplace(new MethodCall(
            Value(lowerResult), "vc4cl_popcount", std::vector<Value>{argParts->lower->createReference()}));
        it = intrinsifyPopcount(method, it, it.get<MethodCall>());
        it.nextInBlock();
        if(resultParts && resultParts->upper)
        {
            it->setArgument(0, argParts->upper->createReference());
            auto upperResult = method.addNewLocal(resultParts->upper->type, "%popcount");
            it->setOutput(upperResult);
            it = intrinsifyPopcount(method, it, callSite);
            it.nextInBlock();
            assign(it, resultParts->upper->createReference()) = INT_ZERO;
            it.emplace(new Operation(OP_ADD, resultParts->lower->createReference(), lowerResult, upperResult));
        }
        else
            it.emplace(new MoveOperation(output, lowerResult));
        return it;
    }

    auto typeWidthMask = arg.type.getScalarWidthMask();

    auto tmp0 = assign(it, arg.type, "%popcount") = as_unsigned{arg} >> 1_val;
    tmp0 = assign(it, arg.type, "%popcount") = tmp0 & Value(Literal(0x55555555 & typeWidthMask), arg.type);
    tmp0 = assign(it, arg.type, "%popcount") = arg - tmp0;

    auto tmp1 = assign(it, arg.type, "%popcount") = tmp0 & Value(Literal(0x33333333 & typeWidthMask), arg.type);
    auto tmp2 = assign(it, arg.type, "%popcount") = as_unsigned{tmp0} >> 2_val;
    tmp2 = assign(it, arg.type, "%popcount") = tmp2 & Value(Literal(0x33333333 & typeWidthMask), arg.type);
    auto tmp3 = assign(it, arg.type, "%popcount") = tmp1 + tmp2;

    auto tmp4 = assign(it, arg.type, "%popcount") = as_unsigned{tmp3} >> 4_val;
    tmp4 = assign(it, arg.type, "%popcount") = tmp3 + tmp4;
    tmp4 = assign(it, arg.type, "%popcount") = tmp4 & Value(Literal(0x0F0F0F0F & typeWidthMask), arg.type);

    // This mul is "harmless", since it should be converted to shift and adds anyway!
    auto tmp5 = method.addNewLocal(arg.type, "%popcount");
    it.emplace(new IntrinsicOperation(
        "mul", Value(tmp5), std::move(tmp4), Value(Literal(0x01010101 & typeWidthMask), arg.type)));
    it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
    it.nextInBlock();

    auto tmp6 = assign(it, arg.type, "%popcount") = as_unsigned{tmp5} >> Value(Literal(typeWidth - CHAR_BIT), arg.type);
    // This is not listed in the original version above, but required, since we do 32-bit calculation and not
    // calculation limited to type width, i.e. we might have non-zero upper bytes for smaller types.
    // Since we shift above all but the upper most byte out of the word, the result is never more than 1 byte wide
    it.reset((new Operation(OP_AND, callSite->getOutput().value(), tmp6, 0xFF_val))->copyExtrasFrom(callSite));
    return it;
}

struct Intrinsic
{
    const IntrinsicFunction func;
    const Optional<UnaryInstruction> unaryInstr;
    const Optional<BinaryInstruction> binaryInstr;

    explicit Intrinsic(const IntrinsicFunction& func) : func(func) {}
    Intrinsic(const IntrinsicFunction& func, const UnaryInstruction& unary) : func(func), unaryInstr(unary) {}
    Intrinsic(const IntrinsicFunction& func, const BinaryInstruction& binary) : func(func), binaryInstr(binary) {}
};

static UnaryInstruction calculateIntrinsic(DataType resultBaseType, std::function<Literal(Literal)>&& func)
{
    return [=](const Value& val) {
        auto resultType = resultBaseType.toVectorType(val.type.getVectorWidth());
        if(auto lit = val.getLiteralValue())
            return Value(func(*lit), resultType);
        else if(auto vec = val.checkVector())
        {
            SIMDVector resultVector{UNDEFINED_LITERAL};
            for(uint8_t i = 0; i < resultVector.size(); ++i)
                resultVector[i] = func((*vec)[i]);
            return SIMDVectorHolder::storeVector(std::move(resultVector), resultType, vec->getStorage());
        }
        throw CompilationError(
            CompilationStep::NORMALIZER, "Invalid value type for pre-calculation of intrinsic", val.to_string());
    };
}

/*
 * NOTE: We sort intrinsics in descending order on purpose, to correctly select e.g. fmaxabs for vc4cl_fmaxabs (and not
 * fmax)
 */
const static std::map<std::string, Intrinsic, std::greater<std::string>> nonaryInstrinsics = {
    {"vc4cl_mutex_lock", Intrinsic{intrinsifyMutexAccess(true)}},
    {"vc4cl_mutex_unlock", Intrinsic{intrinsifyMutexAccess(false)}},
    {"vc4cl_element_number", Intrinsic{intrinsifyValueRead(ELEMENT_NUMBER_REGISTER)}},
    {"vc4cl_qpu_number", Intrinsic{intrinsifyValueRead(Value(REG_QPU_NUMBER, TYPE_INT8))}}};

const static std::map<std::string, Intrinsic, std::greater<std::string>> unaryIntrinsicMapping = {
    {"vc4cl_ftoi",
        Intrinsic{intrinsifyUnaryALUInstruction(OP_FTOI.name),
            [](const Value& val) { return OP_FTOI(val, NO_VALUE).first.value(); }}},
    {"vc4cl_itof",
        Intrinsic{intrinsifyUnaryALUInstruction(OP_ITOF.name),
            [](const Value& val) { return OP_ITOF(val, NO_VALUE).first.value(); }}},
    {"vc4cl_clz",
        Intrinsic{intrinsifyUnaryALUInstruction(OP_CLZ.name),
            [](const Value& val) { return OP_CLZ(val, NO_VALUE).first.value(); }}},
    {"vc4cl_sfu_rsqrt",
        Intrinsic{intrinsifySFUInstruction(REG_SFU_RECIP_SQRT),
            [](const Value& val) { return periphery::precalculateSFU(REG_SFU_RECIP_SQRT, val); }}},
    {"vc4cl_sfu_exp2",
        Intrinsic{intrinsifySFUInstruction(REG_SFU_EXP2),
            [](const Value& val) { return periphery::precalculateSFU(REG_SFU_EXP2, val); }}},
    {"vc4cl_sfu_log2",
        Intrinsic{intrinsifySFUInstruction(REG_SFU_LOG2),
            [](const Value& val) { return periphery::precalculateSFU(REG_SFU_LOG2, val); }}},
    {"vc4cl_sfu_recip",
        Intrinsic{intrinsifySFUInstruction(REG_SFU_RECIP),
            [](const Value& val) { return periphery::precalculateSFU(REG_SFU_RECIP, val); }}},
    {"vc4cl_semaphore_increment", Intrinsic{intrinsifySemaphoreAccess(true)}},
    {"vc4cl_semaphore_decrement", Intrinsic{intrinsifySemaphoreAccess(false)}},
    {"vc4cl_dma_read", Intrinsic{intrinsifyMemoryAccess(MemoryAccess::READ, false)}},
    {"vc4cl_unpack_sext", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_NOP, UNPACK_SHORT_TO_INT_SEXT)}},
    {"vc4cl_unpack_color_byte0", Intrinsic{intrinsifyUnaryALUInstruction(OP_FMIN.name, false, PACK_NOP, UNPACK_8A_32)}},
    {"vc4cl_unpack_color_byte1", Intrinsic{intrinsifyUnaryALUInstruction(OP_FMIN.name, false, PACK_NOP, UNPACK_8B_32)}},
    {"vc4cl_unpack_color_byte2", Intrinsic{intrinsifyUnaryALUInstruction(OP_FMIN.name, false, PACK_NOP, UNPACK_8C_32)}},
    {"vc4cl_unpack_color_byte3", Intrinsic{intrinsifyUnaryALUInstruction(OP_FMIN.name, false, PACK_NOP, UNPACK_8D_32)}},
    {"vc4cl_unpack_byte0", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_NOP, UNPACK_8A_32)}},
    {"vc4cl_unpack_byte1", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_NOP, UNPACK_8B_32)}},
    {"vc4cl_unpack_byte2", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_NOP, UNPACK_8C_32)}},
    {"vc4cl_unpack_byte3", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_NOP, UNPACK_8D_32)}},
    {"vc4cl_pack_truncate", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_INT_TO_SHORT_TRUNCATE)}},
    {"vc4cl_replicate_lsb", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_32_8888)}},
    {"vc4cl_pack_lsb", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_INT_TO_CHAR_TRUNCATE)}},
    {"vc4cl_saturate_lsb", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_INT_TO_UNSIGNED_CHAR_SATURATE)}},
    {"vc4cl_is_nan",
        Intrinsic{intrinsifyCheckNaN(false),
            calculateIntrinsic(TYPE_INT32, [](const Literal& lit) { return std::isnan(lit.real()) ? 1_lit : 0_lit; })}},
    {"vc4cl_is_inf_nan",
        Intrinsic{intrinsifyCheckNaN(true),
            calculateIntrinsic(TYPE_INT32,
                [](const Literal& lit) { return std::isnan(lit.real()) || std::isinf(lit.real()) ? 1_lit : 0_lit; })}},
    {"vc4cl_vload3", Intrinsic{intrinsifyMemoryAccess(MemoryAccess::READ, true)}},
    /* simply set the event to something so it is initialized */
    {"vc4cl_set_event", Intrinsic{intrinsifyValueRead(INT_ZERO), [](const Value& val) -> Value { return INT_ZERO; }}},
    {"vc4cl_barrier", Intrinsic{intrinsifyBarrier}},
    {"vc4cl_popcount", Intrinsic{intrinsifyPopcount}},
};

const static std::map<std::string, Intrinsic, std::greater<std::string>> binaryIntrinsicMapping = {
    {"vc4cl_fmax",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_FMAX.name),
            [](const Value& val0, const Value& val1) { return OP_FMAX(val0, val1).first.value(); }}},
    {"vc4cl_fmin",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_FMIN.name),
            [](const Value& val0, const Value& val1) { return OP_FMIN(val0, val1).first.value(); }}},
    {"vc4cl_fmaxabs",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_FMAXABS.name),
            [](const Value& val0, const Value& val1) { return OP_FMAXABS(val0, val1).first.value(); }}},
    {"vc4cl_fminabs",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_FMINABS.name),
            [](const Value& val0, const Value& val1) { return OP_FMINABS(val0, val1).first.value(); }}},
    {"vc4cl_asr",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_ASR.name),
            [](const Value& val0, const Value& val1) { return OP_ASR(val0, val1).first.value(); }}},
    {"vc4cl_ror",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_ROR.name),
            [](const Value& val0, const Value& val1) { return OP_ROR(val0, val1).first.value(); }}},
    {"vc4cl_min",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_MIN.name, true),
            [](const Value& val0, const Value& val1) { return OP_MIN(val0, val1).first.value(); }}},
    {"vc4cl_max",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_MAX.name, true),
            [](const Value& val0, const Value& val1) { return OP_MAX(val0, val1).first.value(); }}},
    {"vc4cl_and",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_AND.name),
            [](const Value& val0, const Value& val1) { return OP_AND(val0, val1).first.value(); }}},
    {"vc4cl_mul24",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_MUL24.name, true),
            [](const Value& val0, const Value& val1) { return OP_MUL24(val0, val1).first.value(); }}},
    {"vc4cl_dma_write", Intrinsic{intrinsifyMemoryAccess(MemoryAccess::WRITE, false)}},
    {"vc4cl_vector_rotate", Intrinsic{intrinsifyVectorRotation()}},
    // the 32-bit saturation MUST BE applied to the over-/underflowing operation
    {"vc4cl_saturated_add", Intrinsic{intrinsifyBinaryALUInstruction(OP_ADD.name, false, PACK_32_32)}},
    // NOTE: Since 32-bit saturation pack mode works in an unexpected manner for integer subtraction, we cannot simply
    // issue a single subtraction operation
    {"vc4cl_saturated_sub", Intrinsic{intrinsifySaturatedSubtraction}},
    {"vc4cl_add_flags",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_ADD.name, false, PACK_NOP, UNPACK_NOP, true),
            /* can't set flags for pre-calculation, so don't */}},
    {"vc4cl_sub_flags",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_SUB.name, false, PACK_NOP, UNPACK_NOP, true),
            /* can't set flags for pre-calculation, so don't */}},
    {"vc4cl_prefetch", Intrinsic{[](Method& m, InstructionWalker it, const MethodCall* call) -> InstructionWalker {
         /* for now do nothing, TODO make use of this! */
         CPPLOG_LAZY(logging::Level::DEBUG, log << "Dropping intrinsic function: " << it->to_string() << logging::endl);
         return it.erase();
     }}},
    {"vc4cl_v8adds",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_V8ADDS.name, false),
            [](const Value& val0, const Value& val1) { return OP_V8ADDS(val0, val1).first.value(); }}},
    {"vc4cl_v8subs",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_V8SUBS.name, false),
            [](const Value& val0, const Value& val1) { return OP_V8SUBS(val0, val1).first.value(); }}},
    {"vc4cl_v8min",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_V8MIN.name, false),
            [](const Value& val0, const Value& val1) { return OP_V8MIN(val0, val1).first.value(); }}},
    {"vc4cl_v8max",
        Intrinsic{intrinsifyBinaryALUInstruction(OP_V8MAX.name, false),
            [](const Value& val0, const Value& val1) { return OP_V8MAX(val0, val1).first.value(); }}},
    {"vc4cl_vstore3", Intrinsic{intrinsifyMemoryAccess(MemoryAccess::WRITE, true)}},
    {"vc4cl_mul_hi", Intrinsic{intrinsifyIntegerMultiplicationHighPart}}};

const static std::map<std::string, Intrinsic, std::greater<std::string>> ternaryIntrinsicMapping = {
    {"vc4cl_dma_copy", Intrinsic{intrinsifyMemoryAccess(MemoryAccess::COPY, false)}},
    {"vc4cl_flag_cond", Intrinsic{intrinsifyFlagCondition}}};

const static std::map<std::string, std::pair<Intrinsic, Optional<Value>>, std::greater<std::string>>
    typeCastIntrinsics = {
        // since we run all the (not intrinsified) calculations with 32-bit, don't truncate signed conversions to
        // smaller types
        // TODO correct?? Since we do not discard out-of-bounds values!
        {"vc4cl_bitcast_uchar",
            {Intrinsic{intrinsifyBinaryALUInstruction("and", true),
                 calculateIntrinsic(TYPE_INT8, [](const Literal& lit) { return Literal(lit.unsignedInt() & 0xFF); })},
                Value(Literal(0xFFu), TYPE_INT8)}},
        {"vc4cl_bitcast_char",
            {Intrinsic{intrinsifyBinaryALUInstruction("mov"),
                 calculateIntrinsic(TYPE_INT8, [](const Literal& lit) { return lit; })},
                NO_VALUE}},
        {"vc4cl_bitcast_ushort",
            {Intrinsic{intrinsifyBinaryALUInstruction("and", true),
                 calculateIntrinsic(
                     TYPE_INT16, [](const Literal& lit) { return Literal(lit.unsignedInt() & 0xFFFF); })},
                Value(Literal(0xFFFFu), TYPE_INT16)}},
        {"vc4cl_bitcast_short",
            {Intrinsic{intrinsifyBinaryALUInstruction("mov"),
                 calculateIntrinsic(TYPE_INT16, [](const Literal& lit) { return lit; })},
                NO_VALUE}},
        {"vc4cl_bitcast_uint",
            {Intrinsic{intrinsifyBinaryALUInstruction("mov", true),
                 calculateIntrinsic(TYPE_INT32, [](const Literal& lit) { return lit; })},
                NO_VALUE}},
        {"vc4cl_bitcast_int",
            {Intrinsic{intrinsifyBinaryALUInstruction("mov"),
                 calculateIntrinsic(TYPE_INT32, [](const Literal& lit) { return lit; })},
                NO_VALUE}},
        {"vc4cl_bitcast_float",
            {Intrinsic{intrinsifyBinaryALUInstruction("mov"),
                 calculateIntrinsic(TYPE_FLOAT, [](const Literal& lit) { return lit; })},
                NO_VALUE}}};

static bool intrinsifyNoArgs(Method& method, InstructionWalker it)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite == nullptr)
    {
        return false;
    }
    if(callSite->getArguments().size() > 1 /* check for sign-flag too*/)
    {
        return false;
    }
    for(const auto& pair : nonaryInstrinsics)
    {
        if(callSite->methodName.find(pair.first) != std::string::npos)
        {
            pair.second.func(method, it, callSite);
            return true;
        }
    }
    return false;
}

static bool intrinsifyUnary(Method& method, InstructionWalker it)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite == nullptr)
    {
        return false;
    }
    if(callSite->getArguments().empty() || callSite->getArguments().size() > 2 /* check for sign-flag too*/)
    {
        return false;
    }
    const Value& arg = callSite->assertArgument(0);
    Optional<Value> result = NO_VALUE;
    for(const auto& pair : unaryIntrinsicMapping)
    {
        if(callSite->methodName.find(pair.first) != std::string::npos)
        {
            if((arg.getLiteralValue() || arg.checkVector()) && pair.second.unaryInstr &&
                (result = pair.second.unaryInstr.value()(arg)))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Intrinsifying unary '" << callSite->to_string()
                        << "' to pre-calculated value: " << result->to_string() << logging::endl);
                it.reset(new MoveOperation(callSite->getOutput().value(), result.value()));
            }
            else
                pair.second.func(method, it, callSite);
            return true;
        }
    }
    for(const auto& pair : typeCastIntrinsics)
    {
        if(callSite->methodName.find(pair.first) != std::string::npos)
        {
            // TODO support constant type-cast for constant containers
            if(arg.checkLiteral() && pair.second.first.unaryInstr &&
                (result = pair.second.first.unaryInstr.value()(arg)))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Intrinsifying type-cast '" << callSite->to_string()
                        << "' to pre-calculated value: " << result->to_string() << logging::endl);
                it.reset(new MoveOperation(callSite->getOutput().value(), result.value()));
            }
            else if(!pair.second.second) // there is no value to apply -> simple move
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Intrinsifying '" << callSite->to_string() << "' to simple move" << logging::endl);
                it.reset(new MoveOperation(callSite->getOutput().value(), arg));
            }
            else
            {
                // TODO could use pack-mode here, but only for UNSIGNED values!!
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Intrinsifying '" << callSite->to_string() << "' to operation with constant "
                        << pair.second.second.to_string() << logging::endl);
                callSite->setArgument(1, pair.second.second.value());
                pair.second.first.func(method, it, callSite);
            }
            return true;
        }
    }
    return false;
}

static bool intrinsifyBinary(Method& method, InstructionWalker it)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite == nullptr)
    {
        return false;
    }
    if(callSite->getArguments().size() < 2 || callSite->getArguments().size() > 3 /* check for sign-flag too*/)
    {
        return false;
    }
    for(const auto& pair : binaryIntrinsicMapping)
    {
        if(callSite->methodName.find(pair.first) != std::string::npos)
        {
            if(callSite->assertArgument(0).checkLiteral() && callSite->assertArgument(1).checkLiteral() &&
                pair.second.binaryInstr &&
                pair.second.binaryInstr.value()(callSite->assertArgument(0), callSite->assertArgument(1)))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Intrinsifying binary '" << callSite->to_string() << "' to pre-calculated value"
                        << logging::endl);
                it.reset(new MoveOperation(callSite->getOutput().value(),
                    pair.second.binaryInstr.value()(callSite->assertArgument(0), callSite->assertArgument(1)).value()));
            }
            else
                pair.second.func(method, it, callSite);
            return true;
        }
    }
    return false;
}

static bool intrinsifyTernary(Method& method, InstructionWalker it)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite == nullptr)
    {
        return false;
    }
    if(callSite->getArguments().size() < 3 || callSite->getArguments().size() > 4 /* check for sign-flag too*/)
    {
        return false;
    }
    for(const auto& pair : ternaryIntrinsicMapping)
    {
        if(callSite->methodName.find(pair.first) != std::string::npos)
        {
            pair.second.func(method, it, callSite);
            return true;
        }
    }
    return false;
}

static constexpr uint32_t getMaximumUnsignedValue(uint8_t numBits)
{
    return static_cast<uint32_t>((uint64_t{1} << numBits) - 1u);
}

static constexpr int32_t getMaximumSignedValue(uint8_t numBits)
{
    return static_cast<int32_t>((int64_t{1} << (numBits - 1)) - 1);
}

static constexpr int32_t getMinimumSignedValue(uint8_t numBits)
{
    return static_cast<int32_t>(-(int64_t{1} << (numBits - 1)));
}

static_assert(getMaximumUnsignedValue(32) == std::numeric_limits<uint32_t>::max(), "");
static_assert(getMaximumUnsignedValue(16) == std::numeric_limits<uint16_t>::max(), "");
static_assert(getMaximumUnsignedValue(8) == std::numeric_limits<uint8_t>::max(), "");

static_assert(getMaximumSignedValue(32) == std::numeric_limits<int32_t>::max(), "");
static_assert(getMaximumSignedValue(16) == std::numeric_limits<int16_t>::max(), "");
static_assert(getMaximumSignedValue(8) == std::numeric_limits<int8_t>::max(), "");

static_assert(getMinimumSignedValue(32) == std::numeric_limits<int32_t>::min(), "");
static_assert(getMinimumSignedValue(16) == std::numeric_limits<int16_t>::min(), "");
static_assert(getMinimumSignedValue(8) == std::numeric_limits<int8_t>::min(), "");

static bool intrinsifyArithmetic(Method& method, InstructionWalker it, const MathType& mathType)
{
    IntrinsicOperation* op = it.get<IntrinsicOperation>();
    if(op == nullptr)
    {
        return false;
    }
    const Value& arg0 = op->getFirstArg();
    const Value& arg1 = op->getSecondArg().value_or(UNDEFINED_VALUE);
    // integer multiplication
    if(op->opCode == "mul")
    {
        // a * b = b * a
        if(Local::getLocalData<MultiRegisterData>(arg0.checkLocal()) ||
            Local::getLocalData<MultiRegisterData>(arg1.checkLocal()))
        {
            // 64-bit multiplication
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Calculating result for 64-bit integer multiplication: " << op->to_string() << logging::endl);
            it = intrinsifyLongMultiplication(method, it, *op);
        }
        else if(arg0.getLiteralValue() && arg1.getLiteralValue())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Calculating result for multiplication with constants: " << op->to_string() << logging::endl);
            it.reset((new MoveOperation(Value(op->getOutput()->local(), arg0.type),
                          Value(Literal(arg0.getLiteralValue()->signedInt() * arg1.getLiteralValue()->signedInt()),
                              arg0.type)))
                         ->copyExtrasFrom(it.get()));
        }
        else if(arg0.getLiteralValue() && arg0.getLiteralValue()->signedInt() > 0 &&
            isPowerTwo(arg0.getLiteralValue()->unsignedInt()))
        {
            // a * 2^n = a << n
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying multiplication with left-shift: " << op->to_string() << logging::endl);
            it.reset(
                (new Operation(OP_SHL, op->getOutput().value(), arg1,
                     Value(Literal(static_cast<int32_t>(std::log2(arg0.getLiteralValue()->signedInt()))), arg0.type)))
                    ->copyExtrasFrom(it.get()));
        }
        else if(arg1.getLiteralValue() && arg1.getLiteralValue()->signedInt() > 0 &&
            isPowerTwo(arg1.getLiteralValue()->unsignedInt()))
        {
            // a * 2^n = a << n
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying multiplication with left-shift: " << op->to_string() << logging::endl);
            it.reset(
                (new Operation(OP_SHL, op->getOutput().value(), op->getFirstArg(),
                     Value(Literal(static_cast<int32_t>(std::log2(arg1.getLiteralValue()->signedInt()))), arg1.type)))
                    ->copyExtrasFrom(it.get()));
        }
        else if(std::max(arg0.type.getScalarBitCount(), arg1.type.getScalarBitCount()) <= 24)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying multiplication of small integers to mul24: " << op->to_string() << logging::endl);
            it.reset((new Operation(OP_MUL24, op->getOutput().value(), op->getFirstArg(), op->assertArgument(1)))
                         ->copyExtrasFrom(it.get()));
        }
        else if(arg0.getLiteralValue() && arg0.getLiteralValue()->signedInt() > 0 &&
            isPowerTwo(arg0.getLiteralValue()->unsignedInt() + 1))
        {
            // x * (2^k - 1) = x * 2^k - x = x << k - x
            // This is a special case of the "binary method", but since the "binary method" only applies shifts and
            // adds, we handle shift and minus separately.
            // TODO could make more general, similar to "binary method" implementation/integrate into that
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying multiplication with left-shift and minus: " << op->to_string() << logging::endl);
            auto tmp = assign(it, arg1.type, "%mul_shift") = (arg1
                << Value(Literal(static_cast<int32_t>(std::log2(arg0.getLiteralValue()->signedInt() + 1))), arg0.type));
            it.reset((new Operation(OP_SUB, op->getOutput().value(), tmp, arg1))->copyExtrasFrom(it.get()));
        }
        else if(arg1.getLiteralValue() && arg1.getLiteralValue()->signedInt() > 0 &&
            isPowerTwo(arg1.getLiteralValue()->unsignedInt() + 1))
        {
            // x * (2^k - 1) = x * 2^k - x = x << k - x
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying multiplication with left-shift and minus: " << op->to_string() << logging::endl);
            auto tmp = assign(it, arg0.type, "%mul_shift") = (arg0
                << Value(Literal(static_cast<int32_t>(std::log2(arg1.getLiteralValue()->signedInt() + 1))), arg0.type));
            it.reset((new Operation(OP_SUB, op->getOutput().value(), tmp, arg0))->copyExtrasFrom(it.get()));
        }
        else if(canOptimizeMultiplicationWithBinaryMethod(*op))
        {
            // e.g. x * 3 = x << 1 + x
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying multiplication via binary method: " << op->to_string() << logging::endl);
            it = intrinsifyIntegerMultiplicationViaBinaryMethod(method, it, *op);
        }
        else if(std::all_of(op->getArguments().begin(), op->getArguments().end(), [](const Value& arg) -> bool {
                    return vc4c::analysis::ValueRange::getValueRange(arg).isUnsigned();
                }))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying signed multiplication of purely unsigned values to unsigned multiplication: "
                    << op->to_string() << logging::endl);
            it = intrinsifyUnsignedIntegerMultiplication(method, it, *op);
        }
        else
            it = intrinsifySignedIntegerMultiplication(method, it, *op);
        return true;
    }
    // unsigned division
    else if(op->opCode == "udiv")
    {
        if(arg0.getLiteralValue() && arg1.getLiteralValue())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Calculating result for division with constants: " << op->to_string() << logging::endl);
            it.reset((new MoveOperation(Value(op->getOutput()->local(), arg0.type),
                          Value(Literal(arg0.getLiteralValue()->unsignedInt() / arg1.getLiteralValue()->unsignedInt()),
                              arg0.type)))
                         ->copyExtrasFrom(it.get()));
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        }
        // a / 2^n = a >> n
        else if(arg1.getLiteralValue() && arg1.getLiteralValue()->signedInt() > 0 &&
            isPowerTwo(arg1.getLiteralValue()->unsignedInt()))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying division with right-shift: " << op->to_string() << logging::endl);
            it.reset(
                (new Operation(OP_SHR, op->getOutput().value(), op->getFirstArg(),
                     Value(Literal(static_cast<int32_t>(std::log2(arg1.getLiteralValue()->unsignedInt()))), arg1.type)))
                    ->copyExtrasFrom(it.get()));
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        }
        else if((arg1.isLiteralValue() || arg1.checkVector()) && arg0.type.getScalarBitCount() <= 16)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying unsigned division by constant: " << op->to_string() << logging::endl);
            // XXX is never used, LLVM always uses i32 for division??
            it = intrinsifyUnsignedIntegerDivisionByConstant(method, it, *op);
        }
        else if(arg0.type.getScalarBitCount() < 24 && arg1.type.getScalarBitCount() < 24)
        {
            // possible for |type| < 24, since for -2^23 <= x <= 2^23 float is an exact representation
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying unsigned division with floating-point division: " << op->to_string()
                    << logging::endl);
            auto tmpArg0 = method.addNewLocal(TYPE_INT32.toVectorType(arg0.type.getVectorWidth()), "%div");
            it = insertZeroExtension(it, method, arg0, tmpArg0, true);
            op->setArgument(0, tmpArg0);
            auto tmpArg1 = method.addNewLocal(TYPE_INT32.toVectorType(arg1.type.getVectorWidth()), "%div");
            it = insertZeroExtension(it, method, arg1, tmpArg1, true);
            op->setArgument(1, tmpArg1);
            it = intrinsifyIntegerDivisionByFloatingDivision(method, it, *op);
        }
        else
            it = intrinsifyUnsignedIntegerDivision(method, it, *op);
        return true;
    }
    // signed division
    else if(op->opCode == "sdiv")
    {
        if(arg0.getLiteralValue() && arg1.getLiteralValue())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Calculating result for signed division with constants: " << op->to_string() << logging::endl);
            it.reset((new MoveOperation(Value(op->getOutput()->local(), arg0.type),
                          Value(Literal(arg0.getLiteralValue()->signedInt() / arg1.getLiteralValue()->signedInt()),
                              arg0.type)))
                         ->copyExtrasFrom(it.get()));
        }
        // a / 2^n = (abs(a) >> n) * sign(a)
        else if(arg1.isLiteralValue() && arg1.getLiteralValue()->signedInt() > 0 &&
            isPowerTwo(arg1.getLiteralValue()->unsignedInt()))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying signed division with right-shift and sign-copy: " << op->to_string()
                    << logging::endl);
            Value tmp = method.addNewLocal(arg1.type, "%unsigned");
            Value sign = UNDEFINED_VALUE;
            it = insertMakePositive(it, method, arg0, tmp, sign);
            Value tmpResult = assign(it, op->getOutput()->type) = (as_unsigned{tmp} >>
                    Value(Literal(static_cast<int32_t>(std::log2(arg1.getLiteralValue()->unsignedInt()))), arg1.type),
                InstructionDecorations::UNSIGNED_RESULT);
            Value tmpResult2 = op->getOutput().value();
            it = insertRestoreSign(it, method, tmpResult, tmpResult2, sign);
            if(!(tmpResult2 == op->getOutput().value()))
                it.reset(new MoveOperation(op->getOutput().value(), tmpResult2));
            else
            {
                it = it.erase();
                // so next instruction is not skipped
                it.previousInBlock();
            }
        }
        else if((arg1.isLiteralValue() || arg1.checkVector()) && arg0.type.getScalarBitCount() <= 16)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying signed division by constant: " << op->to_string() << logging::endl);
            // XXX is never used, LLVM always uses i32 for division??
            it = intrinsifySignedIntegerDivisionByConstant(method, it, *op);
        }
        else if(arg0.type.getScalarBitCount() < 24 && arg1.type.getScalarBitCount() < 24)
        {
            // possible for |type| < 24, since for -2^23 <= x <= 2^23 float is an exact representation
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying signed division with floating-point division: " << op->to_string()
                    << logging::endl);
            auto tmpArg0 = method.addNewLocal(TYPE_INT32.toVectorType(arg0.type.getVectorWidth()), "%div");
            it = insertSignExtension(it, method, arg0, tmpArg0, true);
            op->setArgument(0, tmpArg0);
            auto tmpArg1 = method.addNewLocal(TYPE_INT32.toVectorType(arg1.type.getVectorWidth()), "%div");
            it = insertSignExtension(it, method, arg1, tmpArg1, true);
            op->setArgument(1, tmpArg1);
            it = intrinsifyIntegerDivisionByFloatingDivision(method, it, *op);
        }
        else
            it = intrinsifySignedIntegerDivision(method, it, *op);
        return true;
    }
    // unsigned modulo
    // LLVM IR calls it urem, SPIR-V umod
    else if(op->opCode == "urem" || op->opCode == "umod")
    {
        if(arg0.getLiteralValue() && arg1.getLiteralValue())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Calculating result for modulo with constants: " << op->to_string() << logging::endl);
            it.reset((new MoveOperation(Value(op->getOutput()->local(), arg0.type),
                          Value(Literal(arg0.getLiteralValue()->unsignedInt() % arg1.getLiteralValue()->unsignedInt()),
                              arg0.type)))
                         ->copyExtrasFrom(it.get()));
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        }
        // a % 2^n = a & 2^n-1
        else if(arg1.getLiteralValue() && isPowerTwo(arg1.getLiteralValue()->unsignedInt()))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying unsigned modulo by power of two: " << op->to_string() << logging::endl);
            it.reset((new Operation(OP_AND, op->getOutput().value(), op->getFirstArg(),
                          Value(Literal(arg1.getLiteralValue()->unsignedInt() - 1), arg1.type)))
                         ->copyExtrasFrom(it.get()));
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        }
        else if((arg1.isLiteralValue() || arg1.checkVector()) && arg0.type.getScalarBitCount() <= 16)
        {
            it = intrinsifyUnsignedIntegerDivisionByConstant(method, it, *op, true);
        }
        else if(arg0.type.getScalarBitCount() < 24 && arg1.type.getScalarBitCount() < 24)
        {
            // possible for |type| < 24, since for -2^23 <= x <= 2^23 float is an exact representation
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying unsigned modulo with floating-point division: " << op->to_string()
                    << logging::endl);
            auto tmpArg0 = method.addNewLocal(TYPE_INT32.toVectorType(arg0.type.getVectorWidth()), "%mod");
            it = insertZeroExtension(it, method, arg0, tmpArg0, true);
            op->setArgument(0, tmpArg0);
            auto tmpArg1 = method.addNewLocal(TYPE_INT32.toVectorType(arg1.type.getVectorWidth()), "%mod");
            it = insertZeroExtension(it, method, arg1, tmpArg1, true);
            op->setArgument(1, tmpArg1);
            it = intrinsifyIntegerDivisionByFloatingDivision(method, it, *op, true);
        }
        else
            it = intrinsifyUnsignedIntegerDivision(method, it, *op, true);
        return true;
    }
    // signed modulo
    else if(op->opCode == "srem")
    {
        if(arg0.getLiteralValue() && arg1.getLiteralValue())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Calculating result for signed modulo with constants: " << op->to_string() << logging::endl);
            it.reset((new MoveOperation(Value(op->getOutput()->local(), arg0.type),
                          Value(Literal(arg0.getLiteralValue()->signedInt() % arg1.getLiteralValue()->signedInt()),
                              arg0.type)))
                         ->copyExtrasFrom(it.get()));
        }
        // a % 2^n = (abs(a) & 2^n-1) * sign(a)
        else if(arg1.isLiteralValue() && arg1.getLiteralValue()->signedInt() > 0 &&
            isPowerTwo(arg1.getLiteralValue()->unsignedInt()))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying signed modulo by power of two: " << op->to_string() << logging::endl);
            Value tmp = method.addNewLocal(arg1.type, "%unsigned");
            Value sign = UNDEFINED_VALUE;
            it = insertMakePositive(it, method, arg0, tmp, sign);
            Value tmpResult = assign(it, op->getOutput()->type) =
                (tmp & Value(Literal(arg1.getLiteralValue()->unsignedInt() - 1), arg1.type),
                    InstructionDecorations::UNSIGNED_RESULT);
            Value tmpResult2 = op->getOutput().value();
            it = insertRestoreSign(it, method, tmpResult, tmpResult2, sign);
            if(!(tmpResult2 == op->getOutput().value()))
                it.reset(new MoveOperation(op->getOutput().value(), tmpResult2));
            else
            {
                it = it.erase();
                // so next instruction is not skipped
                it.previousInBlock();
            }
        }
        else if((arg1.isLiteralValue() || arg1.checkVector()) && arg0.type.getScalarBitCount() <= 16)
        {
            it = intrinsifySignedIntegerDivisionByConstant(method, it, *op, true);
        }
        else if(arg0.type.getScalarBitCount() < 24 && arg1.type.getScalarBitCount() < 24)
        {
            // possible for |type| < 24, since for -2^23 <= x <= 2^23 float is an exact representation
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying signed modulo with floating-point division: " << op->to_string()
                    << logging::endl);
            auto tmpArg0 = method.addNewLocal(TYPE_INT32.toVectorType(arg0.type.getVectorWidth()), "%mod");
            it = insertSignExtension(it, method, arg0, tmpArg0, true);
            op->setArgument(0, tmpArg0);
            auto tmpArg1 = method.addNewLocal(TYPE_INT32.toVectorType(arg1.type.getVectorWidth()), "%mod");
            it = insertSignExtension(it, method, arg1, tmpArg1, true);
            op->setArgument(1, tmpArg1);
            it = intrinsifyIntegerDivisionByFloatingDivision(method, it, *op, true);
        }
        else
            it = intrinsifySignedIntegerDivision(method, it, *op, true);
        return true;
    }
    // floating division
    else if(op->opCode == "fdiv")
    {
        if(arg0.getLiteralValue() && arg1.getLiteralValue())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Calculating result for signed division with constants: " << op->to_string() << logging::endl);
            it.reset((new MoveOperation(Value(op->getOutput()->local(), arg0.type),
                          Value(Literal(arg0.getLiteralValue()->real() / arg1.getLiteralValue()->real()), arg0.type)))
                         ->copyExtrasFrom(it.get()));
        }
        else if(arg1.getLiteralValue() || arg1.checkVector())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying floating division with multiplication of constant inverse: " << op->to_string()
                    << logging::endl);
            it.reset((new Operation(OP_FMUL, op->getOutput().value(), op->getFirstArg(),
                          periphery::precalculateSFU(REG_SFU_RECIP, arg1).value()))
                         ->copyExtrasFrom(it.get()));
        }
        else if(op->hasDecoration(InstructionDecorations::ALLOW_RECIP) ||
            op->hasDecoration(InstructionDecorations::FAST_MATH))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying floating division with multiplication of reciprocal: " << op->to_string()
                    << logging::endl);
            it = periphery::insertSFUCall(REG_SFU_RECIP, it, arg1);
            it.reset((new Operation(OP_FMUL, op->getOutput().value(), op->getFirstArg(),
                          Value(REG_SFU_OUT, op->getFirstArg().type)))
                         ->copyExtrasFrom(it.get()));
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying floating division with multiplication of inverse: " << op->to_string()
                    << logging::endl);
            it = intrinsifyFloatingDivision(method, it, *op);
        }
        return true;
    }
    // truncate bits
    else if(op->opCode == "trunc")
    {
        // if orig = i64, dest = i32 -> move
        // also applies to orig = i32, dest = i32
        // -> This can occur if original is i33 already truncated to i32 in front-end
        if(op->getFirstArg().type.getScalarBitCount() >= 32 && op->getOutput()->type.getScalarBitCount() == 32)
        {
            // do nothing, is just a move of the lower part
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying truncate from 64-bit to 32-bit with move of lower part: " << op->to_string()
                    << logging::endl);
            auto src = op->getFirstArg();
            if(auto data = Local::getLocalData<MultiRegisterData>(src.checkLocal()))
                src = data->lower->createReference();
            it.reset((new MoveOperation(op->getOutput().value(), src))->copyExtrasFrom(op));
        }
        // if dest < i32 -> orig & dest-bits or pack-code
        else if(op->getOutput()->type.getScalarBitCount() < 32)
        {
            CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying truncate with and" << logging::endl);
            it.reset((new Operation(OP_AND, op->getOutput().value(), op->getFirstArg(),
                          Value(Literal(op->getOutput()->type.getScalarWidthMask()), TYPE_INT32)))
                         ->copyExtrasFrom(it.get()));
        }
        else
            throw CompilationError(CompilationStep::NORMALIZER, "Unhandled truncation", op->to_string());
        return true;
    }
    else if(op->opCode == "fptrunc")
    {
        it = insertFloatingPointConversion(it, method, arg0, op->getOutput().value());
        // remove 'fptrunc'
        it.erase();
        return true;
    }
    // arithmetic shift right
    else if(op->opCode == "ashr")
    {
        if(op->getFirstArg().type.getScalarBitCount() < 32)
        {
            // for asr with signed types < 32-bit, we need to sign-extend to 32-bit first, since this might not yet be
            // done (e.g. when directly loaded from TMU)
            // TODO need some central/better way of correctly sign-extending the values always!
            auto tmp = method.addNewLocal(op->getFirstArg().type, "%asr.sext");
            it = insertSignExtension(it, method, op->getFirstArg(), tmp, true);
            it.reset(
                (new Operation(OP_ASR, op->getOutput().value(), tmp, op->assertArgument(1)))->copyExtrasFrom(it.get()));
        }
        else
            it.reset((new Operation(OP_ASR, op->getOutput().value(), op->getFirstArg(), op->assertArgument(1)))
                         ->copyExtrasFrom(it.get()));
        return true;
    }
    // integer to float
    else if(op->opCode == "sitofp")
    {
        it = insertSignedToFloatConversion(it, method, op->getFirstArg(), op->getOutput().value());
        // remove 'sitofp'
        it.erase();
        return true;
    }
    else if(op->opCode == "uitofp")
    {
        it = insertUnsignedToFloatConversion(it, method, op->getFirstArg(), op->getOutput().value());
        // remove 'uitofp'
        it.erase();
        return true;
    }
    // float to integer
    else if(op->opCode == "fptosi")
    {
        if(has_flag(op->decoration, intermediate::InstructionDecorations::SATURATED_CONVERSION))
        {
            // TODO this is only executed for SPIR-V, LLVM does the saturation in OpenCL C code, merge it!
            auto tmp = method.addNewLocal(op->getOutput()->type);
            it = insertFloatToIntegerSaturation(it, method, arg0, tmp,
                getMinimumSignedValue(tmp.type.getScalarBitCount()),
                static_cast<uint32_t>(getMaximumSignedValue(tmp.type.getScalarBitCount())));
            it.reset((new MoveOperation(*op->getOutput(), tmp))->copyExtrasFrom(op));
        }
        else
            it.reset((new Operation(OP_FTOI, op->getOutput().value(), op->getFirstArg()))->copyExtrasFrom(it.get()));
        return true;
    }
    // float to unsigned integer
    else if(op->opCode == "fptoui")
    {
        if(has_flag(op->decoration, InstructionDecorations::SATURATED_CONVERSION))
        {
            // TODO this is only executed for SPIR-V, LLVM does the saturation in OpenCL C code, merge it!
            auto tmp = method.addNewLocal(op->getOutput()->type);
            it = insertFloatToIntegerSaturation(
                it, method, arg0, tmp, 0, getMaximumUnsignedValue(tmp.type.getScalarBitCount()));
            it.reset((new MoveOperation(*op->getOutput(), tmp))
                         ->copyExtrasFrom(op)
                         ->addDecorations(InstructionDecorations::UNSIGNED_RESULT));
        }
        else if(op->getOutput()->type.getScalarBitCount() >= 32)
        {
            // x > 2^31-1 => ftoui(x) = 2^31 + ftoi(x -2^31)
            auto maxInt = static_cast<float>(std::numeric_limits<int32_t>::max());
            auto tmpFloat = assign(it, op->getFirstArg().type) =
                (op->getFirstArg() - Value(Literal(maxInt), TYPE_FLOAT), SetFlag::SET_FLAGS);
            auto tmpInt = method.addNewLocal(op->getOutput()->type);
            it.emplace(new Operation(OP_FTOI, tmpInt, tmpFloat, COND_NEGATIVE_CLEAR));
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
            it.nextInBlock();
            assign(it, op->getOutput().value()) = (tmpInt + Value(0x80000000_lit, TYPE_INT32), COND_NEGATIVE_CLEAR,
                InstructionDecorations::UNSIGNED_RESULT);

            // x <= 2^31-1 => ftoui(x) = ftoi(x)
            it.reset(new Operation(OP_FTOI, op->getOutput().value(), op->getFirstArg(), COND_NEGATIVE_SET));
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        }
        else
        {
            // converting negative float values to unsigned types is implementation defined anyway
            it.reset(new Operation(OP_FTOI, op->getOutput().value(), op->getFirstArg()));
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        }
        return true;
    }
    // sign extension
    else if(op->opCode == "sext")
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying sign extension with shifting: " << op->to_string() << logging::endl);
        it = insertSignExtension(it, method, op->getFirstArg(), op->getOutput().value(), true);
        // remove 'sext'
        it.erase();
        return true;
    }
    // zero extension
    else if(op->opCode == "zext")
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying zero extension with and: " << op->to_string() << logging::endl);
        it = insertZeroExtension(it, method, op->getFirstArg(), op->getOutput().value(), true);
        // remove 'zext'
        it.erase();
        return true;
    }
    // floating point conversion
    else if(op->opCode == "fpext")
    {
        it = insertFloatingPointConversion(it, method, op->getFirstArg(), op->getOutput().value());
        // remove 'fpext'
        it.erase();
        return true;
    }
    else if(op->opCode == "fneg")
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying unary floating-point negation to binary operation" << logging::endl);
        it.reset((new Operation(OP_FSUB, op->getOutput().value(), FLOAT_ZERO, op->getFirstArg()))->copyExtrasFrom(op));
        return true;
    }
    return false;
}

void intrinsics::intrinsify(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(!it.get<IntrinsicOperation>() && !it.get<MethodCall>())
        // fail fast
        return;
    if(intrinsifyComparison(method, it))
        return;
    if(intrinsifyWorkItemFunction(method, it))
        return;
    if(intrinsifyNoArgs(method, it))
        return;
    if(intrinsifyUnary(method, it))
        return;
    if(intrinsifyBinary(method, it))
        return;
    if(intrinsifyTernary(method, it))
        return;
    if(intrinsifyArithmetic(method, it, config.mathType))
        return;
    if(intrinsifyImageFunction(it, method))
        return;
}
