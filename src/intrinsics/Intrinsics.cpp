/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Intrinsics.h"

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
#include "log.h"

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
        if(packMode.hasEffect())
            it->setPackMode(packMode);
        if(unpackMode.hasEffect())
            it->setUnpackMode(unpackMode);
        if(setFlags)
            it->setSetFlags(SetFlag::SET_FLAGS);

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
            it->setPackMode(packMode);
        if(unpackMode.hasEffect())
            it->setUnpackMode(unpackMode);
        if(setFlags)
            it->setSetFlags(SetFlag::SET_FLAGS);

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
        it.reset((new MoveOperation(callSite->getOutput().value(), Value(REG_SFU_OUT, callSite->getOutput()->type)))
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

struct Intrinsic
{
    const IntrinsicFunction func;
    const Optional<UnaryInstruction> unaryInstr;
    const Optional<BinaryInstruction> binaryInstr;

    explicit Intrinsic(const IntrinsicFunction& func) : func(func) {}
    Intrinsic(const IntrinsicFunction& func, const UnaryInstruction& unary) : func(func), unaryInstr(unary) {}
    Intrinsic(const IntrinsicFunction& func, const BinaryInstruction& binary) : func(func), binaryInstr(binary) {}
};

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
    {"vc4cl_saturate_short", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_INT_TO_SIGNED_SHORT_SATURATE)}},
    {"vc4cl_saturate_lsb", Intrinsic{intrinsifyUnaryALUInstruction("mov", false, PACK_INT_TO_UNSIGNED_CHAR_SATURATE)}},
    {"vc4cl_is_nan",
        Intrinsic{intrinsifyCheckNaN(false),
            [](const Value& val) { return std::isnan(val.literal().real()) ? INT_ONE : INT_ZERO; }}},
    {"vc4cl_is_inf_nan",
        Intrinsic{intrinsifyCheckNaN(true),
            [](const Value& val) {
                return std::isnan(val.literal().real()) || std::isinf(val.literal().real()) ? INT_ONE : INT_ZERO;
            }}},
    {"vc4cl_vload3", Intrinsic{intrinsifyMemoryAccess(MemoryAccess::READ, true)}},
    /* simply set the event to something so it is initialized */
    {"vc4cl_set_event", Intrinsic{intrinsifyValueRead(INT_ZERO), [](const Value& val) -> Value { return INT_ZERO; }}}};

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
    {"vc4cl_saturated_sub", Intrinsic{intrinsifyBinaryALUInstruction(OP_SUB.name, false, PACK_32_32)}},
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
                 [](const Value& val) { return Value(Literal(val.literal().unsignedInt() & 0xFF), TYPE_INT8); }},
                Value(Literal(0xFFu), TYPE_INT8)}},
        {"vc4cl_bitcast_char",
            {Intrinsic{intrinsifyBinaryALUInstruction("mov"),
                 [](const Value& val) { return Value(val.literal(), TYPE_INT8); }},
                NO_VALUE}},
        {"vc4cl_bitcast_ushort",
            {Intrinsic{intrinsifyBinaryALUInstruction("and", true),
                 [](const Value& val) { return Value(Literal(val.literal().unsignedInt() & 0xFFFF), TYPE_INT16); }},
                Value(Literal(0xFFFFu), TYPE_INT16)}},
        {"vc4cl_bitcast_short",
            {Intrinsic{intrinsifyBinaryALUInstruction("mov"),
                 [](const Value& val) { return Value(val.literal(), TYPE_INT16); }},
                NO_VALUE}},
        {"vc4cl_bitcast_uint",
            {Intrinsic{intrinsifyBinaryALUInstruction("mov", true),
                 [](const Value& val) { return Value(Literal(val.literal()), TYPE_INT32); }},
                NO_VALUE}},
        {"vc4cl_bitcast_int",
            {Intrinsic{intrinsifyBinaryALUInstruction("mov"),
                 [](const Value& val) { return Value(val.literal(), TYPE_INT32); }},
                NO_VALUE}},
        {"vc4cl_bitcast_float",
            {Intrinsic{intrinsifyBinaryALUInstruction("mov"),
                 [](const Value& val) { return Value(Literal(val.literal()), TYPE_INT32); }},
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
                it.reset(new MoveOperation(
                    callSite->getOutput().value(), result.value(), callSite->conditional, callSite->setFlags));
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
                it.reset(new MoveOperation(
                    callSite->getOutput().value(), result.value(), callSite->conditional, callSite->setFlags));
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
                    pair.second.binaryInstr.value()(callSite->assertArgument(0), callSite->assertArgument(1)).value(),
                    callSite->conditional, callSite->setFlags));
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
    const bool saturateResult = op->hasDecoration(InstructionDecorations::SATURATED_CONVERSION);
    // integer multiplication
    if(op->opCode == "mul")
    {
        // a * b = b * a
        if(arg0.getLiteralValue() && arg1.getLiteralValue())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Calculating result for multiplication with constants: " << op->to_string() << logging::endl);
            it.reset((new MoveOperation(Value(op->getOutput()->local(), arg0.type),
                          Value(Literal(arg0.getLiteralValue()->signedInt() * arg1.getLiteralValue()->signedInt()),
                              arg0.type),
                          op->conditional, op->setFlags))
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
                     Value(Literal(static_cast<int32_t>(std::log2(arg0.getLiteralValue()->signedInt()))), arg0.type),
                     op->conditional, op->setFlags))
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
                     Value(Literal(static_cast<int32_t>(std::log2(arg1.getLiteralValue()->signedInt()))), arg1.type),
                     op->conditional, op->setFlags))
                    ->copyExtrasFrom(it.get()));
        }
        else if(std::max(arg0.type.getScalarBitCount(), arg1.type.getScalarBitCount()) <= 24)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying multiplication of small integers to mul24: " << op->to_string() << logging::endl);
            it.reset((new Operation(OP_MUL24, op->getOutput().value(), op->getFirstArg(), op->assertArgument(1),
                          op->conditional, op->setFlags))
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
            auto tmp = assign(it, arg1.type, "%mul_shift") =
                (arg1 << Value(
                     Literal(static_cast<int32_t>(std::log2(arg0.getLiteralValue()->signedInt() + 1))), arg0.type),
                    op->conditional);
            it.reset((new Operation(OP_SUB, op->getOutput().value(), tmp, arg1, op->conditional, op->setFlags))
                         ->copyExtrasFrom(it.get()));
        }
        else if(arg1.getLiteralValue() && arg1.getLiteralValue()->signedInt() > 0 &&
            isPowerTwo(arg1.getLiteralValue()->unsignedInt() + 1))
        {
            // x * (2^k - 1) = x * 2^k - x = x << k - x
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying multiplication with left-shift and minus: " << op->to_string() << logging::endl);
            auto tmp = assign(it, arg0.type, "%mul_shift") =
                (arg0 << Value(
                     Literal(static_cast<int32_t>(std::log2(arg1.getLiteralValue()->signedInt() + 1))), arg0.type),
                    op->conditional);
            it.reset((new Operation(OP_SUB, op->getOutput().value(), tmp, arg0, op->conditional, op->setFlags))
                         ->copyExtrasFrom(it.get()));
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
                              arg0.type),
                          op->conditional, op->setFlags))
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
                     Value(Literal(static_cast<int32_t>(std::log2(arg1.getLiteralValue()->unsignedInt()))), arg1.type),
                     op->conditional, op->setFlags))
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
                              arg0.type),
                          op->conditional, op->setFlags))
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
                op->conditional, op->setFlags, InstructionDecorations::UNSIGNED_RESULT);
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
        /*        // a / b = ftoi(itof(a) / itof(b))
                // possible for |type| < 24, since for -2^23 <= x <= 2^23 float is an exact representation
                else if(arg0.type.getScalarBitCount() < 24 && arg1.type.getScalarBitCount() < 24)
                {
                    logging::debug() << "Intrinsifying signed division with floating-point division" << logging::endl;
                    Value arg0Float = method.addNewLocal(TYPE_FLOAT.toVectorType(arg0.type.getVectorWidth()));
                    Value arg1Float = method.addNewLocal(TYPE_FLOAT.toVectorType(arg1.type.getVectorWidth()));
                    Value resultFloat =
           method.addNewLocal(TYPE_FLOAT.toVectorType(op->getOutput()->type.getVectorWidth())); it.emplace(new
           Operation(OP_ITOF, arg0Float, arg0)); it.nextInBlock(); it.emplace(new Operation(OP_ITOF, arg1Float, arg1));
                    it.nextInBlock();
                    //insert dummy instruction to replace
                    it.emplace(new IntrinsicOperation("fdiv", resultFloat, arg0Float, arg1Float));
                    it = intrinsifyFloatingDivision(method, it, *it.get<IntrinsicOperation>());
                    it.nextInBlock();
                    it.reset(new Operation(OP_FTOI, op->getOutput().value(), resultFloat));
                }
        */
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
                              arg0.type),
                          op->conditional, op->setFlags))
                         ->copyExtrasFrom(it.get()));
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        }
        // a % 2^n = a & 2^n-1
        else if(arg1.getLiteralValue() && isPowerTwo(arg1.getLiteralValue()->unsignedInt()))
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying unsigned modulo by power of two: " << op->to_string() << logging::endl);
            it.reset((new Operation(OP_AND, op->getOutput().value(), op->getFirstArg(),
                          Value(Literal(arg1.getLiteralValue()->unsignedInt() - 1), arg1.type), op->conditional,
                          op->setFlags))
                         ->copyExtrasFrom(it.get()));
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        }
        else if((arg1.isLiteralValue() || arg1.checkVector()) && arg0.type.getScalarBitCount() <= 16)
        {
            it = intrinsifyUnsignedIntegerDivisionByConstant(method, it, *op, true);
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
                              arg0.type),
                          op->conditional, op->setFlags))
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
                (tmp & Value(Literal(arg1.getLiteralValue()->unsignedInt() - 1), arg1.type), op->conditional,
                    op->setFlags, InstructionDecorations::UNSIGNED_RESULT);
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
                          Value(Literal(arg0.getLiteralValue()->real() / arg1.getLiteralValue()->real()), arg0.type),
                          op->conditional, op->setFlags))
                         ->copyExtrasFrom(it.get()));
        }
        else if(arg1.getLiteralValue() || arg1.checkVector())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying floating division with multiplication of constant inverse: " << op->to_string()
                    << logging::endl);
            it.reset((new Operation(OP_FMUL, op->getOutput().value(), op->getFirstArg(),
                          periphery::precalculateSFU(REG_SFU_RECIP, arg1).value(), op->conditional, op->setFlags))
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
                          Value(REG_SFU_OUT, op->getFirstArg().type), op->conditional, op->setFlags))
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
        if(saturateResult)
        {
            // let pack-mode handle saturation
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying saturated truncate with move and pack-mode: " << op->to_string()
                    << logging::endl);
            it = insertSaturation(
                it, method, op->getFirstArg(), op->getOutput().value(), ConversionType::UNSIGNED_TO_UNSIGNED);
            it.nextInBlock();
            it.erase();
        }
        // if orig = i64, dest = i32 -> move
        // also applies to orig = i32, dest = i32
        // -> This can occur if original is i33 already truncated to i32 in front-end
        else if(op->getFirstArg().type.getScalarBitCount() >= 32 && op->getOutput()->type.getScalarBitCount() == 32)
        {
            // do nothing, is just a move of the lower part
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying truncate from 64-bit to 32-bit with move of lower part: " << op->to_string()
                    << logging::endl);
            auto src = op->getFirstArg();
            if(auto data = Local::getLocalData<MultiRegisterData>(src.checkLocal()))
                src = data->lower->createReference();
            it.reset(
                (new MoveOperation(op->getOutput().value(), src, op->conditional, op->setFlags))->copyExtrasFrom(op));
        }
        // if dest < i32 -> orig & dest-bits or pack-code
        else if(op->getOutput()->type.getScalarBitCount() < 32)
        {
            CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying truncate with and" << logging::endl);
            it.reset((new Operation(OP_AND, op->getOutput().value(), op->getFirstArg(),
                          Value(Literal(op->getOutput()->type.getScalarWidthMask()), TYPE_INT32), op->conditional,
                          op->setFlags))
                         ->copyExtrasFrom(it.get()));
        }
        else
            throw CompilationError(CompilationStep::NORMALIZER, "Unhandled truncation", op->to_string());
        return true;
    }
    else if(op->opCode == "fptrunc")
    {
        if(saturateResult)
        {
            throw CompilationError(CompilationStep::NORMALIZER,
                "Saturation on floating-point conversion is not supported", op->to_string());
        }
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
            it = insertSignExtension(it, method, op->getFirstArg(), tmp, true, op->conditional);
            it.reset((new Operation(
                          OP_ASR, op->getOutput().value(), tmp, op->assertArgument(1), op->conditional, op->setFlags))
                         ->copyExtrasFrom(it.get()));
        }
        else
            it.reset((new Operation(OP_ASR, op->getOutput().value(), op->getFirstArg(), op->assertArgument(1),
                          op->conditional, op->setFlags))
                         ->copyExtrasFrom(it.get()));
        return true;
    }
    // integer to float
    else if(op->opCode == "sitofp")
    {
        // for non 32-bit types, need to sign-extend
        Value tmp = op->getFirstArg();
        if(op->getFirstArg().type.getScalarBitCount() < 32)
        {
            tmp = method.addNewLocal(TYPE_INT32, "%sitofp");
            it = insertSignExtension(it, method, op->getFirstArg(), tmp, true, op->conditional);
        }
        it.reset(new Operation(OP_ITOF, op->getOutput().value(), tmp, op->conditional, op->setFlags));
        return true;
    }
    else if(op->opCode == "uitofp")
    {
        const Value tmp = method.addNewLocal(op->getOutput()->type, "%uitofp");
        if(op->getFirstArg().type.getScalarBitCount() < 32)
        {
            // make sure, leading bits are zeroes
            const uint32_t mask = op->getFirstArg().type.getScalarWidthMask();
            assign(it, tmp) = (op->getFirstArg() & Value(Literal(mask), TYPE_INT32), op->conditional);
            it.reset(new Operation(OP_ITOF, op->getOutput().value(), tmp, op->conditional, op->setFlags));
        }
        else if(op->getFirstArg().type.getScalarBitCount() > 32)
        {
            throw CompilationError(CompilationStep::NORMALIZER,
                "Can't convert long to floating value, since long is not supported", op->to_string());
        }
        else // 32-bits
        {
            // TODO sometimes is off by 1 ULP, e.g. for 1698773569 gets 1698773504 instead of expected 1698773632
            // uitof(x) = y * uitof(x/y) + uitof(x & |y|), where |y| is the bits for y
            auto tmpInt = assign(it, op->getFirstArg().type) = op->getFirstArg() / 2_lit;
            auto tmpFloat = method.addNewLocal(op->getOutput()->type);
            it.emplace(new Operation(OP_ITOF, tmpFloat, tmpInt));
            it.nextInBlock();
            auto tmpFloat2 = assign(it, tmpFloat.type) = tmpFloat * Value(Literal(2.0f), TYPE_FLOAT);
            auto tmpInt2 = assign(it, op->getFirstArg().type) = op->getFirstArg() % 2_lit;
            auto tmpFloat3 = method.addNewLocal(op->getOutput()->type);
            it.emplace(new Operation(OP_ITOF, tmpFloat3, tmpInt2));
            it.nextInBlock();
            it.reset(new Operation(OP_FADD, op->getOutput().value(), tmpFloat2, tmpFloat3));
        }
        return true;
    }
    // float to integer
    else if(op->opCode == "fptosi")
    {
        if(has_flag(op->decoration, intermediate::InstructionDecorations::SATURATED_CONVERSION))
        {
            auto tmp = method.addNewLocal(op->getOutput()->type);
            it = insertFloatToIntegerSaturation(it, method, arg0, tmp,
                getMinimumSignedValue(tmp.type.getScalarBitCount()),
                static_cast<uint32_t>(getMaximumSignedValue(tmp.type.getScalarBitCount())));
            it.reset((new MoveOperation(*op->getOutput(), tmp))->copyExtrasFrom(op));
        }
        else
            it.reset((new Operation(OP_FTOI, op->getOutput().value(), op->getFirstArg(), op->conditional, op->setFlags))
                         ->copyExtrasFrom(it.get()));
        return true;
    }
    // float to unsigned integer
    else if(op->opCode == "fptoui")
    {
        if(has_flag(op->decoration, InstructionDecorations::SATURATED_CONVERSION))
        {
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
            it.reset(new Operation(OP_FTOI, op->getOutput().value(), op->getFirstArg(), op->conditional, op->setFlags));
            it->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        }
        return true;
    }
    // sign extension
    else if(op->opCode == "sext")
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying sign extension with shifting: " << op->to_string() << logging::endl);
        it = insertSignExtension(
            it, method, op->getFirstArg(), op->getOutput().value(), true, op->conditional, op->setFlags);
        // remove 'sext'
        it.erase();
        return true;
    }
    // zero extension
    else if(op->opCode == "zext")
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying zero extension with and: " << op->to_string() << logging::endl);
        it = insertZeroExtension(
            it, method, op->getFirstArg(), op->getOutput().value(), true, op->conditional, op->setFlags);
        // remove 'zext'
        it.erase();
        return true;
    }
    // floating point conversion
    else if(op->opCode == "fpext")
    {
        if(saturateResult)
        {
            throw CompilationError(CompilationStep::NORMALIZER,
                "Saturation on floating-point conversion is not supported", op->to_string());
        }
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

static NODISCARD InstructionWalker intrinsifyReadWorkGroupInfo(Method& method, InstructionWalker it, const Value& arg,
    const std::vector<BuiltinLocal::Type>& locals, const Value& defaultValue, const InstructionDecorations decoration)
{
    if(auto lit = arg.getLiteralValue())
    {
        Value src = UNDEFINED_VALUE;
        switch(lit->unsignedInt())
        {
        case 0:
            src = method.findOrCreateBuiltin(locals.at(0))->createReference();
            break;
        case 1:
            src = method.findOrCreateBuiltin(locals.at(1))->createReference();
            break;
        case 2:
            src = method.findOrCreateBuiltin(locals.at(2))->createReference();
            break;
        default:
            src = defaultValue;
        }
        return it.reset((new MoveOperation(it->getOutput().value(), src))->copyExtrasFrom(it.get()));
    }
    // set default value first and always, so a path for the destination local is guaranteed
    assign(it, it->getOutput().value()) = defaultValue;
    // dim == 0 -> return first value
    assign(it, NOP_REGISTER) = (arg ^ 0_val, SetFlag::SET_FLAGS);
    it.emplace(new MoveOperation(
        it->getOutput().value(), method.findOrCreateBuiltin(locals.at(0))->createReference(), COND_ZERO_SET));
    it->addDecorations(add_flag(decoration, InstructionDecorations::ELEMENT_INSERTION));
    it.nextInBlock();
    // dim == 1 -> return second value
    assign(it, NOP_REGISTER) = (arg ^ 1_val, SetFlag::SET_FLAGS);
    it.emplace(new MoveOperation(
        it->getOutput().value(), method.findOrCreateBuiltin(locals.at(1))->createReference(), COND_ZERO_SET));
    it->addDecorations(add_flag(decoration, InstructionDecorations::ELEMENT_INSERTION));
    it.nextInBlock();
    // dim == 2 -> return third value
    assign(it, NOP_REGISTER) = (arg ^ 2_val, SetFlag::SET_FLAGS);
    it.reset((new MoveOperation(
        it->getOutput().value(), method.findOrCreateBuiltin(locals.at(2))->createReference(), COND_ZERO_SET)));
    it->addDecorations(add_flag(decoration, InstructionDecorations::ELEMENT_INSERTION));
    return it;
}

static NODISCARD InstructionWalker intrinsifyReadWorkItemInfo(Method& method, InstructionWalker it, const Value& arg,
    BuiltinLocal::Type local, const InstructionDecorations decoration)
{
    /*
     * work-item infos (id, size) are stored within a single UNIFORM:
     * high <-> low byte
     * 00 | 3.dim | 2.dim | 1.dim
     * -> res = (UNIFORM >> (dim * 8)) & 0xFF
     */
    const Local* itemInfo = method.findOrCreateBuiltin(local);
    if(auto literalDim = (arg.getConstantValue() & &Value::getLiteralValue))
    {
        // NOTE: This forces the local_ids/local_sizes values to be on register-file A, but safes an instruction per
        // read
        switch(literalDim->unsignedInt())
        {
        case 0:
            return it.reset((new MoveOperation(it->getOutput().value(), itemInfo->createReference()))
                                ->setUnpackMode(UNPACK_8A_32)
                                ->copyExtrasFrom(it.get())
                                ->addDecorations(decoration));
        case 1:
            return it.reset((new MoveOperation(it->getOutput().value(), itemInfo->createReference()))
                                ->setUnpackMode(UNPACK_8B_32)
                                ->copyExtrasFrom(it.get())
                                ->addDecorations(decoration));
        case 2:
            return it.reset((new MoveOperation(it->getOutput().value(), itemInfo->createReference()))
                                ->setUnpackMode(UNPACK_8C_32)
                                ->copyExtrasFrom(it.get())
                                ->addDecorations(decoration));
        case 3:
            return it.reset((new MoveOperation(it->getOutput().value(), itemInfo->createReference()))
                                ->setUnpackMode(UNPACK_8D_32)
                                ->copyExtrasFrom(it.get())
                                ->addDecorations(decoration));
        default:
            return it.reset((new MoveOperation(it->getOutput().value(), INT_ZERO))
                                ->copyExtrasFrom(it.get())
                                ->addDecorations(decoration));
        }
    }
    Value tmp0 = assign(it, TYPE_INT8) = mul24(arg, 8_val);
    Value tmp1 = assign(it, TYPE_INT8) = as_unsigned{itemInfo->createReference()} >> tmp0;
    return it.reset(
        (new Operation(OP_AND, it->getOutput().value(), tmp1, Value(Literal(static_cast<uint32_t>(0xFF)), TYPE_INT8)))
            ->copyExtrasFrom(it.get())
            ->addDecorations(decoration));
}

static NODISCARD InstructionWalker intrinsifyReadLocalSize(Method& method, InstructionWalker it, const Value& arg)
{
    auto decorations =
        add_flag(add_flag(InstructionDecorations::BUILTIN_LOCAL_SIZE, InstructionDecorations::UNSIGNED_RESULT),
            InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
    /*
     * Use the value set via reqd_work_group_size(x, y, z) - if set - and return here.
     * This is valid, since the OpenCL standard states: "is the work-group size that must be used as the local_work_size
     * argument to clEnqueueNDRangeKernel." (page 231)
     */

    if(method.metaData.isWorkGroupSizeSet())
    {
        const auto& workGroupSizes = method.metaData.workGroupSizes;
        Optional<Literal> immediate;
        if(arg.isLiteralValue())
            // the dimension is a literal value -> look this dimension up
            immediate = arg.getLiteralValue();
        else if(std::all_of(workGroupSizes.begin(), workGroupSizes.end(), [](uint32_t u) -> bool { return u == 1; }))
            // all dimensions are 1 (for any set or not explicitly set dimension) -> take any of them
            immediate = Literal(0u);
        if(immediate)
        {
            if(immediate->unsignedInt() > workGroupSizes.size() || workGroupSizes.at(immediate->unsignedInt()) == 0)
            {
                return it.reset((new MoveOperation(it->getOutput().value(), INT_ONE))->addDecorations(decorations));
            }
            return it.reset((new MoveOperation(it->getOutput().value(),
                                 Value(Literal(workGroupSizes.at(immediate->unsignedInt())), TYPE_INT8)))
                                ->addDecorations(decorations));
        }
    }
    // TODO needs to have a size of 1 for all higher dimensions (instead of currently implicit 0)
    return intrinsifyReadWorkItemInfo(method, it, arg, BuiltinLocal::Type::LOCAL_SIZES, decorations);
}

static NODISCARD InstructionWalker intrinsifyReadLocalID(Method& method, InstructionWalker it, const Value& arg)
{
    if(method.metaData.isWorkGroupSizeSet() &&
        std::all_of(method.metaData.workGroupSizes.begin(), method.metaData.workGroupSizes.end(),
            [](uint32_t u) -> bool { return u == 1; }))
    {
        // if all the work-group sizes are 1, the ID is always 0 for all dimensions
        return it.reset((new MoveOperation(it->getOutput().value(), INT_ZERO))
                            ->addDecorations(add_flag(
                                InstructionDecorations::BUILTIN_LOCAL_ID, InstructionDecorations::UNSIGNED_RESULT)));
    }
    return intrinsifyReadWorkItemInfo(method, it, arg, BuiltinLocal::Type::LOCAL_IDS,
        add_flag(InstructionDecorations::BUILTIN_LOCAL_ID, InstructionDecorations::UNSIGNED_RESULT));
}

static bool intrinsifyWorkItemFunctions(Method& method, InstructionWalker it)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite == nullptr)
        return false;
    if(callSite->getArguments().size() > 1)
        return false;

    if(callSite->methodName == "vc4cl_work_dimensions" && callSite->getArguments().empty())
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of work-item dimensions" << logging::endl);
        // setting the type to int8 allows us to optimize e.g. multiplications with work-item values
        Value out = callSite->getOutput().value();
        out.type = TYPE_INT8;
        it.reset(
            (new MoveOperation(out, method.findOrCreateBuiltin(BuiltinLocal::Type::WORK_DIMENSIONS)->createReference()))
                ->copyExtrasFrom(callSite)
                ->addDecorations(add_flag(callSite->decoration,
                    add_flag(add_flag(InstructionDecorations::BUILTIN_WORK_DIMENSIONS,
                                 InstructionDecorations::UNSIGNED_RESULT),
                        InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))));
        return true;
    }
    if(callSite->methodName == "vc4cl_num_groups" && callSite->getArguments().size() == 1)
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Intrinsifying reading of the number of work-groups" << logging::endl);
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::NUM_GROUPS_X, BuiltinLocal::Type::NUM_GROUPS_Y, BuiltinLocal::Type::NUM_GROUPS_Z},
            INT_ONE,
            add_flag(add_flag(InstructionDecorations::BUILTIN_NUM_GROUPS, InstructionDecorations::UNSIGNED_RESULT),
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        return true;
    }
    if(callSite->methodName == "vc4cl_group_id" && callSite->getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of the work-group ids" << logging::endl);
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::GROUP_ID_X, BuiltinLocal::Type::GROUP_ID_Y, BuiltinLocal::Type::GROUP_ID_Z}, INT_ZERO,
            add_flag(add_flag(InstructionDecorations::BUILTIN_GROUP_ID, InstructionDecorations::UNSIGNED_RESULT),
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        return true;
    }
    if(callSite->methodName == "vc4cl_global_offset" && callSite->getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of the global offsets" << logging::endl);
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::GLOBAL_OFFSET_X, BuiltinLocal::Type::GLOBAL_OFFSET_Y,
                BuiltinLocal::Type::GLOBAL_OFFSET_Z},
            INT_ZERO,
            add_flag(add_flag(InstructionDecorations::BUILTIN_GLOBAL_OFFSET, InstructionDecorations::UNSIGNED_RESULT),
                InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
        return true;
    }
    if(callSite->methodName == "vc4cl_local_size" && callSite->getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of local work-item sizes" << logging::endl);
        it = intrinsifyReadLocalSize(method, it, callSite->assertArgument(0));
        return true;
    }
    if(callSite->methodName == "vc4cl_local_id" && callSite->getArguments().size() == 1)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of local work-item ids" << logging::endl);
        it = intrinsifyReadLocalID(method, it, callSite->assertArgument(0));
        return true;
    }
    if(callSite->methodName == "vc4cl_global_size" && callSite->getArguments().size() == 1)
    {
        // global_size(dim) = local_size(dim) * num_groups(dim)
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of global work-item sizes" << logging::endl);

        const Value tmpLocalSize = method.addNewLocal(TYPE_INT8, "%local_size");
        const Value tmpNumGroups = method.addNewLocal(TYPE_INT32, "%num_groups");
        // emplace dummy instructions to be replaced
        it.emplace(new MoveOperation(tmpLocalSize, NOP_REGISTER));
        it = intrinsifyReadLocalSize(method, it, callSite->assertArgument(0));
        it.nextInBlock();
        it.emplace(new MoveOperation(tmpNumGroups, NOP_REGISTER));
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::NUM_GROUPS_X, BuiltinLocal::Type::NUM_GROUPS_Y, BuiltinLocal::Type::NUM_GROUPS_Z},
            INT_ONE, add_flag(InstructionDecorations::BUILTIN_NUM_GROUPS, InstructionDecorations::UNSIGNED_RESULT));
        it.nextInBlock();
        it.reset((new Operation(OP_MUL24, callSite->getOutput().value(), tmpLocalSize, tmpNumGroups))
                     ->copyExtrasFrom(callSite)
                     ->addDecorations(add_flag(callSite->decoration,
                         add_flag(add_flag(InstructionDecorations::BUILTIN_GLOBAL_SIZE,
                                      InstructionDecorations::UNSIGNED_RESULT),
                             InstructionDecorations::WORK_GROUP_UNIFORM_VALUE))));
        return true;
    }
    if(callSite->methodName == "vc4cl_global_id" && callSite->getArguments().size() == 1)
    {
        // global_id(dim) = global_offset(dim) + (group_id(dim) * local_size(dim) + local_id(dim)
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying reading of global work-item ids" << logging::endl);

        const Value tmpGroupID = method.addNewLocal(TYPE_INT32, "%group_id");
        const Value tmpLocalSize = method.addNewLocal(TYPE_INT8, "%local_size");
        const Value tmpGlobalOffset = method.addNewLocal(TYPE_INT32, "%global_offset");
        const Value tmpLocalID = method.addNewLocal(TYPE_INT8, "%local_id");
        const Value tmpRes0 = method.addNewLocal(TYPE_INT32, "%group_global_id");
        const Value tmpRes1 = method.addNewLocal(TYPE_INT32, "%group_global_id");
        // emplace dummy instructions to be replaced
        it.emplace(new MoveOperation(tmpGroupID, NOP_REGISTER));
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::GROUP_ID_X, BuiltinLocal::Type::GROUP_ID_Y, BuiltinLocal::Type::GROUP_ID_Z}, INT_ZERO,
            add_flag(InstructionDecorations::BUILTIN_GROUP_ID, InstructionDecorations::UNSIGNED_RESULT));
        it.nextInBlock();
        it.emplace(new MoveOperation(tmpLocalSize, NOP_REGISTER));
        it = intrinsifyReadLocalSize(method, it, callSite->assertArgument(0));
        it.nextInBlock();
        it.emplace(new MoveOperation(tmpGlobalOffset, NOP_REGISTER));
        it = intrinsifyReadWorkGroupInfo(method, it, callSite->assertArgument(0),
            {BuiltinLocal::Type::GLOBAL_OFFSET_X, BuiltinLocal::Type::GLOBAL_OFFSET_Y,
                BuiltinLocal::Type::GLOBAL_OFFSET_Z},
            INT_ZERO, add_flag(InstructionDecorations::BUILTIN_GLOBAL_OFFSET, InstructionDecorations::UNSIGNED_RESULT));
        it.nextInBlock();
        it.emplace(new MoveOperation(tmpLocalID, NOP_REGISTER));
        it = intrinsifyReadLocalID(method, it, callSite->assertArgument(0));
        it.nextInBlock();
        assign(it, tmpRes0) = mul24(tmpGroupID, tmpLocalSize);
        assign(it, tmpRes1) = tmpGlobalOffset + tmpRes0;
        it.reset(
            (new Operation(OP_ADD, callSite->getOutput().value(), tmpRes1, tmpLocalID))
                ->copyExtrasFrom(callSite)
                ->addDecorations(add_flag(callSite->decoration,
                    add_flag(InstructionDecorations::BUILTIN_GLOBAL_ID, InstructionDecorations::UNSIGNED_RESULT))));
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
    if(intrinsifyWorkItemFunctions(method, it))
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
