/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "../periphery/VPM.h"
#include "Intrinsics.h"
#include "Comparisons.h"
#include "Operators.h"
#include "Images.h"
#include "log.h"
#include "../intermediate/Helper.h"
#include <map>
#include <cmath>
#include <stdbool.h>
#include <vector>

using namespace vc4c;
using namespace vc4c::intermediate;


//The function to apply for pre-calculation
using UnaryInstruction = std::function<Optional<Value>(const Value&)>;
static const UnaryInstruction NO_OP = [](const Value& val){return NO_VALUE;};
//The function to apply for pre-calculation
using BinaryInstruction = std::function<Optional<Value>(const Value&, const Value&)>;
static const BinaryInstruction NO_OP2 = [](const Value& val0, const Value& val1){return NO_VALUE;};

//see VC4CLStdLib (_intrinsics.h)
static constexpr unsigned char VC4CL_UNSIGNED {1};


using IntrinsicFunction = std::function<InstructionWalker(Method&, InstructionWalker, const MethodCall*)>;
//NOTE: copying the captures is on purpose, since the sources do not exist anymore!

static IntrinsicFunction intrinsifyUnaryALUInstruction(const OpCode& opCode, const bool useSignFlag = false)
{
	return [opCode, useSignFlag](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker
	{
		bool isUnsigned = callSite->getArgument(1).hasValue && callSite->getArgument(1).get().hasType(ValueType::LITERAL) && callSite->getArgument(1).get().literal.integer == VC4CL_UNSIGNED;

		logging::debug() << "Intrinsifying unary '" << callSite->to_string() << "' to operation " << opCode.name << logging::endl;
		it.reset((new Operation(opCode, callSite->getOutput(), callSite->getArgument(0)))->copyExtrasFrom(callSite));

		if(useSignFlag && isUnsigned)
			it->setDecorations(InstructionDecorations::UNSIGNED_RESULT);

		return it;
	};
}

static IntrinsicFunction intrinsifyBinaryALUInstruction(const std::string& opCode, const bool useSignFlag = false)
{
	return [opCode, useSignFlag](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker
	{
		bool isUnsigned = callSite->getArgument(2).hasValue && callSite->getArgument(2).get().hasType(ValueType::LITERAL) && callSite->getArgument(2).get().literal.integer == VC4CL_UNSIGNED;

		logging::debug() << "Intrinsifying binary '" << callSite->to_string() << "' to operation " << opCode << logging::endl;
		it.reset((new Operation(opCode, callSite->getOutput(), callSite->getArgument(0), callSite->getArgument(1)))->copyExtrasFrom(callSite));

		if(useSignFlag && isUnsigned)
			it->setDecorations(InstructionDecorations::UNSIGNED_RESULT);

		return it;
	};
}

static IntrinsicFunction intrinsifySFUInstruction(const Register& sfuRegister)
{
	return [sfuRegister](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker
	{
		logging::debug() << "Intrinsifying unary '" << callSite->to_string() << "' to SFU call" << logging::endl;
		it = insertSFUCall(sfuRegister, it, callSite->getArgument(0), callSite->conditional);
		it.reset((new MoveOperation(callSite->getOutput(), Value(REG_SFU_OUT, callSite->getOutput().get().type)))->copyExtrasFrom(callSite));
		return it;
	};
}

static IntrinsicFunction intrinsifyValueRead(const Value& val)
{
	return [val](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker
	{
		logging::debug() << "Intrinsifying method-call '" << callSite->to_string() << "' to value read" << logging::endl;
		it.reset((new MoveOperation(callSite->getOutput(), val))->copyExtrasFrom(callSite));
		return it;
	};
}

static IntrinsicFunction intrinsifySemaphoreAccess(bool increment)
{
	return [increment](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker
	{
		if(!callSite->getArgument(0).get().hasType(ValueType::LITERAL))
			throw CompilationError(CompilationStep::OPTIMIZER, "Semaphore-number needs to be a compile-time constant", callSite->to_string());
		if(callSite->getArgument(0).get().literal.integer < 0 || callSite->getArgument(0).get().literal.integer >= 16)
			throw CompilationError(CompilationStep::OPTIMIZER, "Semaphore-number needs to be between 0 and 15", callSite->to_string());

		if(increment)
		{
			logging::debug() << "Intrinsifying semaphore increment with instruction" << logging::endl;
			it.reset((new SemaphoreAdjustment(static_cast<Semaphore>(callSite->getArgument(0).get().literal.integer), true))->copyExtrasFrom(callSite));
		}
		else
		{
			logging::debug() << "Intrinsifying semaphore decrement with instruction" << logging::endl;
			it.reset((new SemaphoreAdjustment(static_cast<Semaphore>(callSite->getArgument(0).get().literal.integer), false))->copyExtrasFrom(callSite));
		}
		return it;
	};
}

static IntrinsicFunction intrinsifyMutexAccess(bool lock)
{
	return [lock](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker
	{
		if(lock)
		{
			logging::debug() << "Intrinsifying mutex lock with instruction" << logging::endl;
			it.reset(new MoveOperation(NOP_REGISTER, MUTEX_REGISTER));
		}
		else
		{
			logging::debug() << "Intrinsifying mutex unlock with instruction" << logging::endl;
			it.reset(new MoveOperation(MUTEX_REGISTER, BOOL_TRUE));
		}
		return it;
	};
}

enum class DMAAccess
{
	READ,
	WRITE,
	COPY
};

static IntrinsicFunction intrinsifyDMAAccess(DMAAccess access)
{
	return [access](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker
	{
		switch(access)
		{
			case DMAAccess::READ:
			{
				logging::debug() << "Intrinsifying memory read " << callSite->to_string() << logging::endl;
				it = periphery::insertReadDMA(method, it, callSite->getOutput(), callSite->getArgument(0), false);
				break;
			}
			case DMAAccess::WRITE:
			{
				logging::debug() << "Intrinsifying memory write " << callSite->to_string() << logging::endl;
				it = periphery::insertWriteDMA(method, it, callSite->getArgument(1), callSite->getArgument(0), false);
				break;
			}
			case DMAAccess::COPY:
			{
				logging::debug() << "Intrinsifying ternary '" << callSite->to_string() << "' to DMA copy operation " << logging::endl;
				const DataType type = callSite->getArgument(0).get().type.getElementType();
				//TODO number of elements!
				it = method.vpm->insertReadRAM(it, callSite->getArgument(1), type, nullptr, false);
				it = method.vpm->insertWriteRAM(it, callSite->getArgument(0), type, nullptr, false);
				break;
			}
		}

		it.erase();
		//so next instruction is not skipped
		it.previousInBlock();

		return it;
	};
}

static IntrinsicFunction intrinsifyVectorRotation()
{
	return [](Method& method, InstructionWalker it, const MethodCall* callSite) -> InstructionWalker
	{
		logging::debug() << "Intrinsifying vector rotation " << callSite->to_string() << logging::endl;
		it = insertVectorRotation(it, callSite->getArgument(0), callSite->getArgument(1), callSite->getOutput(), Direction::UP);
		it.erase();
		//so next instruction is not skipped
		it.previousInBlock();

		return it;
	};
}

struct Intrinsic
{
	const IntrinsicFunction func;
    const Optional<UnaryInstruction> unaryInstr;
    const Optional<BinaryInstruction> binaryInstr;
    
    Intrinsic(const IntrinsicFunction& func): func(func) { }
    
    Intrinsic(const IntrinsicFunction& func, const UnaryInstruction unary) : func(func), unaryInstr(unary) { }
    
    Intrinsic(const IntrinsicFunction& func, const BinaryInstruction binary) : func(func), binaryInstr(binary) { }
};

const static std::map<std::string, Intrinsic> nonaryInstrinsics = {
    {"vc4cl_mutex_lock", Intrinsic{intrinsifyMutexAccess(true)}},
    {"vc4cl_mutex_unlock", Intrinsic{intrinsifyMutexAccess(false)}},
	{"vc4cl_element_number", Intrinsic{intrinsifyValueRead(ELEMENT_NUMBER_REGISTER)}},
	{"vc4cl_qpu_number", Intrinsic{intrinsifyValueRead(Value(REG_QPU_NUMBER, TYPE_INT8))}}
};

const static std::map<std::string, Intrinsic> unaryIntrinsicMapping = {
    {"vc4cl_ftoi", Intrinsic{intrinsifyUnaryALUInstruction(OP_FTOI), [](const Value& val){return Value(Literal(static_cast<int64_t>(std::round(val.literal.real()))), TYPE_INT32);}}},
    {"vc4cl_itof", Intrinsic{intrinsifyUnaryALUInstruction(OP_ITOF), [](const Value& val){return Value(Literal(static_cast<double>(val.literal.integer)), TYPE_FLOAT);}}},
    {"vc4cl_clz", Intrinsic{intrinsifyUnaryALUInstruction(OP_CLZ), NO_OP}},
    {"vc4cl_sfu_rsqrt", Intrinsic{intrinsifySFUInstruction(REG_SFU_RECIP_SQRT), [](const Value& val){return Value(Literal(1.0 / std::sqrt(val.literal.real())), TYPE_FLOAT);}}},
    {"vc4cl_sfu_exp2", Intrinsic{intrinsifySFUInstruction(REG_SFU_EXP2), [](const Value& val){return Value(Literal(std::exp2(val.literal.real())), TYPE_FLOAT);}}},
    {"vc4cl_sfu_log2", Intrinsic{intrinsifySFUInstruction(REG_SFU_LOG2), [](const Value& val){return Value(Literal(std::log2(val.literal.real())), TYPE_FLOAT);}}},
    {"vc4cl_sfu_recip", Intrinsic{intrinsifySFUInstruction(REG_SFU_RECIP), [](const Value& val){return Value(Literal(1.0 / val.literal.real()), TYPE_FLOAT);}}},
    {"vc4cl_semaphore_increment", Intrinsic{intrinsifySemaphoreAccess(true)}},
    {"vc4cl_semaphore_decrement", Intrinsic{intrinsifySemaphoreAccess(false)}},
    {"vc4cl_dma_read", Intrinsic{intrinsifyDMAAccess(DMAAccess::READ)}}
};

const static std::map<std::string, Intrinsic> binaryIntrinsicMapping = {
    {"vc4cl_fmax", Intrinsic{intrinsifyBinaryALUInstruction("fmax"), [](const Value& val0, const Value& val1){return Value(Literal(std::max(val0.literal.real(), val1.literal.real())), TYPE_FLOAT);}}},
    {"vc4cl_fmin", Intrinsic{intrinsifyBinaryALUInstruction("fmin"), [](const Value& val0, const Value& val1){return Value(Literal(std::min(val0.literal.real(), val1.literal.real())), TYPE_FLOAT);}}},
    {"vc4cl_fmaxabs", Intrinsic{intrinsifyBinaryALUInstruction("fmaxabs"), [](const Value& val0, const Value& val1){return Value(Literal(std::max(std::abs(val0.literal.real()), std::abs(val1.literal.real()))), TYPE_FLOAT);}}},
    {"vc4cl_fminabs", Intrinsic{intrinsifyBinaryALUInstruction("fminabs"), [](const Value& val0, const Value& val1){return Value(Literal(std::min(std::abs(val0.literal.real()), std::abs(val1.literal.real()))), TYPE_FLOAT);}}},
	//FIXME sign / no-sign!!
    {"vc4cl_shr", Intrinsic{intrinsifyBinaryALUInstruction("shr"), [](const Value& val0, const Value& val1){return Value(Literal(val0.literal.integer >> val1.literal.integer), val0.type.getUnionType(val1.type));}}},
    {"vc4cl_asr", Intrinsic{intrinsifyBinaryALUInstruction("asr"), [](const Value& val0, const Value& val1){return Value(Literal(val0.literal.integer >> val1.literal.integer), val0.type.getUnionType(val1.type));}}},
    {"vc4cl_ror", Intrinsic{intrinsifyBinaryALUInstruction("ror"), NO_OP2}},
    {"vc4cl_shl", Intrinsic{intrinsifyBinaryALUInstruction("shl"), [](const Value& val0, const Value& val1){return Value(Literal(val0.literal.integer << val1.literal.integer), val0.type.getUnionType(val1.type));}}},
    {"vc4cl_min", Intrinsic{intrinsifyBinaryALUInstruction("min", true), [](const Value& val0, const Value& val1){return Value(Literal(std::min(val0.literal.integer, val1.literal.integer)), val0.type.getUnionType(val1.type));}}},
    {"vc4cl_max", Intrinsic{intrinsifyBinaryALUInstruction("max", true), [](const Value& val0, const Value& val1){return Value(Literal(std::max(val0.literal.integer, val1.literal.integer)), val0.type.getUnionType(val1.type));}}},
    {"vc4cl_and", Intrinsic{intrinsifyBinaryALUInstruction("and"), [](const Value& val0, const Value& val1){return Value(Literal(val0.literal.integer & val1.literal.integer), val0.type.getUnionType(val1.type));}}},
    {"vc4cl_mul24", Intrinsic{intrinsifyBinaryALUInstruction("mul24", true), [](const Value& val0, const Value& val1){return Value(Literal((val0.literal.integer & 0xFFFFFFL) * (val1.literal.integer & 0xFFFFFFL)), val0.type.getUnionType(val1.type));}}},
    {"vc4cl_dma_write", Intrinsic{intrinsifyDMAAccess(DMAAccess::WRITE)}},
	{"vc4cl_vector_rotate", Intrinsic{intrinsifyVectorRotation()}}
};

const static std::map<std::string, Intrinsic> ternaryIntrinsicMapping = {
	{"vc4cl_dma_copy", Intrinsic{intrinsifyDMAAccess(DMAAccess::COPY)}}
};

const static std::map<std::string, std::pair<Intrinsic, Optional<Value>>> typeCastIntrinsics = {
	//since we run all the (not intrinsified) calculations with 32-bit, don't truncate signed conversions to smaller types
	//TODO correct?? Since we do not discard out-of-bounds values!
    {"vc4cl_bitcast_uchar", {Intrinsic{intrinsifyBinaryALUInstruction("and", true), [](const Value& val){return Value(Literal(val.literal.integer & 0xFF), TYPE_INT8);}}, Value(Literal(static_cast<uint64_t>(0xFF)), TYPE_INT8)}},
    {"vc4cl_bitcast_char", {Intrinsic{intrinsifyBinaryALUInstruction("mov"), [](const Value& val){return Value(val.literal, TYPE_INT8);}}, NO_VALUE}},
    {"vc4cl_bitcast_ushort", {Intrinsic{intrinsifyBinaryALUInstruction("and", true), [](const Value& val){return Value(Literal(val.literal.integer & 0xFFFF), TYPE_INT16);}}, Value(Literal(static_cast<uint64_t>(0xFFFF)), TYPE_INT16)}},
    {"vc4cl_bitcast_short", {Intrinsic{intrinsifyBinaryALUInstruction("mov"), [](const Value& val){return Value(val.literal, TYPE_INT16);}}, NO_VALUE}},
    {"vc4cl_bitcast_uint", {Intrinsic{intrinsifyBinaryALUInstruction("mov", true), [](const Value& val){return Value(Literal(val.literal.integer & static_cast<int64_t>(0xFFFFFFFF)), TYPE_INT32);}}, NO_VALUE}},
    {"vc4cl_bitcast_int", {Intrinsic{intrinsifyBinaryALUInstruction("mov"), [](const Value& val){return Value(val.literal, TYPE_INT32);}}, NO_VALUE}},
    {"vc4cl_bitcast_float", {Intrinsic{intrinsifyBinaryALUInstruction("mov"), [](const Value& val){return Value(Literal(val.literal.integer & static_cast<int64_t>(0xFFFFFFFF)), TYPE_INT32);}}, NO_VALUE}}
};

static InstructionWalker intrinsifyNoArgs(Method& method, InstructionWalker it)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite == nullptr)
    {
        return it;
    }
    if(callSite->getArguments().size() > 1 /* check for sign-flag too*/)
    {
        return it;
    }
    for(const auto& pair : nonaryInstrinsics)
    {
        if(callSite->methodName.find(pair.first) != std::string::npos)
        {
        	return pair.second.func(method, it, callSite);
        }
    }
    return it;
}

static InstructionWalker intrinsifyUnary(Method& method, InstructionWalker it)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite == nullptr)
    {
        return it;
    }
    if(callSite->getArguments().size() == 0 || callSite->getArguments().size() > 2 /* check for sign-flag too*/)
    {
        return it;
    }
    for(const auto& pair : unaryIntrinsicMapping)
    {
        if(callSite->methodName.find(pair.first) != std::string::npos)
        {
        	if(callSite->getArgument(0).get().hasType(ValueType::LITERAL) && pair.second.unaryInstr && pair.second.unaryInstr.get()(callSite->getArgument(0)).hasValue)
        	{
        		logging::debug() << "Intrinsifying unary '" << callSite->to_string() << "' to pre-calculated value" << logging::endl;
        		it.reset(new MoveOperation(callSite->getOutput(), pair.second.unaryInstr.get()(callSite->getArgument(0)), callSite->conditional, callSite->setFlags));
        	}
        	else
        	{
        		return pair.second.func(method, it, callSite);
        	}
            return it;
        }
    }
    for(const auto& pair : typeCastIntrinsics)
    {
        if(callSite->methodName.find(pair.first) != std::string::npos)
        {
        	if(callSite->getArgument(0).get().hasType(ValueType::LITERAL) && pair.second.first.unaryInstr && pair.second.first.unaryInstr.get()(callSite->getArgument(0)).hasValue)
			{
				logging::debug() << "Intrinsifying type-cast '" << callSite->to_string() << "' to pre-calculated value" << logging::endl;
				it.reset(new MoveOperation(callSite->getOutput(), pair.second.first.unaryInstr.get()(callSite->getArgument(0)), callSite->conditional, callSite->setFlags));
			}
        	else if(!pair.second.second.hasValue)	//there is no value to apply -> simple move
        	{
        		logging::debug() << "Intrinsifying '" << callSite->to_string() << "' to simple move" << logging::endl;
				it.reset(new MoveOperation(callSite->getOutput(), callSite->getArgument(0)));
        	}
        	else
            {
        		//TODO could use pack-mode here, but only for UNSIGNED values!!
				logging::debug() << "Intrinsifying '" << callSite->to_string() << "' to operation with constant " << pair.second.second.to_string() << logging::endl;
				callSite->setArgument(1, pair.second.second);
				return pair.second.first.func(method, it, callSite);
            }
            return it;
        }
    }
    return it;
}

static InstructionWalker intrinsifyBinary(Method& method, InstructionWalker it)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite == nullptr)
    {
        return it;
    }
    if(callSite->getArguments().size() < 2 || callSite->getArguments().size() > 3 /* check for sign-flag too*/)
    {
        return it;
    }
    for(const auto& pair : binaryIntrinsicMapping)
    {
        if(callSite->methodName.find(pair.first) != std::string::npos)
        {
        	if(callSite->getArgument(0).get().hasType(ValueType::LITERAL) && callSite->getArgument(1).get().hasType(ValueType::LITERAL) && pair.second.binaryInstr && pair.second.binaryInstr.get()(callSite->getArgument(0), callSite->getArgument(1)).hasValue)
			{
				logging::debug() << "Intrinsifying binary '" << callSite->to_string() << "' to pre-calculated value" << logging::endl;
				it.reset(new MoveOperation(callSite->getOutput(), pair.second.binaryInstr.get()(callSite->getArgument(0), callSite->getArgument(1)), callSite->conditional, callSite->setFlags));
			}
        	else
        	{
        		return pair.second.func(method, it, callSite);
        	}
            return it;
        }
    }
    return it;
}

static InstructionWalker intrinsifyTernary(Method& method, InstructionWalker it)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite == nullptr)
    {
        return it;
    }
    if(callSite->getArguments().size() < 3 || callSite->getArguments().size() > 4 /* check for sign-flag too*/)
    {
        return it;
    }
    for(const auto& pair : ternaryIntrinsicMapping)
    {
        if(callSite->methodName.find(pair.first) != std::string::npos)
        {
        	return pair.second.func(method, it, callSite);
        }
    }
    return it;
}

static void swapComparisons(const std::string& opCode, Comparison* comp)
{
    Value tmp = comp->getFirstArg();
    comp->setArgument(0, comp->getSecondArg());
    comp->setArgument(1, tmp);
    comp->setOpCode(OP_NOP);
    const_cast<std::string&>(comp->opCode) = opCode;
}

static InstructionWalker intrinsifyComparison(Method& method, InstructionWalker it)
{
    Comparison* comp = it.get<Comparison>();
    if(comp == nullptr)
    {
        return it;
    }
    logging::debug() << "Intrinsifying comparison '" << comp->opCode << "' to arithmetic operations" << logging::endl;
    bool isFloating = comp->getFirstArg().type.isFloatingType();
    if(!isFloating)
    {
        //simplification, make a R b -> b R' a
        if(COMP_UNSIGNED_GE.compare(comp->opCode) == 0)
        {
            swapComparisons(COMP_UNSIGNED_LE, comp);
        }
        else if(COMP_UNSIGNED_GT.compare(comp->opCode) == 0)
        {
            swapComparisons(COMP_UNSIGNED_LT, comp);
        }
        else if(COMP_SIGNED_GE.compare(comp->opCode) == 0)
        {
            swapComparisons(COMP_SIGNED_LE, comp);
        }
        else if(COMP_SIGNED_GT.compare(comp->opCode) == 0)
        {
            swapComparisons(COMP_SIGNED_LT, comp);
        }
            
        it = intrinsifyIntegerRelation(method, it, comp);
    }
    else
    {
        //simplification, make a R b -> b R' a
        if(COMP_ORDERED_GE.compare(comp->opCode) == 0)
        {
            swapComparisons(COMP_ORDERED_LE, comp);
        }
        else if(COMP_ORDERED_GT.compare(comp->opCode) == 0)
        {
            swapComparisons(COMP_ORDERED_LT, comp);
        }
        else if(COMP_UNORDERED_GE.compare(comp->opCode) == 0)
        {
            swapComparisons(COMP_UNORDERED_LE, comp);
        }
        else if(COMP_UNORDERED_GT.compare(comp->opCode) == 0)
        {
            swapComparisons(COMP_UNORDERED_LT, comp);
        }
        
        it = intrinsifyFloatingRelation(method, it, comp);
    }
    
    return it;
}

static bool isPowerTwo(int64_t val)
{
    //https://en.wikipedia.org/wiki/Power_of_two#Fast_algorithm_to_check_if_a_positive_number_is_a_power_of_two
    return val > 0 && (val & (val - 1)) == 0;
}

static InstructionWalker intrinsifyArithmetic(Method& method, InstructionWalker it, const MathType& mathType)
{
    Operation* op = it.get<Operation>();
    if(op == nullptr)
    {
        return it;
    }
    const Value& arg0 = op->getFirstArg();
    const Value& arg1 = op->getSecondArg().orElse(UNDEFINED_VALUE);
    const bool saturateResult = has_flag(op->decoration, InstructionDecorations::SATURATED_CONVERSION);
    //integer multiplication
    if(op->opCode.compare("mul") == 0)
    {
        //a * b = b * a
        //a * 2^n = a << n
        if(arg0.hasType(ValueType::LITERAL) && arg1.hasType(ValueType::LITERAL))
        {
            logging::debug() << "Calculating result for multiplication with constants" << logging::endl;
            it.reset(new MoveOperation(Value(op->getOutput().get().local, arg0.type), Value(Literal(arg0.literal.integer * arg1.literal.integer), arg0.type), op->conditional, op->setFlags));
        }
        else if(arg0.hasType(ValueType::LITERAL) && isPowerTwo(arg0.literal.integer))
        {
            logging::debug() << "Intrinsifying multiplication with left-shift" << logging::endl;
            op->setOpCode(OP_SHL);
            op->setArgument(0, arg1);
            op->setArgument(1, Value(Literal(static_cast<int64_t>(std::log2(arg0.literal.integer))), arg0.type));
        }
        else if(arg1.hasType(ValueType::LITERAL) && isPowerTwo(arg1.literal.integer))
        {
            logging::debug() << "Intrinsifying multiplication with left-shift" << logging::endl;
            op->setOpCode(OP_SHL);
            op->setArgument(1, Value(Literal(static_cast<int64_t>(std::log2(arg1.literal.integer))), arg1.type));
        }
        else if(std::max(arg0.type.getScalarBitCount(), arg1.type.getScalarBitCount()) <= 24)
        {
            logging::debug() << "Intrinsifying multiplication of small integers to mul24" << logging::endl;
            op->setOpCode(OP_MUL24);
        }
        else
        {
            it = intrinsifySignedIntegerMultiplication(method, it, *op);
        }
    }
    //unsigned division
    else if(op->opCode.compare("udiv") == 0)
    {
        if(arg0.hasType(ValueType::LITERAL) && arg1.hasType(ValueType::LITERAL))
        {
            logging::debug() << "Calculating result for division with constants" << logging::endl;
            it.reset(new MoveOperation(Value(op->getOutput().get().local, arg0.type), Value(Literal(static_cast<uint64_t>(arg0.literal.integer / arg1.literal.integer)), arg0.type), op->conditional, op->setFlags));
        }
        //a / 2^n = a >> n
        else if(arg1.hasType(ValueType::LITERAL) && isPowerTwo(arg1.literal.integer))
        {
            logging::debug() << "Intrinsifying division with right-shift" << logging::endl;
            op->setOpCode(OP_SHR);
            op->setArgument(1, Value(Literal(static_cast<int64_t>(std::log2(arg1.literal.integer))), arg1.type));
        }
        //TODO for constant numerators, we could check if a 8 or 16-bit division is enough. Extra handling required for d > n?
		//TODO for constant denominator, replace with multiplication and shift (only if multiplication does not overflow!)
		//see: http://forums.parallax.com/discussion/114807/fast-faster-fastest-code-integer-division
		//and: http://www.hackersdelight.org/hdcodetxt/magic.c.txt
        else
        {
            it = intrinsifyUnsignedIntegerDivision(method, it, *op);
        }
    }
    //signed division
    else if(op->opCode.compare("sdiv") == 0)
    {
        if(arg0.hasType(ValueType::LITERAL) && arg1.hasType(ValueType::LITERAL))
        {
            logging::debug() << "Calculating result for signed division with constants" << logging::endl;
            it.reset(new MoveOperation(Value(op->getOutput().get().local, arg0.type), Value(Literal(arg0.literal.integer / arg1.literal.integer), arg0.type), op->conditional, op->setFlags));
        }
        //a / 2^n = a >> n
        else if(arg1.hasType(ValueType::LITERAL) && isPowerTwo(arg1.literal.integer))
        {
            logging::debug() << "Intrinsifying signed division with arithmetic right-shift" << logging::endl;
            op->setOpCode(OP_ASR);
            op->setArgument(1, Value(Literal(static_cast<int64_t>(std::log2(arg1.literal.integer))), arg1.type));
        }
        else
        {
            it = intrinsifySignedIntegerDivision(method, it, *op);
        }
    }
    //unsigned modulo
    //LLVM IR calls it urem, SPIR-V umod
    else if(op->opCode.compare("urem") == 0 || op->opCode.compare("umod") == 0)
    {
        if(arg0.hasType(ValueType::LITERAL) && arg1.hasType(ValueType::LITERAL))
        {
            logging::debug() << "Calculating result for modulo with constants" << logging::endl;
            it.reset(new MoveOperation(Value(op->getOutput().get().local, arg0.type), Value(Literal(static_cast<uint64_t>(arg0.literal.integer % arg1.literal.integer)), arg0.type), op->conditional, op->setFlags));
        }
        else if(arg1.hasType(ValueType::LITERAL) && isPowerTwo(arg1.literal.integer))
        {
            logging::debug() << "Intrinsifying unsigned modulo by power of two" << logging::endl;
            op->setOpCode(OP_AND);
            op->setArgument(1, Value(Literal(arg1.literal.integer - 1), arg1.type));
        }
        else
        {
            it = intrinsifyUnsignedIntegerDivision(method, it, *op, true);
        }
    }
    //signed modulo
    else if(op->opCode.compare("srem") == 0)
    {
        if(arg0.hasType(ValueType::LITERAL) && arg1.hasType(ValueType::LITERAL))
        {
            logging::debug() << "Calculating result for signed modulo with constants" << logging::endl;
            it.reset(new MoveOperation(Value(op->getOutput().get().local, arg0.type), Value(Literal(arg0.literal.integer % arg1.literal.integer), arg0.type), op->conditional, op->setFlags));
        }
        else
        {
            it = intrinsifySignedIntegerDivision(method, it, *op, true);
        }
    }
    //floating division
    else if(op->opCode.compare("fdiv") == 0)
    {
        if(arg0.hasType(ValueType::LITERAL) && arg1.hasType(ValueType::LITERAL))
        {
            logging::debug() << "Calculating result for signed division with constants" << logging::endl;
            it.reset(new MoveOperation(Value(op->getOutput().get().local, arg0.type), Value(Literal(arg0.literal.real() / arg1.literal.real()), arg0.type), op->conditional, op->setFlags));
        }
        else if(arg1.hasType(ValueType::LITERAL))
        {
            logging::debug() << "Intrinsifying floating division with multiplication of constant inverse" << logging::endl;
            op->setOpCode(OP_FMUL);
            op->setArgument(1, Value(Literal(1.0 / arg1.literal.real()), arg1.type));
        }
        else if(has_flag(op->decoration, InstructionDecorations::ALLOW_RECIP) || has_flag(op->decoration, InstructionDecorations::FAST_MATH))
        {
            logging::debug() << "Intrinsifying floating division with multiplication of reciprocal" << logging::endl;
            it = insertSFUCall(REG_SFU_RECIP, it, arg1, op->conditional);
            it.nextInBlock();
            op->setOpCode(OP_FMUL);
            op->setArgument(1, Value(REG_SFU_OUT, op->getFirstArg().type));
        }
        else
        {
            logging::debug() << "Intrinsifying floating division with multiplication of inverse" << logging::endl;
            it = intrinsifyFloatingDivision(method, it, *op);
        }
    }
    //truncate bits
    else if(op->opCode.compare("trunc") == 0)
    {
    	if(saturateResult)
    	{
    		//let pack-mode handle saturation
    		logging::debug() << "Intrinsifying saturated truncate with move and pack-mode" << logging::endl;
    		it = insertSaturation(it, method, op->getFirstArg(), op->getOutput(), !has_flag(op->decoration, InstructionDecorations::UNSIGNED_RESULT));
    		it.nextInBlock();
    		it.erase();
    	}
        //if orig = i64, dest = i32 -> move
    	else if(op->getFirstArg().type.getScalarBitCount() > 32 && op->getOutput().get().type.getScalarBitCount() == 32)
        {
            //do nothing, is just a move, since we truncate the 64-bit integers anyway
            logging::debug() << "Intrinsifying truncate from unsupported type with move" << logging::endl;
            it.reset((new MoveOperation(op->getOutput(), op->getFirstArg(), op->conditional, op->setFlags))->copyExtrasFrom(op));
        }
        //if dest < i32 -> orig & dest-bits or pack-code
        else if(op->getOutput().get().type.getScalarBitCount() < 32)
        {
            logging::debug() << "Intrinsifying truncate with and" << logging::endl;
            op->setOpCode(OP_AND);
            op->setArgument(1, Value(Literal(op->getOutput().get().type.getScalarWidthMask()), TYPE_INT32));
        }
    }
    else if(op->opCode.compare("fptrunc") == 0)
    {
    	if(saturateResult)
    	{
    		//let pack-mode handle saturation
    		it = insertSaturation(it, method, op->getFirstArg(), op->getOutput(), !has_flag(op->decoration, InstructionDecorations::UNSIGNED_RESULT));
			it.nextInBlock();
			it.erase();
    	}
        //if orig = i64, dest = i32 -> move
    	else if(op->getFirstArg().type.getScalarBitCount() >= 32 && op->getOutput().get().type.getScalarBitCount() == 32)
        {
            logging::debug() << "Intrinsifying fptrunc with move" << logging::endl;
            it.reset((new MoveOperation(op->getOutput(), op->getFirstArg(), op->conditional, op->setFlags))->copyExtrasFrom(op));
        }
        else if(op->getFirstArg().type.getScalarBitCount() < 32)
            throw CompilationError(CompilationStep::OPTIMIZER, "Unsupported floating-point type", op->getFirstArg().type.to_string());
        else
            throw CompilationError(CompilationStep::OPTIMIZER, "Unsupported floating-point type", op->getOutput().get().type.to_string());
    }
    //arithmetic shift right
    else if(op->opCode.compare("ashr") == 0)
    {
        //just surgical modification
        op->setOpCode(OP_ASR);
    }
    else if(op->opCode.compare("lshr") == 0)
    {
        //TODO only if type <= i32 and/or offset <= 32
        //just surgical modification
        op->setOpCode(OP_SHR);
    }
    //integer to float
    else if(op->opCode.compare("sitofp") == 0)
    {
    	//for non 32-bit types, need to sign-extend
    	Value tmp = op->getFirstArg();
    	if(op->getFirstArg().type.getScalarBitCount() < 32)
    	{
    		tmp = method.addNewLocal(TYPE_INT32, "%sitofp");
    		it = insertSignExtension(it, method, op->getFirstArg(), tmp, op->conditional);
    		it.nextInBlock();
    	}
        //just surgical modification
        op->setOpCode(OP_ITOF);
        if(tmp != op->getFirstArg())
        	op->setArgument(0, tmp);
    }
    else if(op->opCode.compare("uitofp") == 0)
    {
    	const Value tmp = method.addNewLocal(op->getOutput().get().type, "%uitofp");
        if(op->getFirstArg().type.getScalarBitCount() < 32)
        {
            //make sure, leading bits are zeroes
            const int64_t mask = op->getFirstArg().type.getScalarWidthMask();
            it.emplace(new Operation(OP_AND, tmp, op->getFirstArg(), Value(Literal(mask), TYPE_INT32), op->conditional));
            it.nextInBlock();
            op->setArgument(0, tmp);
            op->setOpCode(OP_ITOF);
        }
        else if(op->getFirstArg().type.getScalarBitCount() > 32)
        {
            throw CompilationError(CompilationStep::OPTIMIZER, "Can't convert long to floating value, since long is not supported!");
        }
        else    //32-bits
        {
            //itofp + if MSB set add 2^31(f)
//            it.emplace(new Operation("and", REG_NOP, Value(Literal(0x80000000UL), TYPE_INT32), op->getFirstArg(), op->conditional, SetFlag::SET_FLAGS));
//            ++it;
//            it.emplace((new Operation("itof", tmp, op->getFirstArg(), op->conditional))->setDecorations(op->decoration));
//            ++it;
//            it.reset(new Operation("fadd", op->getOutput(), tmp, Value(Literal(std::pow(2, 31)), TYPE_FLOAT), COND_ZERO_CLEAR));
        	//TODO this passed OpenCL-CTS parameter_types, but what of large values (MSB set)??
        	op->setOpCode(OP_ITOF);
        }
    }
    //float to integer
    else if(op->opCode.compare("fptosi") == 0)
    {
        //just surgical modification
        op->setOpCode(OP_FTOI);
    }
    //float to unsigned integer
    else if(op->opCode.compare("fptoui") == 0)
    {
        //TODO special treatment??
    	//TODO truncate to type?
        op->setOpCode(OP_FTOI);
        op->decoration = add_flag(op->decoration, InstructionDecorations::UNSIGNED_RESULT);
    }
    //sign extension
    else if(op->opCode.compare("sext") == 0)
    {
        logging::debug() << "Intrinsifying sign extension with shifting" << logging::endl;
        it = insertSignExtension(it, method, op->getFirstArg(), op->getOutput(), true, op->conditional, op->setFlags);
        //remove 'sext'
        it.erase();
        //so next instruction is not skipped
        it.previousInBlock();
    }
    //zero extension
    else if(op->opCode.compare("zext") == 0)
    {
        logging::debug() << "Intrinsifying zero extension with and" << logging::endl;
        it = insertZeroExtension(it, method, op->getFirstArg(), op->getOutput(), true, op->conditional, op->setFlags);
        //remove 'zext'
        it.erase();
        //so next instruction is not skipped
        it.previousInBlock();
    }
    return it;
}

static InstructionWalker intrinsifyReadWorkGroupInfo(Method& method, InstructionWalker it, const Value& arg, const std::vector<std::string>& locals, const Value& defaultValue, const InstructionDecorations decoration)
{
	if(arg.hasType(ValueType::LITERAL))
	{
		Value src = UNDEFINED_VALUE;
		switch(arg.literal.integer)
		{
			case 0:
				src = method.findOrCreateLocal(TYPE_INT32, locals.at(0))->createReference();
				break;
			case 1:
				src = method.findOrCreateLocal(TYPE_INT32, locals.at(1))->createReference();
				break;
			case 2:
				src = method.findOrCreateLocal(TYPE_INT32, locals.at(2))->createReference();
				break;
			default:
				src = defaultValue;
		}
		return it.reset((new MoveOperation(it->getOutput(), src))->copyExtrasFrom(it.get()));
	}
	//dim == 0 -> return first value
	it.emplace((new Operation(OP_XOR, NOP_REGISTER, arg, INT_ZERO))->setSetFlags(SetFlag::SET_FLAGS));
	it.nextInBlock();
	it.emplace(new MoveOperation(it->getOutput(), method.findOrCreateLocal(TYPE_INT32, locals.at(0))->createReference(), COND_ZERO_SET));
	it.nextInBlock();
	//dim == 1 -> return second value
	it.emplace((new Operation(OP_XOR, NOP_REGISTER, arg, INT_ONE))->setSetFlags(SetFlag::SET_FLAGS));
	it.nextInBlock();
	it.emplace(new MoveOperation(it->getOutput(), method.findOrCreateLocal(TYPE_INT32, locals.at(1))->createReference(), COND_ZERO_SET));
	it.nextInBlock();
	//dim == 2 -> return third value
	it.emplace((new Operation(OP_XOR, NOP_REGISTER, arg, Value(Literal(static_cast<int64_t>(2)), TYPE_INT32)))->setSetFlags(SetFlag::SET_FLAGS));
	it.nextInBlock();
	it.emplace(new MoveOperation(it->getOutput(), method.findOrCreateLocal(TYPE_INT32, locals.at(2))->createReference(), COND_ZERO_SET));
	it.nextInBlock();
	//otherwise (dim > 2 -> 2 - dim < 0) return default value
	it.emplace((new Operation(OP_SUB, NOP_REGISTER, Value(Literal(static_cast<int64_t>(2)), TYPE_INT32), arg))->setSetFlags(SetFlag::SET_FLAGS));
	it.nextInBlock();
	return it.reset(new MoveOperation(it->getOutput(), defaultValue, COND_NEGATIVE_SET));
}

static InstructionWalker intrinsifyReadWorkItemInfo(Method& method, InstructionWalker it, const Value& arg, const std::string& local, const InstructionDecorations decoration)
{
	/*
	 * work-item infos (id, size) are stored within a single UNIFORM:
	 * high <-> low byte
	 * 00 | 3.dim | 2.dim | 1.dim
	 * -> res = (UNIFORM >> (dim * 8)) & 0xFF
	 */
	const Local* itemInfo = method.findOrCreateLocal(TYPE_INT32, local);
	Value tmp0(UNDEFINED_VALUE);
	//TODO this pre-calculation causes invalid values for get_global_id(), but why?? (see ./testing/test_work_item.cl)
//	if(arg.hasType(ValueType::LITERAL))
//	{
//		tmp0 = Value(Literal(arg.literal.integer * 8L), TYPE_INT8);
//	}
//	else
	{
		tmp0 = method.addNewLocal(TYPE_INT32, "%local_info");
		it.emplace(new Operation(OP_MUL24, tmp0, arg, Value(Literal(static_cast<int64_t>(8)), TYPE_INT32)));
		it.nextInBlock();
	}
	const Value tmp1 = method.addNewLocal(TYPE_INT32, "%local_info");
	it.emplace(new Operation(OP_SHR, tmp1, itemInfo->createReference(), tmp0));
	it.nextInBlock();
	return it.reset((new Operation(OP_AND, it->getOutput(), tmp1, Value(Literal(static_cast<int64_t>(0xFF)), TYPE_INT8)))->copyExtrasFrom(it.get()));
}

static InstructionWalker intrinsifyWorkItemFunctions(Method& method, InstructionWalker it)
{
	MethodCall* callSite = it.get<MethodCall>();
	if(callSite == nullptr)
		return it;
	if(callSite->getArguments().size() > 1)
		return it;

	if(callSite->methodName.compare("vc4cl_work_dimensions") == 0 && callSite->getArguments().size() == 0)
	{
		logging::debug() << "Intrinsifying reading of work-item dimensions" << logging::endl;
		//setting the type to int8 allows us to optimize e.g. multiplications with work-item values
		Value out = callSite->getOutput();
		out.type = TYPE_INT8;
		return it.reset((new MoveOperation(out, method.findOrCreateLocal(TYPE_INT32, Method::WORK_DIMENSIONS)->createReference()))->copyExtrasFrom(callSite)->setDecorations(add_flag(callSite->decoration, InstructionDecorations::BUILTIN_WORK_DIMENSIONS)));
	}
	if(callSite->methodName.compare("vc4cl_num_groups") == 0 && callSite->getArguments().size() == 1)
	{
		logging::debug() << "Intrinsifying reading of the number of work-groups" << logging::endl;
		return intrinsifyReadWorkGroupInfo(method, it, callSite->getArgument(0), {Method::NUM_GROUPS_X, Method::NUM_GROUPS_Y, Method::NUM_GROUPS_Z}, INT_ONE, InstructionDecorations::BUILTIN_NUM_GROUPS);
	}
	if(callSite->methodName.compare("vc4cl_group_id") == 0 && callSite->getArguments().size() == 1)
	{
		logging::debug() << "Intrinsifying reading of the work-group ids" << logging::endl;
		return intrinsifyReadWorkGroupInfo(method, it, callSite->getArgument(0), {Method::GROUP_ID_X, Method::GROUP_ID_Y, Method::GROUP_ID_Z}, INT_ZERO, InstructionDecorations::BUILTIN_GROUP_ID);
	}
	if(callSite->methodName.compare("vc4cl_global_offset") == 0 && callSite->getArguments().size() == 1)
	{
		logging::debug() << "Intrinsifying reading of the global offsets" << logging::endl;
		return intrinsifyReadWorkGroupInfo(method, it, callSite->getArgument(0), {Method::GLOBAL_OFFSET_X, Method::GLOBAL_OFFSET_Y, Method::GLOBAL_OFFSET_Z}, INT_ZERO, InstructionDecorations::BUILTIN_GLOBAL_OFFSET);
	}
	if(callSite->methodName.compare("vc4cl_local_size") == 0 && callSite->getArguments().size() == 1)
	{
		logging::debug() << "Intrinsifying reading of local work-item sizes" << logging::endl;
		//TODO needs to have a size of 1 for all higher dimensions (instead of currently implicit 0)
		return intrinsifyReadWorkItemInfo(method, it, callSite->getArgument(0), Method::LOCAL_SIZES, InstructionDecorations::BUILTIN_LOCAL_SIZE);
	}
	if(callSite->methodName.compare("vc4cl_local_id") == 0 && callSite->getArguments().size() == 1)
	{
		logging::debug() << "Intrinsifying reading of local work-item ids" << logging::endl;
		return intrinsifyReadWorkItemInfo(method, it, callSite->getArgument(0), Method::LOCAL_IDS, InstructionDecorations::BUILTIN_LOCAL_ID);
	}
	if(callSite->methodName.compare("vc4cl_global_size") == 0 && callSite->getArguments().size() == 1)
	{
		//global_size(dim) = local_size(dim) * num_groups(dim)
		logging::debug() << "Intrinsifying reading of global work-item sizes" << logging::endl;

		const Value tmpLocalSize = method.addNewLocal(TYPE_INT8, "%local_size");
		const Value tmpNumGroups = method.addNewLocal(TYPE_INT32, "%num_groups");
		//emplace dummy instructions to be replaced
		it.emplace(new MoveOperation(tmpLocalSize, NOP_REGISTER));
		it = intrinsifyReadWorkItemInfo(method, it, callSite->getArgument(0), Method::LOCAL_SIZES, InstructionDecorations::BUILTIN_LOCAL_SIZE);
		it.nextInBlock();
		it.emplace(new MoveOperation(tmpNumGroups, NOP_REGISTER));
		it = intrinsifyReadWorkGroupInfo(method, it, callSite->getArgument(0), {Method::NUM_GROUPS_X, Method::NUM_GROUPS_Y, Method::NUM_GROUPS_Z}, INT_ONE, InstructionDecorations::BUILTIN_NUM_GROUPS);
		it.nextInBlock();
		return it.reset((new Operation(OP_MUL24, callSite->getOutput(), tmpLocalSize, tmpNumGroups))->copyExtrasFrom(callSite)->setDecorations(add_flag(callSite->decoration, InstructionDecorations::BUILTIN_GLOBAL_SIZE)));
	}
	if(callSite->methodName.compare("vc4cl_global_id") == 0 && callSite->getArguments().size() == 1)
	{
		//global_id(dim) = global_offset(dim) + (group_id(dim) * local_size(dim) + local_id(dim)
		logging::debug() << "Intrinsifying reading of global work-item ids" << logging::endl;

		const Value tmpGroupID = method.addNewLocal(TYPE_INT32, "%group_id");
		const Value tmpLocalSize = method.addNewLocal(TYPE_INT8, "%local_size");
		const Value tmpGlobalOffset = method.addNewLocal(TYPE_INT32, "%global_offset");
		const Value tmpLocalID = method.addNewLocal(TYPE_INT8, "%local_id");
		const Value tmpRes0 = method.addNewLocal(TYPE_INT32, "%global_id");
		const Value tmpRes1 = method.addNewLocal(TYPE_INT32, "%global_id");
		//emplace dummy instructions to be replaced
		it.emplace(new MoveOperation(tmpGroupID, NOP_REGISTER));
		it = intrinsifyReadWorkGroupInfo(method, it, callSite->getArgument(0), {Method::GROUP_ID_X, Method::GROUP_ID_Y, Method::GROUP_ID_Z}, INT_ZERO, InstructionDecorations::BUILTIN_GROUP_ID);
		it.nextInBlock();
		it.emplace(new MoveOperation(tmpLocalSize, NOP_REGISTER));
		it = intrinsifyReadWorkItemInfo(method, it, callSite->getArgument(0), Method::LOCAL_SIZES, InstructionDecorations::BUILTIN_LOCAL_SIZE);
		it.nextInBlock();
		it.emplace(new MoveOperation(tmpGlobalOffset, NOP_REGISTER));
		it = intrinsifyReadWorkGroupInfo(method, it, callSite->getArgument(0), {Method::GLOBAL_OFFSET_X, Method::GLOBAL_OFFSET_Y, Method::GLOBAL_OFFSET_Z}, INT_ZERO, InstructionDecorations::BUILTIN_GLOBAL_OFFSET);
		it.nextInBlock();
		it.emplace(new MoveOperation(tmpLocalID, NOP_REGISTER));
		it = intrinsifyReadWorkItemInfo(method, it, callSite->getArgument(0), Method::LOCAL_IDS, InstructionDecorations::BUILTIN_LOCAL_ID);
		it.nextInBlock();
		it.emplace(new Operation(OP_MUL24, tmpRes0, tmpGroupID, tmpLocalSize));
		it.nextInBlock();
		it.emplace(new Operation(OP_ADD, tmpRes1, tmpGlobalOffset, tmpRes0));
		it.nextInBlock();
		return it.reset((new Operation(OP_ADD, callSite->getOutput(), tmpRes1, tmpLocalID))->copyExtrasFrom(callSite)->setDecorations(add_flag(callSite->decoration, InstructionDecorations::BUILTIN_GLOBAL_ID)));
	}
	return it;
}

InstructionWalker optimizations::intrinsify(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
	if(!it.has<Operation>() && !it.has<MethodCall>())
		//fail fast
		return it;
	auto newIt = intrinsifyComparison(method, it);
	if(newIt == it)
	{
		//no changes so far
		newIt = intrinsifyWorkItemFunctions(method, it);
	}
	if(newIt == it)
	{
		//no changes so far
		newIt = intrinsifyNoArgs(method, it);
	}
	if(newIt == it)
	{
		//no changes so far
		newIt = intrinsifyUnary(method, it);
	}
	if(newIt == it)
	{
		//no changes so far
		newIt = intrinsifyBinary(method, it);
	}
	if(newIt == it)
	{
		//no changes so far
		newIt = intrinsifyTernary(method, it);
	}
	if(newIt == it)
	{
		//no changes so far
		newIt = intrinsifyArithmetic(method, it, config.mathType);
	}
	if(newIt == it)
	{
		//no changes so far
		newIt = intrinsifyImageFunction(it, method);
	}
	return newIt;
}
