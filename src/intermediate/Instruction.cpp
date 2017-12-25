/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IntermediateInstruction.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::intermediate;

std::string intermediate::toString(const InstructionDecorations decoration)
{
	std::string res;
	if(has_flag(decoration, InstructionDecorations::ALLOW_RECIP))
		res.append("recip ");
	if(has_flag(decoration, InstructionDecorations::FAST_MATH))
		res.append("fast_math ");
	if(has_flag(decoration, InstructionDecorations::NO_INF))
		res.append("no_inf ");
	if(has_flag(decoration, InstructionDecorations::NO_NAN))
		res.append("no_nan ");
	if(has_flag(decoration, InstructionDecorations::SATURATED_CONVERSION))
		res.append("sat ");
	if(has_flag(decoration, InstructionDecorations::BUILTIN_WORK_DIMENSIONS))
		res.append("work-dims ");
	if(has_flag(decoration, InstructionDecorations::BUILTIN_LOCAL_SIZE))
		res.append("local_size ");
	if(has_flag(decoration, InstructionDecorations::BUILTIN_LOCAL_ID))
		res.append("local_id ");
	if(has_flag(decoration, InstructionDecorations::BUILTIN_NUM_GROUPS))
		res.append("num_groups ");
	if(has_flag(decoration, InstructionDecorations::BUILTIN_GROUP_ID))
		res.append("group_id ");
	if(has_flag(decoration, InstructionDecorations::BUILTIN_GLOBAL_OFFSET))
		res.append("global_offset ");
	if(has_flag(decoration, InstructionDecorations::BUILTIN_GLOBAL_SIZE))
		res.append("global_size ");
	if(has_flag(decoration, InstructionDecorations::BUILTIN_GLOBAL_ID))
		res.append("global_id ");
	if(has_flag(decoration, InstructionDecorations::UNSIGNED_RESULT))
		res.append("unsigned ");
	if(has_flag(decoration, InstructionDecorations::PHI_NODE))
		res.append("phi ");
	if(has_flag(decoration, InstructionDecorations::BRANCH_ON_ALL_ELEMENTS))
		res.append("all_elements ");
	if(has_flag(decoration, InstructionDecorations::ELEMENT_INSERTION))
		res.append("single_element ");
	if(has_flag(decoration, InstructionDecorations::AUTO_VECTORIZED))
		res.append("vectorized ");
	return res.substr(0, res.empty() ? 0 : res.size() - 1);
}

IntermediateInstruction::IntermediateInstruction(Optional<Value> output, ConditionCode cond, SetFlag setFlags, Pack packMode) :
signal(SIGNAL_NONE), unpackMode(UNPACK_NOP),  packMode(packMode), conditional(cond), setFlags(setFlags), decoration(InstructionDecorations::NONE), canBeCombined(true), output(output), arguments()
{
	if(output.hasValue)
		addAsUserToValue(output, LocalUser::Type::WRITER);
}

IntermediateInstruction::~IntermediateInstruction()
{
	//this can't be in LocalUser
	//since at the time, the ~LocalUser() is called, the IntermediateInstruction "part" is already destroyed
	for(const auto& pair : getUsedLocals())
		const_cast<Local*>(pair.first)->removeUser(*this, LocalUser::Type::BOTH);
}

bool IntermediateInstruction::mapsToASMInstruction() const
{
	return true;
}

const Optional<Value>& IntermediateInstruction::getOutput() const
{
	return output;
}

bool IntermediateInstruction::hasValueType(const ValueType type) const
{
    return output && output.get().hasType(type);
}

const Optional<Value> IntermediateInstruction::getArgument(const std::size_t index) const
{
	if(arguments.size() > index)
		return arguments.at(index);
	return NO_VALUE;
}

const std::vector<Value>& IntermediateInstruction::getArguments() const
{
	return arguments;
}


void IntermediateInstruction::setArgument(const std::size_t index, const Value& arg)
{
	if(index < arguments.size())
	{
		removeAsUserFromValue(arguments[index], LocalUser::Type::READER);
		arguments[index] = arg;
	}
	else
		//this is somehow required, since it crashes, when an uninitialized Value is assigned a value
		arguments.insert(arguments.begin() + index, arg);

	addAsUserToValue(arg, LocalUser::Type::READER);
}

IntermediateInstruction* IntermediateInstruction::setOutput(const Optional<Value>& output)
{
	if(this->output.hasValue)
		removeAsUserFromValue(this->output, LocalUser::Type::WRITER);
	this->output = output;
	if(output.hasValue)
		addAsUserToValue(output, LocalUser::Type::WRITER);
	return this;
}

IntermediateInstruction* IntermediateInstruction::setSignaling(const Signaling signal)
{
    this->signal = signal;
    return this;
}

IntermediateInstruction* IntermediateInstruction::setPackMode(const Pack packMode)
{
    this->packMode = packMode;
    return this;
}

IntermediateInstruction* IntermediateInstruction::setCondition(const ConditionCode condition)
{
    this->conditional = condition;
    return this;
}

IntermediateInstruction* IntermediateInstruction::setSetFlags(const SetFlag setFlags)
{
    this->setFlags = setFlags;
    return this;
}

IntermediateInstruction* IntermediateInstruction::setUnpackMode(const Unpack unpackMode)
{
    this->unpackMode = unpackMode;
    return this;
}

IntermediateInstruction* IntermediateInstruction::setDecorations(const InstructionDecorations decorations)
{
	this->decoration = add_flag(this->decoration, decorations);
    return this;
}

bool IntermediateInstruction::hasSideEffects() const
{
	if(dynamic_cast<const Branch*>(this) != nullptr)
		return true;
	if(dynamic_cast<const SemaphoreAdjustment*>(this) != nullptr)
		return true;
	if(hasValueType(ValueType::REGISTER) && output.get().reg.hasSideEffectsOnWrite())
		return true;
	for(const Value& arg : arguments)
	{
		if(arg.hasType(ValueType::REGISTER) && arg.reg.hasSideEffectsOnRead())
			return true;
	}
	return signal.hasSideEffects() || setFlags == SetFlag::SET_FLAGS;
}

bool IntermediateInstruction::hasUnpackMode() const
{
	return unpackMode != UNPACK_NOP;
}

bool IntermediateInstruction::hasPackMode() const
{
	return packMode != PACK_NOP;
}

bool IntermediateInstruction::hasConditionalExecution() const
{
	return conditional != COND_ALWAYS;
}

IntermediateInstruction* IntermediateInstruction::copyExtrasFrom(const IntermediateInstruction* src)
{
	if(conditional != COND_ALWAYS && src->conditional != COND_ALWAYS && conditional != src->conditional)
		throw CompilationError(CompilationStep::GENERAL, "Failed to merge two distinct conditions", to_string());
	if(conditional == COND_ALWAYS)
		this->setCondition(src->conditional);
	this->setDecorations(add_flag(this->decoration, src->decoration));
	if(packMode != PACK_NOP && src->packMode != PACK_NOP && packMode != src->packMode)
		throw CompilationError(CompilationStep::GENERAL, "Failed to merge two distinct pack-modes", to_string());
	if(packMode == PACK_NOP)
		this->setPackMode(src->packMode);
	if(setFlags == SetFlag::DONT_SET)
		this->setSetFlags(src->setFlags);
	if(signal != SIGNAL_NONE && src->signal != SIGNAL_NONE && signal != src->signal)
		throw CompilationError(CompilationStep::GENERAL, "Failed to merge two distinct signals", to_string());
	if(signal == SIGNAL_NONE)
		this->setSignaling(src->signal);
	if(unpackMode != UNPACK_NOP && src->unpackMode != UNPACK_NOP && unpackMode != src->unpackMode)
		throw CompilationError(CompilationStep::GENERAL, "Failed to merge two distinct unpack-modes", to_string());
	if(unpackMode == UNPACK_NOP)
		this->setUnpackMode(src->unpackMode);
	return this;
}

Optional<Value> IntermediateInstruction::precalculate(const std::size_t numIterations) const
{
	return NO_VALUE;
}

const Value IntermediateInstruction::renameValue(Method& method, const Value& orig, const std::string& prefix) const
{
	if(!orig.hasType(ValueType::LOCAL))
		return orig;
	if(orig.local->is<Global>())
		return orig;
	if(orig.local->is<StackAllocation>())
	{
		const StackAllocation* alloc = orig.local->as<StackAllocation>();
		if(method.findStackAllocation(prefix + alloc->name) != nullptr)
			return method.findStackAllocation(prefix + alloc->name)->createReference();
		auto pos = method.stackAllocations.emplace(StackAllocation(prefix + alloc->name, alloc->type, alloc->size, alloc->alignment));
		return pos.first->createReference();
	}
	return method.findOrCreateLocal(orig.type, prefix + orig.local->name)->createReference();
}

std::string IntermediateInstruction::createAdditionalInfoString() const
{
	std::string res("(");
	if(signal.hasSideEffects() && signal != SIGNAL_BRANCH)
		res.append(signal.toString()).append(" ");
	if(hasUnpackMode())
		res.append(unpackMode.toString()).append(" ");
	if(hasPackMode())
		res.append(packMode.toString()).append(" ");
	if(hasConditionalExecution())
		res.append(conditional.toString()).append(" ");
	if(setFlags != SetFlag::DONT_SET)
		res.append(vc4c::toString(setFlags)).append(" ");
	res.append(toString(decoration)).append(" ");
	//remove trailing ' ' (or '(' if empty)
	res = res.substr(0, res.size() - 1);
	if(!res.empty())
		res = std::string(" ") + res + ")";
	return std::string(" ()").compare(res) == 0 ? "" : res;
}

Optional<Value> IntermediateInstruction::getPrecalculatedValueForArg(const std::size_t argIndex, const std::size_t numIterations) const
{
	if(numIterations == 0)
		return NO_VALUE;
	if(argIndex > arguments.size())
		throw CompilationError(CompilationStep::GENERAL, "Invalid argument index", std::to_string(argIndex));
	const Value& arg = arguments.at(argIndex);
	switch(arg.valueType)
	{
		case ValueType::SMALL_IMMEDIATE:
			if(arg.immediate.toLiteral().hasValue)
				return Value(arg.immediate.toLiteral(), arg.type);
			break;
		case ValueType::LITERAL:
			return arg;
		case ValueType::LOCAL:
		{
			auto writer = arg.local->getSingleWriter();
			if(dynamic_cast<const IntermediateInstruction*>(writer) != nullptr)
				return dynamic_cast<const IntermediateInstruction*>(writer)->precalculate(numIterations - 1);
			break;
		}
		default:
			return NO_VALUE;
	}
	return NO_VALUE;
}

FastMap<const Local*, LocalUser::Type> IntermediateInstruction::getUsedLocals() const
{
	FastMap<const Local*, LocalUser::Type> locals;
	if(output.hasValue && output.get().hasType(ValueType::LOCAL))
		locals.emplace(output.get().local, LocalUser::Type::WRITER);
	for(const Value& arg : arguments)
	{
		if(arg.hasType(ValueType::LOCAL))
			locals.emplace(arg.local, LocalUser::Type::READER);
	}
	return locals;
}

void IntermediateInstruction::forUsedLocals(const std::function<void(const Local*, LocalUser::Type)>& consumer) const
{
	if(output.hasValue && output.get().hasType(ValueType::LOCAL))
		consumer(output.get().local, LocalUser::Type::WRITER);
	for(const Value& arg : arguments)
	{
		if(arg.hasType(ValueType::LOCAL))
			consumer(arg.local, LocalUser::Type::READER);
	}
}

bool IntermediateInstruction::readsLocal(const Local* local) const
{
	for(const auto& arg : arguments)
	{
		if(arg.hasLocal(local))
			return true;
	}
	return false;
}

bool IntermediateInstruction::writesLocal(const Local* local) const
{
	return hasValueType(ValueType::LOCAL) && output.get().hasLocal(local);
}

void IntermediateInstruction::replaceLocal(const Local* oldLocal, const Local* newLocal, const Type type)
{
	if(oldLocal == newLocal)
		return;
	if(has_flag(type, LocalUser::Type::WRITER) && output.hasValue && output.get().hasLocal(oldLocal))
	{
		removeAsUserFromValue(output, LocalUser::Type::WRITER);
		output.get().local = const_cast<Local*>(newLocal);
		addAsUserToValue(output, LocalUser::Type::WRITER);
	}
	if(has_flag(type, LocalUser::Type::READER))
	{
		for(Value& arg : arguments)
		{
			if(arg.hasLocal(oldLocal))
			{
				removeAsUserFromValue(arg,  LocalUser::Type::READER);
				arg.local = const_cast<Local*>(newLocal);
				addAsUserToValue(arg, LocalUser::Type::READER);
			}
		}
	}
}

void IntermediateInstruction::removeAsUserFromValue(const Value& value, const LocalUser::Type type)
{
	if(value.hasType(ValueType::LOCAL))
		const_cast<Local*>(value.local)->removeUser(*this, type);
}

void IntermediateInstruction::addAsUserToValue(const Value& value, LocalUser::Type type)
{
	if(has_flag(type, LocalUser::Type::READER))
		value.assertReadable();
	if(has_flag(type, LocalUser::Type::WRITER))
		value.assertWriteable();

	if(value.hasType(ValueType::LOCAL))
		const_cast<Local*>(value.local)->addUser(*this, type);
}
