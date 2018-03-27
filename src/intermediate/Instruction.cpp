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

InstructionDecorations intermediate::forwardDecorations(InstructionDecorations decorations)
{
	InstructionDecorations res = InstructionDecorations::NONE;
	if(has_flag(decorations, InstructionDecorations::BUILTIN_GLOBAL_ID))
		res = add_flag(res, InstructionDecorations::BUILTIN_GLOBAL_ID);
	if(has_flag(decorations, InstructionDecorations::BUILTIN_GLOBAL_OFFSET))
		res = add_flag(res, InstructionDecorations::BUILTIN_GLOBAL_OFFSET);
	if(has_flag(decorations, InstructionDecorations::BUILTIN_GLOBAL_SIZE))
		res = add_flag(res, InstructionDecorations::BUILTIN_GLOBAL_SIZE);
	if(has_flag(decorations, InstructionDecorations::BUILTIN_LOCAL_ID))
		res = add_flag(res, InstructionDecorations::BUILTIN_LOCAL_ID);
	if(has_flag(decorations, InstructionDecorations::BUILTIN_LOCAL_SIZE))
		res = add_flag(res, InstructionDecorations::BUILTIN_LOCAL_SIZE);
	if(has_flag(decorations, InstructionDecorations::BUILTIN_NUM_GROUPS))
		res = add_flag(res, InstructionDecorations::BUILTIN_NUM_GROUPS);
	if(has_flag(decorations, InstructionDecorations::BUILTIN_WORK_DIMENSIONS))
		res = add_flag(res, InstructionDecorations::BUILTIN_WORK_DIMENSIONS);
	if(has_flag(decorations, InstructionDecorations::UNSIGNED_RESULT))
		res = add_flag(res, InstructionDecorations::UNSIGNED_RESULT);
	return res;
}

IntermediateInstruction::IntermediateInstruction(Optional<Value> output, ConditionCode cond, SetFlag setFlags, Pack packMode) :
signal(SIGNAL_NONE), unpackMode(UNPACK_NOP),  packMode(packMode), conditional(cond), setFlags(setFlags), decoration(InstructionDecorations::NONE), canBeCombined(true), output(output), arguments()
{
	if(output)
		addAsUserToValue(output.value(), LocalUse::Type::WRITER);
}

IntermediateInstruction::~IntermediateInstruction()
{
	//this can't be in LocalUser
	//since at the time, the ~LocalUser() is called, the IntermediateInstruction "part" is already destroyed
	for(const auto& pair : getUsedLocals())
		const_cast<Local*>(pair.first)->removeUser(*this, LocalUse::Type::BOTH);
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
    return output && output->hasType(type);
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
		removeAsUserFromValue(arguments[index], LocalUse::Type::READER);
		arguments[index] = arg;
	}
	else
		//this is somehow required, since it crashes, when an uninitialized Value is assigned a value
		arguments.insert(arguments.begin() + index, arg);

	addAsUserToValue(arg, LocalUse::Type::READER);
}

IntermediateInstruction* IntermediateInstruction::setOutput(const Optional<Value>& output)
{
	if(this->output)
		removeAsUserFromValue(this->output.value(), LocalUse::Type::WRITER);
	this->output = output;
	if(output)
		addAsUserToValue(output.value(), LocalUse::Type::WRITER);
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

IntermediateInstruction* IntermediateInstruction::addDecorations(const InstructionDecorations decorations)
{
	this->decoration = add_flag(this->decoration, decorations);
    return this;
}

bool IntermediateInstruction::hasDecoration(InstructionDecorations deco) const
{
	return has_flag(decoration, deco);
}

bool IntermediateInstruction::hasSideEffects() const
{
	if(dynamic_cast<const Branch*>(this) != nullptr)
		return true;
	if(dynamic_cast<const SemaphoreAdjustment*>(this) != nullptr)
		return true;
	if(hasValueType(ValueType::REGISTER) && output->reg.hasSideEffectsOnWrite())
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
	this->addDecorations(add_flag(this->decoration, src->decoration));
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
	const Local* copy = method.findOrCreateLocal(orig.type, prefix + orig.local->name);
	if(orig.local->reference.first != nullptr)
		//re-reference the copied local to the (original) source
		const_cast<std::pair<Local*, int>&>(copy->reference) = std::make_pair(renameValue(method, orig.local->reference.first->createReference(), prefix).local, orig.local->reference.second);
	return copy->createReference();
}

std::string IntermediateInstruction::createAdditionalInfoString() const
{
	std::string res("(");
	if(signal.hasSideEffects() && signal != SIGNAL_BRANCH)
		res.append(signal.to_string()).append(" ");
	if(hasUnpackMode())
		res.append(unpackMode.to_string()).append(" ");
	if(hasPackMode())
		res.append(packMode.to_string()).append(" ");
	if(hasConditionalExecution())
		res.append(conditional.to_string()).append(" ");
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
			if(arg.immediate.toLiteral())
				return Value(arg.immediate.toLiteral().value(), arg.type);
			break;
		case ValueType::LITERAL:
			return arg;
		case ValueType::LOCAL:
		{
			auto writer = arg.local->getSingleWriter();
			if(writer != nullptr)
				return writer->precalculate(numIterations - 1);
			break;
		}
		case ValueType::REGISTER:
		{
			if(arg.hasRegister(REG_ELEMENT_NUMBER))
			{
				Value elementIndices(ContainerValue(16), arg.type);
				for(unsigned i = 0; i < NATIVE_VECTOR_SIZE; ++i)
				{
					elementIndices.container.elements.emplace_back(Literal(i), TYPE_INT8);
				}
				return elementIndices;
			}
			return arg;
		}
		case ValueType::CONTAINER:
			return arg;
		default:
			return NO_VALUE;
	}
	return NO_VALUE;
}

FastMap<const Local*, LocalUse::Type> IntermediateInstruction::getUsedLocals() const
{
	FastMap<const Local*, LocalUse::Type> locals;
	if(output && output->hasType(ValueType::LOCAL))
		locals.emplace(output->local, LocalUse::Type::WRITER);
	for(const Value& arg : arguments)
	{
		if(arg.hasType(ValueType::LOCAL))
			locals.emplace(arg.local, LocalUse::Type::READER);
	}
	return locals;
}

void IntermediateInstruction::forUsedLocals(const std::function<void(const Local*, LocalUse::Type)>& consumer) const
{
	if(output && output->hasType(ValueType::LOCAL))
		consumer(output->local, LocalUse::Type::WRITER);
	for(const Value& arg : arguments)
	{
		if(arg.hasType(ValueType::LOCAL))
			consumer(arg.local, LocalUse::Type::READER);
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
	return hasValueType(ValueType::LOCAL) && output->hasLocal(local);
}

void IntermediateInstruction::replaceLocal(const Local* oldLocal, const Local* newLocal, const LocalUse::Type type)
{
	replaceValue(oldLocal->createReference(), newLocal->createReference(), type);
}

void IntermediateInstruction::replaceLocal(const Local *oldLocal, const Value newValue, LocalUse::Type type) {
	replaceValue(oldLocal->createReference(), newValue, type);
}

bool IntermediateInstruction::replaceValue(const Value oldValue, const Value newValue, LocalUse::Type type) {
	bool replaced = false;
	if(newValue == oldValue)
		return false;
	if(has_flag(type, LocalUse::Type::WRITER) && output && output == oldValue)
	{
		logging::debug() << "replaceValue: replace " << output.to_string() << " to " << newValue.to_string(true, true)
						 << " in " << to_string() << logging::endl;
		setOutput(Optional<Value>(newValue));
		replaced = true;
	}

	if(has_flag(type, LocalUse::Type::READER))
	{
		for(Value& arg : arguments)
		{
			if(arg == oldValue)
			{
				logging::debug() << "replaceValue: replace " << arg.to_string() << " to " << newValue.to_string(true, true)
								 << " in " << to_string() << logging::endl;
				removeAsUserFromValue(arg,  LocalUse::Type::READER);
				arg = newValue;
				addAsUserToValue(arg, LocalUse::Type::READER);
				replaced = true;
			}
		}
	}

	return replaced;
}

bool IntermediateInstruction::readsRegister(const Register& reg) const
{
	for(const Value& arg : arguments)
		if(arg.hasRegister(reg))
			return true;
	return false;
}

bool IntermediateInstruction::writesRegister(const Register& reg) const
{
	return output.ifPresent(toFunction(&Value::hasRegister, reg));
}

bool IntermediateInstruction::readsLiteral() const
{
	for(const Value& arg : arguments)
		if(arg.isLiteralValue())
			return true;
	return false;
}

void IntermediateInstruction::removeAsUserFromValue(const Value& value, const LocalUse::Type type)
{
	if(value.hasType(ValueType::LOCAL))
		const_cast<Local*>(value.local)->removeUser(*this, type);
}

void IntermediateInstruction::addAsUserToValue(const Value& value, LocalUse::Type type)
{
	if(has_flag(type, LocalUse::Type::READER))
		value.assertReadable();
	if(has_flag(type, LocalUse::Type::WRITER))
		value.assertWriteable();

	if(value.hasType(ValueType::LOCAL))
		const_cast<Local*>(value.local)->addUser(*this, type);
}

bool IntermediateInstruction::doesSetFlag() const {
	return setFlags == SetFlag::SET_FLAGS;
}

