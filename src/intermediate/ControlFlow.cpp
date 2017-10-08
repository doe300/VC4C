/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IntermediateInstruction.h"
#include "../asm/SemaphoreInstruction.h"
#include "helper.h"

using namespace vc4c;
using namespace vc4c::intermediate;

SemaphoreAdjustment::SemaphoreAdjustment(const Semaphore semaphore, const bool increase, const ConditionCode& cond, const SetFlag setFlags) :
IntermediateInstruction(NO_VALUE, cond, setFlags), semaphore(semaphore), increase(increase)
{

}

SemaphoreAdjustment::~SemaphoreAdjustment()
{

}

std::string SemaphoreAdjustment::to_string() const
{
    return std::string("semaphore ") + (std::to_string(static_cast<unsigned>(semaphore)) + " ") + (increase ? "increase" : "decrease") + createAdditionalInfoString();
}

qpu_asm::Instruction* SemaphoreAdjustment::convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    return new qpu_asm::SemaphoreInstruction(PACK_NOP, conditional, conditional, setFlags, WriteSwap::DONT_SWAP, REG_NOP.num, REG_NOP.num, increase, semaphore);
}

IntermediateInstruction* SemaphoreAdjustment::copyFor(Method& method, const std::string& localPrefix) const
{
    return (new SemaphoreAdjustment(semaphore, increase, conditional, setFlags))->copyExtrasFrom(this);
}

MemoryBarrier::MemoryBarrier(const MemoryScope scope, const MemorySemantics semantics) : IntermediateInstruction(NO_VALUE), scope(scope), semantics(semantics)
{

}

MemoryBarrier::~MemoryBarrier()
{

}

static std::string toString(const MemoryScope scope)
{
	switch(scope)
	{
		case MemoryScope::CROSS_DEVICE:
			return "global";
		case MemoryScope::DEVICE:
			return "device";
		case MemoryScope::SUB_GROUP:
			return "sub-group";
		case MemoryScope::WORK_GROUP:
			return "work-group";
		case MemoryScope::INVOCATION:
			return "invocation";
	}
	throw CompilationError(CompilationStep::GENERAL, "Unsupported memory scope value", std::to_string(static_cast<int>(scope)));
}

static std::string toString(const MemorySemantics semantics)
{
	std::vector<std::string> result;
	if(has_flag(semantics, MemorySemantics::ACQUIRE) || has_flag(semantics, MemorySemantics::ACQUIRE_RELEASE))
		result.push_back("acquire");
	if(has_flag(semantics, MemorySemantics::RELEASE) || has_flag(semantics, MemorySemantics::ACQUIRE_RELEASE))
		result.push_back("release");
	if(has_flag(semantics, MemorySemantics::SEQUENTIALLY_CONSISTENT))
		result.push_back("sequentially consistent");
	if(has_flag(semantics, MemorySemantics::SUBGROUP_MEMORY))
		result.push_back("sub-group");
	if(has_flag(semantics, MemorySemantics::WORK_GROUP_MEMORY))
		result.push_back("work-group");
	if(has_flag(semantics, MemorySemantics::CROSS_WORK_GROUP_MEMORY))
		result.push_back("global");
	if(has_flag(semantics, MemorySemantics::ATOMIC_COUNTER_MEMORY))
		result.push_back("atomic counter");
	if(has_flag(semantics, MemorySemantics::IMAGE_MEMORY))
		result.push_back("image");
	return vc4c::to_string<std::string>(result, "|");
}

std::string MemoryBarrier::to_string() const
{
	return std::string("mem-fence ") + (::toString(scope) + ", ") + ::toString(semantics) + createAdditionalInfoString();
}

qpu_asm::Instruction* MemoryBarrier::convertToAsm(const FastMap<const Local*, Register>& registerMapping, const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
	throw CompilationError(CompilationStep::CODE_GENERATION, "There should be no more memory barriers at this point", to_string());
}

IntermediateInstruction* MemoryBarrier::copyFor(Method& method, const std::string& localPrefix) const
{
	return (new MemoryBarrier(scope, semantics))->copyExtrasFrom(this);
}

bool MemoryBarrier::mapsToASMInstruction() const
{
	return false;
}
