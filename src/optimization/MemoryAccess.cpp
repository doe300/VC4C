/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "MemoryAccess.h"

#include "../intermediate/IntermediateInstruction.h"
#include "../periphery/VPM.h"
#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "log.h"

#include <algorithm>
#include <functional>

using namespace vc4c;
using namespace vc4c::optimizations;
using namespace vc4c::intermediate;
using namespace vc4c::periphery;

struct BaseAndOffset
{
	Optional<Value> base;
	Optional<int64_t> offset;
	Optional<int64_t> maxOffset;

	explicit BaseAndOffset() : base(NO_VALUE), offset(false, -1L), maxOffset(false, -1L)
	{}

	BaseAndOffset(Optional<Value> base, Optional<int64_t> offset, Optional<int64_t> maxOffset = Optional<int64_t>(false, -1L)) : base(base), offset(offset), maxOffset(maxOffset)
	{}
};

static BaseAndOffset findOffset(const Value& val)
{
	if(!val.hasType(ValueType::LOCAL))
		return BaseAndOffset();
	const LocalUser* writer = val.local->getSingleWriter();
	if(dynamic_cast<const IntermediateInstruction*>(writer) != nullptr)
	{
		const Optional<Value> offset =  dynamic_cast<const IntermediateInstruction*>(writer)->precalculate(8);
		if(offset.hasValue && offset.get().hasType(ValueType::LITERAL))
		{
			return BaseAndOffset(NO_VALUE, offset.get().literal.integer, offset.get().literal.integer);
		}
	}
	return BaseAndOffset();
}

static BaseAndOffset findBaseAndOffset(const Value& val)
{
	//TODO add support for offsets via getlocal/global_id, etc.
	//need to the set base to addr + offset and the offset to the offset of the offset (e.g. param[get_local_id(0) + 7])
	//but how to determine?
	if(!val.hasType(ValueType::LOCAL))
		return BaseAndOffset();
	if(val.local->is<Parameter>() || val.local->is<Global>() || val.local->is<StackAllocation>())
		return BaseAndOffset(val, static_cast<int64_t>(0));

	if(val.local->reference.first != nullptr && val.local->reference.second != ANY_ELEMENT)
		return BaseAndOffset(val.local->reference.first->createReference(), static_cast<int64_t>(val.local->reference.second));

	const auto writers = val.local->getUsers(LocalUser::Type::WRITER);
	if(writers.size() != 1)
		return BaseAndOffset();

	//The reader can be one of several valid cases:
	//1. a move from another local -> need to follow the move
	if(dynamic_cast<const MoveOperation*>(*writers.begin()) != nullptr)
		return findBaseAndOffset(dynamic_cast<const MoveOperation*>(*writers.begin())->getSource());
	const auto& args = dynamic_cast<const IntermediateInstruction*>(*writers.begin())->getArguments();
	//2. an arithmetic operation with a local and a literal -> the local is the base, the literal the offset
	if(args.size() == 2 && std::any_of(args.begin(), args.end(), [](const Value& arg) -> bool{return arg.hasType(ValueType::LOCAL);}) && std::any_of(args.begin(), args.end(), [](const Value& arg) -> bool{return arg.hasType(ValueType::LITERAL);}))
	{
		return BaseAndOffset(*std::find_if(args.begin(), args.end(), [](const Value& arg) -> bool{return arg.hasType(ValueType::LOCAL);}),
				static_cast<int64_t>((*std::find_if(args.begin(), args.end(), [](const Value& arg) -> bool{return arg.hasType(ValueType::LITERAL);})).literal.integer / val.type.getElementType().getPhysicalWidth()));
	}

	//3. an arithmetic operation with two locals -> one is the base, the other the calculation of the literal
	if(args.size() == 2 && std::all_of(args.begin(), args.end(), [](const Value& arg) -> bool{return arg.hasType(ValueType::LOCAL);}))
	{
		const auto offset0 = findOffset(args.at(0));
		const auto offset1 = findOffset(args.at(1));
		if(offset0.offset.hasValue && args.at(1).hasType(ValueType::LOCAL))
			return BaseAndOffset(args.at(1), static_cast<int64_t>(offset0.offset.get() / val.type.getElementType().getPhysicalWidth()));
		if(offset1.offset.hasValue && args.at(0).hasType(ValueType::LOCAL))
			return BaseAndOffset(args.at(0), static_cast<int64_t>(offset1.offset.get() / val.type.getElementType().getPhysicalWidth()));
	}

	return BaseAndOffset();
}

static InstructionWalker findDMASetup(InstructionWalker pos, const InstructionWalker end, bool isVPMWrite)
{
	while(!pos.isStartOfBlock())
	{
		if(pos.has<LoadImmediate>() && pos->hasValueType(ValueType::REGISTER))
		{
			if(isVPMWrite && pos->writesRegister(REG_VPM_OUT_SETUP) && VPWSetup::fromLiteral(pos.get<LoadImmediate>()->getImmediate().integer).isDMASetup())
				return pos;
			if(!isVPMWrite && pos->writesRegister(REG_VPM_IN_SETUP) && VPRSetup::fromLiteral(pos.get<LoadImmediate>()->getImmediate().integer).isDMASetup())
				return pos;
		}
		if(pos.has<MutexLock>() && pos.get<MutexLock>()->locksMutex())
			//only read up to the previous mutex acquire
			break;
		pos.previousInBlock();
	}
	return end;
}

static InstructionWalker findGenericSetup(InstructionWalker it, const InstructionWalker end, bool isVPMWrite)
{
	if(isVPMWrite)
	{
		//for VPM writes, the generic setups is located before the address write
		while(!it.isStartOfBlock())
		{
			if(it.has<LoadImmediate>() && it->writesRegister(REG_VPM_OUT_SETUP) && VPWSetup::fromLiteral(it.get<LoadImmediate>()->getImmediate().integer).isGenericSetup())
			{
				return it;
			}
			else if(it.has<MutexLock>() && it.get<MutexLock>()->locksMutex())
				//only search up to the previous mutex write
				break;
			it.previousInBlock();
		}
	}
	else
	{
		//for VPM reads, the generic setups is located after the address write
		while(!it.isEndOfBlock())
		{
			if(it.has<LoadImmediate>() && it->writesRegister(REG_VPM_IN_SETUP) && VPRSetup::fromLiteral(it.get<LoadImmediate>()->getImmediate().integer).isGenericSetup())
			{
				return it;
			}
			else if(it.get() && it->writesRegister(REG_MUTEX))
				//only search  to the next mutex release
				break;
			it.nextInBlock();
		}
	}
	return end;
}

struct VPMAccessGroup
{
	bool isVPMWrite;
	DataType groupType;
	RandomAccessList<InstructionWalker> dmaSetups;
	RandomAccessList<InstructionWalker> genericSetups;
	RandomAccessList<InstructionWalker> addressWrites;
};

static InstructionWalker findGroupOfVPMAccess(VPM& vpm, InstructionWalker start, const InstructionWalker end, VPMAccessGroup& group)
{
	Optional<Value> baseAddress = NO_VALUE;
	int64_t nextOffset = -1;
	group.groupType = TYPE_UNKNOWN;
	group.dmaSetups.clear();
	group.genericSetups.clear();
	group.addressWrites.clear();
	group.dmaSetups.reserve(64);
	group.dmaSetups.reserve(64);
	group.addressWrites.reserve(64);

	//FIXME to not build too large critical sections, only combine, if the resulting critical section:
	//1) is not too large: either in total numbers of instructions or in ratio instructions / VPW writes, since we save a few cycles per write (incl. delay for wait DMA)

	auto it = start;
	for(; !it.isEndOfBlock() && it != end; it.nextInBlock())
	{
		if(it.get() == nullptr)
			continue;

		if(it.has<MemoryBarrier>())
			//memory barriers end groups, also don't check this barrier again
			return it.nextInBlock();
		if(it.has<SemaphoreAdjustment>())
			//semaphore accesses end groups, also don't check this instruction again
			return it.nextInBlock();

		if(!(it->writesRegister(REG_VPM_IN_ADDR) || it->writesRegister(REG_VPM_OUT_ADDR)))
			//for simplicity, we only check for VPM addresses and find all other instructions relative to it
			continue;
		if(!it.has<MoveOperation>())
			throw CompilationError(CompilationStep::OPTIMIZER, "Setting VPM address with non-move is not supported", it->to_string());
		const auto baseAndOffset = findBaseAndOffset(it.get<MoveOperation>()->getSource());
		const bool isVPMWrite = it->writesRegister(REG_VPM_OUT_ADDR);
		logging::debug() << "Found base address " << baseAndOffset.base.to_string() << " with offset " << std::to_string(baseAndOffset.offset.orElse(-1L)) << " for " << (isVPMWrite ? "writing into" : "reading from") << " memory" << logging::endl;

		if(!baseAndOffset.base.hasValue)
			//this address-write could not be fixed to a base and an offset
			//skip this address write for the next check
			return it.nextInBlock();
		if(baseAndOffset.base.hasValue && baseAndOffset.base.get().hasType(ValueType::LOCAL) && baseAndOffset.base.get().local->is<Parameter>() && has_flag(baseAndOffset.base.get().local->as<Parameter>()->decorations, ParameterDecorations::VOLATILE))
			//address points to a volatile parameter, which explicitly forbids combining reads/writes
			//skip this address write for the next check
			return it.nextInBlock();

		//check if this address is consecutive to the previous one (if any)
		if(baseAddress.hasValue)
		{
			if(baseAndOffset.base.hasValue && baseAddress.get() != baseAndOffset.base.get())
				//a group exists, but the base addresses don't match
				break;
			if(!baseAndOffset.offset.hasValue || baseAndOffset.offset.get() != nextOffset)
				//a group exists, but the offsets do not match
				break;
		}

		//check if the access mode (read/write) is the same as for the group
		if(baseAddress.hasValue && group.isVPMWrite != isVPMWrite)
			break;

		auto genericSetup = findGenericSetup(it, end, isVPMWrite);
		auto dmaSetup = findDMASetup(it, end, isVPMWrite);

		//check if the VPM and DMA configurations match with the previous one
		if(baseAddress.hasValue)
		{
			if(genericSetup == end || dmaSetup == end)
				//either there are no setups for this VPM access, or they are not loaded from literals (e.g. dynamic setup)
				break;
			if(!genericSetup.has<LoadImmediate>() || genericSetup.get<LoadImmediate>()->getImmediate().integer != group.genericSetups.at(0).get<LoadImmediate>()->getImmediate().integer)
				//generic setups do not match
				break;
			if(!dmaSetup.has<LoadImmediate>() || dmaSetup.get<LoadImmediate>()->getImmediate().integer != group.dmaSetups.at(0).get<LoadImmediate>()->getImmediate().integer)
				//DMA setups do not match
				break;
		}

		//check for complex types
		DataType elementType = baseAndOffset.base.get().type.isPointerType() ? baseAndOffset.base.get().type.getPointerType().get()->elementType : baseAndOffset.base.get().type;
		elementType = elementType.getArrayType().hasValue ? elementType.getArrayType().get()->elementType : elementType;
		if(elementType.complexType)
			//XXX for now, skip combining any access to complex types (here: only struct, image)
			//don't check this read/write again
			return it.nextInBlock();

		//all matches so far, add to group (or create a new one)
		group.isVPMWrite = isVPMWrite;
		group.groupType = baseAndOffset.base.get().type;
		baseAddress = baseAndOffset.base.get();
		group.addressWrites.push_back(it);
		group.dmaSetups.push_back(dmaSetup);
		group.genericSetups.push_back(genericSetup);
		nextOffset = baseAndOffset.offset.orElse(-1L) + 1;

		if(group.isVPMWrite && group.addressWrites.size() >= vpm.getMaxCacheVectors(elementType, true))
		{
			//since the current address write might be removed, skip to next instruction
			//TODO could the following instruction(s) be removed too?? (See beneath)
			return it.nextInBlock();
		}
		if(!group.isVPMWrite && group.addressWrites.size() >= vpm.getMaxCacheVectors(elementType, false))
		{
			//since the current address write might be removed, skip to next instruction
			//also need to skip the consecutive DMA wait and VPM setup
			do {
				it.nextInBlock();
			}
			while(!it.isEndOfBlock() && it.get() != nullptr && it->hasValueType(ValueType::REGISTER));
			return it;
		}
	}
	//end group, but do check this instruction again
	return it;
}

static void groupVPMWrites(VPM& vpm, VPMAccessGroup& group)
{
	if(group.genericSetups.size() != group.addressWrites.size() || group.genericSetups.size() != group.dmaSetups.size())
			throw CompilationError(CompilationStep::OPTIMIZER, "Number of instructions do not match for combining VPR reads!");
	if(group.addressWrites.size() <= 1)
		return;
	logging::debug() << "Combining " << group.addressWrites.size() << " writes to consecutive memory into one DMA write... " << logging::endl;

	//1. Update DMA setup to the number of rows written
	{
		VPWSetupWrapper dmaSetupValue(group.dmaSetups.at(0).get<LoadImmediate>());
		dmaSetupValue.dmaSetup.setUnits(group.addressWrites.size());
	}
	std::size_t numRemoved = 0;
	vpm.updateScratchSize(group.addressWrites.size() * group.groupType.getElementType().getPhysicalWidth());

	//2. Remove all but the first generic and DMA setups
	for(std::size_t i = 1; i < group.genericSetups.size(); ++i)
	{
		group.genericSetups.at(i).erase();
		const LoadImmediate* strideSetup = group.dmaSetups.at(i).copy().nextInBlock().get<LoadImmediate>();
		if(strideSetup == nullptr || !strideSetup->writesRegister(REG_VPM_OUT_SETUP) || !VPWSetup::fromLiteral(strideSetup->getImmediate().integer).isStrideSetup())
			throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find VPW DMA stride setup for DMA setup", group.dmaSetups.at(i)->to_string());
		group.dmaSetups.at(i).copy().nextInBlock().erase();
		group.dmaSetups.at(i).erase();
		numRemoved += 3;
	}

	//3. remove all but the last address writes (and the following DMA waits), update the last write to write the first address written to
	group.addressWrites.back().get<MoveOperation>()->setSource(group.addressWrites.at(0).get<MoveOperation>()->getSource());
	for(std::size_t i = 0; i < group.addressWrites.size() - 1; ++i)
	{
		if(!group.addressWrites.at(i).copy().nextInBlock()->readsRegister(group.isVPMWrite ? REG_VPM_OUT_WAIT : REG_VPM_IN_WAIT))
			throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find VPW wait for address write", group.addressWrites.at(i)->to_string());
		group.addressWrites.at(i).copy().nextInBlock().erase();
		group.addressWrites.at(i).erase();
		numRemoved += 2;
	}

	//4. remove all Mutex acquires and releases between the first and the last write, so memory consistency is restored
	auto it = group.dmaSetups.front();
	while(!it.isEndOfBlock() && it != group.addressWrites.back())
	{
		if(it.get() && it->writesRegister(REG_MUTEX))
		{
			it = it.erase();
			++numRemoved;
		}
		else if(it.get() && it->readsRegister(REG_MUTEX))
		{
			it = it.erase();
			++numRemoved;
		}
		else
			it.nextInBlock();
	}

	logging::debug() << "Removed " << numRemoved << " instructions by combining VPW writes" << logging::endl;
}

static void groupVPMReads(VPM& vpm, VPMAccessGroup& group)
{
	if(group.genericSetups.size() != group.addressWrites.size() || group.genericSetups.size() != group.dmaSetups.size())
		throw CompilationError(CompilationStep::OPTIMIZER, "Number of instructions do not match for combining VPR reads!");

	if(group.genericSetups.size() <= 1)
		return;
	logging::debug() << "Combining " << group.genericSetups.size() << " reads of consecutive memory into one DMA read... " << logging::endl;

	//1. Update DMA setup to the number of rows read
	{
		VPRSetupWrapper dmaSetupValue(group.dmaSetups.at(0).get<LoadImmediate>());
		dmaSetupValue.dmaSetup.setNumberRows(group.genericSetups.size() % 16);
		vpm.updateScratchSize(group.genericSetups.size() * group.groupType.getElementType().getPhysicalWidth());
	}
	std::size_t numRemoved = 0;

	//1.1 Update generic Setup to the number of rows read
	{
		VPRSetupWrapper genericSetup(group.genericSetups.at(0).get<LoadImmediate>());
		genericSetup.genericSetup.setNumber(group.genericSetups.size() % 16);
	}

	//2. Remove all but the first generic and DMA setups
	for(std::size_t i = 1; i < group.genericSetups.size(); ++i)
	{
		group.genericSetups.at(i).erase();
		const LoadImmediate* strideSetup = group.dmaSetups.at(i).copy().nextInBlock().get<LoadImmediate>();
		if(strideSetup == nullptr || !strideSetup->writesRegister(REG_VPM_IN_SETUP) || !VPRSetup::fromLiteral(strideSetup->getImmediate().integer).isStrideSetup())
			throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find VPR DMA stride setup for DMA setup", group.dmaSetups.at(i)->to_string());
		group.dmaSetups.at(i).copy().nextInBlock().erase();
		group.dmaSetups.at(i).erase();
		numRemoved += 2;
	}

	//3. remove all Mutex acquires and releases between the first and the last write, so memory consistency is restored
	auto it = group.addressWrites.front();
	while(!it.isEndOfBlock() && it != group.addressWrites.back())
	{
		if(it.get() && it->writesRegister(REG_MUTEX))
		{
			it = it.erase();
			++numRemoved;
		}
		else if(it.get() && it->readsRegister(REG_MUTEX))
		{
			it = it.erase();
			++numRemoved;
		}
		else
			it.nextInBlock();
	}

	//4. remove all but the first address writes (and the following DMA writes)
	for(std::size_t i = 1; i < group.addressWrites.size(); ++i)
	{
		if(!group.addressWrites.at(i).copy().nextInBlock()->readsRegister(group.isVPMWrite ? REG_VPM_OUT_WAIT : REG_VPM_IN_WAIT))
			throw CompilationError(CompilationStep::OPTIMIZER, "Failed to find VPR wait for address write", group.addressWrites.at(i)->to_string());
		group.addressWrites.at(i).copy().nextInBlock().erase();
		group.addressWrites.at(i).erase();
		numRemoved += 2;
	}

	logging::debug() << "Removed " << numRemoved << " instructions by combining VPR reads" << logging::endl;
}

void optimizations::combineVPMAccess(const Module& module, Method& method, const Configuration& config)
{
	//combine configurations of VPM (VPW/VPR) which have the same values

	//TODO for now, this cannot handle RAM->VPM, VPM->RAM only access as well as VPM->QPU or QPU->VPM

	// run within all basic blocks
	for(BasicBlock& block : method.getBasicBlocks())
	{
		auto it = block.begin();
		while(!it.isEndOfBlock())
		{
			VPMAccessGroup group;
			it = findGroupOfVPMAccess(*method.vpm.get(), it, block.end(), group);
			if(group.addressWrites.size() > 1)
			{
				if(group.isVPMWrite)
					groupVPMWrites(*method.vpm.get(), group);
				else
					groupVPMReads(*method.vpm.get(), group);
			}
		}
	}

	// clean up empty instructions
	method.cleanEmptyInstructions();
	PROFILE_COUNTER(9010, "Scratch memory size", method.vpm->getScratchArea().size);
}

InstructionWalker optimizations::accessGlobalData(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
	/*
	 * Map pointer to global data to the start-of-global-data parameter
	 *  plus the offset of the global data
	 */
	for(std::size_t i = 0; i < it->getArguments().size(); ++i)
	{
		const auto& arg = it->getArgument(i).get();
		if(arg.hasType(ValueType::LOCAL) && arg.type.isPointerType() && arg.local->is<Global>())
		{
			const Optional<unsigned int> globalOffset = module.getGlobalDataOffset(arg.local);
			if(globalOffset.hasValue)
			{
				logging::debug() << "Replacing access to global data: " << it->to_string() << logging::endl;
				Value tmp = UNDEFINED_VALUE;
				if(globalOffset.get() == 0)
				{
					tmp = method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_DATA_ADDRESS)->createReference();
				}
				else
				{
					//emplace calculation of global-data pointer and replace argument
					tmp = method.addNewLocal(TYPE_INT32, "%global_data_offset");
					it.emplace(new intermediate::Operation(OP_ADD, tmp, method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_DATA_ADDRESS)->createReference(), Value(Literal(static_cast<uint64_t>(globalOffset)), TYPE_INT32)));
					it.nextInBlock();
				}
				it->setArgument(i, tmp);
			}
		}
	}
	return it;
}

void optimizations::spillLocals(const Module& module, Method& method, const Configuration& config)
{
	static const std::size_t MINIMUM_THRESHOLD = 128; /* TODO some better limit */
	//TODO need to know how much of the VPM is still free (need per-kernel VPM object)
	//also need to heed not to write/read into/from VPM from within a block of DMA reads/writes
	//XXX or revert: run this before #combineVPMAccess and reserve as much VPM as required (dynamically, how? or first determine size, then spill)
	//and use the remainder for #combineVPMAccess
	//also need to regard writing into VPM (without DMA) for #combineVPMAccess, so the VPM configurations do not conflict

	/*
	 * 1. find all candidate locals for spilling:
	 * - no labels (since they are never mapped to registers)
	 * - only one write (for now, for easier handling)
	 * - not used only locally within a minimum range, since those locals are more likely to be mapped to registers
	 */
	//tracks the locals and their writing instructions
	FastMap<const Local*, InstructionWalker> spillingCandidates;
	for(const auto& pair : method.readLocals())
	{
		if(pair.second.type == TYPE_LABEL)
			continue;
		//XXX for now, only select locals which are written just once
		//or maybe never (not yet), e.g. for hidden parameter
		//or written several times but read only once
		//TODO also include explicit parameters
		auto numWrites = pair.second.getUsers(LocalUser::Type::WRITER).size();
		auto numReads = pair.second.getUsers(LocalUser::Type::READER).size();
		if((numWrites <= 1 && numReads > 0) || (numWrites >= 1 && numReads == 1))
		{
			spillingCandidates.emplace(&pair.second, InstructionWalker{});
		}
	}

	InstructionWalker it = method.walkAllInstructions();
	//skip all leading empty basic blocks
	while(!it.isEndOfMethod() && it.has<intermediate::BranchLabel>())
		it.nextInMethod();
	auto candIt = spillingCandidates.begin();
	while(!it.isEndOfMethod() && candIt != spillingCandidates.end())
	{
		//check if only read the first X instructions (from the start of the kernel), e.g. for (hidden) parameter, where the next check fails,
		//since they are not yet written anywhere
		if(method.isLocallyLimited(it, candIt->first, MINIMUM_THRESHOLD))
			candIt = spillingCandidates.erase(candIt);
		else
			++candIt;
	}
	while(!it.isEndOfMethod() && !spillingCandidates.empty())
	{
		//TODO if at some point all Basic block have references to their used locals, remove all locals which are used just in one basic block instead of this logic??
		if(it->hasValueType(ValueType::LOCAL) && spillingCandidates.find(it->getOutput().get().local) != spillingCandidates.end())
		{
			if(method.isLocallyLimited(it, it->getOutput().get().local, MINIMUM_THRESHOLD))
				spillingCandidates.erase(it->getOutput().get().local);
			else
				spillingCandidates.at(it->getOutput().get().local) = it;
		}
		it.nextInMethod();
	}

	for(const auto& pair : spillingCandidates)
	{
		logging::debug() << "Spilling candidate: " << pair.first->to_string() << " (" << pair.first->getUsers(LocalUser::Type::WRITER).size() << " writes, " << pair.first->getUsers(LocalUser::Type::READER).size() << " reads)" << logging::endl;
	}

	//TODO do not preemptively spill, only on register conflicts. Which case??
}

static InstructionWalker accessStackAllocations(const Module& module, Method& method, InstructionWalker it)
{
	const unsigned int stackBaseOffset = method.getStackBaseOffset();
	const std::size_t maximumStackSize = method.calculateStackSize();

	for(std::size_t i = 0; i < it->getArguments().size(); ++i)
	{
		const Value arg = it->getArgument(i).get();
		if(arg.hasType(ValueType::LOCAL) && arg.type.isPointerType() && arg.local->is<StackAllocation>())
		{
			if(it.get<intermediate::LifetimeBoundary>() != nullptr)
			{
				logging::debug() << "Dropping life-time instruction for stack-allocation: " << arg.to_string() << logging::endl;
				it.erase();
				//to not skip the next instruction
				it.previousInBlock();
			}
			else
			{
				/*
				 * Stack allocations are located in the binary data after the global data.
				 *
				 *
				 * To reduce the number of calculations, all stack allocations are grouped by their QPU, so the layout is as follows:
				 *
				 * | "Stack" of QPU0 | "Stack" of QPU1 | ...
				 *
				 * The offset of a single stack allocation can be calculated as:
				 * global-data address + global-data size + (QPU-ID * stack allocations maximum size) + offset of stack allocation
				 * = global-data address + (QPU-ID * stack allocations maximum size) + (global-data size + offset of stack allocation)
				 */
				//TODO to save instructions, could pre-calculate 'global-data address + global-data size + (QPU-ID * stack allocations maximum size)' once, if any stack-allocation exists ??

				logging::debug() << "Replacing access to stack allocated data: " << it->to_string() << logging::endl;
				const Value qpuOffset = method.addNewLocal(TYPE_INT32, "%stack_offset");
				const Value addrTemp = method.addNewLocal(arg.type, "%stack_addr");
				const Value finalAddr = method.addNewLocal(arg.type, "%stack_addr");

				it.emplace(new Operation(OP_MUL24, qpuOffset, Value(REG_QPU_NUMBER, TYPE_INT8), Value(Literal(static_cast<uint64_t>(maximumStackSize)), TYPE_INT32)));
				it.nextInBlock();
				it.emplace(new Operation(OP_ADD, addrTemp, qpuOffset, method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_DATA_ADDRESS)->createReference()));
				it.nextInBlock();
				it.emplace(new Operation(OP_ADD, finalAddr, addrTemp, Value(Literal(static_cast<uint64_t>(arg.local->as<StackAllocation>()->offset + stackBaseOffset)), TYPE_INT32)));
				it.nextInBlock();
				it->setArgument(i, finalAddr);
			}
		}
	}
	return it.nextInMethod();
}

void optimizations::resolveStackAllocations(const Module& module, Method& method, const Configuration& config)
{
	//1. calculate the offsets from the start of one QPU's "stack", heed alignment!
	method.calculateStackOffsets();

	//2.remove the life-time instructions
	//3. map the addresses to offsets from global-data pointer (see #accessGlobalData)
	InstructionWalker it = method.walkAllInstructions();
	while(!it.isEndOfMethod())
	{
		it = accessStackAllocations(module, method, it);
	}
}
