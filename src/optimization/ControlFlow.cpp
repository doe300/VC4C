/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ControlFlow.h"

#include "LiteralValues.h"
#include "../ControlFlowGraph.h"
#include "../intermediate/TypeConversions.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <algorithm>
#include <cmath>

using namespace vc4c;
using namespace vc4c::optimizations;

static FastSet<Local*> findLoopIterations(const ControlFlowLoop& loop, const DataDependencyGraph& dependencyGraph)
{
	FastSet<Local*> innerDependencies;
	FastSet<Local*> outerDependencies;
	for(auto& node : loop)
	{
		//not all basic blocks have an entry in the dependency graph (e.g. if they have no dependency)
		auto dependencyNode = dependencyGraph.find(node->key);
		if(dependencyNode != dependencyGraph.end())
		{
			for(auto& neighbor : dependencyNode->second.getNeighbors())
			{
				//check if this basic block has a local dependent on at least two phi-nodes
				for(auto& dependency : neighbor.second)
				{
					if(has_flag(dependency.second, add_flag(DataDependencyType::PHI, DataDependencyType::FLOW)))
					{
						if(std::find_if(loop.begin(), loop.end(), [&neighbor](const CFGNode* node) ->bool { return node->key == neighbor.first->key;}) != loop.end())
							//... one of which lies within the loop
							innerDependencies.emplace(dependency.first);
						else
							//... and the other outside of it
							outerDependencies.emplace(dependency.first);
					}
				}
			}
		}
	}

	FastSet<Local*> intersection;
	std::set_intersection(innerDependencies.begin(), innerDependencies.end(), outerDependencies.begin(), outerDependencies.end(), std::inserter(intersection, intersection.begin()));

	if(intersection.empty())
	{
		logging::debug() << "Failed to find loop iteration variable for loop" << logging::endl;
	}

	return intersection;
}

static Optional<InstructionWalker> findInLoop(const intermediate::IntermediateInstruction* inst, const ControlFlowLoop& loop)
{
	for(const CFGNode* node : loop)
	{
		auto it = node->key->findWalkerForInstruction(inst, node->key->end());
		if(it)
			return it;
	}
	return Optional<InstructionWalker>(false, InstructionWalker());
}

enum class StepKind
{
	//step-kind is not known
	UNKNOWN,
	//integer addition with constant factor, e.g. step of +1. Default for more for-range loops
	ADD_CONSTANT,
	//integer subtraction with constant factor e.g. step of -1. Default for loops counting backwards
	SUB_CONSTANT,
	//integer multiplication with constant factor
	MUL_CONSTANT
};

struct LoopControl
{
	//the initial value for the loop iteration variable
	intermediate::IntermediateInstruction* initialization = nullptr;
	//the value compared with to terminate the loop
	Value terminatingValue = UNDEFINED_VALUE;
	//the local containing the current iteration-variable
	Local* iterationVariable;
	//the operation to change the iteration-variable
	Optional<InstructionWalker> iterationStep;
	//the kind of step performed
	StepKind stepKind = StepKind::UNKNOWN;
	//the comparison to check for continue/end loop
	Optional<InstructionWalker> comparisonInstruction;
	//the branch-instruction to continue the loop
	Optional<InstructionWalker> repetitionJump;
	//the comparison function to abort the loop
	std::string comparison;
	//the vectorization-factor used
	unsigned vectorizationFactor;

	void determineStepKind(const OpCode& code)
	{
		if(code == OP_ADD)
			stepKind = StepKind::ADD_CONSTANT;
		else if(code == OP_SUB)
			stepKind = StepKind::SUB_CONSTANT;
		else if(code == OP_MUL24)
			stepKind = StepKind::MUL_CONSTANT;
	}

	OpCode getStepOperation() const
	{
		switch(stepKind)
		{
			case StepKind::ADD_CONSTANT:
				return OP_ADD;
			case StepKind::SUB_CONSTANT:
				return OP_SUB;
			case StepKind::MUL_CONSTANT:
				return OP_MUL24;
		}
		if(!iterationStep.ifPresent([](const InstructionWalker& it) -> bool {return it.has<const intermediate::Operation>();}))
			return OP_NOP;
		return iterationStep->get<const intermediate::Operation>()->op;
	}

	Optional<Literal> getStep() const
	{
		if(!iterationStep.ifPresent([](const InstructionWalker& it) -> bool {return it.has<const intermediate::Operation>();}))
			return Optional<Literal>(false, Literal(false));
		const intermediate::Operation* op = iterationStep->get<const intermediate::Operation>();
		if(op->getArguments().size() != 2)
			return Optional<Literal>(false, Literal(false));
		if(op->getArgument(0).ifPresent(toFunction(&Value::isLiteralValue)))
			return op->getArgument(0)->getLiteralValue();
		return op->getArgument(1)->getLiteralValue();
	}

	int64_t countIterations(int64_t initial, int64_t limit, int64_t step) const
	{
		switch(stepKind)
		{
			case StepKind::ADD_CONSTANT:
				// iterations = (end - start) / step
				return (limit - initial) / step;
			case StepKind::SUB_CONSTANT:
				// iterations = (start - end) / step
				return (initial - limit) / step;
			case StepKind::MUL_CONSTANT:
				// limit = (start * step) ^ iterations -> iterations = log(start * step) / log(limit)
				return static_cast<int64_t>(std::log(initial * step) / std::log(limit));
			default:
				throw CompilationError(CompilationStep::OPTIMIZER, "Invalid step type!");
		}
	}

	bool operator==(const LoopControl& other) const
	{
		return iterationVariable == other.iterationVariable;
	}
};

struct LoopControlHash : public std::hash<Local*>
{
	size_t operator()(const LoopControl& val) const noexcept
	{
		return std::hash<Local*>::operator()(val.iterationVariable);
	}
};

static LoopControl extractLoopControl(const ControlFlowLoop& loop, const DataDependencyGraph& dependencyGraph)
{
	FastSet<LoopControl, LoopControlHash> availableLoopControls;

	for(Local* local : findLoopIterations(loop, dependencyGraph))
	{
		if(local == nullptr)
			continue;

		logging::debug() << "Loop iteration variable candidate: " << local->to_string(false) << logging::endl;

		LoopControl loopControl;
		loopControl.iterationVariable = local;

		for(const auto& pair : local->getUsers())
		{
			const intermediate::IntermediateInstruction* inst = dynamic_cast<const intermediate::IntermediateInstruction*>(pair.first);
			Optional<InstructionWalker> it = findInLoop(inst, loop);
			//"lower" bound: the initial setting of the value outside of the loop
			if(pair.second.writesLocal() && has_flag(inst->decoration, intermediate::InstructionDecorations::PHI_NODE) && !it)
			{
				auto tmp = inst->precalculate(4);
				if(tmp)
				{
					logging::debug() << "Found lower bound: " << tmp->to_string() << logging::endl;
					loopControl.initialization = const_cast<intermediate::IntermediateInstruction*>(inst);
				}
			}
			//iteration step: the instruction inside the loop where the iteration variable is changed
			//XXX this currently only looks for single operations with immediate values (e.g. +1,-1)
			else if(pair.second.readsLocal() && it)
			{
				if(it->has<intermediate::Operation>() && it.value()->getArguments().size() == 2 && it.value()->readsLiteral() &&
						//TODO could here more simply check against output being the local the iteration variable is set to (in the phi-node inside the loop)
						it.value()->getOutput().ifPresent([](const Value& val) -> bool { return val.hasType(ValueType::LOCAL) && std::any_of(val.local->getUsers().begin(), val.local->getUsers().end(), [](const std::pair<const LocalUser*, LocalUse>& pair) -> bool {return has_flag(dynamic_cast<const intermediate::IntermediateInstruction*>(pair.first)->decoration, intermediate::InstructionDecorations::PHI_NODE);});}))
				{
					logging::debug() << "Found iteration instruction: " << it.value()->to_string() << logging::endl;
					loopControl.iterationStep = it;
					loopControl.determineStepKind(it->get<intermediate::Operation>()->op);
				}
				//for use-with immediate local, TODO need better checking
				else if(it->has<intermediate::MoveOperation>() && it.value()->hasValueType(ValueType::LOCAL))
				{
					//second-level checking for loop iteration step (e.g. if loop variable is copied for use-with-immediate)
					const Local* stepLocal = it.value()->getOutput()->local;
					for(const auto& pair : stepLocal->getUsers())
					{
						const intermediate::IntermediateInstruction* inst = dynamic_cast<const intermediate::IntermediateInstruction*>(pair.first);
						Optional<InstructionWalker> it = findInLoop(inst, loop);
						//iteration step: the instruction inside the loop where the iteration variable is changed
						if(pair.second.readsLocal() && it)
						{
							if(it->has<intermediate::Operation>() && it.value()->getArguments().size() == 2 && it.value()->readsLiteral() &&
									it.value()->getOutput().ifPresent([](const Value& val) -> bool { return val.hasType(ValueType::LOCAL) && std::any_of(val.local->getUsers().begin(), val.local->getUsers().end(), [](const std::pair<const LocalUser*, LocalUse>& pair) -> bool {return has_flag(dynamic_cast<const intermediate::IntermediateInstruction*>(pair.first)->decoration, intermediate::InstructionDecorations::PHI_NODE);});}))
							{
								logging::debug() << "Found iteration instruction: " << it.value()->to_string() << logging::endl;
								loopControl.iterationStep = it;
								loopControl.determineStepKind(it->get<intermediate::Operation>()->op);
							}
						}
					};
				}
			}
		};

		for(const auto& neighbor : loop.front()->getNeighbors())
		{
			if(neighbor.second.isForwardRelation() && !neighbor.second.isImplicit)
			{
				if(std::find(loop.begin(), loop.end(), neighbor.first) != loop.end())
				{
					loopControl.repetitionJump = neighbor.second.predecessor;
					logging::debug() << "Found loop repetition branch: " << loopControl.repetitionJump.value()->to_string() << logging::endl;
				}
			}
		}

		//"upper" bound: the value being checked against inside the loop
		if(loopControl.repetitionJump && loopControl.iterationStep)
		{
			const Value repeatCond = loopControl.repetitionJump->get<intermediate::Branch>()->getCondition();
			const Value iterationStep = loopControl.iterationStep.value()->getOutput().value();

			//check for either local (iteration-variable or iteration-step result) whether they are used in the condition on which the loop is repeated
			//and select the literal used together with in this condition

			//simple case, there exists an instruction, directly mapping the values
			auto userIt = std::find_if(iterationStep.local->getUsers().begin(), iterationStep.local->getUsers().end(), [&repeatCond](const std::pair<const LocalUser*, LocalUse>& pair) -> bool { return pair.first->writesLocal(repeatCond.local);});
			if(userIt == iterationStep.local->getUsers().end())
			{
				//"default" case, the iteration-variable is compared to something and the result of this comparison is used to branch
				//e.g. "- = xor <iteration-variable>, <upper-bound> (setf)"
				userIt = std::find_if(iterationStep.local->getUsers().begin(), iterationStep.local->getUsers().end(), [](const std::pair<const LocalUser*, LocalUse>& pair) -> bool { return dynamic_cast<const intermediate::IntermediateInstruction*>(pair.first)->setFlags == SetFlag::SET_FLAGS;});
				if(userIt != iterationStep.local->getUsers().end())
				{
					//TODO need to check, whether the comparison result is the one used for branching
					//if not, set userIt to loop.end()
					auto instIt = findInLoop(dynamic_cast<const intermediate::IntermediateInstruction*>(userIt->first), loop);
					loopControl.comparisonInstruction = instIt;
					logging::debug() << "Found loop continue condition: " << loopControl.comparisonInstruction.value()->to_string() << logging::endl;
				}
				else
				{
					//TODO more complex case, the iteration-variable is used in an operation, whose result is compared to something and that result is used to branch
					//e.g "<tmp> = max <iteration-variable>, <upper-bound>; - = xor <tmp>, <upper-bound> (setf)"
				}
			}

			if(userIt != iterationStep.local->getUsers().end())
			{
				//userIt converts the loop-variable to the condition. The comparison value is the upper bound
				const intermediate::IntermediateInstruction* inst = dynamic_cast<const intermediate::IntermediateInstruction*>(userIt->first);
				if(inst->getArguments().size() != 2)
				{
					//TODO error
				}
				if(inst->getArgument(0)->hasLocal(iterationStep.local))
					loopControl.terminatingValue = inst->getArgument(1).value();
				else
					loopControl.terminatingValue = inst->getArgument(0).value();
				if(loopControl.terminatingValue.hasType(ValueType::LOCAL) && loopControl.terminatingValue.local->getSingleWriter() != nullptr)
				{
					auto tmp = dynamic_cast<const intermediate::IntermediateInstruction*>(loopControl.terminatingValue.local->getSingleWriter())->precalculate(4);
					if(tmp)
						loopControl.terminatingValue = tmp.value();
				}
				logging::debug() << "Found upper bound: " << loopControl.terminatingValue.to_string() << logging::endl;

				//determine type of comparison
				const intermediate::Operation* comparison = dynamic_cast<const intermediate::Operation*>(inst);
				if(comparison != nullptr)
				{
					bool isEqualityComparison = comparison->op == OP_XOR || comparison->opCode == OP_XOR.name;
					bool isLessThenComparison = comparison->op == OP_SUB || comparison->opCode == OP_SUB.name || comparison->op == OP_FSUB || comparison->opCode == OP_FSUB.name;
					//TODO distinguish ==/!=, </>/<=/>= !! The setting of flags as well as the reading (for branch) can be for positive/negative flags
					//XXX need to distinguish between continuation condition and cancel condition
					if(isEqualityComparison)
						loopControl.comparison = intermediate::COMP_EQ;
					if(isLessThenComparison)
						loopControl.comparison = "lt";
					if(!loopControl.comparison.empty())
						logging::debug() << "Found comparison type: " << loopControl.comparison << logging::endl;
				}

			}
		}

		if(loopControl.initialization && !loopControl.terminatingValue.isUndefined() && loopControl.iterationStep && loopControl.repetitionJump)
		{
			availableLoopControls.emplace(loopControl);
		}
		else
			logging::debug() << "Failed to find all bounds and step for iteration variable, skipping: " << loopControl.iterationVariable->name << logging::endl;
	}

	if(availableLoopControls.empty())
		return LoopControl{};
	else if(availableLoopControls.size() == 1)
		return *availableLoopControls.begin();

	throw CompilationError(CompilationStep::OPTIMIZER, "Selecting from multiple iteration variables is not supported yet!");
}

/*
 * For now uses a very simple algorithm:
 * - checks the maximum vector-width used inside the loop
 * - tries to find an optimal factor, which never exceeds 16 elements and divides the number of iterations equally
 */
static Optional<unsigned> determineVectorizationFactor(const ControlFlowLoop& loop, const LoopControl& loopControl)
{
	unsigned char maxTypeWidth = 1;
	InstructionWalker it = loop.front()->key->begin();
	while(!it.isEndOfMethod() && it != loop.back()->key->end())
	{
		if(it->getOutput())
		{
			//TODO is this check enough?
			maxTypeWidth = std::max(maxTypeWidth, it->getOutput()->type.num);
		}
		it.nextInMethod();
	}

	logging::debug() << "Found maximum used vector-width of " << static_cast<unsigned>(maxTypeWidth) << " elements" << logging::endl;

	const Literal initial = loopControl.initialization->precalculate(4)->getLiteralValue().value();
	const Literal end = loopControl.terminatingValue.getLiteralValue().value();
	//the number of iterations from the bounds depends on the iteration operation
	int64_t iterations = loopControl.countIterations(initial.integer, end.integer, loopControl.getStep()->integer);
	logging::debug() << "Determined iteration count of " << iterations << logging::endl;

	//find the biggest factor fitting into 16 SIMD-elements
	unsigned factor = 16 / maxTypeWidth;
	while(factor > 0)
	{
		//TODO factors not in [1,2,3,4,8,16] possible?? Should be from hardware-specification side
		if((iterations % static_cast<int64_t>(factor)) == 0)
			break;
		--factor;
	}
	logging::debug() << "Determined possible vectorization-factor of " << factor << logging::endl;
	return factor;
}

/*
 * On the cost-side, we have (as increments):
 * - instructions inserted to construct vectors from scalars
 * - additional delay for writing larger vectors through VPM
 * - memory address is read and written from within loop -> abort
 * - vector rotations -> for now abort
 *
 * On the benefit-side, we have (as factors):
 * - the iterations saved (times the number of instructions in an iteration)
 */
static int calculateCostsVsBenefits(const ControlFlowLoop& loop, const LoopControl& loopControl, const DataDependencyGraph& dependencyGraph)
{
	int costs = 0;

	FastSet<const Local*> readAddresses;
	FastSet<const Local*> writtenAddresses;

	InstructionWalker it = loop.front()->key->begin();
	while(!it.isEndOfMethod() && it != loop.back()->key->end())
	{
		if(it.has())
		{
			if(it->getOutput().ifPresent([](const Value& out) -> bool { return out.hasRegister(REG_VPM_IN_ADDR) || out.hasRegister(REG_TMU0_ADDRESS) || out.hasRegister(REG_TMU1_ADDRESS);}))
			{
				for(const Value& arg : it->getArguments())
				{
					if(arg.hasType(ValueType::LOCAL))
					{
						readAddresses.emplace(arg.local);
						readAddresses.emplace(arg.local->reference.first);
					}
				}
			}
			else if(it->getOutput().ifPresent(toFunction(&Value::hasRegister, REG_VPM_OUT_ADDR)))
			{
				for(const Value& arg : it->getArguments())
				{
					if(arg.hasType(ValueType::LOCAL))
					{
						writtenAddresses.emplace(arg.local);
						writtenAddresses.emplace(arg.local->reference.first);
					}
				}
			}
			else if(it.has<intermediate::VectorRotation>())
			{
				//abort
				logging::debug() << "Cannot vectorize loops containing vector rotations: " << it->to_string() << logging::endl;
				return std::numeric_limits<int>::min();
			}
		}

		//TODO check and increase costs
		it.nextInMethod();
	}

	//constant cost - loading immediate for iteration-step for vector-width > 15 (no longer fitting into small immediate)
	if(loopControl.iterationStep.value()->getOutput()->type.num * loopControl.vectorizationFactor > 15)
		++costs;

	FastSet<const Local*> readAndWrittenAddresses;
	std::set_intersection(readAddresses.begin(), readAddresses.end(), writtenAddresses.begin(), writtenAddresses.end(), std::inserter(readAndWrittenAddresses, readAndWrittenAddresses.begin()));
	//the references could be null-pointers
	readAndWrittenAddresses.erase(nullptr);
	if(!readAndWrittenAddresses.empty())
	{
		for(const Local* local : readAndWrittenAddresses)
		{
			logging::debug() << "Cannot vectorize loops reading and writing the same memory addresses: " << local->to_string() << logging::endl;
		}
		//abort
		return std::numeric_limits<int>::min();
	}

	int numInstructions = 0;
	for(const CFGNode* node : loop)
	{
		//XXX to be exact, would need to include delays here too
		numInstructions += static_cast<int>(node->key->size());
	}
	//the number of instructions/cycles saved
	int benefits = numInstructions * static_cast<int>(loopControl.vectorizationFactor);

	logging::debug() << "Calculated an cost-vs-benefit rating of " << (benefits - costs) << " (estimated number of cycles saved, larger is better)" << logging::endl;
	return benefits - costs;
}

static void scheduleForVectorization(const Local* local, FastSet<intermediate::IntermediateInstruction*>& openInstructions, ControlFlowLoop& loop)
{
	local->forUsers(LocalUser::Type::READER, [&openInstructions, &loop](const LocalUser* user) -> void
	{
		intermediate::IntermediateInstruction* inst = dynamic_cast<intermediate::IntermediateInstruction*>(const_cast<LocalUser*>(user));
		if(!has_flag(inst->decoration, intermediate::InstructionDecorations::AUTO_VECTORIZED))
			openInstructions.emplace(inst);
		if(inst->getOutput().ifPresent([](const Value& out) -> bool { return out.hasType(ValueType::REGISTER) && (out.reg.isSpecialFunctionsUnit() || out.reg.isTextureMemoryUnit());}))
		{
			//need to add the reading of SFU/TMU too
			auto optIt = findInLoop(inst, loop);
			if(optIt)
			{
				InstructionWalker it = optIt.value().nextInBlock();
				while(!it.isEndOfBlock())
				{
					if(it->readsRegister(REG_SFU_OUT) && !has_flag(it->decoration, intermediate::InstructionDecorations::AUTO_VECTORIZED))
					{
						openInstructions.emplace(it.get());
						break;
					}

					it.nextInBlock();
				}
			}
		}
	});
}

static void vectorizeInstruction(InstructionWalker it, FastSet<intermediate::IntermediateInstruction*>& openInstructions, unsigned vectorizationFactor, ControlFlowLoop& loop)
{
	logging::debug() << "Vectorizing instruction: " << it->to_string() << logging::endl;

	//1. update types of values matching the types of their locals
	unsigned char vectorWidth = 1;
	for(auto& arg : it->getArguments())
	{
		if(arg.hasType(ValueType::LOCAL) && arg.type != arg.local->type)
		{
			scheduleForVectorization(arg.local, openInstructions, loop);
			const_cast<DataType&>(arg.type).num = arg.local->type.num;
			vectorWidth = std::max(vectorWidth, arg.type.num);
		}
		else if(arg.hasType(ValueType::REGISTER))
		{
			//TODO correct?? This is at least required for reading from TMU
			vectorWidth = vectorizationFactor;
		}
	}

	//2. depending on operation performed, update type of output
	if(it->getOutput() && (it.has<intermediate::Operation>() || it.has<intermediate::MoveOperation>()))
	{
		//TODO vector-rotations need special handling?!
		Value& out = const_cast<Value&>(it->getOutput().value());
		out.type.num = vectorWidth;
		if(out.hasType(ValueType::LOCAL))
		{
			const_cast<DataType&>(out.local->type).num = out.type.num;
			scheduleForVectorization(out.local, openInstructions, loop);
		}
	}

	//TODO need to adapt types of some registers/output of load, etc.?
	//TODO cosmetic errors: depending on the order of vectorization, some locals are written as vectors, but read as scalars, if the read-instruction was vectorized before the write-instruction

	// mark as already processed and remove from open-set
	it->setDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
	openInstructions.erase(it.get());
}

/*
 * Look within this mutex-block for the nearest instruction accessing the VPM in the given way
 */
static Optional<InstructionWalker> findVPMAccess(InstructionWalker pos, bool writeAccess)
{
	auto it = pos;
	while(!it.isStartOfBlock())
	{
		//only look up to the next mutex (un)lock
		if(it.has<intermediate::MutexLock>())
			break;
		//also to not accept a vectorized VPM access while this access is not vectorized (e.g. when accesses are combined), abort at writing of addresses
		if(it->writesRegister(REG_VPM_IN_ADDR) || it->writesRegister(REG_VPM_OUT_ADDR))
			break;
		if(writeAccess && it->writesRegister(REG_VPM_IO))
			return it;
		if(!writeAccess && it->readsRegister(REG_VPM_IO))
			return it;
		it.previousInBlock();
	}

	it = pos;
	while(!it.isEndOfBlock())
	{
		//only look up to the next mutex (un)lock
		if(it.has<intermediate::MutexLock>())
			break;
		//also to not accept a vectorized VPM access while this access is not vectorized (e.g. when accesses are combined), abort at writing of addresses
		if(it->writesRegister(REG_VPM_IN_ADDR) || it->writesRegister(REG_VPM_OUT_ADDR))
			break;
		if(writeAccess && it->writesRegister(REG_VPM_IO))
			return it;
		if(!writeAccess && it->readsRegister(REG_VPM_IO))
			return it;
		it.nextInBlock();
	}

	return {};
}

static std::size_t fixVPMSetups(ControlFlowLoop& loop, LoopControl& loopControl)
{
	InstructionWalker it = loop.front()->key->begin();
	std::size_t numVectorized = 0;

	while(!it.isEndOfMethod() && it != loop.back()->key->end())
	{
		if(it->writesRegister(REG_VPM_OUT_SETUP))
		{
			periphery::VPWSetupWrapper vpwSetup(it.get<intermediate::LoadImmediate>());
			auto vpmWrite = findVPMAccess(it, true);
			if(vpwSetup.isDMASetup() && vpmWrite && has_flag((*vpmWrite)->decoration, intermediate::InstructionDecorations::AUTO_VECTORIZED))
			{
				//Since this is only true for values actually vectorized, the corresponding VPM-write is checked
				vpwSetup.dmaSetup.setDepth(vpwSetup.dmaSetup.getDepth() * loopControl.vectorizationFactor);
				++numVectorized;
				it->setDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
			}
		}
		else if(it->writesRegister(REG_VPM_IN_SETUP))
		{
			periphery::VPRSetupWrapper vprSetup(it.get<intermediate::LoadImmediate>());
			auto vpmRead = findVPMAccess(it, false);
			if(vprSetup.isDMASetup() && vpmRead && has_flag((*vpmRead)->decoration, intermediate::InstructionDecorations::AUTO_VECTORIZED))
			{
				//See VPM write
				vprSetup.dmaSetup.setRowLength((vprSetup.dmaSetup.getRowLength() * loopControl.vectorizationFactor) % 16 /* 0 => 16 */);
				++numVectorized;
				it->setDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
			}
		}

		it.nextInMethod();
	}

	return numVectorized;
}

static void fixInitialValueAndStep(ControlFlowLoop& loop, LoopControl& loopControl)
{
	intermediate::Operation* stepOp = loopControl.iterationStep->get<intermediate::Operation>();
	if(stepOp == nullptr)
		throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled iteration step operation");

	const_cast<DataType&>(loopControl.initialization->getOutput()->type).num = loopControl.iterationVariable->type.num;
	intermediate::MoveOperation* move = dynamic_cast<intermediate::MoveOperation*>(loopControl.initialization);
	if(move != nullptr && move->getSource().hasLiteral(INT_ZERO.literal) && loopControl.stepKind == StepKind::ADD_CONSTANT && loopControl.getStep().is(INT_ONE.literal))
	{
		//special/default case: initial value is zero and step is +1
		move->setSource(ELEMENT_NUMBER_REGISTER);
		move->setDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
		logging::debug() << "Changed initial value: " << loopControl.initialization->to_string() << logging::endl;
	}
	else
		throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled initial value", loopControl.initialization->to_string());

	bool stepChanged = false;
	switch(stepOp->op.opAdd)
	{
		case OP_ADD.opAdd:
		case OP_SUB.opAdd:
			if(stepOp->getFirstArg().hasType(ValueType::LOCAL))
			{
				Value offset = stepOp->getSecondArg().value();
				if(offset.getLiteralValue())
					stepOp->setArgument(1, Value(Literal(offset.getLiteralValue()->integer * loopControl.vectorizationFactor), offset.type.toVectorType(offset.type.num * loopControl.vectorizationFactor)));
				else
					throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled iteration step", stepOp->to_string());
			}
			else
			{
				Value offset = stepOp->getFirstArg();
				if(offset.getLiteralValue())
					stepOp->setArgument(0, Value(Literal(offset.getLiteralValue()->integer * loopControl.vectorizationFactor), offset.type.toVectorType(offset.type.num * loopControl.vectorizationFactor)));
				else
					throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled iteration step", stepOp->to_string());
			}
			logging::debug() << "Changed iteration step: " << stepOp->to_string() << logging::endl;
			stepChanged = true;
	}

	if(!stepChanged)
		throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled iteration step operation", stepOp->to_string());
}

/*
 * Approach:
 * - set the iteration variable (local) to vector
 * - iterative (until no more values changed), modify all value (and local)-types so argument/result-types match again
 * - add new instruction-decoration (vectorized) to facilitate
 * - in final iteration, fix TMU/VPM configuration and address calculation and loop condition
 * - fix initial iteration value and step
 */
static void vectorize(ControlFlowLoop& loop, LoopControl& loopControl, const DataDependencyGraph& dependencyGraph)
{
	FastSet<intermediate::IntermediateInstruction*> openInstructions;

	const_cast<DataType&>(loopControl.iterationVariable->type).num *= loopControl.vectorizationFactor;
	scheduleForVectorization(loopControl.iterationVariable, openInstructions, loop);
	std::size_t numVectorized = 0;

	//iteratively change all instructions
	while(!openInstructions.empty())
	{
		auto it = findInLoop(*openInstructions.begin(), loop);
		if(!it)
		{
			//TODO what to do?? These are e.g. for accumulation-variables (like sum, maximum)
			//FIXME depending on the operation performed on this locals, the vector-elements need to be folded into a scalar/previous vector width
			logging::warn() << "Local is accessed outside of loop: " << (*openInstructions.begin())->to_string() << logging::endl;
			//openInstructions.erase(openInstructions.begin());
			throw CompilationError(CompilationStep::OPTIMIZER, "Accessing vectorized locals outside of the loop is not yet implemented", (*openInstructions.begin())->to_string());
		}
		else
		{
			vectorizeInstruction(it.value(), openInstructions, loopControl.vectorizationFactor, loop);
			++numVectorized;
		}
	}

	numVectorized += fixVPMSetups(loop, loopControl);

	fixInitialValueAndStep(loop, loopControl);
	numVectorized += 2;

	logging::debug() << "Vectorization done, changed " << numVectorized << " instructions!" << logging::endl;
}

void optimizations::vectorizeLoops(const Module& module, Method& method, const Configuration& config)
{
	if(!config.autoVectorization)
		return;

	//1. find loops
	auto cfg = ControlFlowGraph::createCFG(method);
	auto loops = cfg.findLoops();

	//2. determine data dependencies of loop bodies
	auto dependencyGraph = DataDependencyGraph::createDependencyGraph(method);

	for(auto& loop : loops)
	{
		//3. determine operation on iteration variable and bounds
		LoopControl loopControl = extractLoopControl(loop, dependencyGraph);
		if(loopControl.iterationVariable == nullptr)
			//we could not find the iteration variable, skip this loop
			continue;

		if(!loopControl.initialization || loopControl.terminatingValue.isUndefined() || !loopControl.iterationStep || !loopControl.repetitionJump)
		{
			//we need to know both bounds and the iteration step (for now)
			logging::debug() << "Failed to find all bounds and step for loop, aborting vectorization!" << logging::endl;
			continue;
		}

		//4. determine vectorization factor
		Optional<unsigned> vectorizationFactor = determineVectorizationFactor(loop, loopControl);
		if(!vectorizationFactor)
		{
			logging::debug() << "Failed to determine a vectorization factor for the loop, aborting!" << logging::endl;
			continue;
		}
		if(vectorizationFactor.value() == 1)
			//nothing to do
			continue;
		loopControl.vectorizationFactor = vectorizationFactor.value();

		//5. cost-benefit calculation
		int rating = calculateCostsVsBenefits(loop, loopControl, dependencyGraph);
		if(rating < 0 /* TODO some positive factor to be required before vectorizing loops? */)
			//vectorization (probably) doesn't pay off
			continue;

		//6. run vectorization
		vectorize(loop, loopControl, dependencyGraph);
		//increasing the iteration step might create a value not fitting into small immediate
		handleImmediate(module, method, loopControl.iterationStep.value(), config);
	}
}

void optimizations::extendBranches(const Module& module, Method& method, const Configuration& config)
{
	auto it = method.walkAllInstructions();
	//we only need to set the same flag once
	std::pair<Value, intermediate::InstructionDecorations> lastSetFlags = std::make_pair(UNDEFINED_VALUE, intermediate::InstructionDecorations::NONE);
	while(!it.isEndOfMethod())
	{
		intermediate::Branch* branch = it.get<intermediate::Branch>();
		if (branch != nullptr)
		{
			if(branch->hasConditionalExecution() || !branch->getCondition().hasLiteral(BOOL_TRUE.literal))
			{
				/*
				 * branch can only depend on scalar value
				 * -> set any not used vector-element (all except element 0) to a value where it doesn't influence the condition
				 *
				 * Using ELEMENT_NUMBER sets the vector-elements 1 to 15 to a non-zero value and 0 to either 0 (if condition was false) or 1 (if condition was true)
				 */
				//TODO can be skipped, if it is checked/guaranteed, that the last instruction setting flags is the boolean-selection for the given condition
				//but we need to check more than the last instructions, since there could be moves inserted by phi

				//skip setting of flags, if the previous setting wrote the same flags
				if(lastSetFlags.first != branch->getCondition() || has_flag(branch->decoration, intermediate::InstructionDecorations::BRANCH_ON_ALL_ELEMENTS) != has_flag(lastSetFlags.second, intermediate::InstructionDecorations::BRANCH_ON_ALL_ELEMENTS))
				{
					if(has_flag(branch->decoration, intermediate::InstructionDecorations::BRANCH_ON_ALL_ELEMENTS))
						it.emplace(new intermediate::Operation(OP_OR, NOP_REGISTER, branch->getCondition(), branch->getCondition(), COND_ALWAYS, SetFlag::SET_FLAGS));
					else
						it.emplace(new intermediate::Operation(OP_OR, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, branch->getCondition(), COND_ALWAYS, SetFlag::SET_FLAGS));
					it.nextInBlock();
				}
				lastSetFlags.first = branch->getCondition();
				lastSetFlags.second = branch->decoration;
			}
			//go to next instruction
			it.nextInBlock();
			//insert 3 NOPs before
			it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
			it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
			it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
		}
		else if(it.get() != nullptr && it->setFlags == SetFlag::SET_FLAGS)
		{
			//any other instruction setting flags, need to re-set the branch-condition
			lastSetFlags = std::make_pair(UNDEFINED_VALUE, intermediate::InstructionDecorations::NONE);
		}
		it.nextInMethod();
	}
}

static InstructionWalker loadVectorParameter(const Parameter& param, Method& method, InstructionWalker it)
{
	//we need to load a UNIFORM per vector element into the particular vector element
	for(uint8_t i = 0; i < param.type.num; ++i)
	{
		//the first write to the parameter needs to unconditional, so the register allocator can find it
		if(i > 0)
		{
			it.emplace( new intermediate::Operation(OP_XOR, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, Value(SmallImmediate(i), TYPE_INT8), COND_ALWAYS, SetFlag::SET_FLAGS));
			it.nextInBlock();
		}
		if(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND))
		{
			it = intermediate::insertSignExtension(it, method, Value(REG_UNIFORM, param.type), Value(&param, TYPE_INT32), false, i == 0 ? COND_ALWAYS : COND_ZERO_SET);
		}
		else if(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND))
		{
			it = intermediate::insertZeroExtension(it, method, Value(REG_UNIFORM, param.type), Value(&param, TYPE_INT32), false, i == 0 ? COND_ALWAYS : COND_ZERO_SET);
		}
		else
		{
			it.emplace(new intermediate::MoveOperation(param.createReference(), UNIFORM_REGISTER, i == 0 ? COND_ALWAYS : COND_ZERO_SET));
			it.nextInBlock();
		}
		//TODO improve performance by first putting together the vector, then zero/sign extending all elements?
	}
	return it;
}

static void generateStopSegment(Method& method)
{
    //write interrupt for host
    //write QPU number finished (value must be NON-NULL, so we invert it -> the first 28 bits are always 1)
    method.appendToEnd(new intermediate::Operation(OP_NOT, Value(REG_HOST_INTERRUPT, TYPE_INT8), Value(REG_QPU_NUMBER, TYPE_INT8)));
    intermediate::IntermediateInstruction* nop = new intermediate::Nop(intermediate::DelayType::THREAD_END);
    //set signals to stop thread/program
    nop->setSignaling(SIGNAL_END_PROGRAM);
    method.appendToEnd(nop);
    method.appendToEnd(new intermediate::Nop(intermediate::DelayType::THREAD_END));
    method.appendToEnd(new intermediate::Nop(intermediate::DelayType::THREAD_END));
}

void optimizations::addStartStopSegment(const Module& module, Method& method, const Configuration& config)
{
	auto it = method.walkAllInstructions();
	if(!it.has<intermediate::BranchLabel>() || BasicBlock::DEFAULT_BLOCK.compare(it.get<intermediate::BranchLabel>()->getLabel()->name) != 0)
	{
		it = method.emplaceLabel(it, new intermediate::BranchLabel(*method.findOrCreateLocal(TYPE_LABEL, BasicBlock::DEFAULT_BLOCK)));
	}
	it.nextInBlock();

	/*
	 * The first UNIFORMs are reserved for relaying information about the work-item and work-group
	 * - work_dim: number of dimensions
	 * - global_size: global number of work-items per dimension
	 * - global_id: global id of this work-item per dimension
	 * - local_size: local number of work-items in its work-group per dimension
	 * - local_id: local id of this work-item within its work-group
	 * - num_groups: global number of work-groups per dimension
	 * - group_id: id of this work-group
	 * - global_offset: global initial offset per dimension
	 * - address of global data / to load the global data from
	 *
	 */
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::WORK_DIMENSIONS)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::LOCAL_SIZES)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::LOCAL_IDS)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::NUM_GROUPS_X)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::NUM_GROUPS_Y)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::NUM_GROUPS_Z)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GROUP_ID_X)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GROUP_ID_Y)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GROUP_ID_Z)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_OFFSET_X)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_OFFSET_Y)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_OFFSET_Z)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(method.findOrCreateLocal(TYPE_INT32, Method::GLOBAL_DATA_ADDRESS)->createReference(), UNIFORM_REGISTER));
	it.nextInBlock();

	//load arguments to locals (via reading from uniform)
	for(const Parameter& param : method.parameters)
	{
		//do the loading
		//we need special treatment for non-scalar parameter (e.g. vectors), since they can't be read with just 1 UNIFORM
		if(!param.type.isPointerType() && param.type.num != 1)
		{
			it = loadVectorParameter(param, method, it);
		}
		else if(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND))
		{
			it = intermediate::insertSignExtension(it, method, Value(REG_UNIFORM, param.type), Value(&param, TYPE_INT32), false);
		}
		else if(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND))
		{
			it = intermediate::insertZeroExtension(it, method, Value(REG_UNIFORM, param.type), Value(&param, TYPE_INT32), false);
		}
		else
		{
			it.emplace(new intermediate::MoveOperation(param.createReference(), UNIFORM_REGISTER));
			it.nextInBlock();
		}
	}

	generateStopSegment(method);
}
