/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ControlFlow.h"

#include "../ControlFlowGraph.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <algorithm>
#include <cmath>

using namespace vc4c;
using namespace vc4c::optimizations;

static Local* findLoopIteration(const ControlFlowLoop& loop, const DataDependencyGraph& dependencyGraph)
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

	for(Local* candidate : intersection)
	{
		logging::debug() << "Loop iteration variable candidate: " << candidate->to_string(false) << logging::endl;
	}

	if(intersection.empty())
	{
		logging::warn() << "Failed to find loop iteration variable for loop" << logging::endl;
		return nullptr;
	}
	if(intersection.size() == 1)
		return *intersection.begin();

	//TODO how to determine which candidate to use?
	//XXX or just print warning and skip vectorization? To not abort an otherwise working compilation!
	throw CompilationError(CompilationStep::OPTIMIZER, "Selection from multiple candidates for a loop iteration variable is not yet implemented");
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

struct LoopControl
{
	//the initial value for the loop iteration variable
	Value initialValue = UNDEFINED_VALUE;
	//the value compared with to terminate the loop
	Value terminatingValue = UNDEFINED_VALUE;
	//the local containing the current iteration-variable
	Local* iterationVariable;
	//the operation to change the iteration-variable
	Optional<InstructionWalker> iterationStep;
	//the comparison to check for continue/end loop
	Optional<InstructionWalker> comparisonInstruction;
	//the branch-instruction to continue the loop
	Optional<InstructionWalker> repetitionJump;
	//the comparison function to abort the loop
	std::string comparison;
	//the vectorization-factor used
	unsigned vectorizationFactor;

	OpCode getStepOperation() const
	{
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
};

static LoopControl extractLoopControl(const ControlFlowLoop& loop, const DataDependencyGraph& dependencyGraph)
{
	LoopControl loopControl;

	Local* local = findLoopIteration(loop, dependencyGraph);
	if(local == nullptr)
		return loopControl;

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
				loopControl.initialValue = tmp.value();
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
			userIt = std::find_if(iterationStep.local->getUsers().begin(), iterationStep.local->getUsers().end(), [](const std::pair<const LocalUser*, LocalUse>& pair) -> bool { return dynamic_cast<const intermediate::IntermediateInstruction*>(pair.first)->setFlags == SetFlag::SET_FLAGS;});
			if(userIt != iterationStep.local->getUsers().end())
			{
				//TODO need to check, whether the comparison result is the one used for branching
				//if not, set userIt to loop.end()
				auto instIt = findInLoop(dynamic_cast<const intermediate::IntermediateInstruction*>(userIt->first), loop);
				loopControl.comparisonInstruction = instIt;
				logging::debug() << "Found loop continue condition: " << loopControl.comparisonInstruction.value()->to_string() << logging::endl;
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

	return loopControl;
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

	const Literal initial = loopControl.initialValue.getLiteralValue().value();
	const Literal end = loopControl.terminatingValue.getLiteralValue().value();
	int64_t lowerBound = std::min(initial, end).integer;
	int64_t upperBound = std::max(initial, end).integer;
	int64_t iterations = 0;
	//the number of iterations from the bounds depends on the iteration operation
	const OpCode stepOperation = loopControl.getStepOperation();
	Literal step = loopControl.getStep().value();
	if(stepOperation.runsOnAddALU())
	{
		switch(stepOperation.opAdd)
		{
			case OP_ADD.opAdd:
			case OP_SUB.opAdd:
				//for a step of 1, it is upper - lower, for a step of e.g. (upper - lower)/2 iterations
				iterations = (upperBound - lowerBound) / step.integer;
				break;
			default:
				logging::warn() << "Unhandled operation for iteration-step: " << loopControl.iterationStep.value()->to_string() << logging::endl;
				return {};
		}
	}
	else
	{
		switch(stepOperation.opMul)
		{
			case OP_MUL24.opMul:
				//TODO logarithmic with step as exponent?
			default:
				logging::warn() << "Unhandled operation for iteration-step: " << loopControl.iterationStep.value()->to_string() << logging::endl;
				return {};
		}
	}
	logging::debug() << "Determined iteration count of " << iterations << logging::endl;

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
 *
 * On the benefit-side, we have (as factors):
 * - the iterations saved (times the number of instructions in an iteration)
 */
static int calculateCostsVsBenefits(const ControlFlowLoop& loop, const LoopControl& loopControl, const DataDependencyGraph& dependencyGraph)
{
	int costs = 0;

	InstructionWalker it = loop.front()->key->begin();
	while(!it.isEndOfMethod() && it != loop.back()->key->end())
	{
		//TODO check and increase costs
		it.nextInMethod();
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

static void scheduleForVectorization(const Local* local, FastSet<intermediate::IntermediateInstruction*>& openInstructions)
{
	local->forUsers(LocalUser::Type::READER, [&openInstructions](const LocalUser* user) -> void
	{
		intermediate::IntermediateInstruction* inst = dynamic_cast<intermediate::IntermediateInstruction*>(const_cast<LocalUser*>(user));
		if(!has_flag(inst->decoration, intermediate::InstructionDecorations::AUTO_VECTORIZED))
			openInstructions.emplace(inst);
	});
}

static void vectorizeInstruction(InstructionWalker it, FastSet<intermediate::IntermediateInstruction*>& openInstructions, unsigned vectorizationFactor)
{
	logging::debug() << "Vectorizing instruction: " << it->to_string() << logging::endl;

	//1. update types of values matching the types of their locals
	unsigned char vectorWidth = 1;
	for(auto& arg : it->getArguments())
	{
		if(arg.hasType(ValueType::LOCAL) && arg.type != arg.local->type)
		{
			scheduleForVectorization(arg.local, openInstructions);
			const_cast<DataType&>(arg.type).num = arg.local->type.num;
			vectorWidth = std::max(vectorWidth, arg.type.num);
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
			scheduleForVectorization(out.local, openInstructions);
		}
	}

	//TODO need to adapt types of some registers/output of load, etc.?

	// mark as already processed and remove from open-set
	it->setDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
	openInstructions.erase(it.get());
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
			if(vpwSetup.isDMASetup())
			{
				//FIXME this is only true for values actually vectorized! Need to check if corresponding VPM-write is vectorized
				vpwSetup.dmaSetup.setDepth(vpwSetup.dmaSetup.getDepth() * loopControl.vectorizationFactor);
				++numVectorized;
				it->setDecorations(intermediate::InstructionDecorations::AUTO_VECTORIZED);
			}
		}
		else if(it->writesRegister(REG_VPM_IN_SETUP))
			throw CompilationError(CompilationStep::OPTIMIZER, "Reading from VPM is not handled yet by the vectorizer", it->to_string());


		it.nextInMethod();
	}

	return numVectorized;
}

static void fixInitialValueAndStep(ControlFlowLoop& loop, LoopControl& loopControl)
{
	intermediate::Operation* stepOp = loopControl.iterationStep->get<intermediate::Operation>();
	if(stepOp == nullptr)
		throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled iteration step operation");

	//initial value is set by the phi-node writing into the iteration-variable that lies outside of the loop (XXX or store in loop-control?)
	for(auto& user : loopControl.iterationVariable->getUsers())
	{
		intermediate::IntermediateInstruction* inst = const_cast<intermediate::IntermediateInstruction*>(dynamic_cast<const intermediate::IntermediateInstruction*>(user.first));
		if(user.second.writesLocal() && !findInLoop(inst, loop))
		{
			const_cast<DataType&>(inst->getOutput()->type).num = loopControl.iterationVariable->type.num;

			intermediate::MoveOperation* move = dynamic_cast<intermediate::MoveOperation*>(inst);
			if(move != nullptr && (move->getSource().hasLiteral(INT_ZERO.literal) || move->getSource().hasImmediate(SmallImmediate(0))))
			{
				//XXX: This is only true for initial-value of zero and an iteration-step of +1
				move->setSource(ELEMENT_NUMBER_REGISTER);
				logging::debug() << "Changed initial value: " << inst->to_string() << logging::endl;
			}
			else
				throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled initial value", inst->to_string());
			//TODO more general version
		}
	}

	bool stepChanged = false;
	switch(stepOp->op.opAdd)
	{
		case OP_ADD.opAdd:
		case OP_SUB.opAdd:
			if(stepOp->getFirstArg().hasType(ValueType::LOCAL))
			{
				Value offset = stepOp->getSecondArg().value();
				if(offset.hasType(ValueType::LITERAL))
					offset.literal.integer *= loopControl.vectorizationFactor;
				else if(offset.hasType(ValueType::SMALL_IMMEDIATE))
					offset.immediate = SmallImmediate::fromInteger(offset.immediate.getIntegerValue().value() * static_cast<char>(loopControl.vectorizationFactor)).value();
				else
					throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled iteration step", stepOp->to_string());
				stepOp->setArgument(1, offset);
			}
			else
			{
				Value offset = stepOp->getFirstArg();
				if(offset.hasType(ValueType::LITERAL))
					offset.literal.integer *= loopControl.vectorizationFactor;
				else if(offset.hasType(ValueType::SMALL_IMMEDIATE))
					offset.immediate = SmallImmediate::fromInteger(offset.immediate.getIntegerValue().value() * static_cast<char>(loopControl.vectorizationFactor)).value();
				else
					throw CompilationError(CompilationStep::OPTIMIZER, "Unhandled iteration step", stepOp->to_string());
				stepOp->setArgument(0, offset);
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
	scheduleForVectorization(loopControl.iterationVariable, openInstructions);
	std::size_t numVectorized = 0;

	//iteratively change all instructions
	while(!openInstructions.empty())
	{
		auto it = findInLoop(*openInstructions.begin(), loop);
		if(!it)
		{
			//TODO what to do??
			openInstructions.erase(openInstructions.begin());
		}
		else
		{
			vectorizeInstruction(it.value(), openInstructions, loopControl.vectorizationFactor);
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

		if(loopControl.initialValue.isUndefined() || loopControl.terminatingValue.isUndefined() || !loopControl.iterationStep || !loopControl.repetitionJump)
		{
			//we need to know both bounds and the iteration step (for now)
			logging::warn() << "Failed to find all bounds and step for loop, aborting vectorization!" << logging::endl;
			continue;
		}

		//4. determine vectorization factor
		Optional<unsigned> vectorizationFactor = determineVectorizationFactor(loop, loopControl);
		if(!vectorizationFactor)
		{
			logging::warn() << "Failed to determine a vectorization factor for the loop, aborting!" << logging::endl;
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
	}
}
