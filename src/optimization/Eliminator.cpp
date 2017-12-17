/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Eliminator.h"

#include "../InstructionWalker.h"
#include "log.h"

#include <algorithm>
#include <list>
#include <map>

using namespace vc4c;
using namespace vc4c::optimizations;

void optimizations::eliminateDeadStore(const Module& module, Method& method, const Configuration& config)
{
	//TODO (additionally or instead of this) walk through locals, check whether they are never read and writings have no side-effects
	//then walk through all writings of such locals and remove them (example: ./testing/test_vpm_write.cl)
	auto it = method.walkAllInstructions();
	while(!it.isEndOfMethod())
    {
        intermediate::IntermediateInstruction* instr = it.get();
        //fail-fast on all not-supported instruction types
        //also skip all instructions writing to non-locals (registers)
        if(!it.has<intermediate::Branch>() && !it.has<intermediate::BranchLabel>() && !it.has<intermediate::SemaphoreAdjustment>() && instr->hasValueType(ValueType::LOCAL))
        {
        	intermediate::Operation* op = it.get<intermediate::Operation>();
        	intermediate::MoveOperation* move = it.get<intermediate::MoveOperation>();
        	intermediate::LoadImmediate* load = it.get<intermediate::LoadImmediate>();

        	//check whether the output of an instruction is never read
            //only check for ALU-operations and loads, if no flags are set and no special signals are sent
            if((move != nullptr || op != nullptr || load != nullptr) && !instr->hasSideEffects())
            {
                const Local* dest = instr->getOutput().get().local;
                //check whether local is 
                //a) no parameter ??
                if(!dest->is<Parameter>())
                {
                    //b) never read at all
                	//must check from the start, because in SPIR-V, locals can be read before they are written to (e.g. in phi-node and branch backwards)
                    bool isRead = !dest->getUsers(LocalUser::Type::READER).empty();
                    if(!isRead)
                    {
                        logging::debug() << "Removing instruction " << instr->to_string() << ", since its output is never read" << logging::endl;
                        it.erase();
                        //if we removed this instruction, maybe the previous one can be removed too??
                        it.previousInBlock();
                        continue;
                    }
                }
            }
			if(move != nullptr)
			{
				if(move->getSource().hasType(ValueType::LOCAL) && move->getOutput().get().hasType(ValueType::LOCAL) && !move->hasConditionalExecution() && !move->hasPackMode() && !move->hasSideEffects() && dynamic_cast<intermediate::VectorRotation*>(move) == nullptr)
				{
					//if for a move, neither the input-local nor the output-local are written to afterwards,
					//XXX or the input -local is only written after the last use of the output-local
					//both locals can be the same and the move can be removed

					const Local* inLoc = move->getSource().local;
					const Local* outLoc = move->getOutput().get().local;
					//for instruction added by phi-elimination, the result could have been written to (with a different source) previously, so check
					bool isWrittenTo = !outLoc->getUsers(LocalUser::Type::WRITER).empty();
					if(!isWrittenTo && inLoc->type == outLoc->type)
					{
						//TODO what if both locals are written before (and used differently), possible??
						logging::debug() << "Merging locals " << inLoc->to_string() << " and " <<  outLoc->to_string() << " since they contain the same value" << logging::endl;
						outLoc->forUsers(LocalUser::Type::READER, [inLoc, outLoc](const LocalUser* instr) -> void
						{
							//change outLoc to inLoc
							bool outLocFound = false;
							for(std::size_t i = 0; i< dynamic_cast<const intermediate::IntermediateInstruction*>(instr)->getArguments().size(); ++i)
							{
								Value tmp = dynamic_cast<const intermediate::IntermediateInstruction*>(instr)->getArgument(i);
								if(tmp.hasLocal(outLoc))
								{
									tmp = Value(inLoc, tmp.type);
									dynamic_cast<intermediate::IntermediateInstruction*>(const_cast<LocalUser*>(instr))->setArgument(i, tmp);
									outLocFound = true;
								}
							}
							if(!outLocFound)
							{
								throw CompilationError(CompilationStep::OPTIMIZER, "Unsupported case of instruction merging!");
							}
						});
						//skip ++it, so next instructions is looked at too
						it.erase();
						continue;
					}
				}
			}
    	}
        it.nextInMethod();
    }
    //remove unused locals. This is actually not required, but gives us some feedback about the effect of this optimization
    method.cleanLocals();
}

InstructionWalker optimizations::eliminateUselessInstruction(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
	intermediate::Operation* op = it.get<intermediate::Operation>();
	intermediate::MoveOperation* move = it.get<intermediate::MoveOperation>();
	if(op != nullptr)
	{
		if(!op->hasSideEffects() && !op->hasPackMode() && !op->hasUnpackMode())
		{
			//writes into the input -> can be removed, if it doesn't do anything
			if(op->getOutput() && op->getOutput().get() == op->getFirstArg())
			{
				Optional<Value> opIdentity = OpCode::getRightIdentity(op->op);
				//check whether second-arg exists and does nothing
				if(opIdentity && op->getSecondArg() && op->getSecondArg().get().hasLiteral(opIdentity.get().literal))
				{
					logging::debug() << "Removing obsolete " << op->to_string() << logging::endl;
					it.erase();
					//don't skip next instruction
					it.previousInBlock();
				}
			}
			//writes into the input -> can be removed, if it doesn't do anything
			else if(op->getOutput() && op->getSecondArg() && op->getOutput().get() == op->getSecondArg())
			{
				Optional<Value> opIdentity = OpCode::getLeftIdentity(op->op);
				//check whether first-arg does nothing
				if(opIdentity && op->getFirstArg().hasLiteral(opIdentity.get().literal))
				{
					logging::debug() << "Removing obsolete " << op->to_string() << logging::endl;
					it.erase();
					//don't skip next instruction
					it.previousInBlock();
				}
			}
			else    //writes to another local -> can be replaced with move
			{
				Optional<Value> rightIdentity = OpCode::getRightIdentity(op->op);
				Optional<Value> leftIdentity = OpCode::getLeftIdentity(op->op);
				//check whether second argument exists and does nothing
				if(rightIdentity && op->getSecondArg() && op->getSecondArg().get().hasLiteral(rightIdentity.get().literal))
				{
					logging::debug() << "Replacing obsolete " << op->to_string() << " with move" << logging::endl;
					it.reset(new intermediate::MoveOperation(op->getOutput().get(), op->getFirstArg(), op->conditional, op->setFlags));
				}
				//check whether first argument does nothing
				else if(leftIdentity && op->getSecondArg() && op->getFirstArg().hasLiteral(leftIdentity.get().literal))
				{
					logging::debug() << "Replacing obsolete " << op->to_string() << " with move" << logging::endl;
					it.reset(new intermediate::MoveOperation(op->getOutput().get(), op->getSecondArg(), op->conditional, op->setFlags));
				}
			}
		}
		//TODO trunc to int32/float
	}
	else if(move != nullptr)
	{
		if(move->getSource() == move->getOutput().get() && !move->hasSideEffects() && !move->hasPackMode() && !move->hasUnpackMode() && !it.has<intermediate::VectorRotation>())
		{
			//skip copying to same, if no flags/signals/pack and unpack-modes are set
			logging::debug() << "Removing obsolete " << move->to_string() << logging::endl;
			it.erase();
			//don't skip next instruction
			it.previousInBlock();
		}
	}

	return it;
}

InstructionWalker optimizations::eliminateUselessBranch(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    //eliminates branches to the next instruction to save up to 4 instructions (1 branch + 4 NOP)
	intermediate::Branch* branch = it.get<intermediate::Branch>();
	if(branch != nullptr)
	{
		//eliminate branches to the next instruction, such branches are e.g. introduced by method-inlining
		auto nextIt = it.copy().nextInMethod();
		if(!nextIt.isEndOfMethod())
		{
			intermediate::BranchLabel* label = nextIt.get<intermediate::BranchLabel>();
			if(label != nullptr)
			{
				if(label->getLabel() == branch->getTarget())
				{
					logging::debug() << "Removing branch to next instruction: " << branch->to_string() << logging::endl;
					it = it.erase();
					//don't skip next instruction
					it.previousInMethod();
				}
			}
		}
	}
	return it;
}

InstructionWalker optimizations::calculateConstantInstruction(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
	intermediate::Operation* op = it.get<intermediate::Operation>();
	if(op != nullptr && !op->hasUnpackMode())
	{
		//calculations with literals can be pre-calculated
		if(op->getFirstArg().hasType(ValueType::LITERAL) && (!op->getSecondArg() || op->getSecondArg().get().hasType(ValueType::LITERAL)))
		{
			if(op->conditional != COND_ALWAYS && op->opCode == "xor" && op->getSecondArg().is(op->getFirstArg()))
			{
				//skip "xor ?, true, true", so it can be optimized (combined with "move ?, true") afterwards
				//also skip any "xor ?, val, val", since they are created on purpose (by combineSelectionWithZero to allow for combination with the other case)
				return it;
			}
			const Optional<Value> value = op->precalculate(3);
			if(value)
			{
				logging::debug() << "Replacing '" << op->to_string() << "' with constant value: " << value.to_string() << logging::endl;
				it.reset((new intermediate::MoveOperation(op->getOutput(), value))->copyExtrasFrom(op));
			}
		}
	}
	return it;
}

static void mapPhi(const intermediate::PhiNode& node, Method& method, InstructionWalker it)
{
	while(!it.isStartOfBlock())
	{
		it.previousInBlock();
	}
	const Local* label = it.get<intermediate::BranchLabel>()->getLabel();
	for(const auto& pair : node.getValuesForLabels())
	{
		BasicBlock* bb = method.findBasicBlock(pair.first);
		if(bb == nullptr)
		{
			logging::error() << "Cannot map phi-node to label: " << pair.first->name << logging::endl;
			throw CompilationError(CompilationStep::OPTIMIZER, "Failed to map all phi-options to valid basic-blocks");
		}
		logging::debug() << "Inserting 'move' into end of basic-block: " << pair.first->name << logging::endl;
		//make sure, moves are inserted before the outgoing branches
		InstructionWalker it = bb->end();
		ConditionCode jumpCondition = COND_ALWAYS;
		Value condition(UNDEFINED_VALUE);
		while(it.copy().previousInBlock().has<intermediate::Branch>())
		{
			it.previousInBlock();
			if(it.get<intermediate::Branch>()->getTarget() == label)
			{
				jumpCondition = it->conditional;
				condition = it.get<intermediate::Branch>()->getCondition();
			}
		}
		//Since originally the value of the PHI node is set after the jump (at the start of the destination basic block)
		//and we have conditional branches "jump to A or B", we need to only set the value if we take the (conditional) branch jumping to this basic block.

		if(jumpCondition != COND_ALWAYS)
		{
			//Since the correct flags for the branch might not be set, we need to set them here.
			//Also, don't "or" with element number, since we might need to set the flags for more than the first SIMD-element, this way, we set it for all
			it.emplace(new intermediate::MoveOperation(NOP_REGISTER, condition, COND_ALWAYS, SetFlag::SET_FLAGS));
			it.nextInBlock();
		}
		it.emplace((new intermediate::MoveOperation(node.getOutput(), pair.second, jumpCondition))->copyExtrasFrom(&node)->setDecorations(add_flag(node.decoration, intermediate::InstructionDecorations::PHI_NODE)));
	}
}

void optimizations::eliminatePhiNodes(const Module& module, Method& method, const Configuration& config)
{
	//Search for all phi-nodes and insert all mapped instructions to the end of the corresponding basic block
	auto it = method.walkAllInstructions();
	while(!it.isEndOfMethod())
	{
		const intermediate::PhiNode* phiNode = it.get<intermediate::PhiNode>();
		if(phiNode != nullptr)
		{
			//2) map the phi-node to the move-operations per predecessor-label
			logging::debug() << "Eliminating phi-node by inserting moves: " << it->to_string() << logging::endl;
			mapPhi(*phiNode, method, it);
			it.erase();
		}
		else
			it.nextInMethod();
	}
}

InstructionWalker optimizations::eliminateReturn(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
	if(it.has<intermediate::Return>())
	{
		const Local* target = method.findLocal(BasicBlock::LAST_BLOCK);
		if(target == nullptr)
		{
			target = method.findOrCreateLocal(TYPE_LABEL, BasicBlock::LAST_BLOCK);
			method.appendToEnd(new intermediate::BranchLabel(*target));
		}
		logging::debug() << "Replacing return in kernel-function with branch to end-label" << logging::endl;
		it.reset(new intermediate::Branch(target, COND_ALWAYS, BOOL_TRUE));
	}
	return it;
}
