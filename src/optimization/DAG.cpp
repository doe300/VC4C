/*
 * Author: nomaddo
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <log.h>
#include <assert.h>
#include <iostream>
#include <functional>
#include <algorithm>
#include <string>
#include <stack>
#include <utility>
#include <vector>

#include "DAG.h"
#include "../InstructionWalker.h"
#include "log.h"

#include "../analysis/ControlFlowGraph.h"
#include "../analysis/DebugGraph.h"
#include "../InstructionWalker.h"
#include "../Profiler.h"

namespace vc4c {

		/* event that make dependency (signal, writeing or reading from reigster */
		enum Dependency {
				ENUM_READ,
				ENUM_WRITE,
		};

		class Event {
		public:
				bool is_reg;
				bool is_sig;
				Event() = default;
				virtual ~Event() = default;
		};

		class RegisterEvent : public Event {
		public:
				Register reg;
				Dependency relation;

				explicit RegisterEvent(Register reg, Dependency dep) : reg(reg), relation(dep) {
					is_reg = true;
					is_sig = false;
				}
				~RegisterEvent() override = default;
		};

		class SignalEvent : public Event {
		public:
				Signaling sig;

				explicit SignalEvent(Signaling sig) : sig(sig) {
					is_reg = false;
					is_sig = true;
				}

				~SignalEvent() override = default;
		};

		class Relation {
		public:
				/* a trigger that make dependency */
				std::shared_ptr<Event> trigger;

				/* what kind of element is tied*/
				std::shared_ptr<Event> target;

				Relation(Register reg_in, Dependency dep_in, Register reg_out, Dependency dep_out) :
					trigger(new RegisterEvent(reg_in, dep_in)), target(new RegisterEvent(reg_out, dep_out)) {}

				Relation(Register reg_in, Dependency dep_in, Signaling sig) :
					trigger(new RegisterEvent(reg_in, dep_in)), target(new SignalEvent(sig)) {}

				Relation(Signaling sig, Register reg_out, Dependency dep_out) :
					trigger(new SignalEvent(sig)), target(new RegisterEvent(reg_out, dep_out)) {}

				Relation(Signaling sig_in, Signaling sig_out) : trigger(new SignalEvent(sig_in)), target(new SignalEvent(sig_out)){	}
		};


		Relation dependency_table[] = {
			Relation (REG_UNIFORM, ENUM_READ, REG_UNIFORM, ENUM_READ),
			Relation (REG_SFU_EXP2, ENUM_WRITE, REG_TMU_OUT, ENUM_READ),
			Relation (REG_SFU_LOG2, ENUM_WRITE, REG_TMU_OUT, ENUM_READ),
			Relation (REG_TMU0_ADDRESS, ENUM_WRITE, REG_TMU_OUT, ENUM_READ),
			Relation (REG_TMU1_ADDRESS, ENUM_WRITE, REG_TMU_OUT, ENUM_READ),
			Relation (REG_UNIFORM_ADDRESS, ENUM_WRITE, REG_UNIFORM, ENUM_READ),
		};

		/* Special Dependency Controller */
		class DController {
				std::map<Register, std::shared_ptr<Event>> writeTrigger;
				std::map<Register, std::shared_ptr<Event>> readTrigger;
				std::map<Signaling, std::shared_ptr<Event>> signalTrigger;

				std::map<Register, IL*> readMap;
				std::map<Register, IL*> writeMap;
				std::map<Signaling, IL*> sigMap;

				void push_back(std::vector<IL*> * vec, IL * il) {
					/* XXX for first use of registers
					 *  var = or uniform, uniform <- in this case, nullptr is stored in the map
					 *  var = or uniform, uniform <- in this case, the map must point upper instruction
					 */
					if(il != nullptr)
						vec->push_back(il);
				}
		public:
				DController() {
					for (auto r : dependency_table) {
						if (r.trigger->is_reg) {
							auto e = std::dynamic_pointer_cast<RegisterEvent>(r.trigger);
							if (e->relation == Dependency::ENUM_READ) {
								readTrigger[e->reg] = r.target;
								readMap[e->reg] = nullptr;
							}

							if (e->relation == Dependency::ENUM_WRITE) {
								writeTrigger[e->reg] = r.target;
								writeMap[e->reg] = nullptr;
							}
						}
					}
				}

				std::unique_ptr<std::vector<IL*>> getInstructions(IL * il) {
					auto vec = new std::vector<IL*>();

					if (signalTrigger.find(il->signal) != signalTrigger.end()){
						push_back(vec, sigMap[il->signal]);
					}

					if (il->getOutput().has_value() && il->getOutput().value().isRegister()) {
						Value out = il->getOutput().value();
						Register reg = out.reg;

						if (writeTrigger.find(reg) != writeTrigger.end()) {
							auto e = writeTrigger[reg];
							if (e->is_reg) {
								auto ereg = std::dynamic_pointer_cast<RegisterEvent>(e);
								push_back(vec, writeMap[ereg->reg]);
							} else if (e->is_sig) {
								auto esig = std::dynamic_pointer_cast<SignalEvent>(e);
								push_back(vec, sigMap[esig->sig]);
							}
						}

						/* must be last */
						if (writeMap.find(reg) != writeMap.end())
							writeMap[reg] = il;
					}

					for (Value arg : il->getArguments()) {
						if (arg.isRegister()) {
							Register reg = arg.reg;
							if (readTrigger.find(reg) != readTrigger.end()) {
								auto e = readTrigger[reg];
								if (e->is_reg) {
									auto ereg = std::dynamic_pointer_cast<RegisterEvent>(e);
									push_back(vec, readMap[ereg->reg]);
								} else if (e->is_sig) {
									auto esig = std::dynamic_pointer_cast<SignalEvent>(e);
									push_back(vec, sigMap[esig->sig]);
								}
							}

							/* must be last */
							if (readMap.find(reg) != readMap.end())
								readMap[reg] = il;
						}
					}

					if (sigMap.find(il->signal) != sigMap.end())
						sigMap[il->signal] = il;

					return std::unique_ptr<std::vector<IL*>>(vec);
				}
		};

/* XXX should be replaced with common routines that generate such instructions? */
/* reading from mutex I/O register means getting mutex */
bool isGetLocInstruction (IL * instr) {
	for (Value v: instr->getArguments()) {
		if (v.hasRegister(REG_MUTEX)) {
			return true;
		}
	}

	return false;
}

/* writing to mutex I/O register means release of mutex */
bool isReleaseLocInstruction (IL * instr) {
	if (instr->getOutput().has_value()) {
		auto output = instr->getOutput().value();
		return output.hasRegister(REG_MUTEX);
	}

	return false;
}

void DAG::addDependency(IL * ila, IL * ilb) {
	graph->getOrCreateNode(ilb).addNeighbor(&graph->getOrCreateNode(ila), DAGDependType::Depend);
	graph->getOrCreateNode(ila).addNeighbor(&graph->getOrCreateNode(ilb), DAGDependType::AntiDepend);
}

DAG::DAG(Method & method, BasicBlock &bb) : label(bb.getLabel()), method(method){
	graph = new Graph< IL*, DagNode>();
	auto dcon = DController();

	std::map<Value, IL*> map;
	auto mutexInstructions = std::vector<IL*>();
	IL * getMutexInstr = nullptr;

	auto setFlagInstructions = std::vector<IL*>();
	IL * setFlagInstr = nullptr;

	for (auto it = bb.begin(); !it.isEndOfBlock(); it.nextInBlock()) {
		auto instr = it.get()->copyFor(method, "");

		/* Instruction scheduler is responsible for translation to CombinedOperation.
		 * Not allowed to translate before!
		 */
		assert(! dynamic_cast<vc4c::intermediate::CombinedOperation*>(instr));

		if (dynamic_cast<vc4c::intermediate::BranchLabel *>(instr))
			continue;

		/* deal with mutex lock */
		if (isGetLocInstruction(instr)) {
			assert(getMutexInstr == nullptr);
			assert(mutexInstructions.empty());

			getMutexInstr = instr;
			mutexInstructions.push_back(instr);
			continue;
		}

		if (isReleaseLocInstruction(instr)) {
			assert (getMutexInstr != nullptr);
			assert (! mutexInstructions.empty());

			for (auto depInstr : mutexInstructions) {
				addDependency(depInstr, instr);
			}

			getMutexInstr = nullptr;
			mutexInstructions.clear();
			continue;
		}

		if (getMutexInstr != nullptr) {
			mutexInstructions.push_back(instr);
			addDependency(getMutexInstr, instr);
		}

		/* deal with set_flags and conditinal executions */
		if (instr->setFlags == vc4c::SetFlag::SET_FLAGS) {
			setFlagInstr = instr;

			for (auto depInstr : setFlagInstructions) {
				addDependency(depInstr, instr);
			}

			setFlagInstructions.clear();
		}

		if (instr->hasConditionalExecution()){
			if (setFlagInstr != nullptr) {
				/* XXX should be change it to an assertion.
				 * but, in the basic block of end of function, the case of using flags (but not set in the block) appears.
				 */
				setFlagInstructions.push_back(instr);
				addDependency(setFlagInstr, instr);
			}
		}

		auto instrs = dcon.getInstructions(instr);

		for (auto dep : * instrs) {
			addDependency(dep, instr);
		}

		auto const output = instr->getOutput();
		auto const args = instr->getArguments();

		for (Value v: args) {
			if (v.isLiteralValue())
				/* skip if immediate */
				continue;

			auto iter = map.find(v);
			if (iter == map.end()) {
				/* fail to find */
		}	else {
				auto def = iter->second;
				addDependency(def, instr);
			}
		}

		graph->getOrCreateNode(instr);
		if (output.has_value()) {
			const auto &out = output.value();
			  if (map.find(out) != map.end() && ! out.hasRegister(REG_NOP)) {
				  addDependency(map[out], instr);
			  }
		  map[out] = instr;
	  }
	}

#if DEBUG_MODE
	auto nameFunc = [](const IL * il) -> std::string { return il->to_string(); };
	auto weakEdgeFunc = [](const DAGDependType &typ) { return (typ == DAGDependType::Depend); };
	DebugGraph<IL*, DAGDependType>::dumpGraph(*graph, bb.getLabel()->to_string() + ".dot", true, nameFunc, weakEdgeFunc);
#endif
  }

void DAG::erase(IL * il) {
	assert (il != nullptr);

	for (auto it = graph->find(il); it != graph->end(); ++it){
		auto node = (* it).second;
		node.eraseNeighbors(il);
	}

	graph->erase(il);
}

std::vector<IL *> * DAG::getRoots() {
	roots->clear();
	std::for_each(graph->begin(), graph->end(),[&](std::pair<IL* const, DagNode> & nodePair){
		auto neighbors = nodePair.second.getNeighbors();
		int count = 0;
		for (auto pair : neighbors) {
			if (pair.second == DAGDependType::Depend)
				count++;
		}
		if (count == 0) {
			roots->push_back(nodePair.first);
		}
	});

	return roots;
}

int DAG::getNeighborsNumber(IL * il) {
	auto neighbors = graph->at(il).getNeighbors();
	return static_cast<int>(std::count_if (neighbors.begin(), neighbors.end(), [](std::pair<Node<IL*, DAGDependType> * const, DAGDependType> pair) {
			return pair.second == DAGDependType::Depend;
		}));
}

Optional<int> DAG::stepForTMULoad(IL *il) {
	if (il->signal == SIGNAL_LOAD_TMU0 || il->signal == SIGNAL_LOAD_TMU1)
		return new Optional<int>(0);
	if (getNeighborsNumber(il) == 0)
		return new Optional<int>(false, -1);

	auto neighbors = graph->at(il).getNeighbors();
	Optional<int> * max = nullptr;
	for (auto iter = neighbors.begin(); iter != neighbors.end(); iter++) {
		auto nodePair = *iter;
		auto opt = stepForTMULoad(nodePair.first->key);
		if (opt.has_value() && opt.value() > 0) {
			if (max == nullptr)
				max = new Optional<int>(opt.has_value() + 1);
			else if (max->value() < opt.value())
				max = new Optional<int>(opt.has_value() + 1);
		}
	}

	if (max == nullptr)
		return new Optional<int>(false, -1);
	else
		return *max;
}

	void DAG::dumpGraph() {
		auto nameFunc = [](const IL *il) -> std::string { return il->to_string(); };
		auto weakEdgeFunc = [](const DAGDependType &b) -> bool { return b == DAGDependType::Depend; };
		DebugGraph<IL *, DAGDependType>::dumpGraph(*graph, getLabel()->to_string() + ".dot", true, nameFunc, weakEdgeFunc);
	}

	const intermediate::BranchLabel *DAG::getLabel() const {
		return label;
	}

  bool DAG::empty(){
	  return (graph->size() == 0);
  }
}
