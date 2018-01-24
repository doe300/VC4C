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
	auto new_ila = ila->copyFor(method, "");
	auto new_ilb = ilb->copyFor(method, "");
	graph->getOrCreateNode(new_ilb).addNeighbor(&graph->getOrCreateNode(new_ila), DAGDependType::Depend);
	graph->getOrCreateNode(new_ila).addNeighbor(&graph->getOrCreateNode(new_ilb), DAGDependType::AntiDepend);
}

DAG::DAG(Method & method, BasicBlock &bb) : label(bb.getLabel()), method(method){
	std::map<Value, IL*> map;
	auto mutexInstructions = std::vector<IL*>();
	IL * getMutexInstr = nullptr;

	auto setFlagInstructions = std::vector<IL*>();
	IL * setFlagInstr = nullptr;

	for (auto it = bb.begin(); !it.isEndOfBlock(); it.nextInBlock()) {
		auto instr = it.get();

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

		graph->getOrCreateNode(instr->copyFor(method, ""));
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
	for (auto it = graph->find(il); it != graph->end(); ++it){
		auto node = (* it).second;
		node.eraseNeighbors(il);
	}

	graph->erase(il);
}

std::vector<IL *> * DAG::getRoots() {
	roots->clear();
	std::for_each(graph->begin(), graph->end(),[&](std::pair<IL* const, Node<IL*, DAGDependType>> & nodePair){
		auto neighbors = nodePair.second.neighbors;
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
	auto neighbors = graph->at(il).neighbors;
	return static_cast<int>(std::count_if (neighbors.begin(), neighbors.end(), [](std::pair<Node<IL*, DAGDependType> * const, DAGDependType> pair) {
			return pair.second == DAGDependType::Depend;
		}));
}

Optional<int> DAG::stepForTMULoad(IL *il) {
	if (il->signal == SIGNAL_LOAD_TMU0 || il->signal == SIGNAL_LOAD_TMU1)
		return new Optional<int>(0);
	if (getNeighborsNumber(il) == 0)
		return new Optional<int>(false, -1);

	auto neighbors = graph->at(il).neighbors;
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
