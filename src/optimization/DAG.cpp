//
// Created by nomaddo on 17/12/18.
//

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

#include "../ControlFlowGraph.h"
#include "../DebugGraph.h"
#include "../InstructionWalker.h"
#include "../Profiler.h"

namespace vc4c {

/* XXX should be replaced with common routines that generate such instructions? */
/* reading from mutex I/O register means getting mutex */
static bool isGetLocInstruction (IL * instr) {
	for (Value v: instr->getArguments()) {
		if (v.hasRegister(REG_MUTEX)) {
			return true;
		}
	}

	return false;
}

/* writing to mutex I/O register means release of mutex */
static bool isReleaseLocInstruction (IL * instr) {
	if (instr->getOutput().has_value()) {
		auto output = instr->getOutput().value();
		return output.hasRegister(REG_MUTEX);
	}

	return false;
}

DAG::DAG(BasicBlock &bb) {
	std::map<Value, IL*> map;
  auto mutexInstructions = std::shared_ptr<std::vector<IL*>>(new std::vector<IL*>());
	IL * getMutexInstr = nullptr;

	auto setFlagInstructions = std::shared_ptr<std::vector<IL*>>(new std::vector<IL*>());
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
			assert(mutexInstructions->empty());

			getMutexInstr = instr;
			mutexInstructions->push_back(instr);
			continue;
		}

		if (isReleaseLocInstruction(instr)) {
			assert (getMutexInstr != nullptr);
			assert (! mutexInstructions->empty());

			for (auto depInstr : *mutexInstructions) {
				graph->getOrCreateNode(depInstr).addNeighbor(&graph->getOrCreateNode(instr), true);
			}

			getMutexInstr = nullptr;
			mutexInstructions->clear();
			continue;
		}

		if (getMutexInstr != nullptr) {
			mutexInstructions->push_back(instr);
			graph->getOrCreateNode(getMutexInstr).addNeighbor(&graph->getOrCreateNode(instr), true);
		}

		/* deal with set_flags and conditinal executions */
		if (instr->setFlags == vc4c::SetFlag::SET_FLAGS) {
			setFlagInstr = instr;

			for (auto depInstr : *setFlagInstructions) {
				graph->getOrCreateNode(depInstr).addNeighbor(&graph->getOrCreateNode(instr), true);
			}

			setFlagInstructions->clear();
		}

		if (instr->hasConditionalExecution()){
			if (setFlagInstr != nullptr) {
				/* XXX should be change it to an assertion.
				 * but, in the basic block of end of function, the case of using flags (but not set in the block) appears.
				 */
				setFlagInstructions->push_back(instr);
				graph->getOrCreateNode(setFlagInstr).addNeighbor(&graph->getOrCreateNode(instr), true);
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
				graph->getOrCreateNode(def).addNeighbor(&graph->getOrCreateNode(instr), true);
			}
		}

	  if (output.has_value()) {
			const auto &out = output.value();
			  if (map.find(out) != map.end() && ! out.hasRegister(REG_NOP)) {
				  graph->getOrCreateNode(map[out]).addNeighbor(&graph->getOrCreateNode(instr), true);
			  }

			map[out] = instr;
		}
	}

#if DEBUG_MODE
	auto nameFunc = [](const IL * il) -> std::string { return il->to_string(); };
	std::function<bool(const bool &)> weakEdgeFunc = [](const bool &b) -> bool { return b; };
	DebugGraph<IL*, bool>::dumpGraph(*graph, bb.getLabel()->to_string() + ".dot", true, nameFunc, weakEdgeFunc);
#endif
  }
}
