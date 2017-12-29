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

#include "../ControlFlowGraph.h"
#include "../DebugGraph.h"
#include "../InstructionWalker.h"
#include "../Profiler.h"


namespace vc4c {

/* reading from mutex I/O register means getting mutex */
static bool isGetLocInstruction (IL * instr) {
	return instr->readsRegister(REG_MUTEX);
}

/* writing to mutex I/O register means release of mutex */
static bool isReleaseLocInstruction (IL * instr) {
	return instr->writesRegister(REG_MUTEX);
}

DAG::DAG(BasicBlock &bb) {
	std::map<Value, IL*> map;

	/* To fix the order of mutex lock. its release and instructions between them,
	 * - Remember an instruction that get mutex lock (as `getMutexInstr`)
	 * - Remember all of instrctions after execution of the getting mutex lock (as `mutexInstructions`)
	 *
	 * Create dependency between `getMutexInstr` and each `mutexInstructions`
	 * If an instructions, that release mutex lock, is found, create dependencies between the instruction and each `mutexInstructions`
	 *
	 * This code only remember one instruction that get mutex lock.
	 * It doesn't assume as the following case, because such program has no meaning (get stuck)
	 *
	 *   register mutex_rel = bool 1 (1)
	 *   register mutex_rel = bool 1 (1)
	 *   ... getting mutex lock before releaasing it.  ...
	 */
  auto mutexInstructions = std::shared_ptr<std::vector<IL*>>(new std::vector<IL*>());
	IL * getMutexInstr = nullptr;

	/* To fix the order of setting flags, and instructions that use it,
	 * - Remember an instruction that set flags (as `setFlagInstr`)
	 * - Remember all of instructions that use flags (as `setFlagInstructions`)
	 *
	 * Create dependencies between `setFlagInstr` and each of `setFlagInstructions`.
	 * If we found other instruction that set flags, create dependencies between that instructions and each of `setFlagInstructions`.
	 *
	 *   1 ... set flag ...
	 *   2 ... use flags ... must be issued after 1
	 *   3 ... use flags ... must be issued after 1
	 *   ..................
	 *   4 ... set flags ... must be issued after 2 and 3
	 *   5 ... use flags ... must be issued after 4
	 */
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
