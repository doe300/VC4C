/*
 * Author: nomaddo
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_SCHEDULER_H
#define VC4C_SCHEDULER_H

#include <queue>
#include "DAG.h"
#include "../Units.h"
namespace vc4c {

using Ops = std::vector<intermediate::Operation *>;
using Moves = std::vector<intermediate::MoveOperation *>;

class Scheduler {
public:
	static void doScheduling(BasicBlock &bb, DAG &dag);
};

class InstructionSelector {
	DAG& dag;

	// To fix the order of tmu signal and its read, we remember the order
	std::queue<intermediate::Operation*> tmu0Signals = std::queue<intermediate::Operation*>();
	std::queue<intermediate::Operation*> tmu1Signals = std::queue<intermediate::Operation*>();
	IL * previousInstruction = nullptr;

private:
	int balanceTMU();
	bool canIssueTmuLoad();
	std::pair<int, intermediate::Operation *> chooseInstructionForTMUSignal (Ops & ils);
	intermediate::Operation * chooseInstructionForNumber (Ops & ils);


public:
		InstructionSelector (DAG & dag);
		IL * choose();
		bool empty();
		IL *combineOperation(intermediate::Operation *op, Ops & addOp, Ops & mulOp);
		IL * issueCombined (Ops & addOp, Ops & mulOp, Moves & moves);
};

}

#endif //VC4C_SCHEDULER_H
