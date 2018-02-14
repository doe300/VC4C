/*
 * Author: nomaddo
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <tuple>
#include "Scheduler.h"
#include <memory>
#include <algorithm>
#include <log.h>
#include "../Units.h"
#include "../InstructionWalker.h"

namespace vc4c {

void Scheduler::doScheduling(vc4c::BasicBlock &bb, vc4c::DAG &dag) {
	auto selector = InstructionSelector(dag);
	bb.removeAll();

	while(!selector.empty()) {
		IL * il = selector.choose();
		bb.pushBack(il);
	}

	logging::debug() << "doScheduling" << logging::endl;
	bb.dumpInstructions();
}

InstructionSelector::InstructionSelector(DAG &dag) : dag(dag) {}

int InstructionSelector::balanceTMU() {
	return (tmu0Signals.size() < tmu1Signals.size());
}

bool InstructionSelector::canIssueTmuLoad() {
	return (tmu0Signals.size() < 4 && tmu1Signals.size() < 4);
}

IL * InstructionSelector::combineOperation(intermediate::Operation * op, Ops & addOp, Ops & mulOp) {
	Ops vec;
	if (op->op.runsOnMulALU())
		vec = addOp;
	else
		vec = mulOp;

	if (vec.size() == 0) {
		dag.erase(op);
		return op;
	}

	auto max = std::numeric_limits<int>::min();
	intermediate::Operation * maxOp = nullptr;

	std::for_each(vec.begin(), vec.end(), [&](intermediate::Operation * op){
		auto num = dag.getNeighborsNumber(op);
		if (max < num) {
			max = num;
			maxOp = op;
		}
	});

	assert (maxOp != nullptr);
	dag.erase(op);
	dag.erase(maxOp);
	if (op->op.runsOnMulALU())
		return new intermediate::CombinedOperation(maxOp, op);
	else
		return new intermediate::CombinedOperation(op, maxOp);
}

bool isAssigned(IL * il, Value & v) {
	if (auto combined = dynamic_cast<intermediate::CombinedOperation*>(il)) {
		bool flag = true;
		if (combined->op1->getOutput().has_value())
			flag = flag && combined->op1->getOutput().value() == v;
		if (combined->op2->getOutput().has_value())
			flag = flag && combined->op2->getOutput().value() == v;

		return flag;
	}

	if (il->getOutput().has_value())
		return il->getOutput().value() == v;
	return false;
}

class SchedulingProperty {
public:
	bool operator<(SchedulingProperty & p) {
		if (stepNumberForTMU.has_value() && p.stepNumberForTMU.has_value())
			return neighborsNumber < p.neighborsNumber;
		else if (stepNumberForTMU.has_value())
			return true;
		else if (p.stepNumberForTMU.has_value())
			return false;
		else if (! stepNumberForTMU.has_value() && ! p.stepNumberForTMU.has_value())
			return neighborsNumber < p.neighborsNumber;
	}

    Optional<int> stepNumberForTMU;
    int neighborsNumber;
    IL * il;

    SchedulingProperty(Optional<int> opt, int n, IL * il) : stepNumberForTMU(opt), neighborsNumber(n), il(il) {}

		SchedulingProperty(DAG dag, IL * il) {
			auto opt = dag.stepForTMULoad(il);
			auto neighbors = dag.getNeighborsNumber(il);

			stepNumberForTMU = opt;
			neighborsNumber = neighbors;
			this->il = il;
		};

		static std::shared_ptr<SchedulingProperty> create_shared(DAG dag, IL * il){
			return std::shared_ptr<SchedulingProperty>(new SchedulingProperty (dag, il));
		}
};

bool canCombined(IL * ila, IL * ilb) {
	assert (dynamic_cast<intermediate::Operation*>(ila) || dynamic_cast<intermediate::MoveOperation*>(ila));
	assert (dynamic_cast<intermediate::Operation*>(ilb) || dynamic_cast<intermediate::MoveOperation*>(ilb));

	/* check confliction of smallImmediate */
	auto useImm = Optional<SmallImmediate>(false, SmallImmediate('0'));
	for (auto il : {ila, ilb}) {
		for (auto arg : il->getArguments()) {
			if (arg.isSmallImmediate()) {
				if (useImm.has_value() && arg.immediate == useImm.value()) {
					return false;
				}

				/* assume useImm.hasValue() == false */
				useImm = Optional<SmallImmediate>(arg.immediate);
			}
		}
	}

	return true;
}

		using property = std::shared_ptr<SchedulingProperty>;
		using properties = std::vector<property>;

template<typename X>
IL* single(DAG dag, std::vector<X*> vec) {
	auto ps = std::vector<property>(vec.size());
	std::transform(vec.begin(), vec.end(), std::back_inserter(ps), [&](X * il) {
		return SchedulingProperty::create_shared(dag, il);
	});

	property max = nullptr;
	for (auto x : ps) {
		if (max == nullptr || (*max) < *x)
			max = x;
	}

	return max->il;
}

IL * InstructionSelector::issueCombined (Ops & addOp, Ops & mulOp,
																				 Moves & moves) {
	IL * add = nullptr;
	IL * mul = nullptr;

	if (addOp.empty() && mulOp.empty()) {
		add = single(dag, moves);
		dag.erase(add);
		return add;
	}
	else if (addOp.empty() && moves.empty()) {
		mul = single(dag, mulOp);
		dag.erase(mul);
		return mul;

	}
	else if (mulOp.empty() && moves.empty()) {
		add = single(dag, addOp);
		dag.erase(add);
		return add;
	}

	else {
		auto addps = properties();
		std::transform(addOp.begin(), addOp.end(), std::back_inserter(addps),
									 [&](IL * il){ return SchedulingProperty::create_shared(dag, il); });
		auto mulps = properties();
		std::transform(mulOp.begin(), mulOp.end(), std::back_inserter(mulps),
									 [&](IL * il){ return SchedulingProperty::create_shared(dag, il); });
		auto movps = properties();
		std::transform(moves.begin(), moves.end(), std::back_inserter(movps),
									 [&](IL * il){ return SchedulingProperty::create_shared(dag, il); });

		property addp = nullptr;
		property mulp = nullptr;

		auto compare = [](property &x1, property &x2, property &y1, property &y2) {
				int xNum = 0;
				int yNum = 0;

				if (x1->stepNumberForTMU.has_value())
					xNum++;
				if (x2->stepNumberForTMU.has_value())
					xNum++;

				if (y1->stepNumberForTMU.has_value())
					yNum++;
				if (y2->stepNumberForTMU.has_value())
					yNum++;

				if (xNum > yNum)
					return true;
				else if (xNum < yNum)
					return false;
				/* xNum == yNum */

				int sumX = x1->neighborsNumber + x2->neighborsNumber;
				int sumY = y1->neighborsNumber + y2->neighborsNumber;
				return sumX > sumY;
		};

// XXX this function is defined as macro, because this must be closure (capture variables)
// and for type safety
#define search(X, Y) \
		[&](properties & ps1, properties & ps2, std::vector<X *> & ils1, std::vector<Y *> & ils2) { \
      for (int ip1 = 0; ip1 < ps1.size(); ip1++) { \
        for (int ip2 = 0; ip2 < ps2.size(); ip2++) { \
					if (addp == nullptr && mulp == nullptr) { \
						add = ils1[ip1]; mul = ils2[ip2]; \
						addp = ps1[ip1]; mulp = ps2[ip2]; \
					} else if (compare (addp, mulp, ps1[ip1], ps2[ip2]) && canCombined(ils1[ip1], ils2[ip2])) { \
						add = ils1[ip1]; mul = ils2[ip2]; \
						addp = ps1[ip1]; mulp = ps2[ip2]; \
					} \
				} \
			} \
		} \

		search(intermediate::Operation, intermediate::Operation) (addps, mulps, addOp, mulOp);
		search(intermediate::Operation, intermediate::MoveOperation) (addps, movps, addOp, moves);
		search(intermediate::MoveOperation, intermediate::Operation) (movps, mulps, moves, mulOp);
	}

	if (add)
		dag.erase(add);
	if (mul)
		dag.erase(mul);

	if (auto mov = dynamic_cast<intermediate::MoveOperation *>(add)) {
		add = mov->convertToOperation(true);
	}

	if (auto mov = dynamic_cast<intermediate::MoveOperation *>(mul)) {
		mul = mov->convertToOperation(false);
	}

	if (add == nullptr)
		return mul;
	else if (mul == nullptr)
		return add;

	auto addIL = dynamic_cast<intermediate::Operation *>(add);
	auto mulIL = dynamic_cast<intermediate::Operation *>(mul);
	assert (addIL != nullptr);
	assert (mulIL != nullptr);
	return new intermediate::CombinedOperation(addIL, mulIL);
}


/* Choose an instruction from a vector of instructions.
 * Find the shortest path to reach sending signal of TMU load
 *
 * return the instruction and steps of reaching the instruction
 */
std::pair<int, intermediate::Operation *> InstructionSelector::chooseInstructionForTMUSignal (Ops & ils) {
	int step = std::numeric_limits<int>::max();
	intermediate::Operation * foundIL = nullptr;
	std::for_each(ils.begin(), ils.end(), [&](intermediate::Operation * il){
		auto opt = dag.stepForTMULoad(il);
		if (opt.has_value()) {
			auto currentStep = opt.value();
			if (step > currentStep) {
				step = currentStep;
				foundIL = il;
			}
		}
	});

	if (foundIL == nullptr)
		return std::make_pair(-1, foundIL);
	else
		return std::make_pair(step, foundIL);
}

/* Choose an instruction from a vector of instructions.
 * Find the instruction that has the most neighbors
 */
intermediate::Operation *InstructionSelector::chooseInstructionForNumber(Ops & ils) {
	int max = std::numeric_limits<int>::min();
	intermediate::Operation * maxOp = nullptr;

	std::for_each(ils.begin(), ils.end(), [&](intermediate::Operation * op){
		auto currentMax = dag.getNeighborsNumber(op);
		if (max < currentMax) {
			max = currentMax;
			maxOp = op;
		}
	});

	assert (maxOp != nullptr);
	return maxOp;
}

IL * InstructionSelector::choose() {
	auto & ils = * dag.getRoots();
	assert(ils.size() > 0);

	auto addOp = std::vector<intermediate::Operation *>();
	auto mulOp = std::vector<intermediate::Operation *>();
	auto moves = std::vector<intermediate::MoveOperation *>();
	auto others = std::vector<intermediate::IntermediateInstruction *>();
	auto memorySignal = std::vector<intermediate::Operation *>(std::vector<intermediate::Operation *>());

	std::for_each(ils.begin(), ils.end(), [&](IL *il) {
		if (auto op = dynamic_cast<intermediate::Operation *>(il)) {
			if (op->signal == SIGNAL_LOAD_TMU0 || op->signal == SIGNAL_LOAD_TMU1)
				memorySignal.push_back(op);
			else if (op->op.runsOnAddALU())
				addOp.push_back(op);
			else if (op->op.runsOnMulALU())
				mulOp.push_back(op);
			else
				assert(false);
		} else if (auto mov = dynamic_cast<intermediate::MoveOperation *>(il)) {
			moves.push_back(mov);
		} else {
			others.push_back(il);
		}
	});

	if (ils.size() != addOp.size() + mulOp.size() + moves.size() + others.size() + memorySignal.size()){
		static std::string s = "invalid preparation:";
		throw std::runtime_error(s);
	}

	if (!memorySignal.empty() && canIssueTmuLoad()) {
		assert (memorySignal.size() > 0);
		auto il = memorySignal.at(0);
		return combineOperation(il, addOp, mulOp);
	}

	if (!others.empty()) {
		assert(others.size() > 0);
		auto il = others.at(0);
		dag.erase(il);
		return il;
	}

	return issueCombined (addOp, mulOp, moves);
}

bool InstructionSelector::empty() {
	if (dag.getRoots()->size() == 0) {
		assert (dag.empty());
		return true;
	}

	return false;
}

}
