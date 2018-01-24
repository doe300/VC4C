/*
 * Author: nomaddo
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <tuple>
#include "Scheduler.h"
#include <memory>
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

	logging::debug() << "doScheduling" << std::endl;
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
		if (first.has_value() && p.first.has_value())
			return second < p.second;
		else if (first.has_value())
			return true;
		else if (p.first.has_value())
			return false;
		else if (! first.has_value() && ! p.first.has_value())
			return second < p.second;
	}

    Optional<int> first;
    int second;
    IL * third;

    SchedulingProperty(Optional<int> opt, int n, IL * il) : first(opt), second(n), third(il) {}
};

IL * InstructionSelector::issueCombined (Ops & addOp, Ops & mulOp, std::vector<intermediate::MoveOperation*> & moves) {
    auto addp = std::vector<std::shared_ptr<SchedulingProperty>>(addOp.size());
    auto mulp = std::vector<std::shared_ptr<SchedulingProperty>>(mulOp.size());
    auto movp = std::vector<std::shared_ptr<SchedulingProperty>>(moves.size());

    auto getProperties = [&](IL *il) {
        auto opt = dag.stepForTMULoad(il);
        auto neighbors = dag.getNeighborsNumber(il);

        return std::shared_ptr<SchedulingProperty>(new SchedulingProperty(std::move(opt), neighbors, il));
    };

    for (int i = 0; i < addp.size(); i++) {
        addp[i] = getProperties(addOp[i]);
    }
    for (int i = 0; i < mulp.size(); i++) {
        mulp[i] = getProperties(mulOp[i]);
    }
    for (int i = 0; i < movp.size(); i++) {
        movp[i] = getProperties(moves[i]);
    }

    std::sort(addp.begin(), addp.end());
    std::sort(mulp.begin(), mulp.end());
    std::sort(movp.begin(), movp.end());

    auto compare = [](std::shared_ptr<SchedulingProperty> &x1, std::shared_ptr<SchedulingProperty> &x2,
                      std::shared_ptr<SchedulingProperty> &y1, std::shared_ptr<SchedulingProperty> &y2) {
        int xNum = 0;
        int yNum = 0;

        if (x1->first.has_value())
            xNum++;
        if (x2->first.has_value())
            xNum++;

        if (y1->first.has_value())
            yNum++;
        if (y2->first.has_value())
            yNum++;

        if (xNum > yNum)
            return true;
        else if (xNum < yNum)
            return false;
        /* xNum == yNum */

        int sumX = x1->second + x2->second;
        int sumY = y1->second + y2->second;
        return sumX > sumY;
    };

    IL *addIL = nullptr;
    IL *mulIL = nullptr;

    if (addp.size() == 0) {
        if (mulp.size() == 0) {
            assert (movp.size() > 0);
            addIL = movp[0].get()->third;
            mulIL = nullptr;
        } else {
            if (movp.size() > 0) {
                addIL = movp[0].get()->third;
            }
            mulIL = mulp[0].get()->third;
        }
    } else {
        if (mulp.size() == 0) {
            if (movp.size() == 0) {
                addIL = addp[0].get()->third;
            } else {
                addIL = addp[0].get()->third;
                mulIL = movp[0].get()->third;
            }
        } else {
            if (movp.size() == 0) {
                addIL = addp[0].get()->third;
                mulIL = mulp[0].get()->third;
            } else {
                bool x = compare(*addp.begin(), *mulp.begin(), *addp.begin(), *movp.begin());
                bool y = compare(*addp.begin(), *movp.begin(), *mulp.begin(), *movp.begin());

                if (x) {
                    if (y) {
                        addIL = (*movp.begin()).get()->third;
                        mulIL = (*mulp.begin()).get()->third;
                    } else {
                        addIL = (*addp.begin()).get()->third;
                        mulIL = (*movp.begin()).get()->third;
                    }
                } else if (y) {
                    addIL = (*addp.begin()).get()->third;
                    mulIL = (*movp.begin()).get()->third;
                } else {
                    addIL = (*addp.begin()).get()->third;
                    mulIL = (*mulp.begin()).get()->third;
                }
            }
        }
    }

    dag.erase(addIL);
    dag.erase(mulIL);

    if (auto mov = dynamic_cast<intermediate::MoveOperation *>(addIL)) {
        addIL = mov->convertToOperation(true);
    }

    if (auto mov = dynamic_cast<intermediate::MoveOperation *>(mulIL)) {
        mulIL = mov->convertToOperation(false);
    }

    if (addIL == nullptr)
      return mulIL;
    else if (mulIL == nullptr)
      return addIL;

	auto add = dynamic_cast<intermediate::Operation *>(addIL);
	auto mul = dynamic_cast<intermediate::Operation *>(mulIL);
	assert (add != nullptr);
	assert (mul != nullptr);
	return new intermediate::CombinedOperation(add, mul);
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

	auto addOp = std::vector<intermediate::Operation *>(std::vector<intermediate::Operation *>());
	auto mulOp = std::vector<intermediate::Operation *>(std::vector<intermediate::Operation *>());
	auto moves = std::vector<intermediate::MoveOperation *>(std::vector<intermediate::MoveOperation *>());
	auto others = std::vector<intermediate::IntermediateInstruction *>(std::vector<intermediate::IntermediateInstruction *>());
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
