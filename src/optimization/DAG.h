/*
 * Author: nomaddo
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_DAG_H
#define VC4C_DAG_H

#include "../Graph.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../analysis/DebugGraph.h"
#include "log.h"

namespace vc4c
{
		using IL = vc4c::intermediate::IntermediateInstruction;

		enum class DAGDependType { Depend, AntiDepend };

		class DagNode : public Node< IL *, DAGDependType > {
		public:
				using Base = Node< IL *, DAGDependType >;

				explicit DagNode(IL * il) : Base(il) {}
				~DagNode() = default;
		};

		using DagType = Graph< IL *, DagNode > ;

		class DAG {
				/* dependency graph */
				Graph< IL*, DagNode > * graph;
				const intermediate::BranchLabel* label;
				std::vector<IL*> * roots = new std::vector<IL*>();

		public:
				DAG(Method & method, BasicBlock &bb);
				Method & method;
				std::vector<IL*> * getRoots();
				void erase(IL *);
				Optional<int> stepForTMULoad(IL * il);
				int getNeighborsNumber(IL *);
				void dumpGraph();
				const intermediate::BranchLabel* getLabel() const;
				void addDependency (IL *, IL *);
				bool empty();
  };
}

#endif //VC4C_DAG_H
