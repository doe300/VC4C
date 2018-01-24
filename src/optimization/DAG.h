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

namespace vc4c
{
  enum class DAGDependType { Depend, AntiDepend };

  using IL = vc4c::intermediate::IntermediateInstruction;
  using DagType = Graph<IL*, Node <IL*, DAGDependType>>;

  class DAG {
	  /* dependency graph */
	  DagType * graph = new Graph<IL*, Node <IL*, DAGDependType>>;
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
