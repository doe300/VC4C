//
// Created by nomaddo on 17/12/18.
//

#ifndef VC4C_DAG_H
#define VC4C_DAG_H

#include "../Graph.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../analysis/DebugGraph.h"

using namespace vc4c;;

using IL = vc4c::intermediate::IntermediateInstruction;

namespace vc4c
{
  class DAG {
	  /* dependency graph */
	  Graph<IL*, Node <IL*, bool>> * graph = new Graph<IL*, Node <IL*, bool>>;
	  const intermediate::BranchLabel* label;

  public:
	  DAG(BasicBlock &bb);
	  std::vector<IL*> getRoots();
	  void erase(IL *);
	  Optional<int> stepForTMULoad(IL * il);
	  int getNeighborsNumber(IL *);
	  void dumpGraph();
	  const intermediate::BranchLabel* getLabel() const;
  };

}
#endif //VC4C_DAG_H
