//
// Created by nomaddo on 17/12/18.
//

#ifndef VC4C_DAG_H
#define VC4C_DAG_H

#include "../Graph.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../DebugGraph.h"

using namespace vc4c;;

using IL = vc4c::intermediate::IntermediateInstruction;

namespace vc4c
{
  class DAG {
    /* dependency graph */
    Graph<IL*, Node <IL*, bool>> * graph = new Graph<IL*, Node <IL*, bool>>;

  public:
    DAG(BasicBlock &bb);

    void getRoots();

    void addAsRoot();
    void addDepend();
    void remove();

  };

}
#endif //VC4C_DAG_H
