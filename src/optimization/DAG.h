/*
 * Author: nomaddo
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_DAG_H
#define VC4C_DAG_H

#include "../Graph.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../DebugGraph.h"

using IL = vc4c::intermediate::IntermediateInstruction;

namespace vc4c
{
  class DAG {
    /* dependency graph */
    Graph<IL*, Node <IL*, bool>> * graph = new Graph<IL*, Node <IL*, bool>>;

  public:
    DAG(BasicBlock &bb);
  };

}
#endif //VC4C_DAG_H
