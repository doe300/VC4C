/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef CODEGENERATOR_H
#define CODEGENERATOR_H

#include "../performance.h"
#include "Instruction.h"
#include "config.h"

#include <map>
#include <memory>
#include <mutex>
#include <ostream>
#include <vector>

namespace vc4c
{
    class Method;
    class Module;

    namespace qpu_asm
    {
        class CodeGenerator
        {
        public:
            explicit CodeGenerator(const Module& module, const Configuration& config = {});

            std::size_t writeOutput(std::ostream& stream);
            void toMachineCode(Method& kernel);

        private:
            Configuration config;
            const Module& module;
            std::map<Method*, FastAccessList<qpu_asm::DecoratedInstruction>> allInstructions;
            std::mutex instructionsLock;

            /*
             * NOTE: Instruction to Assembler mapping can be run in parallel for different methods,
             * so no static or non-constant global data can be used
             */
            const FastAccessList<qpu_asm::DecoratedInstruction>& generateInstructions(Method& method);
        };
    } // namespace qpu_asm
} // namespace vc4c

#endif /* CODEGENERATOR_H */
