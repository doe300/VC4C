/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef KERNELINFO_H
#define KERNELINFO_H

#include "../KernelMetaData.h"
#include "../Units.h"
#include "../performance.h"
#include "../shared/BinaryHeader.h"
#include "config.h"

#include <array>
#include <iostream>
#include <vector>

namespace vc4c
{
    class Method;
    struct Global;

    namespace qpu_asm
    {
        std::size_t writeModule(std::ostream& stream, ModuleHeader& module, OutputMode mode,
            const StableList<Global>& globalData, Byte totalStackFrameSize);

        KernelHeader createKernelHeader(const Method& method, std::size_t initialOffset, std::size_t numInstructions);
    } // namespace qpu_asm
} // namespace vc4c

#endif /* KERNELINFO_H */
