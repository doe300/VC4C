/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_LIBCLANG
#define VC4C_LIBCLANG

#include "Optional.h"

#include <iostream>
#include <string>
#include <vector>

namespace vc4c
{
    namespace precompilation
    {
        void compileLibClang(const std::vector<std::string>& command, std::istream* inputStream,
            std::ostream* outputStream, const Optional<std::string>& inputFile = {},
            const Optional<std::string>& outputFile = {});
    } // namespace precompilation
} // namespace vc4c

#endif /* VC4C_LIBCLANG */
