/*
 * General header for the public C++ interface, containing all required headers
 *
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_H
#define VC4C_H

#include "config.h"
#include "helper.h"
#include "Compiler.h"
#include "Precompiler.h"

#include <iostream>

namespace vc4c
{
	std::size_t disassemble(std::istream& binary, std::ostream& output, const OutputMode outputMode = OutputMode::HEX);
}

#endif /* VC4C_H */
