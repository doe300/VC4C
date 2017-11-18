/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include "../Bitfield.h"
#include "CompilationError.h"
#include "OpCodes.h"

#include <string>

namespace vc4c
{

	namespace qpu_asm
	{

		class Instruction : protected Bitfield<uint64_t>
		{
		public:

			Instruction();

			virtual ~Instruction();

			virtual std::string toASMString() const = 0;

			uint64_t toBinaryCode() const;

			std::string toHexString(bool withAssemblerCode) const;

		protected:

			static std::string toInputRegister(InputMutex mutex, Address regA, Address regB, bool hasImmediate = false);
			static std::string toOutputRegister(bool regFileA, Address reg);
			static std::string toExtrasString(Signaling sig, ConditionCode cond = COND_ALWAYS, SetFlag flags = SetFlag::DONT_SET, Unpack unpack = UNPACK_NOP, Pack pack = PACK_NOP, bool usesOutputA = true, bool usesInputAOrR4 = true);
		};

		std::string toHexString(uint64_t code);
	} // namespace qpu_asm
} // namespace vc4c

#endif /* INSTRUCTION_H */

