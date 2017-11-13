/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include <string>

#include "OpCodes.h"
#include "CompilationError.h"
#include "../Bitfield.h"
#include "../Values.h"

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

			static std::string toInputRegister(const InputMutex mutex, const Address regA, const Address regB, const bool hasImmediate = false);
			static std::string toOutputRegister(const bool regFileA, const Address reg);
			static std::string toExtrasString(const Signaling sig, const ConditionCode cond = COND_ALWAYS, const SetFlag flags = SetFlag::DONT_SET, const Unpack unpack = UNPACK_NOP, const Pack pack = PACK_NOP, bool usesOutputA = true, bool usesInputAOrR4 = true);
		};

		std::string toHexString(const uint64_t code);
	}
}

#endif /* INSTRUCTION_H */

