/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef REGISTER_ALLOCATION_H
#define REGISTER_ALLOCATION_H

#include "../Values.h"

#include <vector>

namespace vc4c
{

	namespace qpu_asm
	{

		/*
		 * Register allocation maps all locals in use to registers on the VC4 GPU.
		 *
		 * The VideoCore IV GPU has:
		 * - two register-files with 32 general-purpose physical registers
		 * - 6 accumulators (of which 4 can be used for "normal" calculations)
		 *
		 * Following restrictions apply (see Broadcom Documentation, page 37):
		 * - a register (physical or accumulator) can only be used by one local at a time
		 * - an instruction can only read from one physical register per register-file, so two physical input registers for an instruction must be allocated on separate  register-files A and B
		 * - reading a small immediate value is equivalent to reading from register-file B, so the second input cannot be from register-file B (or another distinct small immediate)
		 * - a physical register that is written to can't be accessed (read) in next instruction
		 * - unpacking operations can only be executed on register-file A
		 * - packing operations can only be executed on register-file A
		 * - a full vector rotation cannot be executed on physical registers, so its inputs must be accumulators
		 * - a vector-rotation by accumulator r5 cannot follow an instruction writing to r5
		 * - a vector-rotation of an accumulator cannot follow an instruction writing to that accumulator
		 *
		 * For the best performance (the least delay-instructions required to be inserted), as much locals as possible should be mapped to accumulators.
		 */

//general purpose registers, with no special functions
		const std::vector<Register> GP_REGISTERS = { { RegisterFile::PHYSICAL_A, 0 }, { RegisterFile::PHYSICAL_B, 0 }, { RegisterFile::PHYSICAL_A, 1 }, { RegisterFile::PHYSICAL_B, 1 }, { RegisterFile::PHYSICAL_A, 2 }, {
				RegisterFile::PHYSICAL_B, 2 }, { RegisterFile::PHYSICAL_A, 3 }, { RegisterFile::PHYSICAL_B, 3 }, { RegisterFile::PHYSICAL_A, 4 }, { RegisterFile::PHYSICAL_B, 4 }, { RegisterFile::PHYSICAL_A, 5 }, { RegisterFile::PHYSICAL_B,
				5 }, { RegisterFile::PHYSICAL_A, 6 }, { RegisterFile::PHYSICAL_B, 6 }, { RegisterFile::PHYSICAL_A, 7 }, { RegisterFile::PHYSICAL_B, 7 }, { RegisterFile::PHYSICAL_A, 8 }, { RegisterFile::PHYSICAL_B, 8 }, {
				RegisterFile::PHYSICAL_A, 9 }, { RegisterFile::PHYSICAL_B, 9 }, { RegisterFile::PHYSICAL_A, 10 }, { RegisterFile::PHYSICAL_B, 10 }, { RegisterFile::PHYSICAL_A, 11 }, { RegisterFile::PHYSICAL_B, 11 }, {
				RegisterFile::PHYSICAL_A, 12 }, { RegisterFile::PHYSICAL_B, 12 }, { RegisterFile::PHYSICAL_A, 13 }, { RegisterFile::PHYSICAL_B, 13 }, { RegisterFile::PHYSICAL_A, 14 }, { RegisterFile::PHYSICAL_B, 14 }, {
				RegisterFile::PHYSICAL_A, 15 }, { RegisterFile::PHYSICAL_B, 15 }, { RegisterFile::PHYSICAL_A, 16 }, { RegisterFile::PHYSICAL_B, 16 }, { RegisterFile::PHYSICAL_A, 17 }, { RegisterFile::PHYSICAL_B, 17 }, {
				RegisterFile::PHYSICAL_A, 18 }, { RegisterFile::PHYSICAL_B, 18 }, { RegisterFile::PHYSICAL_A, 19 }, { RegisterFile::PHYSICAL_B, 19 }, { RegisterFile::PHYSICAL_A, 20 }, { RegisterFile::PHYSICAL_B, 20 }, {
				RegisterFile::PHYSICAL_A, 21 }, { RegisterFile::PHYSICAL_B, 21 }, { RegisterFile::PHYSICAL_A, 22 }, { RegisterFile::PHYSICAL_B, 22 }, { RegisterFile::PHYSICAL_A, 23 }, { RegisterFile::PHYSICAL_B, 23 }, {
				RegisterFile::PHYSICAL_A, 24 }, { RegisterFile::PHYSICAL_B, 24 }, { RegisterFile::PHYSICAL_A, 25 }, { RegisterFile::PHYSICAL_B, 25 }, { RegisterFile::PHYSICAL_A, 26 }, { RegisterFile::PHYSICAL_B, 26 }, {
				RegisterFile::PHYSICAL_A, 27 }, { RegisterFile::PHYSICAL_B, 27 }, { RegisterFile::PHYSICAL_A, 28 }, { RegisterFile::PHYSICAL_B, 28 }, { RegisterFile::PHYSICAL_A, 29 }, { RegisterFile::PHYSICAL_B, 29 }, {
				RegisterFile::PHYSICAL_A, 30 }, { RegisterFile::PHYSICAL_B, 30 }, { RegisterFile::PHYSICAL_A, 31 }, { RegisterFile::PHYSICAL_B, 31 } };

//accumulators can only be written, reading from them is handled via input-mux values (see ALUInstruction)
		const std::vector<Register> ACCUMULATORS = { { RegisterFile::ACCUMULATOR, 32 },   //ACC0
				{ RegisterFile::ACCUMULATOR, 33 },   //ACC1
				{ RegisterFile::ACCUMULATOR, 34 },   //ACC2
				{ RegisterFile::ACCUMULATOR, 35 }  //ACC3
				//ACC4 and ACC5 have special meanings
		};
	} // namespace qpu_asm
} // namespace vc4c

#endif /* REGISTER_ALLOCATION_H */

