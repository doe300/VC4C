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
    struct Register;

    namespace qpu_asm
    {
        /*
         * Base class for machine-code instructions.
         *
         * This class is a simple wrapper around the machine-code of the instruction and provides functions for easier
         * access to the values.
         */
        class Instruction : protected Bitfield<uint64_t>
        {
        public:
            Instruction();
            explicit Instruction(uint64_t code);

            /*
             * Generates a string of custom assembler code for this instruction
             */
            std::string toASMString() const;
            uint64_t toBinaryCode() const;
            std::string toHexString(bool withAssemblerCode) const;

            /*
             * Returns whether this instruction is valid.
             *
             * NOTE: The checking here is very basic and only used to determine the correct instruction-type.
             */
            bool isValidInstruction() const;

            /*
             * Reinterprets this Instruction as the given type
             */
            template <typename T>
            const T* as() const
            {
                static_assert(std::is_convertible<T, Instruction>::value, "");
                auto ptr = reinterpret_cast<const T*>(this);
                if(!ptr->isValidInstruction())
                    throw CompilationError(
                        CompilationStep::CODE_GENERATION, "Invalid assembler instruction", ptr->toASMString());
                return ptr;
            }

            template <typename T>
            bool is() const
            {
                static_assert(std::is_convertible<T, Instruction>::value, "");
                auto ptr = reinterpret_cast<const T*>(this);
                return ptr->isValidInstruction();
            }

            BITFIELD_ENTRY(Sig, Signaling, 60, Quadruple)
            BITFIELD_ENTRY(WriteSwap, WriteSwap, 44, Bit)
            BITFIELD_ENTRY(AddOut, Address, 38, Sextuple)
            BITFIELD_ENTRY(MulOut, Address, 32, Sextuple)

            Register getAddOutput() const;
            Register getMulOutput() const;

        protected:
            static std::string toInputRegister(
                InputMultiplex mux, Address regA, Address regB, bool hasImmediate = false);
            static std::string toOutputRegister(bool regFileA, Address reg);
            static std::string toExtrasString(Signaling sig, ConditionCode cond = COND_ALWAYS,
                SetFlag flags = SetFlag::DONT_SET, Unpack unpack = UNPACK_NOP, Pack pack = PACK_NOP,
                bool usesOutputA = true, bool usesInputAOrR4 = true);
        };

        std::string toHexString(uint64_t code);

        /**
         * Wrapper around an Instruction which contains additional information to display
         */
        struct DecoratedInstruction
        {
            Instruction instruction;
            std::string comment;
            std::string previousComment;

            DecoratedInstruction(Instruction instr, std::string&& comment = "") :
                instruction(instr), comment(std::move(comment))
            {
            }

            /*
             * Generates a string of custom assembler code for this instruction
             */
            std::string toASMString(bool addComments = true) const;
            inline uint64_t toBinaryCode() const
            {
                return instruction.toBinaryCode();
            }
            std::string toHexString(bool withAssemblerCode) const;

        private:
            std::string addComment(std::string s) const;
        };
    } // namespace qpu_asm
} // namespace vc4c

#endif /* INSTRUCTION_H */
