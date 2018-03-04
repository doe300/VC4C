/*
 * Author: nomaddo
 *
 * See the file "LICENSE" for the full license governing this code.
 */

// comment instruction for debugging its output

#ifndef VC4C_COMMENTINSTRUCTION_H
#define VC4C_COMMENTINSTRUCTION_H

#include "Instruction.h"

namespace vc4c {
namespace qpu_asm {
class Comment : public Instruction {
public:
	explicit Comment(std::string);
	~Comment() override = default;

	std::string toASMString() const override;
	bool isValidInstruction() const override;
};
}
}

#endif //VC4C_COMMENTINSTRUCTION_H
