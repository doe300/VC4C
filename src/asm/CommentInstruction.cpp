/*
 * Author: nomaddo
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "CommentInstruction.h"

vc4c::qpu_asm::Comment::Comment(std::string s) : Instruction(0x000000) {
	comment = s;
}

std::string vc4c::qpu_asm::Comment::toASMString() const {
	return addComment("");
}

bool vc4c::qpu_asm::Comment::isValidInstruction() const {
	return true;
}


