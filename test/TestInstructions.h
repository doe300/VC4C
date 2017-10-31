/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TEST_INSTRUCTIONS_H
#define TEST_INSTRUCTIONS_H

#include "cpptest.h"

class TestInstructions : public Test::Suite
{
public:
	TestInstructions();

	void testConditionCodes();
	void testConstantSaturations();
	void testBitfields();
};

#endif /* TEST_INSTRUCTIONS_H */
