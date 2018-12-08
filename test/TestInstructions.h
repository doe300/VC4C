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
	~TestInstructions() override;

	void testConditionCodes();
	void testUnpackModes();
	void testPackModes();
	void testConstantSaturations();
	void testBitfields();
	void testOpCodes();
	void testOpCodeProperties();
};

#endif /* TEST_INSTRUCTIONS_H */
