/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TEST_OPERATORS_H
#define TEST_OPERATORS_H

#include "cpptest.h"

class TestOperators : public Test::Suite
{
public:
	TestOperators();
	~TestOperators() override;

	void testASR();
	void testCLZ();
	void testSMOD();
	void testSREM();
	void testFMOD();
	void testFREM();
};

#endif /* TEST_OPERATORS_H */
