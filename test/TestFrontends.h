/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TEST_SPIRVFRONTEND_H
#define TEST_SPIRVFRONTEND_H

#include "cpptest.h"

class TestFrontends : public Test::Suite
{
public:
	TestFrontends();
	~TestFrontends() override;

	void testSPIRVCapabilitiesSupport();
	void testLinking();
};

#endif /* TEST_SPIRVFRONTEND_H */
