/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TEST_SPIRVFRONTEND_H
#define TEST_SPIRVFRONTEND_H

#include "cpptest.h"

class TestSPIRVFrontend : public Test::Suite
{
public:
	TestSPIRVFrontend();

	void testCapabilitiesSupport();
};

#endif /* TEST_SPIRVFRONTEND_H */
