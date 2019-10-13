/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TEST_SPIRVFRONTEND_H
#define TEST_SPIRVFRONTEND_H

#include "cpptest.h"

#include "VC4C.h"

class TestFrontends : public Test::Suite
{
public:
    TestFrontends();
    ~TestFrontends() override;

    void testSPIRVCapabilitiesSupport();
    void testLinking();
    void testSourceTypeDetection();
    void testDisassembler();
    void testCompilation(vc4c::SourceType type);
    void testKernelAttributes();

private:
    void testEmulation(std::stringstream& binary);
};

#endif /* TEST_SPIRVFRONTEND_H */
