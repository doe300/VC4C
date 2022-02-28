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
    void testFrontendConversions(std::string sourceFile, vc4c::SourceType destType);
    void testCompilationDataSerialization();
    void testPrecompileStandardLibrary();
    void printProfilingInfo();

private:
    void testEmulation(const vc4c::CompilationData& binary);
};

#endif /* TEST_SPIRVFRONTEND_H */
