/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestFrontends.h"

#include "VC4C.h"
#include "spirv/SPIRVHelper.h"
#include "tools.h"
#ifdef SPIRV_HEADER
#include SPIRV_PARSER_HEADER

using namespace vc4c::spirv2qasm;
#endif

#include <fstream>
#include <memory>
#include <sstream>

using namespace vc4c;

TestFrontends::TestFrontends()
{
    TEST_ADD(TestFrontends::testSPIRVCapabilitiesSupport);
    TEST_ADD(TestFrontends::testLinking);
}

// out-of-line virtual destructor
TestFrontends::~TestFrontends() = default;

void TestFrontends::testSPIRVCapabilitiesSupport()
{
#ifdef SPIRV_HEADER
    /*
     * see  SPIR-V OpenCL environment specification, section 6.2:
     * "An OpenCL 1.2 Embedded Profile platform is guaranteed to support, at least, the following SPIR-V capabilities:
     *   Address, Float16Buffer, Group, Int16, Int8, Kernel, Linkage, LiteralSampler, Vector16
     *  Furthermore, the following capabilities may be supported:
     *   ImageBasic, Int64"
     */
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityAddresses));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityFloat16Buffer));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityGroups));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityInt16));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityInt8));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityKernel));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityLinkage));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityLiteralSampler));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityVector16));
#endif
}

void TestFrontends::testLinking()
{
    if(!Precompiler::isLinkerAvailable())
    {
        // to not unexpectedly fail if no linker is available
        return;
    }

    std::unique_ptr<std::istream> file0(new std::ifstream("./testing/test_linking_0.cl"));
    std::unique_ptr<std::istream> file1(new std::ifstream("./testing/test_linking_0.cl"));
    std::unordered_map<std::istream*, Optional<std::string>> inputs{
        {file0.get(), Optional<std::string>{"./testing/test_linking_0.cl"}},
        {file1.get(), Optional<std::string>{"./testing/test_linking_1.cl"}}};

    std::stringstream tmp;
    auto type = Precompiler::linkSourceCode(inputs, tmp);
    TEST_ASSERT(type == SourceType::LLVM_IR_BIN || type == SourceType::SPIRV_BIN);

    std::stringstream out;
    Compiler comp(tmp, out);
    comp.getConfiguration().outputMode = OutputMode::BINARY;
    comp.convert();

    std::vector<std::pair<uint32_t, Optional<std::vector<uint32_t>>>> params;
    params.push_back(std::make_pair(0, Optional<std::vector<uint32_t>>{std::vector<uint32_t>{0}}));
    params.push_back(std::make_pair(0, Optional<std::vector<uint32_t>>{std::vector<uint32_t>{42}}));
    tools::EmulationData data(out, "test_linker", params);
    auto res = tools::emulate(data);

    TEST_ASSERT(res.executionSuccessful);
    TEST_ASSERT_EQUALS(res.results[0].second->at(0), res.results[1].second->at(0));
}