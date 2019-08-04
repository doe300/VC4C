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

// TODO add some tests for the different types of (pre) compilation (module, PCH, etc., compile LLVM bin/text)

extern void disassemble(const std::string& input, const std::string& output, const vc4c::OutputMode outputMode);

TestFrontends::TestFrontends()
{
    TEST_ADD(TestFrontends::testSPIRVCapabilitiesSupport);
    TEST_ADD(TestFrontends::testLinking);
    TEST_ADD(TestFrontends::testSourceTypeDetection);
    TEST_ADD(TestFrontends::testDisassembler);

    TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::OPENCL_C);
    TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::LLVM_IR_TEXT);
    TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::LLVM_IR_BIN);
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
    TEST_ASSERT(type == SourceType::LLVM_IR_BIN || type == SourceType::SPIRV_BIN)

    std::stringstream out;
    Compiler comp(tmp, out);
    comp.getConfiguration().outputMode = OutputMode::BINARY;
    comp.convert();

    std::vector<std::pair<uint32_t, Optional<std::vector<uint32_t>>>> params;
    params.push_back(std::make_pair(0, Optional<std::vector<uint32_t>>{std::vector<uint32_t>{0}}));
    params.push_back(std::make_pair(0, Optional<std::vector<uint32_t>>{std::vector<uint32_t>{42}}));
    tools::EmulationData data(out, "test_linker", params);
    auto res = tools::emulate(data);

    TEST_ASSERT(res.executionSuccessful)
    TEST_ASSERT_EQUALS(res.results[0].second->at(0), res.results[1].second->at(0))
}

void TestFrontends::testSourceTypeDetection()
{
    {
        std::ifstream in{"./example/fibonacci.cl"};
        TEST_ASSERT_EQUALS(SourceType::OPENCL_C, Precompiler::getSourceType(in))
    }

    {
        std::ifstream in{"./example/fibonacci.ir"};
        TEST_ASSERT_EQUALS(SourceType::LLVM_IR_TEXT, Precompiler::getSourceType(in))
    }

    {
        std::ifstream in{"./testing/formats/test.bc"};
        TEST_ASSERT_EQUALS(SourceType::LLVM_IR_BIN, Precompiler::getSourceType(in))
    }

    {
        std::ifstream in{"./example/fibonacci.spt"};
        TEST_ASSERT_EQUALS(SourceType::SPIRV_TEXT, Precompiler::getSourceType(in))
    }

    {
        std::ifstream in{"./testing/formats/test.spv"};
        TEST_ASSERT_EQUALS(SourceType::SPIRV_BIN, Precompiler::getSourceType(in))
    }

    {
        std::ifstream in{"./testing/formats/test.hex"};
        TEST_ASSERT_EQUALS(SourceType::QPUASM_HEX, Precompiler::getSourceType(in))
    }

    {
        std::ifstream in{"./testing/formats/test.bin"};
        TEST_ASSERT_EQUALS(SourceType::QPUASM_BIN, Precompiler::getSourceType(in))
    }

    {
        std::ifstream in{"./testing/formats/test.txt"};
        TEST_ASSERT_EQUALS(SourceType::UNKNOWN, Precompiler::getSourceType(in))
    }
}

void TestFrontends::testDisassembler()
{
    std::string inputFile{"./testing/formats/test.bin"};
    vc4c::TemporaryFile tmp{};
    disassemble(inputFile, tmp.fileName, OutputMode::HEX);

    std::string originalContent;
    {
        std::ifstream origFile{"./testing/formats/test_disassembled.hex"};
        std::stringstream tmp;
        tmp << origFile.rdbuf();
        originalContent = tmp.str();
    }

    std::string disassembledContent;
    {
        std::unique_ptr<std::istream> disassembledFile;
        tmp.openInputStream(disassembledFile);
        TEST_ASSERT(!!disassembledFile)
        std::stringstream tmp;
        tmp << disassembledFile->rdbuf();
        disassembledContent = tmp.str();
    }

    TEST_ASSERT_EQUALS(originalContent, disassembledContent)
}

void TestFrontends::testCompilation(vc4c::SourceType type)
{
    std::ifstream in("./example/fibonacci.cl");

    // pre-compile to given type and check result type
    Configuration precompConfig{};
    Precompiler precomp{precompConfig, in, Precompiler::getSourceType(in)};

    std::unique_ptr<std::istream> tmp;
    precomp.run(tmp, type);

    TEST_ASSERT_EQUALS(type, Precompiler::getSourceType(*tmp))

    // compile from given type and emulate code
    std::stringstream out;
    Configuration config;
    config.outputMode = OutputMode::BINARY;
    Compiler::compile(*tmp, out, config);

    testEmulation(out);
}

void TestFrontends::testEmulation(std::stringstream& binary)
{
    std::vector<std::pair<uint32_t, Optional<std::vector<uint32_t>>>> params;
    params.push_back(std::make_pair(1, Optional<std::vector<uint32_t>>{}));
    params.push_back(std::make_pair(1, Optional<std::vector<uint32_t>>{}));
    params.push_back(std::make_pair(0, Optional<std::vector<uint32_t>>{std::vector<uint32_t>(16)}));
    tools::EmulationData data(binary, "fibonacci", params);
    auto res = tools::emulate(data);

    TEST_ASSERT(res.executionSuccessful)

    auto& out = *res.results[2].second;
    TEST_ASSERT_EQUALS(2u, out.at(0))
    TEST_ASSERT_EQUALS(3u, out.at(1))
    TEST_ASSERT_EQUALS(5u, out.at(2))
    TEST_ASSERT_EQUALS(8u, out.at(3))
    TEST_ASSERT_EQUALS(13u, out.at(4))
    TEST_ASSERT_EQUALS(21u, out.at(5))
    TEST_ASSERT_EQUALS(34u, out.at(6))
    TEST_ASSERT_EQUALS(55u, out.at(7))
    TEST_ASSERT_EQUALS(89u, out.at(8))
    TEST_ASSERT_EQUALS(144u, out.at(9))
}
