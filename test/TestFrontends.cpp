/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestFrontends.h"

#include "GlobalValues.h"
#include "VC4C.h"
#include "asm/Instruction.h"
#include "asm/KernelInfo.h"
#include "spirv/SPIRVHelper.h"
#include "tools.h"
#ifdef SPIRV_FRONTEND
#include "spirv/SPIRVHelper.h"

using namespace vc4c::spirv;
#endif

#include <fstream>
#include <memory>
#include <sstream>

using namespace vc4c;

// TODO add some tests for the different types of (pre) compilation (module, PCH, etc., compile LLVM bin/text)

extern void disassemble(const std::string& input, const std::string& output, const vc4c::OutputMode outputMode);
extern void extractBinary(std::istream& binary, qpu_asm::ModuleInfo& moduleInfo, StableList<Global>& globals,
    std::vector<qpu_asm::Instruction>& instructions);

TestFrontends::TestFrontends()
{
    TEST_ADD(TestFrontends::testSPIRVCapabilitiesSupport);
    TEST_ADD(TestFrontends::testLinking);
    TEST_ADD(TestFrontends::testSourceTypeDetection);
    TEST_ADD(TestFrontends::testDisassembler);

    TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::OPENCL_C);
    TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::LLVM_IR_TEXT);
    TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::LLVM_IR_BIN);

    TEST_ADD(TestFrontends::testKernelAttributes);
}

// out-of-line virtual destructor
TestFrontends::~TestFrontends() = default;

void TestFrontends::testSPIRVCapabilitiesSupport()
{
#ifdef SPIRV_FRONTEND
    /*
     * see  SPIR-V OpenCL environment specification, section 6.2:
     * "An OpenCL 1.2 Embedded Profile platform is guaranteed to support, at least, the following SPIR-V capabilities:
     *   Address, Float16Buffer, Group, Int16, Int8, Kernel, Linkage, LiteralSampler, Vector16
     *  Furthermore, the following capabilities may be supported:
     *   ImageBasic, Int64"
     */
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(spv::Capability::Addresses));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(spv::Capability::Float16Buffer));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(spv::Capability::Groups));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(spv::Capability::Int16));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(spv::Capability::Int8));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(spv::Capability::Kernel));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(spv::Capability::Linkage));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(spv::Capability::LiteralSampler));
    TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(spv::Capability::Vector16));
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
    std::unique_ptr<std::istream> file1(new std::ifstream("./testing/test_linking_1.cl"));
    std::unique_ptr<std::istream> file2(new std::ifstream("./testing/test_linking_2.cl"));
    std::unordered_map<std::istream*, Optional<std::string>> inputs{
        {file0.get(), Optional<std::string>{"./testing/test_linking_0.cl"}},
        {file1.get(), Optional<std::string>{"./testing/test_linking_1.cl"}},
        {file2.get(), Optional<std::string>{"./testing/test_linking_2.cl"}}};

    std::stringstream tmp;
    // extra linking in std-lib tests handling of multiple times linking std-lib, since it will also be linked in for
    // the OpenCL C -> LLVM IR compilation
    auto type = Precompiler::linkSourceCode(inputs, tmp, true);
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

static std::pair<std::stringstream, SourceType> compile(
    std::istream& source, SourceType intermediateType, const std::string& options = "")
{
    // pre-compile to given type and check result type
    Configuration precompConfig{};
    Precompiler precomp{precompConfig, source, Precompiler::getSourceType(source)};

    std::unique_ptr<std::istream> tmp;
    precomp.run(tmp, intermediateType, options);
    auto realIntermediateType = Precompiler::getSourceType(*tmp);

    // compile from given type and emulate code
    std::stringstream out;
    Configuration config;
    config.outputMode = OutputMode::BINARY;
    Compiler::compile(*tmp, out, config, options);
    return std::make_pair(std::move(out), realIntermediateType);
}

void TestFrontends::testCompilation(vc4c::SourceType type)
{
    std::ifstream in("./example/fibonacci.cl");

    auto res = compile(in, type);
    TEST_ASSERT_EQUALS(type, res.second)
    testEmulation(res.first);
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

static constexpr char* ATTRIBUTE_KERNEL = R"(
__attribute__((vec_type_hint(int4)))
__attribute__((work_group_size_hint(2, 2, 3)))
__attribute__((reqd_work_group_size(2, 2, 3)))
__kernel void test() { }
)";

void TestFrontends::testKernelAttributes()
{
    std::stringstream ss(ATTRIBUTE_KERNEL);
    auto res = compile(ss, SourceType::OPENCL_C);
    // don't do anything here, just make sure it compiles
    qpu_asm::ModuleInfo module;
    StableList<Global> globals;
    std::vector<qpu_asm::Instruction> instructions;
    extractBinary(res.first, module, globals, instructions);

    TEST_ASSERT(!module.kernelInfos.empty())
    TEST_ASSERT_EQUALS(uint64_t{0x0000000300020002}, module.kernelInfos[0].workGroupSize)
}
