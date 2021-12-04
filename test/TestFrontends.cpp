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
#include "precompilation/FrontendCompiler.h"
#include "spirv/SPIRVHelper.h"
#include "tool_paths.h"
#include "tools.h"

using namespace vc4c::spirv;

#include <algorithm>
#include <fstream>
#include <memory>
#include <sstream>

using namespace vc4c;

// TODO add some tests for the different types of (pre) compilation (module, PCH, etc., compile LLVM bin/text)

extern void disassemble(const std::string& input, const std::string& output, const vc4c::OutputMode outputMode);
extern void extractBinary(const CompilationData& binary, ModuleHeader& module, StableList<Global>& globals,
    std::vector<qpu_asm::Instruction>& instructions);

TestFrontends::TestFrontends()
{
    TEST_ADD(TestFrontends::testSPIRVCapabilitiesSupport);
    if(hasLLVMFrontend())
    {
        // FIXME this SEGFAULTs in llvm-spirv translator
        TEST_ADD(TestFrontends::testLinking);
    }

    TEST_ADD(TestFrontends::testSourceTypeDetection);
    TEST_ADD(TestFrontends::testDisassembler);

    TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::OPENCL_C);
    if(hasLLVMFrontend())
    {
        TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::LLVM_IR_TEXT);
    }
    TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::LLVM_IR_BIN);
    if(precompilation::findToolLocation("llvm-spirv", SPIRV_LLVM_SPIRV_PATH))
    {
        TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::SPIRV_BIN);
        TEST_ADD_SINGLE_ARGUMENT(TestFrontends::testCompilation, SourceType::SPIRV_TEXT);
    }

    TEST_ADD(TestFrontends::testKernelAttributes);

    // OpenCL -> XYZ conversions are already tested with #testCompilation, so don't run them again here
    TEST_ADD_TWO_ARGUMENTS(
        TestFrontends::testFrontendConversions, std::string{"./example/fibonacci.ir"}, SourceType::LLVM_IR_BIN);
    TEST_ADD_TWO_ARGUMENTS(
        TestFrontends::testFrontendConversions, std::string{"./testing/formats/test.bc"}, SourceType::LLVM_IR_TEXT);
}

// out-of-line virtual destructor
TestFrontends::~TestFrontends() = default;

void TestFrontends::testSPIRVCapabilitiesSupport()
{
    /*
     * see  SPIR-V OpenCL environment specification, section 6.2:
     * "An OpenCL 1.2 Embedded Profile platform is guaranteed to support, at least, the following SPIR-V capabilities:
     *   Address, Float16Buffer, Group, Int16, Int8, Kernel, Linkage, LiteralSampler, Vector16
     *  Furthermore, the following capabilities may be supported:
     *   ImageBasic, Int64"
     */
    TEST_ASSERT_EQUALS(ParseResultCode::SUCCESS, checkCapability(spv::Capability::Addresses));
    TEST_ASSERT_EQUALS(ParseResultCode::SUCCESS, checkCapability(spv::Capability::Float16Buffer));
    TEST_ASSERT_EQUALS(ParseResultCode::SUCCESS, checkCapability(spv::Capability::Groups));
    TEST_ASSERT_EQUALS(ParseResultCode::SUCCESS, checkCapability(spv::Capability::Int16));
    TEST_ASSERT_EQUALS(ParseResultCode::SUCCESS, checkCapability(spv::Capability::Int8));
    TEST_ASSERT_EQUALS(ParseResultCode::SUCCESS, checkCapability(spv::Capability::Kernel));
    TEST_ASSERT_EQUALS(ParseResultCode::SUCCESS, checkCapability(spv::Capability::Linkage));
    TEST_ASSERT_EQUALS(ParseResultCode::SUCCESS, checkCapability(spv::Capability::LiteralSampler));
    TEST_ASSERT_EQUALS(ParseResultCode::SUCCESS, checkCapability(spv::Capability::Vector16));
}

void TestFrontends::testLinking()
{
    if(!Precompiler::isLinkerAvailable())
    {
        // to not unexpectedly fail if no linker is available
        return;
    }

    std::vector<CompilationData> inputs{
        CompilationData{"./testing/test_linking_0.cl"},
        CompilationData{"./testing/test_linking_1.cl"},
        CompilationData{"./testing/test_linking_2.cl"},
    };

    // extra linking in std-lib tests handling of multiple times linking std-lib, since it will also be linked in for
    // the OpenCL C -> LLVM IR compilation
    auto intermediate = Precompiler::linkSourceCode(inputs, true);
    TEST_ASSERT(intermediate.getType() == SourceType::LLVM_IR_BIN || intermediate.getType() == SourceType::SPIRV_BIN)

    Configuration config{};
    config.outputMode = OutputMode::BINARY;
    auto out = Compiler::compile(intermediate, config);
    TEST_ASSERT_EQUALS(SourceType::QPUASM_BIN, out.first.getType());

    std::vector<std::pair<uint32_t, Optional<std::vector<uint32_t>>>> params;
    params.push_back(std::make_pair(0, Optional<std::vector<uint32_t>>{std::vector<uint32_t>{0}}));
    params.push_back(std::make_pair(0, Optional<std::vector<uint32_t>>{std::vector<uint32_t>{42}}));
    tools::EmulationData data(out.first, "test_linker", params);
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
        TEST_ASSERT_EQUALS(SourceType::OPENCL_C, Precompiler::getSourceType(in))
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

static std::pair<CompilationData, SourceType> compile(
    const CompilationData& source, SourceType intermediateType, const std::string& options = "")
{
    // pre-compile to given type and check result type
    Configuration precompConfig{};
    auto tmp = Precompiler::precompile(source, intermediateType, precompConfig, options);

    // compile from given type and emulate code
    Configuration config;
    config.outputMode = OutputMode::BINARY;
    auto result = Compiler::compile(tmp, config, options);
    return std::make_pair(result.first, tmp.getType());
}

void TestFrontends::testCompilation(vc4c::SourceType type)
{
    auto res = compile(CompilationData{"./example/fibonacci.cl", SourceType::OPENCL_C}, type);
    TEST_ASSERT_EQUALS(type, res.second)
    testEmulation(res.first);
}

void TestFrontends::testEmulation(const vc4c::CompilationData& binary)
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

static const std::string ATTRIBUTE_KERNEL = R"(
__attribute__((vec_type_hint(int4)))
__attribute__((work_group_size_hint(2, 2, 3)))
__attribute__((reqd_work_group_size(2, 2, 3)))
__kernel void test() { }
)";

void TestFrontends::testKernelAttributes()
{
    auto res = compile(
        CompilationData{ATTRIBUTE_KERNEL.begin(), ATTRIBUTE_KERNEL.end(), SourceType::OPENCL_C}, SourceType::OPENCL_C);
    // don't do anything here, just make sure it compiles
    ModuleHeader module;
    StableList<Global> globals;
    std::vector<qpu_asm::Instruction> instructions;
    extractBinary(res.first, module, globals, instructions);

    TEST_ASSERT(!module.kernels.empty())
    const auto& kernel = module.kernels[0];
    TEST_ASSERT_EQUALS(2, kernel.workGroupSize[0])
    TEST_ASSERT_EQUALS(2, kernel.workGroupSize[1])
    TEST_ASSERT_EQUALS(3, kernel.workGroupSize[2])
    TEST_ASSERT_EQUALS(0, kernel.workItemMergeFactor)
    TEST_ASSERT(std::any_of(kernel.metaData.begin(), kernel.metaData.end(),
        [](const MetaData& meta) { return meta.to_string(false) == "vec_type_hint(int4)"; }))
    TEST_ASSERT(std::any_of(kernel.metaData.begin(), kernel.metaData.end(),
        [](const MetaData& meta) { return meta.to_string(false) == "work_group_size_hint(2, 2, 3)"; }))
    TEST_ASSERT(std::any_of(kernel.metaData.begin(), kernel.metaData.end(),
        [](const MetaData& meta) { return meta.to_string(false) == "reqd_work_group_size(2, 2, 3)"; }))
}

void TestFrontends::testFrontendConversions(std::string sourceFile, vc4c::SourceType destType)
{
    Configuration precompConfig{};
    std::ifstream fis{sourceFile};

    // 1. check for file path
    {
        auto tmp = Precompiler::precompile(CompilationData{sourceFile}, destType, precompConfig, "");
        TEST_ASSERT(tmp);
        TEST_ASSERT_EQUALS(destType, tmp.getType());
        std::stringstream ss;
        tmp.readInto(ss);
        TEST_ASSERT_EQUALS(destType, Precompiler::getSourceType(ss));
    }

    // 2. check for in-memory source
    {
        auto tmp = Precompiler::precompile(CompilationData{fis}, destType, precompConfig, "");
        TEST_ASSERT(tmp);
        TEST_ASSERT_EQUALS(destType, tmp.getType());
        std::stringstream ss;
        tmp.readInto(ss);
        TEST_ASSERT_EQUALS(destType, Precompiler::getSourceType(ss));
    }
}
