/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestMemoryAccess.h"

#include "emulation_helper.h"
#include "test_cases.h"

using namespace vc4c;
using namespace vc4c::tools;

static const std::string MEMORY_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test(__global TYPE* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);

  STORAGE TYPE p11[16];
  STORAGE CAT(TYPE,16) p12[1];
  STORAGE TYPE p21[16];
  STORAGE CAT(TYPE,16) p22[1];

  STORAGE TYPE p31[16];
  STORAGE CAT(TYPE,16) p32[1];

  CAT(TYPE,16) t = vload16(gid, in);

  // test vstore + assignment load
  vstore16(t, 0, p11);
  vstore16(t + (TYPE)3, 0, (STORAGE TYPE*)p12);

  CAT(TYPE,16) t1 = *((STORAGE CAT(TYPE,16)*)p11);
  CAT(TYPE,16) t2 = *p12;

  // test assignment store + vload
  *((STORAGE CAT(TYPE,16)*)p21) = t + (TYPE)7;
  *p22 = t + (TYPE)11;

  CAT(TYPE,16) t3 = vload16(0, p21);
  CAT(TYPE,16) t4 = vload16(0, (STORAGE TYPE*)p22);

  // test vstore + vload
  vstore16(t + (TYPE)13, 0, p31);
  vstore16(t + (TYPE)17, 0, (STORAGE TYPE*)p32);

  CAT(TYPE,16) t5 = vload16(0, p31);
  CAT(TYPE,16) t6 = vload16(0, (STORAGE TYPE*)p32);

  vstore16(t1 + t2 + t3 + t4 + t5 + t6, gid, out);
}
)";

static const std::string GLOBAL_MEMORY_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test(__global TYPE* out, const __global TYPE* in, __global TYPE* p11, __global CAT(TYPE,16)* p12, __global TYPE* p21, __global CAT(TYPE,16)* p22, __global TYPE* p31, __global CAT(TYPE,16)* p32) {
  size_t gid = get_global_id(0);

  CAT(TYPE,16) t = vload16(gid, in);

  // test vstore + assignment load
  vstore16(t, 0, p11);
  vstore16(t + (TYPE)3, 0, (__global TYPE*)p12);

  CAT(TYPE,16) t1 = *((__global CAT(TYPE,16)*)p11);
  CAT(TYPE,16) t2 = *p12;

  // test assignment store + vload
  *((__global CAT(TYPE,16)*)p21) = t + (TYPE)7;
  *p22 = t + (TYPE)11;

  CAT(TYPE,16) t3 = vload16(0, p21);
  CAT(TYPE,16) t4 = vload16(0, (__global TYPE*)p22);

  // test vstore + vload
  vstore16(t + (TYPE)13, 0, p31);
  vstore16(t + (TYPE)17, 0, (__global TYPE*)p32);

  CAT(TYPE,16) t5 = vload16(0, p31);
  CAT(TYPE,16) t6 = vload16(0, (__global TYPE*)p32);

  vstore16(t1 + t2 + t3 + t4 + t5 + t6, gid, out);
}
)";

TestMemoryAccess::TestMemoryAccess(const Configuration& config) : TestEmulator(false, config)
{
    TEST_ADD(TestMemoryAccess::testPrivateStorage);
    TEST_ADD(TestMemoryAccess::testLocalStorage);
    TEST_ADD(TestMemoryAccess::testConstantStorage);
    TEST_ADD(TestMemoryAccess::testRegisterStorage);

    TEST_ADD(TestMemoryAccess::testVPMWrites);
    TEST_ADD(TestMemoryAccess::testVPMReads);

    TEST_ADD(TestMemoryAccess::testVectorLoadStoreCharPrivate);
    TEST_ADD(TestMemoryAccess::testVectorLoadStoreCharLocal);
    TEST_ADD(TestMemoryAccess::testVectorLoadStoreCharGlobal);
    TEST_ADD(TestMemoryAccess::testVectorLoadStoreShortPrivate);
    TEST_ADD(TestMemoryAccess::testVectorLoadStoreShortLocal);
    TEST_ADD(TestMemoryAccess::testVectorLoadStoreShortGlobal);
    TEST_ADD(TestMemoryAccess::testVectorLoadStoreIntPrivate);
    TEST_ADD(TestMemoryAccess::testVectorLoadStoreIntLocal);
    TEST_ADD(TestMemoryAccess::testVectorLoadStoreIntGlobal);
}

void TestMemoryAccess::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result);
}

template <typename T>
static void testPrivateLocalFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, MEMORY_FUNCTION, options);

    auto in = generateInput<T, 16>(true, -10, 10);

    auto out = runEmulation<T, T, 16, 1>(code, {in});
    auto storage = options.find("-DSTORAGE=") + std::string("-DSTORAGE=").size();
    auto type = options.find("-DTYPE=") + std::string("-DTYPE=").size();
    checkUnaryResults<T, T>(in, out,
        [](T val) -> T { return val + (val + 3) + (val + 7) + (val + 11) + (val + 13) + (val + 17); },
        options.substr(type, options.find(' ', type) - type) +
            options.substr(storage, options.find(' ', storage) - storage),
        onError);
}

template <typename T>
static void testGlobalFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, GLOBAL_MEMORY_FUNCTION, options);

    auto in = generateInput<T, 16>(true, -10, 10);
    auto p11 = generateInput<T, 16>(true);
    auto p12 = generateInput<T, 16>(true);
    auto p21 = generateInput<T, 16>(true);
    auto p22 = generateInput<T, 16>(true);
    auto p31 = generateInput<T, 16>(true);
    auto p32 = generateInput<T, 16>(true);

    auto out = runEmulation<T, T, 16, 1>(code, {in, p11, p12, p21, p22, p31, p32});
    auto type = options.find("-DTYPE=") + std::string("-DTYPE=").size();
    checkUnaryResults<T, T>(in, out,
        [](T val) -> T { return val + (val + 3) + (val + 7) + (val + 11) + (val + 13) + (val + 17); },
        options.substr(type, options.find(' ', type) - type) + "__global", onError);
}

void TestMemoryAccess::testPrivateStorage()
{
    const std::vector<uint32_t> expected{
        14, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 14, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28};
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl");

    EmulationData data;
    data.kernelName = "test_private_storage";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.dimensions = 3;
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {12, 1, 1};
    data.workGroup.numGroups = {2, 1, 1};

    // parameter 0 is the input
    data.parameter.emplace_back(0, std::vector<uint32_t>(24, 7));
    // parameter 1 is the output
    data.parameter.emplace_back(0, std::vector<uint32_t>(24));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(2u, result.results.size());

    if(expected != result.results.at(1).second.value())
    {
        auto expectedIt = expected.begin();
        auto resultIt = result.results.at(1).second->end();
        while(expectedIt != expected.end())
        {
            TEST_ASSERT_EQUALS(*expectedIt, *resultIt);
            ++resultIt;
            ++expectedIt;
        }
    }
}

void TestMemoryAccess::testLocalStorage()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl");

    EmulationData data;
    data.kernelName = "test_local_storage";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.dimensions = 3;
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {12, 1, 1};
    data.workGroup.numGroups = {2, 1, 1};

    // parameter 0 is the input
    data.parameter.emplace_back(0, std::vector<uint32_t>(24, 7));
    // parameter 1 is the output
    data.parameter.emplace_back(0, std::vector<uint32_t>(24));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(2u, result.results.size());

    // actual results are indeterministic, since the order of the loads/stores across work-items is not guaranteed
    for(auto res : result.results.at(1).second.value())
    {
        if(res % 7 != 0)
        {
            TEST_ASSERT_EQUALS(7, res);
        }
    }
}

void TestMemoryAccess::testConstantStorage()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl");

    EmulationData data;
    data.kernelName = "test_constant_storage";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.dimensions = 3;
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {12, 1, 1};
    data.workGroup.numGroups = {1, 1, 1};

    // parameter 0 is the output
    data.parameter.emplace_back(0, std::vector<uint32_t>(12));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(1u, result.results.size());
    TEST_ASSERT_EQUALS(
        std::string("Hello World"), std::string(reinterpret_cast<const char*>(result.results[0].second->data())));
}

void TestMemoryAccess::testRegisterStorage()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl");

    EmulationData data;
    data.kernelName = "test_register_storage";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.dimensions = 3;
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {12, 1, 1};
    data.workGroup.numGroups = {1, 1, 1};

    // parameter 0 is the output
    data.parameter.emplace_back(0, std::vector<uint32_t>(12));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(1u, result.results.size());
    TEST_ASSERT_EQUALS(
        std::string("Hello World"), std::string(reinterpret_cast<const char*>(result.results[0].second->data())));
}

void TestMemoryAccess::testVPMWrites()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/test_vpm_write.cl");

    EmulationData data;
    data.kernelName = "test_vpm_write";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // parameter 0 is input
    data.parameter.emplace_back(0, std::vector<uint32_t>(16));
    for(unsigned i = 0; i < data.parameter.back().second->size(); ++i)
        data.parameter.back().second->at(i) = i;
    // parameter 1 is integer output
    data.parameter.emplace_back(0, std::vector<uint32_t>(10 * 16));
    // parameter 2 is short output
    data.parameter.emplace_back(0, std::vector<uint32_t>(10 * 8));
    // parameter 3 is char output
    data.parameter.emplace_back(0, std::vector<uint32_t>(10 * 4));
    // parameter 4 is integer output with stride
    data.parameter.emplace_back(0, std::vector<uint32_t>(10 * 16 * 4));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(5u, result.results.size());

    auto& src = data.parameter[0].second.value();
    auto& res1 = result.results[1].second.value();
    auto& res2 = result.results[2].second.value();
    auto& res3 = result.results[3].second.value();
    auto& res4 = result.results[4].second.value();

    for(unsigned i = 0; i < 10 * 16; ++i)
    {
        TEST_ASSERT_EQUALS(src.at(i % 16), res1.at(i));
        TEST_ASSERT_EQUALS(static_cast<short>(src.at(i % 16)), reinterpret_cast<const short*>(res2.data())[i]);
        TEST_ASSERT_EQUALS(src.at(i % 16), static_cast<unsigned>(reinterpret_cast<const char*>(res3.data())[i]));
        TEST_ASSERT_EQUALS(src.at(i % 16), res4.at((i / 16) * 3 * 16 /* stride */ + (i % 16)));
    }
}

void TestMemoryAccess::testVPMReads()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/test_vpm_read.cl");

    EmulationData data;
    data.kernelName = "test_vpm_read";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // parameters 0 and 1 are both input/output
    data.parameter.emplace_back(0, std::vector<unsigned>(4 * 10));
    data.parameter.emplace_back(0, std::vector<unsigned>(4 * 10 * 3 /* stride */));

    for(unsigned i = 0; i < 4 * 10; ++i)
        data.parameter[0].second->at(i) = i;

    for(unsigned i = 0; i < 4 * 10 * 3; ++i)
        data.parameter[1].second->at(i) = i;

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful);
    TEST_ASSERT_EQUALS(2u, result.results.size());

    auto& v1 = result.results[0].second.value();
    auto& v2 = result.results[1].second.value();

    // we copy (with stride) from v2 to v1 and then from v1 back to v2
    //-> v1 and v2 have same values (for first 10 int4 vectors)

    for(unsigned i = 0; i < 10 * 4; ++i)
    {
        TEST_ASSERT_EQUALS(v1.at(i), v2.at(i));
        TEST_ASSERT_EQUALS((i / 4) * 12 /*stride * elements*/ + (i % 4), v2[i]);
    }
}

void TestMemoryAccess::testVectorLoadStoreCharPrivate()
{
    testPrivateLocalFunction<char>(config, "-DTYPE=char -DSTORAGE=__private",
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMemoryAccess::testVectorLoadStoreCharLocal()
{
    //FIXME manual test is correct!
    testPrivateLocalFunction<char>(config, "-DTYPE=char -DSTORAGE=__local",
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestMemoryAccess::testVectorLoadStoreCharGlobal()
{
    testGlobalFunction<char>(config, "-DTYPE=char",
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestMemoryAccess::testVectorLoadStoreShortPrivate()
{
    testPrivateLocalFunction<short>(config, "-DTYPE=short -DSTORAGE=__private",
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestMemoryAccess::testVectorLoadStoreShortLocal()
{
    //FIXME manual test is correct!
    testPrivateLocalFunction<short>(config, "-DTYPE=short -DSTORAGE=__local",
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestMemoryAccess::testVectorLoadStoreShortGlobal()
{
    testGlobalFunction<short>(config, "-DTYPE=short",
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestMemoryAccess::testVectorLoadStoreIntPrivate()
{
    testPrivateLocalFunction<int>(config, "-DTYPE=int -DSTORAGE=__private",
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestMemoryAccess::testVectorLoadStoreIntLocal()
{
    testPrivateLocalFunction<int>(config, "-DTYPE=int -DSTORAGE=__local",
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}
void TestMemoryAccess::testVectorLoadStoreIntGlobal()
{
    testGlobalFunction<int>(config, "-DTYPE=int",
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}