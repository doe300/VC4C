/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestMemoryAccess.h"

#include "emulation_helper.h"
#include "test_cases.h"

#include <numeric>

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

static const std::string COPY64BIT = R"(
typedef struct {
  uint a;
  uint b;
} Type;

__kernel void test(__global Type* out, __global Type* in, __global const ulong* other) {
  size_t gid = get_global_id(0);
  Type val = in[gid];
  out[gid] = val;
})";

static const std::string LOADSTORE64BIT = R"(
typedef struct {
  uint a;
  uint b;
} Type;

__kernel void test(__global Type* out, __global Type* in, __global const ulong* other) {
  size_t gid = get_global_id(0);
  Type val = in[gid];
  // this prevents e.g. copy optimization
  in[gid] = (Type){.a = 0, .b = 0};
  out[gid] = val;
  in[gid] = val;
})";

static const std::string READ64BITLOWERWORD = R"(
typedef struct {
  uint a;
  uint b;
} Type;

__kernel void test(__global uint* out, __global Type* in, __global const ulong* other) {
  size_t gid = get_global_id(0);
  Type val = in[gid];
  ulong b = other[gid];
  uint2 c = (uint2)(b & 0xFFFFFFFF, b >> 32);
  out[gid] = val.a + c.x;
})";

static const std::string READ64BITUPPERWORD = R"(
typedef struct {
  uint a;
  uint b;
} Type;

__kernel void test(__global uint* out, __global Type* in, __global const ulong* other) {
  size_t gid = get_global_id(0);
  Type val = in[gid];
  ulong b = other[gid];
  uint2 c = (uint2)(b & 0xFFFFFFFF, b >> 32);
  out[gid] = val.b + c.y;
})";

static const std::string READWRITE64BITLOWERWORD = R"(
typedef struct {
  uint a;
  uint b;
} Type;

__kernel void test(__global Type* out, __global Type* in, __global const ulong* other) {
  size_t gid = get_global_id(0);
  // read the input, so we don't merge the below to a copy
  Type val = in[gid];
  ulong b = other[gid];
  uint2 c = (uint2)(b & 0xFFFFFFFF, b >> 32);
  in[gid].a += c.x;
  out[gid] = in[gid];
})";

static const std::string READWRITE64BITUPPERWORD = R"(
typedef struct {
  uint a;
  uint b;
} Type;

__kernel void test(__global Type* out, __global Type* in, __global const ulong* other) {
  size_t gid = get_global_id(0);
  // read the input, so we don't merge the below to a copy
  Type val = in[gid];
  ulong b = other[gid];
  uint2 c = (uint2)(b & 0xFFFFFFFF, b >> 32);
  in[gid].b += c.y;
  out[gid] = in[gid];
})";

static const std::string WRITE64BIT = R"(
typedef struct {
  uint a;
  uint b;
} Type;

__kernel void test(__global Type* out, __global Type* in, __global const ulong* other) {
  size_t gid = get_global_id(0);
  // read the input, so we don't merge the below to a copy
  Type val = in[gid];
  ulong tmp = other[gid];
  Type tmp2;
  tmp2.a = (uint)tmp;
  tmp2.b = (uint)(tmp >> 32);
  in[gid] = tmp2;
  out[gid] = in[gid];
})";

TestMemoryAccess::TestMemoryAccess(const Configuration& config) : TestCompilationHelper(config)
{
    TEST_ADD(TestMemoryAccess::testPrivateStorage);
    TEST_ADD(TestMemoryAccess::testLocalStorage);
    TEST_ADD(TestMemoryAccess::testVectorAssembly);
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

    TEST_ADD(TestMemoryAccess::testVectorLoadStorePrivateRegister);
    TEST_ADD(TestMemoryAccess::testVectorLoadStorePrivateVPMFull);
    TEST_ADD(TestMemoryAccess::testVectorLoadStorePrivateVPMPartial);
    TEST_ADD(TestMemoryAccess::testVectorLoadStoreLocalParameter);
    TEST_ADD(TestMemoryAccess::testVectorLoadStoreGlobalParameter);

    TEST_ADD(TestMemoryAccess::testCopy64BitRAM);
    TEST_ADD(TestMemoryAccess::testLoadStore64BitRAM);
    // covers 64-bit TMU read
    TEST_ADD(TestMemoryAccess::testRead64BitLowerWordFromRAM);
    // covers 64-bit TMU read as well as lshr %src.upper >> 32 into %dest.lower
    TEST_ADD(TestMemoryAccess::testRead64BitUpperWordFromRAM);
    // XXX the following tests does not increase coverage over the above, i.e. the same code is executed -> rewrite!
    TEST_ADD(TestMemoryAccess::testReadWrite64BitLowerWordFromRAM);
    TEST_ADD(TestMemoryAccess::testReadWrite64BitUpperWordFromRAM);
    TEST_ADD(TestMemoryAccess::testWrite64BitToRAM);

    TEST_ADD(TestMemoryAccess::testWriteSelectParameter);
    TEST_ADD(TestMemoryAccess::testReadSelectParameter);
    TEST_ADD(TestMemoryAccess::testReadWriteSelectParameter);
    TEST_ADD(TestMemoryAccess::testCopySelectParameter);
    TEST_ADD(TestMemoryAccess::testWritePhiParameter);
    TEST_ADD(TestMemoryAccess::testReadPhiParameter);
    TEST_ADD(TestMemoryAccess::testReadWritePhiParameter);
    TEST_ADD(TestMemoryAccess::testCopyPhiParameter);
    TEST_ADD(TestMemoryAccess::testReadSelectParameterOrLocal);
    TEST_ADD(TestMemoryAccess::testReadSelectRegister);
}

TestMemoryAccess::~TestMemoryAccess() = default;

void TestMemoryAccess::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result)
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
    checkUnaryResults<T, T>(
        in, out, [](T val) -> T { return val + (val + 3) + (val + 7) + (val + 11) + (val + 13) + (val + 17); },
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
    checkUnaryResults<T, T>(
        in, out, [](T val) -> T { return val + (val + 3) + (val + 7) + (val + 11) + (val + 13) + (val + 17); },
        options.substr(type, options.find(' ', type) - type) + "__global", onError);
}

template <typename In, typename Out, std::size_t N, typename Comparison = CompareEqual<Out>>
static void testBinaryFunction(std::stringstream& code, const std::string& options,
    const std::function<Out(In, In)>& op, const std::function<void(const std::string&, const std::string&)>& onError)
{
    auto in0 = generateInput<In, N * 12>(true);
    auto in1 = generateInput<In, N * 12>(true);

    auto out = runEmulation<In, Out, N, 12>(code, {in0, in1});
    auto pos = options.find("-DFUNC=") + std::string("-DFUNC=").size();
    checkBinaryResults<Out, In, N * 12, Comparison>(
        in0, in1, out, op, options.substr(pos, options.find(' ', pos) - pos), onError);
}

void TestMemoryAccess::testPrivateStorage()
{
    const std::vector<uint32_t> expected{
        14, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 14, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28};
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl", "", true);

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
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(2u, result.results.size())

    if(expected != result.results.at(1).second.value())
    {
        auto expectedIt = expected.begin();
        auto resultIt = result.results.at(1).second->begin();
        while(expectedIt != expected.end())
        {
            TEST_ASSERT_EQUALS(*expectedIt, *resultIt)
            ++resultIt;
            ++expectedIt;
        }
    }
}

void TestMemoryAccess::testLocalStorage()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl", "", true);

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
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(2u, result.results.size())

    // actual results are indeterministic, since the order of the loads/stores across work-items is not guaranteed
    for(auto res : result.results.at(1).second.value())
    {
        if(res % 7u != 0)
        {
            TEST_ASSERT_EQUALS(7u, res)
        }
    }
}

void TestMemoryAccess::testVectorAssembly()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl", "", true);

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
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(1u, result.results.size())
    TEST_ASSERT_EQUALS(
        std::string("Hello World"), std::string(reinterpret_cast<const char*>(result.results[0].second->data())))
}

void TestMemoryAccess::testConstantStorage()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl", "", true);

    EmulationData data;
    data.kernelName = "test_constant_storage2";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);
    data.workGroup.dimensions = 3;
    data.workGroup.globalOffsets = {0, 0, 0};
    data.workGroup.localSizes = {12, 1, 1};
    data.workGroup.numGroups = {1, 1, 1};

    // parameter 0 is the output
    data.parameter.emplace_back(0, std::vector<uint32_t>(12));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(1u, result.results.size())
    TEST_ASSERT_EQUALS(
        std::string("Hello World!"), std::string(reinterpret_cast<const char*>(result.results[0].second->data())))
}

void TestMemoryAccess::testRegisterStorage()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/local_private_storage.cl", "", true);

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
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(1u, result.results.size())
    TEST_ASSERT_EQUALS(
        std::string("Hello World"), std::string(reinterpret_cast<const char*>(result.results[0].second->data())))
}

void TestMemoryAccess::testVPMWrites()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/test_vpm_write.cl", "", false);

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
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(5u, result.results.size())

    auto& src = data.parameter[0].second.value();
    auto& res1 = result.results[1].second.value();
    auto& res2 = result.results[2].second.value();
    auto& res3 = result.results[3].second.value();
    auto& res4 = result.results[4].second.value();

    for(unsigned i = 0; i < 10 * 16; ++i)
    {
        TEST_ASSERT_EQUALS(src.at(i % 16), res1.at(i))
        TEST_ASSERT_EQUALS(static_cast<short>(src.at(i % 16)), reinterpret_cast<const short*>(res2.data())[i])
        TEST_ASSERT_EQUALS(src.at(i % 16), static_cast<unsigned>(reinterpret_cast<const char*>(res3.data())[i]))
        TEST_ASSERT_EQUALS(src.at(i % 16), res4.at((i / 16) * 3 * 16 /* stride */ + (i % 16)))
    }
}

void TestMemoryAccess::testVPMReads()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/test_vpm_read.cl", "", false);

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
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(2u, result.results.size())

    auto& v1 = result.results[0].second.value();
    auto& v2 = result.results[1].second.value();

    // we copy (with stride) from v2 to v1 and then from v1 back to v2
    //-> v1 and v2 have same values (for first 10 int4 vectors)

    for(unsigned i = 0; i < 10 * 4; ++i)
    {
        TEST_ASSERT_EQUALS(v1.at(i), v2.at(i))
        TEST_ASSERT_EQUALS((i / 4) * 12 /*stride * elements*/ + (i % 4), v2[i])
    }
}

void TestMemoryAccess::testVectorLoadStoreCharPrivate()
{
    testPrivateLocalFunction<char>(config, "-DTYPE=char -DSTORAGE=__private",
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMemoryAccess::testVectorLoadStoreCharLocal()
{
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

void TestMemoryAccess::testVectorLoadStorePrivateRegister()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/unaligned_memory_access.cl", "", true);

    EmulationData data;
    data.kernelName = "test_private_register";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // output
    data.parameter.emplace_back(0, std::vector<unsigned>(5 * 8));
    // input
    data.parameter.emplace_back(
        0, std::vector<unsigned>{0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf});
    // offsets
    data.parameter.emplace_back(0, std::vector<unsigned>{0, 1, 2, 3});

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(3u, result.results.size())

    auto& out = result.results[0].second.value();

    const std::vector<uint32_t> golden = {0x0, 0x1, 0xc, 0xd, 0xe, 0xf, 0x6, 0x7, 0x8, 0x9, 0xa, 0xc, 0xd, 0xe, 0xf,
        0x0, 0x8, 0x9, 0xa, 0xc, 0xd, 0xe, 0xf, 0x0, 0xf, 0x6, 0x7, 0x8, 0x9, 0xa, 0xc, 0xd, 0xd, 0xe, 0xf, 0x6, 0x7,
        0x8, 0x9, 0xa};

    TEST_ASSERT_EQUALS(out.size(), golden.size())

    for(unsigned i = 0; i < out.size(); ++i)
    {
        if(out[i] != golden[i])
        {
            TEST_ASSERT_EQUALS(golden[i], out[i])
            TEST_ASSERT_EQUALS("", "element " + std::to_string(i))
        }
    }
}

void TestMemoryAccess::testVectorLoadStorePrivateVPMFull()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/unaligned_memory_access.cl", "", true);

    EmulationData data;
    data.kernelName = "test_private_vpm_full_row";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // output
    data.parameter.emplace_back(0, std::vector<unsigned>(5 * 8));
    // input
    data.parameter.emplace_back(0,
        std::vector<unsigned>{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
            0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f});
    // offsets
    data.parameter.emplace_back(0, std::vector<unsigned>{0, 1, 2, 3, 11});

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(3u, result.results.size())

    auto& out = result.results[0].second.value();

    const std::vector<uint32_t> golden = {0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19,
        0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
        0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f};

    TEST_ASSERT_EQUALS(out.size(), golden.size())

    for(unsigned i = 0; i < out.size(); ++i)
    {
        if(out[i] != golden[i])
        {
            TEST_ASSERT_EQUALS(golden[i], out[i])
            TEST_ASSERT_EQUALS("", "element " + std::to_string(i))
        }
    }
}

void TestMemoryAccess::testVectorLoadStorePrivateVPMPartial()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/unaligned_memory_access.cl", "", true);

    EmulationData data;
    data.kernelName = "test_private_vpm_partial_row";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // output
    data.parameter.emplace_back(0, std::vector<unsigned>(5 * 8));
    // input
    data.parameter.emplace_back(0,
        std::vector<unsigned>{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
            0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f});
    // offsets
    data.parameter.emplace_back(0, std::vector<unsigned>{0, 1, 2, 3, 11});

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(3u, result.results.size())

    auto& out = result.results[0].second.value();

    const std::vector<uint32_t> golden = {0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19,
        0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
        0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f};

    TEST_ASSERT_EQUALS(out.size(), golden.size())

    for(unsigned i = 0; i < out.size(); ++i)
    {
        if(out[i] != golden[i])
        {
            TEST_ASSERT_EQUALS(golden[i], out[i])
            TEST_ASSERT_EQUALS("", "element " + std::to_string(i))
        }
    }
}

void TestMemoryAccess::testVectorLoadStoreLocalParameter()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/unaligned_memory_access.cl", "", true);

    EmulationData data;
    data.kernelName = "test_local_parameter";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // output
    data.parameter.emplace_back(0, std::vector<unsigned>(5 * 8));
    // input
    data.parameter.emplace_back(0,
        std::vector<unsigned>{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
            0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f});
    // offsets
    data.parameter.emplace_back(0, std::vector<unsigned>{0, 1, 2, 3, 11});
    // local buffer
    data.parameter.emplace_back(0, std::vector<unsigned>(2 * 16));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(4u, result.results.size())

    auto& out = result.results[0].second.value();

    const std::vector<uint32_t> golden = {0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19,
        0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
        0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f};

    TEST_ASSERT_EQUALS(out.size(), golden.size())

    for(unsigned i = 0; i < out.size(); ++i)
    {
        if(out[i] != golden[i])
        {
            TEST_ASSERT_EQUALS(golden[i], out[i])
            TEST_ASSERT_EQUALS("", "element " + std::to_string(i))
        }
    }
}

void TestMemoryAccess::testVectorLoadStoreGlobalParameter()
{
    std::stringstream buffer;
    compileFile(buffer, "./testing/unaligned_memory_access.cl", "", true);

    EmulationData data;
    data.kernelName = "test_global";
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &buffer);

    // output
    data.parameter.emplace_back(0, std::vector<unsigned>(5 * 8));
    // input
    data.parameter.emplace_back(0,
        std::vector<unsigned>{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
            0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f});
    // offsets
    data.parameter.emplace_back(0, std::vector<unsigned>{0, 1, 2, 3, 11});
    // global buffer
    data.parameter.emplace_back(0, std::vector<unsigned>(2 * 16));

    const auto result = emulate(data);
    TEST_ASSERT(result.executionSuccessful)
    TEST_ASSERT_EQUALS(4u, result.results.size())

    auto& out = result.results[0].second.value();

    const std::vector<uint32_t> golden = {0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x18, 0x19,
        0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x13, 0x18, 0x19, 0x1a, 0x1b, 0x0a, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
        0x1e, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f};

    TEST_ASSERT_EQUALS(out.size(), golden.size())

    for(unsigned i = 0; i < out.size(); ++i)
    {
        if(out[i] != golden[i])
        {
            TEST_ASSERT_EQUALS(golden[i], out[i])
            TEST_ASSERT_EQUALS("", "element " + std::to_string(i))
        }
    }
}

void TestMemoryAccess::testCopy64BitRAM()
{
    auto func = [](uint64_t i, uint64_t other) -> uint64_t { return i; };

    std::string options = "-DFUNC=memcpy";
    std::stringstream code;
    compileBuffer(config, code, COPY64BIT, options);
    testBinaryFunction<uint64_t, uint64_t, 1>(code, options, func,
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMemoryAccess::testLoadStore64BitRAM()
{
    auto func = [](uint64_t i, uint64_t other) -> uint64_t { return i; };

    std::string options = "-DFUNC=load/store";
    std::stringstream code;
    compileBuffer(config, code, LOADSTORE64BIT, options);
    testBinaryFunction<uint64_t, uint64_t, 1>(code, options, func,
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMemoryAccess::testRead64BitLowerWordFromRAM()
{
    auto func = [](uint64_t i, uint64_t other) -> uint32_t { return static_cast<uint32_t>((i & 0xFFFFFFFFu) + other); };

    std::string options = "-DFUNC=memcpy";
    std::stringstream code;
    compileBuffer(config, code, READ64BITLOWERWORD, options);
    testBinaryFunction<uint64_t, uint32_t, 1>(code, options, func,
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMemoryAccess::testRead64BitUpperWordFromRAM()
{
    auto func = [](uint64_t i, uint64_t other) -> uint32_t { return static_cast<uint32_t>((i >> 32) + (other >> 32)); };

    std::string options = "-DFUNC=memcpy";
    std::stringstream code;
    compileBuffer(config, code, READ64BITUPPERWORD, options);
    testBinaryFunction<uint64_t, uint32_t, 1>(code, options, func,
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMemoryAccess::testReadWrite64BitLowerWordFromRAM()
{
    auto func = [](uint64_t i, uint64_t other) -> uint64_t {
        return (i & uint64_t{0xFFFFFFFF00000000u}) | ((i + other) & 0xFFFFFFFFu);
    };

    std::string options = "-DFUNC=memcpy";
    std::stringstream code;
    compileBuffer(config, code, READWRITE64BITLOWERWORD, options);
    testBinaryFunction<uint64_t, uint64_t, 1>(code, options, func,
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMemoryAccess::testReadWrite64BitUpperWordFromRAM()
{
    auto func = [](uint64_t i, uint64_t other) -> uint64_t {
        auto lower = static_cast<uint32_t>(i & uint64_t{0xFFFFFFFFu});
        auto upper = static_cast<uint32_t>((i >> 32u) + (other >> 32u));
        return lower | (static_cast<uint64_t>(upper) << 32u);
    };

    std::string options = "-DFUNC=memcpy";
    std::stringstream code;
    compileBuffer(config, code, READWRITE64BITUPPERWORD, options);
    testBinaryFunction<uint64_t, uint64_t, 1>(code, options, func,
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMemoryAccess::testWrite64BitToRAM()
{
    auto func = [](uint64_t i, uint64_t other) -> uint64_t { return other; };

    std::string options = "-DFUNC=memcpy";
    std::stringstream code;
    compileBuffer(config, code, WRITE64BIT, options);
    testBinaryFunction<uint64_t, uint64_t, 1>(code, options, func,
        std::bind(&TestMemoryAccess::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestMemoryAccess::testWriteSelectParameter()
{
    std::stringstream code;
    compileFile(code, "testing/test_conditional_address.cl", "", true);

    constexpr unsigned NUM_ITEMS = 30;

    auto tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> in{tmp.begin(), tmp.end()};
    tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> out0(tmp.begin(), tmp.end());
    tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> out1(tmp.begin(), tmp.end());

    std::unique_ptr<vc4c::tools::EmulationResult> result;
    emulateKernel(code, "test_select_write_address_simple", NUM_ITEMS, result, {in, out0, out1});

    auto& resultOut0 = result->results[1].second.value();
    auto& resultOut1 = result->results[2].second.value();

    for(unsigned i = 0; i < NUM_ITEMS; ++i)
    {
        if(i & 1)
        {
            if((in[i] + 17u) != resultOut1[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(in[i]) + " + 17 = " + std::to_string(in[i] + 17) +
                        " for modified element " + std::to_string(i),
                    std::to_string(resultOut1[i]) + " (before " + std::to_string(out1[i]) + ")");
            }
            if(out0[i] != resultOut0[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(out0[i]) + " for unmodified element " + std::to_string(i),
                    std::to_string(resultOut0[i]));
            }
        }
        else
        {
            if((in[i] + 17u) != resultOut0[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(in[i]) + " + 17 = " + std::to_string(in[i] + 17) +
                        " for modified element " + std::to_string(i),
                    std::to_string(resultOut0[i]) + " (before " + std::to_string(out0[i]) + ")");
            }
            if(out1[i] != resultOut1[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(out1[i]) + " for unmodified element " + std::to_string(i),
                    std::to_string(resultOut1[i]));
            }
        }
    }
}

void TestMemoryAccess::testReadSelectParameter()
{
    std::stringstream code;
    compileFile(code, "testing/test_conditional_address.cl", "", true);

    constexpr unsigned NUM_ITEMS = 30;

    auto tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> in0{tmp.begin(), tmp.end()};
    tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> in1(tmp.begin(), tmp.end());
    tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> out(tmp.begin(), tmp.end());

    std::unique_ptr<vc4c::tools::EmulationResult> result;
    emulateKernel(code, "test_select_read_address_simple", NUM_ITEMS, result, {in0, in1, out});

    auto& resultOut = result->results[2].second.value();

    for(unsigned i = 0; i < NUM_ITEMS; ++i)
    {
        if(i & 1)
        {
            if((in1[i] + 17u) != resultOut[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(in1[i]) + " + 17 = " + std::to_string(in1[i] + 17) +
                        " for modified element " + std::to_string(i),
                    std::to_string(resultOut[i]) + " (before " + std::to_string(out[i]) + ")");
            }
        }
        else
        {
            if((in0[i] + 17u) != resultOut[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(in0[i]) + " + 17 = " + std::to_string(in0[i] + 17) +
                        " for modified element " + std::to_string(i),
                    std::to_string(resultOut[i]) + " (before " + std::to_string(out[i]) + ")");
            }
        }
    }
}

void TestMemoryAccess::testReadWriteSelectParameter()
{
    std::stringstream code;
    compileFile(code, "testing/test_conditional_address.cl", "", true);

    constexpr unsigned NUM_ITEMS = 30;

    auto tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> mem0{tmp.begin(), tmp.end()};
    tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> mem1(tmp.begin(), tmp.end());

    std::unique_ptr<vc4c::tools::EmulationResult> result;
    emulateKernel(code, "test_select_read_write_address_simple", NUM_ITEMS, result, {mem0, mem1});

    auto& result0 = result->results[0].second.value();
    auto& result1 = result->results[1].second.value();

    for(unsigned i = 0; i < NUM_ITEMS; ++i)
    {
        if(i & 1)
        {
            if((mem0[i] + 17u) != result1[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(mem0[i]) + " + 17 = " + std::to_string(mem0[i] + 17) +
                        " for modified element " + std::to_string(i),
                    std::to_string(result1[i]) + " (before " + std::to_string(mem1[i]) + ")");
            }
            if(mem0[i] != result0[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(mem0[i]) + " for unmodified element " + std::to_string(i),
                    std::to_string(result0[i]));
            }
        }
        else
        {
            if((mem1[i] + 17u) != result0[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(mem1[i]) + " + 17 = " + std::to_string(mem1[i] + 17) +
                        " for modified element " + std::to_string(i),
                    std::to_string(result0[i]) + " (before " + std::to_string(mem0[i]) + ")");
            }
            if(mem1[i] != result1[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(mem1[i]) + " for unmodified element " + std::to_string(i),
                    std::to_string(result1[i]));
            }
        }
    }
}

void TestMemoryAccess::testCopySelectParameter()
{
    std::stringstream code;
    compileFile(code, "testing/test_conditional_address.cl", "", true);

    constexpr unsigned NUM_ITEMS = 30;

    auto tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> mem0{tmp.begin(), tmp.end()};
    tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> mem1(tmp.begin(), tmp.end());

    std::unique_ptr<vc4c::tools::EmulationResult> result;
    emulateKernel(code, "test_select_copy_address_simple", NUM_ITEMS, result, {mem0, mem1});

    auto& result0 = result->results[0].second.value();
    auto& result1 = result->results[1].second.value();

    for(unsigned i = 0; i < NUM_ITEMS; ++i)
    {
        if(i & 1)
        {
            if((mem0[i]) != result1[i])
            {
                TEST_ASSERT_EQUALS(
                    std::to_string(mem0[i]) + " for copied element " + std::to_string(i), std::to_string(result1[i]));
            }
            if(mem0[i] != result0[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(mem0[i]) + " for unmodified element " + std::to_string(i),
                    std::to_string(result0[i]));
            }
        }
        else
        {
            if((mem1[i]) != result0[i])
            {
                TEST_ASSERT_EQUALS(
                    std::to_string(mem1[i]) + " for copied element " + std::to_string(i), std::to_string(result0[i]));
            }
            if(mem1[i] != result1[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(mem1[i]) + " for unmodified element " + std::to_string(i),
                    std::to_string(result1[i]));
            }
        }
    }
}

void TestMemoryAccess::testWritePhiParameter()
{
    std::stringstream code;
    compileFile(code, "testing/test_conditional_address.cl", "", true);

    constexpr unsigned NUM_ITEMS = 16;
    constexpr unsigned ITERATION_COUNT = 3;

    auto tmp = generateInput<unsigned, 1 * NUM_ITEMS * ITERATION_COUNT>(true);
    std::vector<unsigned> in{tmp.begin(), tmp.end()};
    tmp = generateInput<unsigned, 1 * NUM_ITEMS * ITERATION_COUNT>(true);
    std::vector<unsigned> out0(tmp.begin(), tmp.end());
    tmp = generateInput<unsigned, 1 * NUM_ITEMS * ITERATION_COUNT>(true);
    std::vector<unsigned> out1(tmp.begin(), tmp.end());

    std::unique_ptr<vc4c::tools::EmulationResult> result;
    emulateKernel(code, "test_phi_write_address_simple", NUM_ITEMS, result, {in, out0, out1, {ITERATION_COUNT}});

    auto& resultOut0 = result->results[1].second.value();
    auto& resultOut1 = result->results[2].second.value();

    for(unsigned i = 0; i < NUM_ITEMS; ++i)
    {
        if(i & 1)
        {
            for(unsigned k = 0; k < ITERATION_COUNT; ++k)
            {
                auto n = i + NUM_ITEMS * k;
                if((in[i] + 17u) != resultOut1[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(in[i]) + " + 17 = " + std::to_string(in[i] + 17) +
                            " for modified element " + std::to_string(n),
                        std::to_string(resultOut1[n]) + " (before " + std::to_string(out1[n]) + ")");
                }
                if(out0[n] != resultOut0[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(out0[n]) + " for unmodified element " + std::to_string(n),
                        std::to_string(resultOut0[n]));
                }
            }
        }
        else
        {
            for(unsigned k = 0; k < ITERATION_COUNT; ++k)
            {
                auto n = i + NUM_ITEMS * k;
                if((in[i] + 17u) != resultOut0[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(in[i]) + " + 17 = " + std::to_string(in[i] + 17) +
                            " for modified element " + std::to_string(n),
                        std::to_string(resultOut0[n]) + " (before " + std::to_string(out0[n]) + ")");
                }
                if(out1[n] != resultOut1[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(out1[n]) + " for unmodified element " + std::to_string(n),
                        std::to_string(resultOut1[n]));
                }
            }
        }
    }
}

void TestMemoryAccess::testReadPhiParameter()
{
    std::stringstream code;
    compileFile(code, "testing/test_conditional_address.cl", "", true);

    constexpr unsigned NUM_ITEMS = 16;
    constexpr unsigned ITERATION_COUNT = 3;

    auto tmp = generateInput<unsigned, 1 * NUM_ITEMS * ITERATION_COUNT>(true);
    std::vector<unsigned> in0{tmp.begin(), tmp.end()};
    tmp = generateInput<unsigned, 1 * NUM_ITEMS * ITERATION_COUNT>(true);
    std::vector<unsigned> in1(tmp.begin(), tmp.end());
    tmp = generateInput<unsigned, 1 * NUM_ITEMS * ITERATION_COUNT>(true);
    std::vector<unsigned> out(tmp.begin(), tmp.end());

    std::unique_ptr<vc4c::tools::EmulationResult> result;
    emulateKernel(code, "test_phi_read_address_simple", NUM_ITEMS, result, {in0, in1, out, {ITERATION_COUNT}});

    auto& resultOut = result->results[2].second.value();

    for(unsigned i = 0; i < NUM_ITEMS; ++i)
    {
        if(i & 1)
        {
            for(unsigned k = 0; k < ITERATION_COUNT; ++k)
            {
                auto n = i + NUM_ITEMS * k;
                if((in1[n] + 17u) != resultOut[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(in1[n]) + " + 17 = " + std::to_string(in1[n] + 17) +
                            " for modified element " + std::to_string(n),
                        std::to_string(resultOut[n]) + " (before " + std::to_string(out[n]) + ")");
                }
            }
        }
        else
        {
            for(unsigned k = 0; k < ITERATION_COUNT; ++k)
            {
                auto n = i + NUM_ITEMS * k;
                if((in0[n] + 17u) != resultOut[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(in0[n]) + " + 17 = " + std::to_string(in0[n] + 17) +
                            " for modified element " + std::to_string(n),
                        std::to_string(resultOut[n]) + " (before " + std::to_string(out[n]) + ")");
                }
            }
        }
    }
}

void TestMemoryAccess::testReadWritePhiParameter()
{
    std::stringstream code;
    compileFile(code, "testing/test_conditional_address.cl", "", true);

    constexpr unsigned NUM_ITEMS = 16;
    constexpr unsigned ITERATION_COUNT = 3;

    auto tmp = generateInput<unsigned, 1 * NUM_ITEMS * ITERATION_COUNT>(true);
    std::vector<unsigned> mem0{tmp.begin(), tmp.end()};
    tmp = generateInput<unsigned, 1 * NUM_ITEMS * ITERATION_COUNT>(true);
    std::vector<unsigned> mem1(tmp.begin(), tmp.end());

    std::unique_ptr<vc4c::tools::EmulationResult> result;
    emulateKernel(code, "test_phi_read_write_address_simple", NUM_ITEMS, result, {mem0, mem1, {ITERATION_COUNT}});

    auto& result0 = result->results[0].second.value();
    auto& result1 = result->results[1].second.value();

    for(unsigned i = 0; i < NUM_ITEMS; ++i)
    {
        if(i & 1)
        {
            for(unsigned k = 0; k < ITERATION_COUNT; ++k)
            {
                auto n = i + NUM_ITEMS * k;
                if((mem0[n] + 17u) != result1[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(mem0[n]) + " + 17 = " + std::to_string(mem0[n] + 17) +
                            " for modified element " + std::to_string(n),
                        std::to_string(result1[n]) + " (before " + std::to_string(mem1[n]) + ")");
                }
                if(mem0[n] != result0[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(mem0[n]) + " for unmodified element " + std::to_string(n),
                        std::to_string(result0[n]));
                }
            }
        }
        else
        {
            for(unsigned k = 0; k < ITERATION_COUNT; ++k)
            {
                auto n = i + NUM_ITEMS * k;
                if((mem1[n] + 17u) != result0[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(mem1[n]) + " + 17 = " + std::to_string(mem1[n] + 17) +
                            " for modified element " + std::to_string(n),
                        std::to_string(result0[n]) + " (before " + std::to_string(mem0[n]) + ")");
                }
                if(mem1[n] != result1[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(mem1[n]) + " for unmodified element " + std::to_string(n),
                        std::to_string(result1[n]));
                }
            }
        }
    }
}

void TestMemoryAccess::testCopyPhiParameter()
{
    std::stringstream code;
    compileFile(code, "testing/test_conditional_address.cl", "", true);

    constexpr unsigned NUM_ITEMS = 16;
    constexpr unsigned ITERATION_COUNT = 3;

    auto tmp = generateInput<unsigned, 1 * NUM_ITEMS * ITERATION_COUNT>(true);
    std::vector<unsigned> mem0{tmp.begin(), tmp.end()};
    tmp = generateInput<unsigned, 1 * NUM_ITEMS * ITERATION_COUNT>(true);
    std::vector<unsigned> mem1(tmp.begin(), tmp.end());

    std::unique_ptr<vc4c::tools::EmulationResult> result;
    emulateKernel(code, "test_phi_copy_address_simple", NUM_ITEMS, result, {mem0, mem1, {ITERATION_COUNT}});

    auto& result0 = result->results[0].second.value();
    auto& result1 = result->results[1].second.value();

    for(unsigned i = 0; i < NUM_ITEMS; ++i)
    {
        if(i & 1)
        {
            for(unsigned k = 0; k < ITERATION_COUNT; ++k)
            {
                auto n = i + NUM_ITEMS * k;
                if((mem0[n]) != result1[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(mem0[i]) + " for copied element " + std::to_string(i),
                        std::to_string(result1[i]));
                }
                if(mem0[n] != result0[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(mem0[n]) + " for unmodified element " + std::to_string(n),
                        std::to_string(result0[n]));
                }
            }
        }
        else
        {
            for(unsigned k = 0; k < ITERATION_COUNT; ++k)
            {
                auto n = i + NUM_ITEMS * k;
                if((mem1[n]) != result0[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(mem1[i]) + " for copied element " + std::to_string(i),
                        std::to_string(result0[i]));
                }
                if(mem1[n] != result1[n])
                {
                    TEST_ASSERT_EQUALS(std::to_string(mem1[n]) + " for unmodified element " + std::to_string(n),
                        std::to_string(result1[n]));
                }
            }
        }
    }
}

void TestMemoryAccess::testReadSelectParameterOrLocal()
{
    std::stringstream code;
    compileFile(code, "testing/test_conditional_address.cl", "", true);

    // Needs to be at most the size of the __local buffer in the kernel
    constexpr unsigned NUM_ITEMS = 14;

    auto tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> in0{tmp.begin(), tmp.end()};
    // The local buffer "in1" is set to get_global_id(0)
    tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> out(tmp.begin(), tmp.end());

    std::unique_ptr<vc4c::tools::EmulationResult> result;
    emulateKernel(code, "test_select_read_address_local", NUM_ITEMS, result, {in0, out});

    auto& resultOut = result->results[1].second.value();

    for(unsigned i = 0; i < NUM_ITEMS; ++i)
    {
        if(i & 1)
        {
            if((i + 17u) != resultOut[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(i) + " + 17 = " + std::to_string(i + 17) + " for modified element " +
                        std::to_string(i),
                    std::to_string(resultOut[i]) + " (before " + std::to_string(out[i]) + ")");
            }
        }
        else
        {
            if((in0[i] + 17u) != resultOut[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(in0[i]) + " + 17 = " + std::to_string(in0[i] + 17) +
                        " for modified element " + std::to_string(i),
                    std::to_string(resultOut[i]) + " (before " + std::to_string(out[i]) + ")");
            }
        }
    }
}

void TestMemoryAccess::testReadSelectRegister()
{
    std::stringstream code;
    compileFile(code, "testing/test_conditional_address.cl", "", true);

    // Needs to be at most the size of the __private buffer in the kernel
    constexpr unsigned NUM_ITEMS = 16;

    auto tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> in{tmp.begin(), tmp.end()};
    tmp = generateInput<unsigned, 1 * NUM_ITEMS>(true);
    std::vector<unsigned> out(tmp.begin(), tmp.end());

    std::unique_ptr<vc4c::tools::EmulationResult> result;
    emulateKernel(code, "test_select_read_address_private", NUM_ITEMS, result, {in, out});

    auto& resultOut = result->results[1].second.value();

    for(unsigned i = 0; i < NUM_ITEMS; ++i)
    {
        // both private buffers are set to the input at position local_id
        // either private buffer is read at position global_id
        if(i < 8)
        {
            // for the first work-group, local_id is equal to global_id
            if((in[i] + 17u) != resultOut[i])
            {
                TEST_ASSERT_EQUALS(std::to_string(in[i]) + " + 17 = " + std::to_string(in[i] + 17) +
                        " for modified element " + std::to_string(i),
                    std::to_string(resultOut[i]) + " (before " + std::to_string(out[i]) + ")");
            }
        }
        else
        {
            // for the second work-group, they read either the original 17 or 42
            if(i & 1)
            {
                if((17u + 17u) != resultOut[i])
                {
                    TEST_ASSERT_EQUALS("17 + 17 = 34 for unmodified element " + std::to_string(i),
                        std::to_string(resultOut[i]) + " (before " + std::to_string(out[i]) + ")");
                }
            }
            else
            {
                if((42u + 17u) != resultOut[i])
                {
                    TEST_ASSERT_EQUALS("42 + 17 = 59 for unmodified element " + std::to_string(i),
                        std::to_string(resultOut[i]) + " (before " + std::to_string(out[i]) + ")");
                }
            }
        }
    }
}

void TestMemoryAccess::emulateKernel(std::istream& code, const std::string& kernelName, unsigned numItems,
    std::unique_ptr<vc4c::tools::EmulationResult>& result, const std::vector<std::vector<unsigned>>& args)
{
    EmulationData data;
    data.kernelName = kernelName;
    data.maxEmulationCycles = vc4c::test::maxExecutionCycles;
    data.module = std::make_pair("", &code);

    auto sizeFactor = std::ceil(numItems / static_cast<float>(NUM_QPUS));
    auto localSize = numItems / static_cast<unsigned>(sizeFactor);
    auto numGroups = static_cast<unsigned>(sizeFactor);
    TEST_ASSERT_EQUALS(numItems, localSize * numGroups);
    data.workGroup.localSizes = {localSize, 1, 1};
    data.workGroup.numGroups = {numGroups, 1, 1};

    for(const auto& arg : args)
        data.parameter.emplace_back(0u, arg);

    result.reset(new EmulationResult(emulate(data)));
    TEST_ASSERT(result->executionSuccessful)
    TEST_ASSERT_EQUALS(args.size(), result->results.size())
}
