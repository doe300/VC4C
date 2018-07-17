/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestVectorFunctions.h"
#include "emulation_helper.h"

static const std::string VECTOR_LOAD_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test(__global CAT(TYPE,N)* out, const __global TYPE* in) {
  size_t gid = get_global_id(0);
  out[gid] = CAT(vload,N)(gid, in);
}
)";

static const std::string VECTOR_STORE_FUNCTION = R"(
#define CONCAT(a,b) a ## b
#define CAT(a,b) CONCAT(a,b)
__kernel void test(__global TYPE* out, const __global CAT(TYPE,N)* in) {
  size_t gid = get_global_id(0);
  CAT(vstore,N)(in[gid], gid, out);
}
)";

static const std::string VECTOR_SHUFFLE_FUNCTION = R"(
__kernel void test(__global TYPE* out, const __global TYPE* in, const __global TYPE* mask) {
  size_t gid = get_global_id(0);
  out[gid] = shuffle(in[gid], mask[gid]);
}
)";

static const std::string VECTOR_SHUFFLE2_FUNCTION = R"(
__kernel void test(__global TYPE* out, const __global TYPE* in0, const __global TYPE* in1, const __global TYPE* mask) {
  size_t gid = get_global_id(0);
  out[gid] = shuffle2(in0[gid], in1[gid], mask[gid]);
}
)";

TestVectorFunctions::TestVectorFunctions(const vc4c::Configuration& config) : config(config)
{
    TEST_ADD(TestVectorFunctions::testVectorLoad2Int);
    TEST_ADD(TestVectorFunctions::testVectorLoad2Short);
    TEST_ADD(TestVectorFunctions::testVectorLoad2Char);
    /*XXX comparison is not right for width of 3, because "normal" load/store uses width of 4 per standard
    TEST_ADD(TestVectorFunctions::testVectorLoad3Int);
    TEST_ADD(TestVectorFunctions::testVectorLoad3Short);
    TEST_ADD(TestVectorFunctions::testVectorLoad3Char);
    */
    TEST_ADD(TestVectorFunctions::testVectorLoad4Int);
    TEST_ADD(TestVectorFunctions::testVectorLoad4Short);
    TEST_ADD(TestVectorFunctions::testVectorLoad4Char);
    TEST_ADD(TestVectorFunctions::testVectorLoad8Int);
    TEST_ADD(TestVectorFunctions::testVectorLoad8Short);
    TEST_ADD(TestVectorFunctions::testVectorLoad8Char);
    TEST_ADD(TestVectorFunctions::testVectorLoad16Int);
    TEST_ADD(TestVectorFunctions::testVectorLoad16Short);
    TEST_ADD(TestVectorFunctions::testVectorLoad16Char);

    TEST_ADD(TestVectorFunctions::testVectorStore2Int);
    TEST_ADD(TestVectorFunctions::testVectorStore2Short);
    TEST_ADD(TestVectorFunctions::testVectorStore2Char);
    /*XXX comparison is not right for width of 3, because "normal" load/store uses width of 4 per standard
    TEST_ADD(TestVectorFunctions::testVectorStore3Int);
    TEST_ADD(TestVectorFunctions::testVectorStore3Short);
    TEST_ADD(TestVectorFunctions::testVectorStore3Char);
    */
    TEST_ADD(TestVectorFunctions::testVectorStore4Int);
    TEST_ADD(TestVectorFunctions::testVectorStore4Short);
    TEST_ADD(TestVectorFunctions::testVectorStore4Char);
    TEST_ADD(TestVectorFunctions::testVectorStore8Int);
    TEST_ADD(TestVectorFunctions::testVectorStore8Short);
    TEST_ADD(TestVectorFunctions::testVectorStore8Char);
    TEST_ADD(TestVectorFunctions::testVectorStore16Int);
    TEST_ADD(TestVectorFunctions::testVectorStore16Short);
    TEST_ADD(TestVectorFunctions::testVectorStore16Char);

    TEST_ADD(TestVectorFunctions::testShuffleVector2);
    TEST_ADD(TestVectorFunctions::testShuffleVector4);
    TEST_ADD(TestVectorFunctions::testShuffleVector8);
    TEST_ADD(TestVectorFunctions::testShuffleVector16);

    TEST_ADD(TestVectorFunctions::testShuffle2Vector2);
    TEST_ADD(TestVectorFunctions::testShuffle2Vector4);
    TEST_ADD(TestVectorFunctions::testShuffle2Vector8);
    TEST_ADD(TestVectorFunctions::testShuffle2Vector16);

    /*XXX
        TEST_ADD(TestVectorFunctions::testExtractElement);
        TEST_ADD(TestVectorFunctions::testInsertElement);
        */
}

void TestVectorFunctions::onMismatch(const std::string& expected, const std::string& result)
{
    TEST_ASSERT_EQUALS(expected, result);
}

template <typename T, std::size_t N>
static void testVectorLoadFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, VECTOR_LOAD_FUNCTION, options);

    auto in = generateInput<T, N * 12>(true);

    auto out = runEmulation<T, T, N, 12>(code, {in});
    checkUnaryResults<T, T>(in, out, [](T val) -> T { return val; }, std::string("vload") + std::to_string(N), onError);
}

template <typename T, std::size_t N>
static void testVectorStoreFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, VECTOR_STORE_FUNCTION, options);

    auto in = generateInput<T, N * 12>(true);

    auto out = runEmulation<T, T, N, 12>(code, {in});
    checkUnaryResults<T, T>(
        in, out, [](T val) -> T { return val; }, std::string("vstore") + std::to_string(N), onError);
}

template <typename T, std::size_t N>
std::array<T, N> checkShuffle(const std::array<T, N>& in, const std::array<T, N>& mask)
{
    throw std::runtime_error("TODO");
}

template <typename T, std::size_t N>
static void testVectorShuffleFunction(vc4c::Configuration& config, const std::string& options,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, VECTOR_SHUFFLE_FUNCTION, options);

    auto in = generateInput<T, N * 12>(true);
    auto mask = generateInput<T, N * 12, 0, N>(true);

    auto out = runEmulation<T, T, N, 12>(code, {in, mask});
    checkBinaryGroupedResults<T, T, N * 12, N>(in, mask, out, checkShuffle<T, N>, std::string("shuffle"), onError);
}

template <typename T, std::size_t N>
std::array<T, N> checkShuffle2(const std::array<T, N>& in0, const std::array<T, N>& in1, const std::array<T, N>& mask)
{
    throw std::runtime_error("TODO");
}

template <typename T, std::size_t N>
static void testVectorShuffle2Function(vc4c::Configuration& config, const std::string& options,
    const std::function<void(const std::string&, const std::string&)>& onError)
{
    std::stringstream code;
    compileBuffer(config, code, VECTOR_SHUFFLE_FUNCTION, options);

    auto in0 = generateInput<T, N * 12>(true);
    auto in1 = generateInput<T, N * 12>(true);
    auto mask = generateInput<T, N * 12, 0, N>(true);

    auto out = runEmulation<T, T, N, 12>(code, {in0, in1, mask});
    checkTrinaryGroupedResults<T, T, N * 12, N>(
        in0, in1, mask, out, checkShuffle2<T, N>, std::string("shuffle2"), onError);
}

void TestVectorFunctions::testVectorLoad2Int()
{
    testVectorLoadFunction<int, 2>(config, "-DTYPE=int -DN=2",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad2Short()
{
    testVectorLoadFunction<short, 2>(config, "-DTYPE=short -DN=2",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad2Char()
{
    testVectorLoadFunction<char, 2>(config, "-DTYPE=char -DN=2",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad3Int()
{
    testVectorLoadFunction<int, 3>(config, "-DTYPE=int -DN=3",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad3Short()
{
    testVectorLoadFunction<short, 3>(config, "-DTYPE=short -DN=3",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad3Char()
{
    testVectorLoadFunction<char, 3>(config, "-DTYPE=char -DN=3",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad4Int()
{
    testVectorLoadFunction<int, 4>(config, "-DTYPE=int -DN=4",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad4Short()
{
    testVectorLoadFunction<short, 4>(config, "-DTYPE=short -DN=4",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad4Char()
{
    testVectorLoadFunction<char, 4>(config, "-DTYPE=char -DN=4",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad8Int()
{
    testVectorLoadFunction<int, 8>(config, "-DTYPE=int -DN=8",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad8Short()
{
    testVectorLoadFunction<short, 8>(config, "-DTYPE=short -DN=8",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad8Char()
{
    testVectorLoadFunction<char, 8>(config, "-DTYPE=char -DN=8",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad16Int()
{
    testVectorLoadFunction<int, 16>(config, "-DTYPE=int -DN=16",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad16Short()
{
    testVectorLoadFunction<short, 16>(config, "-DTYPE=short -DN=16",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorLoad16Char()
{
    testVectorLoadFunction<char, 16>(config, "-DTYPE=char -DN=16",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore2Int()
{
    testVectorStoreFunction<int, 2>(config, "-DTYPE=int -DN=2",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore2Short()
{
    testVectorStoreFunction<short, 2>(config, "-DTYPE=short -DN=2",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore2Char()
{
    testVectorStoreFunction<char, 2>(config, "-DTYPE=char -DN=2",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore3Int()
{
    testVectorStoreFunction<int, 3>(config, "-DTYPE=int -DN=3",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore3Short()
{
    testVectorStoreFunction<short, 3>(config, "-DTYPE=short -DN=3",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore3Char()
{
    testVectorStoreFunction<char, 3>(config, "-DTYPE=char -DN=3",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore4Int()
{
    testVectorStoreFunction<int, 4>(config, "-DTYPE=int -DN=4",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore4Short()
{
    testVectorStoreFunction<short, 4>(config, "-DTYPE=short -DN=4",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore4Char()
{
    testVectorStoreFunction<char, 4>(config, "-DTYPE=char -DN=4",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore8Int()
{
    testVectorStoreFunction<int, 8>(config, "-DTYPE=int -DN=8",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore8Short()
{
    testVectorStoreFunction<short, 8>(config, "-DTYPE=short -DN=8",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore8Char()
{
    testVectorStoreFunction<char, 8>(config, "-DTYPE=char -DN=8",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore16Int()
{
    testVectorStoreFunction<int, 16>(config, "-DTYPE=int -DN=16",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore16Short()
{
    testVectorStoreFunction<short, 16>(config, "-DTYPE=short -DN=16",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testVectorStore16Char()
{
    testVectorStoreFunction<char, 16>(config, "-DTYPE=char -DN=16",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testShuffleVector2()
{
    testVectorShuffleFunction<unsigned, 2>(config, "-DTYPE=uint2",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testShuffleVector4()
{
    testVectorShuffleFunction<unsigned, 4>(config, "-DTYPE=uint4",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testShuffleVector8()
{
    testVectorShuffleFunction<unsigned, 8>(config, "-DTYPE=uint8",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testShuffleVector16()
{
    testVectorShuffleFunction<unsigned, 16>(config, "-DTYPE=uint16",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testShuffle2Vector2()
{
    testVectorShuffle2Function<unsigned, 2>(config, "-DTYPE=uint2",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testShuffle2Vector4()
{
    testVectorShuffle2Function<unsigned, 4>(config, "-DTYPE=uint4",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testShuffle2Vector8()
{
    testVectorShuffle2Function<unsigned, 8>(config, "-DTYPE=uint8",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}

void TestVectorFunctions::testShuffle2Vector16()
{
    testVectorShuffle2Function<unsigned, 16>(config, "-DTYPE=uint16",
        std::bind(&TestVectorFunctions::onMismatch, this, std::placeholders::_1, std::placeholders::_2));
}