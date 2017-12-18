/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestOperators.h"

#include "Values.h"
#include "intrinsics/Operators.h"

using namespace vc4c;
using namespace vc4c::intermediate;

TestOperators::TestOperators()
{
	TEST_ADD(TestOperators::testASR);
	TEST_ADD(TestOperators::testCLZ);
	TEST_ADD(TestOperators::testSMOD);
	TEST_ADD(TestOperators::testSREM);
	TEST_ADD(TestOperators::testFMOD);
	TEST_ADD(TestOperators::testFREM);
}

TestOperators::~TestOperators()
{

}

void TestOperators::testASR()
{
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0), asr(TYPE_INT32, INT_ONE.literal, INT_ONE.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0), asr(TYPE_INT16, INT_ONE.literal, INT_ONE.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0), asr(TYPE_INT8, INT_ONE.literal, INT_ONE.literal).integer);

	TEST_ASSERT_EQUALS(static_cast<int64_t>(1), asr(TYPE_INT32, INT_ONE.literal, INT_ZERO.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(1), asr(TYPE_INT16, INT_ONE.literal, INT_ZERO.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(1), asr(TYPE_INT8, INT_ONE.literal, INT_ZERO.literal).integer);

	TEST_ASSERT_EQUALS(static_cast<int64_t>(127), asr(TYPE_INT32, Literal(static_cast<int64_t>(255)), INT_ONE.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(127), asr(TYPE_INT16, Literal(static_cast<int64_t>(255)), INT_ONE.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(255), asr(TYPE_INT8, Literal(static_cast<int64_t>(255)), INT_ONE.literal).integer);

	TEST_ASSERT_EQUALS(static_cast<int64_t>(0xFFFFFFFF), asr(TYPE_INT32, Literal(TYPE_INT32.getScalarWidthMask()), INT_ONE.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0xFFFF), asr(TYPE_INT16, Literal(TYPE_INT16.getScalarWidthMask()), INT_ONE.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0xFF), asr(TYPE_INT8, Literal(TYPE_INT8.getScalarWidthMask()), INT_ONE.literal).integer);
}

void TestOperators::testCLZ()
{
	TEST_ASSERT_EQUALS(static_cast<int64_t>(31), clz(TYPE_INT32, INT_ONE.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(15), clz(TYPE_INT16, INT_ONE.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(7), clz(TYPE_INT8, INT_ONE.literal).integer);

	TEST_ASSERT_EQUALS(static_cast<int64_t>(32), clz(TYPE_INT32, INT_ZERO.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(16), clz(TYPE_INT16, INT_ZERO.literal).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(8), clz(TYPE_INT8, INT_ZERO.literal).integer);

	TEST_ASSERT_EQUALS(static_cast<int64_t>(24), clz(TYPE_INT32, Literal(static_cast<int64_t>(255))).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(8), clz(TYPE_INT16, Literal(static_cast<int64_t>(255))).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0), clz(TYPE_INT8, Literal(static_cast<int64_t>(255))).integer);

	TEST_ASSERT_EQUALS(static_cast<int64_t>(0), clz(TYPE_INT32, Literal(TYPE_INT32.getScalarWidthMask())).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0), clz(TYPE_INT16, Literal(TYPE_INT16.getScalarWidthMask())).integer);
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0), clz(TYPE_INT8, Literal(TYPE_INT8.getScalarWidthMask())).integer);
}

void TestOperators::testSMOD()
{
}

void TestOperators::testSREM()
{

}

void TestOperators::testFMOD()
{

}

void TestOperators::testFREM()
{

}
