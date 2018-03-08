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
	TEST_ASSERT_EQUALS(static_cast<int32_t>(0), asr(TYPE_INT32, INT_ONE.literal, INT_ONE.literal).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(0), asr(TYPE_INT16, INT_ONE.literal, INT_ONE.literal).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(0), asr(TYPE_INT8, INT_ONE.literal, INT_ONE.literal).signedInt());

	TEST_ASSERT_EQUALS(static_cast<int32_t>(1), asr(TYPE_INT32, INT_ONE.literal, INT_ZERO.literal).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(1), asr(TYPE_INT16, INT_ONE.literal, INT_ZERO.literal).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(1), asr(TYPE_INT8, INT_ONE.literal, INT_ZERO.literal).signedInt());

	TEST_ASSERT_EQUALS(static_cast<int32_t>(127), asr(TYPE_INT32, Literal(255u), INT_ONE.literal).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(127), asr(TYPE_INT16, Literal(255u), INT_ONE.literal).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(127), asr(TYPE_INT8, Literal(255u), INT_ONE.literal).signedInt());

	TEST_ASSERT_EQUALS(static_cast<uint32_t>(0xFFFFFFFF), asr(TYPE_INT32, Literal(TYPE_INT32.getScalarWidthMask()), INT_ONE.literal).unsignedInt());
	TEST_ASSERT_EQUALS(static_cast<uint32_t>(0x7FFF), asr(TYPE_INT16, Literal(TYPE_INT16.getScalarWidthMask()), INT_ONE.literal).unsignedInt());
	TEST_ASSERT_EQUALS(static_cast<uint32_t>(0x7F), asr(TYPE_INT8, Literal(TYPE_INT8.getScalarWidthMask()), INT_ONE.literal).unsignedInt());
}

void TestOperators::testCLZ()
{
	TEST_ASSERT_EQUALS(static_cast<int32_t>(31), clz(TYPE_INT32, INT_ONE.literal).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(31), clz(TYPE_INT16, INT_ONE.literal).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(31), clz(TYPE_INT8, INT_ONE.literal).signedInt());

	TEST_ASSERT_EQUALS(static_cast<int32_t>(32), clz(TYPE_INT32, INT_ZERO.literal).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(32), clz(TYPE_INT16, INT_ZERO.literal).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(32), clz(TYPE_INT8, INT_ZERO.literal).signedInt());

	TEST_ASSERT_EQUALS(static_cast<int32_t>(24), clz(TYPE_INT32, Literal(255u)).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(24), clz(TYPE_INT16, Literal(255u)).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(24), clz(TYPE_INT8, Literal(255u)).signedInt());

	TEST_ASSERT_EQUALS(static_cast<int32_t>(0), clz(TYPE_INT32, Literal(TYPE_INT32.getScalarWidthMask())).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(16), clz(TYPE_INT16, Literal(TYPE_INT16.getScalarWidthMask())).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(24), clz(TYPE_INT8, Literal(TYPE_INT8.getScalarWidthMask())).signedInt());
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
