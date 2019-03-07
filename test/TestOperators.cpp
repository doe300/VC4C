/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestOperators.h"

#include "Values.h"
#include "intrinsics/Operators.h"
#include "intermediate/operators.h"
#include "Module.h"

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

TestOperators::TestOperators()
{
	TEST_ADD(TestOperators::testASR);
	TEST_ADD(TestOperators::testCLZ);
	TEST_ADD(TestOperators::testSMOD);
	TEST_ADD(TestOperators::testSREM);
	TEST_ADD(TestOperators::testFMOD);
	TEST_ADD(TestOperators::testFREM);
	TEST_ADD(TestOperators::testOperatorSyntax);
}

TestOperators::~TestOperators()
{

}

void TestOperators::testASR()
{
	TEST_ASSERT_EQUALS(static_cast<int32_t>(0), asr(INT_ONE.literal(), INT_ONE.literal()).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(1), asr(INT_ONE.literal(), INT_ZERO.literal()).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(127), asr(Literal(255u), INT_ONE.literal()).signedInt());

	TEST_ASSERT_EQUALS(static_cast<uint32_t>(0xFFFFFFFF), asr(Literal(TYPE_INT32.getScalarWidthMask()), INT_ONE.literal()).unsignedInt());
	TEST_ASSERT_EQUALS(static_cast<uint32_t>(0x7FFF), asr(Literal(TYPE_INT16.getScalarWidthMask()), INT_ONE.literal()).unsignedInt());
	TEST_ASSERT_EQUALS(static_cast<uint32_t>(0x7F), asr(Literal(TYPE_INT8.getScalarWidthMask()), INT_ONE.literal()).unsignedInt());
}

void TestOperators::testCLZ()
{
	TEST_ASSERT_EQUALS(static_cast<int32_t>(31), clz(INT_ONE.literal()).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(32), clz(INT_ZERO.literal()).signedInt());

	TEST_ASSERT_EQUALS(static_cast<int32_t>(24), clz(Literal(255u)).signedInt());

	TEST_ASSERT_EQUALS(static_cast<int32_t>(0), clz(Literal(TYPE_INT32.getScalarWidthMask())).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(16), clz(Literal(TYPE_INT16.getScalarWidthMask())).signedInt());
	TEST_ASSERT_EQUALS(static_cast<int32_t>(24), clz(Literal(TYPE_INT8.getScalarWidthMask())).signedInt());
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


void TestOperators::testOperatorSyntax()
{
	Module mod({});
	Method method(mod);
	method.appendToEnd(new intermediate::BranchLabel(*method.addNewLocal(TYPE_LABEL).local()));
	auto it = method.walkAllInstructions();

	TEST_ASSERT_EQUALS(42_val, assign(it, TYPE_INT8) = 21_val + 21_val);
}