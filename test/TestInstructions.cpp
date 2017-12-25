/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestInstructions.h"

#include "asm/OpCodes.h"
#include "Bitfield.h"

using namespace vc4c;

TestInstructions::TestInstructions()
{
	TEST_ADD(TestInstructions::testConditionCodes);
	TEST_ADD(TestInstructions::testConstantSaturations);
	TEST_ADD(TestInstructions::testBitfields);
}

TestInstructions::~TestInstructions()
{
	//out-of-line virtual destructor
}

void TestInstructions::testConditionCodes()
{
	TEST_ASSERT(COND_CARRY_CLEAR.isInversionOf(COND_CARRY_SET));
	TEST_ASSERT(COND_CARRY_SET.isInversionOf(COND_CARRY_CLEAR));
	TEST_ASSERT(COND_NEGATIVE_CLEAR.isInversionOf(COND_NEGATIVE_SET));
	TEST_ASSERT(COND_NEGATIVE_SET.isInversionOf(COND_NEGATIVE_CLEAR));
	TEST_ASSERT(COND_ZERO_CLEAR.isInversionOf(COND_ZERO_SET));
	TEST_ASSERT(COND_ZERO_SET.isInversionOf(COND_ZERO_CLEAR));
	TEST_ASSERT(COND_ALWAYS.isInversionOf(COND_NEVER));
	TEST_ASSERT(COND_NEVER.isInversionOf(COND_ALWAYS));

	TEST_ASSERT_EQUALS(COND_NEVER, COND_ALWAYS.invert());
	TEST_ASSERT_EQUALS(COND_CARRY_SET, COND_CARRY_CLEAR.invert());
	TEST_ASSERT_EQUALS(COND_CARRY_CLEAR, COND_CARRY_SET.invert());
	TEST_ASSERT_EQUALS(COND_NEGATIVE_SET, COND_NEGATIVE_CLEAR.invert());
	TEST_ASSERT_EQUALS(COND_NEGATIVE_CLEAR, COND_NEGATIVE_SET.invert());
	TEST_ASSERT_EQUALS(COND_ALWAYS, COND_NEVER.invert());
	TEST_ASSERT_EQUALS(COND_ZERO_SET, COND_ZERO_CLEAR.invert());
	TEST_ASSERT_EQUALS(COND_ZERO_CLEAR, COND_ZERO_SET.invert());

	TEST_ASSERT_EQUALS(BranchCond::ALWAYS, COND_ALWAYS.toBranchCondition());
	TEST_ASSERT_EQUALS(BranchCond::ALL_C_CLEAR, COND_CARRY_CLEAR.toBranchCondition());
	TEST_ASSERT_EQUALS(BranchCond::ANY_C_SET, COND_CARRY_SET.toBranchCondition());
	TEST_ASSERT_EQUALS(BranchCond::ALL_N_CLEAR, COND_NEGATIVE_CLEAR.toBranchCondition());
	TEST_ASSERT_EQUALS(BranchCond::ANY_N_SET, COND_NEGATIVE_SET.toBranchCondition());
	TEST_ASSERT_EQUALS(BranchCond::ALL_Z_CLEAR, COND_ZERO_CLEAR.toBranchCondition());
	TEST_ASSERT_EQUALS(BranchCond::ANY_Z_SET, COND_ZERO_SET.toBranchCondition());
}

void TestInstructions::testConstantSaturations()
{
	TEST_ASSERT_EQUALS(static_cast<int64_t>(127), saturate<signed char>(1024));
	TEST_ASSERT_EQUALS(static_cast<int64_t>(255), saturate<unsigned char>(1024));
	TEST_ASSERT_EQUALS(static_cast<int64_t>(32767), saturate<short>(100000));
	TEST_ASSERT_EQUALS(static_cast<int64_t>(65535), saturate<unsigned short>(100000));
	TEST_ASSERT_EQUALS(static_cast<int64_t>(2147483647), saturate<int>(static_cast<int64_t>(1) << 40));
	TEST_ASSERT_EQUALS(static_cast<int64_t>(4294967295), saturate<unsigned int>(static_cast<int64_t>(1) << 40));

	TEST_ASSERT_EQUALS(static_cast<int64_t>(-128), saturate<signed char>(-1024));
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0), saturate<unsigned char>(-1024));
	TEST_ASSERT_EQUALS(static_cast<int64_t>(-32768), saturate<short>(-100000));
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0), saturate<unsigned short>(-100000));
	TEST_ASSERT_EQUALS(static_cast<int64_t>(-2147483648), saturate<int>(-(static_cast<int64_t>(1) << 40)));
	TEST_ASSERT_EQUALS(static_cast<int64_t>(0), saturate<unsigned int>(-(static_cast<int64_t>(1) << 40)));
}

struct TestBitfield : public Bitfield<int32_t>
{
	BITFIELD_ENTRY(BitOffset1, bool, 1, Bit)
	BITFIELD_ENTRY(ByteOffset7, unsigned char, 7, Byte)
	BITFIELD_ENTRY(TupleOffset9, char, 9, Tuple)
};

void TestInstructions::testBitfields()
{
	TestBitfield t1;
	t1.setBitOffset1(true);
	TEST_ASSERT_EQUALS(true, t1.getBitOffset1());
	TEST_ASSERT_EQUALS(2, t1.value);

	TestBitfield t2;
	t2.setByteOffset7(127);
	TEST_ASSERT_EQUALS(127, t2.getByteOffset7());
	TEST_ASSERT_EQUALS(127 << 7, t2.value);

	TestBitfield t3;
	t3.setTupleOffset9(0x3);
	TEST_ASSERT_EQUALS(3, t3.getTupleOffset9());
	TEST_ASSERT_EQUALS(3 << 9, t3.value);
}
