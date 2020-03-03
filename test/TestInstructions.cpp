/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestInstructions.h"

#include "Bitfield.h"
#include "GlobalValues.h"
#include "HalfType.h"
#include "Module.h"
#include "Values.h"
#include "analysis/ValueRange.h"
#include "asm/ALUInstruction.h"
#include "asm/LoadInstruction.h"
#include "asm/OpCodes.h"
#include "intermediate/IntermediateInstruction.h"
#include "normalization/LiteralValues.h"

#include <functional>

using namespace vc4c;

extern TypeHolder GLOBAL_TYPE_HOLDER;

TestInstructions::TestInstructions()
{
    TEST_ADD(TestInstructions::testConditionCodes);
    TEST_ADD(TestInstructions::testSignals);
    TEST_ADD(TestInstructions::testUnpackModes);
    TEST_ADD(TestInstructions::testPackModes);
    TEST_ADD(TestInstructions::testConstantSaturations);
    TEST_ADD(TestInstructions::testBitfields);
    TEST_ADD(TestInstructions::testElementFlags);
    TEST_ADD(TestInstructions::testOpCodes);
    TEST_ADD(TestInstructions::testOpCodeProperties);
    TEST_ADD(TestInstructions::testHalfFloat);
    TEST_ADD(TestInstructions::testOpCodeFlags);
    TEST_ADD(TestInstructions::testOpCodeRanges);
    TEST_ADD(TestInstructions::testRegister);
    TEST_ADD(TestInstructions::testImmediates);
    TEST_ADD(TestInstructions::testSIMDVector);
    TEST_ADD(TestInstructions::testValue);
    TEST_ADD(TestInstructions::testTypes);
    TEST_ADD(TestInstructions::testCompoundConstants);
    TEST_ADD(TestInstructions::testALUInstructions);
    TEST_ADD(TestInstructions::testLoadInstruction);
    TEST_ADD(TestInstructions::testValueRanges);
    TEST_ADD(TestInstructions::testInstructionEquality);
}

// out-of-line virtual destructor
TestInstructions::~TestInstructions() = default;

void TestInstructions::testConditionCodes()
{
    TEST_ASSERT(COND_CARRY_CLEAR.isInversionOf(COND_CARRY_SET))
    TEST_ASSERT(COND_CARRY_SET.isInversionOf(COND_CARRY_CLEAR))
    TEST_ASSERT(COND_NEGATIVE_CLEAR.isInversionOf(COND_NEGATIVE_SET))
    TEST_ASSERT(COND_NEGATIVE_SET.isInversionOf(COND_NEGATIVE_CLEAR))
    TEST_ASSERT(COND_ZERO_CLEAR.isInversionOf(COND_ZERO_SET))
    TEST_ASSERT(COND_ZERO_SET.isInversionOf(COND_ZERO_CLEAR))
    TEST_ASSERT(COND_ALWAYS.isInversionOf(COND_NEVER))
    TEST_ASSERT(COND_NEVER.isInversionOf(COND_ALWAYS))

    TEST_ASSERT_EQUALS(COND_NEVER, COND_ALWAYS.invert())
    TEST_ASSERT_EQUALS(COND_CARRY_SET, COND_CARRY_CLEAR.invert())
    TEST_ASSERT_EQUALS(COND_CARRY_CLEAR, COND_CARRY_SET.invert())
    TEST_ASSERT_EQUALS(COND_NEGATIVE_SET, COND_NEGATIVE_CLEAR.invert())
    TEST_ASSERT_EQUALS(COND_NEGATIVE_CLEAR, COND_NEGATIVE_SET.invert())
    TEST_ASSERT_EQUALS(COND_ALWAYS, COND_NEVER.invert())
    TEST_ASSERT_EQUALS(COND_ZERO_SET, COND_ZERO_CLEAR.invert())
    TEST_ASSERT_EQUALS(COND_ZERO_CLEAR, COND_ZERO_SET.invert())

    TEST_ASSERT_EQUALS(BranchCond::ALWAYS, COND_ALWAYS.toBranchCondition())
    TEST_ASSERT_EQUALS(BranchCond::ALL_C_CLEAR, COND_CARRY_CLEAR.toBranchCondition())
    TEST_ASSERT_EQUALS(BranchCond::ANY_C_SET, COND_CARRY_SET.toBranchCondition())
    TEST_ASSERT_EQUALS(BranchCond::ALL_N_CLEAR, COND_NEGATIVE_CLEAR.toBranchCondition())
    TEST_ASSERT_EQUALS(BranchCond::ANY_N_SET, COND_NEGATIVE_SET.toBranchCondition())
    TEST_ASSERT_EQUALS(BranchCond::ALL_Z_CLEAR, COND_ZERO_CLEAR.toBranchCondition())
    TEST_ASSERT_EQUALS(BranchCond::ANY_Z_SET, COND_ZERO_SET.toBranchCondition())
}

void TestInstructions::testSignals()
{
    TEST_ASSERT(SIGNAL_SOFT_BREAK.hasSideEffects())
    TEST_ASSERT(!SIGNAL_NONE.hasSideEffects())
    TEST_ASSERT(SIGNAL_SWITCH_THREAD.hasSideEffects())
    TEST_ASSERT(SIGNAL_END_PROGRAM.hasSideEffects())
    TEST_ASSERT(SIGNAL_WAIT_FOR_SCORE.hasSideEffects())
    TEST_ASSERT(SIGNAL_UNLOCK_SCORE.hasSideEffects())
    TEST_ASSERT(SIGNAL_THREAD_SWITCH_LAST.hasSideEffects())
    TEST_ASSERT(SIGNAL_LOAD_COVERAGE.hasSideEffects())
    TEST_ASSERT(SIGNAL_LOAD_COLOR.hasSideEffects())
    TEST_ASSERT(SIGNAL_LOAD_COLOR_END.hasSideEffects())
    TEST_ASSERT(SIGNAL_LOAD_TMU0.hasSideEffects())
    TEST_ASSERT(SIGNAL_LOAD_TMU1.hasSideEffects())
    TEST_ASSERT(SIGNAL_LOAD_ALPHA.hasSideEffects())
    TEST_ASSERT(!SIGNAL_ALU_IMMEDIATE.hasSideEffects())
    TEST_ASSERT(!SIGNAL_LOAD_IMMEDIATE.hasSideEffects())
    // XXX not sure about this one, is "changing PC" a valid side-effect?
    TEST_ASSERT(SIGNAL_BRANCH.hasSideEffects())

    TEST_ASSERT(!SIGNAL_SOFT_BREAK.triggersReadOfR4())
    TEST_ASSERT(!SIGNAL_NONE.triggersReadOfR4())
    TEST_ASSERT(!SIGNAL_SWITCH_THREAD.triggersReadOfR4())
    TEST_ASSERT(!SIGNAL_END_PROGRAM.triggersReadOfR4())
    TEST_ASSERT(!SIGNAL_WAIT_FOR_SCORE.triggersReadOfR4())
    TEST_ASSERT(!SIGNAL_UNLOCK_SCORE.triggersReadOfR4())
    TEST_ASSERT(!SIGNAL_THREAD_SWITCH_LAST.triggersReadOfR4())
    TEST_ASSERT(SIGNAL_LOAD_COVERAGE.triggersReadOfR4())
    TEST_ASSERT(SIGNAL_LOAD_COLOR.triggersReadOfR4())
    TEST_ASSERT(SIGNAL_LOAD_COLOR_END.triggersReadOfR4())
    TEST_ASSERT(SIGNAL_LOAD_TMU0.triggersReadOfR4())
    TEST_ASSERT(SIGNAL_LOAD_TMU1.triggersReadOfR4())
    TEST_ASSERT(SIGNAL_LOAD_ALPHA.triggersReadOfR4())
    TEST_ASSERT(!SIGNAL_ALU_IMMEDIATE.triggersReadOfR4())
    TEST_ASSERT(!SIGNAL_LOAD_IMMEDIATE.triggersReadOfR4())
    TEST_ASSERT(!SIGNAL_BRANCH.triggersReadOfR4())
}

void TestInstructions::testUnpackModes()
{
    uint32_t halfOne = static_cast<uint16_t>(half_t{1.0f});
    uint32_t colorHalf = 0x7F;
    Value floatHalf(Literal(127.0f / 255.0f), TYPE_FLOAT);

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_NOP(INT_MINUS_ONE))
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_NOP(INT_ZERO))

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_16A_32(INT_MINUS_ONE))
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_16A_32(Value(Literal(0xFFFF), TYPE_INT16)))
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_16A_32(INT_ZERO))
    TEST_ASSERT_EQUALS(ELEMENT_NUMBERS, UNPACK_16A_32(ELEMENT_NUMBERS))
    TEST_ASSERT_EQUALS(FLOAT_ONE, UNPACK_16A_32(Value(Literal(halfOne), TYPE_FLOAT)))

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_16B_32(INT_MINUS_ONE))
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_16B_32(Value(Literal(0xFFFF0000u), TYPE_INT32)))
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_16B_32(INT_ZERO))
    TEST_ASSERT_EQUALS(FLOAT_ONE, UNPACK_16B_32(Value(Literal(halfOne << 16), TYPE_FLOAT)))

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_8888_32(INT_MINUS_ONE))
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_8888_32(Value(Literal(0xFF), TYPE_INT8)))
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_8888_32(INT_ZERO))

    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT32), UNPACK_8A_32(Value(Literal(0xFF), TYPE_INT8)))
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_8A_32(INT_ZERO))
    TEST_ASSERT_EQUALS(floatHalf, UNPACK_8A_32(Value(Literal(colorHalf), TYPE_FLOAT)))

    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT32), UNPACK_8B_32(Value(Literal(0xFF00), TYPE_INT8)))
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_8B_32(INT_ZERO))
    TEST_ASSERT_EQUALS(floatHalf, UNPACK_8B_32(Value(Literal(colorHalf << 8), TYPE_FLOAT)))

    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT32), UNPACK_8C_32(Value(Literal(0xFF0000), TYPE_INT8)))
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_8C_32(INT_ZERO))
    TEST_ASSERT_EQUALS(floatHalf, UNPACK_8C_32(Value(Literal(colorHalf << 16), TYPE_FLOAT)))

    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT32), UNPACK_8D_32(Value(Literal(0xFF000000u), TYPE_INT8)))
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_8D_32(INT_ZERO))
    TEST_ASSERT_EQUALS(floatHalf, UNPACK_8D_32(Value(Literal(colorHalf << 24), TYPE_FLOAT)))

    TEST_ASSERT_EQUALS(FLOAT_ZERO, UNPACK_R4_COLOR0(INT_ZERO))
    TEST_ASSERT_EQUALS(FLOAT_ONE, UNPACK_R4_COLOR0(Value(Literal(0xFF), TYPE_INT8)))
    // 127/255 is not exactly 1/2!
    TEST_ASSERT_EQUALS(Value(Literal(0x3efefeffu), TYPE_FLOAT), UNPACK_R4_COLOR0(Value(Literal(0x7F), TYPE_INT8)))

    TEST_ASSERT_EQUALS(FLOAT_ZERO, UNPACK_R4_COLOR1(INT_ZERO))
    TEST_ASSERT_EQUALS(FLOAT_ONE, UNPACK_R4_COLOR1(Value(Literal(0xFF12), TYPE_INT8)))
    TEST_ASSERT_EQUALS(Value(Literal(0x3efefeffu), TYPE_FLOAT), UNPACK_R4_COLOR1(Value(Literal(0x7F12), TYPE_INT8)))

    TEST_ASSERT_EQUALS(FLOAT_ZERO, UNPACK_R4_COLOR2(INT_ZERO))
    TEST_ASSERT_EQUALS(FLOAT_ONE, UNPACK_R4_COLOR2(Value(Literal(0xFF1234), TYPE_INT8)))
    TEST_ASSERT_EQUALS(Value(Literal(0x3efefeffu), TYPE_FLOAT), UNPACK_R4_COLOR2(Value(Literal(0x7F1234), TYPE_INT8)))

    TEST_ASSERT_EQUALS(FLOAT_ZERO, UNPACK_R4_COLOR3(INT_ZERO))
    TEST_ASSERT_EQUALS(FLOAT_ONE, UNPACK_R4_COLOR3(Value(Literal(0xFF123456), TYPE_INT8)))
    TEST_ASSERT_EQUALS(Value(Literal(0x3efefeffu), TYPE_FLOAT), UNPACK_R4_COLOR3(Value(Literal(0x7F123456), TYPE_INT8)))

    TEST_ASSERT_EQUALS(FLOAT_ZERO, UNPACK_R4_16A_32(INT_ZERO))
    TEST_ASSERT_EQUALS(FLOAT_ONE, UNPACK_R4_16A_32(Value(Literal(static_cast<uint16_t>(1.0_h)), TYPE_HALF)))
    TEST_ASSERT_EQUALS(
        FLOAT_ONE, UNPACK_R4_16A_32(Value(Literal(static_cast<uint16_t>(1.0_h) | 0x12340000u), TYPE_HALF)))

    TEST_ASSERT_EQUALS(FLOAT_ZERO, UNPACK_R4_16B_32(INT_ZERO))
    TEST_ASSERT_EQUALS(FLOAT_ONE, UNPACK_R4_16B_32(Value(Literal(static_cast<uint16_t>(1.0_h) << 16), TYPE_HALF)))
    TEST_ASSERT_EQUALS(FLOAT_ONE,
        UNPACK_R4_16B_32(
            Value(Literal(static_cast<uint32_t>(static_cast<uint16_t>(1.0_h) << 16u) | 0x00001234u), TYPE_HALF)))

    // value ranges
    using namespace analysis;
    ValueRange shortRange(static_cast<double>(std::numeric_limits<int16_t>::min()), 12345.0);
    ValueRange notSoShortRange(-65000.0, 17.0);
    TEST_ASSERT_EQUALS(ValueRange(TYPE_FLOAT), UNPACK_NOP(ValueRange(TYPE_FLOAT), true))
    TEST_ASSERT_EQUALS(ValueRange(TYPE_INT32), UNPACK_NOP_PM(ValueRange(TYPE_INT32), false))

    TEST_ASSERT_EQUALS(shortRange, UNPACK_16A_32(shortRange, true))
    TEST_ASSERT_EQUALS(shortRange, UNPACK_16A_32(shortRange, false))
    TEST_ASSERT_EQUALS(ValueRange(TYPE_INT32), UNPACK_16A_32(notSoShortRange, false))

    TEST_ASSERT_EQUALS(ValueRange(17.0 / 255.0, 127.0 / 255.0), UNPACK_8A_32(ValueRange{17.0, 127.0}, true))
    TEST_ASSERT_EQUALS(ValueRange(TYPE_FLOAT), UNPACK_8A_32(ValueRange{-17.0, 2127.0}, true))
    TEST_ASSERT_EQUALS(ValueRange(17.0, 42.0), UNPACK_8A_32(ValueRange(17.0, 42.0), false))
    TEST_ASSERT_EQUALS(ValueRange(TYPE_INT32), UNPACK_8A_32(shortRange, false))

    TEST_ASSERT_EQUALS(shortRange, UNPACK_R4_16A_32(shortRange, true))
    TEST_ASSERT_EQUALS(ValueRange(TYPE_FLOAT), UNPACK_R4_16A_32(ValueRange(-70000.0, 80000.0), true))
    // TODO actually needs to test "not-so-half" range
    TEST_ASSERT_EQUALS(notSoShortRange, UNPACK_R4_16A_32(notSoShortRange, false))

    TEST_ASSERT_EQUALS(ValueRange(17.0 / 255.0, 127.0 / 255.0), UNPACK_R4_COLOR0(ValueRange{17.0, 127.0}, true))
}

void TestInstructions::testPackModes()
{
    uint32_t halfOne = static_cast<uint16_t>(half_t{1.0f});

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, PACK_NOP(INT_MINUS_ONE, {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_NOP(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFFFF), TYPE_INT16), PACK_32_16A(INT_MINUS_ONE, {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_16A(INT_ZERO, {}))
    TEST_ASSERT_EQUALS(ELEMENT_NUMBERS, PACK_32_16A(ELEMENT_NUMBERS, {}))
    TEST_ASSERT_EQUALS(Value(Literal(halfOne), TYPE_FLOAT), PACK_32_16A(FLOAT_ONE, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFFFF0000u), TYPE_INT32), PACK_32_16B(INT_MINUS_ONE, {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_16B(INT_ZERO, {}))
    TEST_ASSERT_EQUALS(Value(Literal(halfOne << 16), TYPE_FLOAT), PACK_32_16B(FLOAT_ONE, {}))

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, PACK_32_8888(INT_MINUS_ONE, {}))
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, PACK_32_8888(Value(Literal(0xFF), TYPE_INT8), {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8888(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT8), PACK_32_8A(INT_MINUS_ONE, {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8A(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFF00), TYPE_INT8), PACK_32_8B(INT_MINUS_ONE, {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8B(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFF0000), TYPE_INT8), PACK_32_8C(INT_MINUS_ONE, {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8C(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFF000000u), TYPE_INT8), PACK_32_8D(INT_MINUS_ONE, {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8D(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFFFF), TYPE_INT16), PACK_32_16A_S(INT_MINUS_ONE, {}))
    TEST_ASSERT_EQUALS(Value(Literal(0x8000), TYPE_INT16), PACK_32_16A_S(Value(Literal(0x87654321u), TYPE_INT32), {}))
    TEST_ASSERT_EQUALS(Value(Literal(0x7FFF), TYPE_INT16), PACK_32_16A_S(Value(Literal(0x12345678u), TYPE_INT32), {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_16A_S(INT_ZERO, {}))
    TEST_ASSERT_EQUALS(Value(Literal(halfOne), TYPE_FLOAT), PACK_32_16A_S(FLOAT_ONE, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFFFF0000u), TYPE_INT16), PACK_32_16B_S(INT_MINUS_ONE, {}))
    TEST_ASSERT_EQUALS(
        Value(Literal(0x80000000u), TYPE_INT16), PACK_32_16B_S(Value(Literal(0x87654321u), TYPE_INT32), {}))
    TEST_ASSERT_EQUALS(
        Value(Literal(0x7FFF0000u), TYPE_INT16), PACK_32_16B_S(Value(Literal(0x12345678u), TYPE_INT32), {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_16B_S(INT_ZERO, {}))
    TEST_ASSERT_EQUALS(Value(Literal(halfOne << 16), TYPE_FLOAT), PACK_32_16B_S(FLOAT_ONE, {}))

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, PACK_32_8888_S(Value(Literal(0xFF), TYPE_INT8), {}))
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, PACK_32_8888_S(Value(Literal(0x12345678u), TYPE_INT32), {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8888_S(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFFu), TYPE_INT8), PACK_32_8A_S(Value(Literal(0x12345678u), TYPE_INT32), {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8A_S(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFF00u), TYPE_INT8), PACK_32_8B_S(Value(Literal(0x12345678u), TYPE_INT32), {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8B_S(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(Value(Literal(0xFF0000u), TYPE_INT8), PACK_32_8C_S(Value(Literal(0x12345678u), TYPE_INT32), {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8C_S(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(
        Value(Literal(0xFF000000u), TYPE_INT8), PACK_32_8D_S(Value(Literal(0x12345678u), TYPE_INT32), {}))
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8D_S(INT_ZERO, {}))

    TEST_ASSERT_EQUALS(INT_ZERO, PACK_MUL_GRAY_REPLICATE(FLOAT_ZERO, {}))
    TEST_ASSERT_EQUALS(
        Value(Literal(0x7F7F7F7F), TYPE_INT8), PACK_MUL_GRAY_REPLICATE(Value(Literal(0.5f), TYPE_FLOAT), {}))
    TEST_ASSERT_EQUALS(Value(Literal(0xFFFFFFFF), TYPE_INT8), PACK_MUL_GRAY_REPLICATE(FLOAT_ONE, {}))

    TEST_ASSERT_EQUALS(INT_ZERO, PACK_MUL_COLOR0(FLOAT_ZERO, {}))
    TEST_ASSERT_EQUALS(Value(Literal(0x7F), TYPE_INT8), PACK_MUL_COLOR0(Value(Literal(0.5f), TYPE_FLOAT), {}))
    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT8), PACK_MUL_COLOR0(FLOAT_ONE, {}))

    TEST_ASSERT_EQUALS(INT_ZERO, PACK_MUL_COLOR1(FLOAT_ZERO, {}))
    TEST_ASSERT_EQUALS(Value(Literal(0x7F00), TYPE_INT8), PACK_MUL_COLOR1(Value(Literal(0.5f), TYPE_FLOAT), {}))
    TEST_ASSERT_EQUALS(Value(Literal(0xFF00), TYPE_INT8), PACK_MUL_COLOR1(FLOAT_ONE, {}))

    TEST_ASSERT_EQUALS(INT_ZERO, PACK_MUL_COLOR2(FLOAT_ZERO, {}))
    TEST_ASSERT_EQUALS(Value(Literal(0x7F0000), TYPE_INT8), PACK_MUL_COLOR2(Value(Literal(0.5f), TYPE_FLOAT), {}))
    TEST_ASSERT_EQUALS(Value(Literal(0xFF0000), TYPE_INT8), PACK_MUL_COLOR2(FLOAT_ONE, {}))

    TEST_ASSERT_EQUALS(INT_ZERO, PACK_MUL_COLOR3(FLOAT_ZERO, {}))
    TEST_ASSERT_EQUALS(Value(Literal(0x7F000000), TYPE_INT8), PACK_MUL_COLOR3(Value(Literal(0.5f), TYPE_FLOAT), {}))
    TEST_ASSERT_EQUALS(Value(Literal(0xFF000000), TYPE_INT8), PACK_MUL_COLOR3(FLOAT_ONE, {}))

    // value ranges
    using namespace analysis;
    ValueRange shortRange(static_cast<double>(std::numeric_limits<int16_t>::min()), 12345.0);
    ValueRange notSoShortRange(-65000.0, 17.0);

    TEST_ASSERT_EQUALS(notSoShortRange, PACK_NOP(notSoShortRange, false))
    TEST_ASSERT_EQUALS(notSoShortRange, PACK_NOP_PM(notSoShortRange, false))

    TEST_ASSERT_EQUALS(shortRange, PACK_32_16A(shortRange, true))
    TEST_ASSERT_EQUALS(ValueRange(TYPE_HALF), PACK_32_16A(ValueRange(-70000.0, 80000.0), true))
    TEST_ASSERT_EQUALS(ValueRange(TYPE_INT32), PACK_32_16A(shortRange, false))
    TEST_ASSERT_EQUALS(ValueRange(17.0, 1227.0), PACK_32_16A(ValueRange(17.0, 1227.0), false))

    TEST_ASSERT_EQUALS(RANGE_UCHAR, PACK_32_8A(shortRange, false))
    TEST_ASSERT_EQUALS(ValueRange(17.0, 127.0), PACK_32_8A(ValueRange(17.0, 127.0), false))

    TEST_ASSERT_EQUALS(shortRange, PACK_32_32(shortRange, false))
    TEST_ASSERT_EQUALS(notSoShortRange, PACK_32_32(notSoShortRange, false))

    TEST_ASSERT_EQUALS(shortRange, PACK_32_16A_S(shortRange, true))
    TEST_ASSERT_EQUALS(ValueRange(TYPE_HALF), PACK_32_16A_S(ValueRange(-70000.0, 80000.0), true))
    TEST_ASSERT_EQUALS(shortRange, PACK_32_16A_S(shortRange, false))
    TEST_ASSERT_EQUALS(ValueRange(static_cast<double>(std::numeric_limits<int16_t>::min()), 17.0),
        PACK_32_16A_S(notSoShortRange, false))

    TEST_ASSERT_EQUALS(ValueRange(0.0, 17.0), PACK_32_8A_S(notSoShortRange, false))
    TEST_ASSERT_EQUALS(ValueRange(17.0, 255.0), PACK_32_8A_S(ValueRange(17.0, 1207.0), false))

    TEST_ASSERT_EQUALS(RANGE_UCHAR, PACK_MUL_COLOR0(ValueRange(-17.0, 255.0), false))
    TEST_ASSERT_EQUALS(ValueRange(17.0, 127.0), PACK_MUL_COLOR0(ValueRange(17.0 / 255.0, 127.0 / 255.0), false))
}

void TestInstructions::testConstantSaturations()
{
    TEST_ASSERT_EQUALS(static_cast<int64_t>(127), saturate<signed char>(1024))
    TEST_ASSERT_EQUALS(static_cast<int64_t>(255), saturate<unsigned char>(1024))
    TEST_ASSERT_EQUALS(static_cast<int64_t>(32767), saturate<short>(100000))
    TEST_ASSERT_EQUALS(static_cast<int64_t>(65535), saturate<unsigned short>(100000))
    TEST_ASSERT_EQUALS(static_cast<int64_t>(2147483647), saturate<int>(static_cast<int64_t>(1) << 40))
    TEST_ASSERT_EQUALS(static_cast<int64_t>(4294967295), saturate<unsigned int>(static_cast<int64_t>(1) << 40))

    TEST_ASSERT_EQUALS(static_cast<int64_t>(-128), saturate<signed char>(-1024))
    TEST_ASSERT_EQUALS(static_cast<int64_t>(0), saturate<unsigned char>(-1024))
    TEST_ASSERT_EQUALS(static_cast<int64_t>(-32768), saturate<short>(-100000))
    TEST_ASSERT_EQUALS(static_cast<int64_t>(0), saturate<unsigned short>(-100000))
    TEST_ASSERT_EQUALS(static_cast<int64_t>(-2147483648), saturate<int>(-(static_cast<int64_t>(1) << 40)))
    TEST_ASSERT_EQUALS(static_cast<int64_t>(0), saturate<unsigned int>(-(static_cast<int64_t>(1) << 40)))

    TEST_ASSERT_EQUALS(42.0f, saturate(42.0))
    TEST_ASSERT_EQUALS(-42.0f, saturate(-42.0))
    TEST_ASSERT_EQUALS(
        std::numeric_limits<float>::lowest(), saturate(2.0 * static_cast<double>(std::numeric_limits<float>::lowest())))
    TEST_ASSERT_EQUALS(
        std::numeric_limits<float>::max(), saturate(2.0 * static_cast<double>(std::numeric_limits<float>::max())))
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
    TEST_ASSERT_EQUALS(true, t1.getBitOffset1())
    TEST_ASSERT_EQUALS(2, t1.value)

    TestBitfield t2;
    t2.setByteOffset7(127);
    TEST_ASSERT_EQUALS(127, t2.getByteOffset7())
    TEST_ASSERT_EQUALS(127 << 7, t2.value)

    TestBitfield t3;
    t3.setTupleOffset9(0x3);
    TEST_ASSERT_EQUALS(3, t3.getTupleOffset9())
    TEST_ASSERT_EQUALS(3 << 9, t3.value)
}

void TestInstructions::testElementFlags()
{
    ElementFlags flags;
    TEST_ASSERT(flags.matchesCondition(COND_ALWAYS))
    TEST_ASSERT(!flags.matchesCondition(COND_NEVER))

    flags = ElementFlags::fromValue(INT_ZERO);
    flags.carry = flags.overflow = FlagStatus::CLEAR;
    ElementFlags zero{FlagStatus::SET, FlagStatus::CLEAR, FlagStatus::CLEAR, FlagStatus::CLEAR};
    TEST_ASSERT_EQUALS(zero, flags)
    TEST_ASSERT(flags.matchesCondition(COND_ZERO_SET))
    TEST_ASSERT(!flags.matchesCondition(COND_ZERO_CLEAR))
    TEST_ASSERT(!flags.matchesCondition(COND_NEGATIVE_SET))
    TEST_ASSERT(flags.matchesCondition(COND_NEGATIVE_CLEAR))
    TEST_ASSERT(!flags.matchesCondition(COND_CARRY_SET))
    TEST_ASSERT(flags.matchesCondition(COND_CARRY_CLEAR))

    flags = ElementFlags::fromValue(INT_MINUS_ONE);
    flags.carry = flags.overflow = FlagStatus::CLEAR;
    ElementFlags minumsOne{FlagStatus::CLEAR, FlagStatus::SET, FlagStatus::CLEAR, FlagStatus::CLEAR};
    TEST_ASSERT_EQUALS(minumsOne, flags)
    TEST_ASSERT(!flags.matchesCondition(COND_ZERO_SET))
    TEST_ASSERT(flags.matchesCondition(COND_ZERO_CLEAR))
    TEST_ASSERT(flags.matchesCondition(COND_NEGATIVE_SET))
    TEST_ASSERT(!flags.matchesCondition(COND_NEGATIVE_CLEAR))
    TEST_ASSERT(!flags.matchesCondition(COND_CARRY_SET))
    TEST_ASSERT(flags.matchesCondition(COND_CARRY_CLEAR))

    flags = ElementFlags{FlagStatus::CLEAR, FlagStatus::CLEAR, FlagStatus::SET, FlagStatus::CLEAR};
    TEST_ASSERT(!flags.matchesCondition(COND_ZERO_SET))
    TEST_ASSERT(flags.matchesCondition(COND_ZERO_CLEAR))
    TEST_ASSERT(!flags.matchesCondition(COND_NEGATIVE_SET))
    TEST_ASSERT(flags.matchesCondition(COND_NEGATIVE_CLEAR))
    TEST_ASSERT(flags.matchesCondition(COND_CARRY_SET))
    TEST_ASSERT(!flags.matchesCondition(COND_CARRY_CLEAR))
}

void TestInstructions::testOpCodes()
{
    const Value FLOAT_MINUS_ONE(Literal(-1.0f), TYPE_FLOAT);
    TEST_ASSERT_EQUALS(INT_ONE, OP_ADD(INT_ONE, INT_ZERO).first.value())
    TEST_ASSERT_EQUALS(INT_ZERO, OP_ADD(INT_ONE, INT_MINUS_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ZERO, OP_AND(INT_ONE, INT_ZERO).first.value())
    TEST_ASSERT_EQUALS(INT_ONE, OP_AND(INT_ONE, INT_MINUS_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ZERO, OP_ASR(INT_ONE, INT_ONE).first.value())
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_ASR(INT_MINUS_ONE, INT_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ZERO, OP_CLZ(INT_MINUS_ONE, NO_VALUE).first.value())
    TEST_ASSERT_EQUALS(Value(Literal(31u), TYPE_INT8), OP_CLZ(INT_ONE, NO_VALUE).first.value())
    TEST_ASSERT_EQUALS(Value(Literal(32u), TYPE_INT8), OP_CLZ(INT_ZERO, NO_VALUE).first.value())

    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FADD(FLOAT_ONE, FLOAT_ZERO).first.value())
    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_FADD(FLOAT_ONE, FLOAT_MINUS_ONE).first.value())

    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FMAX(FLOAT_ONE, FLOAT_ZERO).first.value())
    TEST_ASSERT_EQUALS(FLOAT_INF, OP_FMAX(FLOAT_INF, FLOAT_ONE).first.value())
    TEST_ASSERT_EQUALS(FLOAT_NAN, OP_FMAX(FLOAT_INF, FLOAT_NAN).first.value())

    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FMAXABS(FLOAT_ZERO, FLOAT_MINUS_ONE).first.value())
    TEST_ASSERT_EQUALS(FLOAT_NAN, OP_FMAXABS(FLOAT_INF, FLOAT_NAN).first.value())

    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_FMIN(FLOAT_ONE, FLOAT_ZERO).first.value())
    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FMIN(FLOAT_INF, FLOAT_ONE).first.value())
    TEST_ASSERT_EQUALS(FLOAT_INF, OP_FMIN(FLOAT_INF, FLOAT_NAN).first.value())

    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_FMINABS(FLOAT_ZERO, FLOAT_MINUS_ONE).first.value())
    TEST_ASSERT_EQUALS(FLOAT_INF, OP_FMINABS(FLOAT_INF, FLOAT_NAN).first.value())

    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FMUL(FLOAT_ONE, FLOAT_ONE).first.value())
    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_FMUL(FLOAT_ONE, FLOAT_ZERO).first.value())
    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FMUL(FLOAT_MINUS_ONE, FLOAT_MINUS_ONE).first.value())

    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_FSUB(FLOAT_ONE, FLOAT_ONE).first.value())
    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FSUB(FLOAT_ZERO, FLOAT_MINUS_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ZERO, OP_FTOI(FLOAT_ZERO, NO_VALUE).first.value())
    TEST_ASSERT_EQUALS(INT_ONE, OP_FTOI(FLOAT_ONE, NO_VALUE).first.value())
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_FTOI(FLOAT_MINUS_ONE, NO_VALUE).first.value())
    TEST_ASSERT_EQUALS(INT_ZERO, OP_FTOI(FLOAT_INF, NO_VALUE).first.value())
    TEST_ASSERT_EQUALS(INT_ZERO, OP_FTOI(FLOAT_NAN, NO_VALUE).first.value())

    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_ITOF(INT_ZERO, NO_VALUE).first.value())
    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_ITOF(INT_ONE, NO_VALUE).first.value())
    TEST_ASSERT_EQUALS(FLOAT_MINUS_ONE, OP_ITOF(INT_MINUS_ONE, NO_VALUE).first.value())

    TEST_ASSERT_EQUALS(INT_ONE, OP_MAX(INT_ONE, INT_ZERO).first.value())
    TEST_ASSERT_EQUALS(INT_ZERO, OP_MAX(INT_ZERO, INT_MINUS_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ZERO, OP_MIN(INT_ONE, INT_ZERO).first.value())
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_MIN(INT_ZERO, INT_MINUS_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ONE, OP_MUL24(INT_ONE, INT_ONE).first.value())
    TEST_ASSERT_EQUALS(INT_ZERO, OP_MUL24(INT_ONE, INT_ZERO).first.value())

    TEST_ASSERT_EQUALS(NO_VALUE, OP_NOP(UNDEFINED_VALUE, NO_VALUE).first)

    TEST_ASSERT_EQUALS(INT_ZERO, OP_NOT(INT_MINUS_ONE, NO_VALUE).first.value())
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_NOT(INT_ZERO, NO_VALUE).first.value())

    TEST_ASSERT_EQUALS(INT_ONE, OP_OR(INT_ONE, INT_ZERO).first.value())
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_OR(INT_MINUS_ONE, INT_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_ROR(INT_MINUS_ONE, INT_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ONE, OP_SHL(INT_ONE, INT_ZERO).first.value())

    TEST_ASSERT_EQUALS(INT_ONE, OP_SHR(INT_ONE, INT_ZERO).first.value())
    TEST_ASSERT_EQUALS(INT_ZERO, OP_SHR(INT_ONE, INT_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ZERO, OP_SUB(INT_ONE, INT_ONE).first.value())
    TEST_ASSERT_EQUALS(INT_ONE, OP_SUB(INT_ZERO, INT_MINUS_ONE).first.value())
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_SUB(INT_ZERO, INT_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ONE, OP_V8ADDS(INT_ONE, INT_ZERO).first.value())

    TEST_ASSERT_EQUALS(INT_ZERO, OP_V8MAX(INT_ZERO, INT_ZERO).first.value())
    TEST_ASSERT_EQUALS(INT_ONE, OP_V8MAX(INT_ZERO, INT_ONE).first.value())
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_V8MAX(INT_ONE, INT_MINUS_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ZERO, OP_V8MIN(INT_ZERO, INT_ZERO).first.value())
    TEST_ASSERT_EQUALS(INT_ZERO, OP_V8MIN(INT_ZERO, INT_ONE).first.value())
    TEST_ASSERT_EQUALS(INT_ONE, OP_V8MIN(INT_ONE, INT_MINUS_ONE).first.value())

    TEST_ASSERT_EQUALS(INT_ZERO, OP_V8SUBS(INT_ONE, INT_ONE).first.value())
    TEST_ASSERT_EQUALS(INT_ONE, OP_V8MAX(INT_ONE, INT_ZERO).first.value())

    TEST_ASSERT_EQUALS(INT_ZERO, OP_V8MULD(INT_ZERO, INT_ZERO).first.value())
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_V8MULD(INT_MINUS_ONE, INT_MINUS_ONE).first.value())
}

void TestInstructions::testOpCodeProperties()
{
    static const std::vector<OpCode> opCodes = {OP_ADD, OP_AND, OP_ASR, OP_CLZ, OP_FADD, OP_FMAX, OP_FMAXABS, OP_FMIN,
        OP_FMINABS, OP_FMUL, OP_FSUB, OP_FTOI, OP_ITOF, OP_MAX, OP_MIN, OP_MUL24, OP_NOP, OP_NOT, OP_OR, OP_ROR, OP_SHL,
        OP_SHR, OP_SUB, OP_V8ADDS, OP_V8MAX, OP_V8MIN, OP_V8MULD, OP_V8SUBS, OP_XOR};

    for(const auto& op : opCodes)
    {
        Value arg = op.acceptsFloat ? Value(Literal(-10.0f), TYPE_FLOAT) : Value(Literal(-10), TYPE_INT32);
        Value arg2 = op.acceptsFloat ? Value(Literal(2.0f), TYPE_FLOAT) : Value(Literal(2u), TYPE_INT32);
        if(op.isIdempotent())
        {
            if(op.numOperands == 1)
            {
                if(op(*op(*op(arg2, NO_VALUE).first, NO_VALUE).first, NO_VALUE).first != op(arg2, NO_VALUE).first)
                {
                    TEST_ASSERT_EQUALS(
                        op(*op(*op(arg2, NO_VALUE).first, NO_VALUE).first, NO_VALUE).first, op(arg2, NO_VALUE).first)
                    TEST_ASSERT_EQUALS("idempotent", op.name)
                }
            }
            else
            {
                if(arg2 != op(arg2, arg2).first)
                {
                    TEST_ASSERT_EQUALS(arg2, op(arg2, arg2).first)
                    TEST_ASSERT_EQUALS("idempotent", op.name)
                }
            }
        }
        if(op.isSelfInverse())
        {
            if(op.numOperands == 1)
            {
                if(op(*op(arg2, NO_VALUE).first, NO_VALUE).first != arg2)
                {
                    TEST_ASSERT_EQUALS(op(*op(arg2, NO_VALUE).first, NO_VALUE).first, arg2)
                    TEST_ASSERT_EQUALS("selfInverse", op.name)
                }
            }
            else
            {
                if(op(arg2, arg2).first != INT_ZERO)
                {
                    TEST_ASSERT_EQUALS(op(arg2, arg2).first, INT_ZERO)
                    TEST_ASSERT_EQUALS("selfInverse", op.name)
                }
            }
        }
        if(op.isAssociative())
        {
            if(op(arg, op(arg, arg2).first).first != op(*op(arg, arg).first, arg2).first)
            {
                TEST_ASSERT_EQUALS(op(arg, op(arg, arg2).first).first, op(*op(arg, arg).first, arg2).first)
                TEST_ASSERT_EQUALS("associative", op.name)
            }
        }
        if(op.isCommutative())
        {
            if(op(arg, arg2).first != op(arg2, arg).first)
            {
                TEST_ASSERT_EQUALS(op(arg, arg2).first, op(arg2, arg).first)
                TEST_ASSERT_EQUALS("commutative", op.name)
            }
        }

        auto special = OpCode::getLeftIdentity(op);
        if(special && (op(*special, arg2).first != arg2))
        {
            TEST_ASSERT_EQUALS(op(*special, arg2).first, arg2)
            TEST_ASSERT_EQUALS("left identity", op.name)
        }
        special = OpCode::getRightIdentity(op);
        if(special && (op(arg2, special).first != arg2))
        {
            TEST_ASSERT_EQUALS(op(arg2, special).first, arg2)
            TEST_ASSERT_EQUALS("right identity", op.name)
        }
        special = OpCode::getLeftAbsorbingElement(op);
        if(special && (op(*special, arg2).first != special))
        {
            TEST_ASSERT_EQUALS(op(*special, arg2).first, special)
            TEST_ASSERT_EQUALS("left absorbing element", op.name)
        }
        special = OpCode::getRightAbsorbingElement(op);
        if(special && (op(arg2, special).first != special))
        {
            TEST_ASSERT_EQUALS(op(arg2, special).first, special)
            TEST_ASSERT_EQUALS("right absorbing element", op.name)
        }

        for(const auto& op2 : opCodes)
        {
            if(op.isLeftDistributiveOver(op2))
            {
                if(op(arg, op2(arg, arg2).first).first != op2(*op(arg, arg).first, op(arg, arg2).first).first)
                {
                    TEST_ASSERT_EQUALS(
                        op(arg, op2(arg, arg2).first).first, op2(*op(arg, arg).first, op(arg, arg2).first).first)
                    TEST_ASSERT_EQUALS(std::string("left distributive over ") + op2.name, op.name)
                }
            }
            if(op.isRightDistributiveOver(op2))
            {
                if(op(*op2(arg, arg2).first, arg).first != op2(*op(arg, arg).first, op(arg2, arg).first).first)
                {
                    TEST_ASSERT_EQUALS(
                        op(*op2(arg, arg2).first, arg).first, op2(*op(arg, arg).first, op(arg2, arg).first).first)
                    TEST_ASSERT_EQUALS(std::string("right distributive over ") + op2.name, op.name)
                }
            }
        }
    }
}

void TestInstructions::testHalfFloat()
{
    TEST_ASSERT_EQUALS(0.0f, static_cast<float>(HALF_ZERO))
    TEST_ASSERT_EQUALS(1.0f, static_cast<float>(HALF_ONE))
    TEST_ASSERT_EQUALS(1.0f, static_cast<float>(half_t(1.0f)))
    TEST_ASSERT_EQUALS(0.0f, static_cast<float>(half_t(0.0f)))
    TEST_ASSERT_EQUALS(1000.0f, static_cast<float>(half_t(1000.0f)))

    for(uint32_t i = 0; i <= std::numeric_limits<uint16_t>::max(); ++i)
    {
        // TODO test half with float constructor (e.g. for float subnormals)
        half_t h(static_cast<uint16_t>(i));
        if(h.isNaN())
            TEST_ASSERT(std::isnan(static_cast<float>(h)))
        else
        {
            TEST_ASSERT_EQUALS(static_cast<float>(h),
                UNPACK_16A_32(Value(Literal(static_cast<uint32_t>(i)), TYPE_HALF))->getLiteralValue()->real())
            TEST_ASSERT_EQUALS(
                i, PACK_32_16A(Value(Literal(static_cast<float>(h)), TYPE_FLOAT), {})->getLiteralValue()->unsignedInt())
        }
    }
}

enum class FlagsMask
{
    ZERO,
    NEGATIVE,
    CARRY,
    SIGNED_OVERFLOW
};

void TestInstructions::testOpCodeFlags()
{
    const auto checkFlagSet = [](VectorFlags&& flags, FlagsMask mask) -> bool {
        switch(mask)
        {
        case FlagsMask::ZERO:
            return flags[0].zero == FlagStatus::SET;
        case FlagsMask::NEGATIVE:
            return flags[0].negative == FlagStatus::SET;
        case FlagsMask::CARRY:
            return flags[0].carry == FlagStatus::SET;
        case FlagsMask::SIGNED_OVERFLOW:
            return flags[0].overflow == FlagStatus::SET;
        }
        return false;
    };

    const auto checkFlagClear = [](VectorFlags&& flags, FlagsMask mask) -> bool {
        switch(mask)
        {
        case FlagsMask::ZERO:
            return flags[0].zero == FlagStatus::CLEAR;
        case FlagsMask::NEGATIVE:
            return flags[0].negative == FlagStatus::CLEAR;
        case FlagsMask::CARRY:
            return flags[0].carry == FlagStatus::CLEAR;
        case FlagsMask::SIGNED_OVERFLOW:
            return flags[0].overflow == FlagStatus::CLEAR;
        }
        return false;
    };

    auto INT_MAX = Value(Literal(0x7FFFFFFFu), TYPE_INT32);
    auto FLOAT_MINUS_ONE = Value(Literal(-1.0f), TYPE_FLOAT);

    TEST_ASSERT(checkFlagClear(OP_ADD(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_ADD(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_ADD(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagClear(OP_ADD(INT_ONE, INT_ONE).second, FlagsMask::SIGNED_OVERFLOW))
    TEST_ASSERT(checkFlagSet(OP_ADD(INT_ONE, INT_MINUS_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_ADD(INT_MINUS_ONE, INT_MINUS_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagSet(OP_ADD(INT_MINUS_ONE, INT_MINUS_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_ADD(INT_MAX, INT_MAX).second, FlagsMask::SIGNED_OVERFLOW))

    TEST_ASSERT(checkFlagClear(OP_AND(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_AND(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_AND(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_AND(INT_ZERO, INT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_AND(INT_MINUS_ONE, INT_MINUS_ONE).second, FlagsMask::NEGATIVE))

    TEST_ASSERT(checkFlagClear(OP_ASR(INT_ONE, INT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_ASR(INT_ONE, INT_ZERO).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_ASR(INT_ONE, INT_ZERO).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_ASR(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_ASR(INT_MINUS_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagSet(OP_ASR(INT_ONE, INT_ONE).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_CLZ(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_CLZ(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_CLZ(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagClear(OP_CLZ(INT_ONE, INT_ONE).second, FlagsMask::SIGNED_OVERFLOW))
    TEST_ASSERT(checkFlagSet(OP_CLZ(INT_MINUS_ONE, INT_MINUS_ONE).second, FlagsMask::ZERO))

    TEST_ASSERT(checkFlagClear(OP_FADD(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_FADD(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_FADD(FLOAT_ZERO, FLOAT_ZERO).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FADD(FLOAT_ZERO, FLOAT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_FADD(FLOAT_MINUS_ONE, FLOAT_MINUS_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagSet(OP_FADD(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_FMAX(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_FMAX(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_FMAX(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FMAX(FLOAT_ZERO, FLOAT_MINUS_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_FMAX(FLOAT_MINUS_ONE, FLOAT_MINUS_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagSet(OP_FMAX(FLOAT_ONE, FLOAT_ZERO).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FMAX(FLOAT_NAN, FLOAT_INF).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagClear(OP_FMAX(FLOAT_NAN, FLOAT_NAN).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_FMAXABS(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_FMAXABS(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_FMAXABS(FLOAT_ZERO, FLOAT_ZERO).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FMAXABS(FLOAT_ZERO, FLOAT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_FMAXABS(FLOAT_ONE, FLOAT_ZERO).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FMAXABS(FLOAT_NAN, FLOAT_INF).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagClear(OP_FMAXABS(FLOAT_NAN, FLOAT_NAN).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_FMIN(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_FMIN(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_FMIN(FLOAT_ZERO, FLOAT_ZERO).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FMIN(FLOAT_ONE, FLOAT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_FMIN(FLOAT_MINUS_ONE, FLOAT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagSet(OP_FMIN(FLOAT_ONE, FLOAT_MINUS_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FMIN(FLOAT_NAN, FLOAT_INF).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagClear(OP_FMIN(FLOAT_NAN, FLOAT_NAN).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_FMINABS(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_FMINABS(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_FMINABS(FLOAT_ZERO, FLOAT_ZERO).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FMINABS(FLOAT_ZERO, FLOAT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_FMINABS(FLOAT_ONE, FLOAT_ZERO).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FMINABS(FLOAT_NAN, FLOAT_INF).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagClear(OP_FMINABS(FLOAT_NAN, FLOAT_NAN).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_FMUL(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_FMUL(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::NEGATIVE))
    // TODO

    TEST_ASSERT(checkFlagClear(OP_FSUB(FLOAT_ONE, FLOAT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_FSUB(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_FSUB(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FSUB(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_FSUB(FLOAT_MINUS_ONE, FLOAT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagSet(OP_FSUB(FLOAT_ONE, FLOAT_MINUS_ONE).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_FTOI(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_FTOI(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_FTOI(FLOAT_ONE, FLOAT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_FTOI(FLOAT_ZERO, FLOAT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_FTOI(FLOAT_MINUS_ONE, FLOAT_MINUS_ONE).second, FlagsMask::NEGATIVE))

    TEST_ASSERT(checkFlagClear(OP_ITOF(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_ITOF(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_ITOF(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_ITOF(INT_ZERO, INT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_ITOF(INT_MINUS_ONE, INT_MINUS_ONE).second, FlagsMask::NEGATIVE))

    TEST_ASSERT(checkFlagClear(OP_MAX(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_MAX(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_MAX(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_MAX(INT_MINUS_ONE, INT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_MAX(INT_MINUS_ONE, INT_MINUS_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagSet(OP_MAX(INT_ONE, INT_ZERO).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_MIN(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_MIN(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_MIN(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_MIN(INT_ONE, INT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_MIN(INT_ONE, INT_MINUS_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagSet(OP_MIN(INT_ONE, INT_ZERO).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_MUL24(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_MUL24(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_MUL24(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    // TODO

    TEST_ASSERT(checkFlagClear(OP_NOT(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_NOT(INT_MINUS_ONE, INT_MINUS_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_NOT(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_NOT(INT_MINUS_ONE, INT_MINUS_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_NOT(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))

    TEST_ASSERT(checkFlagClear(OP_OR(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_OR(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_OR(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_OR(INT_ZERO, INT_ZERO).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_OR(INT_MINUS_ONE, INT_ONE).second, FlagsMask::NEGATIVE))

    TEST_ASSERT(checkFlagClear(OP_ROR(INT_MINUS_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_ROR(INT_ZERO, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_ROR(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_ROR(INT_ZERO, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_ROR(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))

    TEST_ASSERT(checkFlagClear(OP_SHL(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_SHL(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_SHL(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_SHL(INT_ZERO, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_SHL(INT_MINUS_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagSet(OP_SHL(INT_MINUS_ONE, INT_ONE).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_SHR(INT_MINUS_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_SHR(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_SHR(INT_ONE, INT_ZERO).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_SHR(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    // XXX negative?
    TEST_ASSERT(checkFlagSet(OP_SHR(INT_ONE, INT_ONE).second, FlagsMask::CARRY))

    TEST_ASSERT(checkFlagClear(OP_SUB(INT_ONE, INT_MINUS_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_SUB(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_SUB(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagClear(OP_SUB(INT_ONE, INT_ONE).second, FlagsMask::SIGNED_OVERFLOW))
    TEST_ASSERT(checkFlagSet(OP_SUB(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_SUB(INT_MINUS_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagSet(OP_SUB(INT_MINUS_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_SUB(INT_MAX, INT_MINUS_ONE).second, FlagsMask::SIGNED_OVERFLOW))

    TEST_ASSERT(checkFlagClear(OP_XOR(INT_ONE, INT_MINUS_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagClear(OP_XOR(INT_ONE, INT_ONE).second, FlagsMask::NEGATIVE))
    TEST_ASSERT(checkFlagClear(OP_XOR(INT_ONE, INT_ONE).second, FlagsMask::CARRY))
    TEST_ASSERT(checkFlagSet(OP_XOR(INT_ONE, INT_ONE).second, FlagsMask::ZERO))
    TEST_ASSERT(checkFlagSet(OP_XOR(INT_ONE, INT_MINUS_ONE).second, FlagsMask::NEGATIVE))

    // TODO v8ops
}

// determines the bounds of the new range by determining the minimum/maximum value of calculating the given operation
// with all bounds combinations
template <typename T, typename Op = std::function<T(T, T)>>
analysis::ValueRange calculateRange(const analysis::ValueRange& in0, const analysis::ValueRange& in1, Op&& op)
{
    // auto res = {op(in0.minValue, in1.minValue), op(in0.minValue, in1.maxValue), op(in0.maxValue, in1.minValue),
    //     op(in0.maxValue, in1.maxValue), op(in0.minValue, T{0}), op(in0.maxValue, T{0}), op(T{0}, in1.minValue),
    //     op(T{0}, in1.maxValue)};
    // This tests almost "all" combinations (not quite, only full integer steps, but still enough)
    std::vector<T> res;
    res.reserve(static_cast<std::size_t>((in0.maxValue - in0.minValue + 1) * (in1.maxValue - in1.minValue + 1)));
    for(T a = static_cast<T>(in0.minValue); a <= static_cast<T>(in0.maxValue); ++a)
    {
        for(T b = static_cast<T>(in1.minValue); b <= static_cast<T>(in1.maxValue); ++b)
            res.emplace_back(op(a, b));
    }
    return analysis::ValueRange{static_cast<double>(*std::min_element(res.begin(), res.end())),
        static_cast<double>(*std::max_element(res.begin(), res.end()))};
}

void TestInstructions::testOpCodeRanges()
{
    using namespace analysis;

    ValueRange emptyRange{TYPE_UNKNOWN};

    std::vector<ValueRange> ranges = {ValueRange{-1024.0, -256.0}, ValueRange{-42.0, 28.0}, ValueRange{17.0, 42.0},
        ValueRange{0.0, 0.0}, ValueRange{20000000.0, 20000010.0} /* < for mul24 */};

    TEST_ASSERT_EQUALS(RANGE_FLOAT, OP_FADD(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_FLOAT, OP_FSUB(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_FLOAT, OP_FMIN(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_FLOAT, OP_FMAX(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_FLOAT, OP_FMINABS(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_FLOAT, OP_FMAXABS(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_FLOAT, OP_FMUL(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_INT, OP_FTOI(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_FLOAT, OP_ITOF(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_INT, OP_ADD(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_INT, OP_SUB(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_UINT, OP_SHR(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_UINT, OP_SHL(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_INT, OP_MIN(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_INT, OP_MAX(ValueRange{}, ValueRange{}))
    TEST_ASSERT_EQUALS(RANGE_UINT, OP_AND(ValueRange{}, ValueRange{}))

    for(const auto& innerFloat0 : ranges)
    {
        ValueRange floatRange0{innerFloat0};
        for(const auto& innerFloat1 : ranges)
        {
            ValueRange floatRange1{innerFloat1};
            TEST_ASSERT_EQUALS(
                calculateRange<double>(innerFloat0, innerFloat1, std::plus<>{}), OP_FADD(floatRange0, floatRange1))
            TEST_ASSERT_EQUALS(
                calculateRange<double>(innerFloat0, innerFloat1, std::minus<>{}), OP_FSUB(floatRange0, floatRange1))
            TEST_ASSERT_EQUALS(
                calculateRange<double>(innerFloat0, innerFloat1, [](auto a, auto b) { return std::min(a, b); }),
                OP_FMIN(floatRange0, floatRange1))
            TEST_ASSERT_EQUALS(
                calculateRange<double>(innerFloat0, innerFloat1, [](auto a, auto b) { return std::max(a, b); }),
                OP_FMAX(floatRange0, floatRange1))
            TEST_ASSERT_EQUALS(calculateRange<double>(innerFloat0, innerFloat1,
                                   [](auto a, auto b) { return std::min(std::abs(a), std::abs(b)); }),
                OP_FMINABS(floatRange0, floatRange1))
            TEST_ASSERT_EQUALS(calculateRange<double>(innerFloat0, innerFloat1,
                                   [](auto a, auto b) { return std::max(std::abs(a), std::abs(b)); }),
                OP_FMAXABS(floatRange0, floatRange1))
            TEST_ASSERT_EQUALS(calculateRange<double>(innerFloat0, innerFloat1, std::multiplies<>{}),
                OP_FMUL(floatRange0, floatRange1))
        }
    }

    {
        ValueRange range0{-1024.0, 4096.0};
        TEST_ASSERT_EQUALS(range0, OP_FTOI(range0, emptyRange))
        TEST_ASSERT_EQUALS(range0, OP_ITOF(range0, emptyRange))
    }

    for(const auto& innerInt0 : ranges)
    {
        ValueRange intRange0{innerInt0};
        for(const auto& innerInt1 : ranges)
        {
            ValueRange intRange1{innerInt1};
            TEST_ASSERT_EQUALS(
                calculateRange<int64_t>(innerInt0, innerInt1, std::plus<>{}), OP_ADD(intRange0, intRange1))
            TEST_ASSERT_EQUALS(
                calculateRange<int64_t>(innerInt0, innerInt1, std::minus<>{}), OP_SUB(intRange0, intRange1))
            if(innerInt0.minValue >= 0)
            {
                TEST_ASSERT_EQUALS(calculateRange<int64_t>(innerInt0, innerInt1,
                                       [](auto a, auto b) {
                                           return saturate<uint32_t>(static_cast<int64_t>(
                                               static_cast<uint64_t>(a) >> (0x1Fu & static_cast<uint64_t>(b))));
                                       }),
                    OP_SHR(intRange0, intRange1))
            }
            else
            {
                TEST_ASSERT_EQUALS(RANGE_UINT, OP_SHR(intRange0, intRange1))
            }
            if(innerInt0.minValue >= 0)
            {
                TEST_ASSERT_EQUALS(calculateRange<int64_t>(innerInt0, innerInt1,
                                       [](auto a, auto b) {
                                           return saturate<uint32_t>(static_cast<int64_t>(
                                               static_cast<uint64_t>(a) << (0x1Fu & static_cast<uint64_t>(b))));
                                       }),
                    OP_SHL(intRange0, intRange1))
            }
            else
            {
                TEST_ASSERT_EQUALS(RANGE_UINT, OP_SHL(intRange0, intRange1))
            }
            TEST_ASSERT_EQUALS(
                calculateRange<int64_t>(innerInt0, innerInt1, [](auto a, auto b) { return std::min(a, b); }),
                OP_MIN(intRange0, intRange1))
            TEST_ASSERT_EQUALS(
                calculateRange<int64_t>(innerInt0, innerInt1, [](auto a, auto b) { return std::max(a, b); }),
                OP_MAX(intRange0, intRange1))
            if(innerInt0.minValue >= 0 && innerInt1.minValue >= 0)
            {
                // XXX the range is too broad, which is not an actual problem, just maybe inefficient
                TEST_ASSERT(calculateRange<int64_t>(innerInt0, innerInt1, [](auto a, auto b) {
                    return static_cast<int64_t>(static_cast<uint64_t>(a) & static_cast<uint64_t>(b));
                }).fitsIntoRange(OP_AND(intRange0, intRange1)))
            }
            else
            {
                TEST_ASSERT_EQUALS(RANGE_UINT, OP_AND(intRange0, intRange1))
            }
            TEST_ASSERT_EQUALS(ValueRange(0.0, 32.0), OP_CLZ(intRange0, emptyRange))
            if(innerInt0.minValue >= 0 && innerInt1.minValue >= 0 && innerInt0.maxValue < 0xFFFFFF &&
                innerInt1.maxValue < 0xFFFFFF)
            {
                TEST_ASSERT_EQUALS(calculateRange<int64_t>(innerInt0, innerInt1,
                                       [](auto a, auto b) {
                                           return static_cast<int64_t>((0xFFFFFFu & static_cast<uint64_t>(a)) *
                                               (0xFFFFFFu & static_cast<uint64_t>(b)));
                                       }),
                    OP_MUL24(intRange0, intRange1))
            }
            else
            {
                TEST_ASSERT_EQUALS(RANGE_UINT, OP_MUL24(intRange0, intRange1))
            }
        }
    }
}

void TestInstructions::testRegister()
{
    TEST_ASSERT(!isFixed(RegisterFile::NONE))
    TEST_ASSERT(isFixed(RegisterFile::ACCUMULATOR))
    TEST_ASSERT(isFixed(RegisterFile::PHYSICAL_A))
    TEST_ASSERT(isFixed(RegisterFile::PHYSICAL_B))
    TEST_ASSERT(!isFixed(RegisterFile::PHYSICAL_ANY))
    TEST_ASSERT(!isFixed(RegisterFile::ANY))

    TEST_ASSERT((Register(RegisterFile::PHYSICAL_A, 16)).isGeneralPurpose())
    TEST_ASSERT(!REG_UNIFORM.isGeneralPurpose())

    TEST_ASSERT_EQUALS(0, REG_ACC0.getAccumulatorNumber())
    TEST_ASSERT_EQUALS(1, REG_ACC1.getAccumulatorNumber())
    TEST_ASSERT_EQUALS(2, REG_ACC2.getAccumulatorNumber())
    TEST_ASSERT_EQUALS(3, REG_ACC3.getAccumulatorNumber())
    TEST_ASSERT_EQUALS(4, REG_TMU_OUT.getAccumulatorNumber())
    TEST_ASSERT_EQUALS(5, REG_ACC5.getAccumulatorNumber())
    TEST_ASSERT_EQUALS(-1, (Register(RegisterFile::PHYSICAL_A, 16)).getAccumulatorNumber())
    TEST_ASSERT_EQUALS(-1, REG_NOP.getAccumulatorNumber())

    TEST_ASSERT(REG_ACC0 < REG_ACC1)
    TEST_ASSERT((Register(RegisterFile::PHYSICAL_A, 16)) < (Register(RegisterFile::PHYSICAL_B, 1)))
    TEST_ASSERT(REG_ACC1 > REG_ACC0)
    TEST_ASSERT((Register(RegisterFile::PHYSICAL_B, 16)) > (Register(RegisterFile::PHYSICAL_A, 1)))

    TEST_ASSERT(!REG_ACC0.isVertexPipelineMemory())
    TEST_ASSERT(REG_VPM_IO.isVertexPipelineMemory())
    TEST_ASSERT(REG_VPM_DMA_LOAD_BUSY.isVertexPipelineMemory())
    TEST_ASSERT(REG_VPM_DMA_STORE_BUSY.isVertexPipelineMemory())
    TEST_ASSERT(REG_VPM_IN_SETUP.isVertexPipelineMemory())
    TEST_ASSERT(REG_VPM_OUT_SETUP.isVertexPipelineMemory())
    TEST_ASSERT(REG_VPM_DMA_LOAD_WAIT.isVertexPipelineMemory())
    TEST_ASSERT(REG_VPM_DMA_STORE_WAIT.isVertexPipelineMemory())
    TEST_ASSERT(REG_VPM_DMA_LOAD_ADDR.isVertexPipelineMemory())
    TEST_ASSERT(REG_VPM_DMA_STORE_ADDR.isVertexPipelineMemory())
    TEST_ASSERT(!REG_SFU_RECIP.isVertexPipelineMemory())

    TEST_ASSERT(!REG_VPM_DMA_STORE_ADDR.isSpecialFunctionsUnit())
    TEST_ASSERT(REG_SFU_RECIP.isSpecialFunctionsUnit())
    TEST_ASSERT(REG_SFU_RECIP_SQRT.isSpecialFunctionsUnit())
    TEST_ASSERT(REG_SFU_LOG2.isSpecialFunctionsUnit())
    TEST_ASSERT(REG_SFU_EXP2.isSpecialFunctionsUnit())
    TEST_ASSERT(!REG_TMU0_ADDRESS.isSpecialFunctionsUnit())

    TEST_ASSERT(!REG_SFU_EXP2.isTextureMemoryUnit())
    TEST_ASSERT(REG_TMU0_ADDRESS.isTextureMemoryUnit())
    TEST_ASSERT(REG_TMU0_COORD_T_V_Y.isTextureMemoryUnit())
    TEST_ASSERT(REG_TMU0_COORD_R_BORDER_COLOR.isTextureMemoryUnit())
    TEST_ASSERT(REG_TMU0_COORD_B_LOD_BIAS.isTextureMemoryUnit())
    TEST_ASSERT(REG_TMU1_ADDRESS.isTextureMemoryUnit())
    TEST_ASSERT(REG_TMU1_COORD_T_V_Y.isTextureMemoryUnit())
    TEST_ASSERT(REG_TMU1_COORD_R_BORDER_COLOR.isTextureMemoryUnit())
    TEST_ASSERT(REG_TMU1_COORD_B_LOD_BIAS.isTextureMemoryUnit())

    for(uint8_t i = 0; i < 32; ++i)
    {
        TEST_ASSERT(!Register(RegisterFile::PHYSICAL_A, i).hasSideEffectsOnRead())
        TEST_ASSERT(!Register(RegisterFile::PHYSICAL_B, i).hasSideEffectsOnRead())
    }
    TEST_ASSERT(REG_UNIFORM.hasSideEffectsOnRead())
    TEST_ASSERT(REG_VARYING.hasSideEffectsOnRead())
    TEST_ASSERT(!REG_ELEMENT_NUMBER.hasSideEffectsOnRead())
    TEST_ASSERT(!REG_QPU_NUMBER.hasSideEffectsOnRead())
    TEST_ASSERT(!REG_NOP.hasSideEffectsOnRead())
    TEST_ASSERT(!REG_X_COORDS.hasSideEffectsOnRead())
    TEST_ASSERT(!REG_Y_COORDS.hasSideEffectsOnRead())
    TEST_ASSERT(!REG_MS_MASK.hasSideEffectsOnRead())
    TEST_ASSERT(!REG_REV_FLAG.hasSideEffectsOnRead())
    TEST_ASSERT(REG_VPM_IO.hasSideEffectsOnRead())
    TEST_ASSERT(REG_VPM_DMA_LOAD_BUSY.hasSideEffectsOnRead())
    TEST_ASSERT(REG_VPM_DMA_STORE_BUSY.hasSideEffectsOnRead())
    TEST_ASSERT(REG_VPM_DMA_LOAD_WAIT.hasSideEffectsOnRead())
    TEST_ASSERT(REG_VPM_DMA_STORE_WAIT.hasSideEffectsOnRead())
    TEST_ASSERT(REG_MUTEX.hasSideEffectsOnRead())

    for(uint8_t i = 0; i < 32; ++i)
    {
        TEST_ASSERT(!Register(RegisterFile::PHYSICAL_A, i).hasSideEffectsOnWrite())
        TEST_ASSERT(!Register(RegisterFile::PHYSICAL_B, i).hasSideEffectsOnWrite())
    }
    TEST_ASSERT(!REG_ACC0.hasSideEffectsOnWrite())
    TEST_ASSERT(!REG_ACC1.hasSideEffectsOnWrite())
    TEST_ASSERT(!REG_ACC2.hasSideEffectsOnWrite())
    TEST_ASSERT(!REG_ACC3.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_TMU_NOSWAP.hasSideEffectsOnWrite())
    TEST_ASSERT(!REG_ACC5.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_HOST_INTERRUPT.hasSideEffectsOnWrite())
    TEST_ASSERT(!REG_NOP.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_UNIFORM_ADDRESS.hasSideEffectsOnWrite())
    // XXX don't know whether these have side-effects, so assume so for now
    TEST_ASSERT(REG_MS_MASK.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_REV_FLAG.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_VPM_IO.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_VPM_IN_SETUP.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_VPM_OUT_SETUP.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_VPM_DMA_LOAD_ADDR.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_VPM_DMA_STORE_ADDR.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_MUTEX.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_SFU_RECIP.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_SFU_RECIP_SQRT.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_SFU_LOG2.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_SFU_EXP2.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_TMU0_ADDRESS.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_TMU0_COORD_T_V_Y.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_TMU0_COORD_R_BORDER_COLOR.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_TMU0_COORD_B_LOD_BIAS.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_TMU1_ADDRESS.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_TMU1_COORD_T_V_Y.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_TMU1_COORD_R_BORDER_COLOR.hasSideEffectsOnWrite())
    TEST_ASSERT(REG_TMU1_COORD_B_LOD_BIAS.hasSideEffectsOnWrite())

    for(uint8_t i = 0; i < 32; ++i)
    {
        TEST_ASSERT(Register(RegisterFile::PHYSICAL_A, i).isReadable())
        TEST_ASSERT(Register(RegisterFile::PHYSICAL_B, i).isReadable())
    }
    TEST_ASSERT(REG_UNIFORM.isReadable())
    TEST_ASSERT(REG_ACC1.isReadable())
    TEST_ASSERT(REG_ACC2.isReadable())
    TEST_ASSERT(REG_VARYING.isReadable())
    TEST_ASSERT(REG_ACC5.isReadable())
    TEST_ASSERT(REG_ELEMENT_NUMBER.isReadable())
    TEST_ASSERT(REG_QPU_NUMBER.isReadable())
    TEST_ASSERT(REG_NOP.isReadable())
    TEST_ASSERT(!REG_UNIFORM_ADDRESS.isReadable())
    TEST_ASSERT(REG_X_COORDS.isReadable())
    TEST_ASSERT(REG_Y_COORDS.isReadable())
    TEST_ASSERT(REG_MS_MASK.isReadable())
    TEST_ASSERT(REG_REV_FLAG.isReadable())
    TEST_ASSERT(REG_VPM_IO.isReadable())
    TEST_ASSERT(REG_VPM_DMA_LOAD_BUSY.isReadable())
    TEST_ASSERT(REG_VPM_DMA_STORE_BUSY.isReadable())
    TEST_ASSERT(REG_VPM_DMA_LOAD_WAIT.isReadable())
    TEST_ASSERT(REG_VPM_DMA_STORE_WAIT.isReadable())
    TEST_ASSERT(REG_MUTEX.isReadable())
    TEST_ASSERT(!REG_SFU_RECIP.isReadable())
    TEST_ASSERT(!REG_SFU_RECIP_SQRT.isReadable())
    TEST_ASSERT(!REG_SFU_LOG2.isReadable())
    TEST_ASSERT(!REG_SFU_EXP2.isReadable())
    TEST_ASSERT(!REG_TMU0_ADDRESS.isReadable())
    TEST_ASSERT(!REG_TMU0_COORD_T_V_Y.isReadable())
    TEST_ASSERT(!REG_TMU0_COORD_R_BORDER_COLOR.isReadable())
    TEST_ASSERT(!REG_TMU0_COORD_B_LOD_BIAS.isReadable())
    TEST_ASSERT(!REG_TMU1_ADDRESS.isReadable())
    TEST_ASSERT(!REG_TMU1_COORD_T_V_Y.isReadable())
    TEST_ASSERT(!REG_TMU1_COORD_R_BORDER_COLOR.isReadable())
    TEST_ASSERT(!REG_TMU1_COORD_B_LOD_BIAS.isReadable())

    for(uint8_t i = 0; i < 32; ++i)
    {
        TEST_ASSERT(Register(RegisterFile::PHYSICAL_A, i).isWriteable())
        TEST_ASSERT(Register(RegisterFile::PHYSICAL_B, i).isWriteable())
    }
    TEST_ASSERT(REG_ACC0.isWriteable())
    TEST_ASSERT(REG_ACC1.isWriteable())
    TEST_ASSERT(REG_ACC2.isWriteable())
    TEST_ASSERT(REG_ACC3.isWriteable())
    TEST_ASSERT(REG_TMU_NOSWAP.isWriteable())
    TEST_ASSERT(REG_ACC5.isWriteable())
    TEST_ASSERT(REG_HOST_INTERRUPT.isWriteable())
    TEST_ASSERT(REG_NOP.isWriteable())
    TEST_ASSERT(REG_UNIFORM_ADDRESS.isWriteable())
    TEST_ASSERT(REG_X_COORDS.isWriteable())
    TEST_ASSERT(REG_Y_COORDS.isWriteable())
    TEST_ASSERT(REG_MS_MASK.isWriteable())
    TEST_ASSERT(REG_REV_FLAG.isWriteable())
    TEST_ASSERT(REG_VPM_IO.isWriteable())
    TEST_ASSERT(REG_VPM_IN_SETUP.isWriteable())
    TEST_ASSERT(REG_VPM_OUT_SETUP.isWriteable())
    TEST_ASSERT(REG_VPM_DMA_LOAD_ADDR.isWriteable())
    TEST_ASSERT(REG_VPM_DMA_STORE_ADDR.isWriteable())
    TEST_ASSERT(REG_MUTEX.isWriteable())
    TEST_ASSERT(REG_SFU_RECIP.isWriteable())
    TEST_ASSERT(REG_SFU_RECIP_SQRT.isWriteable())
    TEST_ASSERT(REG_SFU_LOG2.isWriteable())
    TEST_ASSERT(REG_SFU_EXP2.isWriteable())
    TEST_ASSERT(REG_TMU0_ADDRESS.isWriteable())
    TEST_ASSERT(REG_TMU0_COORD_T_V_Y.isWriteable())
    TEST_ASSERT(REG_TMU0_COORD_R_BORDER_COLOR.isWriteable())
    TEST_ASSERT(REG_TMU0_COORD_B_LOD_BIAS.isWriteable())
    TEST_ASSERT(REG_TMU1_ADDRESS.isWriteable())
    TEST_ASSERT(REG_TMU1_COORD_T_V_Y.isWriteable())
    TEST_ASSERT(REG_TMU1_COORD_R_BORDER_COLOR.isWriteable())
    TEST_ASSERT(REG_TMU1_COORD_B_LOD_BIAS.isWriteable())

    TEST_ASSERT(REG_SFU_RECIP.triggersReadOfR4())
    TEST_ASSERT(REG_SFU_RECIP_SQRT.triggersReadOfR4())
    TEST_ASSERT(REG_SFU_LOG2.triggersReadOfR4())
    TEST_ASSERT(REG_SFU_EXP2.triggersReadOfR4())
    // TEST_ASSERT(REG_TMU0_ADDRESS.triggersReadOfR4())
    // TEST_ASSERT(REG_TMU1_ADDRESS.triggersReadOfR4())

    TEST_ASSERT(!REG_ACC0.isUnsignedInteger())
    TEST_ASSERT(REG_ELEMENT_NUMBER.isUnsignedInteger())
    TEST_ASSERT(REG_QPU_NUMBER.isUnsignedInteger())
    TEST_ASSERT(REG_X_COORDS.isUnsignedInteger())
    TEST_ASSERT(REG_Y_COORDS.isUnsignedInteger())
    TEST_ASSERT(REG_MS_MASK.isUnsignedInteger())
    TEST_ASSERT(REG_REV_FLAG.isUnsignedInteger())
}

void TestInstructions::testImmediates()
{
    for(char c = -16; c < 16; ++c)
    {
        auto s1 = SmallImmediate::fromInteger(c);
        auto s2 = normalization::toImmediate(Literal(static_cast<int>(c)));
        TEST_ASSERT(!!s1)
        TEST_ASSERT(!!s2)
        TEST_ASSERT_EQUALS(s1, s2)
        TEST_ASSERT_EQUALS(static_cast<int>(c), s1->toLiteral()->signedInt())
        TEST_ASSERT_EQUALS(static_cast<int>(c), s2->toLiteral()->signedInt())
    }

    for(float f = 1.0f / 256.0f; f < 130.0f; f *= 2)
    {
        auto s = normalization::toImmediate(Literal(f));
        TEST_ASSERT(!!s)
        TEST_ASSERT_EQUALS(f, s->toLiteral()->real())
    }

    for(uint8_t i = 1; i < 15; ++i)
    {
        TEST_ASSERT_EQUALS(i, SmallImmediate::fromRotationOffset(i).getRotationOffset().value())
    }
    TEST_THROWS(SmallImmediate::fromRotationOffset(0), CompilationError)
    TEST_THROWS(SmallImmediate::fromRotationOffset(42), CompilationError)
}

void TestInstructions::testSIMDVector()
{
    SIMDVector v{};
    TEST_ASSERT(!v.getAllSame())
    TEST_ASSERT(!(Value(&v, TYPE_INT32)).isZeroInitializer());
    v = SIMDVector{Literal{42}};
    TEST_ASSERT_EQUALS(Literal{42}, v.getAllSame())
    v[7] = UNDEFINED_LITERAL;
    TEST_ASSERT_EQUALS(Literal{42}, v.getAllSame())
    v[8] = Literal{13};
    TEST_ASSERT(!v.getAllSame())

    v = SIMDVector{Literal{0u}};
    TEST_ASSERT_EQUALS(Literal(0u), v.getAllSame())
    TEST_ASSERT((Value(&v, TYPE_INT32)).isZeroInitializer());

    v = ELEMENT_NUMBERS.vector();
    TEST_ASSERT(v.isElementNumber(false, false, false))
    v[7] = UNDEFINED_LITERAL;
    TEST_ASSERT(!v.isElementNumber(false, false, false))
    TEST_ASSERT(v.isElementNumber(false, false, true))
    v[7] = Literal{13};
    TEST_ASSERT(!v.isElementNumber(false, false, true))
    v = ELEMENT_NUMBERS.vector().transform([](Literal lit) -> Literal { return Literal(lit.unsignedInt() + 5); });
    TEST_ASSERT(!v.isElementNumber(false, false, false))
    TEST_ASSERT(v.isElementNumber(true, false, false))
    v[7] = UNDEFINED_LITERAL;
    TEST_ASSERT(!v.isElementNumber(true, false, false))
    TEST_ASSERT(v.isElementNumber(true, false, true))
    v[7] = Literal{7};
    TEST_ASSERT(!v.isElementNumber(true, false, true))
    v = ELEMENT_NUMBERS.vector().transform([](Literal lit) -> Literal { return Literal(lit.unsignedInt() * 5); });
    TEST_ASSERT(!v.isElementNumber(false, false, false))
    TEST_ASSERT(v.isElementNumber(false, true, false))
    v[7] = UNDEFINED_LITERAL;
    TEST_ASSERT(!v.isElementNumber(false, true, false))
    TEST_ASSERT(v.isElementNumber(false, true, true))
    v[7] = Literal{7};
    TEST_ASSERT(!v.isElementNumber(false, true, true))
    // undefined vector is only "element numbers" iff undefined are ignored
    v = SIMDVector{};
    TEST_ASSERT(!v.isElementNumber(false, false, false))
    TEST_ASSERT(v.isElementNumber(false, false, true))
    TEST_ASSERT(!v.isElementNumber(true, false, false))
    TEST_ASSERT(v.isElementNumber(true, false, true))
    TEST_ASSERT(!v.isElementNumber(false, true, false))
    TEST_ASSERT(v.isElementNumber(false, true, true))

    v = SIMDVector{};
    TEST_ASSERT(v.isUndefined())
    TEST_ASSERT_EQUALS(SIMDVector(UNDEFINED_LITERAL), v);
    TEST_ASSERT(v != SIMDVector(Literal(0)));
    TEST_ASSERT(SIMDVector(Literal(0)) != v);

    v[3] = Literal{3};
    TEST_ASSERT(!v.isUndefined())
    v = ELEMENT_NUMBERS.vector().rotate(4);
    TEST_ASSERT_EQUALS(
        SIMDVector({Literal(12), Literal(13), Literal(14), Literal(15), Literal(0), Literal(1), Literal(2), Literal(3),
            Literal(4), Literal(5), Literal(6), Literal(7), Literal(8), Literal(9), Literal(10), Literal(11)}),
        v)

    v = ELEMENT_NUMBERS.vector().rotatePerQuad(3);
    TEST_ASSERT_EQUALS(
        SIMDVector({Literal(1), Literal(2), Literal(3), Literal(0), Literal(5), Literal(6), Literal(7), Literal(4),
            Literal(9), Literal(10), Literal(11), Literal(8), Literal(13), Literal(14), Literal(15), Literal(12)}),
        v)
}

void TestInstructions::testValue()
{
    TEST_ASSERT(Value(Literal(14), TYPE_INT8).hasImmediate(SmallImmediate::fromInteger(14).value()))
    TEST_ASSERT(!Value(Literal(14), TYPE_INT8).hasImmediate(SmallImmediate::fromInteger(13).value()))
    TEST_ASSERT(
        Value(SmallImmediate::fromInteger(14).value(), TYPE_INT8).hasImmediate(SmallImmediate::fromInteger(14).value()))

    TEST_ASSERT(!INT_ONE.isWriteable())
    TEST_ASSERT(!ELEMENT_NUMBERS.isWriteable())

    TEST_ASSERT(!Value(REG_UNIFORM_ADDRESS, TYPE_INT32).isReadable())
    TEST_ASSERT(!Value(REG_SFU_EXP2, TYPE_INT32).isReadable())
    TEST_ASSERT(!Value(REG_SFU_LOG2, TYPE_INT32).isReadable())
    TEST_ASSERT(!Value(REG_SFU_RECIP, TYPE_INT32).isReadable())
    TEST_ASSERT(!Value(REG_SFU_RECIP_SQRT, TYPE_INT32).isReadable())
    TEST_ASSERT(!Value(REG_TMU0_ADDRESS, TYPE_INT32).isReadable())
    TEST_ASSERT(!Value(REG_TMU1_ADDRESS, TYPE_INT32).isReadable())

    TEST_ASSERT(INT_ONE.isUniform())
    TEST_ASSERT(UNDEFINED_VALUE.isUniform())
    SIMDVector vec{Literal{13}};
    TEST_ASSERT(Value(&vec, TYPE_INT32).isUniform())
    vec = SIMDVector{};
    TEST_ASSERT(Value(&vec, TYPE_INT32).isUniform())
    TEST_ASSERT(UNIFORM_REGISTER.isUniform())
    TEST_ASSERT(Value(SmallImmediate(5), TYPE_INT8).isUniform())
    TEST_ASSERT(!ELEMENT_NUMBER_REGISTER.isUniform())
    TEST_ASSERT(!ELEMENT_NUMBERS.isUniform())

    TEST_ASSERT(INT_ONE.isUnsignedInteger())
    TEST_ASSERT(!UNDEFINED_VALUE.isUnsignedInteger())
    vec = SIMDVector{Literal{13}};
    TEST_ASSERT(Value(&vec, TYPE_INT32).isUnsignedInteger())
    vec = SIMDVector{};
    TEST_ASSERT(!Value(&vec, TYPE_INT32).isUnsignedInteger())
    TEST_ASSERT(ELEMENT_NUMBER_REGISTER.isUnsignedInteger())
    TEST_ASSERT(Value(SmallImmediate(5), TYPE_INT8).isUnsignedInteger())
    TEST_ASSERT(!UNIFORM_REGISTER.isUnsignedInteger())

    TEST_ASSERT_EQUALS(INT_ONE, INT_ONE.getConstantValue())
    TEST_ASSERT_EQUALS(UNDEFINED_VALUE, UNDEFINED_VALUE.getConstantValue())
    TEST_ASSERT_EQUALS(ELEMENT_NUMBER_REGISTER, ELEMENT_NUMBER_REGISTER.getConstantValue())
    TEST_ASSERT_EQUALS(ELEMENT_NUMBERS, ELEMENT_NUMBERS.getConstantValue())
    TEST_ASSERT(!UNIFORM_REGISTER.getConstantValue())

    Value v1(UNDEFINED_LITERAL, TYPE_INT32);
    Value v2(Literal(0), TYPE_INT32);
    TEST_ASSERT(v1 == v1);
    TEST_ASSERT(v2 == v2);
    TEST_ASSERT(v1 != v2);
    TEST_ASSERT(v2 != v1);

    for(auto type : {TYPE_INT32, TYPE_FLOAT.toVectorType(5), TYPE_INT8.toVectorType(16), TYPE_VOID_POINTER})
    {
        auto zero = Value::createZeroInitializer(type);
        TEST_ASSERT(zero.isZeroInitializer());
    }
}

void TestInstructions::testTypes()
{
    DataType type{17, 3, false};
    TEST_ASSERT(type.isSimpleType())
    TEST_ASSERT(!type.isFloatingType())
    TEST_ASSERT(type.isIntegralType())
    TEST_ASSERT(!type.isScalarType())
    TEST_ASSERT(!type.isUnknown())
    TEST_ASSERT(type.isVectorType())
    TEST_ASSERT(!type.getArrayType())
    TEST_ASSERT(!type.getPointerType())
    TEST_ASSERT(!type.getStructType())
    TEST_ASSERT(!type.getImageType())

    TEST_ASSERT_EQUALS("<3 x i17>", type.to_string())
    TEST_ASSERT_EQUALS("int", TYPE_INT32.getTypeName(true, false))
    TEST_ASSERT_EQUALS("uint", TYPE_INT32.getTypeName(false, true))

    TEST_ASSERT_EQUALS(type, type.toVectorType(3))
    TEST_ASSERT_EQUALS(TYPE_INT32, DataType(32, 1, false))
    TEST_ASSERT(TYPE_INT32 != DataType(33, 1, false))
    TEST_ASSERT(TYPE_INT32 < DataType(33, 1, false))
    TEST_ASSERT(TYPE_INT16 < TYPE_INT32)
    TEST_ASSERT(TYPE_INT16 < TYPE_INT16.toVectorType(5))
    TEST_ASSERT(type < type.toVectorType(7))

    TEST_ASSERT(type.getElementType().isScalarType())
    TEST_ASSERT_EQUALS(17, type.getElementType().getScalarBitCount())
    TEST_ASSERT_EQUALS(0x1FFFFu, type.getScalarWidthMask())
    TEST_ASSERT_EQUALS(9, type.getLogicalWidth())
    TEST_ASSERT_EQUALS(12, type.getInMemoryWidth())
    TEST_ASSERT_EQUALS(3, type.getVectorWidth(false))
    TEST_ASSERT_EQUALS(4, type.getVectorWidth(true))
    TEST_ASSERT_EQUALS(12, type.getInMemoryAlignment())

    TEST_ASSERT(TYPE_INT32.containsType(TYPE_INT16))
    TEST_ASSERT(TYPE_INT32.containsType(TYPE_INT32))
    TEST_ASSERT(!TYPE_FLOAT.containsType(TYPE_DOUBLE))
    TEST_ASSERT(TYPE_FLOAT.containsType(TYPE_FLOAT))
    TEST_ASSERT(!TYPE_FLOAT.containsType(TYPE_HALF))
    TEST_ASSERT(!TYPE_INT16.toVectorType(16).containsType(TYPE_INT32))
    TEST_ASSERT(!TYPE_INT16.toVectorType(4).containsType(TYPE_INT16.toVectorType(7)))
    TEST_ASSERT(TYPE_INT16.toVectorType(7).containsType(TYPE_INT16.toVectorType(3)))
    TEST_ASSERT(TYPE_INT16.containsType(TYPE_BOOL))
    TEST_ASSERT(TYPE_VOID_POINTER.containsType(TYPE_INT16))
    TEST_ASSERT(TYPE_VOID_POINTER.containsType(TYPE_VOID_POINTER))
    TEST_ASSERT(!TYPE_INT16.containsType(TYPE_VOID_POINTER))

    TEST_ASSERT_EQUALS(TYPE_INT32, TYPE_INT32.getUnionType(TYPE_INT16))
    TEST_ASSERT_EQUALS(TYPE_INT16, TYPE_INT16.getUnionType(TYPE_INT16))
    TEST_ASSERT_EQUALS(TYPE_INT32.toVectorType(14), TYPE_INT16.toVectorType(14).getUnionType(TYPE_INT32))

    // pointer type
    {
        type = DataType(GLOBAL_TYPE_HOLDER.createPointerType(TYPE_INT16.toVectorType(3), AddressSpace::PRIVATE));
        TEST_ASSERT(!type.isSimpleType())
        TEST_ASSERT(!!type.getPointerType())
        TEST_ASSERT(!type.getArrayType())
        TEST_ASSERT(!type.getStructType())
        TEST_ASSERT(!type.getImageType())
        TEST_ASSERT(type.isIntegralType())
        TEST_ASSERT_EQUALS(TYPE_INT16.toVectorType(3), type.getElementType())
        TEST_ASSERT_EQUALS(4, type.getLogicalWidth())
        TEST_ASSERT_EQUALS(4, type.getInMemoryWidth())
        TEST_ASSERT_EQUALS(4, type.getInMemoryAlignment())
        TEST_ASSERT_EQUALS(32, type.getScalarBitCount())
        TEST_ASSERT_EQUALS("(p) <3 x i16>*", type.to_string())
        auto ptr = type.getPointerType();
        TEST_ASSERT_EQUALS(TYPE_INT16.toVectorType(3), ptr->elementType)
        TEST_ASSERT_EQUALS(AddressSpace::PRIVATE, ptr->addressSpace)
        TEST_ASSERT_EQUALS(8, ptr->getAlignment())
        TEST_ASSERT(*ptr == *type.getPointerType())
        TEST_ASSERT(!(*ptr == *TYPE_VOID_POINTER.getPointerType()))
    }

    // struct type
    {
        // 4 + 4 + 1 + 1 (padding) + 2 = 12
        type = DataType(
            GLOBAL_TYPE_HOLDER.createStructType("MyStruct", {TYPE_INT32, TYPE_FLOAT, TYPE_INT8, TYPE_INT16}, false));
        // 4 + 4 + 1 + 2 = 11
        auto packedType = DataType(GLOBAL_TYPE_HOLDER.createStructType(
            "MyPackedStruct", {TYPE_INT32, TYPE_FLOAT, TYPE_INT8, TYPE_INT16}, true));
        TEST_ASSERT(!type.isSimpleType())
        TEST_ASSERT(!type.getPointerType())
        TEST_ASSERT(!type.getArrayType())
        TEST_ASSERT(!!type.getStructType())
        TEST_ASSERT(!type.getImageType())
        TEST_ASSERT_EQUALS(TYPE_INT16, type.getElementType(3))
        TEST_ASSERT_EQUALS(12, type.getLogicalWidth())
        TEST_ASSERT_EQUALS(11, packedType.getLogicalWidth())
        TEST_ASSERT_EQUALS(12, type.getInMemoryWidth())
        TEST_ASSERT_EQUALS(11, packedType.getInMemoryWidth())
        TEST_ASSERT_EQUALS(12, type.getInMemoryAlignment())
        // This can e.g. be tested in compiler explorer
        TEST_ASSERT_EQUALS(1, packedType.getInMemoryAlignment())
        TEST_ASSERT_EQUALS("MyStruct", type.to_string())
        TEST_ASSERT_EQUALS("MyPackedStruct", packedType.to_string())
        auto struct0 = type.getStructType();
        auto struct1 = packedType.getStructType();
        TEST_ASSERT(struct0->elementTypes == struct1->elementTypes)
        TEST_ASSERT(!struct0->isPacked)
        TEST_ASSERT(struct1->isPacked)
        TEST_ASSERT_EQUALS(10, struct0->getStructSize(3))
        TEST_ASSERT_EQUALS(9, struct1->getStructSize(3))
        TEST_ASSERT_EQUALS(12, struct0->getStructSize())
        TEST_ASSERT_EQUALS(11, struct1->getStructSize())
        TEST_ASSERT_EQUALS("{i32, f32, i8, i16}", struct0->getContent())
        TEST_ASSERT_EQUALS("<{i32, f32, i8, i16}>", struct1->getContent())
        TEST_ASSERT(*struct0 == *type.getStructType())
        TEST_ASSERT(!(*struct0 == *struct1))
    }

    // array type
    {
        type = DataType(GLOBAL_TYPE_HOLDER.createArrayType(TYPE_INT8.toVectorType(3), 17));
        TEST_ASSERT(!type.isSimpleType())
        TEST_ASSERT(!type.getPointerType())
        TEST_ASSERT(!!type.getArrayType())
        TEST_ASSERT(!type.getStructType())
        TEST_ASSERT(!type.getImageType())
        TEST_ASSERT_EQUALS(TYPE_INT8.toVectorType(3), type.getElementType())
        TEST_ASSERT_EQUALS(3 * 17, type.getLogicalWidth())
        TEST_ASSERT_EQUALS(4 * 17, type.getInMemoryWidth())
        TEST_ASSERT_EQUALS(4, type.getInMemoryAlignment())
        TEST_ASSERT_EQUALS("<3 x i8>[17]", type.to_string())
        auto array = type.getArrayType();
        TEST_ASSERT_EQUALS(TYPE_INT8.toVectorType(3), array->elementType)
        TEST_ASSERT_EQUALS(17u, array->size)
        TEST_ASSERT(*array == *type.getArrayType())
        TEST_ASSERT(!(*array == *GLOBAL_TYPE_HOLDER.createArrayType(TYPE_INT32, 5)))
    }

    // image type
    {
        type = DataType(GLOBAL_TYPE_HOLDER.createImageType(3, true, false, true));
        TEST_ASSERT(!type.isSimpleType())
        TEST_ASSERT(!type.getPointerType())
        TEST_ASSERT(!type.getArrayType())
        TEST_ASSERT(!type.getStructType())
        TEST_ASSERT(!!type.getImageType())
        TEST_ASSERT_EQUALS(4, type.getLogicalWidth())
        TEST_ASSERT_EQUALS(4, type.getInMemoryWidth())
        // TODO TEST_ASSERT_EQUALS(4, type.getInMemoryAlignment())
        TEST_ASSERT_EQUALS(32, type.getScalarBitCount())
        TEST_ASSERT_EQUALS("image3D_array", type.to_string())
        auto image = type.getImageType();
        TEST_ASSERT_EQUALS(3, image->dimensions)
        TEST_ASSERT(image->isImageArray)
        TEST_ASSERT(!image->isImageBuffer)
        TEST_ASSERT(image->isSampled)
        TEST_ASSERT(*image == *type.getImageType())
        TEST_ASSERT(!(*image == *GLOBAL_TYPE_HOLDER.createImageType(2)))
    }
}

void TestInstructions::testCompoundConstants()
{
    // simple
    {
        CompoundConstant constant{TYPE_INT16, Literal(42)};
        TEST_ASSERT_EQUALS(TYPE_INT16, constant.type)
        TEST_ASSERT(constant.isAllSame())
        TEST_ASSERT(!constant.isUndefined())
        TEST_ASSERT(!constant.isZeroInitializer())
        TEST_ASSERT_EQUALS(Literal(42), constant.getScalar())
        TEST_ASSERT(!constant.getCompound())
        TEST_ASSERT_EQUALS(Value(Literal(42), TYPE_INT16), constant.toValue())
    }

    // undefined
    {
        CompoundConstant constant{TYPE_INT16, 5};
        TEST_ASSERT_EQUALS(TYPE_INT16, constant.type)
        TEST_ASSERT(constant.isAllSame())
        TEST_ASSERT(constant.isUndefined())
        TEST_ASSERT(constant.isZeroInitializer())
        TEST_ASSERT(!!constant.getCompound())
        SIMDVector vec{};
        TEST_ASSERT_EQUALS(Value(&vec, TYPE_INT16), constant.toValue())
    }

    // vector
    {
        CompoundConstant element{TYPE_INT16, Literal(42)};
        CompoundConstant constant{TYPE_INT16.toVectorType(5), {element, element, element, element, element}};
        TEST_ASSERT_EQUALS(TYPE_INT16.toVectorType(5), constant.type)
        TEST_ASSERT(constant.isAllSame())
        TEST_ASSERT(!constant.isUndefined())
        TEST_ASSERT(!constant.isZeroInitializer())
        TEST_ASSERT(!constant.getScalar())
        TEST_ASSERT(!!constant.getCompound())
        SIMDVector vec({Literal(42), Literal(42), Literal(42), Literal(42), Literal(42)});
        TEST_ASSERT_EQUALS(Value(&vec, TYPE_INT16.toVectorType(5)), constant.toValue())
    }
}

void TestInstructions::testALUInstructions()
{
    using namespace vc4c::qpu_asm;

    ALUInstruction ins{SIGNAL_ALU_IMMEDIATE, UNPACK_16A_32, PACK_32_16A, COND_CARRY_CLEAR, COND_CARRY_SET,
        SetFlag::DONT_SET, WriteSwap::SWAP, 17, 19, OP_FMUL, OP_MIN, 1, 50, InputMultiplex::REGB, InputMultiplex::REGA,
        InputMultiplex::ACC3, InputMultiplex::ACC0};

    TEST_ASSERT(ins.isValidInstruction())
    TEST_ASSERT_EQUALS(SIGNAL_ALU_IMMEDIATE, ins.getSig())
    TEST_ASSERT_EQUALS(UNPACK_16A_32, ins.getUnpack())
    TEST_ASSERT_EQUALS(PACK_32_16A, ins.getPack())
    TEST_ASSERT_EQUALS(COND_CARRY_CLEAR, ins.getAddCondition())
    TEST_ASSERT_EQUALS(COND_CARRY_SET, ins.getMulCondition())
    TEST_ASSERT_EQUALS(SetFlag::DONT_SET, ins.getSetFlag())
    TEST_ASSERT_EQUALS(WriteSwap::SWAP, ins.getWriteSwap())
    TEST_ASSERT_EQUALS(1, ins.getInputA())
    TEST_ASSERT_EQUALS(50, ins.getInputB())
    TEST_ASSERT_EQUALS(OP_FMUL.opMul, ins.getMultiplication())
    TEST_ASSERT_EQUALS(OP_MIN.opAdd, ins.getAddition())
    TEST_ASSERT_EQUALS(17, ins.getAddOut())
    TEST_ASSERT_EQUALS(19, ins.getMulOut())
    TEST_ASSERT_EQUALS(InputMultiplex::REGB, ins.getAddMultiplexA())
    TEST_ASSERT_EQUALS(InputMultiplex::REGA, ins.getAddMultiplexB())
    TEST_ASSERT_EQUALS(InputMultiplex::ACC3, ins.getMulMultiplexA())
    TEST_ASSERT_EQUALS(InputMultiplex::ACC0, ins.getMulMultiplexB())

    TEST_ASSERT_EQUALS(Register(RegisterFile::PHYSICAL_B, 17), ins.getAddOutput())
    TEST_ASSERT_EQUALS(Register(RegisterFile::PHYSICAL_A, 19), ins.getMulOutput())
    TEST_ASSERT_EQUALS(REG_NOP, ins.getAddFirstOperand()) // is small immediate
    TEST_ASSERT_EQUALS(Register(RegisterFile::PHYSICAL_A, 1), ins.getAddSecondOperand())
    TEST_ASSERT_EQUALS(REG_ACC3, ins.getMulFirstOperand())
    TEST_ASSERT_EQUALS(REG_ACC0, ins.getMulSecondOperand())
    TEST_ASSERT(ins.isVectorRotation())
    TEST_ASSERT(ins.isFullRangeRotation())

    TEST_ASSERT(!!ins.as<ALUInstruction>())
    TEST_ASSERT(ins.toBinaryCode() == ins.as<ALUInstruction>()->toBinaryCode())
}

void TestInstructions::testLoadInstruction()
{
    using namespace vc4c::qpu_asm;

    // "normal" load
    {
        LoadInstruction ins{
            PACK_32_8888_S, COND_ZERO_CLEAR, COND_ZERO_SET, SetFlag::SET_FLAGS, WriteSwap::DONT_SWAP, 17, 34, 42};

        TEST_ASSERT(ins.isValidInstruction())
        TEST_ASSERT_EQUALS(OpLoad::LOAD_IMM_32, ins.getType())
        TEST_ASSERT_EQUALS(SIGNAL_LOAD_IMMEDIATE, ins.getSig())
        TEST_ASSERT_EQUALS(PACK_32_8888_S, ins.getPack())
        TEST_ASSERT_EQUALS(COND_ZERO_CLEAR, ins.getAddCondition())
        TEST_ASSERT_EQUALS(COND_ZERO_SET, ins.getMulCondition())
        TEST_ASSERT_EQUALS(SetFlag::SET_FLAGS, ins.getSetFlag())
        TEST_ASSERT_EQUALS(WriteSwap::DONT_SWAP, ins.getWriteSwap())
        TEST_ASSERT_EQUALS(17, ins.getAddOut())
        TEST_ASSERT_EQUALS(34, ins.getMulOut())
        TEST_ASSERT_EQUALS(42, ins.getImmediateInt())

        TEST_ASSERT_EQUALS(Register(RegisterFile::PHYSICAL_A, 17), ins.getAddOutput())
        TEST_ASSERT_EQUALS(REG_ACC2, ins.getMulOutput())
    }

    // unsigned load
    {
        LoadInstruction ins{PACK_32_8888_S, COND_ZERO_CLEAR, COND_ZERO_SET, SetFlag::SET_FLAGS, WriteSwap::DONT_SWAP,
            17, 34, uint16_t{0}, uint16_t{0xFF}};

        TEST_ASSERT(ins.isValidInstruction())
        TEST_ASSERT_EQUALS(OpLoad::LOAD_UNSIGNED, ins.getType())
        TEST_ASSERT_EQUALS(SIGNAL_LOAD_IMMEDIATE, ins.getSig())
        TEST_ASSERT_EQUALS(PACK_32_8888_S, ins.getPack())
        TEST_ASSERT_EQUALS(COND_ZERO_CLEAR, ins.getAddCondition())
        TEST_ASSERT_EQUALS(COND_ZERO_SET, ins.getMulCondition())
        TEST_ASSERT_EQUALS(SetFlag::SET_FLAGS, ins.getSetFlag())
        TEST_ASSERT_EQUALS(WriteSwap::DONT_SWAP, ins.getWriteSwap())
        TEST_ASSERT_EQUALS(17, ins.getAddOut())
        TEST_ASSERT_EQUALS(34, ins.getMulOut())
        TEST_ASSERT_EQUALS(0xFFu, ins.getImmediateInt())
        TEST_ASSERT_EQUALS(0, ins.getImmediateShort0())
        TEST_ASSERT_EQUALS(0xFF, ins.getImmediateShort1())

        TEST_ASSERT_EQUALS(Register(RegisterFile::PHYSICAL_A, 17), ins.getAddOutput())
        TEST_ASSERT_EQUALS(REG_ACC2, ins.getMulOutput())
    }

    // signed load
    {
        LoadInstruction ins{PACK_32_8888_S, COND_ZERO_CLEAR, COND_ZERO_SET, SetFlag::SET_FLAGS, WriteSwap::DONT_SWAP,
            17, 34, int16_t{0}, int16_t{-1}};

        TEST_ASSERT(ins.isValidInstruction())
        TEST_ASSERT_EQUALS(OpLoad::LOAD_SIGNED, ins.getType())
        TEST_ASSERT_EQUALS(SIGNAL_LOAD_IMMEDIATE, ins.getSig())
        TEST_ASSERT_EQUALS(PACK_32_8888_S, ins.getPack())
        TEST_ASSERT_EQUALS(COND_ZERO_CLEAR, ins.getAddCondition())
        TEST_ASSERT_EQUALS(COND_ZERO_SET, ins.getMulCondition())
        TEST_ASSERT_EQUALS(SetFlag::SET_FLAGS, ins.getSetFlag())
        TEST_ASSERT_EQUALS(WriteSwap::DONT_SWAP, ins.getWriteSwap())
        TEST_ASSERT_EQUALS(17, ins.getAddOut())
        TEST_ASSERT_EQUALS(34, ins.getMulOut())
        TEST_ASSERT_EQUALS(0xFFFF, ins.getImmediateInt())
        TEST_ASSERT_EQUALS(0, ins.getImmediateShort0())
        TEST_ASSERT_EQUALS(0xFFFF, ins.getImmediateShort1())
        TEST_ASSERT_EQUALS(0, ins.getImmediateSignedShort0())
        TEST_ASSERT_EQUALS(-1, ins.getImmediateSignedShort1())

        TEST_ASSERT_EQUALS(Register(RegisterFile::PHYSICAL_A, 17), ins.getAddOutput())
        TEST_ASSERT_EQUALS(REG_ACC2, ins.getMulOutput())
    }
}

void TestInstructions::testValueRanges()
{
    // undefined/unlimited range
    {
        auto range = analysis::ValueRange::getValueRange(UNDEFINED_VALUE);
        TEST_ASSERT(!range.hasExplicitBoundaries())
        TEST_ASSERT(!range.fitsIntoType(TYPE_INT32))
        TEST_ASSERT(!range.fitsIntoType(TYPE_VOID))
        TEST_ASSERT(!range.fitsIntoRange(analysis::ValueRange(-17.0, 42.0)))
        TEST_ASSERT_EQUALS(analysis::ValueRange(), range)
        TEST_ASSERT(!range.getLowerLimit(TYPE_FLOAT))
        TEST_ASSERT(!range.getLowerLimit(TYPE_VOID))
        TEST_ASSERT(!range.getUpperLimit(TYPE_INT32))
        TEST_ASSERT(!range.getUpperLimit(TYPE_VOID))
        TEST_ASSERT(!range.toAbsoluteRange().hasExplicitBoundaries())
    }

    // constant range
    {
        Value constant(Literal(0.5f), TYPE_FLOAT);
        auto range = analysis::ValueRange::getValueRange(constant);
        TEST_ASSERT(range.hasExplicitBoundaries())
        TEST_ASSERT(range.isUnsigned())
        TEST_ASSERT(range.fitsIntoType(TYPE_HALF))
        TEST_ASSERT(range.fitsIntoRange(analysis::ValueRange(0.0, 1.0)))
        TEST_ASSERT(!range.fitsIntoRange(analysis::ValueRange(0.0, 0.1)))
        TEST_ASSERT_EQUALS(constant, range.getLowerLimit(TYPE_FLOAT))
        TEST_ASSERT_EQUALS(constant, range.getUpperLimit(TYPE_FLOAT))
        TEST_ASSERT_EQUALS(INT_ZERO, range.getLowerLimit(TYPE_INT32))
        TEST_ASSERT_EQUALS(INT_ONE, range.getUpperLimit(TYPE_INT8))
        TEST_ASSERT(!!range.getSingletonValue())
        TEST_ASSERT_ULP(0.5, *range.getSingletonValue(), 1)
        TEST_ASSERT_EQUALS(range, range.toAbsoluteRange())
    }

    // constant vector range
    {
        SIMDVector vec({Literal(-16.4f), Literal(15.0f), Literal(-3.2f)});
        Value constant(&vec, TYPE_FLOAT);
        auto range = analysis::ValueRange::getValueRange(constant);
        TEST_ASSERT(range.hasExplicitBoundaries())
        TEST_ASSERT(!range.isUnsigned())
        TEST_ASSERT(range.fitsIntoType(TYPE_HALF))
        TEST_ASSERT(range.fitsIntoRange(analysis::ValueRange(-23.0, 18.0)))
        TEST_ASSERT(!range.fitsIntoRange(analysis::ValueRange(0.0, 0.1)))
        TEST_ASSERT_EQUALS(Value(Literal(-16.4f), TYPE_FLOAT), range.getLowerLimit(TYPE_FLOAT))
        TEST_ASSERT_EQUALS(Value(Literal(15.0f), TYPE_FLOAT), range.getUpperLimit(TYPE_FLOAT))
        TEST_ASSERT_EQUALS(Value(Literal(-17), TYPE_INT32), range.getLowerLimit(TYPE_INT32))
        TEST_ASSERT_EQUALS(Value(Literal(15), TYPE_INT8), range.getUpperLimit(TYPE_INT8))
        TEST_ASSERT(!range.getSingletonValue())

        vec = SIMDVector{};
        constant = Value(&vec, TYPE_INT32);
        range = analysis::ValueRange::getValueRange(constant);
        TEST_ASSERT(!range.hasExplicitBoundaries())
    }

    // negative range
    {
        analysis::ValueRange range{-420000.0, -17.0};
        TEST_ASSERT(range.hasExplicitBoundaries())
        TEST_ASSERT(!range.isUnsigned())
        TEST_ASSERT(!range.fitsIntoType(TYPE_HALF))
        TEST_ASSERT(!range.fitsIntoType(TYPE_INT16))
        TEST_ASSERT(range.fitsIntoType(TYPE_INT32))
        TEST_ASSERT(!range.fitsIntoType(TYPE_VOID_POINTER))
        TEST_ASSERT(range.fitsIntoRange(analysis::ValueRange(-600000.0, 1.0)))
        TEST_ASSERT(!range.fitsIntoRange(analysis::ValueRange(0.0, 0.1)))
        TEST_ASSERT_EQUALS(Value(Literal(-420000.0f), TYPE_FLOAT), range.getLowerLimit(TYPE_FLOAT))
        TEST_ASSERT_EQUALS(Value(Literal(-17.0f), TYPE_FLOAT), range.getUpperLimit(TYPE_FLOAT))
        TEST_ASSERT_EQUALS(Value(Literal(-420000), TYPE_INT32), range.getLowerLimit(TYPE_INT32))
        TEST_ASSERT_EQUALS(Value(Literal(-17), TYPE_INT32), range.getUpperLimit(TYPE_INT8))
        TEST_ASSERT_EQUALS(NO_VALUE, range.getLowerLimit(TYPE_VOID_POINTER))
        TEST_ASSERT(!range.getSingletonValue())
        TEST_ASSERT_EQUALS(analysis::ValueRange(17.0, 420000.0), range.toAbsoluteRange())
    }

    // signed range
    {
        analysis::ValueRange range{-42.0, 17.0};
        TEST_ASSERT(range.hasExplicitBoundaries())
        TEST_ASSERT(!range.isUnsigned())
        TEST_ASSERT(range.fitsIntoType(TYPE_HALF))
        TEST_ASSERT(range.fitsIntoType(TYPE_INT16))
        TEST_ASSERT(range.fitsIntoRange(analysis::ValueRange(-60.0, 18.0)))
        TEST_ASSERT(!range.fitsIntoRange(analysis::ValueRange(0.0, 0.1)))
        TEST_ASSERT_EQUALS(Value(Literal(-42.0f), TYPE_FLOAT), range.getLowerLimit(TYPE_FLOAT))
        TEST_ASSERT_EQUALS(Value(Literal(17.0f), TYPE_FLOAT), range.getUpperLimit(TYPE_FLOAT))
        TEST_ASSERT_EQUALS(Value(Literal(-42), TYPE_INT32), range.getLowerLimit(TYPE_INT32))
        TEST_ASSERT_EQUALS(Value(Literal(17), TYPE_INT32), range.getUpperLimit(TYPE_INT8))
        TEST_ASSERT(!range.getSingletonValue())
        TEST_ASSERT_EQUALS(analysis::ValueRange(0.0, 42.0), range.toAbsoluteRange())
    }

    TEST_ASSERT_EQUALS(analysis::ValueRange(0.0, 11.0),
        analysis::ValueRange::getValueRange(intermediate::InstructionDecorations::BUILTIN_LOCAL_ID))
    TEST_ASSERT_EQUALS(analysis::ValueRange(0.0, 12.0),
        analysis::ValueRange::getValueRange(intermediate::InstructionDecorations::BUILTIN_LOCAL_SIZE))
    TEST_ASSERT_EQUALS(analysis::RANGE_UINT,
        analysis::ValueRange::getValueRange(intermediate::InstructionDecorations::BUILTIN_GROUP_ID))
    TEST_ASSERT_EQUALS(analysis::ValueRange(1.0, 3.0),
        analysis::ValueRange::getValueRange(intermediate::InstructionDecorations::BUILTIN_WORK_DIMENSIONS))
}

void TestInstructions::testInstructionEquality()
{
    Configuration config{};
    Module module{config};
    Method method(module);

    intermediate::Operation op1(OP_AND, UNIFORM_REGISTER, UNIFORM_REGISTER, Value(Literal(16), TYPE_INT32));
    op1.setCondition(COND_NEGATIVE_CLEAR);
    op1.setPackMode(PACK_32_16A_S);
    op1.setSetFlags(SetFlag::SET_FLAGS);
    op1.setSignaling(SIGNAL_LOAD_COLOR_END);
    op1.setUnpackMode(UNPACK_16B_32);

    intermediate::InlineMapping mapping;

    std::unique_ptr<intermediate::IntermediateInstruction> op2(op1.copyFor(method, "test", mapping));

    TEST_ASSERT_EQUALS(op1, *op2)
    op1.setCondition(COND_ALWAYS);
    TEST_ASSERT(op1 != *op2)
    op1.setCondition(COND_NEGATIVE_CLEAR);
    TEST_ASSERT_EQUALS(op1, *op2)
    op1.setPackMode(PACK_NOP);
    TEST_ASSERT(op1 != *op2)
    op1.setPackMode(PACK_32_16A_S);
    TEST_ASSERT_EQUALS(op1, *op2)
    op1.setSetFlags(SetFlag::DONT_SET);
    TEST_ASSERT(op1 != *op2)
    op1.setSetFlags(SetFlag::SET_FLAGS);
    TEST_ASSERT_EQUALS(op1, *op2)
    op1.setSignaling(SIGNAL_NONE);
    TEST_ASSERT(op1 != *op2)
    op1.setSignaling(SIGNAL_LOAD_COLOR_END);
    TEST_ASSERT_EQUALS(op1, *op2)
    op1.setUnpackMode(UNPACK_NOP);
    TEST_ASSERT(op1 != *op2)
    op1.setUnpackMode(UNPACK_16B_32);
    TEST_ASSERT_EQUALS(op1, *op2)
    op1.setOutput(NO_VALUE);
    TEST_ASSERT(op1 != *op2)
    op1.setOutput(UNIFORM_REGISTER);
    TEST_ASSERT_EQUALS(op1, *op2)
    op1.op = OP_OR;
    TEST_ASSERT(op1 != *op2)
    op1.op = OP_AND;
    TEST_ASSERT_EQUALS(op1, *op2)
    op1.setArgument(0, INT_ZERO);
    TEST_ASSERT(op1 != *op2)
    op1.setArgument(0, UNIFORM_REGISTER);
    TEST_ASSERT_EQUALS(op1, *op2)

    {
        intermediate::IntrinsicOperation inst("dummy", Value(NOP_REGISTER), Value(INT_ONE), Value(INT_MINUS_ONE));
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
        inst.opCode = "another";
        TEST_ASSERT(inst != *op2)
    }

    {
        intermediate::MethodCall inst(Value(NOP_REGISTER), "dummy", {INT_ONE, INT_MINUS_ONE});
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
        inst.methodName = "another";
        TEST_ASSERT(inst != *op2)
    }

    // {
    //     intermediate::Return inst{Value(INT_MINUS_ONE)};
    //     op2.reset(inst.copyFor(method, ""));
    //     TEST_ASSERT_EQUALS(inst, *op2)
    // }

    {
        intermediate::MoveOperation inst{Value(NOP_REGISTER), Value(INT_MINUS_ONE)};
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
        inst.setSource(Value(INT_ZERO));
        TEST_ASSERT(inst != *op2)
    }

    {
        intermediate::VectorRotation inst(
            Value(NOP_REGISTER), INT_ONE, SmallImmediate::fromRotationOffset(11), intermediate::RotationType::FULL);
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
        inst.setSource(Value(FLOAT_NAN));
        TEST_ASSERT(inst != *op2)
    }

    {
        intermediate::BranchLabel inst(*method.addNewLocal(TYPE_LABEL).local());
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
    }

    {
        intermediate::Branch inst(method.addNewLocal(TYPE_LABEL).local(), COND_ZERO_CLEAR, UNIFORM_REGISTER);
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
        inst.setCondition(COND_ZERO_SET);
        TEST_ASSERT(inst != *op2)
    }

    {
        intermediate::Nop inst(intermediate::DelayType::WAIT_SFU);
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
        inst.type = intermediate::DelayType::WAIT_UNIFORM;
        TEST_ASSERT(inst != *op2)
    }

    {
        intermediate::Comparison inst(
            intermediate::COMP_FALSE, Value(NOP_REGISTER), Value(INT_ONE), Value(INT_MINUS_ONE));
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
        inst.opCode = "another";
        TEST_ASSERT(inst != *op2)
    }

    {
        intermediate::LoadImmediate inst(Value(NOP_REGISTER), Literal(16));
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
        inst.setImmediate(Literal(42u));
        TEST_ASSERT(inst != *op2)
    }

    {
        intermediate::SemaphoreAdjustment inst(Semaphore::BARRIER_WORK_ITEM_9, true);
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
    }

    {
        intermediate::PhiNode inst(Value(NOP_REGISTER),
            {{Value(INT_ONE), method.addNewLocal(TYPE_LABEL).local()},
                {Value(INT_MINUS_ONE), method.addNewLocal(TYPE_LABEL).local()}});
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
    }

    {
        intermediate::MemoryBarrier inst(intermediate::MemoryScope::WORK_GROUP, intermediate::MemorySemantics::ACQUIRE);
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
        inst.scope = intermediate::MemoryScope::DEVICE;
        TEST_ASSERT(inst != *op2)
    }

    // {
    //     intermediate::LifetimeBoundary inst(method.addNewLocal(TYPE_FLOAT), false);
    //     op2.reset(inst.copyFor(method, ""));
    //     TEST_ASSERT_EQUALS(inst, *op2)
    // }

    {
        intermediate::MutexLock inst(intermediate::MutexAccess::LOCK);
        op2.reset(inst.copyFor(method, "", mapping));
        TEST_ASSERT_EQUALS(inst, *op2)
    }
}
