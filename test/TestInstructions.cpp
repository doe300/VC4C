/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestInstructions.h"

#include "Bitfield.h"
#include "Values.h"
#include "asm/OpCodes.h"

using namespace vc4c;

TestInstructions::TestInstructions()
{
    TEST_ADD(TestInstructions::testConditionCodes);
    TEST_ADD(TestInstructions::testUnpackModes);
    TEST_ADD(TestInstructions::testPackModes);
    TEST_ADD(TestInstructions::testConstantSaturations);
    TEST_ADD(TestInstructions::testBitfields);
    TEST_ADD(TestInstructions::testOpCodes);
    TEST_ADD(TestInstructions::testOpCodeProperties);
}

TestInstructions::~TestInstructions()
{
    // out-of-line virtual destructor
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

void TestInstructions::testUnpackModes()
{
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_NOP.unpack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_NOP.unpack(INT_ZERO));

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_16A_32.unpack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_16A_32.unpack(Value(Literal(0xFFFF), TYPE_INT16)));
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_16A_32.unpack(INT_ZERO));

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_16B_32.unpack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_16B_32.unpack(Value(Literal(0xFFFF0000u), TYPE_INT32)));
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_16B_32.unpack(INT_ZERO));

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_8888_32.unpack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, UNPACK_8888_32.unpack(Value(Literal(0xFF), TYPE_INT8)));
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_8888_32.unpack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT32), UNPACK_8A_32.unpack(Value(Literal(0xFF), TYPE_INT8)));
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_8A_32.unpack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT32), UNPACK_8B_32.unpack(Value(Literal(0xFF00), TYPE_INT8)));
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_8B_32.unpack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT32), UNPACK_8C_32.unpack(Value(Literal(0xFF0000), TYPE_INT8)));
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_8C_32.unpack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT32), UNPACK_8D_32.unpack(Value(Literal(0xFF000000u), TYPE_INT8)));
    TEST_ASSERT_EQUALS(INT_ZERO, UNPACK_8D_32.unpack(INT_ZERO));
}

void TestInstructions::testPackModes()
{
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, PACK_NOP.pack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_NOP.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFFFF), TYPE_INT16), PACK_32_16A.pack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_16A.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFFFF0000u), TYPE_INT32), PACK_32_16B.pack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_16B.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, PACK_32_8888.pack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, PACK_32_8888.pack(Value(Literal(0xFF), TYPE_INT8)));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8888.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFF), TYPE_INT8), PACK_32_8A.pack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8A.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFF00), TYPE_INT8), PACK_32_8B.pack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8B.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFF0000), TYPE_INT8), PACK_32_8C.pack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8C.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFF000000u), TYPE_INT8), PACK_32_8D.pack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8D.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFFFF), TYPE_INT16), PACK_32_16A_S.pack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(Value(Literal(0x8000), TYPE_INT16), PACK_32_16A_S.pack(Value(Literal(0x87654321u), TYPE_INT32)));
    TEST_ASSERT_EQUALS(Value(Literal(0x7FFF), TYPE_INT16), PACK_32_16A_S.pack(Value(Literal(0x12345678u), TYPE_INT32)));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_16A_S.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFFFF0000u), TYPE_INT16), PACK_32_16B_S.pack(INT_MINUS_ONE));
    TEST_ASSERT_EQUALS(
        Value(Literal(0x80000000u), TYPE_INT16), PACK_32_16B_S.pack(Value(Literal(0x87654321u), TYPE_INT32)));
    TEST_ASSERT_EQUALS(
        Value(Literal(0x7FFF0000u), TYPE_INT16), PACK_32_16B_S.pack(Value(Literal(0x12345678u), TYPE_INT32)));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_16B_S.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, PACK_32_8888_S.pack(Value(Literal(0xFF), TYPE_INT8)));
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, PACK_32_8888_S.pack(Value(Literal(0x12345678u), TYPE_INT32)));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8888_S.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFFu), TYPE_INT8), PACK_32_8A_S.pack(Value(Literal(0x12345678u), TYPE_INT32)));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8A_S.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(Value(Literal(0xFF00u), TYPE_INT8), PACK_32_8B_S.pack(Value(Literal(0x12345678u), TYPE_INT32)));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8B_S.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(
        Value(Literal(0xFF0000u), TYPE_INT8), PACK_32_8C_S.pack(Value(Literal(0x12345678u), TYPE_INT32)));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8C_S.pack(INT_ZERO));

    TEST_ASSERT_EQUALS(
        Value(Literal(0xFF000000u), TYPE_INT8), PACK_32_8D_S.pack(Value(Literal(0x12345678u), TYPE_INT32)));
    TEST_ASSERT_EQUALS(INT_ZERO, PACK_32_8D_S.pack(INT_ZERO));
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

void TestInstructions::testOpCodes()
{
    const Value FLOAT_MINUS_ONE(Literal(-1.0f), TYPE_FLOAT);
    TEST_ASSERT_EQUALS(INT_ONE, OP_ADD(INT_ONE, INT_ZERO).value());
    TEST_ASSERT_EQUALS(INT_ZERO, OP_ADD(INT_ONE, INT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(INT_ZERO, OP_AND(INT_ONE, INT_ZERO).value());
    TEST_ASSERT_EQUALS(INT_ONE, OP_AND(INT_ONE, INT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(INT_ZERO, OP_ASR(INT_ONE, INT_ONE).value());
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_ASR(INT_MINUS_ONE, INT_ONE).value());

    TEST_ASSERT_EQUALS(INT_ZERO, OP_CLZ(INT_MINUS_ONE, NO_VALUE).value());
    TEST_ASSERT_EQUALS(Value(Literal(31u), TYPE_INT8), OP_CLZ(INT_ONE, NO_VALUE).value());
    // TODO is this test correct?
    TEST_ASSERT_EQUALS(Value(Literal(32u), TYPE_INT8), OP_CLZ(INT_ZERO, NO_VALUE).value());

    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FADD(FLOAT_ONE, FLOAT_ZERO).value());
    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_FADD(FLOAT_ONE, FLOAT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FMAX(FLOAT_ONE, FLOAT_ZERO).value());
    TEST_ASSERT_EQUALS(FLOAT_INF, OP_FMAX(FLOAT_INF, FLOAT_ONE).value());

    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FMAXABS(FLOAT_ZERO, FLOAT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_FMIN(FLOAT_ONE, FLOAT_ZERO).value());
    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FMIN(FLOAT_INF, FLOAT_ONE).value());

    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_FMINABS(FLOAT_ZERO, FLOAT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FMUL(FLOAT_ONE, FLOAT_ONE).value());
    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_FMUL(FLOAT_ONE, FLOAT_ZERO).value());
    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FMUL(FLOAT_MINUS_ONE, FLOAT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_FSUB(FLOAT_ONE, FLOAT_ONE).value());
    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_FSUB(FLOAT_ZERO, FLOAT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(INT_ZERO, OP_FTOI(FLOAT_ZERO, NO_VALUE).value());
    TEST_ASSERT_EQUALS(INT_ONE, OP_FTOI(FLOAT_ONE, NO_VALUE).value());
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_FTOI(FLOAT_MINUS_ONE, NO_VALUE).value());

    TEST_ASSERT_EQUALS(FLOAT_ZERO, OP_ITOF(INT_ZERO, NO_VALUE).value());
    TEST_ASSERT_EQUALS(FLOAT_ONE, OP_ITOF(INT_ONE, NO_VALUE).value());
    TEST_ASSERT_EQUALS(FLOAT_MINUS_ONE, OP_ITOF(INT_MINUS_ONE, NO_VALUE).value());

    TEST_ASSERT_EQUALS(INT_ONE, OP_MAX(INT_ONE, INT_ZERO).value());
    TEST_ASSERT_EQUALS(INT_ZERO, OP_MAX(INT_ZERO, INT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(INT_ZERO, OP_MIN(INT_ONE, INT_ZERO).value());
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_MIN(INT_ZERO, INT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(INT_ONE, OP_MUL24(INT_ONE, INT_ONE).value());
    TEST_ASSERT_EQUALS(INT_ZERO, OP_MUL24(INT_ONE, INT_ZERO).value());

    TEST_ASSERT_EQUALS(NO_VALUE, OP_NOP(NO_VALUE, NO_VALUE));

    TEST_ASSERT_EQUALS(INT_ZERO, OP_NOT(INT_MINUS_ONE, NO_VALUE).value());
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_NOT(INT_ZERO, NO_VALUE).value());

    TEST_ASSERT_EQUALS(INT_ONE, OP_OR(INT_ONE, INT_ZERO).value());
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_OR(INT_MINUS_ONE, INT_ONE).value());

    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_ROR(INT_MINUS_ONE, INT_ONE).value());

    TEST_ASSERT_EQUALS(INT_ONE, OP_SHL(INT_ONE, INT_ZERO).value());

    TEST_ASSERT_EQUALS(INT_ONE, OP_SHR(INT_ONE, INT_ZERO).value());
    TEST_ASSERT_EQUALS(INT_ZERO, OP_SHR(INT_ONE, INT_ONE).value());

    TEST_ASSERT_EQUALS(INT_ZERO, OP_SUB(INT_ONE, INT_ONE).value());
    TEST_ASSERT_EQUALS(INT_ONE, OP_SUB(INT_ZERO, INT_MINUS_ONE).value());
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_SUB(INT_ZERO, INT_ONE).value());

    TEST_ASSERT_EQUALS(INT_ONE, OP_V8ADDS(INT_ONE, INT_ZERO).value());

    TEST_ASSERT_EQUALS(INT_ZERO, OP_V8MAX(INT_ZERO, INT_ZERO).value());
    TEST_ASSERT_EQUALS(INT_ONE, OP_V8MAX(INT_ZERO, INT_ONE).value());
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_V8MAX(INT_ONE, INT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(INT_ZERO, OP_V8MIN(INT_ZERO, INT_ZERO).value());
    TEST_ASSERT_EQUALS(INT_ZERO, OP_V8MIN(INT_ZERO, INT_ONE).value());
    TEST_ASSERT_EQUALS(INT_ONE, OP_V8MIN(INT_ONE, INT_MINUS_ONE).value());

    TEST_ASSERT_EQUALS(INT_ZERO, OP_V8SUBS(INT_ONE, INT_ONE).value());
    TEST_ASSERT_EQUALS(INT_ONE, OP_V8MAX(INT_ONE, INT_ZERO).value());

    TEST_ASSERT_EQUALS(INT_ZERO, OP_V8MULD(INT_ZERO, INT_ZERO).value());
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, OP_V8MULD(INT_MINUS_ONE, INT_MINUS_ONE).value());
}

void TestInstructions::testOpCodeProperties()
{
    static const std::vector<OpCode> opCodes = {OP_ADD, OP_AND, OP_ASR, OP_CLZ, OP_FADD, OP_FMAX, OP_FMAXABS, OP_FMIN,
        OP_FMINABS, OP_FMUL, OP_FSUB, OP_FTOI, OP_ITOF, OP_MAX, OP_MIN, OP_MUL24, OP_NOP, OP_NOT, OP_OR, OP_ROR, OP_SHL,
        OP_SHR, OP_SUB, OP_V8ADDS, OP_V8MAX, OP_V8MIN, OP_V8MULD, OP_V8SUBS, OP_XOR};

    for(const auto& op : opCodes)
    {
        Value arg = op.acceptsFloat ? FLOAT_ONE : INT_ONE;
        Value arg2 = op.acceptsFloat ? Value(Literal(2.0f), TYPE_FLOAT) : Value(Literal(2u), TYPE_INT32);
        if(op.isIdempotent())
        {
            if(op.numOperands == 1)
            {
                if(op(op(op(arg2, NO_VALUE), NO_VALUE), NO_VALUE) != op(arg2, NO_VALUE))
                {
                    TEST_ASSERT_EQUALS("", op.name);
                }
            }
            else
            {
                if(arg2 != op(arg2, arg2))
                {
                    TEST_ASSERT_EQUALS("", op.name);
                }
            }
        }
        if(op.isAssociative())
        {
            if(op(arg, op(arg, arg2)) != op(op(arg, arg), arg2))
            {
                TEST_ASSERT_EQUALS("", op.name);
            }
        }
        if(op.isCommutative())
        {
            if(op(arg, arg2) != op(arg2, arg))
            {
                TEST_ASSERT_EQUALS("", op.name);
            }
        }

        auto special = OpCode::getLeftIdentity(op);
        if(special && (op(special, arg2) != arg2))
        {
            TEST_ASSERT_EQUALS("", op.name);
        }
        special = OpCode::getRightIdentity(op);
        if(special && (op(arg2, special) != arg2))
        {
            TEST_ASSERT_EQUALS("", op.name);
        }
        special = OpCode::getLeftAbsorbingElement(op);
        if(special && (op(special, arg2) != special))
        {
            TEST_ASSERT_EQUALS("", op.name);
        }
        special = OpCode::getRightAbsorbingElement(op);
        if(special && (op(arg2, special) != special))
        {
            TEST_ASSERT_EQUALS("", op.name);
        }
    }
}