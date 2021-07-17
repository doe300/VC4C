/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestPatternMatching.h"

#include "Expression.h"
#include "Method.h"
#include "Module.h"
#include "analysis/PatternMatching.h"
#include "intermediate/operators.h"

using namespace vc4c;
using namespace vc4c::pattern;
using namespace vc4c::operators;

static const Configuration config{};
static Module module{config};

// TODO add test for matching same value success/failure

TestPatternMatching::TestPatternMatching()
{
    TEST_ADD(TestPatternMatching::testInstructionMatch);
    TEST_ADD(TestPatternMatching::testExpressionMatch);
    TEST_ADD(TestPatternMatching::testSingleSearch);
    TEST_ADD(TestPatternMatching::testConsecutiveSearch);
    TEST_ADD(TestPatternMatching::testGappedSearch);
}

TestPatternMatching::~TestPatternMatching() = default;

void TestPatternMatching::testInstructionMatch()
{
    Method m(module);
    auto& block = m.createAndInsertNewBlock(m.begin(), "dummyLabel");
    auto it = block.walkEnd();

    // successful test - match binary instruction with wildcards
    {
        auto in = m.addNewLocal(TYPE_INT32);
        auto out = assign(it, TYPE_INT32) = (15_val + in, COND_NEGATIVE_CLEAR);
        auto inst = (it.copy().previousInBlock()).get();

        Value realOut = UNDEFINED_VALUE;
        Literal realConstant = UNDEFINED_LITERAL;
        Value realIn = UNDEFINED_VALUE;
        OpCode realOp = OP_NOP;
        ConditionCode realCond = COND_NEVER;
        auto part = capture(realOut) = (capture(realOp), capture(realConstant), capture(realIn), capture(realCond));

        TEST_ASSERT(matches(inst, part))
        TEST_ASSERT_EQUALS(out, realOut)
        TEST_ASSERT(OP_ADD == realOp)
        TEST_ASSERT_EQUALS(15_lit, realConstant)
        TEST_ASSERT_EQUALS(in, realIn)
        TEST_ASSERT_EQUALS(COND_NEGATIVE_CLEAR, realCond)
    }

    // successful test - match unary instruction specifically
    {
        auto out = assign(it, TYPE_INT32) = 42_val;
        auto inst = (it.copy().previousInBlock()).get();

        auto part = match(out) = (match(FAKEOP_MOV), match(42_val));

        TEST_ASSERT(matches(inst, part))
    }

    // successful test - match load
    {
        auto out = m.addNewLocal(TYPE_INT32);
        it.emplace(std::make_unique<intermediate::LoadImmediate>(out, 42_lit));

        Value realOut = UNDEFINED_VALUE;
        Literal realConstant = UNDEFINED_LITERAL;
        ConditionCode realCond = COND_NEVER;
        auto part = capture(realOut) = (match(FAKEOP_LDI), capture(realConstant), capture(realCond));

        TEST_ASSERT(matches(it.get(), part))
        TEST_ASSERT_EQUALS(out, realOut)
        TEST_ASSERT_EQUALS(42_lit, realConstant)
        TEST_ASSERT_EQUALS(COND_ALWAYS, realCond)
    }

    // successful test - match same input
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER ^ UNIFORM_REGISTER;
        auto inst = (it.copy().previousInBlock()).get();
        (void) out;

        Value arg = UNDEFINED_VALUE;
        auto part = anyValue() = (match(OP_XOR), capture(arg), capture(arg));

        TEST_ASSERT(matches(inst, part))
        TEST_ASSERT_EQUALS(UNIFORM_REGISTER, arg)
    }

    // successful test - matching flags
    {
        auto out = assign(it, TYPE_INT16) = (17_val ^ UNIFORM_REGISTER, SetFlag::SET_FLAGS);
        auto inst = (it.copy().previousInBlock()).get();
        (void) out;

        Value arg = UNDEFINED_VALUE;
        SetFlag flags = SetFlag::DONT_SET;
        auto part = anyValue() = (match(OP_XOR), capture(arg), anyValue(), capture(flags));

        TEST_ASSERT(matches(inst, part))
        TEST_ASSERT_EQUALS(17_val, arg)
        TEST_ASSERT(flags == SetFlag::SET_FLAGS)
    }

    // successful test - match rotation
    {
        auto out = m.addNewLocal(TYPE_INT32);
        it.emplace(std::make_unique<intermediate::VectorRotation>(
            out, 17_val, SmallImmediate::fromRotationOffset(3), intermediate::RotationType::FULL));

        Value arg = UNDEFINED_VALUE;
        auto part = anyValue() =
            (match(FAKEOP_ROTATE), capture(arg), match(Value(SmallImmediate::fromRotationOffset(3), TYPE_INT8)));

        TEST_ASSERT(matches(it.get(), part))
        TEST_ASSERT_EQUALS(17_val, arg)

        it.nextInBlock();
    }

    // successful test - capture rotation
    {
        auto out = m.addNewLocal(TYPE_INT32);
        it.emplace(std::make_unique<intermediate::VectorRotation>(
            out, 42_val, VECTOR_ROTATE_R5, intermediate::RotationType::PER_QUAD));

        Value arg = UNDEFINED_VALUE;
        Value offset = UNDEFINED_VALUE;
        auto part = anyValue() = (match(FAKEOP_ROTATE), capture(arg), capture(offset));

        TEST_ASSERT(matches(it.get(), part))
        TEST_ASSERT_EQUALS(42_val, arg)
        TEST_ASSERT_EQUALS(ROTATION_REGISTER, offset);

        it.nextInBlock();
    }

    // successful match - commutative match
    {
        auto tmp = m.addNewLocal(TYPE_INT32);
        auto out = assign(it, TYPE_INT32) = tmp + 42_val;
        auto inst = (it.copy().previousInBlock()).get();
        (void) out;

        Value arg = UNDEFINED_VALUE;
        auto part = anyValue() = (match(OP_ADD), capture(arg), match(tmp), allowCommutation());

        TEST_ASSERT(matches(inst, part))
        TEST_ASSERT_EQUALS(42_val, arg)
    }

    // failing test - value mismatch
    {
        auto in = m.addNewLocal(TYPE_INT32);
        auto out = assign(it, TYPE_INT32) = (15_val + in, COND_NEGATIVE_CLEAR);
        auto inst = (it.copy().previousInBlock()).get();
        (void) out;

        Value realIn = UNDEFINED_VALUE;
        ConditionCode realCond = COND_NEVER;
        auto part = anyValue() = (match(OP_ADD), match(17_val), capture(realIn), capture(realCond));

        TEST_ASSERT(!matches(inst, part))
        // check captures not updated
        TEST_ASSERT(realIn.isUndefined())
        TEST_ASSERT_EQUALS(COND_NEVER, realCond)
    }

    // failing test - opcode mismatch
    {
        auto in = m.addNewLocal(TYPE_INT32);
        auto out = assign(it, TYPE_INT32) = (15_val + in, COND_NEGATIVE_CLEAR);
        auto inst = (it.copy().previousInBlock()).get();
        (void) out;

        Value realIn = UNDEFINED_VALUE;
        ConditionCode realCond = COND_NEVER;
        auto part = anyValue() = (match(OP_SUB), anyValue(), capture(realIn), capture(realCond));

        TEST_ASSERT(!matches(inst, part))
        // check captures not updated
        TEST_ASSERT(realIn.isUndefined())
        TEST_ASSERT_EQUALS(COND_NEVER, realCond)
    }

    // failing test - unsupported operation
    {
        it.emplace(std::make_unique<intermediate::SemaphoreAdjustment>(Semaphore::BARRIER_WORK_ITEM_10, false));

        Value realOut = UNDEFINED_VALUE;
        OpCode realCode = OP_NOP;
        auto part = capture(realOut) = (capture(realCode), anyValue());

        TEST_ASSERT(!matches(it.get(), part))
        // check captures not updated
        TEST_ASSERT(realOut.isUndefined())
        TEST_ASSERT(OP_NOP == realCode)
    }

    // failing test - value type (local, literal) mismatch
    {
        auto in = m.addNewLocal(TYPE_INT32);
        auto out = assign(it, TYPE_INT32) = (15_val + in, COND_NEGATIVE_CLEAR);
        auto inst = (it.copy().previousInBlock()).get();
        (void) out;

        Value realOut = UNDEFINED_VALUE;
        const Local* loc = nullptr;
        auto part = capture(realOut) = (match(OP_ADD), capture(loc), anyValue());

        TEST_ASSERT(!matches(inst, part))
        // check captures not updated
        TEST_ASSERT(realOut.isUndefined())
        TEST_ASSERT(loc == nullptr)
    }

    // failing test - condition mismatch
    {
        auto in = m.addNewLocal(TYPE_INT32);
        auto out = assign(it, TYPE_INT32) = (15_val + in, COND_NEGATIVE_CLEAR);
        auto inst = (it.copy().previousInBlock()).get();
        (void) out;

        Value realOut = UNDEFINED_VALUE;
        OpCode realCode = OP_NOP;
        auto part = capture(realOut) = (capture(realCode), anyValue(), match(COND_CARRY_CLEAR));

        TEST_ASSERT(!matches(inst, part))
        // check captures not updated
        TEST_ASSERT(realOut.isUndefined())
        TEST_ASSERT(OP_NOP == realCode)
    }

    // failing test - argument size mismatch
    {
        auto out = assign(it, TYPE_INT32) = 42_val;
        auto inst = (it.copy().previousInBlock()).get();

        Value secondArg = UNDEFINED_VALUE;
        auto part = match(out) = (match(FAKEOP_MOV), match(42_val), capture(secondArg));

        TEST_ASSERT(!matches(inst, part))
        // check captures not updated
        TEST_ASSERT(secondArg.isUndefined())
    }

    // failing test - pack mode
    {
        auto out = assign(it, TYPE_INT32) = (42_val, PACK_32_16A);
        auto inst = (it.copy().previousInBlock()).get();

        Value arg = UNDEFINED_VALUE;
        auto part = match(out) = (anyOperation(), capture(arg));

        TEST_ASSERT(!matches(inst, part))
        // check captures not updated
        TEST_ASSERT(arg.isUndefined())
    }

    // failing test - unpack mode
    {
        auto out = assign(it, TYPE_INT32) = (42_val, UNPACK_16A_32);
        auto inst = (it.copy().previousInBlock()).get();

        Value arg = UNDEFINED_VALUE;
        auto part = match(out) = (anyOperation(), capture(arg));

        TEST_ASSERT(!matches(inst, part))
        // check captures not updated
        TEST_ASSERT(arg.isUndefined())
    }

    // failing test - no instruction
    {
        auto part = anyValue() = (anyOperation(), anyValue());
        TEST_ASSERT(!matches(nullptr, part))
    }

    // failing test - same value capture conflict
    {
        auto out = assign(it, TYPE_INT32) = (17_val << ELEMENT_NUMBER_REGISTER);
        auto inst = (it.copy().previousInBlock()).get();

        Value arg = UNDEFINED_VALUE;
        auto part = match(out) = (anyOperation(), capture(arg), capture(arg));

        TEST_ASSERT(!matches(inst, part))
        // check captures not updated
        TEST_ASSERT(arg.isUndefined())
    }

    // successful test - mismatching flags
    {
        auto out = assign(it, TYPE_INT16) = (17_val ^ UNIFORM_REGISTER, SetFlag::DONT_SET);
        auto inst = (it.copy().previousInBlock()).get();

        Value arg = UNDEFINED_VALUE;
        auto part = match(out) = (match(OP_XOR), capture(arg), anyValue(), match(SetFlag::SET_FLAGS));

        TEST_ASSERT(!matches(inst, part))
        TEST_ASSERT(arg.isUndefined())
    }

    // failing match - commutative match
    {
        auto tmp = m.addNewLocal(TYPE_INT32);
        auto out = assign(it, TYPE_INT32) = tmp + 42_val;
        auto inst = (it.copy().previousInBlock()).get();
        (void) out;

        Value arg = UNDEFINED_VALUE;
        auto part = anyValue() = (match(OP_ADD), capture(arg), match(tmp) /* no  allowCommutation() */);

        TEST_ASSERT(!matches(inst, part))
        TEST_ASSERT(arg.isUndefined())
    }

    // TODO successful and failing test for supported arithmetic properties
}

void TestPatternMatching::testExpressionMatch()
{
    // successful test - exact match
    {
        Expression expr{OP_V8ADDS, 42_val, 17_val};

        auto part = anyValue() = (match(OP_V8ADDS), match(42_val), match(17_val));
        TEST_ASSERT(matches(expr, part))
    }

    // successful test - wildcard match
    {
        Expression expr{OP_V8ADDS, 42_val, 17_val};

        Value out = UNDEFINED_VALUE;
        OpCode code = OP_NOP;
        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        auto part = capture(out) = (capture(code), capture(arg0), capture(arg1));
        TEST_ASSERT(matches(expr, part))
        TEST_ASSERT(out.isUndefined())
        TEST_ASSERT(OP_V8ADDS == code)
        TEST_ASSERT_EQUALS(42_val, arg0)
        TEST_ASSERT_EQUALS(17_val, arg1)
    }

    // successful test - match same input
    {
        Expression expr{OP_V8ADDS, 42_val, 42_val};

        Value out = UNDEFINED_VALUE;
        OpCode code = OP_NOP;
        Value arg = UNDEFINED_VALUE;

        auto part = capture(out) = (capture(code), capture(arg), capture(arg));
        TEST_ASSERT(matches(expr, part))
        TEST_ASSERT(out.isUndefined())
        TEST_ASSERT(OP_V8ADDS == code)
        TEST_ASSERT_EQUALS(42_val, arg)
    }

    // successful match - commutative match
    {
        Expression expr{OP_FADD, 42_val, 17_val};

        Value arg = UNDEFINED_VALUE;
        auto part = anyValue() = (match(OP_FADD), capture(arg), match(42_val), allowCommutation());

        TEST_ASSERT(matches(expr, part))
        TEST_ASSERT_EQUALS(17_val, arg)
    }

    // failing test - value mismatch
    {
        Expression expr{OP_V8ADDS, 42_val, 17_val};

        OpCode code = OP_NOP;
        Value arg1 = UNDEFINED_VALUE;

        auto part = anyValue() = (capture(code), match(43_val), capture(arg1));
        TEST_ASSERT(!matches(expr, part))
        TEST_ASSERT(code == OP_NOP)
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - opcode mismatch
    {
        Expression expr{OP_V8ADDS, 42_val, 17_val};

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        auto part = anyValue() = (match(OP_ADD), capture(arg0), capture(arg1));
        TEST_ASSERT(!matches(expr, part))
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - pack mode
    {
        Expression expr{OP_V8ADDS, 42_val, 17_val, UNPACK_NOP, PACK_32_32};

        OpCode code = OP_NOP;
        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        auto part = anyValue() = (capture(code), capture(arg0), capture(arg1));
        TEST_ASSERT(!matches(expr, part))
        TEST_ASSERT(code == OP_NOP)
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - unpack mode
    {
        Expression expr{OP_V8ADDS, 42_val, 17_val, UNPACK_8A_32};

        OpCode code = OP_NOP;
        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        auto part = anyValue() = (capture(code), capture(arg0), capture(arg1));
        TEST_ASSERT(!matches(expr, part))
        TEST_ASSERT(code == OP_NOP)
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - same value capture conflict
    {
        Expression expr{OP_V8ADDS, 42_val, 17_val};

        OpCode code = OP_NOP;
        Value arg = UNDEFINED_VALUE;

        auto part = anyValue() = (capture(code), capture(arg), capture(arg));
        TEST_ASSERT(!matches(expr, part))
        TEST_ASSERT(code == OP_NOP)
        TEST_ASSERT(arg.isUndefined())
    }

    // failing test - commutative match
    {
        Expression expr{OP_FADD, 42_val, 17_val};

        Value arg = UNDEFINED_VALUE;
        auto part = anyValue() = (match(OP_FADD), capture(arg), match(42_val) /* no allowCommutation() */);

        TEST_ASSERT(!matches(expr, part))
        TEST_ASSERT(arg.isUndefined())
    }
}

void TestPatternMatching::testSingleSearch()
{
    Method m(module);
    auto& block = m.createAndInsertNewBlock(m.begin(), "dummyLabel");
    auto it = block.walkEnd();

    // successful test - matching start
    {
        assignNop(it) = 42_val + 17_val;

        Value arg = UNDEFINED_VALUE;
        auto start = block.walk().nextInBlock();
        auto part = anyValue() = (anyOperation(), capture(arg));
        TEST_ASSERT(start == search(start, part))
        TEST_ASSERT_EQUALS(42_val, arg)
    }

    // successful test - matching last instruction
    {
        auto out = assign(it, TYPE_INT32) = 42_val + UNIFORM_REGISTER;

        Value arg = UNDEFINED_VALUE;
        auto part = match(out) = (anyOperation(), anyValue(), capture(arg));
        auto match = search(block.walk(), part);
        TEST_ASSERT(!match.isEndOfBlock())
        TEST_ASSERT(block.walkEnd().previousInBlock() == match)
        TEST_ASSERT_EQUALS(UNIFORM_REGISTER, arg)
    }

    // successful test - same input match
    {
        assignNop(it) = 19_val + 19_val;

        Value arg = UNDEFINED_VALUE;
        auto start = block.walk().nextInBlock();
        auto part = anyValue() = (anyOperation(), capture(arg), capture(arg));
        TEST_ASSERT(!search(start, part).isEndOfBlock())
        TEST_ASSERT_EQUALS(19_val, arg)
    }

    // successful test - reset of commutative flag
    {
        assignNop(it) = 13_val | 17_val;
        assignNop(it) = 11_val | 13_val;

        Value arg = UNDEFINED_VALUE;
        auto start = block.walk().nextInBlock();
        auto part = anyValue() = (match(OP_OR), capture(arg), match(13_val), allowCommutation());
        auto it = search(start, part);
        TEST_ASSERT(!it.isEndOfBlock())
        TEST_ASSERT_EQUALS(17_val, arg)

        it.nextInBlock();
        TEST_ASSERT(!it.isEndOfBlock())
        TEST_ASSERT(!search(it, part).isEndOfBlock())
        TEST_ASSERT_EQUALS(11_val, arg)
    }

    // failing test - start is end of block
    {
        auto part = anyValue() = (anyOperation(), anyValue());
        TEST_ASSERT(search(InstructionWalker{}, part).isEndOfBlock())
    }

    // failing test - no matching instructions
    {
        auto part = match(UNIFORM_REGISTER) = (anyOperation(), anyValue());
        TEST_ASSERT(search(block.walk(), part).isEndOfBlock())
    }

    // failing test - start after matching instruction
    {
        auto out = assign(it, TYPE_FLOAT) = ELEMENT_NUMBER_REGISTER / 2_lit;
        auto start = it.previousInBlock().nextInBlock();
        assignNop(it) = UNIFORM_REGISTER - 17_val;

        auto part = match(out) = (anyOperation(), match(ELEMENT_NUMBER_REGISTER));
        TEST_ASSERT(search(start, part).isEndOfBlock())
    }

    // failing test - same value capture conflict
    {
        assignNop(it) = 19_val ^ 17_val;

        Value arg = UNDEFINED_VALUE;
        auto start = block.walk().nextInBlock();
        auto part = anyValue() = (match(OP_XOR), capture(arg), capture(arg));
        TEST_ASSERT(search(start, part).isEndOfBlock())
        TEST_ASSERT(arg.isUndefined())
    }
}

void TestPatternMatching::testConsecutiveSearch()
{
    Method m(module);
    auto& block = m.createAndInsertNewBlock(m.begin(), "dummyLabel");
    auto it = block.walkEnd();

    // successful test - start matches
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER - 2_val;
        auto out2 = assign(it, TYPE_INT32) = out | 110_val;

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{match(out) = (anyOperation(), match(UNIFORM_REGISTER), capture(arg0)),
                            match(out2) = (match(OP_OR), match(out), capture(arg1))},
            false};

        auto start = block.walk().nextInBlock();
        auto last = block.walkEnd().previousInBlock();
        auto matchIt = search(start, pattern);
        TEST_ASSERT(matchIt == start)
        TEST_ASSERT_EQUALS(2_val, arg0)
        TEST_ASSERT_EQUALS(110_val, arg1)

        matchIt = search(start, pattern, true);
        TEST_ASSERT(matchIt == last);
        TEST_ASSERT_EQUALS(2_val, arg0)
        TEST_ASSERT_EQUALS(110_val, arg1)

        // should have same result on repeat
        matchIt = search(start, pattern);
        TEST_ASSERT(matchIt == start)
        TEST_ASSERT_EQUALS(2_val, arg0)
        TEST_ASSERT_EQUALS(110_val, arg1)
    }

    // successful test - last instructions match
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER + 17_val;
        auto out2 = assign(it, TYPE_INT32) = out & 110_val;

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{match(out) = (anyOperation(), match(UNIFORM_REGISTER), capture(arg0)),
                            match(out2) = (match(OP_AND), match(out), capture(arg1))},
            false};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(!matchIt.isEndOfBlock())
        TEST_ASSERT_EQUALS(17_val, arg0)
        TEST_ASSERT_EQUALS(110_val, arg1)
    }

    // successful test - same capture across instructions
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER ^ 17_val;
        auto out2 = assign(it, TYPE_INT32) = out & 17_val;

        Value arg = UNDEFINED_VALUE;

        Pattern pattern{{match(out) = (match(OP_XOR), match(UNIFORM_REGISTER), capture(arg)),
                            match(out2) = (match(OP_AND), match(out), capture(arg))},
            false};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(!matchIt.isEndOfBlock())
        TEST_ASSERT_EQUALS(17_val, arg)
    }

    // successful test - same code capture across instructions
    {
        auto out = assign(it, TYPE_BOOL) = 111_val | UNIFORM_REGISTER;
        auto out2 = assign(it, TYPE_INT16) = out | 156_val;

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;
        OpCode code = OP_NOP;

        Pattern pattern{{match(out) = (capture(code), capture(arg0), match(UNIFORM_REGISTER)),
                            match(out2) = (capture(code), match(out), capture(arg1))},
            false};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(!matchIt.isEndOfBlock())
        TEST_ASSERT(code == OP_OR)
        TEST_ASSERT_EQUALS(111_val, arg0)
        TEST_ASSERT_EQUALS(156_val, arg1)
    }

    // successful test - match inverted conditions
    {
        auto out = assign(it, TYPE_INT64) = (max(UNIFORM_REGISTER, 1717_val), COND_CARRY_CLEAR);
        auto out2 = assign(it, TYPE_INT16) = (min(ELEMENT_NUMBER_REGISTER, 1717_val), COND_CARRY_SET);

        Value arg = UNDEFINED_VALUE;
        ConditionCode cond = COND_NEVER;

        Pattern pattern{{match(out) = (match(OP_MAX), anyValue(), capture(arg), capture(cond)),
                            match(out2) = (match(OP_MIN), anyValue(), capture(arg), captureInverse(cond))},
            false};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(!matchIt.isEndOfBlock())
        TEST_ASSERT_EQUALS(1717_val, arg)
        TEST_ASSERT_EQUALS(COND_CARRY_CLEAR, cond)
    }

    // failing test - start is end of block
    {
        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{anyValue() = (anyOperation(), match(UNIFORM_REGISTER), capture(arg0)),
                            anyValue() = (match(OP_AND), anyValue(), capture(arg1))},
            false};

        auto matchIt = search(block.walkEnd().previousInBlock(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - no matching instructions
    {
        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{anyValue() = (match(OP_MUL24), match(UNIFORM_REGISTER), capture(arg0)),
                            anyValue() = (match(OP_AND), anyValue(), capture(arg1))},
            false};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - start after matching instructions
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER + 17_val;
        auto out2 = assign(it, TYPE_INT32) = out & 111_val;

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{match(out) = (anyOperation(), match(UNIFORM_REGISTER), capture(arg0)),
                            match(out2) = (match(OP_AND), match(out), capture(arg1))},
            false};

        auto matchIt = search(it, pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - other instruction within match
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER + 17_val;
        assignNop(it) = ELEMENT_NUMBER_REGISTER ^ UNIFORM_REGISTER;
        auto out2 = assign(it, TYPE_INT32) = out ^ 111_val;

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{match(out) = (anyOperation(), match(UNIFORM_REGISTER), capture(arg0)),
                            match(out2) = (match(OP_XOR), match(out), capture(arg1))},
            false};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - matching instructions in wrong order
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER + 17_val;
        auto out2 = assign(it, TYPE_INT32) = out & 111_val;

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{match(out2) = (anyOperation(), match(out), capture(arg0)),
                            match(out) = (match(OP_ADD), match(UNIFORM_REGISTER), capture(arg1))},
            false};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - same value capture conflict
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER & 17_val;
        auto out2 = assign(it, TYPE_INT32) = out & 111_val;

        Value arg = UNDEFINED_VALUE;

        Pattern pattern{{match(out) = (match(OP_AND), match(UNIFORM_REGISTER), capture(arg)),
                            match(out2) = (match(OP_AND), match(out), capture(arg))},
            false};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg.isUndefined())
    }

    // failing test - same condition capture conflict
    {
        auto out = assign(it, TYPE_INT16) = (UNIFORM_REGISTER + 17_val, COND_ZERO_CLEAR);
        auto out2 = assign(it, TYPE_INT32) = (out & 111_val, COND_NEGATIVE_SET);

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;
        ConditionCode cond = COND_NEVER;

        Pattern pattern{{match(out) = (anyOperation(), match(UNIFORM_REGISTER), capture(arg0), capture(cond)),
                            match(out2) = (match(OP_AND), match(out), capture(arg1), capture(cond))},
            false};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
        TEST_ASSERT_EQUALS(cond, COND_NEVER)
    }

    // failing test - inverted conditions capture conflict
    {
        auto out = assign(it, TYPE_INT64) = (max(UNIFORM_REGISTER, 1717_val), COND_CARRY_CLEAR);
        auto out2 = assign(it, TYPE_INT16) = (min(ELEMENT_NUMBER_REGISTER, 1717_val), COND_NEGATIVE_CLEAR);

        Value arg = UNDEFINED_VALUE;
        ConditionCode cond = COND_NEVER;

        Pattern pattern{{match(out) = (match(OP_MAX), anyValue(), capture(arg), capture(cond)),
                            match(out2) = (match(OP_MIN), anyValue(), capture(arg), captureInverse(cond))},
            false};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg.isUndefined())
        TEST_ASSERT_EQUALS(cond, COND_NEVER)
    }
}

void TestPatternMatching::testGappedSearch()
{
    Method m(module);
    auto& block = m.createAndInsertNewBlock(m.begin(), "dummyLabel");
    auto it = block.walkEnd();

    // successful test - matching instructions without gap
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER - 2_val;
        auto out2 = assign(it, TYPE_INT32) = out | 110_val;

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{match(out) = (anyOperation(), match(UNIFORM_REGISTER), capture(arg0)),
                            match(out2) = (match(OP_OR), match(out), capture(arg1))},
            true};

        auto start = block.walk().nextInBlock();
        auto last = block.walkEnd().previousInBlock();
        auto matchIt = search(start, pattern);
        TEST_ASSERT(matchIt == start)
        TEST_ASSERT_EQUALS(2_val, arg0)
        TEST_ASSERT_EQUALS(110_val, arg1)

        matchIt = search(start, pattern, true);
        TEST_ASSERT(matchIt == last)
        TEST_ASSERT_EQUALS(2_val, arg0)
        TEST_ASSERT_EQUALS(110_val, arg1)

        // should have same result on repeat
        matchIt = search(start, pattern);
        TEST_ASSERT(matchIt == start)
        TEST_ASSERT_EQUALS(2_val, arg0)
        TEST_ASSERT_EQUALS(110_val, arg1)
    }

    // successful test - matching instructions with gaps
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER + 17_val;
        assignNop(it) = ELEMENT_NUMBER_REGISTER ^ UNIFORM_REGISTER;
        assignNop(it) = out & 17_val;
        auto out2 = assign(it, TYPE_INT32) = out ^ 111_val;
        assignNop(it) = out2 ^ 17_val;
        auto out3 = assign(it, TYPE_FLOAT) = out2 & out;

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{match(out) = (anyOperation(), match(UNIFORM_REGISTER), capture(arg0)),
                            match(out2) = (match(OP_XOR), match(out), capture(arg1)), match(out3) = (out2 & out)},
            true};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(!matchIt.isEndOfBlock())
        TEST_ASSERT_EQUALS(17_val, arg0)
        TEST_ASSERT_EQUALS(111_val, arg1)
    }

    // successful test - same capture across instructions
    {
        auto out = assign(it, TYPE_FLOAT) = 13.0_val + Value(REG_UNIFORM, TYPE_FLOAT);
        assignNop(it) = ELEMENT_NUMBER_REGISTER ^ UNIFORM_REGISTER;
        assignNop(it) = out & 17_val;
        auto out2 = assign(it, TYPE_INT32) = out ^ 111_val;
        assignNop(it) = out2 ^ 17_val;
        auto out3 = assign(it, TYPE_FLOAT) = out2 & 13.0_val;

        Value arg = UNDEFINED_VALUE;

        Pattern pattern{{match(out) = (anyOperation(), capture(arg), match(Value(REG_UNIFORM, TYPE_FLOAT))),
                            match(out2) = (match(OP_XOR), match(out), anyValue()),
                            match(out3) = (match(OP_AND), match(out2), capture(arg))},
            true};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(!matchIt.isEndOfBlock())
        TEST_ASSERT_EQUALS(13.0_val, arg)
    }

    // successful test - same condition capture across instructions
    {
        auto out = assign(it, TYPE_FLOAT) = (Value(REG_UNIFORM, TYPE_FLOAT), COND_CARRY_CLEAR);
        assignNop(it) = ELEMENT_NUMBER_REGISTER ^ UNIFORM_REGISTER;
        assignNop(it) = out & 18_val;
        auto out2 = assign(it, TYPE_INT32) = out ^ 141_val;
        assignNop(it) = out2 ^ 18_val;
        auto out3 = assign(it, TYPE_FLOAT) = (out2 & 11.0_val, COND_CARRY_CLEAR);

        Value arg = UNDEFINED_VALUE;
        ConditionCode cond = COND_NEVER;

        Pattern pattern{{match(out) = (anyOperation(), match(Value(REG_UNIFORM, TYPE_FLOAT)), capture(cond)),
                            match(out2) = (match(OP_XOR), match(out), anyValue()),
                            match(out3) = (match(OP_AND), match(out2), capture(arg), capture(cond))},
            true};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(!matchIt.isEndOfBlock())
        TEST_ASSERT_EQUALS(11.0_val, arg)
        TEST_ASSERT_EQUALS(COND_CARRY_CLEAR, cond)
    }

    // successful test - value written in gap (before first capture)
    {
        auto tmp1 = assign(it, TYPE_INT32) = UNIFORM_REGISTER + 17_val;
        auto tmp2 = assign(it, TYPE_INT32) = ELEMENT_NUMBER_REGISTER;
        assignNop(it) = tmp1 - tmp2;

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{capture(arg0) = (match(OP_ADD), match(UNIFORM_REGISTER), anyValue()),
                            anyValue() = (match(OP_SUB), capture(arg0), capture(arg1))},
            true};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(!matchIt.isEndOfBlock())
        TEST_ASSERT_EQUALS(tmp1, arg0)
        TEST_ASSERT_EQUALS(tmp2, arg1)
    }

    // failing test - start is end of block
    {
        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{anyValue() = (anyOperation(), match(UNIFORM_REGISTER), capture(arg0)),
                            anyValue() = (match(OP_XOR), anyValue(), capture(arg1))},
            true};

        auto matchIt = search(block.walkEnd().previousInBlock(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - matching instructions in wrong order
    {
        auto out = assign(it, TYPE_INT16) = UNIFORM_REGISTER + 17_val;
        assignNop(it) = ELEMENT_NUMBER_REGISTER ^ UNIFORM_REGISTER;
        auto out2 = assign(it, TYPE_INT32) = out ^ 111_val;

        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{match(out2) = (match(OP_XOR), match(out), capture(arg0)),
                            match(out) = (match(OP_ADD), match(UNIFORM_REGISTER), capture(arg1))},
            true};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - no matching instructions
    {
        Value arg0 = UNDEFINED_VALUE;
        Value arg1 = UNDEFINED_VALUE;

        Pattern pattern{{anyValue() = (match(OP_V8MULD), anyValue(), capture(arg0)),
                            anyValue() = (match(OP_ADD), match(UNIFORM_REGISTER), capture(arg1))},
            true};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg0.isUndefined())
        TEST_ASSERT(arg1.isUndefined())
    }

    // failing test - same value capture conflict
    {
        auto out = assign(it, TYPE_INT16) = 17_val - UNIFORM_REGISTER;
        assignNop(it) = ELEMENT_NUMBER_REGISTER ^ UNIFORM_REGISTER;
        auto out2 = assign(it, TYPE_INT32) = 111_val & out;

        Value arg = UNDEFINED_VALUE;

        Pattern pattern{{match(out) = (match(OP_SUB), capture(arg), match(UNIFORM_REGISTER)),
                            match(out2) = (match(OP_AND), capture(arg), match(out))},
            true};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg.isUndefined())
    }

    // failing test - same condition capture conflict
    {
        auto out = assign(it, TYPE_FLOAT) = (ELEMENT_NUMBER_REGISTER, COND_CARRY_CLEAR);
        assignNop(it) = out & 21_val;
        auto out2 = assign(it, TYPE_INT32) = out ^ 1234_val;
        assignNop(it) = out2 ^ 22_val;
        auto out3 = assign(it, TYPE_FLOAT) = (out2 & 1311.0_val, COND_NEGATIVE_SET);

        Value arg = UNDEFINED_VALUE;
        ConditionCode cond = COND_NEVER;

        Pattern pattern{{match(out) = (anyOperation(), match(ELEMENT_NUMBER_REGISTER), capture(cond)),
                            match(out2) = (match(OP_XOR), match(out), anyValue()),
                            match(out3) = (match(OP_AND), match(out2), capture(arg), capture(cond))},
            true};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg.isUndefined())
        TEST_ASSERT_EQUALS(COND_NEVER, cond)
    }

    // failing test - value overwritten in gap (after first capture)
    {
        auto tmp = assign(it, TYPE_INT32) = UNIFORM_REGISTER + 17_val;
        assign(it, tmp) = ELEMENT_NUMBER_REGISTER;
        assignNop(it) = tmp + tmp;

        Value arg = UNDEFINED_VALUE;

        Pattern pattern{{capture(arg) = (match(OP_ADD), match(UNIFORM_REGISTER), anyValue()),
                            anyValue() = (match(OP_ADD), capture(arg), capture(arg))},
            true};

        auto matchIt = search(block.walk(), pattern);
        TEST_ASSERT(matchIt.isEndOfBlock())
        TEST_ASSERT(arg.isUndefined())
    }
}
