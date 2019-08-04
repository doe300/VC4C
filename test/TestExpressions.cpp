/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestExpressions.h"

#include "Expression.h"
#include "InstructionWalker.h"
#include "Method.h"
#include "Module.h"
#include "intermediate/operators.h"

using namespace vc4c;
using namespace vc4c::operators;

// TODO test all different types of combination + expressions which should not be combined

TestExpressions::TestExpressions()
{
    TEST_ADD(TestExpressions::testCreation);
    TEST_ADD(TestExpressions::testCombination);
}

TestExpressions::~TestExpressions() = default;

void TestExpressions::testCreation()
{
    Module mod{Configuration{}};
    Method method(mod);

    method.appendToEnd(new intermediate::BranchLabel(*method.addNewLocal(TYPE_LABEL).local()));
    auto it = method.begin()->walkEnd();

    {
        // skip instruction with side-effects
        it.emplace(new intermediate::MoveOperation(NOP_REGISTER, INT_ONE));
        it->setSetFlags(SetFlag::SET_FLAGS);
        TEST_ASSERT(!Expression::createExpression(*it.get()))
    }

    {
        // skip instruction with conditional execution
        it.emplace(new intermediate::MoveOperation(NOP_REGISTER, INT_ONE, COND_CARRY_SET));
        TEST_ASSERT(!Expression::createExpression(*it.get()))
    }

    {
        // skip instruction reading replication output
        it.emplace(new intermediate::MoveOperation(NOP_REGISTER, Value(REG_REPLICATE_ALL, TYPE_INT32)));
        TEST_ASSERT(!Expression::createExpression(*it.get()))
    }

    {
        // skip branch instruction, semaphore
        it.emplace(new intermediate::Branch(method.begin()->getLabel()->getLabel(), COND_ALWAYS, BOOL_TRUE));
        TEST_ASSERT(!Expression::createExpression(*it.get()))

        it.emplace(new intermediate::SemaphoreAdjustment(Semaphore::BARRIER_WORK_ITEM_0, false));
        TEST_ASSERT(!Expression::createExpression(*it.get()))
    }

    {
        // skip vector rotation
        it.emplace(new intermediate::VectorRotation(NOP_REGISTER, Value(REG_REPLICATE_ALL, TYPE_INT32), INT_ONE));
        TEST_ASSERT(!Expression::createExpression(*it.get()))
    }

    {
        // skip masked load
        it.emplace(
            new intermediate::LoadImmediate(NOP_REGISTER, 0x12345678u, intermediate::LoadType::PER_ELEMENT_SIGNED));
        TEST_ASSERT(!Expression::createExpression(*it.get()))

        it.emplace(
            new intermediate::LoadImmediate(NOP_REGISTER, 0x12345678u, intermediate::LoadType::PER_ELEMENT_UNSIGNED));
        TEST_ASSERT(!Expression::createExpression(*it.get()))
    }

    {
        // create expression for "normal" load
        it.emplace(new intermediate::LoadImmediate(NOP_REGISTER, 1234_lit));
        auto expr = Expression::createExpression(*it.get());
        TEST_ASSERT(!!expr)
        TEST_ASSERT(expr->isMoveExpression())
        TEST_ASSERT(expr->hasConstantOperand())
        TEST_ASSERT(expr->getConstantExpression() == Value(1234_lit, TYPE_INT32))
    }

    {
        // create expression for moves
        it.emplace(new intermediate::MoveOperation(NOP_REGISTER, FLOAT_ONE));
        auto expr = Expression::createExpression(*it.get());
        TEST_ASSERT(!!expr)
        TEST_ASSERT(expr->isMoveExpression())
        TEST_ASSERT(expr->hasConstantOperand())
        TEST_ASSERT(expr->getConstantExpression() == FLOAT_ONE)
    }

    {
        // create expression for ALU operations
        it.emplace(new intermediate::Operation(OP_ITOF, NOP_REGISTER, FLOAT_ONE));
        auto expr = Expression::createExpression(*it.get());
        TEST_ASSERT(!!expr)
        TEST_ASSERT(!expr->isMoveExpression())
        TEST_ASSERT(expr->code == OP_ITOF)
        TEST_ASSERT(!expr->arg1)

        it.emplace(new intermediate::Operation(OP_FSUB, NOP_REGISTER, FLOAT_ONE, Value(2345.0_lit, TYPE_FLOAT)));
        auto expr2 = Expression::createExpression(*it.get());
        TEST_ASSERT(!!expr2)
        TEST_ASSERT(!expr2->isMoveExpression())
        TEST_ASSERT(expr2->code == OP_FSUB)
        TEST_ASSERT(!!expr2->arg1)
    }
}

void TestExpressions::testCombination()
{
    Module mod{Configuration{}};
    Method method(mod);
    FastMap<const Local*, Expression> expressions;

    auto loc0 = method.addNewLocal(TYPE_INT32);
    Expression loc0Plus1{OP_ADD, loc0, INT_ONE};
    auto loc1 = method.addNewLocal(TYPE_INT32);
    expressions.emplace(loc1.local(), loc0Plus1);
    TEST_ASSERT_EQUALS(loc0Plus1, loc0Plus1.combineWith(expressions))

    Expression moveLoc1{OP_OR, loc1, loc1};
    auto loc2 = method.addNewLocal(TYPE_INT32);
    expressions.emplace(loc2.local(), moveLoc1);
    TEST_ASSERT_EQUALS(loc0Plus1, moveLoc1.combineWith(expressions))

    Expression loc2Plus1{OP_ADD, loc2, INT_ONE};
    auto loc3 = method.addNewLocal(TYPE_INT32);
    expressions.emplace(loc3.local(), loc2Plus1);

    Expression moveLoc3{OP_V8MIN, loc3, loc3};
    Expression moveZero{OP_V8MIN, INT_ZERO, INT_ZERO};
    Expression loc0Plus2{OP_ADD, loc0, Value(2_lit, TYPE_INT32)};

    {
        Expression loc2Pack{OP_OR, loc2, NO_VALUE, UNPACK_NOP, PACK_32_16B};
        TEST_ASSERT_EQUALS(loc2Pack, loc2Pack.combineWith(expressions))
    }

    {
        // single operand combination rules
        Expression notLoc3{OP_NOT, loc3, NO_VALUE};
        auto loc4 = method.addNewLocal(TYPE_INT32);
        expressions.emplace(loc4.local(), notLoc3);

        Expression notLoc4{OP_NOT, loc4, NO_VALUE};
        TEST_ASSERT_EQUALS(moveLoc3, notLoc4.combineWith(expressions))
    }

    {
        // 2 operands idempotent, identity and absorbing
        Expression andLoc3{OP_AND, loc3, loc3};
        TEST_ASSERT_EQUALS(loc0Plus2, andLoc3.combineWith(expressions))

        Expression andLoc3Id{OP_AND, loc3, INT_MINUS_ONE};
        TEST_ASSERT_EQUALS(moveLoc3, andLoc3Id.combineWith(expressions))

        Expression andIdLoc3{OP_AND, INT_MINUS_ONE, loc3};
        TEST_ASSERT_EQUALS(moveLoc3, andIdLoc3.combineWith(expressions))

        Expression andLoc3Absorb{OP_AND, loc3, INT_ZERO};
        TEST_ASSERT_EQUALS(moveZero, andLoc3Absorb.combineWith(expressions))

        Expression andAbsorbLoc3{OP_AND, INT_ZERO, loc3};
        TEST_ASSERT_EQUALS(moveZero, andAbsorbLoc3.combineWith(expressions))
    }

    {
        // 2 operands associativity (and commutativity) with constants
        Expression onePlusLoc0{OP_ADD, INT_ONE, loc0};
        auto loc4 = method.addNewLocal(TYPE_INT32);
        expressions.emplace(loc4.local(), onePlusLoc0);
        Expression onePlusLoc4{OP_ADD, INT_ONE, loc4};
        // x + 2 = 1 + (1 + x)
        TEST_ASSERT_EQUALS(loc0Plus2, onePlusLoc4.combineWith(expressions))
        // x + 2 = (x + 1) + 1
        TEST_ASSERT_EQUALS(loc0Plus2, loc2Plus1.combineWith(expressions))
        // x + 2 = 1 + (x + 1)
        Expression onePlusLoc2{OP_ADD, INT_ONE, loc2};
        TEST_ASSERT_EQUALS(loc0Plus2, onePlusLoc2.combineWith(expressions))
        // x + 2 = (1 + x) + 1
        Expression loc4Plus1{OP_ADD, loc4, INT_ONE};
        TEST_ASSERT_EQUALS(loc0Plus2, loc4Plus1.combineWith(expressions))
    }

    {
        // 2 operands associativity and idempotence (and commutativity)
        auto a = method.addNewLocal(TYPE_FLOAT);
        auto b = method.addNewLocal(TYPE_FLOAT);
        auto c = method.addNewLocal(TYPE_FLOAT);
        Expression minAB{OP_FMIN, a, b};
        expressions.emplace(c.local(), minAB);
        // min(a, min(a, b)) = min(a, b)
        Expression minAC{OP_FMIN, a, c};
        TEST_ASSERT_EQUALS(minAB, minAC.combineWith(expressions))
        // min(min(a, b), b) = min(a, b)
        Expression minCB{OP_FMIN, c, b};
        TEST_ASSERT_EQUALS(minAB, minCB.combineWith(expressions))
        // min(b, min(a, b)) = min(a, b)
        Expression minBC{OP_FMIN, b, c};
        TEST_ASSERT_EQUALS(minAB, minBC.combineWith(expressions))
        // min(min(a, b), a) = min(a, b)
        Expression minCA{OP_FMIN, c, a};
        TEST_ASSERT_EQUALS(minAB, minCA.combineWith(expressions))
    }

    {
        // 2 operands distributivity
        auto a = method.addNewLocal(TYPE_FLOAT);

        Expression aMul2{OP_FMUL, a, Value(2.0_lit, TYPE_FLOAT)};
        auto a2 = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(a2.local(), aMul2);
        Expression aMul3{OP_FMUL, a, Value(3.0_lit, TYPE_FLOAT)};
        auto a3 = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(a3.local(), aMul3);

        Expression twoMulA{OP_FMUL, Value(2.0_lit, TYPE_FLOAT), a};
        auto twoA = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(twoA.local(), twoMulA);
        Expression threeMulA{OP_FMUL, Value(3.0_lit, TYPE_FLOAT), a};
        auto threeA = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(threeA.local(), threeMulA);

        Expression aMul5{OP_FMUL, a, Value(5.0_lit, TYPE_FLOAT)};

        // (a * 2) + (a * 3) = a * (2 + 3)
        Expression a2PlusA3{OP_FADD, a2, a3};
        TEST_ASSERT_EQUALS(aMul5, a2PlusA3.combineWith(expressions))
        // (2 * a) + (3 * a) = (2 + 3) * a
        Expression twoAPlusThreeA{OP_FADD, twoA, threeA};
        TEST_ASSERT_EQUALS(aMul5, twoAPlusThreeA.combineWith(expressions))
        
        //XXX tests for general case, when added
    }
}
