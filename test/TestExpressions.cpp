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
#include "analysis/ValueRange.h"
#include "intermediate/operators.h"

using namespace vc4c;
using namespace vc4c::operators;

// TODO test all different types of combination + expressions which should not be combined

TestExpressions::TestExpressions()
{
    TEST_ADD(TestExpressions::testCreation);
    TEST_ADD(TestExpressions::testCombination);
    TEST_ADD(TestExpressions::testConvergence);
    TEST_ADD(TestExpressions::testValueRange);
}

TestExpressions::~TestExpressions() = default;

void TestExpressions::testCreation()
{
    Configuration config{};
    Module mod{config};
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
        it.emplace(new intermediate::VectorRotation(NOP_REGISTER, Value(REG_REPLICATE_ALL, TYPE_INT32),
            SmallImmediate::fromRotationOffset(1), intermediate::RotationType::FULL));
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

    {
        // recursive + simplification
        auto loc = method.addNewLocal(TYPE_INT32);
        auto origLoc = loc;
        for(auto i = 0; i < 10; ++i)
        {
            loc = assign(it, TYPE_INT32) = loc + 1_val;
        }
        it.previousInBlock();
        auto expr = Expression::createRecursiveExpression(*it.get(), 13);
        TEST_ASSERT(!!expr)
        auto combined = expression(origLoc + 10_val);
        TEST_ASSERT_EQUALS(*combined, *expr)
    }

    {
        // recursive + simplification with fake operations
        auto loc = method.addNewLocal(TYPE_INT32);
        auto origLoc = loc;

        // x * 4 ...
        loc = assign(it, TYPE_INT32) = loc << 2_val;
        // ... + x ...
        loc = assign(it, TYPE_INT32) = loc + origLoc;
        // ... * 4 ...
        loc = assign(it, TYPE_INT32) = loc << 2_val;
        // ... + x ...
        loc = assign(it, TYPE_INT32) = loc + origLoc;
        // (x * 4 + x) * 4 + x = x * 21

        it.previousInBlock();
        auto expr = Expression::createRecursiveExpression(*it.get());
        TEST_ASSERT(!!expr)
        auto combined = std::make_shared<Expression>(Expression::FAKEOP_UMUL, origLoc, 21_val);
        TEST_ASSERT_EQUALS(*combined, *expr)
    }
}

void TestExpressions::testCombination()
{
    Configuration config{};
    Module mod{config};
    Method method(mod);
    FastMap<const Local*, std::shared_ptr<Expression>> expressions;

    auto loc0 = method.addNewLocal(TYPE_INT32);
    auto loc0Plus1 = expression(loc0 + INT_ONE);
    auto loc1 = method.addNewLocal(TYPE_INT32);
    expressions.emplace(loc1.local(), loc0Plus1);
    TEST_ASSERT_EQUALS(loc0Plus1, loc0Plus1->combineWith(expressions))

    auto moveLoc1 = expression(loc1 | loc1);
    auto loc2 = method.addNewLocal(TYPE_INT32);
    expressions.emplace(loc2.local(), moveLoc1);
    TEST_ASSERT_EQUALS(loc0Plus1, moveLoc1->combineWith(expressions))

    auto loc2Plus1 = expression(loc2 + INT_ONE);
    auto loc3 = method.addNewLocal(TYPE_INT32);
    expressions.emplace(loc3.local(), loc2Plus1);

    Expression moveLoc3{OP_V8MIN, loc3, loc3};
    Expression moveZero{OP_V8MIN, INT_ZERO, INT_ZERO};
    Expression loc0Plus2{OP_ADD, loc0, Value(2_lit, TYPE_INT32)};

    {
        auto loc2Pack = expression((loc2 | INT_ONE, PACK_32_16B));
        TEST_ASSERT_EQUALS(*loc2Pack, *loc2Pack->combineWith(expressions))
    }

    {
        // single operand combination rules
        auto notLoc3 = expression(~loc3);
        auto loc4 = method.addNewLocal(TYPE_INT32);
        expressions.emplace(loc4.local(), notLoc3);

        Expression notLoc4{OP_NOT, loc4, NO_VALUE};
        TEST_ASSERT_EQUALS(moveLoc3, *notLoc4.combineWith(expressions))
    }

    {
        // 2 operands, self-inverse
        auto selfInverse = expression(loc3 - loc3);
        TEST_ASSERT_EQUALS(moveZero, *selfInverse->combineWith(expressions))

        auto someLocal = method.addNewLocal(TYPE_INT32);
        expressions.emplace(someLocal.local(), selfInverse);
        selfInverse = expression(someLocal - someLocal);
        Expression moveZeroFloat{OP_V8MIN, FLOAT_ZERO, FLOAT_ZERO};
        TEST_ASSERT_EQUALS(moveZeroFloat, *selfInverse->combineWith(expressions))
    }

    {
        // 2 operands idempotent, identity and absorbing
        auto andLoc3 = expression(loc3 & loc3);
        TEST_ASSERT_EQUALS(loc0Plus2, *andLoc3->combineWith(expressions))

        Expression andLoc3Id{OP_AND, loc3, INT_MINUS_ONE};
        TEST_ASSERT_EQUALS(*loc2Plus1, *andLoc3Id.combineWith(expressions))

        Expression andIdLoc3{OP_AND, INT_MINUS_ONE, loc3};
        TEST_ASSERT_EQUALS(*loc2Plus1, *andIdLoc3.combineWith(expressions))

        Expression andLoc3Absorb{OP_AND, loc3, INT_ZERO};
        TEST_ASSERT_EQUALS(moveZero, *andLoc3Absorb.combineWith(expressions))

        Expression andAbsorbLoc3{OP_AND, INT_ZERO, loc3};
        TEST_ASSERT_EQUALS(moveZero, *andAbsorbLoc3.combineWith(expressions))
    }

    {
        // 2 operands associativity (and commutativity) with constants
        auto onePlusLoc0 = std::make_shared<Expression>(OP_ADD, INT_ONE, loc0);
        auto loc4 = method.addNewLocal(TYPE_INT32);
        expressions.emplace(loc4.local(), onePlusLoc0);
        Expression onePlusLoc4{OP_ADD, INT_ONE, loc4};
        // x + 2 = 1 + (1 + x)
        TEST_ASSERT_EQUALS(loc0Plus2, *onePlusLoc4.combineWith(expressions))
        // x + 2 = (x + 1) + 1
        TEST_ASSERT_EQUALS(loc0Plus2, *loc2Plus1->combineWith(expressions))
        // x + 2 = 1 + (x + 1)
        Expression onePlusLoc2{OP_ADD, INT_ONE, loc2};
        TEST_ASSERT_EQUALS(loc0Plus2, *onePlusLoc2.combineWith(expressions))
        // x + 2 = (1 + x) + 1
        Expression loc4Plus1{OP_ADD, loc4, INT_ONE};
        TEST_ASSERT_EQUALS(loc0Plus2, *loc4Plus1.combineWith(expressions))
    }

    {
        // 2 operands associativity and idempotence (and commutativity)
        auto a = method.addNewLocal(TYPE_FLOAT);
        auto b = method.addNewLocal(TYPE_FLOAT);
        auto c = method.addNewLocal(TYPE_FLOAT);
        auto minAB = std::make_shared<Expression>(OP_FMIN, a, b);
        expressions.emplace(c.local(), minAB);
        // min(a, min(a, b)) = min(a, b)
        Expression minAC{OP_FMIN, a, c};
        TEST_ASSERT_EQUALS(*minAB, *minAC.combineWith(expressions))
        // min(min(a, b), b) = min(a, b)
        Expression minCB{OP_FMIN, c, b};
        TEST_ASSERT_EQUALS(*minAB, *minCB.combineWith(expressions))
        // min(b, min(a, b)) = min(a, b)
        Expression minBC{OP_FMIN, b, c};
        TEST_ASSERT_EQUALS(*minAB, *minBC.combineWith(expressions))
        // min(min(a, b), a) = min(a, b)
        Expression minCA{OP_FMIN, c, a};
        TEST_ASSERT_EQUALS(*minAB, *minCA.combineWith(expressions))
    }

    {
        // 2 operands distributivity
        auto a = method.addNewLocal(TYPE_FLOAT);

        auto aMul2 = std::make_shared<Expression>(OP_FMUL, a, Value(2.0_lit, TYPE_FLOAT));
        auto a2 = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(a2.local(), aMul2);
        auto aMul3 = std::make_shared<Expression>(OP_FMUL, a, Value(3.0_lit, TYPE_FLOAT));
        auto a3 = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(a3.local(), aMul3);

        auto twoMulA = std::make_shared<Expression>(OP_FMUL, Value(2.0_lit, TYPE_FLOAT), a);
        auto twoA = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(twoA.local(), twoMulA);
        auto threeMulA = std::make_shared<Expression>(OP_FMUL, Value(3.0_lit, TYPE_FLOAT), a);
        auto threeA = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(threeA.local(), threeMulA);

        Expression aMul5{OP_FMUL, a, Value(5.0_lit, TYPE_FLOAT)};

        // (a * 2) + (a * 3) = a * (2 + 3)
        Expression a2PlusA3{OP_FADD, a2, a3};
        TEST_ASSERT_EQUALS(aMul5, *a2PlusA3.combineWith(expressions))
        // (2 * a) + (3 * a) = (2 + 3) * a
        Expression twoAPlusThreeA{OP_FADD, twoA, threeA};
        TEST_ASSERT_EQUALS(aMul5, *twoAPlusThreeA.combineWith(expressions))

        // general non-constant case
        auto b = method.addNewLocal(TYPE_FLOAT);
        auto c = method.addNewLocal(TYPE_FLOAT);

        auto aMulB = std::make_shared<Expression>(OP_FMUL, a, b);
        auto abc2 = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(abc2.local(), aMulB);
        auto aMulC = std::make_shared<Expression>(OP_FMUL, a, c);
        auto abc3 = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(abc3.local(), aMulC);

        auto BMulA = std::make_shared<Expression>(OP_FMUL, b, a);
        auto twoABC = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(twoABC.local(), BMulA);
        auto CMulA = std::make_shared<Expression>(OP_FMUL, c, a);
        auto threeABC = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(threeABC.local(), CMulA);

        auto bc = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(bc.local(), expression(b + c));
        auto aMulBC = expression(as_float{a} * as_float{bc})->combineWith(expressions);

        // (a * b) + (a * c) = a * (b + c)
        Expression abc2PlusABC3{OP_FADD, abc2, abc3};
        TEST_ASSERT_EQUALS(*aMulBC, *abc2PlusABC3.combineWith(expressions))
        // (b * a) + (c * a) = (b + c) * a
        Expression twoABCPlusThreeABC{OP_FADD, twoABC, threeABC};
        TEST_ASSERT_EQUALS(*aMulBC, *twoABCPlusThreeABC.combineWith(expressions))
    }

    {
        // 2 operands, special code-specific rules
        auto a = method.addNewLocal(TYPE_FLOAT);
        auto b = method.addNewLocal(TYPE_FLOAT);

        // fadd(fmul(a, constB), a) = fmul(a, constB+1)
        auto innerMul = expression(a * Value(Literal(42.0f), TYPE_FLOAT));
        auto inner = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(inner.local(), innerMul);
        auto outerAdd = expression(inner + a);
        Expression result{OP_FMUL, a, Value(Literal(43.0f), TYPE_FLOAT)};
        TEST_ASSERT_EQUALS(result, *outerAdd->combineWith(expressions))

        // fadd(fmul(constB, a), a) = fmul(a, constB+1)
        innerMul = expression(Value(Literal(42.0f), TYPE_FLOAT) * a);
        expressions.at(inner.local()) = innerMul;
        outerAdd = expression(inner + a);
        result = Expression{OP_FMUL, a, Value(Literal(43.0f), TYPE_FLOAT)};
        TEST_ASSERT_EQUALS(result, *outerAdd->combineWith(expressions))

        // fadd(a, fmul(a, constB)) = fmul(a, constB+1)
        innerMul = expression(a * Value(Literal(42.0f), TYPE_FLOAT));
        expressions.at(inner.local()) = innerMul;
        outerAdd = expression(a + inner);
        result = Expression{OP_FMUL, a, Value(Literal(43.0f), TYPE_FLOAT)};
        TEST_ASSERT_EQUALS(result, *outerAdd->combineWith(expressions))

        // fadd(a, fmul(constB, a)) = fmul(a, constB+1)
        innerMul = expression(Value(Literal(42.0f), TYPE_FLOAT) * a);
        expressions.at(inner.local()) = innerMul;
        outerAdd = expression(a + inner);
        result = Expression{OP_FMUL, a, Value(Literal(43.0f), TYPE_FLOAT)};
        TEST_ASSERT_EQUALS(result, *outerAdd->combineWith(expressions))

        // (a + b) - a = b
        auto innerAdd = expression(a + b);
        expressions.at(inner.local()) = innerAdd;
        auto outer = expression(inner - a);
        result = Expression{OP_V8MIN, b, b};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions))

        // (a + b) - b = a
        outer = expression(inner - b);
        result = Expression{OP_V8MIN, a, a};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions))

        // (a - b) + b = a
        auto innerSub = expression(a - b);
        expressions.at(inner.local()) = innerSub;
        outer = expression(inner + b);
        result = Expression{OP_V8MIN, a, a};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions))

        // (a - b) - a = -b
        outer = expression(inner - a);
        result = Expression{OP_FSUB, FLOAT_ZERO, b};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions))

        // a + (b - a) = b
        innerSub = expression(b - a);
        expressions.at(inner.local()) = innerSub;
        outer = expression(a + inner);
        result = Expression{OP_V8MIN, b, b};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions))

        // a - (b + a) = -b
        innerAdd = expression(b + a);
        expressions.at(inner.local()) = innerAdd;
        outer = expression(a - inner);
        result = Expression{OP_FSUB, FLOAT_ZERO, b};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions))

        // a - (a + b) = -b
        innerAdd = expression(a + b);
        expressions.at(inner.local()) = innerAdd;
        outer = expression(a - inner);
        result = Expression{OP_FSUB, FLOAT_ZERO, b};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions))

        // (a << const) + a = a + (a << const) -> a * ((1 << const) + 1)
        auto innerShift = expression(loc0 << 12_val);
        inner = method.addNewLocal(TYPE_INT32);
        expressions.emplace(inner.local(), innerShift);
        outer = expression(inner + loc0);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 4097_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))

        outer = expression(loc0 + inner);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 4097_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))

        // (a << const) - a = a * ((1 << const) - 1)
        outer = expression(inner - loc0);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 4095_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))

        // (a * constA) << constB = (constA * a) << constB -> a * (constA << constB)
        innerMul = std::make_shared<Expression>(Expression::FAKEOP_UMUL, loc0, 27_val);
        expressions.at(inner.local()) = innerMul;
        outer = expression(inner << 13_val);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 221184_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))

        innerMul = std::make_shared<Expression>(Expression::FAKEOP_UMUL, 27_val, loc0);
        expressions.at(inner.local()) = innerMul;
        outer = expression(inner << 13_val);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 221184_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))

        // (a * constA) + a = a * (constA + 1)
        innerMul = std::make_shared<Expression>(Expression::FAKEOP_UMUL, loc0, 17_val);
        expressions.at(inner.local()) = innerMul;
        outer = expression(inner + loc0);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 18_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))

        // (constA * a) + a = a * (constA + 1)
        innerMul = std::make_shared<Expression>(Expression::FAKEOP_UMUL, 17_val, loc0);
        expressions.at(inner.local()) = innerMul;
        outer = expression(inner + loc0);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 18_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))

        // a + (a * constA) = a * (constA + 1)
        innerMul = std::make_shared<Expression>(Expression::FAKEOP_UMUL, loc0, 17_val);
        expressions.at(inner.local()) = innerMul;
        outer = expression(loc0 + inner);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 18_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))

        // a + (constA * a) = a * (constA + 1)
        innerMul = std::make_shared<Expression>(Expression::FAKEOP_UMUL, 17_val, loc0);
        expressions.at(inner.local()) = innerMul;
        outer = expression(loc0 + inner);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 18_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))

        // (a * constA) - a = a * (constA - 1)
        innerMul = std::make_shared<Expression>(Expression::FAKEOP_UMUL, loc0, 17_val);
        expressions.at(inner.local()) = innerMul;
        outer = expression(inner - loc0);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 16_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))

        // (constA * a) - a = a * (constA - 1)
        innerMul = std::make_shared<Expression>(Expression::FAKEOP_UMUL, 17_val, loc0);
        expressions.at(inner.local()) = innerMul;
        outer = expression(inner - loc0);
        result = Expression{Expression::FAKEOP_UMUL, loc0, 16_val};
        TEST_ASSERT_EQUALS(result, *outer->combineWith(expressions, ExpressionOptions::ALLOW_FAKE_OPS))
    }

    {
        // special cases with sub-expression

        // f(f(a)) = a
        auto someLoc = method.addNewLocal(TYPE_INT32);
        auto subExpr = std::make_shared<Expression>(OP_MUL24, someLoc, someLoc);
        auto middleExp = std::make_shared<Expression>(OP_NOT, subExpr);
        auto outer = std::make_shared<Expression>(OP_NOT, middleExp);
        // pointer-equality checked on purpose!
        TEST_ASSERT_EQUALS(subExpr, outer->combineWith(expressions))

        // f(a, a) = a
        outer = std::make_shared<Expression>(OP_AND, subExpr, subExpr);
        // pointer-equality checked on purpose!
        TEST_ASSERT_EQUALS(subExpr, outer->combineWith(expressions))

        // (a + b) - a = b
        auto someExpr = std::make_shared<Expression>(OP_MUL24, someLoc, someLoc);
        subExpr = std::make_shared<Expression>(OP_ADD, someLoc, someExpr);
        outer = std::make_shared<Expression>(OP_SUB, subExpr, someLoc);
        // pointer-equality checked on purpose!
        TEST_ASSERT_EQUALS(someExpr, outer->combineWith(expressions))

        // a + (b - a) = b
        subExpr = std::make_shared<Expression>(OP_SUB, someExpr, someLoc);
        outer = std::make_shared<Expression>(OP_ADD, someLoc, subExpr);
        // pointer-equality checked on purpose!
        TEST_ASSERT_EQUALS(someExpr, outer->combineWith(expressions))
    }
}

void TestExpressions::testConvergence()
{
    Configuration config{};
    Module mod{config};
    Method method(mod);

    auto floatVal = method.addNewLocal(TYPE_FLOAT);
    auto intVal = method.addNewLocal(TYPE_INT32);

    Value floatPos = 23.0_val;
    Value floatNeg(Literal(-23.0f), TYPE_FLOAT);
    Value intPos = 23_val;
    Value intNeg(Literal(-23), TYPE_INT32);
    Value intMin = 0x80000000_val;
    Value intMax = 0x7FFFFFFF_val;

    // fadd
    TEST_ASSERT(!expression(floatVal + floatVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(FLOAT_INF, expression(floatVal + floatVal)->getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(FLOAT_NEG_INF, expression(floatVal + floatVal)->getConvergenceLimit(floatNeg.literal()))
    TEST_ASSERT_EQUALS(FLOAT_INF, expression(floatVal + floatPos)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(FLOAT_INF, expression(floatPos + floatVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(FLOAT_NEG_INF, expression(floatVal + floatNeg)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(FLOAT_NEG_INF, expression(floatNeg + floatVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(floatVal + FLOAT_ZERO)->getConvergenceLimit())
    TEST_ASSERT(!expression(FLOAT_ZERO + floatVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(floatPos, expression(floatVal + FLOAT_ZERO)->getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(floatPos, expression(FLOAT_ZERO + floatVal)->getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(46.0_val, expression(floatPos + floatPos)->getConvergenceLimit())

    // fsub
    TEST_ASSERT_EQUALS(FLOAT_ZERO, expression(floatVal - floatVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(FLOAT_NEG_INF, expression(floatVal - floatPos)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(FLOAT_INF, expression(floatVal - floatNeg)->getConvergenceLimit())
    TEST_ASSERT(!expression(floatPos - floatVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(floatNeg - floatVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(floatVal - FLOAT_ZERO)->getConvergenceLimit())
    TEST_ASSERT(!expression(FLOAT_ZERO - floatVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(floatPos, expression(floatVal - FLOAT_ZERO)->getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(46.0_val, expression(floatPos - floatNeg)->getConvergenceLimit())

    // fmin
    TEST_ASSERT(!expression(min(floatVal, floatVal))->getConvergenceLimit())
    TEST_ASSERT_EQUALS(floatNeg, expression(min(floatVal, floatPos))->getConvergenceLimit(floatNeg.literal()))
    TEST_ASSERT_EQUALS(floatNeg, expression(min(floatVal, floatNeg))->getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(floatNeg, expression(min(floatPos, floatVal))->getConvergenceLimit(floatNeg.literal()))
    TEST_ASSERT_EQUALS(floatNeg, expression(min(floatNeg, floatVal))->getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT(!expression(min(floatNeg, floatVal))->getConvergenceLimit())
    TEST_ASSERT_EQUALS(floatNeg, expression(min(floatPos, floatNeg))->getConvergenceLimit())

    // fmax
    TEST_ASSERT(!expression(max(floatVal, floatVal))->getConvergenceLimit())
    TEST_ASSERT_EQUALS(floatPos, expression(max(floatVal, floatNeg))->getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(floatPos, expression(max(floatVal, floatPos))->getConvergenceLimit(floatNeg.literal()))
    TEST_ASSERT_EQUALS(floatPos, expression(max(floatNeg, floatVal))->getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(floatPos, expression(max(floatPos, floatVal))->getConvergenceLimit(floatNeg.literal()))
    TEST_ASSERT(!expression(max(floatNeg, floatVal))->getConvergenceLimit())
    TEST_ASSERT_EQUALS(floatNeg, expression(min(floatPos, floatNeg))->getConvergenceLimit())

    // fminabs
    TEST_ASSERT(!(Expression{OP_FMINABS, floatVal, floatVal}).getConvergenceLimit())
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_FMINABS, floatVal, floatNeg}).getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_FMINABS, floatVal, floatPos}).getConvergenceLimit(floatNeg.literal()))
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_FMINABS, floatNeg, floatVal}).getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_FMINABS, floatPos, floatVal}).getConvergenceLimit(floatNeg.literal()))
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_FMINABS, floatPos, floatNeg}).getConvergenceLimit())

    // fmaxabs
    TEST_ASSERT(!(Expression{OP_FMAXABS, floatVal, floatVal}).getConvergenceLimit())
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_FMAXABS, floatVal, floatNeg}).getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_FMAXABS, floatVal, floatPos}).getConvergenceLimit(floatNeg.literal()))
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_FMAXABS, floatNeg, floatVal}).getConvergenceLimit(floatPos.literal()))
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_FMAXABS, floatPos, floatVal}).getConvergenceLimit(floatNeg.literal()))
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_FMAXABS, floatPos, floatNeg}).getConvergenceLimit())

    // ftoi
    TEST_ASSERT(!(Expression{OP_FTOI, floatVal, NO_VALUE}).getConvergenceLimit())
    TEST_ASSERT_EQUALS(intPos, (Expression{OP_FTOI, floatPos, NO_VALUE}).getConvergenceLimit())
    // itof
    TEST_ASSERT(!(Expression{OP_ITOF, intVal, NO_VALUE}).getConvergenceLimit())
    TEST_ASSERT_EQUALS(floatPos, (Expression{OP_ITOF, intPos, NO_VALUE}).getConvergenceLimit())

    // add
    TEST_ASSERT(!expression(intVal + intVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intMax, expression(intVal + intVal)->getConvergenceLimit(intPos.literal()))
    TEST_ASSERT_EQUALS(intMin, expression(intVal + intVal)->getConvergenceLimit(intNeg.literal()))
    TEST_ASSERT_EQUALS(intMax, expression(intVal + intPos)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intMax, expression(intPos + intVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intMin, expression(intVal + intNeg)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intMin, expression(intNeg + intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intVal + INT_ZERO)->getConvergenceLimit())
    TEST_ASSERT(!expression(INT_ZERO + intVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intPos, expression(intVal + INT_ZERO)->getConvergenceLimit(intPos.literal()))
    TEST_ASSERT_EQUALS(intPos, expression(INT_ZERO + intVal)->getConvergenceLimit(intPos.literal()))
    TEST_ASSERT_EQUALS(46_val, expression(intPos + intPos)->getConvergenceLimit())

    // sub
    TEST_ASSERT_EQUALS(INT_ZERO, expression(intVal - intVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intMin, expression(intVal - intPos)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intMax, expression(intVal - intNeg)->getConvergenceLimit())
    TEST_ASSERT(!expression(intPos - intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intNeg - intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intVal - INT_ZERO)->getConvergenceLimit())
    TEST_ASSERT(!expression(INT_ZERO - intVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(46_val, expression(intPos - intNeg)->getConvergenceLimit())

    // shr
    TEST_ASSERT(!expression(as_unsigned{intVal} >> intVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(INT_ZERO, expression(as_unsigned{intVal} >> intPos)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(INT_ZERO, expression(as_unsigned{intVal} >> intVal)->getConvergenceLimit(INT_ZERO.literal()))
    TEST_ASSERT_EQUALS(5_val, expression(as_unsigned{intPos} >> 2_val)->getConvergenceLimit())

    // asr
    TEST_ASSERT(!expression(as_signed{intVal} >> intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(as_signed{intVal} >> intPos)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(INT_ZERO, expression(as_signed{intVal} >> intVal)->getConvergenceLimit(INT_ZERO.literal()))
    TEST_ASSERT_EQUALS(
        INT_MINUS_ONE, expression(as_signed{intVal} >> intVal)->getConvergenceLimit(INT_MINUS_ONE.literal()))
    // XXX actually converges to -1 for all values <= 0x8000001F
    TEST_ASSERT_EQUALS(5_val, expression(as_signed{intPos} >> 2_val)->getConvergenceLimit())

    // ror
    TEST_ASSERT(!(Expression{OP_ROR, intVal, intVal}).getConvergenceLimit())
    TEST_ASSERT(!(Expression{OP_ROR, intPos, intVal}).getConvergenceLimit())
    TEST_ASSERT(!(Expression{OP_ROR, intVal, intPos}).getConvergenceLimit())
    TEST_ASSERT_EQUALS(23_val, (Expression{OP_ROR, 1472_val, 6_val}).getConvergenceLimit())

    // shl
    TEST_ASSERT(!expression(intVal << intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intPos << intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intVal << intPos)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intVal, expression(intVal << INT_ZERO)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(INT_ZERO, expression(INT_ZERO << intVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(1472_val, expression(23_val << 6_val)->getConvergenceLimit())

    // min
    TEST_ASSERT(!expression(min(intVal, intVal))->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intNeg, expression(min(intVal, intPos))->getConvergenceLimit(intNeg.literal()))
    TEST_ASSERT_EQUALS(intNeg, expression(min(intVal, intNeg))->getConvergenceLimit(intPos.literal()))
    TEST_ASSERT_EQUALS(intNeg, expression(min(intPos, intVal))->getConvergenceLimit(intNeg.literal()))
    TEST_ASSERT_EQUALS(intNeg, expression(min(intNeg, intVal))->getConvergenceLimit(intPos.literal()))
    TEST_ASSERT(!expression(min(intNeg, intVal))->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intNeg, expression(min(intNeg, intPos))->getConvergenceLimit())

    // max
    TEST_ASSERT(!expression(max(intVal, intVal))->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intPos, expression(max(intVal, intNeg))->getConvergenceLimit(intPos.literal()))
    TEST_ASSERT_EQUALS(intPos, expression(max(intVal, intPos))->getConvergenceLimit(intNeg.literal()))
    TEST_ASSERT_EQUALS(intPos, expression(max(intNeg, intVal))->getConvergenceLimit(intPos.literal()))
    TEST_ASSERT_EQUALS(intPos, expression(max(intPos, intVal))->getConvergenceLimit(intNeg.literal()))
    TEST_ASSERT(!expression(max(intNeg, intVal))->getConvergenceLimit())
    TEST_ASSERT_EQUALS(intPos, expression(max(intNeg, intPos))->getConvergenceLimit())

    // and
    TEST_ASSERT(!expression(intVal & intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intPos & intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intVal & intPos)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(INT_ONE, expression(intPos & intNeg)->getConvergenceLimit())

    // or
    TEST_ASSERT(!expression(intVal | intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intPos | intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intVal | intPos)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(INT_MINUS_ONE, expression(intPos | intNeg)->getConvergenceLimit())

    // xor
    TEST_ASSERT(!expression(intVal ^ intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intPos ^ intVal)->getConvergenceLimit())
    TEST_ASSERT(!expression(intVal ^ intPos)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(Value(Literal(-2), TYPE_INT32), expression(intPos ^ intNeg)->getConvergenceLimit())

    // not
    TEST_ASSERT(!expression(~intVal)->getConvergenceLimit())
    TEST_ASSERT_EQUALS(INT_ZERO, expression(~INT_MINUS_ONE)->getConvergenceLimit())

    // clz
    TEST_ASSERT(!(Expression{OP_CLZ, intVal, NO_VALUE}).getConvergenceLimit())
    TEST_ASSERT_EQUALS(32_val, (Expression{OP_CLZ, 0_val, NO_VALUE}).getConvergenceLimit())

    // v8adds
    TEST_ASSERT(!(Expression{OP_V8ADDS, intVal, intVal}).getConvergenceLimit())
    TEST_ASSERT(!(Expression{OP_V8ADDS, intPos, intVal}).getConvergenceLimit())
    TEST_ASSERT(!(Expression{OP_V8ADDS, intVal, intPos}).getConvergenceLimit())
    // XXX actually, may converge to per-element all bits set (for each element where a bit is set)
    // v8subs
    TEST_ASSERT(!(Expression{OP_V8SUBS, intVal, intVal}).getConvergenceLimit())
    TEST_ASSERT(!(Expression{OP_V8SUBS, intPos, intVal}).getConvergenceLimit())
    TEST_ASSERT(!(Expression{OP_V8SUBS, intVal, intPos}).getConvergenceLimit())
    // XXX actually, may converge to per-element zero (for each element where a bit is set)

    // fmul
    // TODO converges for |value| </> 1 and value </==/> 0 in different directions
    TEST_ASSERT_EQUALS(46.0_val, expression(floatPos * 2.0_val)->getConvergenceLimit())
    // mul24
    TEST_ASSERT(!(expression(mul24(intVal, intVal)))->getConvergenceLimit())
    TEST_ASSERT_EQUALS(46_val, expression(intPos * 2_val)->getConvergenceLimit())
    // XXX
    // V8muld
    // V8min
    // V8max

    // fake umul
    TEST_ASSERT_EQUALS(INT_ZERO, (Expression{Expression::FAKEOP_UMUL, INT_ZERO, intVal}).getConvergenceLimit())
    TEST_ASSERT_EQUALS(INT_ZERO, (Expression{Expression::FAKEOP_UMUL, intVal, INT_ZERO}).getConvergenceLimit())
    TEST_ASSERT_EQUALS(
        INT_ZERO, (Expression{Expression::FAKEOP_UMUL, intVal, intVal}).getConvergenceLimit(INT_ZERO.literal()))
    TEST_ASSERT_EQUALS(46_val, (Expression{Expression::FAKEOP_UMUL, 23_val, 2_val}).getConvergenceLimit())
    TEST_ASSERT_EQUALS(529_val, (Expression{Expression::FAKEOP_UMUL, intVal, intVal}).getConvergenceLimit(23_lit))
    TEST_ASSERT_EQUALS(
        INT_MINUS_ONE /* UINT_MAX */, (Expression{Expression::FAKEOP_UMUL, intVal, intVal}).getConvergenceLimit())
}

void TestExpressions::testValueRange()
{
    using namespace analysis;
    Configuration config{};
    Module mod{config};
    Method method(mod);
    FastMap<const Local*, std::shared_ptr<Expression>> expressions;
    auto options = add_flag(ExpressionOptions::ALLOW_FAKE_OPS, ExpressionOptions::STOP_AT_BUILTINS);

    auto lid = method.addNewLocal(TYPE_INT32);
    expressions.emplace(lid.local(),
        expression((method.addNewLocal(TYPE_INT32) + 4_val, intermediate::InstructionDecorations::BUILTIN_LOCAL_ID)));
    auto gid = method.addNewLocal(TYPE_INT32);
    expressions.emplace(gid.local(),
        expression((method.addNewLocal(TYPE_INT32) + 4_val, intermediate::InstructionDecorations::BUILTIN_GLOBAL_ID)));

    // (lid << 2) -lid -> lid * 3 -> [0, 33]
    {
        auto inner = method.addNewLocal(TYPE_INT32);
        expressions.emplace(inner.local(), expression(lid << 2_val)->combineWith(expressions, options));
        auto expr = expression(inner - lid)->combineWith(expressions, options);
        auto range = ValueRange::getValueRange(*expr);
        TEST_ASSERT(!!range)
        TEST_ASSERT_EQUALS(ValueRange(0.0, 33.0), range)
    }

    // constant >> local -> [0, constant]
    {
        auto loc = method.addNewLocal(TYPE_INT16);
        auto expr = expression(as_unsigned{42_val} >> loc);
        auto range = ValueRange::getValueRange(*expr);
        TEST_ASSERT(!!range)
        TEST_ASSERT_EQUALS(ValueRange(0.0, 42.0), range)
    }

    // unsigned local >> constant -> [0, UINT_MAX >> constant]
    {
        auto loc = method.addNewLocal(TYPE_INT16);
        auto inner = method.addNewLocal(TYPE_INT32);
        expressions.emplace(
            inner.local(), expression((loc << 2_val, intermediate::InstructionDecorations::UNSIGNED_RESULT)));
        auto expr = expression(as_unsigned{inner} >> 17_val)->combineWith(expressions, options);
        auto range = ValueRange::getValueRange(*expr);
        TEST_ASSERT(!!range)
        TEST_ASSERT_EQUALS(ValueRange(0.0, 32767.0), range)
    }

    // lid + constant -> [constant, constant + 11]
    {
        auto expr = expression(lid + 42_val)->combineWith(expressions, options);
        auto range = ValueRange::getValueRange(*expr);
        TEST_ASSERT(!!range)
        TEST_ASSERT_EQUALS(ValueRange(42.0, 53.0), range)
    }

    // min(gid, constant) -> [0, constant]
    {
        auto expr = expression(min(as_signed{gid}, as_signed{17_val}))->combineWith(expressions, options);
        auto range = ValueRange::getValueRange(*expr);
        TEST_ASSERT(!!range)
        TEST_ASSERT_EQUALS(ValueRange(0.0, 17.0), range)
    }

    // itof(lid) * constant -> [0.0, 11.0 * constant]
    {
        auto conv = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(
            conv.local(), std::make_shared<Expression>(OP_ITOF, lid, lid)->combineWith(expressions, options));
        auto expr = expression(conv * Value(Literal(-15.0f), TYPE_FLOAT))->combineWith(expressions, options);
        auto range = ValueRange::getValueRange(*expr);
        TEST_ASSERT(!!range)
        TEST_ASSERT_EQUALS(ValueRange(-165.0, 0.0), range)
    }

    // fmax(itof(lid), neg constant) -> [0, 11]
    {
        auto conv = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(
            conv.local(), std::make_shared<Expression>(OP_ITOF, lid, lid)->combineWith(expressions, options));
        auto expr = expression(max(as_float{conv}, as_float{Value(Literal(-42.0f), TYPE_FLOAT)}))
                        ->combineWith(expressions, options);
        auto range = ValueRange::getValueRange(*expr);
        TEST_ASSERT(!!range)
        TEST_ASSERT_EQUALS(ValueRange(0.0, 11.0), range)
    }

    // fmaxabs(itof(lid), neg constant) -> [|constant|, |constant|]
    {
        auto conv = method.addNewLocal(TYPE_FLOAT);
        expressions.emplace(
            conv.local(), std::make_shared<Expression>(OP_ITOF, lid, lid)->combineWith(expressions, options));
        auto expr = std::make_shared<Expression>(OP_FMAXABS, conv, Value(Literal(-42.0f), TYPE_FLOAT))
                        ->combineWith(expressions, options);
        auto range = ValueRange::getValueRange(*expr);
        TEST_ASSERT(!!range)
        TEST_ASSERT_EQUALS(ValueRange(42.0, 42.0), range)
    }
}
