/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestOptimizationSteps.h"

#include "Expression.h"
#include "Method.h"
#include "Module.h"
#include "intermediate/Helper.h"
#include "intermediate/operators.h"
#include "optimization/Combiner.h"
#include "optimization/ControlFlow.cpp"
#include "optimization/ControlFlow.h"
#include "optimization/Eliminator.h"
#include "optimization/Flags.h"
#include "optimization/Vector.h"

#include <cmath>

using namespace vc4c;
using namespace vc4c::optimizations;
using namespace vc4c::operators;

TestOptimizationSteps::TestOptimizationSteps()
{
    TEST_ADD(TestOptimizationSteps::testCombineSelectionWithZero);
    TEST_ADD(TestOptimizationSteps::testCombineSettingSameFlags);
    TEST_ADD(TestOptimizationSteps::testCombineSettingFlagsWithOutput);
    TEST_ADD(TestOptimizationSteps::testFoldConstants);
    TEST_ADD(TestOptimizationSteps::testSimplifyArithmetics);
    TEST_ADD(TestOptimizationSteps::testCombineArithmetics);
    TEST_ADD(TestOptimizationSteps::testRewriteConstantSFU);
    TEST_ADD(TestOptimizationSteps::testReorderBasicBlocks);
    TEST_ADD(TestOptimizationSteps::testSimplifyBranches);
    TEST_ADD(TestOptimizationSteps::testMergeBasicBlocks);
    TEST_ADD(TestOptimizationSteps::testCombineConstantLoads);
    TEST_ADD(TestOptimizationSteps::testEliminateBitOperations);
    TEST_ADD(TestOptimizationSteps::testCombineRotations);
    TEST_ADD(TestOptimizationSteps::testEliminateMoves);
    TEST_ADD(TestOptimizationSteps::testRemoveFlags);
    TEST_ADD(TestOptimizationSteps::testRemoveConditionalFlags);
    TEST_ADD(TestOptimizationSteps::testCombineVectorElementCopies);
    TEST_ADD(TestOptimizationSteps::testLoopInvariantCodeMotion);
}

static bool checkEquals(
    const intermediate::IntermediateInstruction* one, const intermediate::IntermediateInstruction* other)
{
    if(one == other)
        return true;
    if(!one || !other)
        return false;
    return *one == *other;
}

void TestOptimizationSteps::testMethodsEquals(vc4c::Method& m1, vc4c::Method& m2)
{
    auto inIt = m1.walkAllInstructions();
    auto outIt = m2.walkAllInstructions();

    while(!inIt.isEndOfMethod() && !outIt.isEndOfMethod())
    {
        if(!checkEquals(inIt.get(), outIt.get()))
        {
            TEST_ASSERT_EQUALS(outIt.has() ? outIt->to_string() : "(null)", inIt.has() ? inIt->to_string() : "(null)")
        }
        inIt.nextInMethod();
        outIt.nextInMethod();
    }
    while(!inIt.isEndOfMethod())
    {
        TEST_ASSERT_EQUALS("(no more input)", inIt.has() ? inIt->to_string() : "(null)")
        inIt.nextInMethod();
    }
    while(!outIt.isEndOfMethod())
    {
        TEST_ASSERT_EQUALS("(no more output)", outIt.has() ? outIt->to_string() : "(null)")
        outIt.nextInMethod();
    }
}

void TestOptimizationSteps::testCombineSelectionWithZero()
{
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.begin(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.begin(), "%dummy").walkEnd();

    // %in = uniform
    auto in = assign(inIt, TYPE_INT32, "%in") = UNIFORM_REGISTER;
    assign(outIt, in) = UNIFORM_REGISTER;

    // positive tests -> changes
    {
        // - = %in xor 5 (setf)
        auto cond = assignNop(inIt) = as_signed{in} == as_signed{5_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{in} == as_signed{5_val});
        // %a = %in (ifz)
        auto a = assign(inIt, TYPE_INT32, "%a") = (in, cond);
        assign(outIt, a) = (in, cond);
        // %a = 0 (ifzc) -> %a = %in xor %in (ifzc)
        assign(inIt, a) = (0_val, cond.invert());
        assign(outIt, a) = (in ^ in, cond.invert());
    }

    {
        // - = %in xor 6 (setf)
        auto cond = assignNop(inIt) = as_signed{in} == as_signed{6_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{in} == as_signed{6_val});
        // %b = 0 (ifz) -> %b = %in xor %in (ifz)
        auto b = assign(inIt, TYPE_INT32, "%b") = (0_val, cond);
        assign(outIt, b) = (in ^ in, cond);
        // %b = 42 (ifzc)
        assign(inIt, b) = (in, cond.invert());
        assign(outIt, b) = (in, cond.invert());
    }

    {
        // - = min(%in, 7) (setf)
        auto cond = assignNop(inIt) = as_signed{in} < as_signed{6_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{in} < as_signed{6_val});
        // %c = %in (ifcs)
        auto c = assign(inIt, TYPE_INT32, "%c") = (in, cond);
        assign(outIt, c) = (in, cond);
        // %a = 0 (ifcc) -> %a = %in xor %in (ifzc)
        assign(inIt, c) = (0_val, cond.invert());
        assign(outIt, c) = (in ^ in, cond.invert());
    }

    {
        // - = min(%in, 9) (setf)
        auto cond = assignNop(inIt) = as_signed{in} < as_signed{9_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{in} < as_signed{9_val});
        // %d = 0 (ifcs) -> %d = %in xor %in (ifz)
        auto d = assign(inIt, TYPE_INT32, "%d") = (0_val, cond);
        assign(outIt, d) = (in ^ in, cond);
        // %d = %in (ifcc)
        assign(inIt, d) = (in, cond.invert());
        assign(outIt, d) = (in, cond.invert());
    }

    {
        // - = min(%in, 9) (setf)
        auto cond = assignNop(inIt) = as_signed{in} < as_signed{9_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{in} < as_signed{9_val});
        // %e = 0 (ifcs) -> %d = 0 xor 0 (ifz)
        auto e = assign(inIt, TYPE_INT32, "%e") = (0_val, cond);
        assign(outIt, e) = (0_val, cond);
        // %e = 0 (ifcc)
        assign(inIt, e) = (0_val, cond.invert());
        assign(outIt, e) = (0_val ^ 0_val, cond.invert());
    }

    // negative tests -> no changes
    {
        // check conditional write of value
        auto src = assign(inIt, TYPE_INT32, "%src") = (in, COND_NEGATIVE_CLEAR);
        assign(outIt, src) = (in, COND_NEGATIVE_CLEAR);
        // - = %in xor 5 (setf)
        auto cond = assignNop(inIt) = as_signed{in} == as_signed{5_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{in} == as_signed{5_val});
        // %f = %src (ifz)
        auto f = assign(inIt, TYPE_INT32, "%f") = (src, cond);
        assign(outIt, f) = (src, cond);
        // %f = 0 (ifzc)
        assign(inIt, f) = (0_val, cond.invert());
        assign(outIt, f) = (0_val, cond.invert());
    }

    {
        // check side-effects
        // - = %in xor 5 (setf)
        auto cond = assignNop(inIt) = as_signed{in} == as_signed{5_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{in} == as_signed{5_val});
        // vpm = %src (ifz)
        Value vpm(REG_VPM_IO, TYPE_INT32);
        assign(inIt, vpm) = (in, cond);
        assign(outIt, vpm) = (in, cond);
        // vpm = 0 (ifzc)
        assign(inIt, vpm) = (0_val, cond.invert());
        assign(outIt, vpm) = (0_val, cond.invert());
    }

    // run step for all instructions
    auto it = inputMethod.walkAllInstructions();
    while(!it.isEndOfBlock())
    {
        it = combineSelectionWithZero(module, inputMethod, it, config);
        it.nextInBlock();
    }
    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testCombineSettingSameFlags()
{
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.begin(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.begin(), "%dummy").walkEnd();

    // %in = uniform
    auto in = assign(inIt, TYPE_INT32, "%in") = UNIFORM_REGISTER;
    assign(outIt, in) = UNIFORM_REGISTER;

    // positive tests -> flags are combined
    {
        auto cond = assignNop(inIt) = as_signed{in} == as_signed{0_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{in} == as_signed{0_val});
        auto a = assign(inIt, TYPE_INT32, "%a") = (17_val, cond);
        assign(inIt, a) = (42_val, cond.invert());
        assign(outIt, a) = (17_val, cond);
        assign(outIt, a) = (42_val, cond.invert());

        cond = assignNop(inIt) = as_signed{in} == as_signed{0_val};
        auto b = assign(inIt, TYPE_FLOAT, "%b") = (5.0_val, cond);
        assign(outIt, b) = (5.0_val, cond);
        cond = assignNop(inIt) = as_signed{in} == as_signed{0_val};
        cond = assignNop(inIt) = as_signed{in} == as_signed{0_val};
        auto c = assign(inIt, TYPE_HALF, "%c") = (2_val, cond.invert());
        assign(inIt, c) = (13_val, cond);
        assign(outIt, c) = (2_val, cond.invert());
        assign(outIt, c) = (13_val, cond);
    }

    // negative tests -> flags are not combined
    {
        // check handling of other flag set in between
        auto tmp = assign(inIt, TYPE_INT32) = in + 3_val;
        assign(outIt, tmp) = in + 3_val;
        auto cond = assignNop(inIt) = as_signed{tmp} == as_signed{0_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{tmp} == as_signed{0_val});
        auto c = assign(inIt, TYPE_FLOAT, "%c") = (7.0_val, cond);
        assign(outIt, c) = (7.0_val, cond);
        assignNop(inIt) = (17_val, SetFlag::SET_FLAGS);
        assignNop(outIt) = (17_val, SetFlag::SET_FLAGS);
        cond = assignNop(inIt) = as_signed{tmp} == as_signed{0_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{tmp} == as_signed{0_val});
        auto d = assign(inIt, TYPE_HALF, "%d") = (2_val, cond.invert());
        assign(inIt, d) = (13_val, cond);
        assign(outIt, d) = (2_val, cond.invert());
        assign(outIt, d) = (13_val, cond);
    }

    {
        // check handling when writing into non-op
        auto tmp = assign(inIt, TYPE_INT32) = in + 5_val;
        assign(outIt, tmp) = in + 5_val;
        auto cond = assignNop(inIt) = as_signed{tmp} == as_signed{0_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{tmp} == as_signed{0_val});
        auto e = assign(inIt, TYPE_FLOAT, "%e") = (7.0_val, cond);
        assign(outIt, e) = (7.0_val, cond);
        cond = assign(inIt, e) = as_signed{tmp} == as_signed{0_val};
        ignoreReturnValue(assign(outIt, e) = as_signed{tmp} == as_signed{0_val});
        auto f = assign(inIt, TYPE_HALF, "%f") = (2_val, cond.invert());
        assign(inIt, f) = (13_val, cond);
        assign(outIt, f) = (2_val, cond.invert());
        assign(outIt, f) = (13_val, cond);
    }

    {
        // check handling when using non-moves to set flags
        auto tmp = assign(inIt, TYPE_INT32) = in + 6_val;
        assign(outIt, tmp) = in + 6_val;
        auto cond = assignNop(inIt) = as_signed{tmp} == as_signed{7_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{tmp} == as_signed{7_val});
        auto c = assign(inIt, TYPE_FLOAT, "%c") = (7.0_val, cond);
        assign(outIt, c) = (7.0_val, cond);
        cond = assignNop(inIt) = as_signed{tmp} == as_signed{7_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{tmp} == as_signed{7_val});
        auto d = assign(inIt, TYPE_HALF, "%d") = (2_val, cond.invert());
        assign(inIt, d) = (13_val, cond);
        assign(outIt, d) = (2_val, cond.invert());
        assign(outIt, d) = (13_val, cond);
    }

    {
        // check handling when using flag setters with other side-effects
        auto tmp = assign(inIt, TYPE_INT32) = in + 6_val;
        assign(outIt, tmp) = in + 6_val;
        auto cond = assignNop(inIt) = as_signed{tmp} == as_signed{0_val};
        ignoreReturnValue(assignNop(outIt) = as_signed{tmp} == as_signed{0_val});
        auto c = assign(inIt, TYPE_FLOAT, "%c") = (7.0_val, cond);
        assign(outIt, c) = (7.0_val, cond);
        assignNop(inIt) = (tmp ^ 0_val, SetFlag::SET_FLAGS, SIGNAL_THREAD_SWITCH_LAST);
        assignNop(outIt) = (tmp ^ 0_val, SetFlag::SET_FLAGS, SIGNAL_THREAD_SWITCH_LAST);
        auto d = assign(inIt, TYPE_HALF, "%d") = (2_val, cond.invert());
        assign(inIt, d) = (13_val, cond);
        assign(outIt, d) = (2_val, cond.invert());
        assign(outIt, d) = (13_val, cond);
    }

    // run step for all instructions
    auto it = inputMethod.walkAllInstructions();
    while(!it.isEndOfBlock())
    {
        it = combineSameFlags(module, inputMethod, it, config);
        it.nextInBlock();
    }
    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testCombineSettingFlagsWithOutput()
{
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.begin(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.begin(), "%dummy").walkEnd();

    // %in = uniform
    auto in = assign(inIt, TYPE_INT32, "%in") = UNIFORM_REGISTER;
    assign(outIt, in) = UNIFORM_REGISTER;

    // positive tests -> instructions are combined
    {
        // write before flag (write -> read -> flag -> cond)
        // => write + flag -> read -> cond
        auto a = inputMethod.addNewLocal(TYPE_INT32, "%a");
        auto b = assign(inIt, TYPE_INT32, "%b") = in + 1_val;
        assign(inIt, a) = b;
        auto some = assign(inIt, TYPE_INT32) = max(a, in);
        assignNop(inIt) = (b, SetFlag::SET_FLAGS);
        auto other = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);

        assign(outIt, b) = in + 1_val;
        assign(outIt, a) = (b, SetFlag::SET_FLAGS);
        assign(outIt, some) = max(a, in);
        assign(outIt, other) = (5.0_val, COND_ZERO_CLEAR);
    }

    {
        // flag before write (flag -> read -> write -> cond -> read)
        // => read -> write + flag -> cond -> read
        auto c = inputMethod.addNewLocal(TYPE_INT32, "%c");
        auto d = assign(inIt, TYPE_INT32, "%d") = in + 2_val;
        assignNop(inIt) = (d, SetFlag::SET_FLAGS);
        auto tmp = assign(inIt, TYPE_INT32) = c;
        assign(inIt, c) = d;
        auto some = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);
        auto other = assign(inIt, TYPE_INT32) = max(c, in);

        assign(outIt, d) = in + 2_val;
        assign(outIt, tmp) = c;
        assign(outIt, c) = (d, SetFlag::SET_FLAGS);
        assign(outIt, some) = (5.0_val, COND_ZERO_CLEAR);
        assign(outIt, other) = max(c, in);
    }

    // negative tests -> instructions are not combined
    {
        // flag before write (flag -> cond -> write -> read)
        // XXX could be combined to (write + flag -> cond -> read)
        auto e = inputMethod.addNewLocal(TYPE_INT32, "%e");
        auto f = assign(inIt, TYPE_INT32, "%f") = in + 3_val;
        assignNop(inIt) = (f, SetFlag::SET_FLAGS);
        auto some = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);
        assign(inIt, e) = f;
        auto other = assign(inIt, TYPE_INT32) = max(e, in);

        assign(outIt, f) = in + 3_val;
        assignNop(outIt) = (f, SetFlag::SET_FLAGS);
        assign(outIt, some) = (5.0_val, COND_ZERO_CLEAR);
        assign(outIt, e) = f;
        assign(outIt, other) = max(e, in);
    }

    {
        // flag write is non-move
        // XXX could be combined with improved optimization to (write + flag -> read -> cond)
        auto g = inputMethod.addNewLocal(TYPE_INT32, "%g");
        auto h = assign(inIt, TYPE_INT32, "%h") = in + 4_val;
        assign(inIt, g) = (h ^ in);
        auto some = assign(inIt, TYPE_INT32) = max(g, in);
        auto cond = assignNop(inIt) = (as_signed{h} == as_signed{12_val});
        auto other = assign(inIt, TYPE_FLOAT) = (5.0_val, cond);

        assign(outIt, h) = in + 4_val;
        assign(outIt, g) = (h ^ in);
        assign(outIt, some) = max(g, in);
        cond = assignNop(outIt) = (as_signed{h} == as_signed{12_val});
        assign(outIt, other) = (5.0_val, cond);
    }

    {
        // flag write already writes to output
        auto i = inputMethod.addNewLocal(TYPE_INT32, "%i");
        auto j = inputMethod.addNewLocal(TYPE_INT32, "%j");
        auto k = assign(inIt, TYPE_INT32, "%k") = in + 5_val;
        assign(inIt, i) = k;
        auto some = assign(inIt, TYPE_INT32) = max(i, in);
        assign(inIt, j) = (k, SetFlag::SET_FLAGS);
        auto other = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);

        assign(outIt, k) = in + 5_val;
        assign(outIt, i) = k;
        assign(outIt, some) = max(i, in);
        assign(outIt, j) = (k, SetFlag::SET_FLAGS);
        assign(outIt, other) = (5.0_val, COND_ZERO_CLEAR);
    }

    {
        // write before other Flag before flag (write -> read -> Flag -> flag -> cond)
        auto l = inputMethod.addNewLocal(TYPE_INT32, "%l");
        auto m = assign(inIt, TYPE_INT32, "%m") = in + 6_val;
        assign(inIt, l) = m;
        auto some = assign(inIt, TYPE_INT32) = max(l, in);
        assignNop(inIt) = (in, SetFlag::SET_FLAGS);
        assignNop(inIt) = (m, SetFlag::SET_FLAGS);
        auto other = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);

        assign(outIt, m) = in + 6_val;
        assign(outIt, l) = m;
        assign(outIt, some) = max(l, in);
        assignNop(outIt) = (in, SetFlag::SET_FLAGS);
        assignNop(outIt) = (m, SetFlag::SET_FLAGS);
        assign(outIt, other) = (5.0_val, COND_ZERO_CLEAR);
    }

    {
        // flag before Flag before write (flag -> cond -> Flag -> write -> read)
        // XXX could be combined to (write + flag -> cond -> Flag -> read)
        auto n = inputMethod.addNewLocal(TYPE_INT32, "%n");
        auto o = assign(inIt, TYPE_INT32, "%o") = in + 7_val;
        assignNop(inIt) = (o, SetFlag::SET_FLAGS);
        auto some = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);
        assignNop(inIt) = (in, SetFlag::SET_FLAGS);
        assign(inIt, n) = o;
        auto other = assign(inIt, TYPE_INT32) = max(n, in);

        assign(outIt, o) = in + 7_val;
        assignNop(outIt) = (o, SetFlag::SET_FLAGS);
        assign(outIt, some) = (5.0_val, COND_ZERO_CLEAR);
        assignNop(outIt) = (in, SetFlag::SET_FLAGS);
        assign(outIt, n) = o;
        assign(outIt, other) = max(n, in);
    }

    {
        // write before flag (write -> cond -> flag -> cond -> read)
        // XXX could be combined to (cond -> write + flag -> cond -> read)
        auto p = inputMethod.addNewLocal(TYPE_INT32, "%p");
        auto q = assign(inIt, TYPE_INT32, "%q") = in + 8_val;
        assign(inIt, p) = q;
        assignNop(inIt) = (4.0_val, COND_ZERO_CLEAR);
        assignNop(inIt) = (q, SetFlag::SET_FLAGS);
        auto some = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);
        auto other = assign(inIt, TYPE_INT32) = max(p, in);

        assign(outIt, q) = in + 8_val;
        assign(outIt, p) = q;
        assignNop(outIt) = (4.0_val, COND_ZERO_CLEAR);
        assignNop(outIt) = (q, SetFlag::SET_FLAGS);
        assign(outIt, some) = (5.0_val, COND_ZERO_CLEAR);
        assign(outIt, other) = max(p, in);
    }

    {
        // flag before read before write (flag -> cond -> read -> write -> read)
        auto r = inputMethod.addNewLocal(TYPE_INT32, "%r");
        auto s = assign(inIt, TYPE_INT32, "%s") = in + 9_val;
        assignNop(inIt) = (s, SetFlag::SET_FLAGS);
        auto some = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);
        assignNop(inIt) = min(r, in);
        assign(inIt, r) = s;
        auto other = assign(inIt, TYPE_INT32) = max(r, in);

        assign(outIt, s) = in + 9_val;
        assignNop(outIt) = (s, SetFlag::SET_FLAGS);
        assign(outIt, some) = (5.0_val, COND_ZERO_CLEAR);
        assignNop(outIt) = min(r, in);
        assign(outIt, r) = s;
        assign(outIt, other) = max(r, in);
    }

    {
        // write before cond before flag (write -> cond -> read -> flag -> cond)
        auto t = inputMethod.addNewLocal(TYPE_INT32, "%t");
        auto u = assign(inIt, TYPE_INT32, "%u") = in + 9_val;
        assign(inIt, t) = u;
        auto some = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);
        assignNop(inIt) = min(t, in);
        assignNop(inIt) = (u, SetFlag::SET_FLAGS);
        assignNop(inIt) = (4.0_val, COND_ZERO_CLEAR);

        assign(outIt, u) = in + 9_val;
        assign(outIt, t) = u;
        assign(outIt, some) = (5.0_val, COND_ZERO_CLEAR);
        assignNop(outIt) = min(t, in);
        assignNop(outIt) = (u, SetFlag::SET_FLAGS);
        assignNop(outIt) = (4.0_val, COND_ZERO_CLEAR);
    }

    {
        // Write of flag input in between write and flag (write -> read -> Write -> flag -> cond)
        auto v = inputMethod.addNewLocal(TYPE_INT32, "%v");
        auto w = assign(inIt, TYPE_INT32, "%w") = in + 10_val;
        assign(inIt, v) = w;
        auto some = assign(inIt, TYPE_INT32) = max(v, in);
        assign(inIt, w) = w + 1_val;
        assignNop(inIt) = (w, SetFlag::SET_FLAGS);
        auto other = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);

        assign(outIt, w) = in + 10_val;
        assign(outIt, v) = w;
        assign(outIt, some) = max(v, in);
        assign(outIt, w) = w + 1_val;
        assignNop(outIt) = (w, SetFlag::SET_FLAGS);
        assign(outIt, other) = (5.0_val, COND_ZERO_CLEAR);
    }

    {
        // Write of write input in between flag and write (flag -> read -> Write -> write -> cond -> read)
        // => read -> write + flag -> cond -> read
        auto x = inputMethod.addNewLocal(TYPE_INT32, "%x");
        auto y = assign(inIt, TYPE_INT32, "%y") = in + 11_val;
        assignNop(inIt) = (y, SetFlag::SET_FLAGS);
        auto tmp = assign(inIt, TYPE_INT32) = x;
        assign(inIt, y) = y + 1_val;
        assign(inIt, x) = y;
        auto some = assign(inIt, TYPE_FLOAT) = (5.0_val, COND_ZERO_CLEAR);
        auto other = assign(inIt, TYPE_INT32) = max(x, in);

        assign(outIt, y) = in + 11_val;
        assignNop(outIt) = (y, SetFlag::SET_FLAGS);
        assign(outIt, tmp) = x;
        assign(outIt, y) = y + 1_val;
        assign(outIt, x) = y;
        assign(outIt, some) = (5.0_val, COND_ZERO_CLEAR);
        assign(outIt, other) = max(x, in);
    }

    // TODO test negative:
    // either/both instructions have side-effects

    // run step for all instructions
    auto it = inputMethod.walkAllInstructions();
    while(!it.isEndOfBlock())
    {
        it = combineFlagWithOutput(module, inputMethod, it, config);
        it.nextInBlock();
    }
    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testFoldConstants()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.begin(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.begin(), "%dummy").walkEnd();

    // %in = uniform
    auto in = assign(inIt, TYPE_INT32, "%in") = UNIFORM_REGISTER;
    assign(outIt, in) = UNIFORM_REGISTER;

    // NOTE: Need to do manual creation of instructions to not trigger the precalculation in operators.h

    // simple case, "pure" arithmetic operation
    inIt.emplace(std::make_unique<Operation>(OP_ADD, NOP_REGISTER, 17_val, 5_val));
    inIt.nextInBlock();
    assignNop(outIt) = 22_val;

    // transfer signal
    inIt.emplace(std::make_unique<Operation>(OP_ADD, NOP_REGISTER, 17_val, 7_val)).setSignaling(SIGNAL_LOAD_ALPHA);
    inIt.nextInBlock();
    assignNop(outIt) = (24_val, SIGNAL_LOAD_ALPHA);

    // transfer conditional
    inIt.emplace(std::make_unique<Operation>(OP_ADD, NOP_REGISTER, 17_val, 8_val, COND_CARRY_CLEAR));
    inIt.nextInBlock();
    assignNop(outIt) = (25_val, COND_CARRY_CLEAR);

    // flags not folded (move cannot set carry flag)
    inIt.emplace(std::make_unique<Operation>(OP_ADD, NOP_REGISTER, 17_val, 4_val)).setSetFlags(SetFlag::SET_FLAGS);
    inIt.nextInBlock();
    assignNop(outIt) = (17_val + 4_val, SetFlag::SET_FLAGS);

    // transfer decorations
    inIt.emplace(std::make_unique<Operation>(OP_SUB, NOP_REGISTER, 17_val, 4_val));
    inIt->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
    inIt.nextInBlock();
    assignNop(outIt) = (13_val, InstructionDecorations::UNSIGNED_RESULT);

    // pack mode not folded (move can't 32-bit saturate, some other pack modes also rely on that)
    inIt.emplace(std::make_unique<Operation>(OP_SUB, NOP_REGISTER, 17_val, 17_val)).setPackMode(PACK_32_16A_S);
    inIt.nextInBlock();
    outIt.emplace(std::make_unique<Operation>(OP_SUB, NOP_REGISTER, 17_val, 17_val)).setPackMode(PACK_32_16A_S);
    outIt.nextInBlock();

    // non-constants not folded
    inIt.emplace(std::make_unique<Operation>(OP_ADD, NOP_REGISTER, 17_val, UNIFORM_REGISTER));
    inIt.nextInBlock();
    assignNop(outIt) = 17_val + UNIFORM_REGISTER;

    // unpack modes not folded (don't know which input to unpack)
    inIt.emplace(std::make_unique<Operation>(OP_ADD, NOP_REGISTER, 17_val, 11_val)).setUnpackMode(UNPACK_16A_32);
    inIt.nextInBlock();
    assignNop(outIt) = (17_val + 11_val, UNPACK_16A_32);

    // a xor a (cond) not folded (so it can be reused by different optimization
    inIt.emplace(std::make_unique<Operation>(OP_XOR, NOP_REGISTER, 11_val, 11_val, COND_NEGATIVE_CLEAR));
    inIt.nextInBlock();
    assignNop(outIt) = (11_val ^ 11_val, COND_NEGATIVE_CLEAR);

    // a xor a (always) folded
    inIt.emplace(std::make_unique<Operation>(OP_XOR, NOP_REGISTER, 17_val, 17_val));
    inIt.nextInBlock();
    assignNop(outIt) = 0_val;

    // run step for all instructions
    auto it = inputMethod.walkAllInstructions();
    while(!it.isEndOfBlock())
    {
        it = foldConstants(module, inputMethod, it, config);
        it.nextInBlock();
    }
    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testSimplifyArithmetics()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%dummy").walkEnd();

    // NOTE: Need to execute the optimization pass before adding the expected instructions, since the optimization pass
    // checks for single writer, which we violate by reusing the local across methods!

    auto inA = inputMethod.addNewLocal(TYPE_INT32, "%a");
    auto out0 = inputMethod.addNewLocal(TYPE_INT32, "%out0");
    auto out1 = inputMethod.addNewLocal(TYPE_INT32, "%out1");
    auto out2 = inputMethod.addNewLocal(TYPE_INT32, "%out2");
    auto out3 = inputMethod.addNewLocal(TYPE_INT32, "%out3");
    auto out4 = inputMethod.addNewLocal(TYPE_INT32, "%out4");

    // left absorbing (part 1)
    {
        inIt.emplace(std::make_unique<Operation>(OP_AND, out0, INT_ZERO, inA));
        inIt.nextInBlock();
    }

    // right absorbing (part 1)
    {
        inIt.emplace(std::make_unique<Operation>(OP_AND, out1, inA, INT_ZERO));
        inIt.nextInBlock();
    }

    // self inverse (part 1)
    {
        inIt.emplace(std::make_unique<Operation>(OP_SUB, out2, inA, inA));
        inIt.nextInBlock();
    }

    // write to self with right identity (is removed, so no part 2)
    {
        inIt.emplace(std::make_unique<Operation>(OP_ADD, out3, out3, INT_ZERO));
        inIt.nextInBlock();
    }

    // write to self with idempotent value (is removed, so no part 2)
    {
        inIt.emplace(std::make_unique<Operation>(OP_AND, out3, out3, out3));
        inIt.nextInBlock();
    }

    // write to self with left identity (is removed, so no part 2)
    {
        inIt.emplace(std::make_unique<Operation>(OP_ADD, out3, INT_ZERO, out3));
        inIt.nextInBlock();
    }

    // write to other with idempotent value (part 1)
    {
        inIt.emplace(std::make_unique<Operation>(OP_AND, out3, inA, inA));
        inIt.nextInBlock();
    }

    // xor with -1 (part 1)
    {
        inIt.emplace(std::make_unique<Operation>(OP_XOR, out4, INT_MINUS_ONE, inA));
        inIt.nextInBlock();
    }

    // run step for all instructions
    auto it = inputMethod.walkAllInstructions();
    while(!it.isEndOfBlock())
    {
        it = simplifyOperation(module, inputMethod, it, config);
        it.nextInBlock();
    }

    // left absorbing (part 2)
    {
        outIt.emplace(std::make_unique<MoveOperation>(out0, INT_ZERO));
        outIt.nextInBlock();
    }

    // right absorbing (part 2)
    {
        outIt.emplace(std::make_unique<MoveOperation>(out1, INT_ZERO));
        outIt.nextInBlock();
    }

    // self inverse (part 2)
    {
        outIt.emplace(std::make_unique<MoveOperation>(out2, INT_ZERO));
        outIt.nextInBlock();
    }

    // write to other with idempotent value (part 2)
    {
        outIt.emplace(std::make_unique<MoveOperation>(out3, inA));
        outIt.nextInBlock();
    }

    // xor with -1 (part 2)
    {
        outIt.emplace(std::make_unique<Operation>(OP_NOT, out4, inA));
        outIt.nextInBlock();
    }

    testMethodsEquals(inputMethod, outputMethod);
}

static const std::vector<OpCode> opCodes = {OP_ADD, OP_AND, OP_ASR, OP_CLZ, OP_FADD, OP_FMAX, OP_FMAXABS, OP_FMIN,
    OP_FMINABS, OP_FMUL, OP_FSUB, OP_FTOI, OP_ITOF, OP_MAX, OP_MIN, OP_MUL24, OP_NOP, OP_NOT, OP_OR, OP_ROR, OP_SHL,
    OP_SHR, OP_SUB, OP_V8ADDS, OP_V8MAX, OP_V8MIN, OP_V8MULD, OP_V8SUBS, OP_XOR};

void TestOptimizationSteps::testCombineArithmetics()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.begin(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.begin(), "%dummy").walkEnd();

    // %in = uniform
    auto in = assign(inIt, TYPE_INT32, "%in") = UNIFORM_REGISTER;
    assign(outIt, in) = UNIFORM_REGISTER;

    for(const auto& op : opCodes)
    {
        auto returnType = op.returnsFloat ? TYPE_FLOAT : TYPE_INT32;
        auto oneConst = op.acceptsFloat ? 15.0_val : 15_val;
        auto otherConst = op.acceptsFloat ? 3.0_val : 3_val;
        auto local = assign(inIt, op.acceptsFloat ? TYPE_FLOAT : TYPE_INT32, op.name) = in;
        assign(outIt, local) = in;

        if(op.numOperands == 2 && op.isAssociative())
        {
            // for all associative operations:
            // (a op constB) op constC -> a op (constB op constC)
            auto intermediate = inputMethod.addNewLocal(returnType, op.name);
            auto out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(std::make_unique<Operation>(op, intermediate, local, oneConst));
            inIt.nextInBlock();
            inIt.emplace(std::make_unique<Operation>(op, out, intermediate, otherConst));
            inIt.nextInBlock();
            outIt.emplace(std::make_unique<Operation>(op, out, local, op(oneConst, otherConst).first.value()));
            outIt.nextInBlock();

            // constA op (constB op c) -> (constA op constB) op c
            intermediate = inputMethod.addNewLocal(returnType, op.name);
            out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(std::make_unique<Operation>(op, intermediate, otherConst, local));
            inIt.nextInBlock();
            inIt.emplace(std::make_unique<Operation>(op, out, oneConst, intermediate));
            inIt.nextInBlock();
            if(op.isCommutative())
                outIt.emplace(std::make_unique<Operation>(op, out, local, op(oneConst, otherConst).first.value()));
            else
                outIt.emplace(std::make_unique<Operation>(op, out, op(oneConst, otherConst).first.value(), local));
            outIt.nextInBlock();

            if(op.isCommutative())
            {
                // for all additionally commutative operations:
                // (constA op b) op constC -> (constA op constC) op b
                intermediate = inputMethod.addNewLocal(returnType, op.name);
                out = inputMethod.addNewLocal(returnType, op.name);
                inIt.emplace(std::make_unique<Operation>(op, intermediate, oneConst, local));
                inIt.nextInBlock();
                inIt.emplace(std::make_unique<Operation>(op, out, intermediate, otherConst));
                inIt.nextInBlock();
                outIt.emplace(std::make_unique<Operation>(op, out, local, op(oneConst, otherConst).first.value()));
                outIt.nextInBlock();

                // constA op (b op constC) -> b op (constA op constC)
                intermediate = inputMethod.addNewLocal(returnType, op.name);
                out = inputMethod.addNewLocal(returnType, op.name);
                inIt.emplace(std::make_unique<Operation>(op, intermediate, local, otherConst));
                inIt.nextInBlock();
                inIt.emplace(std::make_unique<Operation>(op, out, oneConst, intermediate));
                inIt.nextInBlock();
                outIt.emplace(std::make_unique<Operation>(op, out, local, op(oneConst, otherConst).first.value()));
                outIt.nextInBlock();
            }
        }
        else if(op == OP_SHL || op == OP_SHR || op == OP_ROR || op == OP_ROR)
        {
            // for all shifts
            // (a op constB) op constC -> a op (constB + constC)
            auto intermediate = inputMethod.addNewLocal(returnType, op.name);
            auto out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(std::make_unique<Operation>(op, intermediate, local, oneConst));
            inIt.nextInBlock();
            inIt.emplace(std::make_unique<Operation>(op, out, intermediate, otherConst));
            inIt.nextInBlock();
            outIt.emplace(std::make_unique<Operation>(op, out, local, OP_ADD(oneConst, otherConst).first.value()));
            outIt.nextInBlock();

            // a op (constB op constC) -> unchanged
            intermediate = inputMethod.addNewLocal(returnType, op.name);
            out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(std::make_unique<Operation>(op, intermediate, oneConst, otherConst));
            inIt.nextInBlock();
            inIt.emplace(std::make_unique<Operation>(op, out, local, intermediate));
            inIt.nextInBlock();
            outIt.emplace(std::make_unique<Operation>(op, intermediate, oneConst, otherConst));
            outIt.nextInBlock();
            outIt.emplace(std::make_unique<Operation>(op, out, local, intermediate));
            outIt.nextInBlock();

            // (constA op b) op constC -> unchanged
            intermediate = inputMethod.addNewLocal(returnType, op.name);
            out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(std::make_unique<Operation>(op, intermediate, oneConst, local));
            inIt.nextInBlock();
            inIt.emplace(std::make_unique<Operation>(op, out, intermediate, otherConst));
            inIt.nextInBlock();
            outIt.emplace(std::make_unique<Operation>(op, intermediate, oneConst, local));
            outIt.nextInBlock();
            outIt.emplace(std::make_unique<Operation>(op, out, intermediate, otherConst));
            outIt.nextInBlock();
        }
        else if(op.numOperands == 2)
        {
            // any other operation -> not combined
            auto intermediate = inputMethod.addNewLocal(returnType, op.name);
            auto out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(std::make_unique<Operation>(op, intermediate, local, oneConst));
            inIt.nextInBlock();
            inIt.emplace(std::make_unique<Operation>(op, out, intermediate, otherConst));
            inIt.nextInBlock();
            outIt.emplace(std::make_unique<Operation>(op, intermediate, local, oneConst));
            outIt.nextInBlock();
            outIt.emplace(std::make_unique<Operation>(op, out, intermediate, otherConst));
            outIt.nextInBlock();
        }
    }

    auto local = assign(inIt, TYPE_INT32) = in;
    assign(outIt, local) = in;

    // no combination on conditional execution
    {
        // XXX could combine if both have same condition/associated flags
        auto intermediate = assign(inIt, TYPE_INT32, "%cond") = (local + 11_val, COND_NEGATIVE_CLEAR);
        auto out = assign(inIt, TYPE_INT32, "%cond") = intermediate + 17_val;
        assign(outIt, intermediate) = (local + 11_val, COND_NEGATIVE_CLEAR);
        assign(outIt, out) = intermediate + 17_val;

        // XXX can't this be combined anyway??
        intermediate = assign(inIt, TYPE_INT32, "%cond") = (local + 11_val);
        out = assign(inIt, TYPE_INT32, "%cond") = (intermediate + 17_val, COND_NEGATIVE_SET);
        assign(outIt, intermediate) = local + 11_val;
        assign(outIt, out) = (intermediate + 17_val, COND_NEGATIVE_SET);
    }

    // no combination on side-effects
    {
        auto intermediate = assign(inIt, TYPE_INT32, "%effect") = (local + 11_val, SIGNAL_LOAD_ALPHA);
        auto out = assign(inIt, TYPE_INT32, "%effect") = intermediate + 17_val;
        assign(outIt, intermediate) = (local + 11_val, SIGNAL_LOAD_ALPHA);
        assign(outIt, out) = intermediate + 17_val;

        // XXX this particular case (pack at the end, same for setting of flags) could be combined
        intermediate = assign(inIt, TYPE_INT32, "%effect") = (local + 11_val);
        out = assign(inIt, TYPE_INT32, "%effect") = (intermediate + 17_val, PACK_32_16A_S);
        assign(outIt, intermediate) = local + 11_val;
        assign(outIt, out) = (intermediate + 17_val, PACK_32_16A_S);
    }

    // no combination if intermediate result is used otherwise
    {
        auto intermediate = assign(inIt, TYPE_INT32, "%used") = local + 11_val;
        assignNop(inIt) = intermediate + 16_val;
        auto out = assign(inIt, TYPE_INT32, "%used") = intermediate + 17_val;
        assign(outIt, intermediate) = local + 11_val;
        assignNop(outIt) = intermediate + 16_val;
        assign(outIt, out) = intermediate + 17_val;
    }

    // check transfer of additional fields (which are allowed, e.g. decorations)
    {
        auto intermediate = assign(inIt, TYPE_INT32, "%deco") = local + 11_val;
        auto out = assign(inIt, TYPE_INT32, "%deco") = (intermediate + 17_val, InstructionDecorations::UNSIGNED_RESULT);
        assign(outIt, out) = (local + 28_val, InstructionDecorations::UNSIGNED_RESULT);
    }

    // run step for all instructions
    auto it = inputMethod.walkAllInstructions();
    while(!it.isEndOfBlock())
    {
        it = combineArithmeticOperations(module, inputMethod, it, config);
        it.nextInBlock();
    }
    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testRewriteConstantSFU()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.begin(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.begin(), "%dummy").walkEnd();

    // check rewriting of constant SFU call (+transfer of additional fields)
    {
        assign(inIt, Value(REG_SFU_EXP2, TYPE_FLOAT)) = 5.0_val;
        nop(inIt, DelayType::WAIT_SFU);
        nop(inIt, DelayType::WAIT_SFU);
        auto outExp = assign(inIt, TYPE_FLOAT) =
            (Value(REG_SFU_OUT, TYPE_FLOAT), InstructionDecorations::UNSIGNED_RESULT);
        assign(inIt, Value(REG_SFU_LOG2, TYPE_FLOAT)) = 5.0_val;
        nop(inIt, DelayType::WAIT_SFU);
        nop(inIt, DelayType::WAIT_SFU);
        auto outLog = assign(inIt, TYPE_FLOAT) = (Value(REG_SFU_OUT, TYPE_FLOAT), SetFlag::SET_FLAGS);
        assign(inIt, Value(REG_SFU_RECIP, TYPE_FLOAT)) = 5.0_val;
        nop(inIt, DelayType::WAIT_SFU);
        nop(inIt, DelayType::WAIT_SFU);
        auto outRecip = assign(inIt, TYPE_FLOAT) = (Value(REG_SFU_OUT, TYPE_FLOAT), PACK_32_16B_S);
        assign(inIt, Value(REG_SFU_RECIP_SQRT, TYPE_FLOAT)) = 5.0_val;
        nop(inIt, DelayType::WAIT_SFU);
        nop(inIt, DelayType::WAIT_SFU);
        auto outRSqrt = assign(inIt, TYPE_FLOAT) = (Value(REG_SFU_OUT, TYPE_FLOAT), SIGNAL_LOAD_COLOR, COND_CARRY_SET);

        assign(outIt, outExp) = (Value(Literal(exp2f(5.0f)), TYPE_FLOAT), InstructionDecorations::UNSIGNED_RESULT);
        assign(outIt, outLog) = (Value(Literal(log2f(5.0f)), TYPE_FLOAT), SetFlag::SET_FLAGS);
        assign(outIt, outRecip) = (Value(Literal(1.0f / 5.0f), TYPE_FLOAT), PACK_32_16B_S);
        assign(outIt, outRSqrt) = (Value(Literal(1.0f / sqrtf(5.0f)), TYPE_FLOAT), SIGNAL_LOAD_COLOR, COND_CARRY_SET);
    }

    // check not rewriting non-constant SFU call
    {
        auto in = assign(inIt, TYPE_INT32, "%in") = UNIFORM_REGISTER;
        assign(inIt, Value(REG_SFU_EXP2, TYPE_FLOAT)) = in;
        nop(inIt, DelayType::WAIT_SFU);
        nop(inIt, DelayType::WAIT_SFU);
        auto outExp = assign(inIt, TYPE_FLOAT) = Value(REG_SFU_OUT, TYPE_FLOAT);

        assign(outIt, in) = UNIFORM_REGISTER;
        assign(outIt, Value(REG_SFU_EXP2, TYPE_FLOAT)) = in;
        nop(outIt, DelayType::WAIT_SFU);
        nop(outIt, DelayType::WAIT_SFU);
        assign(outIt, outExp) = Value(REG_SFU_OUT, TYPE_FLOAT);
    }

    // check not rewriting if write is conditional
    {
        assign(inIt, Value(REG_SFU_EXP2, TYPE_FLOAT)) = (7_val, COND_CARRY_SET);
        nop(inIt, DelayType::WAIT_SFU);
        nop(inIt, DelayType::WAIT_SFU);
        auto outExp = assign(inIt, TYPE_FLOAT) = Value(REG_SFU_OUT, TYPE_FLOAT);

        assign(outIt, Value(REG_SFU_EXP2, TYPE_FLOAT)) = (7_val, COND_CARRY_SET);
        nop(outIt, DelayType::WAIT_SFU);
        nop(outIt, DelayType::WAIT_SFU);
        assign(outIt, outExp) = Value(REG_SFU_OUT, TYPE_FLOAT);
    }

    // check not rewriting if source has other side-effects/pack/unpack-modes
    {
        assign(inIt, Value(REG_SFU_EXP2, TYPE_FLOAT)) = (7_val, SIGNAL_LOAD_COLOR);
        nop(inIt, DelayType::WAIT_SFU);
        nop(inIt, DelayType::WAIT_SFU);
        auto outExp = assign(inIt, TYPE_FLOAT) = Value(REG_SFU_OUT, TYPE_FLOAT);

        assign(outIt, Value(REG_SFU_EXP2, TYPE_FLOAT)) = (7_val, SIGNAL_LOAD_COLOR);
        nop(outIt, DelayType::WAIT_SFU);
        nop(outIt, DelayType::WAIT_SFU);
        assign(outIt, outExp) = Value(REG_SFU_OUT, TYPE_FLOAT);

        assign(inIt, Value(REG_SFU_LOG2, TYPE_FLOAT)) = (7_val, SetFlag::SET_FLAGS);
        nop(inIt, DelayType::WAIT_SFU);
        nop(inIt, DelayType::WAIT_SFU);
        auto outLog = assign(inIt, TYPE_FLOAT) = Value(REG_SFU_OUT, TYPE_FLOAT);

        assign(outIt, Value(REG_SFU_LOG2, TYPE_FLOAT)) = (7_val, SetFlag::SET_FLAGS);
        nop(outIt, DelayType::WAIT_SFU);
        nop(outIt, DelayType::WAIT_SFU);
        assign(outIt, outLog) = Value(REG_SFU_OUT, TYPE_FLOAT);

        // XXX (un)pack modes could be accepted, if we use them for calculation
        assign(inIt, Value(REG_SFU_RECIP, TYPE_FLOAT)) = (7_val, UNPACK_16B_32);
        nop(inIt, DelayType::WAIT_SFU);
        nop(inIt, DelayType::WAIT_SFU);
        auto outRecip = assign(inIt, TYPE_FLOAT) = Value(REG_SFU_OUT, TYPE_FLOAT);

        assign(outIt, Value(REG_SFU_RECIP, TYPE_FLOAT)) = (7_val, UNPACK_16B_32);
        nop(outIt, DelayType::WAIT_SFU);
        nop(outIt, DelayType::WAIT_SFU);
        assign(outIt, outRecip) = Value(REG_SFU_OUT, TYPE_FLOAT);

        assign(inIt, Value(REG_SFU_RECIP_SQRT, TYPE_FLOAT)) = (7_val, PACK_32_16B_S);
        nop(inIt, DelayType::WAIT_SFU);
        nop(inIt, DelayType::WAIT_SFU);
        auto outRSqrt = assign(inIt, TYPE_FLOAT) = Value(REG_SFU_OUT, TYPE_FLOAT);

        assign(outIt, Value(REG_SFU_RECIP_SQRT, TYPE_FLOAT)) = (7_val, PACK_32_16B_S);
        nop(outIt, DelayType::WAIT_SFU);
        nop(outIt, DelayType::WAIT_SFU);
        assign(outIt, outRSqrt) = Value(REG_SFU_OUT, TYPE_FLOAT);
    }

    // TODO test:
    // - not rewriting/using read unpack modes. Does this even make sense?

    // run step for all instructions
    auto it = inputMethod.walkAllInstructions();
    while(!it.isEndOfBlock())
    {
        it = rewriteConstantSFUCall(module, inputMethod, it, config);
        it.nextInBlock();
    }
    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testReorderBasicBlocks()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.begin(), "%default").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.begin(), "%default").walkEnd();

    // Blocks 1 (->2), 2 (->3), 3  don't get reordered
    {
        auto input1 = inputMethod.addNewLocal(TYPE_LABEL, "%first.1").local();
        auto input2 = inputMethod.addNewLocal(TYPE_LABEL, "%first.2").local();
        auto input3 = inputMethod.addNewLocal(TYPE_LABEL, "%first.3").local();
        label(inIt, input1);
        branch(inIt, input2);
        label(inIt, input2);
        branch(inIt, input3);
        label(inIt, input3);

        auto output1 = outputMethod.createLocal(TYPE_LABEL, input1->name);
        auto output2 = outputMethod.createLocal(TYPE_LABEL, input2->name);
        auto output3 = outputMethod.createLocal(TYPE_LABEL, input3->name);
        label(outIt, output1);
        branch(outIt, output2);
        label(outIt, output2);
        branch(outIt, output3);
        label(outIt, output3);
    }

    // Blocks 1 (->2), 3 (->4), 2 (->3), 4 get reordered
    {
        auto input1 = inputMethod.addNewLocal(TYPE_LABEL, "%second.1").local();
        auto input2 = inputMethod.addNewLocal(TYPE_LABEL, "%second.2").local();
        auto input3 = inputMethod.addNewLocal(TYPE_LABEL, "%second.3").local();
        auto input4 = inputMethod.addNewLocal(TYPE_LABEL, "%second.4").local();
        label(inIt, input1);
        branch(inIt, input2);
        label(inIt, input3);
        branch(inIt, input4);
        label(inIt, input2);
        branch(inIt, input3);
        label(inIt, input4);

        auto output1 = outputMethod.createLocal(TYPE_LABEL, input1->name);
        auto output2 = outputMethod.createLocal(TYPE_LABEL, input2->name);
        auto output3 = outputMethod.createLocal(TYPE_LABEL, input3->name);
        auto output4 = outputMethod.createLocal(TYPE_LABEL, input4->name);
        label(outIt, output1);
        branch(outIt, output2);
        label(outIt, output2);
        branch(outIt, output3);
        label(outIt, output3);
        branch(outIt, output4);
        label(outIt, output4);
    }

    // Blocks 1 (->2/3), 2 (->4), 4 (->3), 3 don't get reordered (if-else)
    {
        auto input1 = inputMethod.addNewLocal(TYPE_LABEL, "%third.1").local();
        auto input2 = inputMethod.addNewLocal(TYPE_LABEL, "%third.2").local();
        auto input3 = inputMethod.addNewLocal(TYPE_LABEL, "%third.3").local();
        auto input4 = inputMethod.addNewLocal(TYPE_LABEL, "%third.4").local();
        label(inIt, input1);
        branch(inIt, input2, BRANCH_ALL_Z_CLEAR);
        branch(inIt, input3, BRANCH_ANY_Z_SET);
        label(inIt, input2);
        branch(inIt, input4);
        label(inIt, input4);
        branch(inIt, input3);
        label(inIt, input3);

        auto output1 = outputMethod.createLocal(TYPE_LABEL, input1->name);
        auto output2 = outputMethod.createLocal(TYPE_LABEL, input2->name);
        auto output3 = outputMethod.createLocal(TYPE_LABEL, input3->name);
        auto output4 = outputMethod.createLocal(TYPE_LABEL, input4->name);
        label(outIt, output1);
        branch(outIt, output2, BRANCH_ALL_Z_CLEAR);
        branch(outIt, output3, BRANCH_ANY_Z_SET);
        label(outIt, output2);
        branch(outIt, output4);
        label(outIt, output4);
        branch(outIt, output3);
        label(outIt, output3);
    }

    // Blocks 1 (->3 + fall-through), 2 (fall-through), 3 don't get reordered (if-then)
    {
        auto input1 = inputMethod.addNewLocal(TYPE_LABEL, "%fourth.1").local();
        auto input2 = inputMethod.addNewLocal(TYPE_LABEL, "%fourth.2").local();
        auto input3 = inputMethod.addNewLocal(TYPE_LABEL, "%fourth.3").local();
        label(inIt, input1);
        branch(inIt, input3, BRANCH_ANY_Z_CLEAR);
        label(inIt, input2);
        label(inIt, input3);

        auto output1 = outputMethod.createLocal(TYPE_LABEL, input1->name);
        auto output2 = outputMethod.createLocal(TYPE_LABEL, input2->name);
        auto output3 = outputMethod.createLocal(TYPE_LABEL, input3->name);
        label(outIt, output1);
        branch(outIt, output3, BRANCH_ANY_Z_CLEAR);
        label(outIt, output2);
        label(outIt, output3);
    }

    // run pass
    reorderBasicBlocks(module, inputMethod, config);
    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testSimplifyBranches()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    // test simplification of successive branches to some label (inclusive fall-throughs in between)
    auto& oneIn = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%one");
    auto& oneOut = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%one");
    {
        auto inIt = oneIn.walkEnd();
        auto outIt = oneOut.walkEnd();

        inIt.emplace(std::make_unique<Branch>(oneIn.getLabel()->getLabel()));
        inIt.nextInBlock();
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%one.remove1").walkEnd();
        inIt.emplace(std::make_unique<Branch>(oneIn.getLabel()->getLabel()));
        inIt.nextInBlock();
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%one.remove2").walkEnd();
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%one.remove3").walkEnd();
        inIt.emplace(std::make_unique<Branch>(oneIn.getLabel()->getLabel()));
        inIt.nextInBlock();

        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%one.remove1").walkEnd();
        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%one.remove2").walkEnd();
        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%one.remove3").walkEnd();
        outIt.emplace(std::make_unique<Branch>(oneOut.getLabel()->getLabel()));
        outIt.nextInBlock();
    }

    // test removal of branch to next instruction
    {
        auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%second").walkEnd();
        auto inNextLabel =
            inputMethod.createAndInsertNewBlock(inputMethod.end(), "%second.next").getLabel()->getLabel();
        inIt.emplace(std::make_unique<Branch>(inNextLabel));
        inIt.nextInBlock();

        outputMethod.createAndInsertNewBlock(outputMethod.end(), "%second");
        outputMethod.createAndInsertNewBlock(outputMethod.end(), "%second.next");
    }

    // test removal of if-else branch, if second branch points to next label
    {
        auto& ifIn = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%ifelse");
        auto& ifOut = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%ifelse");
        auto inIt = ifIn.walkEnd();
        auto outIt = ifOut.walkEnd();

        auto inNextLabel =
            inputMethod.createAndInsertNewBlock(inputMethod.end(), "%ifelse.next").getLabel()->getLabel();
        outputMethod.createAndInsertNewBlock(outputMethod.end(), "%ifelse.next");

        BranchCond cond = BRANCH_ALWAYS;
        std::tie(inIt, cond) = insertBranchCondition(inputMethod, inIt, UNIFORM_REGISTER);
        inIt.emplace(std::make_unique<Branch>(ifIn.getLabel()->getLabel(), cond));
        inIt.nextInBlock();
        inIt.emplace(std::make_unique<Branch>(inNextLabel, cond.invert()));
        inIt.nextInBlock();

        std::tie(outIt, cond) = insertBranchCondition(outputMethod, outIt, UNIFORM_REGISTER);
        outIt.emplace(std::make_unique<Branch>(ifOut.getLabel()->getLabel(), cond));
        outIt.nextInBlock();
    }

    // test removal of if-else branch, if first branch points to next label
    {
        auto& ifElseIn = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%ifelse2");
        auto& ifElseOut = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%ifelse2");
        auto inIt = ifElseIn.walkEnd();
        auto outIt = ifElseOut.walkEnd();

        auto inNextLabel =
            inputMethod.createAndInsertNewBlock(inputMethod.end(), "%ifelse2.next").getLabel()->getLabel();
        outputMethod.createAndInsertNewBlock(outputMethod.end(), "%ifelse2.next");

        BranchCond cond = BRANCH_ALWAYS;
        std::tie(inIt, cond) = insertBranchCondition(inputMethod, inIt, UNIFORM_REGISTER);
        inIt.emplace(std::make_unique<Branch>(inNextLabel, cond));
        inIt.nextInBlock();
        inIt.emplace(std::make_unique<Branch>(ifElseIn.getLabel()->getLabel(), cond.invert()));
        inIt.nextInBlock();

        std::tie(outIt, cond) = insertBranchCondition(outputMethod, outIt, UNIFORM_REGISTER);
        outIt.emplace(std::make_unique<Branch>(ifElseOut.getLabel()->getLabel(), cond.invert()));
        outIt.nextInBlock();
    }

    // test not simplify of successive conditional branches to some label
    {
        auto& thirdIn = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%third");
        auto& thirdOut = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%third");
        auto inIt = thirdIn.walkEnd();
        auto outIt = thirdOut.walkEnd();

        BranchCond cond = BRANCH_ALWAYS;
        std::tie(inIt, cond) = insertBranchCondition(inputMethod, inIt, UNIFORM_REGISTER);
        inIt.emplace(std::make_unique<Branch>(thirdIn.getLabel()->getLabel(), cond));
        inIt.nextInBlock();
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%third.notremove1").walkEnd();
        inIt.emplace(std::make_unique<Branch>(thirdIn.getLabel()->getLabel(), cond));
        inIt.nextInBlock();
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%third.notremove2").walkEnd();
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%third.notremove3").walkEnd();
        inIt.emplace(std::make_unique<Branch>(thirdIn.getLabel()->getLabel()));
        inIt.nextInBlock();

        std::tie(outIt, cond) = insertBranchCondition(outputMethod, outIt, UNIFORM_REGISTER);
        outIt.emplace(std::make_unique<Branch>(thirdOut.getLabel()->getLabel(), cond));
        outIt.nextInBlock();
        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%third.notremove1").walkEnd();
        outIt.emplace(std::make_unique<Branch>(thirdOut.getLabel()->getLabel(), cond));
        outIt.nextInBlock();
        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%third.notremove2").walkEnd();
        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%third.notremove3").walkEnd();
        outIt.emplace(std::make_unique<Branch>(thirdOut.getLabel()->getLabel()));
        outIt.nextInBlock();
    }

    // test not simplify with different branch in between
    {
        auto& otherIn = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%other");
        auto& otherOut = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%other");
        auto inIt = otherIn.walkEnd();
        auto outIt = otherOut.walkEnd();

        inIt.emplace(std::make_unique<Branch>(otherIn.getLabel()->getLabel()));
        inIt.nextInBlock();
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%other.notremove1").walkEnd();
        inIt.emplace(std::make_unique<Branch>(oneIn.getLabel()->getLabel()));
        inIt.nextInBlock();
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%other.notremove2").walkEnd();
        inIt.emplace(std::make_unique<Branch>(otherIn.getLabel()->getLabel()));
        inIt.nextInBlock();

        outIt.emplace(std::make_unique<Branch>(otherOut.getLabel()->getLabel()));
        outIt.nextInBlock();
        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%other.notremove1").walkEnd();
        outIt.emplace(std::make_unique<Branch>(oneOut.getLabel()->getLabel()));
        outIt.nextInBlock();
        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%other.notremove2").walkEnd();
        outIt.emplace(std::make_unique<Branch>(otherOut.getLabel()->getLabel()));
        outIt.nextInBlock();
    }

    // test not simplify with different instruction in between
    {
        auto& someIn = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%some");
        auto& someOut = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%some");
        auto inIt = someIn.walkEnd();
        auto outIt = someOut.walkEnd();

        inIt.emplace(std::make_unique<Branch>(someIn.getLabel()->getLabel()));
        inIt.nextInBlock();
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%some.notremove1").walkEnd();
        inIt.emplace(std::make_unique<MoveOperation>(UNIFORM_REGISTER, INT_ONE));
        inIt.nextInBlock();
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%some.notremove2").walkEnd();
        inIt.emplace(std::make_unique<Branch>(someIn.getLabel()->getLabel()));
        inIt.nextInBlock();

        outIt.emplace(std::make_unique<Branch>(someOut.getLabel()->getLabel()));
        outIt.nextInBlock();
        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%some.notremove1").walkEnd();
        outIt.emplace(std::make_unique<MoveOperation>(UNIFORM_REGISTER, INT_ONE));
        outIt.nextInBlock();
        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%some.notremove2").walkEnd();
        outIt.emplace(std::make_unique<Branch>(someOut.getLabel()->getLabel()));
        outIt.nextInBlock();
    }

    // run pass
    simplifyBranches(module, inputMethod, config);
    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testMergeBasicBlocks()
{
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.begin(), "%default").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.begin(), "%default").walkEnd();

    // Merge simple fall-through-only blocks
    {
        auto tmp = assign(inIt, TYPE_INT32) = UNIFORM_REGISTER;
        label(inIt, inputMethod.createLocal(TYPE_INT32, "%1.merged"));
        auto tmp2 = assign(inIt, TYPE_HALF) = tmp * 2_lit;

        assign(outIt, tmp) = UNIFORM_REGISTER;
        // no corresponding output label
        assign(outIt, tmp2) = tmp * 2_lit;
    }

    // Merge label which is a jump-target if the predecessor block is empty
    {
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%empty_blockA").walkEnd();
        auto targetLabel = label(inIt, "%target_blockA");
        auto cond = assignNop(inIt) = as_unsigned{UNIFORM_REGISTER} == as_unsigned{11_val};
        branch(inIt, targetLabel, cond.toBranchCondition());

        targetLabel = label(outIt, "%target_blockA");
        cond = assignNop(outIt) = as_unsigned{UNIFORM_REGISTER} == as_unsigned{11_val};
        branch(outIt, targetLabel, cond.toBranchCondition());
    }

    // Don't merge label which is a jump-target if the predecessor block is not empty
    {
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%non_empty_blockB").walkEnd();
        auto tmp = assign(inIt, TYPE_INT8) = UNIFORM_REGISTER;
        auto targetLabel = label(inIt, "%target_blockB");
        assignNop(inIt) = (tmp & 5_val, SetFlag::SET_FLAGS);
        branch(inIt, targetLabel, BRANCH_ALL_Z_CLEAR);

        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%non_empty_blockB").walkEnd();
        assign(outIt, tmp) = UNIFORM_REGISTER;
        targetLabel = label(outIt, "%target_blockB");
        assignNop(outIt) = (tmp & 5_val, SetFlag::SET_FLAGS);
        branch(outIt, targetLabel, BRANCH_ALL_Z_CLEAR);
    }

    // Merge empty block which is a jump-target with successor block
    {
        auto targetLabel = label(inIt, "%target_blockC");
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%other_blockC").walkEnd();
        auto cond = assignNop(inIt) = as_unsigned{UNIFORM_REGISTER} == as_unsigned{11_val};
        branch(inIt, targetLabel, cond.toBranchCondition());

        targetLabel = label(outIt, "%target_blockC");
        cond = assignNop(outIt) = as_unsigned{UNIFORM_REGISTER} == as_unsigned{11_val};
        branch(outIt, targetLabel, cond.toBranchCondition());
    }

    // Merge non-empty jump-target block if successor block is fall-through-only
    {
        auto targetLabel = label(inIt, "%target_blockD");
        auto tmp = assign(inIt, TYPE_INT8) = UNIFORM_REGISTER;
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%other_blockD").walkEnd();
        assignNop(inIt) = (tmp & 5_val, SetFlag::SET_FLAGS);
        branch(inIt, targetLabel, BRANCH_ALL_Z_CLEAR);

        targetLabel = label(outIt, "%target_blockD");
        assign(outIt, tmp) = UNIFORM_REGISTER;
        assignNop(outIt) = (tmp & 5_val, SetFlag::SET_FLAGS);
        branch(outIt, targetLabel, BRANCH_ALL_Z_CLEAR);
    }

    // Don't merge non-empty jump-target block if successor-block has other predecessors
    {
        auto targetLabel = label(inIt, "%target_blockE");
        auto tmp = assign(inIt, TYPE_INT8) = UNIFORM_REGISTER;
        auto otherLabel = label(inIt, "%other_blockE");
        assignNop(inIt) = (tmp & 5_val, SetFlag::SET_FLAGS);
        branch(inIt, targetLabel, BRANCH_ALL_Z_CLEAR);
        branch(inIt, otherLabel, BRANCH_ANY_Z_SET);

        targetLabel = label(outIt, "%target_blockE");
        assign(outIt, tmp) = UNIFORM_REGISTER;
        otherLabel = label(outIt, "%other_blockE");
        assignNop(outIt) = (tmp & 5_val, SetFlag::SET_FLAGS);
        branch(outIt, targetLabel, BRANCH_ALL_Z_CLEAR);
        branch(outIt, otherLabel, BRANCH_ANY_Z_SET);
    }

    // Don't merge across branches
    {
        auto targetLabel = label(inIt, "%first_blockF");
        auto tmp = assign(inIt, TYPE_INT8) = UNIFORM_REGISTER;
        assignNop(inIt) = (tmp & 5_val, SetFlag::SET_FLAGS);
        branch(inIt, targetLabel, BRANCH_ALL_Z_CLEAR);
        inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%other_blockF").walkEnd();
        branch(inIt, targetLabel, BRANCH_ALL_Z_CLEAR);

        targetLabel = label(outIt, "%first_blockF");
        assign(outIt, tmp) = UNIFORM_REGISTER;
        assignNop(outIt) = (tmp & 5_val, SetFlag::SET_FLAGS);
        branch(outIt, targetLabel, BRANCH_ALL_Z_CLEAR);
        outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%other_blockF").walkEnd();
        branch(outIt, targetLabel, BRANCH_ALL_Z_CLEAR);
    }

    // run pass
    mergeAdjacentBasicBlocks(module, inputMethod, config);
    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testCombineConstantLoads()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%dummy").walkEnd();

    // NOTE: Need to execute the optimization pass before adding the expected instructions, since the optimization pass
    // checks for single writer, which we violate by reusing the local across methods!

    auto first = inputMethod.addNewLocal(TYPE_INT32, "%first");
    auto second = inputMethod.addNewLocal(TYPE_INT32, "%second");
    auto third = inputMethod.addNewLocal(TYPE_INT32, "%third");

    Value qpuNum(REG_QPU_NUMBER, TYPE_INT8);
    auto fourth = inputMethod.addNewLocal(TYPE_INT32, "%fourth");
    auto fifth = inputMethod.addNewLocal(TYPE_INT32, "%fifth");

    auto sixth = inputMethod.addNewLocal(TYPE_INT32, "%sixth");
    auto seventh = inputMethod.addNewLocal(TYPE_INT32, "%seventh");

    auto eight = inputMethod.addNewLocal(TYPE_INT32, "%eigth");
    auto nineth = inputMethod.addNewLocal(TYPE_INT32, "%nineth");

    auto tenth = inputMethod.addNewLocal(TYPE_INT32, "%tenth");
    auto eleventh = inputMethod.addNewLocal(TYPE_INT32, "%eleventh");

    // test combine loading of constants (part 1)
    {
        inIt.emplace(std::make_unique<LoadImmediate>(first, 123456_lit, COND_ALWAYS, SetFlag::SET_FLAGS));
        inIt.nextInBlock();
        inIt.emplace(std::make_unique<LoadImmediate>(second, 123456_lit));
        inIt.nextInBlock();
        assign(inIt, UNIFORM_REGISTER) = second;
        inIt.emplace(std::make_unique<LoadImmediate>(third, 123456_lit));
        inIt.nextInBlock();
        assign(inIt, UNIFORM_REGISTER) = third;
    }

    // test combine read of constant registers (part 1)
    {
        assign(inIt, fourth) = (qpuNum, SIGNAL_LOAD_ALPHA);
        assign(inIt, UNIFORM_REGISTER) = fourth;
        assign(inIt, fifth) = qpuNum;
        assign(inIt, UNIFORM_REGISTER) = fifth;
    }

    // test not combine loads with side-effects (part 1)
    {
        inIt.emplace(std::make_unique<LoadImmediate>(sixth, 123456_lit));
        inIt.nextInBlock();
        assign(inIt, UNIFORM_REGISTER) = sixth;
        inIt.emplace(std::make_unique<LoadImmediate>(seventh, 123456_lit, COND_ALWAYS, SetFlag::SET_FLAGS));
        inIt.nextInBlock();
        assign(inIt, UNIFORM_REGISTER) = seventh;

        assign(inIt, eight) = qpuNum;
        assign(inIt, UNIFORM_REGISTER) = eight;
        assign(inIt, nineth) = (qpuNum, SIGNAL_LOAD_COVERAGE);
        assign(inIt, UNIFORM_REGISTER) = nineth;
    }

    // test not combine loads with different constants (part 1)
    {
        inIt.emplace(std::make_unique<LoadImmediate>(tenth, 123456_lit));
        inIt.nextInBlock();
        assign(inIt, UNIFORM_REGISTER) = tenth;
        inIt.emplace(std::make_unique<LoadImmediate>(eleventh, 654321_lit));
        inIt.nextInBlock();
        assign(inIt, UNIFORM_REGISTER) = eleventh;
    }

    // run pass
    combineLoadingConstants(module, inputMethod, config);

    // test combine loading of constants (part 2)
    {
        outIt.emplace(std::make_unique<LoadImmediate>(first, 123456_lit, COND_ALWAYS, SetFlag::SET_FLAGS));
        outIt.nextInBlock();
        assign(outIt, UNIFORM_REGISTER) = first;
        assign(outIt, UNIFORM_REGISTER) = first;
    }

    // test combine read of constant registers (part 2)
    {
        assign(outIt, fourth) = (qpuNum, SIGNAL_LOAD_ALPHA);
        assign(outIt, UNIFORM_REGISTER) = fourth;
        assign(outIt, UNIFORM_REGISTER) = fourth;
    }

    // test not combine loads with side-effects (part 2)
    {
        outIt.emplace(std::make_unique<LoadImmediate>(sixth, 123456_lit));
        outIt.nextInBlock();
        assign(outIt, UNIFORM_REGISTER) = sixth;
        outIt.emplace(std::make_unique<LoadImmediate>(seventh, 123456_lit, COND_ALWAYS, SetFlag::SET_FLAGS));
        outIt.nextInBlock();
        assign(outIt, UNIFORM_REGISTER) = seventh;

        assign(outIt, eight) = qpuNum;
        assign(outIt, UNIFORM_REGISTER) = eight;
        assign(outIt, nineth) = (qpuNum, SIGNAL_LOAD_COVERAGE);
        assign(outIt, UNIFORM_REGISTER) = nineth;
    }

    // test not combine loads with different constants (part 2)
    {
        outIt.emplace(std::make_unique<LoadImmediate>(tenth, 123456_lit));
        outIt.nextInBlock();
        assign(outIt, UNIFORM_REGISTER) = tenth;
        outIt.emplace(std::make_unique<LoadImmediate>(eleventh, 654321_lit));
        outIt.nextInBlock();
        assign(outIt, UNIFORM_REGISTER) = eleventh;
    }

    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testEliminateBitOperations()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%dummy").walkEnd();

    auto a = assign(inIt, TYPE_INT32, "%a") = UNIFORM_REGISTER;
    auto b = assign(inIt, TYPE_INT32, "%b") = UNIFORM_REGISTER;
    assign(outIt, a) = UNIFORM_REGISTER;
    assign(outIt, b) = UNIFORM_REGISTER;

    // positive tests - optimization is applied
    {
        // (%a & %b) & %a -> %a & %b
        auto c = assign(inIt, TYPE_INT32, "%c") = a & b;
        auto d = assign(inIt, TYPE_INT32, "%d") = c & a;

        // (%a & %b) & %b -> %a & %b
        auto e = assign(inIt, TYPE_INT32, "%e") = c & b;

        // %a & (%a & %b) -> %a & %b
        auto f = assign(inIt, TYPE_INT32, "%f") = a & c;

        // %b & (%a & %b) -> %a & %b
        auto g = assign(inIt, TYPE_INT32, "%g") = b & c;

        assign(outIt, c) = a & b;
        assign(outIt, d) = c;
        assign(outIt, e) = c;
        assign(outIt, f) = c;
        assign(outIt, g) = c;
    }

    {
        // (%a | %b) | %a -> %a | %b
        auto h = assign(inIt, TYPE_INT32, "%h") = a | b;
        auto i = assign(inIt, TYPE_INT32, "%i") = h | a;

        // (%a | %b) | %b -> %a | %b
        auto j = assign(inIt, TYPE_INT32, "%j") = h | b;

        // %a | (%a | %b) -> %a | %b
        auto k = assign(inIt, TYPE_INT32, "%k") = a | h;

        // %b | (%a | %b) -> %a | %b
        auto l = assign(inIt, TYPE_INT32, "%l") = b | h;

        assign(outIt, h) = a | b;
        assign(outIt, i) = h;
        assign(outIt, j) = h;
        assign(outIt, k) = h;
        assign(outIt, l) = h;
    }

    {
        // (%a | %b) & %a -> %a
        auto m = assign(inIt, TYPE_INT32, "%m") = a | b;
        auto n = assign(inIt, TYPE_INT32, "%n") = m & a;

        // (%a | %b) & %b -> %b
        auto o = assign(inIt, TYPE_INT32, "%o") = m & b;

        // %a & (%a | %b) -> %a
        auto p = assign(inIt, TYPE_INT32, "%p") = a & m;

        // %b & (%a | %b) -> %b
        auto q = assign(inIt, TYPE_INT32, "%q") = b & m;

        assign(outIt, m) = a | b;
        assign(outIt, n) = a;
        assign(outIt, o) = b;
        assign(outIt, p) = a;
        assign(outIt, q) = b;
    }

    {
        // (%a & %b) | %a -> %a
        auto r = assign(inIt, TYPE_INT32, "%r") = a & b;
        auto s = assign(inIt, TYPE_INT32, "%s") = r | a;

        // (%a & %b) | %b -> %b
        auto t = assign(inIt, TYPE_INT32, "%t") = r | b;

        // %a | (%a & %b) -> %a
        auto u = assign(inIt, TYPE_INT32, "%u") = a | r;

        // %b | (%a & %b) -> %b
        auto v = assign(inIt, TYPE_INT32, "%v") = b | r;

        assign(outIt, r) = a & b;
        assign(outIt, s) = a;
        assign(outIt, t) = b;
        assign(outIt, u) = a;
        assign(outIt, v) = b;
    }

    // check transfer of additional info (decorations, conditionals, flags) for replaced operations
    {
        auto w = assign(inIt, TYPE_INT32, "%w") = (a & b, SetFlag::SET_FLAGS, UNPACK_16A_32);
        auto x = assign(inIt, TYPE_INT32, "%x") = (w & a, COND_CARRY_SET, PACK_32_8888);
        auto y = assign(inIt, TYPE_INT32, "%y") = (w & b, SetFlag::SET_FLAGS);
        auto z = assign(inIt, TYPE_INT32, "%z") = (w & a, SIGNAL_LOAD_COLOR, InstructionDecorations::UNSIGNED_RESULT);
        assign(inIt, Value(REG_UNIFORM_ADDRESS, TYPE_INT32)) = w & a;

        assign(outIt, w) = (a & b, SetFlag::SET_FLAGS, UNPACK_16A_32);
        assign(outIt, x) = (w, COND_CARRY_SET, PACK_32_8888);
        assign(outIt, y) = (w, SetFlag::SET_FLAGS);
        assign(outIt, z) = (w, SIGNAL_LOAD_COLOR, InstructionDecorations::UNSIGNED_RESULT);
        assign(outIt, Value(REG_UNIFORM_ADDRESS, TYPE_INT32)) = w;
    }

    // negative tests - optimization not applied

    // no elimination if conditional execution on first operation
    {
        auto A = assign(inIt, TYPE_INT32, "%A") = (a & b, COND_NEGATIVE_CLEAR);
        auto B = assign(inIt, TYPE_INT32, "%B") = A & a;

        assign(outIt, A) = (a & b, COND_NEGATIVE_CLEAR);
        assign(outIt, B) = A & a;
    }

    // check no elimination if reading input has side-effects
    {
        Value uniform(REG_UNIFORM, TYPE_INT32);
        auto C = assign(inIt, TYPE_INT32, "%C") = (uniform & b, COND_NEGATIVE_CLEAR);
        auto D = assign(inIt, TYPE_INT32, "%D") = C & uniform;

        assign(outIt, C) = (uniform & b, COND_NEGATIVE_CLEAR);
        assign(outIt, D) = C & uniform;
    }

    // check no elimination if second operation has unpack mode
    {
        auto E = assign(inIt, TYPE_INT32, "%E") = a & b;
        auto F = assign(inIt, TYPE_INT32, "%F") = (E & a, UNPACK_16A_32);

        assign(outIt, E) = a & b;
        assign(outIt, F) = (E & a, UNPACK_16A_32);
    }

    // positive check again, this time split (part 1)
    Value a0 = UNDEFINED_VALUE;
    Value b0 = UNDEFINED_VALUE;
    Value c0 = UNDEFINED_VALUE;
    Value d0 = UNDEFINED_VALUE;
    Value e0 = UNDEFINED_VALUE;
    Value f0 = UNDEFINED_VALUE;
    Value g0 = UNDEFINED_VALUE;
    Value h0 = UNDEFINED_VALUE;
    Value i0 = UNDEFINED_VALUE;
    Value j0 = UNDEFINED_VALUE;
    Value k0 = UNDEFINED_VALUE;
    Value l0 = UNDEFINED_VALUE;
    Value m0 = UNDEFINED_VALUE;
    Value n0 = UNDEFINED_VALUE;
    Value o0 = UNDEFINED_VALUE;
    Value p0 = UNDEFINED_VALUE;
    Value q0 = UNDEFINED_VALUE;
    Value r0 = UNDEFINED_VALUE;
    Value s0 = UNDEFINED_VALUE;
    Value t0 = UNDEFINED_VALUE;
    Value u0 = UNDEFINED_VALUE;

    {
        // (%a asr 16) & 0xFFF -> (%a shr 16) & 0xFFF
        a0 = assign(inIt, TYPE_INT32, "%a0") = as_signed{a} >> 16_val;
        b0 = assign(inIt, TYPE_INT32, "%b0") = a0 & 0xFFF_val;
    }

    {
        // (%a shl 15) shr 15 -> %a & 0x1FFFF
        c0 = assign(inIt, TYPE_INT32, "%c0") = a << 15_val;
        d0 = assign(inIt, TYPE_INT32, "%d0") = as_unsigned{c0} >> 15_val;
    }

    // negative tests again (part 1)
    // check transfer of additional info (decorations, conditionals, flags) for replaced operations (part 1)
    {
        e0 = assign(inIt, TYPE_INT32, "%e0") = (as_signed{b} >> 16_val, InstructionDecorations::EXACT_OPERATION);
        f0 = assign(inIt, TYPE_INT32, "%f0") = (0xFF_val & e0, COND_CARRY_SET, PACK_32_16A);
        g0 = assign(inIt, TYPE_INT32, "%g0") =
            (b << 14_val, InstructionDecorations::EXACT_OPERATION, SetFlag::SET_FLAGS, SIGNAL_LOAD_ALPHA);
        h0 = assign(inIt, TYPE_INT32, "%h0") = (as_unsigned{g0} >> 14_val, InstructionDecorations::EXACT_OPERATION);
    }

    // check no replacement if asr has pack mode (part 1)
    {
        i0 = assign(inIt, TYPE_INT32, "%i0") = (as_signed{b} >> 14_val, PACK_32_8888);
        j0 = assign(inIt, TYPE_INT32, "%j0") = i0 & 0xFF_val;
    }

    // check no replacement if asr sets flags (since e.g. negative flag would differ) (part 1)
    {
        k0 = assign(inIt, TYPE_INT32, "%k0") = (as_signed{b} >> 14_val, SetFlag::SET_FLAGS);
        l0 = assign(inIt, TYPE_INT32, "%l0") = k0 & 0xFF_val;
    }

    // check no replacement if asr has non-and readers (part 1)
    {
        m0 = assign(inIt, TYPE_INT32, "%m0") = (as_signed{b} >> 14_val, SetFlag::SET_FLAGS);
        n0 = assign(inIt, TYPE_INT32, "%n0") = m0 & 0xFF_val;
        o0 = assign(inIt, TYPE_INT32, "%o0") = m0 + 0xFF_val;
    }

    // check no replacement if and mask bigger than asr offset (part 1)
    {
        p0 = assign(inIt, TYPE_INT32, "%p0") = (as_signed{b} >> 18_val, SetFlag::SET_FLAGS);
        q0 = assign(inIt, TYPE_INT32, "%q0") = p0 & 0xFFFF_val;
    }

    // check no replacement if shl/shr offsets do not match (part 1)
    {
        r0 = assign(inIt, TYPE_INT32, "%r0") = a << 16_val;
        s0 = assign(inIt, TYPE_INT32, "%s0") = as_unsigned{r0} >> 15_val;
    }

    // check no replacement if shr has unpack mode (part 1)
    {
        t0 = assign(inIt, TYPE_INT32, "%t0") = a << 16_val;
        u0 = assign(inIt, TYPE_INT32, "%u0") = (as_unsigned{t0} >> 15_val, PACK_32_16B);
    }

    // NOTE: Since the optimization partially checks for single writers, we need to add the output instructions after
    // the optimization is applied. Otherwise the write from the output method will fail that check.
    // run pass
    eliminateRedundantBitOp(module, inputMethod, config);

    // positive check again, this time split (part 2)
    {
        // (%a asr 16) & 0xFFF -> (%a shr 16) & 0xFFF
        assign(outIt, a0) = as_unsigned{a} >> 16_val;
        assign(outIt, b0) = a0 & 0xFFF_val;
    }

    {
        // (%a shl 15) shr 15 -> %a & 0x1FFFF
        assign(outIt, c0) = a << 15_val;
        assign(outIt, d0) = a & 0x1FFFF_val;
    }

    // check transfer of additional info (decorations, conditionals, flags) for replaced operations (part 2)
    {
        assign(outIt, e0) = (as_unsigned{b} >> 16_val, InstructionDecorations::EXACT_OPERATION);
        assign(outIt, f0) = (0xFF_val & e0, COND_CARRY_SET, PACK_32_16A);
        assign(outIt, g0) =
            (b << 14_val, InstructionDecorations::EXACT_OPERATION, SetFlag::SET_FLAGS, SIGNAL_LOAD_ALPHA);
        assign(outIt, h0) = (b & 0x3FFFF_val, InstructionDecorations::EXACT_OPERATION);
    }

    // check no replacement if asr has pack mode (part 2)
    {
        assign(outIt, i0) = (as_signed{b} >> 14_val, PACK_32_8888);
        assign(outIt, j0) = i0 & 0xFF_val;
    }

    // check no replacement if asr sets flags (since e.g. negative flag would differ) (part 2)
    {
        assign(outIt, k0) = (as_signed{b} >> 14_val, SetFlag::SET_FLAGS);
        assign(outIt, l0) = k0 & 0xFF_val;
    }

    // check no replacement if asr has non-and readers (part 2)
    {
        assign(outIt, m0) = (as_signed{b} >> 14_val, SetFlag::SET_FLAGS);
        assign(outIt, n0) = m0 & 0xFF_val;
        assign(outIt, o0) = m0 + 0xFF_val;
    }

    // check no replacement if and mask bigger than asr offset (part 2)
    {
        assign(outIt, p0) = (as_signed{b} >> 18_val, SetFlag::SET_FLAGS);
        assign(outIt, q0) = p0 & 0xFFFF_val;
    }

    // check no replacement if shl/shr offsets do not match (part 2)
    {
        assign(outIt, r0) = a << 16_val;
        assign(outIt, s0) = as_unsigned{r0} >> 15_val;
    }

    // check no replacement if shr has unpack mode (part 2)
    {
        assign(outIt, t0) = a << 16_val;
        assign(outIt, u0) = (as_unsigned{t0} >> 15_val, PACK_32_16B);
    }

    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testCombineRotations()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%dummy").walkEnd();

    auto in = assign(inIt, TYPE_INT32, "%in") = UNIFORM_REGISTER;
    assign(outIt, in) = UNIFORM_REGISTER;

    // positive tests - rotations are combined
    {
        // test basic rotation with constant immediate/r5 offset
        auto a = inputMethod.addNewLocal(TYPE_INT32, "%a");
        auto b = inputMethod.addNewLocal(TYPE_INT32, "%b");
        assign(inIt, ROTATION_REGISTER) = 11_val;
        assign(outIt, ROTATION_REGISTER) = 11_val;

        inIt.emplace(
            std::make_unique<VectorRotation>(a, in, SmallImmediate::fromRotationOffset(4), RotationType::FULL));
        inIt->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        inIt.nextInBlock();
        inIt.emplace(std::make_unique<VectorRotation>(b, a, VECTOR_ROTATE_R5, RotationType::ANY))
            .setCondition(COND_NEGATIVE_SET)
            .setSetFlags(SetFlag::SET_FLAGS);
        inIt->addDecorations(InstructionDecorations::ALLOW_RECIP);
        inIt.nextInBlock();

        outIt
            .emplace(
                std::make_unique<VectorRotation>(b, in, SmallImmediate::fromRotationOffset(15), RotationType::FULL))
            .setCondition(COND_NEGATIVE_SET)
            .setSetFlags(SetFlag::SET_FLAGS);
        outIt->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
        outIt->addDecorations(InstructionDecorations::ALLOW_RECIP);
        outIt.nextInBlock();
    }

    {
        // test combination if first has unpack mode/second has pack mode
        auto c = inputMethod.addNewLocal(TYPE_INT32, "%c");
        auto d = inputMethod.addNewLocal(TYPE_INT32, "%d");

        inIt.emplace(
                std::make_unique<VectorRotation>(c, in, SmallImmediate::fromRotationOffset(1), RotationType::PER_QUAD))
            .setUnpackMode(UNPACK_8A_32);
        inIt.nextInBlock();
        inIt.emplace(
                std::make_unique<VectorRotation>(d, c, SmallImmediate::fromRotationOffset(2), RotationType::PER_QUAD))
            .setPackMode(PACK_32_8888);
        inIt.nextInBlock();

        outIt
            .emplace(
                std::make_unique<VectorRotation>(d, in, SmallImmediate::fromRotationOffset(3), RotationType::PER_QUAD))
            .setUnpackMode(UNPACK_8A_32)
            .setPackMode(PACK_32_8888);
        outIt.nextInBlock();
    }

    {
        // test simplification of special rotation values: 0/16 (full), 0/4 (quad), reduce to move
        auto e = inputMethod.addNewLocal(TYPE_INT32, "%e");
        auto f = inputMethod.addNewLocal(TYPE_INT32, "%f");
        auto g = inputMethod.addNewLocal(TYPE_INT32, "%g");
        auto h = inputMethod.addNewLocal(TYPE_INT32, "%h");
        auto i = inputMethod.addNewLocal(TYPE_INT32, "%i");
        auto j = inputMethod.addNewLocal(TYPE_INT32, "%j");

        inIt.emplace(
            std::make_unique<VectorRotation>(e, in, SmallImmediate::fromRotationOffset(1), RotationType::PER_QUAD));
        inIt.nextInBlock();
        inIt.emplace(
            std::make_unique<VectorRotation>(f, e, SmallImmediate::fromRotationOffset(3), RotationType::PER_QUAD));
        inIt.nextInBlock();
        inIt.emplace(
            std::make_unique<VectorRotation>(g, in, SmallImmediate::fromRotationOffset(7), RotationType::FULL));
        inIt.nextInBlock();
        inIt.emplace(std::make_unique<VectorRotation>(h, g, SmallImmediate::fromRotationOffset(9), RotationType::FULL));
        inIt.nextInBlock();
        assign(inIt, ROTATION_REGISTER) = 4_val;
        inIt.emplace(std::make_unique<VectorRotation>(i, in, VECTOR_ROTATE_R5, RotationType::PER_QUAD));
        inIt->addDecorations(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        inIt.nextInBlock();
        assign(inIt, ROTATION_REGISTER) = 16_val;
        inIt.emplace(std::make_unique<VectorRotation>(j, in, VECTOR_ROTATE_R5, RotationType::FULL))
            .setPackMode(PACK_32_16B_S);
        inIt.nextInBlock();

        assign(outIt, f) = in;
        assign(outIt, h) = in;
        assign(outIt, ROTATION_REGISTER) = 4_val;
        assign(outIt, i) = (in, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        assign(outIt, ROTATION_REGISTER) = 16_val;
        assign(outIt, j) = (in, PACK_32_16B_S);
    }

    // more tests see below

    // negative tests - instructions are unchanged

    {
        // test no combination if rotation types do not overlap
        auto A = inputMethod.addNewLocal(TYPE_INT32, "%A");
        auto B = inputMethod.addNewLocal(TYPE_INT32, "%B");

        inIt.emplace(
            std::make_unique<VectorRotation>(A, in, SmallImmediate::fromRotationOffset(1), RotationType::FULL));
        inIt.nextInBlock();
        inIt.emplace(
            std::make_unique<VectorRotation>(B, A, SmallImmediate::fromRotationOffset(2), RotationType::PER_QUAD));
        inIt.nextInBlock();

        outIt.emplace(
            std::make_unique<VectorRotation>(A, in, SmallImmediate::fromRotationOffset(1), RotationType::FULL));
        outIt.nextInBlock();
        outIt.emplace(
            std::make_unique<VectorRotation>(B, A, SmallImmediate::fromRotationOffset(2), RotationType::PER_QUAD));
        outIt.nextInBlock();
    }

    {
        // test no combination if first has pack mode
        auto C = inputMethod.addNewLocal(TYPE_INT32, "%C");
        auto D = inputMethod.addNewLocal(TYPE_INT32, "%D");

        inIt.emplace(
                std::make_unique<VectorRotation>(C, in, SmallImmediate::fromRotationOffset(1), RotationType::PER_QUAD))
            .setPackMode(PACK_32_16A_S);
        inIt.nextInBlock();
        inIt.emplace(
            std::make_unique<VectorRotation>(D, C, SmallImmediate::fromRotationOffset(2), RotationType::PER_QUAD));
        inIt.nextInBlock();

        outIt
            .emplace(
                std::make_unique<VectorRotation>(C, in, SmallImmediate::fromRotationOffset(1), RotationType::PER_QUAD))
            .setPackMode(PACK_32_16A_S);
        outIt.nextInBlock();
        outIt.emplace(
            std::make_unique<VectorRotation>(D, C, SmallImmediate::fromRotationOffset(2), RotationType::PER_QUAD));
        outIt.nextInBlock();
    }

    {
        // test no combination if second has unpack mode
        auto E = inputMethod.addNewLocal(TYPE_INT32, "%E");
        auto F = inputMethod.addNewLocal(TYPE_INT32, "%F");

        inIt.emplace(
            std::make_unique<VectorRotation>(E, in, SmallImmediate::fromRotationOffset(1), RotationType::PER_QUAD));
        inIt.nextInBlock();
        inIt.emplace(
                std::make_unique<VectorRotation>(F, E, SmallImmediate::fromRotationOffset(2), RotationType::PER_QUAD))
            .setUnpackMode(UNPACK_8A_32);
        inIt.nextInBlock();

        outIt.emplace(
            std::make_unique<VectorRotation>(E, in, SmallImmediate::fromRotationOffset(1), RotationType::PER_QUAD));
        outIt.nextInBlock();
        outIt
            .emplace(
                std::make_unique<VectorRotation>(F, E, SmallImmediate::fromRotationOffset(2), RotationType::PER_QUAD))
            .setUnpackMode(UNPACK_8A_32);
        outIt.nextInBlock();
    }

    {
        // test no removal of rotation which has side-effects
        auto G = inputMethod.addNewLocal(TYPE_INT32, "%G");
        auto H = inputMethod.addNewLocal(TYPE_INT32, "%H");

        inIt.emplace(
                std::make_unique<VectorRotation>(G, in, SmallImmediate::fromRotationOffset(1), RotationType::PER_QUAD))
            .setSetFlags(SetFlag::SET_FLAGS);
        inIt.nextInBlock();
        inIt.emplace(
            std::make_unique<VectorRotation>(H, G, SmallImmediate::fromRotationOffset(3), RotationType::PER_QUAD));
        inIt.nextInBlock();

        outIt
            .emplace(
                std::make_unique<VectorRotation>(G, in, SmallImmediate::fromRotationOffset(1), RotationType::PER_QUAD))
            .setSetFlags(SetFlag::SET_FLAGS);
        outIt.nextInBlock();
        outIt.emplace(
            std::make_unique<VectorRotation>(H, G, SmallImmediate::fromRotationOffset(3), RotationType::PER_QUAD));
        outIt.nextInBlock();
    }

    // NOTE: Since the optimization partially checks for single writers, we need to add the output instructions after
    // the optimization is applied. Otherwise the write from the output method will fail that check.
    auto k = inputMethod.addNewLocal(TYPE_INT32, "%k");
    auto l = inputMethod.addNewLocal(TYPE_INT32, "%l");
    auto m = inputMethod.addNewLocal(TYPE_INT32, "%m");
    auto n = inputMethod.addNewLocal(TYPE_INT32, "%n");
    auto o = inputMethod.addNewLocal(TYPE_INT32, "%o");
    auto p = inputMethod.addNewLocal(TYPE_INT32, "%p");
    auto r = inputMethod.addNewLocal(TYPE_INT32, "%r");
    auto s = inputMethod.addNewLocal(TYPE_INT32, "%s");

    {
        // test rewrite rotation of constant to load of constant (move and load instructions) (part 1)
        assign(inIt, k) = 116_val;
        assign(inIt, ROTATION_REGISTER) = 7_val;
        inIt.emplace(std::make_unique<VectorRotation>(l, k, SmallImmediate::fromRotationOffset(3), RotationType::FULL));
        inIt.nextInBlock();
        inIt.emplace(std::make_unique<LoadImmediate>(m, 1234_lit));
        inIt.nextInBlock();
        inIt.emplace(std::make_unique<VectorRotation>(n, m, VECTOR_ROTATE_R5, RotationType::FULL));
        inIt->addDecorations(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        inIt.nextInBlock();
    }

    {
        // test rewrite rotation of masked load to rotated load of mask (full and quad rotation) (part 1)
        inIt.emplace(std::make_unique<LoadImmediate>(o, 0x12340000, LoadType::PER_ELEMENT_SIGNED));
        inIt.nextInBlock();
        inIt.emplace(std::make_unique<VectorRotation>(p, o, SmallImmediate::fromRotationOffset(4), RotationType::FULL))
            .setSetFlags(SetFlag::SET_FLAGS);
        inIt.nextInBlock();
        inIt.emplace(std::make_unique<LoadImmediate>(r, 0x00001234, LoadType::PER_ELEMENT_UNSIGNED));
        inIt.nextInBlock();
        // TODO test per-quad rotation, full rotation is already tested above
        inIt.emplace(std::make_unique<VectorRotation>(s, r, SmallImmediate::fromRotationOffset(2), RotationType::FULL));
        inIt->addDecorations(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        inIt.nextInBlock();
    }

    // run pass
    combineVectorRotations(module, inputMethod, config);

    {
        // test rewrite rotation of constant to load of constant (move and load instructions) (part 2)
        assign(outIt, k) = 116_val;
        assign(outIt, ROTATION_REGISTER) = 7_val;
        assign(outIt, l) = k;
        outIt.emplace(std::make_unique<LoadImmediate>(m, 1234_lit));
        outIt.nextInBlock();
        outIt.emplace(std::make_unique<LoadImmediate>(n, 1234_lit));
        outIt->addDecorations(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        outIt.nextInBlock();
    }

    {
        // test rewrite rotation of masked load to rotated load of mask (full and quad rotation) (part 2)
        outIt.emplace(std::make_unique<LoadImmediate>(o, 0x12340000, LoadType::PER_ELEMENT_SIGNED));
        outIt.nextInBlock();
        outIt.emplace(std::make_unique<LoadImmediate>(p, 0x23410000, LoadType::PER_ELEMENT_SIGNED))
            .setSetFlags(SetFlag::SET_FLAGS);
        outIt.nextInBlock();
        outIt.emplace(std::make_unique<LoadImmediate>(r, 0x00001234, LoadType::PER_ELEMENT_UNSIGNED));
        outIt.nextInBlock();
        outIt.emplace(std::make_unique<LoadImmediate>(s, 0x000048D0, LoadType::PER_ELEMENT_UNSIGNED));
        outIt->addDecorations(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE);
        outIt.nextInBlock();
    }

    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testEliminateMoves()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%dummy").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%dummy").walkEnd();

    auto a = assign(inIt, TYPE_INT32, "%a") = UNIFORM_REGISTER;
    auto b = assign(inIt, TYPE_INT32, "%b") = UNIFORM_REGISTER;
    assign(outIt, a) = UNIFORM_REGISTER;
    assign(outIt, b) = UNIFORM_REGISTER;

    // NOTE: Since the optimization checks for single writers, we need to add the output instructions after the
    // optimization is applied. Otherwise the write from the output method will fail that check.

    auto c = inputMethod.addNewLocal(TYPE_INT32, "%c");
    auto d = inputMethod.addNewLocal(TYPE_INT32, "%d");
    auto e = inputMethod.addNewLocal(TYPE_INT32, "%e");

    // positive tests - move is removed
    {
        // tests removal of move + transfer of additional instruction info (part 1)
        assign(inIt, a) = a;
        assign(inIt, b) = (b, SIGNAL_LOAD_COLOR);
        assign(inIt, c) = (a + b, InstructionDecorations::UNSIGNED_OVERFLOW_IS_UB);
        assign(inIt, d) = c;
        assign(inIt, e) = (d - b, UNPACK_8888_32);
        assign(inIt, Value(REG_VPM_IO, TYPE_INT32)) = e;
    }

    auto f = inputMethod.addNewLocal(TYPE_INT32, "%f");
    auto g = inputMethod.addNewLocal(TYPE_INT32, "%g");
    auto h = inputMethod.addNewLocal(TYPE_INT32, "%h");
    auto i = inputMethod.addNewLocal(TYPE_INT32, "%i");

    {
        // tests removal of moves according to optimization comment (part 1)
        assign(inIt, f) = as_signed{a} + as_signed{b};
        assign(inIt, g) = f;
        assign(inIt, h) = UNIFORM_REGISTER;
        assign(inIt, i) = as_signed{g} + as_signed{h};
    }

    auto k = inputMethod.addNewLocal(TYPE_INT32, "%k");
    auto l = inputMethod.addNewLocal(TYPE_INT32, "%l");
    auto m = inputMethod.addNewLocal(TYPE_INT32, "%m");

    // negative tests - move is not removed
    {
        // tests not removing moves with side-effects (part 1)
        assign(inIt, k) = (i, SetFlag::SET_FLAGS);
        assign(inIt, l) = (a, SIGNAL_LOAD_TMU0);
        assign(inIt, m) = k | l;
    }

    auto n = inputMethod.addNewLocal(TYPE_INT32, "%n");
    auto o = inputMethod.addNewLocal(TYPE_INT32, "%o");

    {
        // tests not removing move of conditionally written local (part 1)
        assign(inIt, n) = a;
        assign(inIt, n) = (b, COND_ZERO_CLEAR);
        assign(inIt, o) = n;
    }

    auto p = inputMethod.addNewLocal(TYPE_INT32, "%p");
    auto q = inputMethod.addNewLocal(TYPE_INT32, "%q");
    auto r = inputMethod.addNewLocal(TYPE_INT32, "%r");
    auto s = inputMethod.addNewLocal(TYPE_INT32, "%s");

    {
        // tests not removing move with (un)pack modes (part 1)
        assign(inIt, p) = as_signed{a} - as_signed{Value(REG_X_COORDS, TYPE_INT8)};
        assign(inIt, q) = (p, PACK_32_16A_S);
        assign(inIt, r) = (a, UNPACK_16A_32);
        assign(inIt, s) = as_signed{r} - as_signed{Value(REG_X_COORDS, TYPE_INT8)};
    }

    auto t = inputMethod.addNewLocal(TYPE_INT32, "%t");
    auto u = inputMethod.addNewLocal(TYPE_INT32, "%u");

    {
        // tests not removing of vector rotations (part 1)
        inIt.emplace(std::make_unique<VectorRotation>(t, a, SmallImmediate::fromRotationOffset(3), RotationType::FULL));
        inIt.nextInBlock();
        assign(inIt, u) = t;
    }

    auto v = inputMethod.addNewLocal(TYPE_INT32, "%v");
    auto w = inputMethod.addNewLocal(TYPE_INT32, "%w");
    auto x = inputMethod.addNewLocal(TYPE_INT32, "%x");
    auto y = inputMethod.addNewLocal(TYPE_INT32, "%y");
    auto z = inputMethod.addNewLocal(TYPE_INT32, "%z");

    {
        // tests not reordering reading of registers with side-effects (part 1)
        assign(inIt, v) = UNIFORM_REGISTER;
        assign(inIt, w) = a + UNIFORM_REGISTER;
        assign(inIt, x) = v + w;
        assign(inIt, y) = UNIFORM_REGISTER;
        assign(inIt, z) = x + y;
    }

    // run pass
    eliminateRedundantMoves(module, inputMethod, config);

    {
        // tests removal of move + transfer of additional instruction info (part 2)
        nop(outIt, DelayType::WAIT_REGISTER, SIGNAL_LOAD_COLOR);
        assign(outIt, c) = (a + b, InstructionDecorations::UNSIGNED_OVERFLOW_IS_UB);
        assign(outIt, Value(REG_VPM_IO, TYPE_INT32)) = (c - b, UNPACK_8888_32);
    }

    {
        // tests removal of moves according to optimization comment (part 2)
        assign(outIt, f) = as_signed{a} + as_signed{b};
        assign(outIt, i) = as_signed{f} + as_signed{UNIFORM_REGISTER};
    }

    {
        // tests not removing moves with side-effects (part 2)
        assign(outIt, k) = (i, SetFlag::SET_FLAGS);
        assign(outIt, l) = (a, SIGNAL_LOAD_TMU0);
        assign(outIt, m) = k | l;
    }

    {
        // tests not removing move of conditionally written local (part 2)
        assign(outIt, n) = a;
        assign(outIt, n) = (b, COND_ZERO_CLEAR);
        assign(outIt, o) = n;
    }

    {
        // tests not removing move with (un)pack modes if we cannot use register-file A (part 2)
        assign(outIt, p) = as_signed{a} - as_signed{Value(REG_X_COORDS, TYPE_INT8)};
        assign(outIt, q) = (p, PACK_32_16A_S);
        assign(outIt, r) = (a, UNPACK_16A_32);
        assign(outIt, s) = as_signed{r} - as_signed{Value(REG_X_COORDS, TYPE_INT8)};
    }

    {
        // tests not removing of vector rotations (part 1)
        outIt.emplace(
            std::make_unique<VectorRotation>(t, a, SmallImmediate::fromRotationOffset(3), RotationType::FULL));
        outIt.nextInBlock();
        assign(outIt, u) = t;
    }

    {
        // tests not reordering reading of registers with side-effects (part 2)
        assign(outIt, v) = UNIFORM_REGISTER;
        assign(outIt, w) = a + UNIFORM_REGISTER;
        assign(outIt, x) = v + w;
        assign(outIt, z) = x + UNIFORM_REGISTER;
    }

    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testRemoveFlags()
{
    Configuration config{};
    Module module{config};

    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%dummy").walkEnd();

    // NOTE: Need to execute the optimization pass before adding the expected instructions, since the optimization pass
    // checks for single writer, which we violate by reusing the local across methods!

    auto a = assign(inIt, TYPE_INT32, "%src") = UNIFORM_REGISTER;
    auto b = assign(inIt, TYPE_INT32, "%src") = UNIFORM_REGISTER;

    auto c = inputMethod.addNewLocal(TYPE_INT32, "%c");
    {
        // Remove unused flag setter instruction without side-effects (part 1)
        auto cond = assignNop(inIt) = as_unsigned{a} == as_unsigned{1_val};
        cond = assignNop(inIt) = as_signed{a} == as_signed{b};
        assign(inIt, c) = (a, cond);
    }

    auto d = inputMethod.addNewLocal(TYPE_INT32, "%d");
    {
        // Remove flag bit from unused flag setter with side-effects (part 1)
        assignNop(inIt) = (a ^ 1_val, SetFlag::SET_FLAGS, SIGNAL_LOAD_TMU0);
        auto cond = assignNop(inIt) = as_signed{a} == as_signed{b};
        assign(inIt, d) = (a, cond);
    }

    auto e = inputMethod.addNewLocal(TYPE_INT32, "%e");
    {
        // Remove flag bit from unused flag setter without side-effects writing a local (part 1)
        assign(inIt, e) = (a ^ b, SetFlag::SET_FLAGS);
        auto cond = assignNop(inIt) = as_signed{a} == as_signed{b};
        assign(inIt, e) = (a, cond);
    }

    auto f = inputMethod.addNewLocal(TYPE_INT32, "%f");
    auto phi1 = inputMethod.addNewLocal(TYPE_INT32, "%phi");
    auto phi2 = inputMethod.addNewLocal(TYPE_INT32, "%phi");
    {
        // Make constant conditional instructions unconditional or remove them (part 1)
        auto cond = assignNop(inIt) = as_signed{17_val} != as_signed{42_val};
        assign(inIt, f) = (a, cond);
        assign(inIt, f) = (b, cond.invert());
        assign(inIt, phi1) = (a + b, cond, InstructionDecorations::PHI_NODE);
        assign(inIt, phi2) = (a + b, cond.invert(), InstructionDecorations::PHI_NODE);
        branch(inIt, inputMethod.begin()->getLabel()->getLabel(), cond.toBranchCondition());
    }

    auto g = inputMethod.addNewLocal(TYPE_INT32, "%g");
    {
        // Don't remove flags used by other flag setters (part 1)
        auto cond = assignNop(inIt) = as_unsigned{a} == as_unsigned{1_val};
        assignNop(inIt) = (a ^ b, SetFlag::SET_FLAGS, cond);
        assign(inIt, g) = (b, cond);
    }

    auto h = inputMethod.addNewLocal(TYPE_INT32, "%h");
    {
        // Don't remove conditional instructions depending on non-constant flags (part 1)
        assignNop(inIt) = (a + b, SetFlag::SET_FLAGS);
        assign(inIt, h) = (a, COND_CARRY_SET);
    }

    auto i = inputMethod.addNewLocal(TYPE_INT32.toVectorType(4), "%i");
    {
        // Apply optimization also if not all (but enough) element flags match constantly (part 1)
        assignNop(inIt) = (ELEMENT_NUMBER_REGISTER - 7_val, SetFlag::SET_FLAGS);
        assign(inIt, i) = (a, COND_NEGATIVE_SET);
    }

    auto k = inputMethod.addNewLocal(TYPE_INT32.toVectorType(8), "%k");
    {
        // Don't optimization if not all (and not enough) element flags match constantly (part 1)
        assignNop(inIt) = (ELEMENT_NUMBER_REGISTER - 7_val, SetFlag::SET_FLAGS);
        assign(inIt, k) = (a, COND_NEGATIVE_SET);
        assign(inIt, k) = (b, COND_NEGATIVE_CLEAR);
    }

    // run pass
    removeUselessFlags(module, inputMethod, config);

    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%dummy").walkEnd();

    assign(outIt, a) = UNIFORM_REGISTER;
    assign(outIt, b) = UNIFORM_REGISTER;

    {
        // Remove unused flag setter instruction without side-effects (part 2)
        auto cond = assignNop(outIt) = as_signed{a} == as_signed{b};
        assign(outIt, c) = (a, cond);
    }

    {
        // Remove flag bit from flag setter with side-effects (part 2)
        assignNop(outIt) = (a ^ 1_val, SIGNAL_LOAD_TMU0);
        auto cond = assignNop(outIt) = as_signed{a} == as_signed{b};
        assign(outIt, d) = (a, cond);
    }

    {
        // Remove flag bit from unused flag setter without side-effects writing a local (part 2)
        assign(outIt, e) = (a ^ b);
        auto cond = assignNop(outIt) = as_signed{a} == as_signed{b};
        assign(outIt, e) = (a, cond);
    }

    {
        // Make constant conditional instructions unconditional or remove them (part 2)
        assign(outIt, f) = a;
        assign(outIt, phi1) = (a + b, InstructionDecorations::PHI_NODE);
        assign(outIt, phi2) = (UNDEFINED_VALUE, COND_ZERO_SET, InstructionDecorations::PHI_NODE);
        branch(outIt, outputMethod.begin()->getLabel()->getLabel());
    }

    {
        // Don't remove flags used by other flag setters (part 2)
        auto cond = assignNop(outIt) = as_unsigned{a} == as_unsigned{1_val};
        assignNop(outIt) = (a ^ b, SetFlag::SET_FLAGS, cond);
        assign(outIt, g) = (b, cond);
    }

    {
        // Don't remove conditional instructions depending on non-constant flags (part 2)
        assignNop(outIt) = (a + b, SetFlag::SET_FLAGS);
        assign(outIt, h) = (a, COND_CARRY_SET);
    }

    {
        // Apply optimization also if not all (but enough) element flags match constantly (part 2)
        assign(outIt, i) = a;
    }

    {
        // Don't optimization if not all (and not enough) element flags match constantly (part 2)
        assignNop(outIt) = (ELEMENT_NUMBER_REGISTER - 7_val, SetFlag::SET_FLAGS);
        assign(outIt, k) = (a, COND_NEGATIVE_SET);
        assign(outIt, k) = (b, COND_NEGATIVE_CLEAR);
    }

    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testRemoveConditionalFlags()
{
    Configuration config{};
    Module module{config};

    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.end(), "%dummy").walkEnd();

    // NOTE: Need to execute the optimization pass before adding the expected instructions, since the optimization pass
    // checks for single writer, which we violate by reusing the local across methods!

    auto a = assign(inIt, TYPE_INT32, "%src") = UNIFORM_REGISTER;
    auto b = assign(inIt, TYPE_INT32, "%src") = UNIFORM_REGISTER;

    auto c = inputMethod.addNewLocal(TYPE_BOOL, "%c");
    auto d = inputMethod.addNewLocal(TYPE_INT32, "%d");
    {
        // Remove bool flag setter (zero/non-zero) depending on other scalar flag setter, rewrite conditional
        // instructions (part 1)
        auto cond = assignNop(inIt) = as_signed{a} < as_signed{1_val};
        assign(inIt, c) = (BOOL_TRUE, cond);
        assign(inIt, c) = (BOOL_FALSE, cond.invert());
        assignNop(inIt) = (c, SetFlag::SET_FLAGS);
        assign(inIt, d) = (a, COND_ZERO_CLEAR);
        assign(inIt, d) = (b, COND_ZERO_SET);
        branch(inIt, inputMethod.begin()->getLabel()->getLabel(), BRANCH_ANY_Z_CLEAR);
        branch(inIt, inputMethod.begin()->getLabel()->getLabel(), BRANCH_ALL_Z_SET);
    }

    // run pass
    removeConditionalFlags(module, inputMethod, config);

    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.end(), "%dummy").walkEnd();

    assign(outIt, a) = UNIFORM_REGISTER;
    assign(outIt, b) = UNIFORM_REGISTER;

    {
        // Remove bool flag setter (zero/non-zero) depending on other scalar flag setter, rewrite conditional
        // instructions (part 1)
        auto cond = assignNop(outIt) = as_signed{a} < as_signed{1_val};
        // the bool setter themselves are not removed by this optimization
        assign(outIt, c) = (BOOL_TRUE, cond);
        assign(outIt, c) = (BOOL_FALSE, cond.invert());
        assign(outIt, d) = (a, cond);
        assign(outIt, d) = (b, cond.invert());
        branch(outIt, outputMethod.begin()->getLabel()->getLabel(), BRANCH_ANY_C_SET);
        branch(outIt, outputMethod.begin()->getLabel()->getLabel(), BRANCH_ANY_C_CLEAR);
    }

    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testCombineVectorElementCopies()
{
    Configuration config{};
    Module module{config};
    Method method(module);

    Method inputMethod(module);
    Method outputMethod(module);

    auto inIt = inputMethod.createAndInsertNewBlock(inputMethod.begin(), "%default").walkEnd();
    auto outIt = outputMethod.createAndInsertNewBlock(outputMethod.begin(), "%default").walkEnd();

    // Combine element-wise full copy of vector
    {
        auto inSrc = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(inIt) = selectSIMDElement(i);
            assign(inIt, inDest) = (inSrc, cond, InstructionDecorations::ELEMENT_INSERTION);
        }

        auto outSrc = outputMethod.createLocal(inSrc.type, inSrc.local()->name)->createReference();
        assign(outIt, outSrc) = UNIFORM_REGISTER;
        assignNop(outIt) = (load(0_lit), SetFlag::SET_FLAGS);
        auto outDest = outputMethod.createLocal(inDest.type, inDest.local()->name)->createReference();
        assign(outIt, outDest) = (outSrc, COND_ZERO_SET);
    }

    // Combine element-masked full copy of vector
    {
        auto inSrc = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; i += 4u)
        {
            std::bitset<NATIVE_VECTOR_SIZE> mask{0xFu << i};
            auto cond = assignNop(inIt) = selectSIMDElements(mask);
            assign(inIt, inDest) = (inSrc, cond, InstructionDecorations::ELEMENT_INSERTION);
        }

        auto outSrc = outputMethod.createLocal(inSrc.type, inSrc.local()->name)->createReference();
        assign(outIt, outSrc) = UNIFORM_REGISTER;
        assignNop(outIt) = (load(1_lit), SetFlag::SET_FLAGS);
        auto outDest = outputMethod.createLocal(inDest.type, inDest.local()->name)->createReference();
        assign(outIt, outDest) = (outSrc, COND_ZERO_CLEAR);
    }

    // Combine element-wise partial copy of vector into new vector
    {
        auto inSrc = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        for(uint8_t i = 0; i < 8; ++i)
        {
            auto cond = assignNop(inIt) = selectSIMDElement(i);
            assign(inIt, inDest) = (inSrc, cond, InstructionDecorations::ELEMENT_INSERTION);
        }

        auto outSrc = outputMethod.createLocal(inSrc.type, inSrc.local()->name)->createReference();
        assign(outIt, outSrc) = UNIFORM_REGISTER;
        assignNop(outIt) = (load(0x0000FF00u, LoadType::PER_ELEMENT_SIGNED), SetFlag::SET_FLAGS);
        auto outDest = outputMethod.createLocal(inDest.type, inDest.local()->name)->createReference();
        assign(outIt, outDest) = (outSrc, COND_ZERO_SET);
    }

    // Combine element-wise partial copy of vector into existing vector
    {
        auto inSrc = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest") = 14_val;
        for(uint8_t i = 0; i < 7; ++i)
        {
            auto cond = assignNop(inIt) = selectSIMDElement(i);
            assign(inIt, inDest) = (inSrc, cond, InstructionDecorations::ELEMENT_INSERTION);
        }

        auto outSrc = outputMethod.createLocal(inSrc.type, inSrc.local()->name)->createReference();
        assign(outIt, outSrc) = UNIFORM_REGISTER;
        auto outDest = outputMethod.createLocal(inDest.type, inDest.local()->name)->createReference();
        assign(outIt, outDest) = 14_val;
        assignNop(outIt) = (load(0x0000FF80u, LoadType::PER_ELEMENT_SIGNED), SetFlag::SET_FLAGS);
        assign(outIt, outDest) = (outSrc, COND_ZERO_SET);
    }

    // Combine element-wise copy of vector with identical (un)pack modes
    {
        auto inSrc = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(inIt) = selectSIMDElement(i);
            assign(inIt, inDest) = (inSrc, cond, UNPACK_16A_32);
        }

        auto outSrc = outputMethod.createLocal(inSrc.type, inSrc.local()->name)->createReference();
        assign(outIt, outSrc) = UNIFORM_REGISTER;
        assignNop(outIt) = (load(0_lit), SetFlag::SET_FLAGS);
        auto outDest = outputMethod.createLocal(inDest.type, inDest.local()->name)->createReference();
        assign(outIt, outDest) = (outSrc, COND_ZERO_SET, UNPACK_16A_32);
    }

    {
        auto inSrc = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(inIt) = selectSIMDElement(i);
            assign(inIt, inDest) = (inSrc, cond, PACK_32_16A);
        }

        auto outSrc = outputMethod.createLocal(inSrc.type, inSrc.local()->name)->createReference();
        assign(outIt, outSrc) = UNIFORM_REGISTER;
        assignNop(outIt) = (load(0_lit), SetFlag::SET_FLAGS);
        auto outDest = outputMethod.createLocal(inDest.type, inDest.local()->name)->createReference();
        assign(outIt, outDest) = (outSrc, COND_ZERO_SET, PACK_32_16A);
    }

    // Do not combine element-wise copy of vector with different (un)pack modes
    {
        auto inSrc = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(inIt) = selectSIMDElement(i);
            auto mode = i % 3 == 0 ? UNPACK_16A_32 : i % 3 == 1 ? UNPACK_8888_32 : UNPACK_NOP;
            assign(inIt, inDest) = (inSrc, cond, mode);
        }

        auto outSrc = outputMethod.createLocal(inSrc.type, inSrc.local()->name)->createReference();
        assign(outIt, outSrc) = UNIFORM_REGISTER;
        auto outDest = outputMethod.createLocal(inDest.type, inDest.local()->name)->createReference();
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(outIt) = selectSIMDElement(i);
            auto mode = i % 3 == 0 ? UNPACK_16A_32 : i % 3 == 1 ? UNPACK_8888_32 : UNPACK_NOP;
            assign(outIt, outDest) = (outSrc, cond, mode);
        }
    }

    {
        auto inSrc = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(inIt) = selectSIMDElement(i);
            auto mode = i % 3 == 0 ? PACK_32_16A : i % 3 == 1 ? PACK_32_8888 : PACK_NOP;
            assign(inIt, inDest) = (inSrc, cond, mode);
        }

        auto outSrc = outputMethod.createLocal(inSrc.type, inSrc.local()->name)->createReference();
        assign(outIt, outSrc) = UNIFORM_REGISTER;
        auto outDest = outputMethod.createLocal(inDest.type, inDest.local()->name)->createReference();
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(outIt) = selectSIMDElement(i);
            auto mode = i % 3 == 0 ? PACK_32_16A : i % 3 == 1 ? PACK_32_8888 : PACK_NOP;
            assign(outIt, outDest) = (outSrc, cond, mode);
        }
    }

    // Do not combine element-wise copy of vector with different sources
    {
        auto inSrc0 = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inSrc1 = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(inIt) = selectSIMDElement(i);
            assign(inIt, inDest) = (i % 2 ? inSrc0 : inSrc1, cond, InstructionDecorations::ELEMENT_INSERTION);
        }

        auto outSrc0 = outputMethod.createLocal(inSrc0.type, inSrc0.local()->name)->createReference();
        assign(outIt, outSrc0) = UNIFORM_REGISTER;
        auto outSrc1 = outputMethod.createLocal(inSrc1.type, inSrc1.local()->name)->createReference();
        assign(outIt, outSrc1) = UNIFORM_REGISTER;
        auto outDest = outputMethod.createLocal(inDest.type, inDest.local()->name)->createReference();
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(outIt) = selectSIMDElement(i);
            assign(outIt, outDest) = (i % 2 ? outSrc0 : outSrc1, cond, InstructionDecorations::ELEMENT_INSERTION);
        }
    }

    // Do not combine element-wise copy of vector with different destinations
    {
        auto inSrc = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest0 = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        auto inDest1 = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(inIt) = selectSIMDElement(i);
            assign(inIt, i % 2 ? inDest0 : inDest1) = (inSrc, cond, InstructionDecorations::ELEMENT_INSERTION);
        }

        auto outSrc = outputMethod.createLocal(inSrc.type, inSrc.local()->name)->createReference();
        assign(outIt, outSrc) = UNIFORM_REGISTER;
        auto outDest0 = outputMethod.createLocal(inDest0.type, inDest0.local()->name)->createReference();
        auto outDest1 = outputMethod.createLocal(inDest1.type, inDest1.local()->name)->createReference();
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(outIt) = selectSIMDElement(i);
            assign(outIt, i % 2 ? outDest0 : outDest1) = (outSrc, cond, InstructionDecorations::ELEMENT_INSERTION);
        }
    }

    // Do not combine element-wise copy of vector if copies have side-effects
    {
        auto inSrc = assign(inIt, TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%src") = UNIFORM_REGISTER;
        auto inDest = inputMethod.addNewLocal(TYPE_INT32.toVectorType(NATIVE_VECTOR_SIZE), "%dest");
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(inIt) = selectSIMDElement(i);
            assign(inIt, inDest) = (inSrc, cond, SIGNAL_LOAD_TMU0);
        }

        auto outSrc = outputMethod.createLocal(inSrc.type, inSrc.local()->name)->createReference();
        assign(outIt, outSrc) = UNIFORM_REGISTER;
        auto outDest = outputMethod.createLocal(inDest.type, inDest.local()->name)->createReference();
        for(uint8_t i = 0; i < NATIVE_VECTOR_SIZE; ++i)
        {
            auto cond = assignNop(outIt) = selectSIMDElement(i);
            assign(outIt, outDest) = (outSrc, cond, SIGNAL_LOAD_TMU0);
        }
    }

    // run pass
    combineVectorElementCopies(module, inputMethod, config);
    testMethodsEquals(inputMethod, outputMethod);
}

void TestOptimizationSteps::testLoopInvariantCodeMotion()
{
    using namespace vc4c::intermediate;
    Configuration config{};
    Module module{config};
    Method method(module);

    auto it = method.createAndInsertNewBlock(method.end(), "%dummy").walkEnd();

    auto& outerLoop = insertLoop(method, it, BOOL_TRUE, "%outerLoop");
    it = outerLoop.walk().nextInBlock();

    FastAccessList<const IntermediateInstruction*> firstHoistedInstructions;
    FastAccessList<const IntermediateInstruction*> secondHoistedInstructions;
    auto out0 = method.addNewLocal(TYPE_INT32, "%out0");
    auto out1 = method.addNewLocal(TYPE_INT32, "%out1");
    auto out2 = method.addNewLocal(TYPE_INT32, "%out2");
    auto out3 = method.addNewLocal(TYPE_INT32, "%out3");

    // loading of constants
    {
        it.emplace(std::make_unique<LoadImmediate>(out0, Literal(12345)));
        firstHoistedInstructions.emplace_back(it.get());
        it.nextInBlock();
    }

    // constant operation
    {
        it.emplace(std::make_unique<Operation>(OP_ADD, out1, out0, INT_ONE));
        firstHoistedInstructions.emplace_back(it.get());
        it.nextInBlock();
    }

    auto& innerLoop = insertLoop(method, it, BOOL_TRUE, "%innerLoop");
    it = innerLoop.walk().nextInBlock();

    // loading of constant in inner loop
    {
        it.emplace(std::make_unique<LoadImmediate>(out2, Literal(42)));
        secondHoistedInstructions.emplace_back(it.get());
        it.nextInBlock();
    }

    // calculation depending on value of outer loop
    {
        it.emplace(std::make_unique<Operation>(OP_ADD, out3, out0, out2));
        secondHoistedInstructions.emplace_back(it.get());
        it.nextInBlock();
    }

    // non-constant operation which is not moved
    {
        it.emplace(std::make_unique<Operation>(OP_SUB, NOP_REGISTER, out3, UNIFORM_REGISTER));
        it.nextInBlock();
    }

    // run pass
    moveLoopInvariantCode(module, method, config);

    // NOTE: We need to check the same method, since we cannot compare methods, since the loop creates unique labels
    // that we cannot reproduce identically.

    it = method.walkAllInstructions();

    // %dummy label
    TEST_ASSERT(!!it.get<BranchLabel>());
    TEST_ASSERT_EQUALS("%dummy", it.get<BranchLabel>()->getLabel()->name);
    it.nextInMethod();

    // here the instructions from the first loop are hoisted into
    for(auto inst : firstHoistedInstructions)
    {
        if(it.get() != inst)
        {
            TEST_ASSERT_EQUALS(inst->to_string(), it->to_string());
        }
        it.nextInMethod();
    }

    // outer loop header label
    TEST_ASSERT(!!it.get<BranchLabel>());
    TEST_ASSERT(it.get<BranchLabel>()->getLabel()->name.find("outerLoop") != std::string::npos);
    TEST_ASSERT(it.get<BranchLabel>()->getLabel()->name.find("header") != std::string::npos);
    it.nextInMethod();

    // outer loop content label
    it = outerLoop.walk();
    TEST_ASSERT(!!it.get<BranchLabel>());
    TEST_ASSERT(it.get<BranchLabel>()->getLabel()->name.find("outerLoop") != std::string::npos);
    TEST_ASSERT(it.get<BranchLabel>()->getLabel()->name.find("header") == std::string::npos);

    it.nextInMethod();
    // here the instructions from the second loop are hoisted into
    for(auto inst : secondHoistedInstructions)
    {
        if(it.get() != inst)
        {
            TEST_ASSERT_EQUALS(inst->to_string(), it->to_string());
        }
        it.nextInMethod();
    }

    // the inner loop has now only label, single non-moved instruction and unconditional branch to header
    TEST_ASSERT_EQUALS(3u, innerLoop.size());
    it = innerLoop.walk();
    TEST_ASSERT(!!it.get<BranchLabel>());
    TEST_ASSERT(it.get<BranchLabel>()->getLabel()->name.find("innerLoop") != std::string::npos);
    TEST_ASSERT(it.get<BranchLabel>()->getLabel()->name.find("header") == std::string::npos);
    it.nextInMethod();
    TEST_ASSERT(it->getOutput().value() == NOP_REGISTER);
    it.nextInMethod();
    TEST_ASSERT(!!it.get<Branch>());
}
