/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#include "TestOptimizationSteps.h"

#include "Expression.h"
#include "Method.h"
#include "Module.h"
#include "intermediate/operators.h"
#include "optimization/Combiner.h"
#include "optimization/Eliminator.h"
#include "optimization/Flags.h"

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

    while(!inIt.isEndOfBlock() && !outIt.isEndOfBlock())
    {
        if(!checkEquals(inIt.get(), outIt.get()))
        {
            TEST_ASSERT_EQUALS(outIt.has() ? outIt->to_string() : "(null)", inIt.has() ? inIt->to_string() : "(null)")
        }
        inIt.nextInBlock();
        outIt.nextInBlock();
    }
    TEST_ASSERT(inIt.isEndOfBlock())
    TEST_ASSERT(outIt.isEndOfBlock())
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
    inIt.emplace(new Operation(OP_ADD, NOP_REGISTER, 17_val, 5_val));
    inIt.nextInBlock();
    assignNop(outIt) = 22_val;

    // transfer signal
    inIt.emplace(new Operation(OP_ADD, NOP_REGISTER, 17_val, 7_val));
    inIt->setSignaling(SIGNAL_LOAD_ALPHA);
    inIt.nextInBlock();
    assignNop(outIt) = (24_val, SIGNAL_LOAD_ALPHA);

    // transfer conditional
    inIt.emplace(new Operation(OP_ADD, NOP_REGISTER, 17_val, 8_val, COND_CARRY_CLEAR));
    inIt.nextInBlock();
    assignNop(outIt) = (25_val, COND_CARRY_CLEAR);

    // transfer flags
    inIt.emplace(new Operation(OP_ADD, NOP_REGISTER, 17_val, 4_val));
    inIt->setSetFlags(SetFlag::SET_FLAGS);
    inIt.nextInBlock();
    assignNop(outIt) = (21_val, SetFlag::SET_FLAGS);

    // transfer decorations
    inIt.emplace(new Operation(OP_SUB, NOP_REGISTER, 17_val, 4_val));
    inIt->addDecorations(InstructionDecorations::UNSIGNED_RESULT);
    inIt.nextInBlock();
    assignNop(outIt) = (13_val, InstructionDecorations::UNSIGNED_RESULT);

    // transfer pack mode
    inIt.emplace(new Operation(OP_SUB, NOP_REGISTER, 17_val, 17_val));
    inIt->setPackMode(PACK_32_16A_S);
    inIt.nextInBlock();
    assignNop(outIt) = (0_val, PACK_32_16A_S);

    // non-constants not folded
    inIt.emplace(new Operation(OP_ADD, NOP_REGISTER, 17_val, UNIFORM_REGISTER));
    inIt.nextInBlock();
    assignNop(outIt) = 17_val + UNIFORM_REGISTER;

    // unpack modes not folded (don't know which input to unpack)
    inIt.emplace(new Operation(OP_ADD, NOP_REGISTER, 17_val, 11_val));
    inIt->setUnpackMode(UNPACK_16A_32);
    inIt.nextInBlock();
    assignNop(outIt) = (17_val + 11_val, UNPACK_16A_32);

    // a xor a (cond) not folded (so it can be reused by different optimization
    inIt.emplace(new Operation(OP_XOR, NOP_REGISTER, 11_val, 11_val, COND_NEGATIVE_CLEAR));
    inIt.nextInBlock();
    assignNop(outIt) = (11_val ^ 11_val, COND_NEGATIVE_CLEAR);

    // a xor a (always) folded
    inIt.emplace(new Operation(OP_XOR, NOP_REGISTER, 17_val, 17_val));
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
    // TODO isn't this already covered (mostly) by TestInstructions#testOpCodeProperties() ??
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
            inIt.emplace(new Operation(op, intermediate, local, oneConst));
            inIt.nextInBlock();
            inIt.emplace(new Operation(op, out, intermediate, otherConst));
            inIt.nextInBlock();
            outIt.emplace(new Operation(op, out, local, op(oneConst, otherConst).first.value()));
            outIt.nextInBlock();

            // constA op (constB op c) -> (constA op constB) op c
            intermediate = inputMethod.addNewLocal(returnType, op.name);
            out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(new Operation(op, intermediate, otherConst, local));
            inIt.nextInBlock();
            inIt.emplace(new Operation(op, out, oneConst, intermediate));
            inIt.nextInBlock();
            if(op.isCommutative())
                outIt.emplace(new Operation(op, out, local, op(oneConst, otherConst).first.value()));
            else
                outIt.emplace(new Operation(op, out, op(oneConst, otherConst).first.value(), local));
            outIt.nextInBlock();

            if(op.isCommutative())
            {
                // for all additionally commutative operations:
                // (constA op b) op constC -> (constA op constC) op b
                intermediate = inputMethod.addNewLocal(returnType, op.name);
                out = inputMethod.addNewLocal(returnType, op.name);
                inIt.emplace(new Operation(op, intermediate, oneConst, local));
                inIt.nextInBlock();
                inIt.emplace(new Operation(op, out, intermediate, otherConst));
                inIt.nextInBlock();
                outIt.emplace(new Operation(op, out, local, op(oneConst, otherConst).first.value()));
                outIt.nextInBlock();

                // constA op (b op constC) -> b op (constA op constC)
                intermediate = inputMethod.addNewLocal(returnType, op.name);
                out = inputMethod.addNewLocal(returnType, op.name);
                inIt.emplace(new Operation(op, intermediate, local, otherConst));
                inIt.nextInBlock();
                inIt.emplace(new Operation(op, out, oneConst, intermediate));
                inIt.nextInBlock();
                outIt.emplace(new Operation(op, out, local, op(oneConst, otherConst).first.value()));
                outIt.nextInBlock();
            }
        }
        else if(op == OP_SHL || op == OP_SHR || op == OP_ROR || op == OP_ROR)
        {
            // for all shifts
            // (a op constB) op constC -> a op (constB + constC)
            auto intermediate = inputMethod.addNewLocal(returnType, op.name);
            auto out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(new Operation(op, intermediate, local, oneConst));
            inIt.nextInBlock();
            inIt.emplace(new Operation(op, out, intermediate, otherConst));
            inIt.nextInBlock();
            outIt.emplace(new Operation(op, out, local, OP_ADD(oneConst, otherConst).first.value()));
            outIt.nextInBlock();

            // a op (constB op constC) -> unchanged
            intermediate = inputMethod.addNewLocal(returnType, op.name);
            out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(new Operation(op, intermediate, oneConst, otherConst));
            inIt.nextInBlock();
            inIt.emplace(new Operation(op, out, local, intermediate));
            inIt.nextInBlock();
            outIt.emplace(new Operation(op, intermediate, oneConst, otherConst));
            outIt.nextInBlock();
            outIt.emplace(new Operation(op, out, local, intermediate));
            outIt.nextInBlock();

            // (constA op b) op constC -> unchanged
            intermediate = inputMethod.addNewLocal(returnType, op.name);
            out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(new Operation(op, intermediate, oneConst, local));
            inIt.nextInBlock();
            inIt.emplace(new Operation(op, out, intermediate, otherConst));
            inIt.nextInBlock();
            outIt.emplace(new Operation(op, intermediate, oneConst, local));
            outIt.nextInBlock();
            outIt.emplace(new Operation(op, out, intermediate, otherConst));
            outIt.nextInBlock();
        }
        else if(op.numOperands == 2)
        {
            // any other operation -> not combined
            auto intermediate = inputMethod.addNewLocal(returnType, op.name);
            auto out = inputMethod.addNewLocal(returnType, op.name);
            inIt.emplace(new Operation(op, intermediate, local, oneConst));
            inIt.nextInBlock();
            inIt.emplace(new Operation(op, out, intermediate, otherConst));
            inIt.nextInBlock();
            outIt.emplace(new Operation(op, intermediate, local, oneConst));
            outIt.nextInBlock();
            outIt.emplace(new Operation(op, out, intermediate, otherConst));
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
