/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "RegisterAnalysis.h"

#include <sstream>

using namespace vc4c;
using namespace vc4c::analysis;

UsedElementsAnalysis::UsedElementsAnalysis() :
    LocalAnalysis(UsedElementsAnalysis::analyzeUsedSIMDElements, UsedElementsAnalysis::to_string)
{
}

// Taken from https://stackoverflow.com/questions/25249929/how-to-rotate-the-bits-of-given-number
template <typename T>
static constexpr T rotate_left(T value, unsigned char count)
{
    return static_cast<T>((value << count) | (value >> (sizeof(T) * 8 - count)));
}

UsedElements UsedElementsAnalysis::analyzeUsedSIMDElements(
    const intermediate::IntermediateInstruction* inst, const UsedElements& nextValues, ConditionalUsages& cache)
{
    UsedElements values(nextValues);
    if(inst && !dynamic_cast<const intermediate::BranchLabel*>(inst))
    {
        if(auto combined = dynamic_cast<const intermediate::CombinedOperation*>(inst))
        {
            auto first =
                combined->op1 ? analyzeUsedSIMDElements(combined->op1.get(), nextValues, cache) : UsedElements{};
            auto second =
                combined->op2 ? analyzeUsedSIMDElements(combined->op2.get(), nextValues, cache) : UsedElements{};
            // since both instructions can add and erase elements, we need to create the intersection
            auto it = first.begin();
            while(it != first.end())
            {
                auto it2 = second.find(it->first);
                if(it2 == second.end())
                    it = first.erase(it);
                else
                {
                    it->second |= it2->second;
                    ++it;
                }
            }
            for(const auto& elem : second)
            {
                it = first.find(elem.first);
                if(it == first.end())
                    first.emplace(elem);
            }
            return first;
        }
        if(inst->checkOutputLocal() &&
            (!inst->hasConditionalExecution() ||
                (cache.find(inst->getOutput()->local()) != cache.end() &&
                    cache.at(inst->getOutput()->local()).isInversionOf(inst->conditional))))
        {
            // TODO for exact check, would also need to compare the instruction setting the flags
            values.erase(inst->getOutput()->local());
        }
        else if(inst->checkOutputLocal() && inst->hasConditionalExecution())
        {
            auto it = cache.find(inst->getOutput()->local());
            if(it != cache.end() && it->second.isInversionOf(inst->conditional))
                it->second = COND_ALWAYS;
            else
                cache.emplace(inst->getOutput()->local(), inst->conditional);
        }
        UsedElements newValues;
        if(inst->writesRegister(REG_VPM_DMA_LOAD_ADDR) || inst->writesRegister(REG_VPM_DMA_STORE_ADDR) ||
            inst->writesRegister(REG_VPM_IN_SETUP) || inst->writesRegister(REG_VPM_OUT_SETUP) ||
            inst->writesRegister(REG_REPLICATE_ALL) || inst->writesRegister(REG_TMU_NOSWAP) ||
            inst->writesRegister(REG_UNIFORM_ADDRESS) || inst->writesRegister(REG_REV_FLAG))
        {
            // only 0st element used
            inst->forUsedLocals([&](const Local* loc, LocalUse::Type type) {
                if(has_flag(type, LocalUse::Type::READER))
                {
                    newValues[loc].set(0);
                }
            });
        }
        else if(inst->writesRegister(REG_REPLICATE_QUAD))
        {
            // only 0st, 4st, 8th and 12th element is used
            inst->forUsedLocals([&](const Local* loc, LocalUse::Type type) {
                if(has_flag(type, LocalUse::Type::READER))
                {
                    auto& elements = newValues[loc];
                    elements.set(0);
                    elements.set(4);
                    elements.set(8);
                    elements.set(12);
                }
            });
        }
        else if(auto branch = dynamic_cast<const intermediate::Branch*>(inst))
        {
            // XXX branch target input (currently not used), is only set by SIMD element 15
            if(branch->hasConditionalExecution() && branch->getCondition().checkLocal())
            {
                auto it = cache.find(branch->getCondition().local());
                if(it != cache.end() && it->second.isInversionOf(branch->conditional))
                    it->second = COND_ALWAYS;
                else
                    cache.emplace(branch->getCondition().local(), branch->conditional);
            }
        }
        else
        {
            // by default, we need all the elements our outputs need
            // if we do not know the mask, assume all elements are needed
            auto outIt = inst->checkOutputLocal() ? nextValues.find(inst->getOutput()->local()) : nextValues.end();
            inst->forUsedLocals([&](const Local* loc, LocalUse::Type type) {
                if(has_flag(type, LocalUse::Type::READER))
                {
                    if(outIt != nextValues.end())
                        newValues[loc] |= outIt->second;
                    else if(values.find(loc) != values.end())
                        // we know the elements the local will be used afterwards TODO is this correct?
                        newValues[loc] |= 0;
                    else if(inst->doesSetFlag() && inst->writesRegister(REG_NOP))
                        // is handled by block below
                        newValues[loc] |= 0;
                    else
                        // we don't know anything about the usage, assume all elements
                        newValues[loc] |= 0xFFFF;
                }
            });
        }
        if(auto rot = dynamic_cast<const intermediate::VectorRotation*>(inst))
        {
            if(rot->getOffset().hasRegister(REG_ACC5))
            {
                // we don't know the exact offset, so reserve all
                for(auto& val : newValues)
                    val.second.set();
            }
            else if(rot->getOffset().immediate().getRotationOffset())
            {
                // we rotate all the used elements by the offset (if known)
                for(auto& val : newValues)
                {
                    // TODO rotation in the correct (opposite to actual vector rotation) direction?
                    uint16_t tmp = static_cast<uint16_t>(val.second.to_ulong());
                    tmp = rotate_left(tmp, rot->getOffset().immediate().getRotationOffset().value());
                    val.second = tmp;
                }
            }
        }
        if(inst->doesSetFlag())
        {
            // all conditionals which come afterwards are handled
            static const auto zeroCheck = [](const std::pair<const Local*, ConditionCode>& entry) -> bool {
                return entry.second == COND_ZERO_CLEAR || entry.second == COND_ZERO_SET || entry.second == COND_ALWAYS;
            };
            static const auto literalImmediateCheck = [](const Value& arg) -> bool {
                return arg.getLiteralValue().has_value();
            };
            auto op = dynamic_cast<const intermediate::Operation*>(inst);
            if(op && std::all_of(cache.begin(), cache.end(), zeroCheck))
            {
                if(op->op == OP_OR && op->readsRegister(REG_ELEMENT_NUMBER))
                {
                    // if all conditions in cache are for zero set/clear, only the first element is of any meaning
                    // (since the others are never zero as of the register-number) e.g. for setting branch flags
                    inst->forUsedLocals([&](const Local* loc, LocalUse::Type type) {
                        if(has_flag(type, LocalUse::Type::READER))
                        {
                            newValues[loc] = 0x1;
                        }
                    });
                }
                else if(op->op == OP_XOR && op->readsRegister(REG_ELEMENT_NUMBER) &&
                    std::any_of(inst->getArguments().begin(), inst->getArguments().end(), literalImmediateCheck))
                {
                    // only the element with the given value is set (only if all conditions are checks for zero
                    // set/clear)
                    auto lit = op->getFirstArg().getLiteralValue() ? op->getFirstArg().getLiteralValue() :
                                                                     op->assertArgument(1).getLiteralValue();
                    inst->forUsedLocals([&](const Local* loc, LocalUse::Type type) {
                        if(has_flag(type, LocalUse::Type::READER))
                        {
                            newValues[loc] = (1 << lit->unsignedInt());
                        }
                    });
                }
                else if(op->op == OP_SUB && op->getFirstArg().hasRegister(REG_ELEMENT_NUMBER) &&
                    (op->getSecondArg() & &Value::getLiteralValue))
                {
                    // only the first n elements are set where n is the literal (only if all conditions are checks
                    // for zero set/clear)
                    auto lit = op->assertArgument(1).getLiteralValue();
                    inst->forUsedLocals([&](const Local* loc, LocalUse::Type type) {
                        if(has_flag(type, LocalUse::Type::READER))
                        {
                            newValues[loc] = (1 << lit->unsignedInt()) - 1;
                        }
                    });
                }
                else if(inst->writesRegister(REG_NOP))
                {
                    // check the conditionally set locals for the elements that are used
                    auto mask = std::accumulate(cache.begin(), cache.end(), std::bitset<16>{},
                        [&](std::bitset<16> in,
                            const std::pair<const Local*, ConditionCode>& entry) -> std::bitset<16> {
                            auto locIt = nextValues.find(entry.first);
                            return in | (locIt != nextValues.end() ? locIt->second : std::bitset<16>{0xFFFF});
                        });
                    inst->forUsedLocals([&](const Local* loc, LocalUse::Type type) {
                        if(has_flag(type, LocalUse::Type::READER))
                        {
                            newValues[loc] |= mask;
                        }
                    });
                }
            }
            cache.clear();
            if(inst->checkOutputLocal() && !inst->hasConditionalExecution())
                cache.emplace(inst->getOutput()->local(), inst->conditional);
        }
        for(const auto& val : newValues)
            values[val.first] |= val.second;
    }

    return values;
}

LCOV_EXCL_START
std::string UsedElementsAnalysis::to_string(const UsedElements& registerElements)
{
    if(registerElements.empty())
        return "";
    std::stringstream s;
    auto it = registerElements.begin();
    s << it->first->to_string() << " (" << it->second.to_string() << ")";
    ++it;
    for(; it != registerElements.end(); ++it)
        s << ", " << it->first->to_string() << " (" << it->second.to_string() << ")";
    return s.str();
}
LCOV_EXCL_STOP
