/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "WorkItemAnalysis.h"

#include "../Logger.h"
#include "../Method.h"
#include "../Profiler.h"
#include "../helper.h"
#include "../intermediate/Helper.h"
#include "ControlFlowGraph.h"
#include "FlagsAnalysis.h"

#include <algorithm>
#include <queue>

using namespace vc4c;
using namespace vc4c::analysis;

bool ActiveWorkItems::isWorkItemCondition() const noexcept
{
    switch(condition)
    {
    case WorkItemCondition::LOCAL_ID_X:
    case WorkItemCondition::LOCAL_ID_Y:
    case WorkItemCondition::LOCAL_ID_Z:
    case WorkItemCondition::LOCAL_ID_SCALAR:
    case WorkItemCondition::GLOBAL_ID_X:
    case WorkItemCondition::GLOBAL_ID_Y:
    case WorkItemCondition::GLOBAL_ID_Z:
        return true;
    default:
        return false;
    }
}

Optional<ActiveWorkItems> ActiveWorkItems::mergeWith(const ActiveWorkItems& other) const
{
    if(condition != other.condition)
    {
        // If the conditions differ, we can never merge.
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Cannot merge work-item conditions '" << to_string() << "' and '" << other.to_string()
                << "', assuming no condition!" << logging::endl);
        return {};
    }

    auto result = *this;
    result.activeElements.insert(other.activeElements.begin(), other.activeElements.end());
    result.inactiveElements.insert(other.inactiveElements.begin(), other.inactiveElements.end());

    if(!result.activeElements.empty() && !result.inactiveElements.empty())
    {
        if(result.activeElements == result.inactiveElements)
        {
            // If the included and excluded elements are identical, the conditions does not constraint anything anymore.
            // In words: If one path allows elements A, B and C and the other path allows all elements except A, B and
            // C, together they allow all elements.
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Merged work-item conditions '" << to_string() << "' and '" << other.to_string()
                    << "' cancel each other out" << logging::endl);
            return {};
        }

        if(std::includes(result.inactiveElements.begin(), result.inactiveElements.end(), result.activeElements.begin(),
               result.activeElements.end()))
        {
            // If all elements included by one are excluded by the other, remove explicit include/exclude of matching
            // values. In words: If one path allows elements A, B and C and the other path allows all but A, B, C and D,
            // together they allow all but D.
            for(auto element : result.activeElements)
                result.inactiveElements.erase(element);
            result.activeElements.clear();
        }
        else
        {
            // XXX If we have the same condition, but one is exclusive and the other inclusive, we could merge, if we
            // knew the set of all possible values (which we do at least for local IDs).
            // XXX When merging, be pessimistic, add every entry as active, which is at least active in one of the
            // objects!
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Cannot merge work-item conditions '" << to_string() << "' and '" << other.to_string()
                    << "', assuming no condition!" << logging::endl);
            return {};
        }
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Merged work-item conditions '" << to_string() << "' and '" << other.to_string()
            << "' into: " << result.to_string() << logging::endl);

    return result;
}

LCOV_EXCL_START
static std::string toString(WorkItemCondition cond)
{
    switch(cond)
    {
    case WorkItemCondition::LOCAL_ID_X:
        return "local IDs X";
    case WorkItemCondition::LOCAL_ID_Y:
        return "local IDs Y";
    case WorkItemCondition::LOCAL_ID_Z:
        return "local IDs Z";
    case WorkItemCondition::LOCAL_ID_SCALAR:
        return "local IDs scalar";
    case WorkItemCondition::LOCAL_SIZE_X:
        return "local sizes X";
    case WorkItemCondition::LOCAL_SIZE_Y:
        return "local sizes Y";
    case WorkItemCondition::LOCAL_SIZE_Z:
        return "local sizes Z";
    case WorkItemCondition::LOCAL_SIZE_SCALAR:
        return "local sizes scalar";
    case WorkItemCondition::GLOBAL_ID_X:
        return "global IDs X";
    case WorkItemCondition::GLOBAL_ID_Y:
        return "global IDs Y";
    case WorkItemCondition::GLOBAL_ID_Z:
        return "global IDs Z";
    case WorkItemCondition::GROUP_ID_X:
        return "group IDs X";
    case WorkItemCondition::GROUP_ID_Y:
        return "group IDs Y";
    case WorkItemCondition::GROUP_ID_Z:
        return "group IDs Z";
    }
    throw CompilationError(CompilationStep::GENERAL, "Unhandled work-item condition for basic block",
        std::to_string(static_cast<unsigned>(cond)));
}

std::string ActiveWorkItems::to_string() const
{
    auto toElementString = [](std::size_t element) { return std::to_string(element); };
    auto result = ::toString(condition);
    if(!activeElements.empty())
        result += " included: " + vc4c::to_string<std::size_t>(activeElements, toElementString);
    if(!inactiveElements.empty())
        result += " excluded: " + vc4c::to_string<std::size_t>(inactiveElements, toElementString);
    return result;
}
LCOV_EXCL_STOP

static Optional<ActiveWorkItems> getGlobalMask(const Method& method)
{
    Optional<ActiveWorkItems> globalMask{};
    uint32_t maxSize = 0;
    // TODO additionally set (how to represent??) local_sizes limits!
    if(method.metaData.workGroupSizes[0] >= 1 && method.metaData.workGroupSizes[1] <= 1 &&
        method.metaData.workGroupSizes[2] <= 1)
    {
        globalMask = ActiveWorkItems{WorkItemCondition::LOCAL_ID_X, {}, {}};
        maxSize = method.metaData.workGroupSizes[0];
    }
    else if(method.metaData.workGroupSizes[0] <= 1 && method.metaData.workGroupSizes[1] >= 1 &&
        method.metaData.workGroupSizes[2] <= 1)
    {
        globalMask = ActiveWorkItems{WorkItemCondition::LOCAL_ID_Y, {}, {}};
        maxSize = method.metaData.workGroupSizes[1];
    }
    else if(method.metaData.workGroupSizes[0] <= 1 && method.metaData.workGroupSizes[1] <= 1 &&
        method.metaData.workGroupSizes[2] > 0)
    {
        globalMask = ActiveWorkItems{WorkItemCondition::LOCAL_ID_Z, {}, {}};
        maxSize = method.metaData.workGroupSizes[2];
    }
    else if(auto localSize = method.metaData.getFixedWorkGroupSize())
    {
        globalMask = ActiveWorkItems{WorkItemCondition::LOCAL_ID_SCALAR, {}, {}};
        maxSize = *localSize;
    }
    if(globalMask && maxSize > 0)
    {
        for(uint32_t i = 0; i < maxSize; ++i)
            globalMask->activeElements.emplace(i);
    }
    return globalMask;
}

static Optional<WorkItemCondition> findWorkItemCondition(const Value& value)
{
    auto loc = value.checkLocal();
    if(!loc)
        return {};

    if(auto builtin = loc->as<BuiltinLocal>())
    {
        switch(builtin->builtinType)
        {
        case BuiltinLocal::Type::GROUP_ID_X:
            return WorkItemCondition::GROUP_ID_X;
        case BuiltinLocal::Type::GROUP_ID_Y:
            return WorkItemCondition::GROUP_ID_Y;
        case BuiltinLocal::Type::GROUP_ID_Z:
            return WorkItemCondition::GROUP_ID_Z;
        default:; // handle afterwards
        }
    }

    auto writer = loc->getSingleWriter();
    if(!writer)
        return {};

    if(writer->hasDecoration(intermediate::InstructionDecorations::BUILTIN_LOCAL_ID))
    {
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_X))
            return WorkItemCondition::LOCAL_ID_X;
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_Y))
            return WorkItemCondition::LOCAL_ID_Y;
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_Z))
            return WorkItemCondition::LOCAL_ID_Z;
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_SCALAR))
            return WorkItemCondition::LOCAL_ID_SCALAR;

        auto unpackingInst = dynamic_cast<const intermediate::UnpackingInstruction*>(writer);
        if(unpackingInst && unpackingInst->getUnpackMode() == UNPACK_8A_32)
            return WorkItemCondition::LOCAL_ID_X;
        if(unpackingInst && unpackingInst->getUnpackMode() == UNPACK_8B_32)
            return WorkItemCondition::LOCAL_ID_Y;
        if(unpackingInst && unpackingInst->getUnpackMode() == UNPACK_8C_32)
            return WorkItemCondition::LOCAL_ID_Z;
        return {};
    }
    else if(writer->hasDecoration(intermediate::InstructionDecorations::BUILTIN_LOCAL_SIZE))
    {
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_X))
            return WorkItemCondition::LOCAL_SIZE_X;
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_Y))
            return WorkItemCondition::LOCAL_SIZE_Y;
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_Z))
            return WorkItemCondition::LOCAL_SIZE_Z;
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_SCALAR))
            return WorkItemCondition::LOCAL_SIZE_SCALAR;

        auto unpackingInst = dynamic_cast<const intermediate::UnpackingInstruction*>(writer);
        if(unpackingInst && unpackingInst->getUnpackMode() == UNPACK_8A_32)
            return WorkItemCondition::LOCAL_SIZE_X;
        if(unpackingInst && unpackingInst->getUnpackMode() == UNPACK_8B_32)
            return WorkItemCondition::LOCAL_SIZE_Y;
        if(unpackingInst && unpackingInst->getUnpackMode() == UNPACK_8C_32)
            return WorkItemCondition::LOCAL_SIZE_Z;
        return {};
    }
    else if(writer->hasDecoration(intermediate::InstructionDecorations::BUILTIN_GLOBAL_ID))
    {
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_X))
            return WorkItemCondition::GLOBAL_ID_X;
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_Y))
            return WorkItemCondition::GLOBAL_ID_Y;
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_Z))
            return WorkItemCondition::GLOBAL_ID_Z;

        // global ID is addition of group ID * local size + local ID, so reuse the check for local ID to determine group
        // ID dimensions
        auto op = dynamic_cast<const intermediate::Operation*>(writer);
        if(op && op->op == OP_ADD)
        {
            auto localIdCondition = findWorkItemCondition(op->getFirstArg());
            if(!localIdCondition)
                localIdCondition = findWorkItemCondition(op->assertArgument(1));
            if(localIdCondition)
            {
                switch(*localIdCondition)
                {
                case WorkItemCondition::LOCAL_ID_X:
                case WorkItemCondition::GROUP_ID_X:
                    return WorkItemCondition::GLOBAL_ID_X;
                case WorkItemCondition::LOCAL_ID_Y:
                case WorkItemCondition::GROUP_ID_Y:
                    return WorkItemCondition::GLOBAL_ID_Y;
                case WorkItemCondition::LOCAL_ID_Z:
                case WorkItemCondition::GROUP_ID_Z:
                    return WorkItemCondition::GLOBAL_ID_Z;
                default:
                    return {};
                }
            }
        }
    }
    else if(writer->hasDecoration(intermediate::InstructionDecorations::BUILTIN_GROUP_ID))
    {
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_X))
            return WorkItemCondition::GROUP_ID_X;
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_Y))
            return WorkItemCondition::GROUP_ID_Y;
        if(writer->hasDecoration(intermediate::InstructionDecorations::DIMENSION_Z))
            return WorkItemCondition::GROUP_ID_Z;
    }

    auto source = intermediate::getSourceInstruction(writer);
    if(source && source != writer)
        return findWorkItemCondition(source->getOutput().value());

    return {};
}

static Optional<ComparisonInfo> findDynamicBranchConditon(
    const intermediate::Branch* branch, const Local* targetLabel, InstructionWalker it)
{
    auto loc = branch->getTarget().checkLocal();
    if(!loc)
        return {};
    const intermediate::IntermediateInstruction* matchingWrite = nullptr;
    loc->forUsers(LocalUse::Type::WRITER, [&](const intermediate::IntermediateInstruction* writer) {
        auto addressOf = dynamic_cast<const intermediate::CodeAddress*>(writer);
        if(addressOf && addressOf->getLabel() == targetLabel)
            matchingWrite = writer;
    });

    return matchingWrite ? getComparison(matchingWrite, it, true) : Optional<ComparisonInfo>{};
}

static SortedSet<std::size_t> toElementRange(std::size_t begin, std::size_t end)
{
    SortedSet<std::size_t> result;
    while(begin != end)
    {
        result.emplace(begin);
        ++begin;
    }
    return result;
}

static Optional<ActiveWorkItems> getWorkItemMask(
    const CFGEdge& edge, const CFGNode& entry, const CFGNode& exit, Optional<ActiveWorkItems>&& defaultValue)
{
    auto predecessorBranch = edge.data.getPredecessor(entry.key).get<intermediate::Branch>();
    if(!predecessorBranch)
        // (unconditional) fall-through, copy mask (if exists) of source branch
        return std::move(defaultValue);

    bool invertCondition = edge.data.isImplicit(entry.key) && predecessorBranch->getSingleTargetLabel();
    Optional<ComparisonInfo> comparison;
    if(predecessorBranch->branchCondition == BRANCH_ALWAYS && !predecessorBranch->isDynamicBranch())
        // unconditional branch, copy mask (if exists) of source branch
        return std::move(defaultValue);
    else if(predecessorBranch->isDynamicBranch())
        // try to find the condition for taking the dynamic branch to the current label
        comparison = findDynamicBranchConditon(predecessorBranch, exit.key->getLabel()->getLabel(), entry.key->walk());
    else if(!invertCondition && predecessorBranch->getSingleTargetLabel() != exit.key->getLabel()->getLabel())
        // unsupported branch type, copy mask (if exists) of source branch
        return std::move(defaultValue);
    else
        comparison = getComparison(predecessorBranch, entry.key->walk(), true);
    if(!comparison)
        // unsupported kind of branch condition, cannot deduce comparison, copy mask (if exists) of source branch
        return std::move(defaultValue);

    if(predecessorBranch->getSingleTargetLabel() == exit.key->getLabel()->getLabel() &&
        comparison->condition.isInversionOf(predecessorBranch->branchCondition.toConditionCode()))
        // this is an if-else-branch and the comparison found was for the other target block
        invertCondition = true;

    WorkItemCondition condition;
    Value otherValue = UNDEFINED_VALUE;
    if(auto leftCondition = findWorkItemCondition(comparison->leftOperand))
    {
        condition = *leftCondition;
        otherValue = comparison->rightOperand;
    }
    else if(auto rightCondition = findWorkItemCondition(comparison->rightOperand))
    {
        condition = *rightCondition;
        otherValue = comparison->leftOperand;
    }
    else
        // condition is not depending on work-item information, copy mask (if exists) of source branch
        return std::move(defaultValue);

    auto literalLimit = otherValue.getConstantValue() & &Value::getLiteralValue;
    if(!literalLimit)
        // bounds are not constant, so we don't know how much the condition is limited, since a branch cannot extend the
        // parent bounds, we can safely return them (if set)
        return std::move(defaultValue);

    if((comparison->name == intermediate::COMP_EQ && !invertCondition) ||
        (comparison->name == intermediate::COMP_NEQ && invertCondition))
        // only the single item matches
        return ActiveWorkItems{condition, {literalLimit->unsignedInt()}, {}};
    if((comparison->name == intermediate::COMP_NEQ && !invertCondition) ||
        (comparison->name == intermediate::COMP_EQ && invertCondition))
        // all but the single item match
        return ActiveWorkItems{condition, {}, {literalLimit->unsignedInt()}};

    if(comparison->name == intermediate::COMP_UNSIGNED_LT || comparison->name == intermediate::COMP_SIGNED_LT)
    {
        // A < X <=> A in [0, X - 1]
        auto range = toElementRange(0, literalLimit->unsignedInt());
        return invertCondition ? ActiveWorkItems{condition, {}, std::move(range)} :
                                 ActiveWorkItems{condition, std::move(range), {}};
    }
    if(comparison->name == intermediate::COMP_UNSIGNED_LE || comparison->name == intermediate::COMP_SIGNED_LE)
    {
        // A <= X <=> A in [0, X]
        auto range = toElementRange(0, literalLimit->unsignedInt() + 1u);
        return invertCondition ? ActiveWorkItems{condition, {}, std::move(range)} :
                                 ActiveWorkItems{condition, std::move(range), {}};
    }
    if(comparison->name == intermediate::COMP_UNSIGNED_GT || comparison->name == intermediate::COMP_SIGNED_GT)
    {
        // A > X <=> A not in [0, X]
        auto range = toElementRange(0, literalLimit->unsignedInt() + 1u);
        return invertCondition ? ActiveWorkItems{condition, std::move(range), {}} :
                                 ActiveWorkItems{condition, {}, std::move(range)};
    }
    if(comparison->name == intermediate::COMP_UNSIGNED_GE || comparison->name == intermediate::COMP_SIGNED_GE)
    {
        // A >= X <=> A not in [0, X - 1]
        auto range = toElementRange(0, literalLimit->unsignedInt());
        return invertCondition ? ActiveWorkItems{condition, std::move(range), {}} :
                                 ActiveWorkItems{condition, {}, std::move(range)};
    }

    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Unhandled case of work-item condition for '" << ::toString(condition) << "' and "
            << literalLimit.to_string() << ": " << comparison->to_string() << logging::endl);
    return {};
}

FastMap<const BasicBlock*, ActiveWorkItems> analysis::determineActiveWorkItems(
    const Method& method, ControlFlowGraph& cfg)
{
    PROFILE_SCOPE(determineActiveWorkItems);
    auto globalMask = getGlobalMask(method);

    FastMap<const BasicBlock*, ActiveWorkItems> results;
    // XXX This cannot be a set (even though this would remove duplicates), since if we skip an entry, the same entry
    // might get added to the front back again (e.g. due to sorting/hashing), which would cause an infinite loop
    std::queue<const CFGNode*> openNodes;
    FastSet<const CFGNode*> closedNodes;

    auto processNode = [&](const CFGNode& node) {
        if(closedNodes.find(&node) != closedNodes.end())
            return true;

        bool allPredecessorsHandled = true;
        bool hasIncomingEdges = false;
        node.forAllIncomingEdges([&](const CFGNode& neighbor, const CFGEdge& edge) {
            if(edge.data.isBackEdge(neighbor.key))
                // Ignore back edges, since they cannot modify the active work-items:
                // - A back edge cannot limit the active work-items, since the target block is already accessed
                // initially by more work-items
                // - A back-edge cannot expand the active work-items, since the loop is only visited by the limited
                // number of work-items
                return true;

            hasIncomingEdges = true;
            if(closedNodes.find(&neighbor) == closedNodes.end())
            {
                allPredecessorsHandled = false;
                return false;
            }
            return true;
        });

        if(!allPredecessorsHandled)
        {
            // re-run for this node again after we processed all predecessors
            openNodes.emplace(&node);
            return true;
        }

        Optional<ActiveWorkItems> nodeResult{};
        node.forAllIncomingEdges([&](const CFGNode& neighbor, const CFGEdge& edge) {
            if(edge.data.isBackEdge(neighbor.key))
                return true;

            auto prevResult = globalMask;
            auto resultIt = results.find(neighbor.key);
            if(resultIt != results.end())
                prevResult = resultIt->second;

            if(auto edgeResult = getWorkItemMask(edge, neighbor, node, std::move(prevResult)))
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Found work-item condition for edge '" << neighbor.key->to_string() << " -> "
                        << node.key->to_string() << "': " << edgeResult.to_string() << logging::endl);
                if(!nodeResult)
                    nodeResult = edgeResult;
                else
                    nodeResult = nodeResult->mergeWith(edgeResult.value()) | globalMask;
                // if we manage to create an empty merged result (e.g. non-mergeable conditions), treat as if there were
                // none
                return nodeResult.has_value();
            }
            else
                // could not deduce any work-item limit for this edge -> the node has at least a single entry without a
                // limit
                return false;
        });

        if(!hasIncomingEdges)
            // This is the case for the start block
            nodeResult = globalMask;

        if(nodeResult)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Basic " << node.key->to_string()
                    << " is accessed with work-item condition: " << nodeResult->to_string() << logging::endl);
            results.emplace(node.key, *std::move(nodeResult));
        }

        closedNodes.emplace(&node);

        node.forAllOutgoingEdges([&](const CFGNode& neighbor, const CFGEdge& edge) {
            openNodes.emplace(&neighbor);
            return true;
        });
        return true;
    };

    cfg.forAllSources(processNode);
    while(!openNodes.empty())
    {
        auto* node = openNodes.front();
        openNodes.pop();
        processNode(*node);
    }

#ifndef NDEBUG
    cfg.dumpGraph("/tmp/vc4c-cfg-active-workitems.dot", [&](const BasicBlock* block) {
        auto label = block->getLabel()->getLabel()->name;
        auto blockIt = results.find(block);
        if(blockIt != results.end())
            label += " (" + blockIt->second.to_string() + ")";
        return label;
    });
#endif

    return results;
}
