/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "ControlFlow.h"

#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "../analysis/ControlFlowGraph.h"
#include "../analysis/DominatorTree.h"
#include "../intermediate/Helper.h"
#include "../intermediate/TypeConversions.h"
#include "../intermediate/operators.h"
#include "../intrinsics/WorkItems.h"
#include "./Combiner.h"
#include "Optimizer.h"
#include "log.h"

#include <algorithm>
#include <map>
#include <queue>
#include <set>
#include <tuple>
#include <vector>

using namespace vc4c;
using namespace vc4c::analysis;
using namespace vc4c::optimizations;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

static NODISCARD InstructionWalker loadScalarParameter(
    Parameter& param, DataType type, Method& method, InstructionWalker it, bool isElement = false)
{
    auto decorations =
        add_flag(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE, InstructionDecorations::IDENTICAL_ELEMENTS);
    if(type.isSimpleType() && type.getScalarBitCount() > 32 && param.get<MultiRegisterData>())
    {
        // 64-bit integer direct value load, need to insert two loads
        auto parts = Local::getLocalData<MultiRegisterData>(&param);

        // data is stored in little endian, so lower word first
        assign(it, parts->lower->createReference()) =
            (Value(REG_UNIFORM, TYPE_INT32), isElement ? COND_ZERO_SET : COND_ALWAYS, decorations,
                isElement ? InstructionDecorations::ELEMENT_INSERTION : InstructionDecorations::NONE);
        assign(it, parts->upper->createReference()) =
            (Value(REG_UNIFORM, TYPE_INT32), isElement ? COND_ZERO_SET : COND_ALWAYS, decorations,
                isElement ? InstructionDecorations::ELEMENT_INSERTION : InstructionDecorations::NONE);
    }
    else if(has_flag(param.decorations, ParameterDecorations::SIGN_EXTEND))
    {
        it = insertSignExtension(it, method, Value(REG_UNIFORM, type), Value(&param, TYPE_INT32), false,
            isElement ? COND_ZERO_SET : COND_ALWAYS);
    }
    else if(has_flag(param.decorations, ParameterDecorations::ZERO_EXTEND))
    {
        it = insertZeroExtension(it, method, Value(REG_UNIFORM, type), Value(&param, TYPE_INT32), false,
            isElement ? COND_ZERO_SET : COND_ALWAYS);
    }
    else
    {
        assign(it, param.createReference()) =
            (Value(REG_UNIFORM, type), isElement ? COND_ZERO_SET : COND_ALWAYS, decorations,
                // all pointers are unsigned
                param.type.getPointerType() ? InstructionDecorations::UNSIGNED_RESULT : InstructionDecorations::NONE);
    }
    if(isElement)
        it.copy().previousInBlock()->addDecorations(InstructionDecorations::ELEMENT_INSERTION);

    return it;
}

static NODISCARD InstructionWalker loadVectorParameter(Parameter& param, Method& method, InstructionWalker it)
{
    // we need to load a UNIFORM per vector element into the particular vector element
    for(uint8_t i = 0; i < param.type.getVectorWidth(); ++i)
    {
        // the first write to the parameter needs to unconditional, so the register allocator can find it
        if(i > 0)
        {
            assign(it, NOP_REGISTER) =
                (ELEMENT_NUMBER_REGISTER ^ Value(SmallImmediate(i), TYPE_INT8), SetFlag::SET_FLAGS);
        }
        it = loadScalarParameter(param, param.type.getElementType(), method, it, i != 0);
        // TODO improve performance by first putting together the vector, then zero/sign extending all elements?
    }
    return it;
}

static void generateStopSegment(Method& method)
{
    // write interrupt for host
    // write QPU number finished (value must be NON-NULL, so we invert it -> the first 28 bits are always 1)
    method.appendToEnd(
        (new intermediate::Operation(OP_NOT, Value(REG_HOST_INTERRUPT, TYPE_INT8), Value(REG_QPU_NUMBER, TYPE_INT8)))
            ->addDecorations(InstructionDecorations::IDENTICAL_ELEMENTS));
    auto nop = new intermediate::Nop(intermediate::DelayType::THREAD_END);
    // set signals to stop thread/program
    nop->setSignaling(SIGNAL_END_PROGRAM);
    method.appendToEnd(nop);
    method.appendToEnd(new intermediate::Nop(intermediate::DelayType::THREAD_END));
    method.appendToEnd(new intermediate::Nop(intermediate::DelayType::THREAD_END));
}

static const Local* isLocalUsed(Method& method, BuiltinLocal::Type type, bool forceUse = false)
{
    if(forceUse)
        return method.findOrCreateBuiltin(type);
    auto loc = method.findBuiltin(type);
    if(loc != nullptr && loc->hasUsers(LocalUse::Type::READER))
        return loc;
    return nullptr;
}

/*
 * For inserting the work-group loop, we need the presence of the the local IDs and local sizes UNIFORM, independent of
 * whether they are ever used in the "actual" kernel code.
 */
static bool isLocalInformationRequired(const Configuration& config)
{
    return Optimizer::isEnabled(optimizations::PASS_WORK_GROUP_LOOP, config) ||
        Optimizer::isEnabled(optimizations::PASS_CACHE_MEMORY, config);
}

void optimizations::addStartStopSegment(const Module& module, Method& method, const Configuration& config)
{
    auto it = method.walkAllInstructions();
    if(it.isEndOfMethod() || !it.get<intermediate::BranchLabel>() ||
        BasicBlock::DEFAULT_BLOCK != it.get<intermediate::BranchLabel>()->getLabel()->name)
    {
        it = method.createAndInsertNewBlock(method.begin(), BasicBlock::DEFAULT_BLOCK).walk();
    }
    it.nextInBlock();

    // if the second TMU was used explicitly at some point, we disable TMU_SWAP
    {
        bool tmu1Used = false;
        auto checkIt = method.walkAllInstructions();
        while(!checkIt.isEndOfMethod())
        {
            if(checkIt->writesRegister(REG_TMU1_ADDRESS))
            {
                tmu1Used = true;
                break;
            }
            checkIt.nextInMethod();
        }
        if(tmu1Used)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Using both TMUs explicitly, disable automatic swapping!" << logging::endl);
            assign(it, Value(REG_TMU_NOSWAP, TYPE_BOOL)) = BOOL_TRUE;
        }
    }

    /*
     * The first UNIFORMs are reserved for relaying information about the work-item and work-group
     * - work_dim: number of dimensions
     * - local_sizes: local number of work-items in its work-group per dimension
     * - local_ids: local id of this work-item within its work-group
     * - num_groups (x,y,z): global number of work-groups per dimension
     * - group_id (x, y, z): id of this work-group
     * - global_offset (x, y, z): global initial offset per dimension
     * - address of global data / to load the global data from
     *
     */
    // initially set all implicit UNIFORMs to unused
    method.metaData.uniformsUsed.value = 0;
    auto workInfoDecorations = add_flag(InstructionDecorations::UNSIGNED_RESULT,
        InstructionDecorations::WORK_GROUP_UNIFORM_VALUE, InstructionDecorations::IDENTICAL_ELEMENTS);
    bool isLocalInfoRequired = isLocalInformationRequired(config);
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::WORK_DIMENSIONS))
    {
        method.metaData.uniformsUsed.setWorkDimensionsUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT8), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::LOCAL_SIZES, isLocalInfoRequired))
    {
        method.metaData.uniformsUsed.setLocalSizesUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::LOCAL_IDS, isLocalInfoRequired))
    {
        method.metaData.uniformsUsed.setLocalIDsUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32),
            remove_flag(workInfoDecorations, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE));
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::NUM_GROUPS_X))
    {
        method.metaData.uniformsUsed.setNumGroupsXUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::NUM_GROUPS_Y))
    {
        method.metaData.uniformsUsed.setNumGroupsYUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::NUM_GROUPS_Z))
    {
        method.metaData.uniformsUsed.setNumGroupsZUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GROUP_ID_X))
    {
        method.metaData.uniformsUsed.setGroupIDXUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GROUP_ID_Y))
    {
        method.metaData.uniformsUsed.setGroupIDYUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GROUP_ID_Z))
    {
        method.metaData.uniformsUsed.setGroupIDZUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GROUP_IDS))
    {
        method.metaData.uniformsUsed.setGroupIDXUsed(true);
        method.metaData.uniformsUsed.setGroupIDYUsed(true);
        method.metaData.uniformsUsed.setGroupIDZUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GLOBAL_OFFSET_X))
    {
        method.metaData.uniformsUsed.setGlobalOffsetXUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GLOBAL_OFFSET_Y))
    {
        method.metaData.uniformsUsed.setGlobalOffsetYUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GLOBAL_OFFSET_Z))
    {
        method.metaData.uniformsUsed.setGlobalOffsetZUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }
    if(auto loc = isLocalUsed(method, BuiltinLocal::Type::GLOBAL_DATA_ADDRESS))
    {
        method.metaData.uniformsUsed.setGlobalDataAddressUsed(true);
        assign(it, loc->createReference()) = (Value(REG_UNIFORM, TYPE_INT32), workInfoDecorations);
    }

    // load arguments to locals (via reading from uniform)
    for(Parameter& param : method.parameters)
    {
        // do the loading
        // we need special treatment for non-scalar parameter (e.g. vectors), since they can't be read with just 1
        // UNIFORM
        if(!param.type.getPointerType() && param.type.getVectorWidth() != 1)
        {
            it = loadVectorParameter(param, method, it);
        }
        else
        {
            /*
             * NOTE: Pointers with the byval decoration are treated as simple pointers, saving us from having to
             * re-write all instructions accessing them. In return, the VC4CL run-time needs to convert the direct
             * kernel argument (e.g. a struct) to a pointer-to-data argument by allocating a buffer (similar to
             * local arguments).
             *
             * Alternative ways of solving this:
             * - Read parameter from UNIFORMs and write to VPM, where it can be accessed like "normal" pointed-to
             * data
             * - Read directly from UNIFORM storage, needs pointer to UNIFORM and re-set UNIFORM pointer for
             * successive parameter
             * - Load the single parts separately via UNIFORMs like any other vector/scalar, replace index-chain and
             * access functions.
             */
            it = loadScalarParameter(param, param.type, method, it);
        }
    }

    generateStopSegment(method);
}

bool optimizations::moveLoopInvariantCode(const Module& module, Method& method, const Configuration& config)
{
    const int moveDepth = config.additionalOptions.moveConstantsDepth;
    bool hasChanged = false;

    auto& cfg = method.getCFG();
    if(cfg.getNodes().empty())
        return false;

    // 2. Find loops
    auto dominatorTree = analysis::DominatorTree::createDominatorTree(cfg);
    auto loops = cfg.findLoops(true, true, dominatorTree.get());

    // 3. Generate inclusion relation of loops as trees
    auto inclusionTree = createLoopInclusionTree(loops);

    // 4. Move constant load operations from root of trees
    std::map<LoopInclusionTreeNode*, std::vector<InstructionWalker>> instMapper;

    // to facilitate moving loop invariant code, map all (for now only single) writer of locals to their basic block, so
    // we have it easier checking whether the writer is in the currently processed loop or not
    FastMap<const Local*, BasicBlock*> localSourceBlocks;
    for(auto it = method.walkAllInstructions(); !it.isEndOfMethod(); it.nextInMethod())
    {
        if(!it.has())
            continue;
        auto loc = it->checkOutputLocal();
        if(loc && loc->getSingleWriter() == it.get())
            localSourceBlocks.emplace(loc, it.getBasicBlock());
    }

    // find instructions to be moved
    for(auto& loop : inclusionTree->getNodes())
    {
        auto& node = inclusionTree->getOrCreateNode(loop.first);
        auto& loopHeaderDominatorNode = dominatorTree->assertNode(loop.first->getHeader());
        for(auto& cfgNode : *node.key)
        {
            if(node.hasCFGNodeInChildren(cfgNode))
            {
                // treat this node as that it's in child nodes.
                continue;
            }

            auto checkLoopInvariantSource = [&](const Value& arg) -> bool {
                if(arg.checkRegister())
                    // any constant register would already have been moved by the #isConstantInstruction() check
                    return false;
                if(arg.checkImmediate() || arg.checkLiteral() || arg.checkVector())
                    // compile-time constants are always loop invariant
                    return true;
                if(auto loc = arg.checkLocal())
                {
                    auto writer = loc->getSingleWriter();
                    if(!writer)
                        return false;
                    // local is loop invariant if:
                    // - (single) writer is outside of loop and dominates the whole loop or
                    auto sourceBlockIt = localSourceBlocks.find(loc);
                    if(sourceBlockIt != localSourceBlocks.end())
                    {
                        // check whether the writing block is not in the current loop and actually dominates the loop
                        // the dominator check is required to not hoist the instruction above its writer
                        auto& blockDominatorNode = dominatorTree->assertNode(&cfg.assertNode(sourceBlockIt->second));
                        if(loopHeaderDominatorNode.isDominatedBy(blockDominatorNode) &&
                            std::none_of(loop.first->begin(), loop.first->end(), [&](const CFGNode* loopNode) -> bool {
                                return loopNode->key == sourceBlockIt->second;
                            }))
                            return true;
                    }
                    // - (single) writer is in loop, but already marked for being hoisted (e.g. since loop invariant
                    // itself)
                    auto loopMapperIt = instMapper.find(&node);
                    if(loopMapperIt != instMapper.end())
                    {
                        if(std::any_of(loopMapperIt->second.begin(), loopMapperIt->second.end(),
                               [&](const InstructionWalker& it) -> bool { return it.get() == writer; }))
                            return true;
                    }
                }
                // should never happen, since we handled all value types
                return false;
            };

            // skip label
            auto it = cfgNode->key->walk().nextInBlock();
            for(; !it.isEndOfBlock(); it.nextInBlock())
            {
                if(!it.has())
                    continue;
                bool isSingleWriter = (check(it->checkOutputLocal()) & &Local::getSingleWriter) == it.get();
                bool isHoistableInstruction = it.get<intermediate::Operation>() ||
                    it.get<intermediate::MoveOperation>() || it.get<intermediate::LoadImmediate>();
                if(!isSingleWriter || !isHoistableInstruction)
                    continue;
                if(it->isConstantInstruction())
                {
                    // can only move constant writes of locals only written exactly here
                    instMapper[&node].emplace_back(it);
                    hasChanged = true;
                }
                const auto& args = it->getArguments();
                if(!it->hasSideEffects() && !it->hasConditionalExecution() &&
                    std::all_of(args.begin(), args.end(), checkLoopInvariantSource))
                {
                    // not a constant calculation, but does not depend on anything written in this loop, so we can move
                    // it too
                    instMapper[&node].emplace_back(it);
                    hasChanged = true;
                }
            }
        }
    }

    std::set<LoopInclusionTreeNode*> processedNodes;
    std::set<IntermediateInstruction*> processedInsts;
    BasicBlock* insertedBlock = nullptr;

    // move instructions
    for(auto& loop : inclusionTree->getNodes())
    {
        auto& node = inclusionTree->getOrCreateNode(loop.first);
        auto root = castToTreeNode(node.findRoot({}));

        if(processedNodes.find(root) != processedNodes.end())
            continue;
        processedNodes.insert(root);

        // process tree nodes with BFS
        std::queue<LoopInclusionTreeNode*> queue;
        queue.push(root);

        while(!queue.empty())
        {
            auto currentNode = queue.front();
            queue.pop();

            /*
             * TODO for now don't hoist any code out of to the root loop, but only out of the current loop.
             * This is required, since we now have non-constant instructions which might depend on code from the parent
             * loops.
             * TODO better yet, distinguish between actual constant instructions (hoist out of the outermost loop) and
             * non-constant, but loop invariant instructions (hoist only outside of the current loop).
             *
             * auto targetTreeNode = castToTreeNode(currentNode->findRoot(moveDepth == -1 ? Optional<int>() :
             * Optional<int>(moveDepth - 1)));
             * auto targetLoop = targetTreeNode->key;
             */
            auto targetLoop = currentNode->key;

            currentNode->forAllOutgoingEdges(
                [&](const LoopInclusionTreeNode& child, const LoopInclusionTreeEdge&) -> bool {
                    queue.push(const_cast<LoopInclusionTreeNode*>(&child));
                    return true;
                });

            auto insts = instMapper.find(currentNode);
            if(insts == instMapper.end())
            {
                continue;
            }

            const CFGNode* targetCFGNode = nullptr;
            // Find the predecessor block of targetLoop.
            if(auto node = dominatorTree->findNode(targetLoop->getHeader()))
            {
                node->forAllIncomingEdges([&](const DominatorTreeNode& node, const auto& edge) -> bool {
                    if(targetCFGNode)
                    {
                        // multiple direct dominators, can't actually happen
                        targetCFGNode = nullptr;
                        return false;
                    }
                    targetCFGNode = node.key;
                    return true;
                });
            }

            auto targetBlock = targetCFGNode != nullptr ? targetCFGNode->key : insertedBlock;

            if(targetBlock != nullptr)
            {
                // insert before first 'br' operation (if any)
                auto targetInst = targetBlock->walk().nextInBlock();
                while(!targetInst.isEndOfBlock() && !targetInst.get<Branch>())
                    targetInst.nextInBlock();
                for(auto it : insts->second)
                {
                    auto inst = it.get();
                    if(!it.has() || processedInsts.find(inst) != processedInsts.end())
                        continue;
                    processedInsts.insert(inst);

                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Moving invariant code out of loop: " << it->to_string() << logging::endl);
                    targetInst.emplace(it.release());
                    // go to the next instruction to preserve the order of insertion, e.g. to manage dependencies
                    targetInst.nextInBlock();
                    it.reset(nullptr);
                }
            }
            else
            {
                logging::debug() << "Creating a new block to hoist loop invariant code into" << logging::endl;

                auto headBlock = method.begin();

                insertedBlock = &method.createAndInsertNewBlock(method.begin(), "%createdByLoopInvariantCodeMotion");
                for(auto it : insts->second)
                {
                    auto inst = it.get();
                    if(!it.has() || processedInsts.find(inst) != processedInsts.end())
                        continue;
                    processedInsts.insert(inst);

                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Moving invariant code out of loop: " << it->to_string() << logging::endl);
                    // append always to the end to preserve the order, e.g. for dependencies
                    insertedBlock->walkEnd().emplace(it.release());
                    it.reset(nullptr);
                }
                // Clear processed instructions
                insts->second.clear();

                auto headLabel = headBlock->getLabel()->getLabel();
                auto insertedLabel = insertedBlock->getLabel()->getLabel();
                if(headLabel && insertedLabel && headLabel->name == BasicBlock::DEFAULT_BLOCK)
                {
                    // swap labels because DEFAULT_BLOCK is treated as head block.
                    headLabel->name.swap(insertedLabel->name);
                }
            }
        }
    }

#ifdef DEBUG_MODE
    auto& cfg2 = method.getCFG();
    cfg2.dumpGraph("/tmp/vc4c-loop-invariant-code-motion-after.dot", true);
#endif

    if(hasChanged)
    {
        method.cleanEmptyInstructions();
        // combine the newly reordered load instructions, since we might have grouped same loads together
        combineLoadingConstants(module, method, config);
    }

    return hasChanged;
}

static const Local* findSourceBlock(const Local* label, const FastMap<const Local*, const Local*>& blockMap)
{
    auto it = blockMap.find(label);
    if(it == blockMap.end())
        return label;
    return findSourceBlock(it->second, blockMap);
}

bool optimizations::mergeAdjacentBasicBlocks(const Module& module, Method& method, const Configuration& config)
{
    if(method.empty())
        return false;
    auto& graph = method.getCFG();

    std::vector<std::pair<const Local*, const Local*>> blocksToMerge;

    auto it = method.begin();
    ++it;
    while(it != method.end())
    {
        // XXX currently, this only merges adjacent (in list of blocks) blocks
        auto prevIt = it;
        --prevIt;

        const auto& prevNode = graph.assertNode(&(*prevIt));
        const auto& node = graph.assertNode(&(*it));
        if(node.getSinglePredecessor() == &prevNode && prevNode.getSingleSuccessor() == &node)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Found basic block with single direct successor: " << prevIt->to_string() << " and "
                    << it->to_string() << logging::endl);
            blocksToMerge.emplace_back(prevIt->getLabel()->getLabel(), it->getLabel()->getLabel());
        }
        ++it;
    }

    // this is required to be able to merge more than 2 blocks together
    FastMap<const Local*, const Local*> blockMap;

    for(auto& pair : blocksToMerge)
    {
        BasicBlock* sourceBlock = method.findBasicBlock(findSourceBlock(pair.second, blockMap));
        BasicBlock* destBlock = method.findBasicBlock(findSourceBlock(pair.first, blockMap));

        // remove all instructions from source block and append to destination block (skipping the source label)
        auto sourceIt = sourceBlock->walk().nextInBlock();
        while(!sourceIt.isEndOfBlock())
        {
            destBlock->walkEnd().emplace(sourceIt.release());
            sourceIt.nextInBlock();
        }
        // remove explicit branch from destination block to source block, if any
        auto& sourceNode = method.getCFG().assertNode(sourceBlock);
        auto& destNode = method.getCFG().assertNode(destBlock);
        if(auto edge = sourceNode.getEdge(&destNode))
        {
            auto predecessorIt = edge->data.getPredecessor(destBlock);
            if(auto branch = predecessorIt.get<intermediate::Branch>())
            {
                if(branch->getSingleTargetLabel() == sourceBlock->getLabel()->getLabel())
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Removing explicit branch to basic block about to be merged: "
                            << predecessorIt->to_string() << logging::endl);
                    predecessorIt.erase();
                }
            }
        }

        // then remove the source block
        if(method.removeBlock(*sourceBlock))
        {
            blockMap.emplace(pair.second, pair.first);
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Merged block " << pair.second->to_string() << " into " << pair.first->to_string()
                    << logging::endl);
        }
        else
        {
            LCOV_EXCL_START
            CPPLOG_LAZY_BLOCK(logging::Level::ERROR, {
                logging::error() << "Failed to merge block " << pair.second->to_string() << " into "
                                 << pair.first->to_string() << logging::endl;
                if(!sourceBlock->empty())
                {
                    logging::error() << "Block was not empty: " << logging::endl;
                    sourceBlock->dumpInstructions();
                }
                sourceBlock->forPredecessors([](InstructionWalker it) {
                    if(it.get())
                        logging::error() << "Block has explicit predecessor: " << it->to_string() << logging::endl;
                });
            });
            LCOV_EXCL_STOP
            throw CompilationError(
                CompilationStep::OPTIMIZER, "Failed to remove empty basic block: ", sourceBlock->to_string());
        }
    }

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Merged " << blocksToMerge.size() << " pair of blocks!" << logging::endl);
    return !blocksToMerge.empty();
}

bool optimizations::reorderBasicBlocks(const Module& module, Method& method, const Configuration& config)
{
    if(method.empty())
        return false;
    auto& cfg = method.getCFG();
    auto blockIt = method.begin();
    auto prevIt = method.begin();
    ++blockIt;
    while(blockIt != method.end())
    {
        auto& node = cfg.assertNode(&(*blockIt));
        const auto predecessor = node.getSinglePredecessor();
        // Never re-order end-of-block. Though it should work, there could be trouble anyway
        if(blockIt->getLabel()->getLabel()->name != BasicBlock::LAST_BLOCK && predecessor != nullptr &&
            predecessor->key != &(*prevIt) && !prevIt->fallsThroughToNextBlock())
        {
            auto predecessorIt = method.begin();
            while(predecessorIt != method.end())
            {
                if(&(*predecessorIt) == predecessor->key)
                    break;
                ++predecessorIt;
            }

            if(predecessorIt == method.end())
                throw CompilationError(CompilationStep::OPTIMIZER,
                    "Failed to find predecessor basic block: ", predecessor->key->to_string());

            // we insert before the iteration, so we need to set the iterator after the predecessor
            ++predecessorIt;

            // don't insert if the block after the predecessor is also a successor of the predecessor
            bool predecessorAlreadyFollowedBySuccessor = false;
            predecessor->forAllOutgoingEdges(
                [&, successorKey(&*predecessorIt)](const CFGNode& successor, const CFGEdge& edge) -> bool {
                    if(successor.key == successorKey)
                    {
                        predecessorAlreadyFollowedBySuccessor = true;
                        return false;
                    }
                    return true;
                });
            if(predecessorAlreadyFollowedBySuccessor)
            {
                ++blockIt;
                ++prevIt;
                continue;
            }

            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Reordering block with single predecessor not being the previous block: " << blockIt->to_string()
                    << logging::endl);

            method.moveBlock(blockIt, predecessorIt);
            // prevIt stays the same, since we removed the block and the next blockIt now follows prevIt
            blockIt = prevIt;
            ++blockIt;

            // if the now moved block did fall-through, we need to insert an explicit branch to its previous successor,
            // since they might now not longer be adjacent.
            const CFGNode* fallThroughSuccessor = nullptr;
            node.forAllOutgoingEdges([&](const CFGNode& successor, const CFGEdge& edge) -> bool {
                if(edge.data.isImplicit(node.key))
                {
                    if(fallThroughSuccessor)
                        throw CompilationError(CompilationStep::GENERAL, "Multiple implicit branches from basic block",
                            node.key->to_string());
                    fallThroughSuccessor = &successor;
                }
                return true;
            });
            if(fallThroughSuccessor)
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Inserting explicit branch to previous fall-through successor for moved block '"
                        << node.key->to_string() << "' to '" << fallThroughSuccessor->key->to_string() << '\''
                        << logging::endl);
                node.key->walkEnd().emplace(
                    new intermediate::Branch(fallThroughSuccessor->key->getLabel()->getLabel()));
            }
        }
        else
        {
            ++blockIt;
            ++prevIt;
        }
    }

#ifdef DEBUG_MODE
    cfg.dumpGraph("/tmp/vc4c-cfg-reordered.dot", false);
#endif
    return false;
}

struct IfElseBlock
{
    // The common predecessor block, the block whether the condition(s) are checked
    CFGNode* predecessor;
    // The blocks executed for the different cases (may be a single for if without else or several for switch-cases)
    FastSet<CFGNode*> conditionalBlocks;
    // The common successor block, i.e. the block after the if-else or switch-case block
    CFGNode* successor;
};

static FastAccessList<IfElseBlock> findIfElseBlocks(ControlFlowGraph& graph)
{
    FastAccessList<IfElseBlock> blocks;
    graph.forAllNodes([&](CFGNode& node) {
        IfElseBlock candidateBlock{&node, {}, nullptr};

        node.forAllOutgoingEdges([&](CFGNode& candidate, CFGEdge& edge) -> bool {
            /*
             * edge is a candidate, if it has a single successor (the same as all other candidates) and a single
             * predecessor (the base node being checked):
             *
             * If-Else:
             *     Node
             *     /  \
             * Then    Else  <- candidates
             *     \  /
             *   Successor
             *
             * Switch-Case-Default:
             *        Node
             *       /  |  \
             *      /   |   \
             * Case0  Case1  Default  <- candidates
             *      \   |   /
             *       \  |  /
             *      Successor
             */

            // TODO to guarantee that we not only save instructions, but also execution cycles, we should check the
            // maximum length of the resulting block not exceeding the instructions we save executing one of the
            // cases (e.g. 2 branches + some conditionals/phi).
            if(auto succ = candidate.getSingleSuccessor())
            {
                if((candidateBlock.successor == nullptr || succ == candidateBlock.successor) &&
                    candidate.getSinglePredecessor() == &node)
                {
                    candidateBlock.conditionalBlocks.emplace(&candidate);
                    candidateBlock.successor = succ;
                    return true;
                }
            }
            // first level successors have different/multiple second level successors (or multiple predecessors),
            // abort
            candidateBlock.successor = nullptr;
            return false;
        });

        if(candidateBlock.successor != nullptr && candidateBlock.conditionalBlocks.size() > 1)
            blocks.emplace_back(std::move(candidateBlock));
        else if(/* DISABLES CODE */ (false)) // TODO needs testing!
        {
            // TODO also needs extension in rewriting, special handling for successor which is conditional!
            // we failed with simple version above, recheck for more complex version:
            FastSet<CFGNode*> candidates;

            node.forAllOutgoingEdges([&](CFGNode& candidate, CFGEdge& edge) -> bool {
                candidates.emplace(&candidate);
                return true;
            });

            if(candidates.size() == 1 || candidates.find(&node) != candidates.end())
                // single successors will always match!  Also simple nodes might!
                return;

            FastSet<CFGNode*> conditionalBlocks;
            FastSet<CFGNode*> fallThroughBlocks;

            node.forAllOutgoingEdges([&](CFGNode& candidate, CFGEdge& edge) -> bool {
                /*
                 * edge is a candidate, if
                 * - it has a single successor (the same as almost all other candidates) and a single predecessor
                 * (the base node being checked) or
                 * - it is the single successor of all other candidates and has only candidates and the base node as
                 * successors
                 *
                 * If-without-Else:
                 * Node
                 *  | \
                 *  |  Then  <- candidate
                 *  | /
                 * Successor  <-candidate
                 *
                 * Switch-Case:
                 *       Node
                 *       / | \
                 *  Case0  |  Case1  <- candidates
                 *       \ | /
                 *     Successor  <- candidate
                 */
                if(auto succ = candidate.getSingleSuccessor())
                {
                    if(candidates.find(succ) != candidates.end() && candidate.getSinglePredecessor() == &node)
                    {
                        // this block is succeeded by another candidate, this is a "normal" conditional block
                        conditionalBlocks.emplace(&candidate);
                        return true;
                    }
                    bool allPredecessorsAreCandidates = true;
                    candidate.forAllIncomingEdges([&](CFGNode& pred, CFGEdge& e) -> bool {
                        if(candidates.find(&pred) == candidates.end() && &pred != &node)
                        {
                            allPredecessorsAreCandidates = false;
                            return false;
                        }
                        return true;
                    });
                    if(allPredecessorsAreCandidates && candidates.find(succ) == candidates.end())
                    {
                        // this block is succeeded by any other block and all predecessors are candidates for
                        // conditional, this is also a "fallthrough" block
                        conditionalBlocks.emplace(&candidate);
                        fallThroughBlocks.emplace(&candidate);
                        return true;
                    }
                }
                return false;
            });

            if(candidates.size() == conditionalBlocks.size() && fallThroughBlocks.size() == 1)
            {
                // all blocks could be assigned to either category and there is only 1 fall-through block
                blocks.emplace_back(IfElseBlock{&node, std::move(conditionalBlocks), *fallThroughBlocks.begin()});
                logging::error() << "ADVANCED VERSION" << logging::endl;
            }
        }
    });
    return blocks;
}

bool optimizations::simplifyConditionalBlocks(const Module& module, Method& method, const Configuration& config)
{
    if(method.empty())
        return false;
    // NOTE: boost-compute/test_binary_search.cl/calls to atomic_min are good test candidates!
    bool changedCode = false;
    for(const auto& block : findIfElseBlocks(method.getCFG()))
    {
        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            logging::debug() << "Found conditional block candidate: " << block.predecessor->key->to_string()
                             << logging::endl;
            for(auto succ : block.conditionalBlocks)
                logging::debug() << "\t" << succ->key->to_string() << logging::endl;
            logging::debug() << "Successor: " << block.successor->key->to_string() << logging::endl;
        });

        bool hasSideEffects = false;
        FastSet<const Local*> nonlocalLocals;
        for(auto succ : block.conditionalBlocks)
        {
            auto it = succ->key->walk().nextInBlock(); // skip label
            while(it != succ->key->walkEnd())
            {
                if(it.get<intermediate::Branch>() == nullptr && (it->hasSideEffects() || it->hasConditionalExecution()))
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Side effect in " << succ->key->to_string() << " - " << it->to_string()
                            << logging::endl);
                    hasSideEffects = true;
                    break;
                }
                if(it->checkOutputLocal() && !succ->key->isLocallyLimited(it, it->getOutput()->local(), 8))
                    nonlocalLocals.emplace(it->getOutput()->local());
                it.nextInBlock();
            }
            if(hasSideEffects)
                break;
        }

        if(hasSideEffects)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Skipping this candidate, since conditional block has side effects" << logging::endl);
            continue;
        }

        CPPLOG_LAZY_BLOCK(logging::Level::DEBUG, {
            for(auto loc : nonlocalLocals)
                logging::debug() << "Non-local: " << loc->to_string() << logging::endl;
        });

        // need to reorder successive blocks, so that default branch (without any condition) is inserted top-most
        // and not at last!
        InstructionWalker beforeBranchesIt = block.predecessor->key->walk().nextInBlock();
        while(!beforeBranchesIt.get<intermediate::Branch>())
            beforeBranchesIt.nextInBlock();
        // go to last before the first branch
        beforeBranchesIt.previousInBlock();

        for(auto succ : block.conditionalBlocks)
        {
            succ->forAllIncomingEdges([&](CFGNode& predecessor, CFGEdge& edge) -> bool {
                // the predecessor instruction is the branch to this block (if not fall-through)
                auto lastIt = edge.data.getPredecessor(predecessor.key);

                // copy the whole block content before the branch to the block, modify writing all external locals
                // to only be applied for the same condition the branch is applied and remove the branch (if not
                // fall-through).
                // at the moment of this optimization, the writing of the conditional the branch depends on is
                // already generated, so we can just re-use the conditional.
                Optional<Value> condVal{};
                ConditionCode cond = COND_ALWAYS;
                {
                    auto branch = lastIt.get<intermediate::Branch>();
                    Optional<InstructionWalker> branchCondition{};
                    if(branch && branch->getSingleTargetLabel() == succ->key->getLabel()->getLabel() &&
                        !branch->isUnconditional() &&
                        (branchCondition = predecessor.key->findLastSettingOfFlags(lastIt)))
                    {
                        condVal =
                            intermediate::getBranchCondition(branchCondition->get<intermediate::ExtendedInstruction>())
                                .first.value();
                        cond = branch->branchCondition.toConditionCode();
                    }
                    else
                    {
                        // the last branch maybe unconditional (e.g. the default for switch-cases), but we need to
                        // insert the unconditional local assignment as first instruction.
                        if(branch && branch->getSingleTargetLabel() == succ->key->getLabel()->getLabel())
                            // remove original unconditional branch. If this is a fall-through don't remove anything
                            lastIt.erase();
                        // make sure the instructions are inserted before all other
                        lastIt = beforeBranchesIt;
                    }
                }

                // 1.) insert flag depending on the conditional of the branch
                if(condVal && cond != COND_ALWAYS)
                    assign(lastIt, NOP_REGISTER) = (*condVal, SetFlag::SET_FLAGS);

                // 2.) insert all instructions
                for(auto& inst : *succ->key)
                {
                    if(dynamic_cast<const intermediate::BranchLabel*>(inst.get()))
                        // neither move nor delete the label
                        continue;

                    if(dynamic_cast<const intermediate::Branch*>(inst.get()))
                    {
                        // do not copy branches to successor label
                        inst.reset();
                        continue;
                    }

                    lastIt.emplace(inst.release());

                    // 3.) modify all instructions writing non-locals to only write under same condition as the
                    // branch
                    // XXX do we win anything in making all the instructions conditional? Technically this would be
                    // possible
                    for(auto loc : nonlocalLocals)
                    {
                        if(lastIt->writesLocal(loc) && lastIt.get<ExtendedInstruction>())
                        {
                            lastIt.get<ExtendedInstruction>()->setCondition(cond);
                            lastIt->decoration =
                                remove_flag(lastIt->decoration, InstructionDecorations::IDENTICAL_ELEMENTS);
                            break;
                        }
                    }

                    // 3.1) remove phi-node decoration, since they are no longer phi-nodes
                    // This would then just confuse the live range analysis
                    lastIt->decoration = remove_flag(lastIt->decoration, InstructionDecorations::PHI_NODE);

                    lastIt.nextInBlock();
                }

                // 4.) remove branch to original block
                if(condVal)
                    lastIt.erase();

                // 5.) remove original block
                if(!method.removeBlock(*succ->key))
                {
                    CPPLOG_LAZY_BLOCK(logging::Level::WARNING, {
                        logging::warn() << "Failed to remove move-from basic block: " << succ->key->to_string()
                                        << logging::endl;
                        succ->key->dumpInstructions();
                    });
                    // XXX throw exception here or continue??
                }

                // there is only one incoming edge
                return false;
            });
        }

        // insert branch to successor block to guarantee we switch into that, independent of the block order
        block.predecessor->key->walkEnd().emplace(
            new intermediate::Branch(block.successor->key->getLabel()->getLabel()));

        changedCode = true;
    }

    return changedCode;
}

// If we loop through all work-groups, the initial work-group id for all dimensions is always zero
// Also, to not override the work-group index at every iteration, extract the writing out of the loops
NODISCARD static bool moveGroupIdInitializers(Method& method, BasicBlock& defaultBlock, BasicBlock& newStartBlock)
{
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Moving group ID initializers out of work-group-loop..." << logging::endl);
    auto insertIt = newStartBlock.walkEnd();
    // whether the group id locals are not written (i.e. as converted by the intrinsic of vc4cl_get_group_id(n)) and
    // therefore not used inside the "real" kernel code
    bool groupIdsOnlyRead = true;
    for(auto type : {BuiltinLocal::Type::GROUP_ID_X, BuiltinLocal::Type::GROUP_ID_Y, BuiltinLocal::Type::GROUP_ID_Z})
    {
        auto loc = method.findOrCreateBuiltin(type);
        if(auto writer = loc->getSingleWriter())
        {
            groupIdsOnlyRead = false;
            if(auto it = defaultBlock.findWalkerForInstruction(writer, defaultBlock.walkEnd()))
                it->erase();
        }
        assign(insertIt, loc->createReference()) = (INT_ZERO, InstructionDecorations::IDENTICAL_ELEMENTS,
            InstructionDecorations::WORK_GROUP_UNIFORM_VALUE, InstructionDecorations::PHI_NODE);
    }

    if(groupIdsOnlyRead)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Merging group ID locals, since they are not used in the kernel code" << logging::endl);
        // If the group IDs are never actually used in the kernel code, we can compress them into a single register.
        // 1. Remove all previous assignments
        auto it = newStartBlock.walk().nextInBlock();
        while(!it.isEndOfBlock())
            it.erase();
        // 2. Add new assignment to grouped value
        assign(it, method.findOrCreateBuiltin(BuiltinLocal::Type::GROUP_IDS)->createReference()) =
            (INT_ZERO, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE, InstructionDecorations::PHI_NODE);
    }

    return groupIdsOnlyRead;
}

// After the main kernel code executed, insert a block which
// - reads uniform address
// - reads maximum values for all group id dimensions
// - resets uniform pointer to previously read address
NODISCARD static InstructionWalker insertAddressResetBlock(
    Method& method, InstructionWalker it, const Value& maxGroupIdX, const Value& maxGroupIdY, const Value& maxGroupIdZ)
{
    it = method.emplaceLabel(
        it, new intermediate::BranchLabel(*method.addNewLocal(TYPE_LABEL, "", "%work_group_repetition").local()));

    // insert after label, not before
    it.nextInBlock();

    // NOTE: This is NOT work-group uniform, since every QPU has its own UNIFORM values (e.g. for local ID)
    auto tmp = assign(it, TYPE_INT32) = (UNIFORM_REGISTER, InstructionDecorations::IDENTICAL_ELEMENTS);
    auto decorations =
        add_flag(InstructionDecorations::WORK_GROUP_UNIFORM_VALUE, InstructionDecorations::IDENTICAL_ELEMENTS);
    assign(it, maxGroupIdX) = (UNIFORM_REGISTER, decorations);
    assign(it, maxGroupIdY) = (UNIFORM_REGISTER, decorations);
    assign(it, maxGroupIdZ) = (UNIFORM_REGISTER, decorations);
    assign(it, Value(REG_UNIFORM_ADDRESS, TYPE_INT32)) = (tmp, InstructionDecorations::IDENTICAL_ELEMENTS);
    return it;
}

// For every dimension insert a block which
// - Resets the previous id to zero
// - Increments the current id by one
// - Checks whether the current id is smaller than the maximum value
// - If so, jumps back to the default block
// - Otherwise, falls through to the next block
NODISCARD static InstructionWalker insertSingleDimensionRepetitionBlock(Method& method, const BasicBlock& defaultBlock,
    Value id, const Value& maxValue, InstructionWalker it, const Local* previousId, int8_t mergedValueIndex)
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Inserting repetition block for: " << id.to_string() << logging::endl);
    it = method.emplaceLabel(it,
        new intermediate::BranchLabel(
            *method.addNewLocal(TYPE_LABEL, "", "%repeat_" + id.local()->name.substr(1)).local()));
    it->addDecorations(InstructionDecorations::WORK_GROUP_LOOP);
    // insert after label, not before
    it.nextInBlock();

    InstructionDecorations condDecorations = InstructionDecorations::NONE;
    if(mergedValueIndex < 0)
    {
        // First increment current id then reset previous dimension to be able to skip inserting a nop, since we read
        // the current id immediately afterwards.
        assign(it, id) = (id + INT_ONE, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE,
            InstructionDecorations::IDENTICAL_ELEMENTS, InstructionDecorations::PHI_NODE);
        if(previousId)
            assign(it, previousId->createReference()) = (INT_ZERO, InstructionDecorations::WORK_GROUP_UNIFORM_VALUE,
                InstructionDecorations::IDENTICAL_ELEMENTS, InstructionDecorations::PHI_NODE);
        // setting this decoration allows for most of the conditional code inserted below to be optimized away
        condDecorations = InstructionDecorations::IDENTICAL_ELEMENTS;
    }
    else
    {
        // Do the same as above, but with the current and the previous dimension merged into the same vector (with index
        // mergedValueIndex for this dimension and mergedValueIndex -1 for the previous dimension).
        auto mergedValue = method.findOrCreateBuiltin(BuiltinLocal::Type::GROUP_IDS)->createReference();
        assign(it, NOP_REGISTER) =
            (ELEMENT_NUMBER_REGISTER - Value(Literal(mergedValueIndex), TYPE_INT8), SetFlag::SET_FLAGS);
        assign(it, mergedValue) = (mergedValue + INT_ONE, COND_ZERO_SET);
        if(previousId)
            assign(it, mergedValue) = (INT_ZERO, COND_NEGATIVE_SET, InstructionDecorations::PHI_NODE);
        // we explicitly set the SIMD element to set the branch condition below, so use the whole group ids merged value
        // here.
        id = mergedValue;
    }

    // NOTE: using signed comparison limits the number of work-groups per dimension to INT_MAX - 1 to avoid overflow.
    // This is also reflected host-side on the kernel_config::MAX_WORK_ITEM_DIMENSIONS limit and checked in
    // Kernel::enqueueNDRange.
    auto cond = assignNop(it) = (as_signed{id} < as_signed{maxValue}, condDecorations);
    auto condValue = method.addNewLocal(TYPE_BOOL);
    assign(it, condValue) = (BOOL_TRUE, cond, condDecorations);
    assign(it, condValue) = (BOOL_TRUE ^ BOOL_TRUE, cond.invert(), condDecorations);
    BranchCond branchCond = BRANCH_ALWAYS;
    std::tie(it, branchCond) =
        intermediate::insertBranchCondition(method, it, condValue, 1u << std::max(int8_t{0}, mergedValueIndex));
    it.emplace((new intermediate::Branch(defaultBlock.getLabel()->getLabel(), branchCond))
                   ->addDecorations(InstructionDecorations::WORK_GROUP_LOOP));
    it.nextInMethod();
    return it;
}

static void insertRepetitionBlocks(
    Method& method, const BasicBlock& defaultBlock, BasicBlock& lastBlock, bool mergeGroupIds)
{
    auto maxGroupIdX = method.findOrCreateBuiltin(BuiltinLocal::Type::MAX_GROUP_ID_X)->createReference();
    auto maxGroupIdY = method.findOrCreateBuiltin(BuiltinLocal::Type::MAX_GROUP_ID_Y)->createReference();
    auto maxGroupIdZ = method.findOrCreateBuiltin(BuiltinLocal::Type::MAX_GROUP_ID_Z)->createReference();

    auto it = lastBlock.walk();
    it = insertAddressResetBlock(method, it, maxGroupIdX, maxGroupIdY, maxGroupIdZ);

    auto groupIdX = method.findOrCreateBuiltin(BuiltinLocal::Type::GROUP_ID_X)->createReference();
    it = insertSingleDimensionRepetitionBlock(
        method, defaultBlock, groupIdX, maxGroupIdX, it, nullptr, mergeGroupIds ? 0 : -1);

    auto groupIdY = method.findOrCreateBuiltin(BuiltinLocal::Type::GROUP_ID_Y)->createReference();
    it = insertSingleDimensionRepetitionBlock(
        method, defaultBlock, groupIdY, maxGroupIdY, it, groupIdX.local(), mergeGroupIds ? 1 : -1);

    auto groupIdZ = method.findOrCreateBuiltin(BuiltinLocal::Type::GROUP_ID_Z)->createReference();
    it = insertSingleDimensionRepetitionBlock(
        method, defaultBlock, groupIdZ, maxGroupIdZ, it, groupIdY.local(), mergeGroupIds ? 2 : -1);
}

/**
 * We need to synchronize the execution paths of all work-item executions (QPUs).
 *
 * This is required due to some work-item executions might still be stuck in the previous work-group iteration (e.g. if
 * stalled on memory access), while some work-item executions already moved to the next work-group iteration. Thus, the
 * work-item executions might access memory which is already overwritten in the next work-group iteration.
 *
 * This is especially true for code which differs in the execution path based on the local ID, e.g. for some special
 * reduction step only executed by get_local_id() == 0, which is not an uncommon pattern!
 *
 * To avoid this, we need to make sure all work-item executions are done with the previous work-group iteration before
 * any of them can start executing the next work-group iteration. The expected behavior is very similar to the
 * barrier(...) OpenCL C function.
 */
static bool insertSynchronizationBlock(Method& method, BasicBlock& lastBlock)
{
    // TODO move this synchronization block(s) after the work-group loop, to skip for last work-group?!

    // we insert the label/block in any case, since otherwise we would need to do similar in the caller if this
    // synchronization block is not inserted. In case the synchronization is not used, the empty block is just optimized
    // away anyway.
    auto it = method.emplaceLabel(lastBlock.walk(),
        new intermediate::BranchLabel(*method.addNewLocal(TYPE_LABEL, "", "%work_item_synchronization").local()));
    auto& syncBlock = *it.getBasicBlock();
    intermediate::redirectAllBranches(lastBlock, syncBlock);

    if(has_flag(method.flags, MethodFlags::TRAILING_CONTROL_FLOW_BARRIER) ||
        has_flag(method.flags, MethodFlags::LEADING_CONTROL_FLOW_BARRIER))
    {
        // If there is already a control flow barrier at the end of the kernel code (e.g. as inserted by writing back
        // memory cached in VPM), don't insert another barrier, since all work-items are already synchronized!
        // That same counts for any control flow barrier at the beginning of the kernel code (e.g. as inserted by
        // pre-loading memory into VPM cache).
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Skipping work-item synchronization block due to already trailing barrier in kernel code!"
                << logging::endl);
        return false;
    }

    if(method.metaData.getFixedWorkGroupSize() == 1u)
    {
        // Only a single work-item (per work-group), so no need to synchronize, since all work-items/work-groups are
        // guaranteed to be executed serially.
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Skipping work-item synchronization block due to single work-item per work-group!" << logging::endl);
        return false;
    }

    if(has_flag(method.flags, MethodFlags::NO_CROSS_ITEM_MEMORY_ACCESS))
    {
        // There is no memory written which is read by another work-item, so we cannot run into data races there!
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Skipping work-item synchronization block due to absence of cross-item data dependencies!"
                << logging::endl);
        return false;
    }

    // TODO Only insert when already barrier call (and therefore semaphore access to barrier semaphores) present?
    //   Reasoning: This is to avoid write (by other work-items) from next work-group overwriting reads in the previous
    //   work-group. For such code to be correct (even without work-group loop), barrier() needs to be used between
    //   writes and reads, to avoid data being read which is not yet written (by other work-items).

    CPPLOG_LAZY(logging::Level::DEBUG, log << "Inserting work-item synchronization block..." << logging::endl);

    it.nextInBlock();
    intrinsics::insertControlFlowBarrier(method, it);
    method.flags = add_flag(method.flags, MethodFlags::TRAILING_CONTROL_FLOW_BARRIER);
    return true;
}

bool optimizations::addWorkGroupLoop(const Module& module, Method& method, const Configuration& config)
{
    if(method.walkAllInstructions().isEndOfMethod())
        return false;
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Wrapping kernel " << method.name << " in a work-group loop..." << logging::endl);

    // The old head block, the head block of the actual kernel execution
    auto& defaultBlock = *method.begin();
    if(defaultBlock.empty())
        return false;

    // The new head block, the block initializing the group ids
    auto startIt = method.emplaceLabel(method.walkAllInstructions(),
        new intermediate::BranchLabel(*method.addNewLocal(TYPE_LABEL, "", "%group_id_initializer").local()));
    startIt->addDecorations(InstructionDecorations::WORK_GROUP_LOOP);
    auto& startBlock = *startIt.getBasicBlock();

    // The old and new tail block which indicates kernel execution finished
    auto lastBlock = method.findBasicBlock(BasicBlock::LAST_BLOCK);
    if(!lastBlock)
    {
        CPPLOG_LAZY(
            logging::Level::WARNING, log << "Failed to find the default last block, aborting!" << logging::endl);
        return false;
    }

    // Remove reads of UNIFORMs for group ids and move initializing to zero out of loop
    bool groupIdsNotUsed = moveGroupIdInitializers(method, defaultBlock, startBlock);

    // Insert a block to synchronize all work-item/QPUs to avoid data races between work-group iterations
    auto syncBlockInserted = insertSynchronizationBlock(method, *lastBlock);
    PROFILE_COUNTER(
        vc4c::profiler::COUNTER_OPTIMIZATION + 7000, "Work-group synchronization blocks", syncBlockInserted);

    // Insert all the code required to increment/reset the ids and repeat the kernel code
    insertRepetitionBlocks(method, defaultBlock, *lastBlock, groupIdsNotUsed);

    // set correct information to metadata
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Adjusting kernel metadata..." << logging::endl);
    method.flags = add_flag(method.flags, MethodFlags::WORK_GROUP_LOOP);
    method.metaData.uniformsUsed.setGroupIDXUsed(false);
    method.metaData.uniformsUsed.setGroupIDYUsed(false);
    method.metaData.uniformsUsed.setGroupIDZUsed(false);
    method.metaData.uniformsUsed.setUniformAddressUsed(true);
    method.metaData.uniformsUsed.setMaxGroupIDXUsed(true);
    method.metaData.uniformsUsed.setMaxGroupIDYUsed(true);
    method.metaData.uniformsUsed.setMaxGroupIDZUsed(true);

    return true;
}
