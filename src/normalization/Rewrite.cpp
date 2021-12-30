/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Rewrite.h"

#include "../InstructionWalker.h"
#include "../intermediate/Helper.h"
#include "../intermediate/operators.h"
#include "LiteralValues.h"
#include "LongOperations.h"
#include "log.h"

#include <algorithm>

using namespace vc4c;
using namespace vc4c::normalization;
using namespace vc4c::operators;

static RegisterFile getFixedRegisterFile(const Value& val)
{
    if(auto reg = val.checkRegister())
    {
        if(reg->file == RegisterFile::PHYSICAL_A || reg->file == RegisterFile::PHYSICAL_B)
            return reg->file;
    }
    else if(val.isLiteralValue())
        return RegisterFile::PHYSICAL_B;
    else if(auto local = val.checkLocal())
    {
        for(const auto& user : local->getUsers())
        {
            if(user.second.readsLocal() && user.first->hasUnpackMode())
                return RegisterFile::PHYSICAL_A;
            if(user.second.writesLocal() && user.first->hasPackMode())
                return RegisterFile::PHYSICAL_A;
        }
    }

    return RegisterFile::ANY;
}

static void resolveRegisterConflicts(Method& method, InstructionWalker it, FastSet<Value>& fixedArgs)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Found instruction with conflicting fixed registers: " << it->to_string() << logging::endl);

    if(it->hasUnpackMode())
        // TODO if this instruction unpacks, then we need to insert the copy somewhere else
        // XXX can an instruction which unpacks take multiple parameters?
        throw CompilationError(CompilationStep::NORMALIZER, "Unhandled case of register conflicts", it->to_string());

    // otherwise, we can copy one of the arguments into a temporary
    auto& fixedArg =
        *std::find_if(fixedArgs.begin(), fixedArgs.end(), [](const Value& val) -> bool { return val.checkLocal(); });
    auto tmp = method.addNewLocal(fixedArg.type);
    auto cond = (check(it.get<intermediate::ExtendedInstruction>()) & &intermediate::ExtendedInstruction::getCondition)
                    .value_or(COND_ALWAYS);
    assign(it, tmp) = (fixedArg, cond);
    it->replaceValue(fixedArg, tmp, LocalUse::Type::READER);
}

void normalization::splitRegisterConflicts(
    Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    auto fixedFiles = RegisterFile::NONE;
    FastSet<Value> fixedArgs;
    bool hasRegisterConflict = false;
    bool anyLocalArgument = false;
    for(const Value& arg : it->getArguments())
    {
        auto file = getFixedRegisterFile(arg);
        if(file != RegisterFile::ANY)
        {
            if(has_flag(fixedFiles, file))
                hasRegisterConflict = true;
            fixedArgs.emplace(arg);
            fixedFiles = add_flag(fixedFiles, file);
        }
        if(arg.checkLocal())
            anyLocalArgument = true;
    }
    if(hasRegisterConflict && fixedArgs.size() > 1 && anyLocalArgument)
        resolveRegisterConflicts(method, it, fixedArgs);
}

void normalization::extendBranches(const Module& module, Method& method, const Configuration& config)
{
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        if(auto branch = it.get<intermediate::Branch>())
        {
            CPPLOG_LAZY(logging::Level::DEBUG, log << "Extending branch: " << branch->to_string() << logging::endl);
            // go to next instruction
            it.nextInBlock();
            // insert 3 NOPs before
            it.emplace(std::make_unique<intermediate::Nop>(intermediate::DelayType::BRANCH_DELAY));
            it.emplace(std::make_unique<intermediate::Nop>(intermediate::DelayType::BRANCH_DELAY));
            it.emplace(std::make_unique<intermediate::Nop>(intermediate::DelayType::BRANCH_DELAY));
        }
        it.nextInMethod();
    }
}

static Optional<std::pair<Literal, Literal>> is64BitLiteralLoad(const Local* input)
{
    if(auto loc = Local::getLocalData<MultiRegisterData>(input))
    {
        auto lowSource = intermediate::getSourceValue(loc->lower->createReference());
        auto upSource = intermediate::getSourceValue(loc->upper->createReference());
        auto lowLit = lowSource.getConstantValue() & &Value::getLiteralValue;
        auto upLit = upSource.getConstantValue() & &Value::getLiteralValue;
        if(lowLit && upLit)
            return std::make_pair(*lowLit, *upLit);
    }
    return {};
}

struct PhiMove
{
    std::shared_ptr<intermediate::PhiNode> node;
    Value input;
};

// The algorithm is adapted from: https://cp-algorithms.com/graph/topological-sort.html
static void sortTopological(
    PhiMove& current, FastSet<const Local*>& visited, FastAccessList<PhiMove>& input, FastAccessList<PhiMove>& output)
{
    visited.emplace(current.node->checkOutputLocal());
    auto local = current.input.checkLocal();
    if(local && visited.find(local) == visited.end())
    {
        auto it = std::find_if(input.begin(), input.end(),
            [local](const PhiMove& node) { return node.node && node.node->checkOutputLocal() == local; });
        if(it != input.end())
            sortTopological(*it, visited, input, output);
    }
    output.push_back(std::move(current));
}

static void mapPhiMoves(BasicBlock& sourceBlock, const Local* targetLabel, FastAccessList<PhiMove>& nodes)
{
    // make sure, moves are inserted before the outgoing branches
    InstructionWalker blockIt = sourceBlock.walkEnd();
    ConditionCode jumpCondition = COND_ALWAYS;
    Value condition(UNDEFINED_VALUE);
    while(
        blockIt.copy().previousInBlock().get<intermediate::Branch>() || blockIt.copy().previousInBlock()->doesSetFlag())
    {
        blockIt.previousInBlock();
        auto branch = blockIt.get<intermediate::Branch>();
        if(branch && branch->getSingleTargetLabel() == targetLabel)
        {
            jumpCondition = branch->branchCondition.toConditionCode();
            if(branch->branchCondition != BRANCH_ALWAYS)
            {
                if(auto branchCondition = sourceBlock.findLastSettingOfFlags(blockIt))
                    condition =
                        intermediate::getBranchCondition(branchCondition->get<intermediate::ExtendedInstruction>())
                            .first.value();
            }
        }
        else if(branch && branch->isDynamicBranch())
        {
            const intermediate::CodeAddress* associatedWriter = nullptr;
            branch->getTarget().local()->forUsers(LocalUse::Type::WRITER, [&](const LocalUser* writer) {
                auto codeAddress = dynamic_cast<const intermediate::CodeAddress*>(writer);
                if(codeAddress && codeAddress->getLabel() == targetLabel)
                    associatedWriter = codeAddress;
            });
            if(!associatedWriter)
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Failed to find code-address writer for dynamic branch to '" + targetLabel->to_string() + "'",
                    branch->to_string());
            auto writerIt = blockIt.getBasicBlock()->findWalkerForInstruction(
                associatedWriter, blockIt.getBasicBlock()->walk(), blockIt);
            if(!writerIt)
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Failed to find instruction walker for code-address write", associatedWriter->to_string());
            // we need to insert the phi-node for the same set-flags instruction and conditional flags as the
            // address-write of the target label
            blockIt = *writerIt;
            condition = UNDEFINED_VALUE;
            jumpCondition = associatedWriter->getCondition();
            break;
        }
    }
    // Since originally the value of the PHI node is set after the jump (at the start of the destination basic
    // block) and we have conditional branches "jump to A or B", we need to only set the value if we take the
    // (conditional) branch jumping to this basic block.

    if(jumpCondition != COND_ALWAYS && !condition.isUndefined())
        // Since the correct flags for the branch might not be set, we need to set them here.
        // Also, don't "or" with element number, since we might need to set the flags for more than the first
        // SIMD-element, this way, we set it for all
        assignNop(blockIt) = (condition, SetFlag::SET_FLAGS);

    if(nodes.size() > 1)
    {
        /*
         * Since all phi-nodes happen "at the same time" on the branch, we need to make sure one inserted move does not
         * overwrite an input value for another inserted move.
         * To do so, we need to sort the phi-nodes according to their input/output values.
         *
         * To achieve this, we do a topological sorting via a depth-first-search.
         */
        FastSet<const Local*> visitedEntries;
        FastAccessList<PhiMove> sortedNodes;
        sortedNodes.reserve(nodes.size());

        for(auto& entry : nodes)
        {
            // we move the entries, so anything already visited is moved-from
            if(entry.node)
                sortTopological(entry, visitedEntries, nodes, sortedNodes);
        }
        // we sort every dependencies before the dependent nodes, so invert the result
        std::reverse(sortedNodes.begin(), sortedNodes.end());
        nodes = std::move(sortedNodes);

        // sanity check to make sure the sorting is correct
        FastSet<const Local*> writtenLocals;
        for(auto& entry : nodes)
        {
            if(writtenLocals.find(entry.input.checkLocal()) != writtenLocals.end())
            {
                logging::error() << "Phi-moves to be inserted: " << to_string<PhiMove>(nodes, [](const PhiMove& node) {
                    return node.node->getOutput().to_string() + " <- " + node.input.to_string();
                }) << logging::endl;
                throw CompilationError(CompilationStep::NORMALIZER,
                    "Phi-node source value will be overridden by previous phi-node mode for", sourceBlock.to_string());
            }
            if(auto local = entry.node->checkOutputLocal())
                writtenLocals.emplace(local);
        }
    }

    for(auto& entry : nodes)
    {
        if(auto literalParts = is64BitLiteralLoad(entry.input.checkLocal()))
        {
            // for phi-nodes moving 64-bit literal values, the source constant is not directly used as the argument for
            // the phi-node (like it is for max 32-bit type-sizes), but instead loaded in separate instructions into the
            // lower and upper parts. To not read values which are not written yet, we need to move the original loaded
            // values.
            // We do not care for non-constant values, since in this case the writing is anyway split up in the block we
            // insert the phi-node into (since phi-nodes are always at the very beginning of a block, no calculation can
            // be done before them) by the successive normalization step splitting up all other 64-bit operations.

            Value lowDest = UNDEFINED_VALUE;
            Value upDest = UNDEFINED_VALUE;
            std::tie(lowDest, upDest) = normalization::getLowerAndUpperWords(entry.node->getOutput().value());

            blockIt
                .emplace(intermediate::createWithExtras<intermediate::LoadImmediate>(
                    *entry.node, lowDest, literalParts->first, jumpCondition))
                .addDecorations(add_flag(entry.node->decoration, intermediate::InstructionDecorations::PHI_NODE));
            blockIt.nextInBlock();
            blockIt
                .emplace(intermediate::createWithExtras<intermediate::LoadImmediate>(
                    *entry.node, upDest, literalParts->second, jumpCondition))
                .addDecorations(add_flag(entry.node->decoration, intermediate::InstructionDecorations::PHI_NODE));
        }
        else
            blockIt
                .emplace(intermediate::createWithExtras<intermediate::MoveOperation>(
                    *entry.node, entry.node->getOutput().value(), entry.input, jumpCondition))
                .addDecorations(add_flag(entry.node->decoration, intermediate::InstructionDecorations::PHI_NODE));

        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Inserting into '" << sourceBlock.to_string() << "': " << blockIt->to_string() << logging::endl);
        blockIt.nextInBlock();
    }
}

static void setPhiReferences(const intermediate::PhiNode& node, Local* origLocal)
{
    // set reference of local to original reference, if always the same for all possible sources
    if(auto output = node.checkOutputLocal())
    {
        const Local* ref = nullptr;
        for(const auto& pair : node.getValuesForLabels())
        {
            if(!pair.second.checkLocal())
                // cannot set universal reference
                return;
            if(pair.second.hasLocal(output) || pair.second.local()->getBase(true) == output ||
                pair.second.hasLocal(origLocal) || pair.second.local()->getBase(true) == origLocal)
                // phi node references its own result, ignore
                continue;
            if(ref != nullptr && pair.second.local()->getBase(true) != ref)
                // references differ
                return;
            ref = pair.second.local()->getBase(true);
        }

        if(ref && node.getOutput()->type.getPointerType())
            node.getOutput()->local()->set(ReferenceData(*ref, ANY_ELEMENT));
        if(ref && origLocal->type.getPointerType())
            origLocal->set(ReferenceData(*ref, ANY_ELEMENT));
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "PHI output: " << node.getOutput()->to_string(true, true) << " (for " << origLocal->to_string(true)
                << ')' << logging::endl);
    }
}

void normalization::eliminatePhiNodes(const Module& module, Method& method, const Configuration& config)
{
    // 1. search for all phi-nodes, remove from instructions and create mapping
    // mapping of source block -> target block -> phi node data
    FastMap<const Local*, FastMap<const Local*, FastAccessList<PhiMove>>> nodeMapping;
    FastMap<std::shared_ptr<intermediate::PhiNode>, Local*> phiNodes;
    for(auto& block : method)
    {
        auto it = block.walk();
        while(!it.isEndOfBlock())
        {
            if(auto phiNode = it.get<intermediate::PhiNode>())
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Eliminating phi-node by inserting moves: " << phiNode->to_string() << logging::endl);

                std::shared_ptr<intermediate::IntermediateInstruction> tmp{it.release()};
                auto sharedNode = std::dynamic_pointer_cast<intermediate::PhiNode>(tmp);
                auto targetLabel = block.getLabel()->getLabel();

                for(const auto& entry : phiNode->getValuesForLabels())
                    nodeMapping[entry.first][targetLabel].push_back(PhiMove{sharedNode, entry.second});

                /*
                 * Create temporary value to be inserted into the source blocks and replace original phi-node (in
                 * destination block) with a move from the temporary value to the original value.
                 *
                 * This is required, since otherwise for phi-nodes on branches which depend on the value written by a
                 * phi-node, inserting a write to the original phi output before the branch would modify the branch
                 * condition and thus the control flow. By writing all phi-node values to temporaries first and only in
                 * the destination block overwriting the original values, we avoid this problem and come a lot closer to
                 * the original behavior of a phi-node being executed "on the edge" between blocks.
                 *
                 * Most of the additional copies are eliminated anyway by later optimization steps.
                 */
                auto orig = sharedNode->getOutput().value();
                phiNodes.emplace(sharedNode, orig.local());
                auto phiTmp = method.addNewLocal(orig.type, orig.local()->name, "phi");
                sharedNode->setOutput(phiTmp);
                it.reset(std::make_unique<intermediate::MoveOperation>(orig, phiTmp));
            }
            else
                it.nextInBlock();
        }
    }

    // 2. insert new instructions at the end of the source blocks
    for(auto& entry : nodeMapping)
    {
        BasicBlock* bb = method.findBasicBlock(entry.first);
        if(bb == nullptr)
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to map all phi-options to valid basic-blocks",
                entry.first->to_string());
        for(auto& move : entry.second)
            mapPhiMoves(*bb, move.first, move.second);
    }

    // 3. update references where necessary
    for(const auto& entry : phiNodes)
    {
        setPhiReferences(*entry.first, entry.second);
    }
}

void normalization::moveRotationSourcesToAccumulators(
    Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    // makes sure, all sources for vector-rotations have a usage-range small enough to be on an accumulator
    /*
     * "The full horizontal vector rotate is only available when both of the mul ALU input arguments are taken from
     * accumulators r0-r3."
     * - Broadcom specification, page 20
     *
     */
    auto vectorRotation = it.has() ? it->getVectorRotation() : Optional<intermediate::RotationInfo>{};
    if(vectorRotation && vectorRotation->isFullRotationAllowed())
    {
        for(auto& arg : it->getArguments())
        {
            // NOTE: can either run on if-full-rotation allowed, this is greedy, will rewrite some cases where not
            // necessary or on if-quad-rotation-not-allowed, this is generous, will only rewrite when necessary, but
            // might cause register allocation errors
            if(auto loc = arg.checkLocal())
            {
                InstructionWalker writer = it.copy().previousInBlock();
                while(!writer.isStartOfBlock())
                {
                    if(writer.has() && writer->writesLocal(loc))
                        break;
                    writer.previousInBlock();
                }
                // if the local is either written in another block or the usage-range exceeds the accumulator threshold,
                // move to temporary
                if(writer.isStartOfBlock() ||
                    !writer.getBasicBlock()->isLocallyLimited(
                        writer, loc, config.additionalOptions.accumulatorThreshold))
                {
                    InstructionWalker mapper = it.copy().previousInBlock();
                    // insert mapper before first NOP
                    while(!mapper.isStartOfBlock() && mapper.copy().previousInBlock().get<intermediate::Nop>())
                        mapper.previousInBlock();
                    if(mapper.isStartOfBlock())
                        mapper.nextInBlock();
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Moving source of vector-rotation to temporary for: " << it->to_string()
                            << logging::endl);
                    const Value tmp = method.addNewLocal(loc->type, "%vector_rotation");
                    mapper.emplace(std::make_unique<intermediate::MoveOperation>(tmp, loc->createReference()));
                    if(mapper.nextInBlock() == it)
                        /*
                         * I.e. if the vector rotation is the first (non-label) instruction in a block, there are no
                         * other instructions that we can insert the move before, so we need to manually insert an
                         * instruction to not violate the rule:
                         * "An instruction that does a vector rotate must not immediately follow an instruction that
                         * writes to the accumulator that is being rotated."
                         * - Broadcom specification, page 37
                         */
                        mapper.emplace(std::make_unique<intermediate::Nop>(intermediate::DelayType::WAIT_REGISTER));
                    it->replaceLocal(loc, tmp.local(), LocalUse::Type::READER);
                }
            }
            else if(arg.checkRegister() && (!arg.reg().isAccumulator() || arg.reg().getAccumulatorNumber() > 3) &&
                arg != ROTATION_REGISTER)
            {
                // e.g. inserting into vector from reading VPM
                // insert temporary local to be read into, rotate local and NOP, since it is required
                auto tmp = method.addNewLocal(arg.type, "%vector_rotation");
                assign(it, tmp) = arg;
                nop(it, intermediate::DelayType::WAIT_REGISTER);
                it->replaceValue(arg, tmp, LocalUse::Type::READER);
            }
        }
    }
}
