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

static NODISCARD InstructionWalker resolveRegisterConflicts(
    Method& method, InstructionWalker it, FastSet<Value>& fixedArgs)
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
    it.emplace(new intermediate::MoveOperation(tmp, fixedArg, cond));
    it.nextInBlock();
    it->replaceValue(fixedArg, tmp, LocalUse::Type::READER);
    return it;
}

InstructionWalker normalization::splitRegisterConflicts(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
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
        return resolveRegisterConflicts(method, it, fixedArgs);

    return it;
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
            it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
            it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
            it.emplace(new intermediate::Nop(intermediate::DelayType::BRANCH_DELAY));
        }
        it.nextInMethod();
    }
}

/*
 * Check whether there is a phi-instruction that writes the output which is read by the phi-instruction to be inserted.
 *
 * In such case, we need to insert the new phi-instruction before the old one, since we need to act "as-if" the
 * phi-writes would have happen at the beginning of the jumped-to block (or on the edge itself), in which case the input
 * variable still has its old value.
 */
static bool doesWritePhiInput(InstructionWalker it, const Value& phiInput)
{
    return it->hasDecoration(intermediate::InstructionDecorations::PHI_NODE) && it->getOutput() == phiInput;
}

static void mapPhi(const intermediate::PhiNode& node, Method& method, InstructionWalker it)
{
    while(!it.isStartOfBlock())
    {
        it.previousInBlock();
    }
    const Local* label = it.get<intermediate::BranchLabel>()->getLabel();
    for(const auto& pair : node.getValuesForLabels())
    {
        BasicBlock* bb = method.findBasicBlock(pair.first);
        if(bb == nullptr)
        {
            logging::error() << "Cannot map phi-node to label: " << pair.first->name << logging::endl;
            throw CompilationError(CompilationStep::OPTIMIZER, "Failed to map all phi-options to valid basic-blocks",
                pair.first->to_string());
        }
        // make sure, moves are inserted before the outgoing branches
        InstructionWalker blockIt = bb->walkEnd();
        ConditionCode jumpCondition = COND_ALWAYS;
        Value condition(UNDEFINED_VALUE);
        while(blockIt.copy().previousInBlock().get<intermediate::Branch>() ||
            doesWritePhiInput(blockIt.copy().previousInBlock(), pair.second) ||
            blockIt.copy().previousInBlock()->doesSetFlag())
        {
            blockIt.previousInBlock();
            auto branch = blockIt.get<intermediate::Branch>();
            if(branch && branch->getSingleTargetLabel() == label)
            {
                jumpCondition = branch->branchCondition.toConditionCode();
                if(branch->branchCondition != BRANCH_ALWAYS)
                {
                    if(auto branchCondition = bb->findLastSettingOfFlags(blockIt))
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
                    if(codeAddress && codeAddress->getLabel() == label)
                        associatedWriter = codeAddress;
                });
                if(!associatedWriter)
                    throw CompilationError(CompilationStep::NORMALIZER,
                        "Failed to find code-address writer for dynamic branch to target '" + label->to_string() + ":",
                        branch->to_string());
                auto writerIt = blockIt.getBasicBlock()->findWalkerForInstruction(associatedWriter, blockIt);
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
        // block)  and we have conditional branches "jump to A or B", we need to only set the value if we take the
        // (conditional) branch jumping to this basic block.

        if(jumpCondition != COND_ALWAYS && !condition.isUndefined())
        {
            // Since the correct flags for the branch might not be set, we need to set them here.
            // Also, don't "or" with element number, since we might need to set the flags for more than the first
            // SIMD-element, this way, we set it for all
            blockIt.emplace(new intermediate::MoveOperation(NOP_REGISTER, condition, COND_ALWAYS, SetFlag::SET_FLAGS));
            blockIt.nextInBlock();
        }
        if(auto loc = Local::getLocalData<MultiRegisterData>(pair.second.checkLocal()))
        {
            // for phi-nodes moving 64-bit values, the source constant is not directly used as the argument for the
            // phi-node (like it is for max 32-bit type-sizes), but instead loaded in separate instructions into the
            // lower and upper parts. To not read values which are not written yet, we need to move the original loaded
            // values.
            // We do not care for non-constant values, since in this case the writing is anyway split up in the block we
            // insert the phi-node into (since phi-nodes are always at the very beginning of a block, no calculation can
            // be done before them).
            auto lowSource = intermediate::getSourceValue(loc->lower->createReference());
            auto upSource = intermediate::getSourceValue(loc->upper->createReference());
            lowSource = lowSource.getConstantValue().value_or(lowSource);
            upSource = upSource.getConstantValue().value_or(upSource);

            Value lowDest = UNDEFINED_VALUE;
            Value upDest = UNDEFINED_VALUE;
            std::tie(lowDest, upDest) = normalization::getLowerAndUpperWords(node.getOutput().value());

            blockIt.emplace(
                (new intermediate::MoveOperation(lowDest, lowSource, jumpCondition))
                    ->copyExtrasFrom(&node)
                    ->addDecorations(add_flag(node.decoration, intermediate::InstructionDecorations::PHI_NODE)));
            blockIt.nextInBlock();
            blockIt.emplace(
                (new intermediate::MoveOperation(upDest, upSource, jumpCondition))
                    ->copyExtrasFrom(&node)
                    ->addDecorations(add_flag(node.decoration, intermediate::InstructionDecorations::PHI_NODE)));
        }
        else
            blockIt.emplace(
                (new intermediate::MoveOperation(node.getOutput().value(), pair.second, jumpCondition))
                    ->copyExtrasFrom(&node)
                    ->addDecorations(add_flag(node.decoration, intermediate::InstructionDecorations::PHI_NODE)));
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Inserting into end of basic-block '" << pair.first->name << "': " << blockIt->to_string()
                << logging::endl);
    }

    // set reference of local to original reference, if always the same for all possible sources
    if(auto output = node.checkOutputLocal())
    {
        const Local* ref = nullptr;
        for(const auto& pair : node.getValuesForLabels())
        {
            if(!pair.second.checkLocal())
                // cannot set universal reference
                return;
            if(pair.second.hasLocal(output) || pair.second.local()->getBase(true) == output)
                // phi node references its own result, ignore
                continue;
            if(ref != nullptr && pair.second.local()->getBase(true) != ref)
                // references differ
                return;
            ref = pair.second.local()->getBase(true);
        }

        if(ref && node.getOutput()->type.getPointerType())
            node.getOutput()->local()->set(ReferenceData(*ref, ANY_ELEMENT));
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "PHI output: " << node.getOutput()->to_string(true, true) << logging::endl);
    }
}

void normalization::eliminatePhiNodes(const Module& module, Method& method, const Configuration& config)
{
    // Search for all phi-nodes and insert all mapped instructions to the end of the corresponding basic block
    auto it = method.walkAllInstructions();
    while(!it.isEndOfMethod())
    {
        if(auto phiNode = it.get<intermediate::PhiNode>())
        {
            // 2) map the phi-node to the move-operations per predecessor-label
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Eliminating phi-node by inserting moves: " << it->to_string() << logging::endl);
            mapPhi(*phiNode, method, it);
            it.erase();
        }
        else
            it.nextInMethod();
    }
}
