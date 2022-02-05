/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "../asm/BranchInstruction.h"
#include "IntermediateInstruction.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::intermediate;

BranchLabel::BranchLabel(const Local& label) : IntermediateInstruction(label.createReference())
{
    setArgument(0, label.createReference());
}

LCOV_EXCL_START
std::string BranchLabel::to_string() const
{
    return std::string("label: ") + getLabel()->name + createAdditionalInfoString();
}
LCOV_EXCL_STOP

std::unique_ptr<IntermediateInstruction> BranchLabel::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    return std::make_unique<BranchLabel>(*renameValue(method, assertArgument(0), localPrefix, localMapping).local());
}

LCOV_EXCL_START
qpu_asm::DecoratedInstruction BranchLabel::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    throw CompilationError(
        CompilationStep::CODE_GENERATION, "There should be no more labels at this point", to_string());
}
LCOV_EXCL_STOP

bool BranchLabel::mapsToASMInstruction() const
{
    return false;
}

bool BranchLabel::isNormalized() const
{
    return true;
}

const Local* BranchLabel::getLabel() const
{
    return assertArgument(0).local();
}

Local* BranchLabel::getLabel()
{
    return assertArgument(0).local();
}

bool BranchLabel::innerEquals(const IntermediateInstruction& other) const
{
    // no extra fields to check
    return dynamic_cast<const BranchLabel*>(&other);
}

Branch::Branch(const Local* target) : Branch(target, BRANCH_ALWAYS) {}

Branch::Branch(const Local* target, BranchCond branchCond) :
    SignalingInstruction(SIGNAL_BRANCH), branchCondition(branchCond)
{
    setArgument(0, target->createReference());
    if(target->type != TYPE_LABEL && target->type != TYPE_CODE_ADDRESS)
        throw CompilationError(CompilationStep::GENERAL, "Can only jump to label or code address", target->to_string());
}

Branch::Branch(const Value& linkAddressOut, const Local* target, BranchCond branchCond) :
    SignalingInstruction(SIGNAL_BRANCH, linkAddressOut), branchCondition(branchCond)
{
    setArgument(0, target->createReference());
    if(linkAddressOut.type != TYPE_CODE_ADDRESS)
        throw CompilationError(CompilationStep::GENERAL, "Can only store code addresses in value of proper type",
            linkAddressOut.to_string(true));
    if(target->type != TYPE_LABEL && target->type != TYPE_CODE_ADDRESS)
        throw CompilationError(CompilationStep::GENERAL, "Can only jump to label or code address", target->to_string());
}

LCOV_EXCL_START
std::string Branch::to_string() const
{
    auto linkString = getOutput() ? (getOutput()->to_string(true) + " = ") : "";
    auto targetName = getTarget().local()->name;
    auto dynamicString = isDynamicBranch() ? " (dynamic)" : "";
    auto condString = branchCondition == BRANCH_ALWAYS ? "" : ("." + branchCondition.to_string());
    return linkString + "br" + condString + " " + targetName + dynamicString + createAdditionalInfoString();
}
LCOV_EXCL_STOP

std::unique_ptr<IntermediateInstruction> Branch::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    auto newOut = getOutput() ? renameValue(method, *getOutput(), localPrefix, localMapping) : NO_VALUE;
    auto copy = createWithExtras<Branch>(
        *this, renameValue(method, getTarget(), localPrefix, localMapping).local(), branchCondition);
    copy->setOutput(newOut);
    return copy;
}

qpu_asm::DecoratedInstruction Branch::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    /*
     * We use relative targets, since the absolute address is completely absolute and not relative to the start of the
     * code-block. Since we do not know the start of the code-block (where the QPU code is loaded to), we use relative
     * addresses.
     *
     * Broadcom specification, page 34:
     * "branch target is relative to PC+4 (add PC+4 to target)"
     */
    Register outReg = REG_NOP;
    if(auto out = getOutput())
        outReg = out->checkLocal() ? registerMapping.at(out->local()) : out->reg();
    auto addOut = outReg.file == RegisterFile::PHYSICAL_A ? outReg : REG_NOP;
    auto mulOut = outReg.file == RegisterFile::PHYSICAL_B ? outReg : REG_NOP;
    int64_t branchOffset = 0;
    std::string targetName;
    BranchReg branchRegister = BranchReg::NONE;
    Address addressRegister = 0 /* only 5 bits, so REG_NOP doesn't fit */;
    if(auto label = getSingleTargetLabel())
    {
        // static branch to single label, relative address offset is the label address - the current address
        auto labelPos = labelMapping.find(label);
        if(labelPos == labelMapping.end())
            throw CompilationError(
                CompilationStep::CODE_GENERATION, "Target label not mapped to any position", to_string());

        branchOffset = static_cast<int64_t>(labelPos->second);
        targetName = label->name;
    }
    else
    {
        // dynamic branch, relative address offset is address value - the current address
        branchOffset = 0;
        targetName = vc4c::to_string<const Local*>(getTargetLabels(), [](const Local* label) { return label->name; });
        auto addressReg = getTarget().checkLocal() ? registerMapping.at(getTarget().local()) : getTarget().reg();
        if(addressReg.file != RegisterFile::PHYSICAL_A || !addressReg.isGeneralPurpose())
            throw CompilationError(CompilationStep::CODE_GENERATION,
                "Can only read dynamic branch address from physical register-file A", to_string());
        branchRegister = BranchReg::BRANCH_REG;
        addressRegister = addressReg.num;
    }

    branchOffset = branchOffset -
        static_cast<int64_t>(
            (instructionIndex + 4 /*  NOPs */) * sizeof(uint64_t)) /* convert instruction-index to byte-index */;
    if(branchOffset < static_cast<int64_t>(std::numeric_limits<int32_t>::min()) ||
        branchOffset > static_cast<int64_t>(std::numeric_limits<int32_t>::max()))
        throw CompilationError(CompilationStep::CODE_GENERATION,
            "Cannot jump a distance not fitting into 32-bit integer", std::to_string(branchOffset));
    return qpu_asm::DecoratedInstruction(
        qpu_asm::BranchInstruction(branchCondition, BranchRel::BRANCH_RELATIVE, branchRegister, addressRegister,
            addOut.num, mulOut.num, static_cast<int32_t>(branchOffset)),
        "to " + targetName);
}

bool Branch::isNormalized() const
{
    return true;
}

SideEffectType Branch::getSideEffects() const
{
    return add_flag(IntermediateInstruction::getSideEffects(), SideEffectType::BRANCH);
}

const Value& Branch::getTarget() const
{
    return assertArgument(0);
}

void Branch::setTarget(const Local* target)
{
    setArgument(0, target->createReference());
    if(target->type != TYPE_LABEL && target->type != TYPE_CODE_ADDRESS)
        throw CompilationError(CompilationStep::GENERAL, "Can only jump to label or code address", target->to_string());
}

const Local* Branch::getSingleTargetLabel() const
{
    auto loc = getTarget().local();
    if(loc && loc->type == TYPE_LABEL)
        // direct jump to label
        return loc;
    return nullptr;
}

FastSet<const Local*> Branch::getTargetLabels() const
{
    auto loc = getTarget().local();
    if(loc->type == TYPE_LABEL)
        // single target
        return {loc};

    // otherwise we have multiple targets
    FastSet<const Local*> targets;
    loc->forUsers(LocalUse::Type::WRITER, [&](const IntermediateInstruction* writer) {
        if(auto addressOf = dynamic_cast<const CodeAddress*>(writer))
        {
            if(auto label = addressOf->getLabel())
                targets.emplace(label);
            else
                throw CompilationError(CompilationStep::GENERAL,
                    "Can't determine branch target for code address not pointing to a label", writer->to_string());
        }
        else
            throw CompilationError(CompilationStep::GENERAL,
                "Can't determine branch target for code address writer not an addressof instruction",
                writer->to_string());
    });
    return targets;
}

bool Branch::isDynamicBranch() const
{
    return getTarget().type != TYPE_LABEL;
}

bool Branch::isUnconditional() const
{
    return branchCondition == BRANCH_ALWAYS;
}

bool Branch::innerEquals(const IntermediateInstruction& other) const
{
    auto otherBranch = dynamic_cast<const Branch*>(&other);
    return otherBranch && branchCondition == otherBranch->branchCondition;
}

PhiNode::PhiNode(Value&& dest, std::vector<std::pair<Value, const Local*>>&& labelPairs) :
    IntermediateInstruction(std::move(dest))
{
    for(std::size_t i = 0; i < labelPairs.size(); ++i)
    {
        setArgument(i * 2, labelPairs[i].second->createReference());
        setArgument(i * 2 + 1, std::move(labelPairs[i].first));
    }
}

LCOV_EXCL_START
std::string PhiNode::to_string() const
{
    std::string args;
    for(const auto& pair : getValuesForLabels())
        args.append(", ").append(pair.first->name).append(" -> ").append(pair.second.to_string());
    return (getOutput().to_string() + " = phi") + args.substr(1) + createAdditionalInfoString();
}

qpu_asm::DecoratedInstruction PhiNode::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    throw CompilationError(
        CompilationStep::CODE_GENERATION, "There should be no more phi-nodes at this point", to_string());
}

bool PhiNode::isNormalized() const
{
    return false;
}
LCOV_EXCL_STOP

std::unique_ptr<IntermediateInstruction> PhiNode::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    auto tmp = createWithExtras<PhiNode>(*this, renameValue(method, getOutput().value(), localPrefix, localMapping),
        std::vector<std::pair<Value, const Local*>>{});
    for(std::size_t i = 0; i < getArguments().size(); ++i)
    {
        tmp->setArgument(i, renameValue(method, assertArgument(i), localPrefix, localMapping));
    }
    return tmp;
}

FastMap<const Local*, Value> PhiNode::getValuesForLabels() const
{
    FastMap<const Local*, Value> res;
    for(std::size_t i = 0; i < getArguments().size(); i += 2)
    {
        res.emplace(assertArgument(i).local(), assertArgument(i + 1));
    }
    return res;
}

bool PhiNode::innerEquals(const IntermediateInstruction& other) const
{
    // no extra fields to check
    return dynamic_cast<const PhiNode*>(&other);
}

CodeAddress::CodeAddress(const Value& dest, const Local* label, ConditionCode cond, SetFlag setFlags) :
    ExtendedInstruction(SIGNAL_LOAD_IMMEDIATE, cond, setFlags, PACK_NOP, dest)
{
    if(dest.type != TYPE_CODE_ADDRESS)
        throw CompilationError(
            CompilationStep::GENERAL, "Can only store code addresses in value of proper type", dest.to_string(true));

    if(label)
    {
        if(label->type != TYPE_LABEL)
            throw CompilationError(
                CompilationStep::GENERAL, "Can only take code addresses of labels", label->to_string());
        setArgument(0, label->createReference());
    }
    // addresses are always unsigned
    addDecorations(InstructionDecorations::UNSIGNED_RESULT);
}

LCOV_EXCL_START
std::string CodeAddress::to_string() const
{
    std::string address = "(self)";
    if(auto label = getLabel())
        address = label->to_string();
    return (getOutput()->to_string(true) + " = addressof ") + std::move(address) + createAdditionalInfoString();
}
LCOV_EXCL_STOP

std::unique_ptr<IntermediateInstruction> CodeAddress::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    auto newOut = getOutput() ? renameValue(method, *getOutput(), localPrefix, localMapping) : NO_VALUE;
    const Local* address = nullptr;
    if(auto arg = getArgument(0))
        address = renameValue(method, *arg, localPrefix, localMapping).checkLocal();
    return createWithExtras<CodeAddress>(*this, newOut.value(), address);
}

qpu_asm::DecoratedInstruction CodeAddress::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const
{
    // if no label is set, take address of this instruction itself
    auto address = instructionIndex * sizeof(uint64_t) /* convert instruction-index to byte-index */;
    std::string addressName = "(self)";
    if(auto label = getLabel())
    {
        auto labelPos = labelMapping.find(label);
        if(labelPos == labelMapping.end())
            throw CompilationError(
                CompilationStep::CODE_GENERATION, "Target label not mapped to any position", to_string());
        address = labelPos->second;
        addressName = label->name;
    }
    if(address > static_cast<std::size_t>(std::numeric_limits<int32_t>::max()))
        throw CompilationError(CompilationStep::CODE_GENERATION,
            "Cannot load an address not fitting into 32-bit integer", std::to_string(address));
    LoadImmediate dummy{getOutput().value(), Literal(static_cast<int32_t>(address))};
    dummy.copyExtrasFrom(*this);
    return qpu_asm::DecoratedInstruction(
        dummy.convertToAsm(registerMapping, labelMapping, instructionIndex).instruction, std::move(addressName));
}

bool CodeAddress::isNormalized() const
{
    return true;
}

const Local* CodeAddress::getLabel() const
{
    return getArgument(0) & &Value::checkLocal;
}

const IntermediateInstruction* CodeAddress::getAddressedInstruction() const
{
    if(auto label = getLabel())
        return label->getSingleWriter();
    return this;
}

bool CodeAddress::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherAddress = dynamic_cast<const CodeAddress*>(&other))
        return otherAddress->getLabel() == getLabel();
    return false;
}
