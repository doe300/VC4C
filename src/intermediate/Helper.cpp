/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Helper.h"

#include "../SIMDVector.h"
#include "CompilationError.h"
#include "TypeConversions.h"
#include "config.h"
#include "log.h"
#include "operators.h"

#include <algorithm>

using namespace vc4c;
using namespace vc4c::intermediate;
using namespace vc4c::operators;

InstructionWalker intermediate::insertMakePositive(
    InstructionWalker it, Method& method, const Value& src, Value& dest, Value& writeIsNegative)
{
    if(auto lit = src.getLiteralValue())
    {
        bool isNegative = lit->signedInt() < 0;
        dest = isNegative ? Value(Literal(-lit->signedInt()), src.type) : src;
        writeIsNegative = isNegative ? INT_MINUS_ONE : INT_ZERO;
    }
    else if(auto vector = src.checkVector())
    {
        SIMDVector tmpDest;
        SIMDVector tmpNegative;
        for(unsigned i = 0; i < vector->size(); ++i)
        {
            auto elem = (*vector)[i];
            bool isNegative = elem.signedInt() < 0;
            tmpDest[i] = isNegative ? Literal(-elem.signedInt()) : elem;
            tmpNegative[i] = isNegative ? Literal(-1) : Literal(0u);
        }
        dest = SIMDVectorHolder::storeVector(std::move(tmpDest), src.type, vector->getStorage());
        writeIsNegative = SIMDVectorHolder::storeVector(std::move(tmpNegative), src.type, vector->getStorage());
    }
    else if(src.getSingleWriter() != nullptr &&
        src.getSingleWriter()->hasDecoration(InstructionDecorations::UNSIGNED_RESULT))
    {
        // the value is already unsigned
        dest = src;
        writeIsNegative = INT_ZERO;
    }
    else
    {
        /*
         * Calculation of positive value:
         * %sign = asr %src, 31 -> -1 for negative, 0 for positive numbers
         * %tmp = xor %src, %sign
         * %unsigned = sub %tmp, %sign
         *
         * For positive:
         * %sign = 0
         * %tmp = %src
         * %unsigned = sub %src, 0 -> %src
         *
         * For negative:
         * %sign = -1
         * %tmp = ~%src
         * %unsigned = ~%src, -1 -> ~%src + 1 -> two's complement
         *
         * Source:
         * https://llvm.org/doxygen/IntegerDivision_8cpp_source.html
         */

        //%sign = asr %src, 31 -> -1 for negative, 0 for positive numbers
        Value srcInt = src;
        if(src.type.getScalarBitCount() < 32)
        {
            // to make sure, the leading bits are set
            srcInt = method.addNewLocal(TYPE_INT32.toVectorType(src.type.getVectorWidth()), "%sext");
            it = insertSignExtension(it, method, src, srcInt, true);
        }
        if(!writeIsNegative.checkLocal())
            writeIsNegative = method.addNewLocal(TYPE_INT32.toVectorType(src.type.getVectorWidth()), "%sign");
        assign(it, writeIsNegative) = as_signed{srcInt} >> 31_val;
        //%tmp = xor %src, %sign
        Value tmp = assign(it, src.type, "%twos_complement") = srcInt ^ writeIsNegative;
        //%unsigned = sub %tmp, %sign
        if(!dest.isWriteable())
            dest = method.addNewLocal(src.type, "%unsigned");
        assign(it, dest) = (tmp - writeIsNegative, InstructionDecorations::UNSIGNED_RESULT);
    }
    return it;
}

InstructionWalker intermediate::insertRestoreSign(
    InstructionWalker it, Method& method, const Value& src, Value& dest, const Value& sign)
{
    if(src.getLiteralValue() && sign.getLiteralValue())
    {
        dest = sign.isZeroInitializer() ? src : Value(Literal(-src.getLiteralValue()->signedInt()), src.type);
    }
    else
    {
        /*
         * Calculation of signed value:
         *
         * %tmp = xor %src, %sign
         * %dest = sub %tmp, %sign
         *
         * To restore positive value (%sign = 0):
         * %tmp = %src
         * %dest = sub %src, 0 -> %src
         *
         * To restore negative value (%sign = -1):
         * %tmp = ~%src
         * %dest = sub ~%src, -1 -> ~%src + 1 -> tow's complement
         *
         * Source:
         * https://llvm.org/doxygen/IntegerDivision_8cpp_source.html
         */

        //%tmp = xor %src, %sign
        Value tmp = assign(it, src.type, "%twos_complement") = src ^ sign;
        //%dest = sub %tmp, %sign
        if(!dest.isWriteable())
            dest = method.addNewLocal(src.type, "%twos_complement");
        assign(it, dest) = tmp - sign;
    }
    return it;
}

InstructionWalker intermediate::insertCalculateIndices(InstructionWalker it, Method& method, const Value& container,
    const Value& dest, const std::vector<Value>& indices, const bool firstIndexIsElement)
{
    // handle multi-level indices
    Value offset = INT_ZERO;
    DataType subContainerType = container.type;
    for(const Value& index : indices)
    {
        Value subOffset(UNDEFINED_VALUE);
        if(subContainerType.getPointerType() || subContainerType.getArrayType())
        {
            // index is index in pointer/array
            //-> add offset of element at given index to global offset
            if(auto lit = index.getLiteralValue())
            {
                subOffset = Value(Literal(lit->signedInt() *
                                      static_cast<int32_t>(subContainerType.getElementType().getInMemoryWidth())),
                    TYPE_INT32);
            }
            else
            {
                subOffset = method.addNewLocal(TYPE_INT32, "%index_offset");
                it.emplace(new intermediate::IntrinsicOperation("mul", Value(subOffset), Value(index),
                    Value(Literal(subContainerType.getElementType().getInMemoryWidth()), TYPE_INT32)));
                it->addDecorations(InstructionDecorations::SIGNED_OVERFLOW_IS_UB);
                it.nextInBlock();
            }

            // according to SPIR-V 1.2 specification, the type doesn't change if the first index is the "element":
            //"The type of Base after being dereferenced with Element is still the same as the original type of Base."
            if(!firstIndexIsElement || &index != &indices.front())
                subContainerType = subContainerType.getElementType();
        }
        else if(auto structType = subContainerType.getStructType())
        {
            // index is element in struct -> MUST be literal
            if(!index.getLiteralValue())
                throw CompilationError(CompilationStep::LLVM_2_IR, "Can't access struct-element with non-literal index",
                    index.to_string());

            subOffset = Value(Literal(structType->getStructSize(index.getLiteralValue()->signedInt())), TYPE_INT32);
            subContainerType = subContainerType.getElementType(index.getLiteralValue()->signedInt());
        }
        else if(subContainerType.isVectorType())
        {
            // takes the address of an element of the vector
            if(auto lit = index.getLiteralValue())
                subOffset = Value(Literal(lit->signedInt() *
                                      static_cast<int32_t>(subContainerType.getElementType().getInMemoryWidth())),
                    TYPE_INT32);
            else
                subOffset = assign(it, TYPE_INT32, "%vector_element_offset") =
                    (index * Literal(subContainerType.getElementType().getInMemoryWidth()),
                        InstructionDecorations::UNSIGNED_RESULT, InstructionDecorations::UNSIGNED_OVERFLOW_IS_UB);
            subContainerType = subContainerType.getElementType();
        }
        else
            throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid container-type to retrieve element via index",
                subContainerType.to_string());

        if(offset.getLiteralValue() && subOffset.getLiteralValue())
        {
            offset = Value(
                Literal(offset.getLiteralValue()->signedInt() + subOffset.getLiteralValue()->signedInt()), TYPE_INT32);
        }
        else if(offset.isZeroInitializer())
        {
            // previous offset is zero -> zero + x = x
            offset = subOffset;
        }
        else if(subOffset.isZeroInitializer())
        {
            // sub-offset is zero -> x + zero = x
            // offset = offset -> do nothing
        }
        else
        {
            Value tmp = assign(it, TYPE_INT32, "%index_offset") =
                (offset + subOffset, InstructionDecorations::SIGNED_OVERFLOW_IS_UB);
            offset = tmp;
        }
    }
    // add last offset to container
    assign(it, dest) =
        (container + offset, InstructionDecorations::UNSIGNED_RESULT, InstructionDecorations::UNSIGNED_OVERFLOW_IS_UB);

    /*
     * associates the index with the local/parameter it refers to.
     * This is required, so the input/output-parameters are correctly recognized
     *
     * NOTE: The associated index can only be set, if there is a single literal index.
     * (Or the element is element 0, than the reference-index can be retrieved from the second index)
     */
    Value index = UNDEFINED_VALUE;
    if(indices.size() == 1)
        index = indices[0];
    if(firstIndexIsElement && indices.at(0).isZeroInitializer())
        index = indices.size() > 1 ? indices.at(1) : UNDEFINED_VALUE;
    const int refIndex = index.getLiteralValue().value_or(Literal(ANY_ELEMENT)).signedInt();
    if(dest.type.getPointerType() && container.checkLocal())
        const_cast<Local*>(dest.local())->set(ReferenceData(*container.local(), refIndex));

    DataType finalType = subContainerType;
    if(subContainerType.getArrayType() && !dest.type.getElementType().getArrayType())
    {
        // decay x[num] to x* (unless we actually address the array itself)
        auto arrayType = subContainerType.getArrayType();
        finalType = method.createPointerType(arrayType->elementType,
            container.type.getPointerType() ? container.type.getPointerType()->addressSpace : AddressSpace::PRIVATE);
    }
    else if(!(firstIndexIsElement && indices.size() == 1))
        finalType = method.createPointerType(subContainerType, container.type.getPointerType()->addressSpace);

    if(dest.type != finalType)
    {
        LCOV_EXCL_START
        logging::error() << "Final index does not match expected type for source " << container.to_string()
                         << ", destination " << dest.to_string() << ", final index type " << finalType.to_string()
                         << " and indices: " << to_string<Value>(indices)
                         << (firstIndexIsElement ? " (first index is element)" : "") << logging::endl;
        throw CompilationError(CompilationStep::LLVM_2_IR, "Types of retrieving indices do not match!",
            dest.type.to_string() + " and " + finalType.to_string());
        LCOV_EXCL_STOP
    }

    return it;
}

InstructionWalker intermediate::insertByteSwap(
    InstructionWalker it, Method& method, const Value& src, const Value& dest)
{
    /*
     * llvm.bswap:
     * "The llvm.bswap.i16 intrinsic returns an i16 value that has the high and low byte of the input i16 swapped.
     * Similarly, the llvm.bswap.i32 intrinsic returns an i32 value that has the four bytes of the input i32 swapped,
     * so that if the input bytes are numbered 0, 1, 2, 3 then the returned i32 will have its bytes in 3, 2, 1, 0 order.
     * "
     */
    auto numBytes = src.type.getScalarBitCount() / 8;

    if(numBytes == 2)
    {
        // TODO shorts lose signedness!

        // ? ? A B -> 0 ? ? A
        Value tmpA0 = assign(it, src.type, "byte_swap") = as_unsigned{src} >> 8_val;
        // ? ? A B -> ? A B 0
        Value tmpB0 = assign(it, src.type, "byte_swap") = src << 8_val;
        // 0 ? ? A -> 0 0 0 A
        Value tmpA1 = assign(it, src.type, "byte_swap") = tmpA0 & 0x000000FF_val;
        // ? A B 0 -> 0 0 B 0
        Value tmpB1 = assign(it, src.type, "byte_swap") = tmpB0 & 0x0000FF00_val;
        // 0 0 0 A | 0 0 B 0 -> 0 0 A B
        assign(it, dest) = tmpA1 | tmpB1;
    }
    else if(numBytes == 4)
    {
        // A B C D -> B C D A
        const Value tmpAC0 = method.addNewLocal(src.type, "byte_swap");
        it.emplace(new Operation(OP_ROR, tmpAC0, src, Value(Literal(24u), TYPE_INT8)));
        it.nextInBlock();
        // A B C D -> D A B C
        const Value tmpBD0 = method.addNewLocal(src.type, "byte_swap");
        it.emplace(new Operation(OP_ROR, tmpBD0, src, Value(Literal(8u), TYPE_INT8)));
        it.nextInBlock();
        // B C D A -> 0 0 0 A
        Value tmpA1 = assign(it, src.type, "byte_swap") = tmpAC0 & 0x000000FF_val;
        // D A B C -> 0 0 B 0
        Value tmpB1 = assign(it, src.type, "byte_swap") = tmpBD0 & 0x0000FF00_val;
        // B C D A -> 0 C 0 0
        Value tmpC1 = assign(it, src.type, "byte_swap") = tmpAC0 & 0x00FF0000_val;
        // D A B C -> D 0 0 0
        Value tmpD1 = assign(it, src.type, "byte_swap") = tmpBD0 & 0xFF000000_val;
        // 0 0 0 A | 0 0 B 0 -> 0 0 B A
        Value tmpAB2 = assign(it, src.type, "byte_swap") = tmpA1 | tmpB1;
        // 0 C 0 0 | D 0 0 0 -> D C 0 0
        Value tmpCD2 = assign(it, src.type, "byte_swap") = tmpC1 | tmpD1;
        // 0 0 B A | D C 0 0 -> D C B A
        assign(it, dest) = tmpAB2 | tmpCD2;
    }
    else
        throw CompilationError(
            CompilationStep::GENERAL, "Invalid number of bytes for byte-swap", std::to_string(numBytes));

    return it;
}

static bool isIdentityOperand(const Optional<Value>& identity, const Value& operand)
{
    if(auto lit = identity & &Value::getLiteralValue)
        return operand.getLiteralValue() == lit;
    return false;
}

Value intermediate::getSourceValue(Value value)
{
    while(auto writer = value.getSingleWriter())
    {
        if(writer->isSimpleMove() && !writer->hasConditionalExecution())
        {
            auto src = writer->getMoveSource().value();
            if(src.checkRegister())
                // e.g. to avoid returning the replication register (or UNIFORM) where the contents are
                // position-dependent
                break;
            value = src;
        }
        else
        {
            auto writeOp = dynamic_cast<const Operation*>(writer);
            if(writeOp && writeOp->isSimpleOperation() && !writeOp->hasConditionalExecution() &&
                writeOp->getArguments().size() == 2 &&
                (isIdentityOperand(writeOp->op.getLeftIdentity(), writeOp->assertArgument(0)) ||
                    isIdentityOperand(writeOp->op.getRightIdentity(), writeOp->assertArgument(1))))
            {
                auto src = writeOp->assertArgument(writeOp->op.getLeftIdentity() == writeOp->assertArgument(0) ? 1 : 0);
                if(src.checkRegister())
                    // e.g. to avoid returning the replication register (or UNIFORM) where the contents are
                    // position-dependent
                    break;
                value = src;
            }
            else
                break;
        }
    }
    return value;
}

const IntermediateInstruction* intermediate::getSourceInstruction(const IntermediateInstruction* inst)
{
    while(inst)
    {
        const IntermediateInstruction* writer = nullptr;
        if(inst->isSimpleMove() && !inst->hasConditionalExecution() &&
            (writer = inst->getMoveSource() & &Value::getSingleWriter))
            inst = writer;
        else
            break;
    }
    return inst;
}

static const Local* getSourceLocal(const Local* local)
{
    while(auto writer = local->getSingleWriter())
    {
        if(writer->isSimpleMove() && !writer->hasConditionalExecution() && writer->readsLocal())
            local = writer->getMoveSource().value().local();
        else
            break;
    }
    return local;
}

FastSet<const Local*> intermediate::getEquivalenceClass(const Local* local)
{
    FastSet<const Local*> clazz;

    // go backward until the beginning of the class to make sure we get all the branches
    auto startLocal = getSourceLocal(local);
    // go forward - this takes all the possible branches
    std::function<void(const LocalUser*)> processNext = [&](const LocalUser* reader) {
        if(reader && reader->isSimpleMove() && !reader->hasConditionalExecution() && reader->checkOutputLocal())
        {
            auto out = reader->getOutput()->local();
            if(out->getSingleWriter() == reader)
            {
                // if there are multiple writers, the local could take different values
                clazz.emplace(out);
                out->forUsers(LocalUse::Type::READER, processNext);
            }
        }
    };

    clazz.emplace(startLocal);
    startLocal->forUsers(LocalUse::Type::READER, processNext);

    return clazz;
}

BasicBlock& intermediate::insertLoop(
    Method& method, InstructionWalker& it, const Value& conditionValue, const std::string& label)
{
    auto loopLabel = method.addNewLocal(TYPE_LABEL, label);
    auto headerLabel = method.addNewLocal(TYPE_LABEL, loopLabel.local()->name, "header");
    auto afterLoopLabel = method.addNewLocal(TYPE_LABEL, loopLabel.local()->name, "after");

    // we need to insert all blocks before inserting the branches to them to make a possible existing CFG happy!
    auto headerIt = method.emplaceLabel(it, new BranchLabel(*headerLabel.local()));
    auto inLoopIt = method.emplaceLabel(headerIt.copy().nextInBlock(), new BranchLabel(*loopLabel.local()));
    it = method.emplaceLabel(inLoopIt.copy().nextInBlock(), new BranchLabel(*afterLoopLabel.local()));

    // in the header, jump over loop only when condition becomes false, otherwise fall through loop content block
    headerIt.nextInBlock();
    BranchCond cond = BRANCH_ALWAYS;
    std::tie(headerIt, cond) = insertBranchCondition(method, headerIt, conditionValue);
    headerIt.emplace(new Branch(loopLabel.local(), cond));
    headerIt.nextInBlock();
    headerIt.emplace(new Branch(afterLoopLabel.local(), cond.invert()));
    headerIt.nextInBlock();

    // in loop content block, unconditionally jump back to header
    inLoopIt.nextInBlock();
    inLoopIt.emplace(new Branch(headerLabel.local()));
    inLoopIt.nextInBlock();

    return *inLoopIt.getBasicBlock();
}

std::pair<InstructionWalker, BranchCond> intermediate::insertBranchCondition(Method& method, InstructionWalker it,
    const Value& conditionValue, std::bitset<NATIVE_VECTOR_SIZE> conditionalElements, bool branchOnAllElements)
{
    /*
     * branch usually only depends on scalar value
     * -> set any not used vector-element (all except element 0) to a value where it doesn't influence
     * the condition
     *
     * Using ELEMENT_NUMBER sets the vector-elements 1 to 15 to a non-zero value and 0 to either 0 (if
     * condition was false) or 1 (if condition was true)
     */
    if(conditionalElements == 0x1)
        // default case for simple jump on 0th element
        assign(it, NOP_REGISTER) = (ELEMENT_NUMBER_REGISTER | conditionValue, SetFlag::SET_FLAGS);
    else
    {
        // more special case for jump on different element(s)
        auto elementMask = it.getBasicBlock()->getMethod().addNewLocal(TYPE_INT8.toVectorType(16));
        it.emplace(new intermediate::LoadImmediate(elementMask,
            static_cast<uint32_t>((~conditionalElements).to_ulong()), intermediate::LoadType::PER_ELEMENT_UNSIGNED));
        it.nextInBlock();
        assign(it, NOP_REGISTER) = (elementMask | conditionValue, SetFlag::SET_FLAGS);
    }
    return std::make_pair(it, BRANCH_ALL_Z_CLEAR);
}

std::pair<Optional<Value>, std::bitset<NATIVE_VECTOR_SIZE>> intermediate::getBranchCondition(
    const ExtendedInstruction* inst)
{
    if(!inst || !inst->doesSetFlag())
        return std::make_pair(NO_VALUE, 0);

    auto op = dynamic_cast<const Operation*>(inst);
    if(!op || op->op != OP_OR)
        return std::make_pair(NO_VALUE, 0);

    // simple case
    if(inst->readsRegister(REG_ELEMENT_NUMBER))
        return std::make_pair(inst->findOtherArgument(ELEMENT_NUMBER_REGISTER), 0x1);

    auto toBitset = [](Literal lit) -> std::bitset<NATIVE_VECTOR_SIZE> {
        // element is set if at least one of low or high parts are set
        return (lit.unsignedInt() >> 16u) | (lit.unsignedInt() & 0xFFu);
    };

    // more special case
    if(auto writer = dynamic_cast<const LoadImmediate*>(op->getFirstArg().getSingleWriter()))
    {
        if(writer->type != LoadType::REPLICATE_INT32)
            return std::make_pair(op->getSecondArg(), toBitset(writer->getImmediate()));
    }
    if(auto writer = dynamic_cast<const LoadImmediate*>(op->assertArgument(1).getSingleWriter()))
    {
        if(writer->type != LoadType::REPLICATE_INT32)
            return std::make_pair(op->getFirstArg(), toBitset(writer->getImmediate()));
    }

    // failed to determine
    return std::make_pair(NO_VALUE, 0);
}

void intermediate::redirectAllBranches(BasicBlock& oldTarget, BasicBlock& newTarget)
{
    // Since (at least if the CFG for the method is already created), by resetting the branch to this block we modify
    // the incoming edges we iterate over, we need to split the finding the function to modify and replacing it.
    FastAccessList<InstructionWalker> instructionsToBeReset;
    oldTarget.forPredecessors([&](InstructionWalker walker) {
        if(auto branch = walker.get<intermediate::Branch>())
        {
            auto targets = branch->getTargetLabels();
            if(targets.find(oldTarget.getLabel()->getLabel()) != targets.end())
            {
                if(targets.size() != 1)
                    throw CompilationError(CompilationStep::GENERAL,
                        "Removing dynamic branch to target is not yet implemented", branch->to_string());
                instructionsToBeReset.emplace_back(walker);
            }
        }
        // fall-throughs are already handled by inserting the block
    });

    for(auto& walker : instructionsToBeReset)
    {
        auto branch = walker.get<intermediate::Branch>();
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Resetting branch in block '" << walker.getBasicBlock()->to_string() << "' to jump to '"
                << newTarget.to_string() << "' instead: " << walker->to_string() << logging::endl);
        // need to reset the instruction to correctly update the CFG
        walker.reset((new intermediate::Branch(newTarget.getLabel()->getLabel(), branch->branchCondition))
                         ->copyExtrasFrom(branch));
    }
}

bool intermediate::needsDelay(
    const IntermediateInstruction* firstInst, const IntermediateInstruction* secondInst, const Local* local)
{
    // we also need to insert an instruction, if the local is unpacked in any successive instruction,
    // in which case it cannot be on an accumulator. Since we have a direct read-after-write, the local
    // can also not be on register-file A -> we need to insert buffer
    bool isUnpacked = false;
    local->forUsers(LocalUse::Type::READER, [&isUnpacked](const LocalUser* user) {
        if(user->hasUnpackMode())
            isUnpacked = true;
    });

    auto hasPackMode = firstInst->hasPackMode();
    if(auto combined = dynamic_cast<const CombinedOperation*>(firstInst))
        hasPackMode = (combined->getFirstOp() && combined->getFirstOp()->hasPackMode()) ||
            (combined->getSecondOp() && combined->getSecondOp()->hasPackMode());

    auto hasUnpackMode = secondInst->hasUnpackMode();
    if(auto combined = dynamic_cast<const CombinedOperation*>(secondInst))
        hasUnpackMode = (combined->getFirstOp() && combined->getFirstOp()->hasUnpackMode()) ||
            (combined->getSecondOp() && combined->getSecondOp()->hasUnpackMode());

    bool isDynamicBranchAddress = false;
    if(auto branch = dynamic_cast<const Branch*>(secondInst))
        // addresses used for branches need to be read from physical register-file A
        isDynamicBranchAddress = branch->isDynamicBranch() && branch->getTarget() == local->createReference();

    return hasPackMode || hasUnpackMode || secondInst->getVectorRotation() || isUnpacked || isDynamicBranchAddress;
}

bool intermediate::needsDelay(
    InstructionWalker firstIt, InstructionWalker secondIt, const Local* local, std::size_t accumulatorThreshold)
{
    return needsDelay(firstIt.get(), secondIt.get(), local) ||
        !firstIt.getBasicBlock()->isLocallyLimited(firstIt, local, accumulatorThreshold);
}

bool intermediate::needsDelay(const IntermediateInstruction* firstInst, const IntermediateInstruction* secondInst)
{
    if((firstInst->writesRegister(REG_REPLICATE_ALL) || firstInst->writesRegister(REG_REPLICATE_QUAD) ||
           firstInst->writesRegister(REG_ACC5)) &&
        secondInst->readsRegister(REG_ACC5) && secondInst->getVectorRotation())
        // below we only check for local, so explicitly check for registers here
        return true;
    bool isDelayNeeded = false;
    firstInst->forUsedLocals([&](const Local* loc, LocalUse::Type type, const IntermediateInstruction&) {
        if(has_flag(type, LocalUse::Type::WRITER) && secondInst->readsLocal(loc))
            isDelayNeeded = isDelayNeeded || needsDelay(firstInst, secondInst, loc);
    });
    return isDelayNeeded;
}
