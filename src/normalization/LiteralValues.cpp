/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LiteralValues.h"

#include "../InstructionWalker.h"
#include "../Module.h"
#include "../Profiler.h"
#include "../intermediate/VectorHelper.h"
#include "log.h"

#include <algorithm>
#include <cmath>
#include <unordered_map>

using namespace vc4c;
using namespace vc4c::normalization;

static Optional<Literal> getMostCommonElement(const SIMDVector& container, uint8_t numRelevantEntries)
{
    FastMap<Literal, unsigned> histogram;
    histogram.reserve(container.size());
    for(uint8_t i = 0; i < std::min(container.size(), numRelevantEntries); ++i)
    {
        ++histogram[container[i]];
    }

    auto maxVal = histogram.begin();
    for(auto it = histogram.begin(); it != histogram.end(); ++it)
    {
        if(it->second > maxVal->second)
            maxVal = it;
    }
    // if all are equal, default to first element
    return maxVal->second == 1 ? Optional<Literal>{} : maxVal->first;
}

static bool fitsIntoUnsignedMaskedLoad(Literal lit)
{
    return lit.unsignedInt() == 0 || lit.unsignedInt() == 1 || lit.unsignedInt() == 2 || lit.unsignedInt() == 3;
}

static bool fitsIntoSignedMaskedLoad(Literal lit)
{
    return lit.signedInt() == 0 || lit.signedInt() == 1 || lit.signedInt() == -2 || lit.signedInt() == -1;
}

static NODISCARD InstructionWalker copyVector(Method& method, InstructionWalker it, const Value& out, const Value& in)
{
    const auto& inContainer = in.vector();
    if(inContainer.isAllSame())
    {
        // if all values within a container are the same, there is no need to extract them separately, a simple move
        // (e.g. load) will do
        it.emplace((new intermediate::MoveOperation(out, Value(inContainer[0], in.type.getElementType())))
                       ->copyExtrasFrom(it.get()));
        return it.nextInBlock();
    }
    if(inContainer.isElementNumber(false, false, true))
    {
        // if the value in the container corresponds to the element-number, simply copy it
        it.emplace((new intermediate::MoveOperation(out, ELEMENT_NUMBER_REGISTER))->copyExtrasFrom(it.get()));
        return it.nextInBlock();
    }
    if(inContainer.isElementNumber(true, false, true))
    {
        // if the value in the container corresponds to the element number plus an offset (a general ascending range),
        // convert to addition
        auto offset = inContainer[0].signedInt();
        it.emplace((new intermediate::Operation(OP_ADD, out, ELEMENT_NUMBER_REGISTER, Value(Literal(offset), in.type)))
                       ->copyExtrasFrom(it.get()));
        return it.nextInBlock();
    }
    if(inContainer.isElementNumber(false, true, true) && inContainer[NATIVE_VECTOR_SIZE - 1].unsignedInt() < (1 << 16))
    {
        // if the value in the container corresponds to the element number times a factor (a general ascending range
        // with step of more than 1), convert to multiplication (only if it fits in mul24)
        auto factor = inContainer[1].signedInt();
        it.emplace(
            (new intermediate::Operation(OP_MUL24, out, ELEMENT_NUMBER_REGISTER, Value(Literal(factor), in.type)))
                ->copyExtrasFrom(it.get()));
        return it.nextInBlock();
    }
    if(std::all_of(inContainer.begin(), inContainer.end(), fitsIntoUnsignedMaskedLoad))
    {
        // if the container elements all fit into the results of an unsigned bit-masked load, use such an instruction
        auto mask =
            intermediate::LoadImmediate::fromLoadedValues(inContainer, intermediate::LoadType::PER_ELEMENT_UNSIGNED);
        it.emplace(new intermediate::LoadImmediate(out, mask, intermediate::LoadType::PER_ELEMENT_UNSIGNED));
        it->copyExtrasFrom(it.get());
        return it.nextInBlock();
    }
    if(std::all_of(inContainer.begin(), inContainer.end(), fitsIntoSignedMaskedLoad))
    {
        // if the container elements all fit into the results of an signed bit-masked load, use such an instruction
        auto mask =
            intermediate::LoadImmediate::fromLoadedValues(inContainer, intermediate::LoadType::PER_ELEMENT_SIGNED);
        it.emplace(new intermediate::LoadImmediate(out, mask, intermediate::LoadType::PER_ELEMENT_SIGNED));
        it->copyExtrasFrom(it.get());
        return it.nextInBlock();
    }
    if(auto sources = intermediate::checkVectorCanBeAssembled(in.type, inContainer))
    {
        // try the more complex vector assembly before falling back to almost complete element-wise
        return intermediate::insertAssembleVector(it, method, out, *std::move(sources));
    }
    Value realOut = out;
    if(!out.checkLocal())
    {
        realOut = method.addNewLocal(TYPE_UNKNOWN, "%container");
    }
    if(!out.checkRegister() && !out.type.isUnknown() && in.type.getVectorWidth() > out.type.getVectorWidth())
    {
        throw CompilationError(CompilationStep::OPTIMIZER, "Input vector has invalid type", in.to_string(false, true));
    }

    // the input could be an array lowered into register, so the type is not required to be a vector
    auto typeWidth =
        in.type.getArrayType() ? static_cast<uint8_t>(in.type.getArrayType()->size) : in.type.getVectorWidth();
    auto elemType = in.type.getElementType();
    // copy first element without test for flags, so the register allocator finds an unconditional write of the
    // container
    it.emplace(new intermediate::MoveOperation(realOut, Value(inContainer[0], elemType)));
    it.nextInBlock();
    // optimize this by looking for the most common value and initializing all (but the first) elements with this one
    // all other elements (not in the original data, but part of the SIMD vector, e.g. if source has less than 16
    // elements), we don't care about their value
    auto maxOccurrence = getMostCommonElement(inContainer, typeWidth);
    if(maxOccurrence && maxOccurrence.value() != inContainer[0])
    {
        it.emplace(new intermediate::Operation(
            OP_SUB, NOP_REGISTER, INT_ZERO, ELEMENT_NUMBER_REGISTER, COND_ALWAYS, SetFlag::SET_FLAGS));
        it.nextInBlock();
        it.emplace(new intermediate::MoveOperation(realOut, Value(maxOccurrence.value(), elemType), COND_NEGATIVE_SET));
        it->addDecorations(intermediate::InstructionDecorations::ELEMENT_INSERTION);
        it.nextInBlock();
    }
    for(uint8_t i = 0; i < typeWidth; ++i)
    {
        if(maxOccurrence && inContainer[i] == maxOccurrence.value())
            continue;
        // 1) set flags for element i
        it.emplace(new intermediate::Operation(OP_XOR, NOP_REGISTER, ELEMENT_NUMBER_REGISTER,
            Value(SmallImmediate(i), TYPE_INT8), COND_ALWAYS, SetFlag::SET_FLAGS));
        it.nextInBlock();

        // 2) copy element i of the input vector to the output
        it.emplace(new intermediate::MoveOperation(realOut, Value(inContainer[i], elemType), COND_ZERO_SET));
        it->addDecorations(intermediate::InstructionDecorations::ELEMENT_INSERTION);
        it.nextInBlock();
    }
    if(realOut != out)
    {
        it.emplace((new intermediate::MoveOperation(out, realOut))->copyExtrasFrom(it.get()));
        it.nextInBlock();
    }
    return it;
}

InstructionWalker normalization::handleContainer(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    // TODO signals, flags, conditional
    intermediate::MoveOperation* move = it.get<intermediate::MoveOperation>();
    intermediate::Operation* op = it.get<intermediate::Operation>();
    intermediate::VectorRotation* rot = it.get<intermediate::VectorRotation>();
    if(rot != nullptr && rot->getSource().checkVector() && rot->getOffset().getRotationOffset())
    {
        Value src = rot->getSource();
        // vector rotation -> rotate container (if static offset)
        // TODO negative offset possible?
        std::size_t offset = rot->getOffset().getRotationOffset().value();
        //"Rotates the order of the elements in the range [first,last), in such a way that the element pointed by middle
        // becomes the new first element."
        if(rot->type == intermediate::RotationType::PER_QUAD)
        {
            // TODO per-quad rotation. First need to find a code, where this is actually used...
            throw CompilationError(
                CompilationStep::NORMALIZER, "Rotating constant vectors is currently not supported", it->to_string());
        }
        // need to rotate all (possible non-existing) 16 elements, so use a temporary vector with 16 elements and rotate
        // it
        SIMDVector tmp = src.vector().rotate(static_cast<uint8_t>(offset));
        rot->setSource(method.module.storeVector(std::move(tmp), src.type));
        // TODO next step could be optimized, if we used the vector-rotation to extract an element
        // In which case, a simple copy suffices?? At least, we don't need to set the other elements
    }
    // jump from previous block to next one intended, so no "else"
    // if the source is a container and the offset is a literal (already applied above), remove the rotation (see move
    // branch) if the source is a container and the offset is no literal, extract the container, but keep rotation
    if(rot != nullptr && rot->getSource().checkVector() && !rot->getOffset().getRotationOffset())
    {
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Rewriting rotation from container " << rot->to_string() << logging::endl);
        auto tmp = method.addNewLocal(rot->getSource().type);
        // insert vector assembly and use the resulting vector in the rotation
        it = copyVector(method, it, tmp, move->getSource());
        it->setArgument(0, std::move(tmp));
    }
    else if(move != nullptr && move->getSource().checkVector())
    {
        if(!move->getSource().type.getPointerType())
        {
            CPPLOG_LAZY(
                logging::Level::DEBUG, log << "Rewriting move from container " << move->to_string() << logging::endl);
            it = copyVector(method, it, move->getOutput().value(), move->getSource());
            it.erase();
            // don't skip next instruction
            it.previousInBlock();
        }
    }
    else if(op != nullptr && (op->getFirstArg().checkVector() || (op->getSecondArg() & &Value::checkVector)))
    {
        for(std::size_t i = 0; i < op->getArguments().size(); ++i)
        {
            // for special containers, rewrite to scalars/registers
            if(auto con = op->assertArgument(i).checkVector())
            {
                if(con->isAllSame())
                {
                    const Value& container = op->assertArgument(i);
                    op->setArgument(i, Value((*con)[0], container.type));
                }
                else if(con->isElementNumber())
                {
                    op->setArgument(i, Value(REG_ELEMENT_NUMBER, op->assertArgument(i).type));
                }
            }
        }
        if(op->getFirstArg().checkVector() && !op->getFirstArg().type.getPointerType())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Rewriting operation with container-input " << op->to_string() << logging::endl);
            const Value tmpVal = method.addNewLocal(op->getFirstArg().type, "%container");
            it = copyVector(method, it, tmpVal, op->getFirstArg());
            op->setArgument(0, std::move(tmpVal));
            // don't skip next instruction
            it.previousInBlock();
        }
        if((op->getSecondArg() & &Value::checkVector) && !op->assertArgument(1).type.getPointerType())
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Rewriting operation with container-input " << op->to_string() << logging::endl);
            const Value tmpVal = method.addNewLocal(op->assertArgument(1).type, "%container");
            it = copyVector(method, it, tmpVal, op->assertArgument(1));
            op->setArgument(1, std::move(tmpVal));
            // don't skip next instruction
            it.previousInBlock();
        }
    }
    // TODO other operations too
    return it;
}

struct ImmediateHandler
{
    // if set to true, value was changed and any of the other values are valid
    bool changeValue = false;
    // if set to true, value must be loaded via a load-immediate instruction
    bool loadImmediate = false;
    // if set to SHL, the value is the immediate left-shifted by itself (y << y)
    // if set to ADD/FADD, the value is the immediate added with itself (y + y)
    // if set to MUL24/FMUL, value is the square (y * y) of the given immediate
    OpCode opCode = OP_NOP;
    // the immediate to load/use
    SmallImmediate immediate = SmallImmediate(0);
};

struct ImmediateSupplier
{
    const OpCode& opCode;
    const SmallImmediate immediate;

    // insert operation on one of the ALUs
    ImmediateSupplier(const OpCode& opCode, SmallImmediate imm) : opCode(opCode), immediate(imm) {}
    // no operation required, directly use small immediate value
    explicit constexpr ImmediateSupplier(SmallImmediate imm) : opCode(OP_NOP), immediate(imm) {}
};

static Literal toLiteral(uint32_t mask, float f)
{
    // static_assert(bit_cast<float, uint32_t>(f) == mask, "Values do not match!");
    if(bit_cast<float, uint32_t>(f) != mask)
        throw CompilationError(CompilationStep::GENERAL,
            std::string("Small immediate values do not match for ") + std::to_string(mask) + " and",
            (std::to_string(f) + " aka ") + std::to_string(bit_cast<float, uint32_t>(f)));
    return Literal(mask);
}

static Literal toLiteral(uint32_t mask, int32_t j)
{
    if((static_cast<int64_t>(j) & 0xFFFFFFFF) != (static_cast<int64_t>(mask) & 0xFFFFFFFF))
        throw CompilationError(CompilationStep::GENERAL,
            std::string("Small immediate values do not match for ") + std::to_string(mask) + " and", std::to_string(j));
    return Literal(mask);
}

// source: http://maazl.de/project/vc4asm/doc/smallimmediate.html
static const std::map<Literal, ImmediateSupplier> immediateMappings = {
    {toLiteral(0x00000000, 0), ImmediateSupplier(SmallImmediate(0))},
    {toLiteral(0x00000001, 1), ImmediateSupplier(SmallImmediate(1))},
    {toLiteral(0x00000002, 2), ImmediateSupplier(SmallImmediate(2))},
    {toLiteral(0x00000003, 3), ImmediateSupplier(SmallImmediate(3))},
    {toLiteral(0x00000004, 4), ImmediateSupplier(SmallImmediate(4))},
    {toLiteral(0x00000005, 5), ImmediateSupplier(SmallImmediate(5))},
    {toLiteral(0x00000006, 6), ImmediateSupplier(SmallImmediate(6))},
    {toLiteral(0x00000007, 7), ImmediateSupplier(SmallImmediate(7))},
    {toLiteral(0x00000008, 8), ImmediateSupplier(SmallImmediate(8))},
    {toLiteral(0x00000009, 9), ImmediateSupplier(SmallImmediate(9))},
    {toLiteral(0x0000000a, 10), ImmediateSupplier(SmallImmediate(10))},
    {toLiteral(0x0000000b, 11), ImmediateSupplier(SmallImmediate(11))},
    {toLiteral(0x0000000c, 12), ImmediateSupplier(SmallImmediate(12))},
    {toLiteral(0x0000000d, 13), ImmediateSupplier(SmallImmediate(13))},
    {toLiteral(0x0000000e, 14), ImmediateSupplier(SmallImmediate(14))},
    {toLiteral(0x0000000f, 15), ImmediateSupplier(SmallImmediate(15))},
    {toLiteral(0x00000010, 8 + 8), ImmediateSupplier(OP_V8ADDS, SmallImmediate(8))},
    //{toLiteral(0x00000010, 8 + 8), ImmediateSupplier(OP_ADD, SmallImmediate(8))},
    //{toLiteral(0x00000010, 4 * 4), ImmediateSupplier(OP_MUL24, SmallImmediate(4))},
    {toLiteral(0x00000012, 9 + 9), ImmediateSupplier(OP_V8ADDS, SmallImmediate(9))},
    //{toLiteral(0x00000012, 9 + 9), ImmediateSupplier(OP_ADD, SmallImmediate(9))},
    {toLiteral(0x00000014, 10 + 10), ImmediateSupplier(OP_V8ADDS, SmallImmediate(10))},
    //{toLiteral(0x00000014, 10 + 10), ImmediateSupplier(OP_ADD, SmallImmediate(10))},
    {toLiteral(0x00000016, 11 + 11), ImmediateSupplier(OP_V8ADDS, SmallImmediate(11))},
    //{toLiteral(0x00000016, 11 + 11), ImmediateSupplier(OP_ADD, SmallImmediate(11))},
    {toLiteral(0x00000018, 12 + 12), ImmediateSupplier(OP_V8ADDS, SmallImmediate(12))},
    //{toLiteral(0x00000018, 12 + 12), ImmediateSupplier(OP_ADD, SmallImmediate(12))},
    //{toLiteral(0x00000018, 3 << 3), ImmediateSupplier(OP_SHL, SmallImmediate(3))},
    {toLiteral(0x00000019, 5 * 5), ImmediateSupplier(OP_MUL24, SmallImmediate(5))},
    {toLiteral(0x0000001a, 13 + 13), ImmediateSupplier(OP_V8ADDS, SmallImmediate(13))},
    //{toLiteral(0x0000001a, 13 + 13), ImmediateSupplier(OP_ADD, SmallImmediate(13))},
    {toLiteral(0x0000001c, 14 + 14), ImmediateSupplier(OP_V8ADDS, SmallImmediate(14))},
    //{toLiteral(0x0000001c, 14 + 14), ImmediateSupplier(OP_ADD, SmallImmediate(14))},
    {toLiteral(0x0000001c, 28), ImmediateSupplier(OP_CLZ, SmallImmediate(8))},
    {toLiteral(0x0000001d, 29), ImmediateSupplier(OP_CLZ, SmallImmediate(4))},
    {toLiteral(0x0000001e, 15 + 15), ImmediateSupplier(OP_V8ADDS, SmallImmediate(15))},
    //{toLiteral(0x0000001e, 15 + 15), ImmediateSupplier(OP_ADD, SmallImmediate(15))},
    //{toLiteral(0x0000001e, 30), ImmediateSupplier(OP_CLZ, SmallImmediate(2))},
    {toLiteral(0x0000001f, 31), ImmediateSupplier(OP_CLZ, SmallImmediate(1))},
    {toLiteral(0x00000020, static_cast<int32_t>(32.0f)), ImmediateSupplier(OP_FTOI, SmallImmediate(37))},
    //{toLiteral(0x00000020, 32), ImmediateSupplier(OP_CLZ, SmallImmediate(0))},
    {toLiteral(0x00000024, 6 * 6), ImmediateSupplier(OP_MUL24, SmallImmediate(6))},
    {toLiteral(0x00000040, 4 << 4), ImmediateSupplier(OP_SHL, SmallImmediate(4))},
    {toLiteral(0x00000031, 7 * 7), ImmediateSupplier(OP_MUL24, SmallImmediate(7))},
    {toLiteral(0x00000040, 8 * 8), ImmediateSupplier(OP_MUL24, SmallImmediate(8))},
    {toLiteral(0x00000051, 9 * 9), ImmediateSupplier(OP_MUL24, SmallImmediate(9))},
    {toLiteral(0x00000064, 10 * 10), ImmediateSupplier(OP_MUL24, SmallImmediate(10))},
    {toLiteral(0x00000079, 11 * 11), ImmediateSupplier(OP_MUL24, SmallImmediate(11))},
    {toLiteral(0x00000080, static_cast<int32_t>(128.0f)), ImmediateSupplier(OP_FTOI, SmallImmediate(39))},
    {toLiteral(0x00000090, 12 * 12), ImmediateSupplier(OP_MUL24, SmallImmediate(12))},
    {toLiteral(0x000000a0, 5 << 5), ImmediateSupplier(OP_SHL, SmallImmediate(5))},
    {toLiteral(0x000000a9, 13 * 13), ImmediateSupplier(OP_MUL24, SmallImmediate(13))},
    {toLiteral(0x000000c4, 14 * 14), ImmediateSupplier(OP_MUL24, SmallImmediate(14))},
    {toLiteral(0x000000e1, 15 * 15), ImmediateSupplier(OP_MUL24, SmallImmediate(15))},

    {toLiteral(0x00000180, 6 << 6), ImmediateSupplier(OP_SHL, SmallImmediate(6))},
    {toLiteral(0x00000380, 7 << 7), ImmediateSupplier(OP_SHL, SmallImmediate(7))},
    {toLiteral(0x00000800, 8 << 8), ImmediateSupplier(OP_SHL, SmallImmediate(8))},
    {toLiteral(0x00001200, 9 << 9), ImmediateSupplier(OP_SHL, SmallImmediate(9))},
    {toLiteral(0x00002800, 10 << 10), ImmediateSupplier(OP_SHL, SmallImmediate(10))},
    {toLiteral(0x00005800, 11 << 11), ImmediateSupplier(OP_SHL, SmallImmediate(11))},
    {toLiteral(0x0000c000, 12 << 12), ImmediateSupplier(OP_SHL, SmallImmediate(12))},
    {toLiteral(0x0001a000, 13 << 13), ImmediateSupplier(OP_SHL, SmallImmediate(13))},
    {toLiteral(0x00038000, 14 << 14), ImmediateSupplier(OP_SHL, SmallImmediate(14))},
    {toLiteral(0x00078000, 15 << 15), ImmediateSupplier(OP_SHL, SmallImmediate(15))},

    {Literal(0x001e0000u), ImmediateSupplier(OP_ROR, SmallImmediate(15))},
    {Literal(0x00380000u), ImmediateSupplier(OP_ROR, SmallImmediate(14))},
    {Literal(0x00680000u), ImmediateSupplier(OP_ROR, SmallImmediate(13))},
    {Literal(0x00c00000u), ImmediateSupplier(OP_ROR, SmallImmediate(12))},
    {Literal(0x01600000u), ImmediateSupplier(OP_ROR, SmallImmediate(11))},
    {Literal(0x02800000u), ImmediateSupplier(OP_ROR, SmallImmediate(10))},
    {Literal(0x04800000u), ImmediateSupplier(OP_ROR, SmallImmediate(9))},
    {Literal(0x08000000u), ImmediateSupplier(OP_ROR, SmallImmediate(8))},
    {Literal(0x0e000000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(41))},
    //{Literal(0x0e000000u), ImmediateSupplier(OP_ROR, SmallImmediate(7))},
    {Literal(0x0e400000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(40))},
    //{Literal(0x0e400000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(42))},
    {Literal(0x0f000000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(43))},
    //{Literal(0x0f000000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(45))},
    {Literal(0x0f400000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(44))},
    //{Literal(0x0f400000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(46))},
    {Literal(0x10000000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(47))},
    //{Literal(0x10000000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(33))},
    {Literal(0x10400000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(32))},
    //{Literal(0x10400000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(34))},
    {Literal(0x11000000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(35))},
    //{Literal(0x11000000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(37))},
    {Literal(0x11400000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(36))},
    //{Literal(0x11400000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(38))},
    {Literal(0x12000000u), ImmediateSupplier(OP_V8MULD, SmallImmediate(39))},
    {Literal(0x18000000u), ImmediateSupplier(OP_ROR, SmallImmediate(6))},
    {Literal(0x28000000u), ImmediateSupplier(OP_ROR, SmallImmediate(5))},

    {toLiteral(0x37800000, 1.0f / (256.0f * 256.0f)), ImmediateSupplier(OP_FMUL, SmallImmediate(40))},
    {toLiteral(0x38800000, 1.0f / (128.0f * 128.0f)), ImmediateSupplier(OP_FMUL, SmallImmediate(41))},
    {toLiteral(0x39800000, 1.0f / (64.0f * 64.0f)), ImmediateSupplier(OP_FMUL, SmallImmediate(42))},
    {toLiteral(0x3a800000, 1.0f / (32.0f * 32.0f)), ImmediateSupplier(OP_FMUL, SmallImmediate(43))},
    {toLiteral(0x3b800000, 1.0f / 256.0f), ImmediateSupplier(SmallImmediate(40))},
    {toLiteral(0x3c000000, 1.0f / 128.0f), ImmediateSupplier(SmallImmediate(41))},
    {toLiteral(0x3c800000, 1.0f / 64.0f), ImmediateSupplier(SmallImmediate(42))},
    {toLiteral(0x3d000000, 1.0f / 32.0f), ImmediateSupplier(SmallImmediate(43))},
    {toLiteral(0x3d800000, 1.0f / 16.0f), ImmediateSupplier(SmallImmediate(44))},
    {toLiteral(0x3e000000, 1.0f / 8.0f), ImmediateSupplier(SmallImmediate(45))},
    {toLiteral(0x3e800000, 1.0f / 4.0f), ImmediateSupplier(SmallImmediate(46))},
    {toLiteral(0x3f000000, 1.0f / 2.0f), ImmediateSupplier(SmallImmediate(47))},
    {toLiteral(0x3f800000, 1.0f), ImmediateSupplier(SmallImmediate(32))},
    {toLiteral(0x40000000, 2.0f), ImmediateSupplier(SmallImmediate(33))},
    //{Literal(0x40000000u), ImmediateSupplier(OP_ROR, SmallImmediate(4))},
    {toLiteral(0x40400000, static_cast<float>(3)), ImmediateSupplier(OP_ITOF, SmallImmediate(3))},
    {toLiteral(0x40800000, 4.0f), ImmediateSupplier(SmallImmediate(34))},
    {toLiteral(0x40a00000, static_cast<float>(5)), ImmediateSupplier(OP_ITOF, SmallImmediate(5))},
    {toLiteral(0x40c00000, static_cast<float>(6)), ImmediateSupplier(OP_ITOF, SmallImmediate(6))},
    {toLiteral(0x40e00000, static_cast<float>(7)), ImmediateSupplier(OP_ITOF, SmallImmediate(7))},
    {toLiteral(0x41000000, 8.0f), ImmediateSupplier(SmallImmediate(35))},
    {toLiteral(0x41100000, static_cast<float>(9)), ImmediateSupplier(OP_ITOF, SmallImmediate(9))},
    {toLiteral(0x41200000, static_cast<float>(10)), ImmediateSupplier(OP_ITOF, SmallImmediate(10))},
    {toLiteral(0x41300000, static_cast<float>(11)), ImmediateSupplier(OP_ITOF, SmallImmediate(11))},
    {toLiteral(0x41400000, static_cast<float>(12)), ImmediateSupplier(OP_ITOF, SmallImmediate(12))},
    {toLiteral(0x41500000, static_cast<float>(13)), ImmediateSupplier(OP_ITOF, SmallImmediate(13))},
    {toLiteral(0x41600000, static_cast<float>(14)), ImmediateSupplier(OP_ITOF, SmallImmediate(14))},
    {toLiteral(0x41700000, static_cast<float>(15)), ImmediateSupplier(OP_ITOF, SmallImmediate(15))},
    {toLiteral(0x41800000, 16.0f), ImmediateSupplier(SmallImmediate(36))},
    {toLiteral(0x42000000, 32.0f), ImmediateSupplier(SmallImmediate(37))},
    {toLiteral(0x42800000, 64.0f), ImmediateSupplier(SmallImmediate(38))},
    {toLiteral(0x43000000, 128.0f), ImmediateSupplier(SmallImmediate(39))},
    {toLiteral(0x43800000, 16.0f * 16.0f), ImmediateSupplier(OP_FMUL, SmallImmediate(36))},
    //{toLiteral(0x43800000, 128.0f + 128.0f), ImmediateSupplier(OP_FADD, SmallImmediate(39))},
    {toLiteral(0x44800000, 32.0f * 32.0f), ImmediateSupplier(OP_FMUL, SmallImmediate(37))},
    {toLiteral(0x45800000, 64.0f * 64.0f), ImmediateSupplier(OP_FMUL, SmallImmediate(38))},
    {toLiteral(0x46800000, 128.0f * 128.0f), ImmediateSupplier(OP_FMUL, SmallImmediate(39))},

    {toLiteral(0x4e6e0000, static_cast<float>(bit_cast<float, int32_t>(1.0f / 256.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(40))},
    {toLiteral(0x4e700000, static_cast<float>(bit_cast<float, int32_t>(1.0f / 128.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(41))},
    {toLiteral(0x4e720000, static_cast<float>(bit_cast<float, int32_t>(1.0f / 64.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(42))},
    {toLiteral(0x4e740000, static_cast<float>(bit_cast<float, int32_t>(1.0f / 32.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(43))},
    {toLiteral(0x4e760000, static_cast<float>(bit_cast<float, int32_t>(1.0f / 16.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(44))},
    {toLiteral(0x4e780000, static_cast<float>(bit_cast<float, int32_t>(1.0f / 8.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(45))},
    {toLiteral(0x4e7a0000, static_cast<float>(bit_cast<float, int32_t>(1.0f / 4.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(46))},
    {toLiteral(0x4e7c0000, static_cast<float>(bit_cast<float, int32_t>(1.0f / 2.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(47))},
    {toLiteral(0x4e7e0000, static_cast<float>(bit_cast<float, int32_t>(1.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(32))},
    {toLiteral(0x4e800000, static_cast<float>(bit_cast<float, int32_t>(2.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(33))},
    {toLiteral(0x4e810000, static_cast<float>(bit_cast<float, int32_t>(4.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(34))},
    {toLiteral(0x4e820000, static_cast<float>(bit_cast<float, int32_t>(8.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(35))},
    {toLiteral(0x4e830000, static_cast<float>(bit_cast<float, int32_t>(16.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(36))},
    {toLiteral(0x4e840000, static_cast<float>(bit_cast<float, int32_t>(32.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(37))},
    {toLiteral(0x4e850000, static_cast<float>(bit_cast<float, int32_t>(64.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(38))},
    {toLiteral(0x4e860000, static_cast<float>(bit_cast<float, int32_t>(128.0f))),
        ImmediateSupplier(OP_ITOF, SmallImmediate(39))},

    {Literal(0x60000000u), ImmediateSupplier(OP_ROR, SmallImmediate(3))},

    {Literal(0x76ff0000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(40))},
    {toLiteral(0x77000000, bit_cast<float, int32_t>(1.0f / 256.0f) + bit_cast<float, int32_t>(1.0f / 256.0f)),
        ImmediateSupplier(OP_ADD, SmallImmediate(40))},
    {Literal(0x78000000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(41))},
    //{toLiteral(0x78000000, std::pow(2.0f, 113.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(41))},
    {Literal(0x78ff0000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(42))},
    {toLiteral(0x79000000, bit_cast<float, int32_t>(1.0f / 64.0f) + bit_cast<float, int32_t>(1.0f / 64.0f)),
        ImmediateSupplier(OP_ADD, SmallImmediate(42))},
    {Literal(0x7a000000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(43))},
    //{toLiteral(0x7a000000, std::pow(2.0f, 117.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(43))},
    {Literal(0x7aff0000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(44))},
    {toLiteral(0x7b000000, bit_cast<float, int32_t>(1.0f / 16.0f) + bit_cast<float, int32_t>(1.0f / 16.0f)),
        ImmediateSupplier(OP_ADD, SmallImmediate(44))},
    {Literal(0x7c000000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(45))},
    //{toLiteral(0x7c000000, std::pow(2.0f, 121.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(45))},
    {Literal(0x7cff0000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(46))},
    {toLiteral(0x7d000000, bit_cast<float, int32_t>(1.0f / 4.0f) + bit_cast<float, int32_t>(1.0f / 4.0f)),
        ImmediateSupplier(OP_ADD, SmallImmediate(46))},
    {Literal(0x7e000000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(47))},
    //{toLiteral(0x7e000000, std::pow(2.0f, 125.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(47))},
    {Literal(0x7eff0000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(32))},
    {toLiteral(0x7f000000, bit_cast<float, int32_t>(1.0f) + bit_cast<float, int32_t>(1.0f)),
        ImmediateSupplier(OP_ADD, SmallImmediate(32))},
    {toLiteral(0x80000000, bit_cast<float, int32_t>(2.0f) + bit_cast<float, int32_t>(2.0f)),
        ImmediateSupplier(OP_V8ADDS, SmallImmediate(33))},
    //-2^-126 (a denormal number), pow results in -0.0
    //{Literal(0x80000000u), ImmediateSupplier(OP_ADD, SmallImmediate(33))},
    {Literal(0x80ff0000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(34))},
    //-2^-125, pow results in -0.0
    {toLiteral(0x81000000, bit_cast<float, int32_t>(4.0f) + bit_cast<float, int32_t>(4.0f)),
        ImmediateSupplier(OP_ADD, SmallImmediate(34))},
    {Literal(0x82000000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(35))},
    //-2^-123, pow results in -0.0
    //{Literal(0x82000000u), ImmediateSupplier(OP_ADD, SmallImmediate(35))},
    {Literal(0x82ff0000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(36))},
    //-2^-121, pow results in -0.0
    {toLiteral(0x83000000, bit_cast<float, int32_t>(16.0f) + bit_cast<float, int32_t>(16.0f)),
        ImmediateSupplier(OP_ADD, SmallImmediate(36))},
    {Literal(0x84000000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(37))},
    //-2^-119, pow results in -0.0
    //{Literal(0x84000000u), ImmediateSupplier(OP_ADD, SmallImmediate(37))},
    {Literal(0x84ff0000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(38))},
    //-2^-117, pow results in -0.0
    {toLiteral(0x85000000, bit_cast<float, int32_t>(64.0f) + bit_cast<float, int32_t>(64.0f)),
        ImmediateSupplier(OP_ADD, SmallImmediate(38))},
    {Literal(0x86000000u), ImmediateSupplier(OP_V8ADDS, SmallImmediate(39))},
    //-2^-115, pow results in -0.0
    //{Literal(0x86000000u), ImmediateSupplier(OP_ADD, SmallImmediate(39))},

    {toLiteral(0xbcffffff, ~0x43000000), ImmediateSupplier(OP_NOT, SmallImmediate(39))},
    {toLiteral(0xbd7fffff, ~0x42800000), ImmediateSupplier(OP_NOT, SmallImmediate(38))},
    {toLiteral(0xbdffffff, ~0x42000000), ImmediateSupplier(OP_NOT, SmallImmediate(37))},
    {toLiteral(0xbe7fffff, ~0x41800000), ImmediateSupplier(OP_NOT, SmallImmediate(36))},
    {toLiteral(0xbeffffff, ~0x41000000), ImmediateSupplier(OP_NOT, SmallImmediate(35))},
    {toLiteral(0xbf7fffff, ~0x40800000), ImmediateSupplier(OP_NOT, SmallImmediate(34))},
    {toLiteral(0xbfffffff, ~0x40000000), ImmediateSupplier(OP_NOT, SmallImmediate(33))},
    {toLiteral(0xbf800000, static_cast<float>(-1)), ImmediateSupplier(OP_ITOF, SmallImmediate(31))},
    {toLiteral(0xc0000000, static_cast<float>(-2)), ImmediateSupplier(OP_ITOF, SmallImmediate(30))},
    {toLiteral(0xc0400000, static_cast<float>(-3)), ImmediateSupplier(OP_ITOF, SmallImmediate(29))},
    {toLiteral(0xc0800000, static_cast<float>(-4)), ImmediateSupplier(OP_ITOF, SmallImmediate(28))},
    {toLiteral(0xc0a00000, static_cast<float>(-5)), ImmediateSupplier(OP_ITOF, SmallImmediate(27))},
    {toLiteral(0xc0c00000, static_cast<float>(-6)), ImmediateSupplier(OP_ITOF, SmallImmediate(26))},
    {toLiteral(0xc0e00000, static_cast<float>(-7)), ImmediateSupplier(OP_ITOF, SmallImmediate(25))},
    {toLiteral(0xc1000000, static_cast<float>(-8)), ImmediateSupplier(OP_ITOF, SmallImmediate(24))},
    {toLiteral(0xc1100000, static_cast<float>(-9)), ImmediateSupplier(OP_ITOF, SmallImmediate(23))},
    {toLiteral(0xc1200000, static_cast<float>(-10)), ImmediateSupplier(OP_ITOF, SmallImmediate(22))},
    {toLiteral(0xc1300000, static_cast<float>(-11)), ImmediateSupplier(OP_ITOF, SmallImmediate(21))},
    {toLiteral(0xc1400000, static_cast<float>(-12)), ImmediateSupplier(OP_ITOF, SmallImmediate(20))},
    {toLiteral(0xc1500000, static_cast<float>(-13)), ImmediateSupplier(OP_ITOF, SmallImmediate(19))},
    {toLiteral(0xc1600000, static_cast<float>(-14)), ImmediateSupplier(OP_ITOF, SmallImmediate(18))},
    {toLiteral(0xc1700000, static_cast<float>(-15)), ImmediateSupplier(OP_ITOF, SmallImmediate(17))},
    {toLiteral(0xc1800000, static_cast<float>(-16)), ImmediateSupplier(OP_ITOF, SmallImmediate(16))},

    {toLiteral(0xc07fffff, ~0x3f800000), ImmediateSupplier(OP_NOT, SmallImmediate(32))},
    {toLiteral(0xc0ffffff, ~0x3f000000), ImmediateSupplier(OP_NOT, SmallImmediate(47))},
    {toLiteral(0xc17fffff, ~0x3e800000), ImmediateSupplier(OP_NOT, SmallImmediate(46))},
    {toLiteral(0xc1ffffff, ~0x3e000000), ImmediateSupplier(OP_NOT, SmallImmediate(45))},
    {toLiteral(0xc27fffff, ~0x3d800000), ImmediateSupplier(OP_NOT, SmallImmediate(44))},
    {toLiteral(0xc2ffffff, ~0x3d000000), ImmediateSupplier(OP_NOT, SmallImmediate(43))},
    {toLiteral(0xc37fffff, ~0x3c800000), ImmediateSupplier(OP_NOT, SmallImmediate(42))},
    {toLiteral(0xc3ffffff, ~0x3c000000), ImmediateSupplier(OP_NOT, SmallImmediate(41))},
    {toLiteral(0xc47fffff, ~0x3b800000), ImmediateSupplier(OP_NOT, SmallImmediate(40))},

    {toLiteral(
         0xe0000100, static_cast<int32_t>(static_cast<int64_t>(-16 & 0xFFFFFF) * static_cast<int64_t>(-16 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(16))},
    {toLiteral(
         0xe20000e1, static_cast<int32_t>(static_cast<int64_t>(-15 & 0xFFFFFF) * static_cast<int64_t>(-15 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(17))},
    {toLiteral(
         0xe40000c4, static_cast<int32_t>(static_cast<int64_t>(-14 & 0xFFFFFF) * static_cast<int64_t>(-14 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(18))},
    {toLiteral(
         0xe60000a9, static_cast<int32_t>(static_cast<int64_t>(-13 & 0xFFFFFF) * static_cast<int64_t>(-13 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(18))},
    {toLiteral(
         0xe8000090, static_cast<int32_t>(static_cast<int64_t>(-12 & 0xFFFFFF) * static_cast<int64_t>(-12 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(20))},
    {toLiteral(
         0xea000079, static_cast<int32_t>(static_cast<int64_t>(-11 & 0xFFFFFF) * static_cast<int64_t>(-11 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(21))},
    {toLiteral(
         0xec000064, static_cast<int32_t>(static_cast<int64_t>(-10 & 0xFFFFFF) * static_cast<int64_t>(-10 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(22))},
    {toLiteral(
         0xee000051, static_cast<int32_t>(static_cast<int64_t>(-9 & 0xFFFFFF) * static_cast<int64_t>(-9 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(23))},
    {toLiteral(
         0xf0000040, static_cast<int32_t>(static_cast<int64_t>(-8 & 0xFFFFFF) * static_cast<int64_t>(-8 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(24))},
    {toLiteral(
         0xf2000031, static_cast<int32_t>(static_cast<int64_t>(-7 & 0xFFFFFF) * static_cast<int64_t>(-7 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(25))},
    {toLiteral(
         0xf4000024, static_cast<int32_t>(static_cast<int64_t>(-6 & 0xFFFFFF) * static_cast<int64_t>(-6 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(26))},
    {toLiteral(
         0xf6000019, static_cast<int32_t>(static_cast<int64_t>(-5 & 0xFFFFFF) * static_cast<int64_t>(-5 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(27))},
    {toLiteral(
         0xf8000010, static_cast<int32_t>(static_cast<int64_t>(-4 & 0xFFFFFF) * static_cast<int64_t>(-4 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(28))},
    {toLiteral(
         0xfa000009, static_cast<int32_t>(static_cast<int64_t>(-3 & 0xFFFFFF) * static_cast<int64_t>(-3 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(29))},
    {toLiteral(
         0xfc000004, static_cast<int32_t>(static_cast<int64_t>(-2 & 0xFFFFFF) * static_cast<int64_t>(-2 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(30))},
    {toLiteral(
         0xfe000001, static_cast<int32_t>(static_cast<int64_t>(-1 & 0xFFFFFF) * static_cast<int64_t>(-1 & 0xFFFFFF))),
        ImmediateSupplier(OP_MUL24, SmallImmediate(31))},

    {Literal(0xfff0ffffu), ImmediateSupplier(OP_ROR, SmallImmediate(16))},
    {Literal(0xfff8ffffu), ImmediateSupplier(OP_ROR, SmallImmediate(17))},
    {Literal(0xfffcbfffu), ImmediateSupplier(OP_ROR, SmallImmediate(18))},
    {Literal(0xfffe7fffu), ImmediateSupplier(OP_ROR, SmallImmediate(19))},
    {Literal(0xffff4fffu), ImmediateSupplier(OP_ROR, SmallImmediate(20))},
    {Literal(0xffffafffu), ImmediateSupplier(OP_ROR, SmallImmediate(21))},
    {Literal(0xffffdbffu), ImmediateSupplier(OP_ROR, SmallImmediate(22))},
    {Literal(0xffffefffu), ImmediateSupplier(OP_ROR, SmallImmediate(23))},
    {Literal(0xfffff8ffu), ImmediateSupplier(OP_ROR, SmallImmediate(24))},
    {Literal(0xfffffcffu), ImmediateSupplier(OP_ROR, SmallImmediate(25))},
    {Literal(0xfffffebfu), ImmediateSupplier(OP_ROR, SmallImmediate(26))},

    {toLiteral(0xffffffe0, -16 - 16), ImmediateSupplier(OP_ADD, SmallImmediate(16))},
    {toLiteral(0xffffffe2, -30), ImmediateSupplier(OP_V8MULD, SmallImmediate(16))},
    //{toLiteral(0xffffffe2, -15 - 15), ImmediateSupplier(OP_ADD, SmallImmediate(17))},
    {toLiteral(0xffffffe4, -28), ImmediateSupplier(OP_V8MULD, SmallImmediate(17))},
    //{toLiteral(0xffffffe4, -14 - 14), ImmediateSupplier(OP_ADD, SmallImmediate(18))},
    {toLiteral(0xffffffe6, -26), ImmediateSupplier(OP_V8MULD, SmallImmediate(18))},
    ///{toLiteral(0xffffffe6, -13 - 13), ImmediateSupplier(OP_ADD, SmallImmediate(19))},
    {toLiteral(0xffffffe8, -24), ImmediateSupplier(OP_V8MULD, SmallImmediate(19))},
    //{toLiteral(0xffffffe8, -12 - 12), ImmediateSupplier(OP_ADD, SmallImmediate(20))},
    {toLiteral(0xffffffe9, -23), ImmediateSupplier(OP_V8MULD, SmallImmediate(20))},
    {toLiteral(0xffffffea, -11 - 11), ImmediateSupplier(OP_ADD, SmallImmediate(21))},
    {toLiteral(0xffffffeb, -21), ImmediateSupplier(OP_V8MULD, SmallImmediate(21))},
    {toLiteral(0xffffffec, -10 - 10), ImmediateSupplier(OP_ADD, SmallImmediate(22))},
    {toLiteral(0xffffffed, -19), ImmediateSupplier(OP_V8MULD, SmallImmediate(22))},
    {toLiteral(0xffffffee, -9 - 9), ImmediateSupplier(OP_ADD, SmallImmediate(23))},
    {toLiteral(0xffffffef, -17), ImmediateSupplier(OP_V8MULD, SmallImmediate(23))},
    //{toLiteral(0xffffffef, -17), ImmediateSupplier(OP_ROR, SmallImmediate(29))},
    {toLiteral(0xfffffff0, -16), ImmediateSupplier(SmallImmediate(16))},
    {toLiteral(0xfffffff1, -15), ImmediateSupplier(SmallImmediate(17))},
    {toLiteral(0xfffffff2, -14), ImmediateSupplier(SmallImmediate(18))},
    {toLiteral(0xfffffff3, -13), ImmediateSupplier(SmallImmediate(19))},
    {toLiteral(0xfffffff4, -12), ImmediateSupplier(SmallImmediate(20))},
    {toLiteral(0xfffffff5, -11), ImmediateSupplier(SmallImmediate(21))},
    {toLiteral(0xfffffff6, -10), ImmediateSupplier(SmallImmediate(22))},
    {toLiteral(0xfffffff7, -9), ImmediateSupplier(SmallImmediate(23))},
    {toLiteral(0xfffffff8, -8), ImmediateSupplier(SmallImmediate(24))},
    {toLiteral(0xfffffff9, -7), ImmediateSupplier(SmallImmediate(25))},
    {toLiteral(0xfffffffa, -6), ImmediateSupplier(SmallImmediate(26))},
    {toLiteral(0xfffffffb, -5), ImmediateSupplier(SmallImmediate(27))},
    {toLiteral(0xfffffffc, -4), ImmediateSupplier(SmallImmediate(28))},
    {toLiteral(0xfffffffd, -3), ImmediateSupplier(SmallImmediate(29))},
    {toLiteral(0xfffffffe, -2), ImmediateSupplier(SmallImmediate(30))},
    {toLiteral(0xffffffff, -1), ImmediateSupplier(SmallImmediate(31))},
};

/*
 * New version:
 * - passes more test-cases (179 vs. 178)
 * - is slightly faster than old version (complete SingleSteps slower than before, probably due to reversion via
 * precalculate)
 * - introduces less additional instructions (~2464 for 243 test-cases, complete SingleSteps!)
 * - causes less loads to be combined (~10k), resulting in 	more instructions remaining
 * - requires more split read-after-write (~100)
 * - results in ~300 less replaced NOPs (either no match found or NOP not introduced)
 * - allows for ~4k more ALU instructions being combined
 * - requires CodeGenerator to fix more register-errors (case 2: 74 vs. 61) and 153 vs. 123 fixToRegister calls
 * ->results in a total of ~5k more instructions (!!also with 1 complete test-case more!!)
 *
 */
static ImmediateHandler mapImmediateValue(const Literal& source)
{
    ImmediateHandler handler;
    handler.changeValue = true;

    auto it = immediateMappings.find(source);
    if(it != immediateMappings.end())
    {
        handler.immediate = it->second.immediate;
        if(it->second.opCode == OP_NOP)
            // directly use value
            return handler;
        else
            handler.opCode = it->second.opCode;
        return handler;
    }
    handler.loadImmediate = true;
    return handler;
}

static NODISCARD InstructionWalker handleImmediateInOperation(
    Method& method, InstructionWalker it, intermediate::Operation* op)
{
    for(std::size_t i = 0; i < op->getArguments().size(); ++i)
    {
        const Value source = op->assertArgument(i);
        if(auto lit = source.checkLiteral())
        {
            PROFILE_START(mapImmediateValue);
            ImmediateHandler mapped = mapImmediateValue(*lit);
            PROFILE_END(mapImmediateValue);
            if(mapped.changeValue)
            {
                const Value tmp = method.addNewLocal(source.type, "%immediate");

                // value was changed
                if(mapped.loadImmediate)
                {
                    // requires load immediate
                    CPPLOG_LAZY(
                        logging::Level::DEBUG, log << "Loading immediate value: " << lit->to_string() << logging::endl);
                    it.emplace(new intermediate::LoadImmediate(tmp, *lit, op->conditional));
                    // propagate the decorations so the loads are displayed as setups, not value loads
                    if(op->hasDecoration(intermediate::InstructionDecorations::VPM_READ_CONFIGURATION))
                        it->addDecorations(intermediate::InstructionDecorations::VPM_READ_CONFIGURATION);
                    if(op->hasDecoration(intermediate::InstructionDecorations::VPM_WRITE_CONFIGURATION))
                        it->addDecorations(intermediate::InstructionDecorations::VPM_WRITE_CONFIGURATION);
                    it.nextInBlock();
                    op->setArgument(i, std::move(tmp));
                }
                else if(mapped.opCode != OP_NOP)
                {
                    DataType type = mapped.immediate.getFloatingValue() ? TYPE_FLOAT : TYPE_INT32;
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Calculating immediate value " << lit->to_string() << " with operation '"
                            << mapped.opCode.name << "' and immediate value " << mapped.immediate.to_string()
                            << logging::endl);
                    if(mapped.opCode.numOperands == 1)
                        it.emplace(new intermediate::Operation(
                            mapped.opCode, tmp, Value(mapped.immediate, type), op->conditional));
                    else
                        it.emplace(new intermediate::Operation(mapped.opCode, tmp, Value(mapped.immediate, type),
                            Value(mapped.immediate, type), op->conditional));
                    it.nextInBlock();
                    op->setArgument(i, std::move(tmp));
                }
                else
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Mapping constant for immediate value " << lit->to_string()
                            << " to: " << mapped.immediate.to_string() << logging::endl);
                    op->setArgument(i, Value(mapped.immediate, source.type));
                }
            }
        }
    }

    return it;
}

static NODISCARD InstructionWalker handleImmediateInMove(
    Method& method, InstructionWalker it, intermediate::MoveOperation* move)
{
    Value source = move->getSource();
    if(auto lit = source.checkLiteral())
    {
        PROFILE_START(mapImmediateValue);
        ImmediateHandler mapped = mapImmediateValue(*lit);
        PROFILE_END(mapImmediateValue);
        if(mapped.changeValue)
        {
            // value was changed
            if(mapped.loadImmediate)
            {
                // requires load immediate
                CPPLOG_LAZY(
                    logging::Level::DEBUG, log << "Loading immediate value: " << lit->to_string() << logging::endl);
                it.reset((new intermediate::LoadImmediate(move->getOutput().value(), *lit))->copyExtrasFrom(move));
            }
            else if(mapped.opCode != OP_NOP)
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Calculating immediate value " << lit->to_string() << " with operation '"
                        << mapped.opCode.name << "' and immediate value " << mapped.immediate.to_string()
                        << logging::endl);
                if(mapped.opCode.numOperands == 1)
                    it.reset((new intermediate::Operation(mapped.opCode, move->getOutput().value(),
                                  Value(mapped.immediate, move->getSource().type)))
                                 ->copyExtrasFrom(move));
                else
                    it.reset((new intermediate::Operation(mapped.opCode, move->getOutput().value(),
                                  Value(mapped.immediate, move->getSource().type),
                                  Value(mapped.immediate, move->getSource().type)))
                                 ->copyExtrasFrom(move));
            }
            else
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Mapping constant for immediate value " << lit->to_string()
                        << " to: " << mapped.immediate.to_string() << logging::endl);
                move->setSource(Value(mapped.immediate, source.type));
            }
        }
    }
    return it;
}

InstructionWalker normalization::handleImmediate(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    if(auto move = it.get<intermediate::MoveOperation>())
    {
        it = handleImmediateInMove(method, it, move);
    }
    if(auto op = it.get<intermediate::Operation>())
    {
        // check for both arguments
        it = handleImmediateInOperation(method, it, op);
    }
    if(auto comb = it.get<intermediate::CombinedOperation>())
    {
        if(comb->getFirstOp() != nullptr)
            it = handleImmediateInOperation(method, it, const_cast<intermediate::Operation*>(comb->getFirstOp()));
        if(comb->getSecondOP() != nullptr)
            it = handleImmediateInOperation(method, it, const_cast<intermediate::Operation*>(comb->getSecondOP()));
    }
    return it;
}

static NODISCARD InstructionWalker findWriteOfLocal(InstructionWalker it, const Local* loc)
{
    // TODO could already abort after X steps (X being the accumulator threshold)
    while(!it.isStartOfBlock() && !(it->checkOutputLocal() == loc))
    {
        it.previousInBlock();
    }
    return it;
}

static const std::string localPrefix = "%use_with_literal";

static Optional<Value> findPreviousUseWithImmediate(
    InstructionWalker it, const Value& arg, unsigned accumulatorThreshold)
{
    auto instRemaining = accumulatorThreshold;

    while(instRemaining > 0 && !it.isStartOfBlock())
    {
        if(it.get<intermediate::MoveOperation>() && it->getArgument(0) == arg && it->conditional == COND_ALWAYS &&
            (it->getOutput() & [](const Value& val) -> bool {
                return val.checkLocal() && val.local()->name.find(localPrefix) == 0;
            }))
        {
            return it->getOutput();
        }

        --instRemaining;
        it.previousInBlock();
    }

    return NO_VALUE;
}

InstructionWalker normalization::handleUseWithImmediate(
    const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    //- for all locals used together with small immediate values
    //- if the range of the local exceeds the accumulator threshold
    //-> copy the local before the use into a new temporary, which is used with the immediate instead
    //-> fixes blocked register-file B for long-living locals
    if(auto op = it.get<intermediate::Operation>())
    {
        if(op->readsLiteral())
        {
            // at least one immediate value is used
            const auto& args = op->getArguments();
            const auto localIt =
                std::find_if(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.checkLocal(); });
            if(localIt != args.end() &&
                !it.getBasicBlock()->isLocallyLimited(findWriteOfLocal(it, localIt->local()), localIt->local(),
                    config.additionalOptions.accumulatorThreshold))
            {
                // one other local is used and its range is greater than the accumulator threshold
                // check if we have introduced an earlier use-with-immediate for the same value within the
                // accumulator-range
                Optional<Value> prefTemp =
                    findPreviousUseWithImmediate(it, *localIt, config.additionalOptions.accumulatorThreshold);
                const Local* oldLocal = localIt->local();
                if(prefTemp)
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Re-using temporary to split up use of long-living local with immediate value: "
                            << op->to_string() << logging::endl);
                    op->replaceLocal(oldLocal, prefTemp->local(), LocalUse::Type::READER);
                }
                else
                {
                    CPPLOG_LAZY(logging::Level::DEBUG,
                        log << "Inserting temporary to split up use of long-living local with immediate value: "
                            << op->to_string() << logging::endl);
                    Value tmp = method.addNewLocal(localIt->type, localPrefix);
                    if(auto data = localIt->local()->get<ReferenceData>())
                        // the use-with literal also references the value referenced by the original local
                        tmp.local()->set(ReferenceData(*data));
                    it.emplace(new intermediate::MoveOperation(tmp, *localIt));
                    // since we simply move the source, some decorations for the writing of the source still apply
                    // TODO or more generally propagate (unsigned) decoration for every moves and some operations (e.g.
                    // and with constant/unsigned, etc.)
                    if(localIt->getSingleWriter() != nullptr)
                        it->addDecorations(intermediate::forwardDecorations(localIt->getSingleWriter()->decoration));
                    it.nextInBlock();
                    op->replaceLocal(oldLocal, tmp.local(), LocalUse::Type::READER);
                }
            }
            const auto registerIt =
                std::find_if(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.checkRegister(); });
            if(registerIt != args.end() && registerIt->reg().file == RegisterFile::PHYSICAL_B)
            {
                CPPLOG_LAZY(logging::Level::DEBUG,
                    log << "Inserting temporary to split up use of physical register on file B with immediate value: "
                        << op->to_string() << logging::endl);
                auto tmp = method.addNewLocal(registerIt->type);
                it.emplace(new intermediate::MoveOperation(tmp, *registerIt));
                it.nextInBlock();
                op->replaceValue(*registerIt, tmp, LocalUse::Type::READER);
            }
        }
    }

    return it;
}

Optional<SmallImmediate> normalization::toImmediate(Literal lit)
{
    auto it = immediateMappings.find(lit);
    if(it != immediateMappings.end() && it->second.opCode == OP_NOP)
        return it->second.immediate;
    return {};
}
