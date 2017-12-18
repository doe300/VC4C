/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LiteralValues.h"

#include "../InstructionWalker.h"
#include "../Profiler.h"
#include "log.h"

#include <algorithm>
#include <cmath>

using namespace vc4c;
using namespace vc4c::optimizations;

static InstructionWalker copyVector(Method& method, InstructionWalker it, const Value& out, const Value& in)
{
	if(in.container.isAllSame())
	{
		//if all values within a container are the same, there is no need to extract them separately, a simple move (e.g. load) will do
		it.emplace((new intermediate::MoveOperation(out, in.container.elements.at(0)))->copyExtrasFrom(it.get()));
		return it.nextInBlock();
	}
	if(in.container.isElementNumber())
	{
		//if the value in the container corresponds to the element-number, simply copy it
		it.emplace((new intermediate::MoveOperation(out, ELEMENT_NUMBER_REGISTER))->copyExtrasFrom(it.get()));
		return it.nextInBlock();
	}
    Value realOut = out;
    if(!out.hasType(ValueType::LOCAL))
    {
        realOut = method.addNewLocal(TYPE_UNKNOWN, "%container");
    }
    if(!out.hasType(ValueType::REGISTER) && !out.type.isUnknown() && in.type.getVectorWidth() > out.type.getVectorWidth())
    {
        throw CompilationError(CompilationStep::OPTIMIZER, "Input vector has invalid type", in.type.to_string());
    }
    
    for(std::size_t i = 0; i < in.type.getVectorWidth(); ++i)
    {
    	//copy first element without test for flags, so the register allocator finds an unconditional write of the container
        //1) set flags for element i
    	if(i > 0)
    	{
			it.emplace(new intermediate::Operation(OP_XOR, NOP_REGISTER, ELEMENT_NUMBER_REGISTER, Value(SmallImmediate(i), TYPE_INT8), COND_ALWAYS, SetFlag::SET_FLAGS));
			it.nextInBlock();
    	}
        //2) copy element i of the input vector to the output
        it.emplace(new intermediate::MoveOperation(realOut, in.getCompoundPart(i), i == 0 ? COND_ALWAYS : COND_ZERO_SET));
        it.nextInBlock();
    }
    if(realOut != out)
    {
        it.emplace((new intermediate::MoveOperation(out, realOut))->copyExtrasFrom(it.get()));
        it.nextInBlock();
    }
    return it;    
}

InstructionWalker optimizations::handleContainer(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
    //TODO signals, flags, conditional
	intermediate::MoveOperation* move = it.get<intermediate::MoveOperation>();
	intermediate::Operation* op = it.get<intermediate::Operation>();
	intermediate::VectorRotation* rot = it.get<intermediate::VectorRotation>();
	if(rot != nullptr && rot->getSource().hasType(ValueType::CONTAINER) && (rot->getOffset().hasType(ValueType::LITERAL) || rot->getOffset().hasType(ValueType::SMALL_IMMEDIATE)))
	{
		Value src = rot->getSource();
		//vector rotation -> rotate container (if static offset)
		std::size_t offset = rot->getOffset().hasType(ValueType::LITERAL) ? rot->getOffset().literal.integer : rot->getOffset().immediate.getRotationOffset().get();
		//"Rotates the order of the elements in the range [first,last), in such a way that the element pointed by middle becomes the new first element."
		offset = (16 - offset);
		//need to rotate all (possible non-existing) 16 elements, so use a temporary vector with 16 elements and rotate it
		std::vector<Value> tmp;
		tmp.reserve(16);
		for(const Value& e : src.container.elements)
			tmp.push_back(e);
		while(tmp.size() != 16)
			tmp.push_back(UNDEFINED_VALUE);
		std::rotate(tmp.begin(), tmp.begin() + offset, tmp.end());
		for(std::size_t i = 0; i < src.container.elements.size(); ++i)
		{
			src.container.elements[i] = tmp.at(i);
		}
		rot->setSource(src);
		//TODO next step could be optimized, if we used the vector-rotation to extract an element
		//In which case, a simple copy suffices?? At least, we don't need to set the other elements
	}
	//jump from previous block to next one intended, so no "else"
	if(move != nullptr && move->getSource().hasType(ValueType::CONTAINER))
	{
		if(!move->getSource().type.isPointerType())
		{
			logging::debug() << "Rewriting move from container " << move->to_string() << logging::endl;
			it = copyVector(method, it, move->getOutput(), move->getSource());
			it.erase();
			//don't skip next instruction
			it.previousInBlock();
		}
	}
	else if(op != nullptr && (op->getFirstArg().hasType(ValueType::CONTAINER) || (op->getSecondArg() && op->getSecondArg().get().hasType(ValueType::CONTAINER))))
	{
		if(op->getFirstArg().hasType(ValueType::CONTAINER) && !op->getFirstArg().type.isPointerType())
		{
			logging::debug() << "Rewriting operation with container-input " << op->to_string() << logging::endl;
			const Value tmpVal = method.addNewLocal(op->getOutput().get().type, "%container");
			it = copyVector(method, it, tmpVal, op->getFirstArg());
			op->setArgument(0, tmpVal);
			//don't skip next instruction
			it.previousInBlock();
		}
		if(op->getSecondArg() && op->getSecondArg().get().hasType(ValueType::CONTAINER) && !op->getSecondArg().get().type.isPointerType())
		{
			logging::debug() << "Rewriting operation with container-input " << op->to_string() << logging::endl;
			const Value tmpVal = method.addNewLocal(op->getOutput().get().type, "%container");
			it = copyVector(method, it, tmpVal, op->getSecondArg());
			op->setArgument(1, tmpVal);
			//don't skip next instruction
			it.previousInBlock();
		}
	}
	//TODO other operations too
	return it;
}

struct ImmediateHandler
{
    //if set to true, value was changed and any of the other values are valid
    bool changeValue = false;
    //if set to true, value must be loaded via a load-immediate instruction
    bool loadImmediate = false;
    //if set to SHL, the value is the immediate left-shifted by itself (y << y)
    //if set to ADD/FADD, the value is the immediate added with itself (y + y)
    //if set to MUL24/FMUL, value is the square (y * y) of the given immediate
    OpCode opCode = OP_NOP;
    //the immediate to load/use
    SmallImmediate immediate = SmallImmediate(0);
};

struct ImmediateSupplier
{
	const OpCode& opCode;
	const SmallImmediate immediate;

	//insert operation on one of the ALUs
	ImmediateSupplier(const OpCode& opCode, SmallImmediate imm) : opCode(opCode), immediate(imm) {}
	//no operation required, directly use small immediate value
	explicit ImmediateSupplier(SmallImmediate imm) : opCode(OP_NOP), immediate(imm) {}
};

static Literal toLiteral(uint32_t mask, float f)
{
	//TODO static_assert(bit_cast<double, int64_t>(d) == i, "Values do not match!");
	if(bit_cast<float, uint32_t>(f) != mask)
		throw CompilationError(CompilationStep::GENERAL, std::string("Small immediate values do not match for ") + std::to_string(mask) + " and", (std::to_string(f) + " aka ") + std::to_string(bit_cast<float, uint32_t>(f)));
	return Literal(static_cast<uint64_t>(mask));
}

static Literal toLiteral(uint32_t mask, int32_t j)
{
	if((static_cast<int64_t>(j) & 0xFFFFFFFF) != (static_cast<int64_t>(mask) & 0xFFFFFFFF))
		throw CompilationError(CompilationStep::GENERAL, std::string("Small immediate values do not match for ") + std::to_string(mask) + " and", std::to_string(j));
	return Literal(static_cast<uint64_t>(mask));
}

//source: http://maazl.de/project/vc4asm/doc/smallimmediate.html
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
		{toLiteral(0x00000020, 32), ImmediateSupplier(OP_CLZ, SmallImmediate(0))},
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

		{Literal(static_cast<int64_t>(0x001e0000)), ImmediateSupplier(OP_ROR, SmallImmediate(15))},
		{Literal(static_cast<int64_t>(0x00380000)), ImmediateSupplier(OP_ROR, SmallImmediate(14))},
		{Literal(static_cast<int64_t>(0x00680000)), ImmediateSupplier(OP_ROR, SmallImmediate(13))},
		{Literal(static_cast<int64_t>(0x00c00000)), ImmediateSupplier(OP_ROR, SmallImmediate(12))},
		{Literal(static_cast<int64_t>(0x01600000)), ImmediateSupplier(OP_ROR, SmallImmediate(11))},
		{Literal(static_cast<int64_t>(0x02800000)), ImmediateSupplier(OP_ROR, SmallImmediate(10))},
		{Literal(static_cast<int64_t>(0x04800000)), ImmediateSupplier(OP_ROR, SmallImmediate(9))},
		{Literal(static_cast<int64_t>(0x08000000)), ImmediateSupplier(OP_ROR, SmallImmediate(8))},
		{Literal(static_cast<int64_t>(0x0e000000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(41))},
		//{Literal(static_cast<int64_t>(0x0e000000)), ImmediateSupplier(OP_ROR, SmallImmediate(7))},
		{Literal(static_cast<int64_t>(0x0e400000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(40))},
		//{Literal(static_cast<int64_t>(0x0e400000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(42))},
		{Literal(static_cast<int64_t>(0x0f000000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(43))},
		//{Literal(static_cast<int64_t>(0x0f000000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(45))},
		{Literal(static_cast<int64_t>(0x0f400000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(44))},
		//{Literal(static_cast<int64_t>(0x0f400000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(46))},
		{Literal(static_cast<int64_t>(0x10000000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(47))},
		//{Literal(static_cast<int64_t>(0x10000000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(33))},
		{Literal(static_cast<int64_t>(0x10400000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(32))},
		//{Literal(static_cast<int64_t>(0x10400000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(34))},
		{Literal(static_cast<int64_t>(0x11000000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(35))},
		//{Literal(static_cast<int64_t>(0x11000000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(37))},
		{Literal(static_cast<int64_t>(0x11400000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(36))},
		//{Literal(static_cast<int64_t>(0x11400000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(38))},
		{Literal(static_cast<int64_t>(0x12000000)), ImmediateSupplier(OP_V8MULD, SmallImmediate(39))},
		{Literal(static_cast<int64_t>(0x18000000)), ImmediateSupplier(OP_ROR, SmallImmediate(6))},
		{Literal(static_cast<int64_t>(0x28000000)), ImmediateSupplier(OP_ROR, SmallImmediate(5))},

		{toLiteral(0x37800000, 1.0f / (256.0f * 256.0f)), ImmediateSupplier(OP_FMUL, SmallImmediate(40))},
		{toLiteral(0x38800000, 1.0f / (128.0f * 128.0f)), ImmediateSupplier(OP_FMUL, SmallImmediate(41))},
		{toLiteral(0x39800000, 1.0f / (64.0f * 64.0f)), ImmediateSupplier(OP_FMUL, SmallImmediate(42))},
		{toLiteral(0x3a800000, 1.0f / (32.0f * 32.0f)), ImmediateSupplier(OP_FMUL, SmallImmediate(43))},
		{toLiteral(0x3b800000, 1.0f/256.0f), ImmediateSupplier(SmallImmediate(40))},
		{toLiteral(0x3c000000, 1.0f/128.0f), ImmediateSupplier(SmallImmediate(41))},
		{toLiteral(0x3c800000, 1.0f/64.0f), ImmediateSupplier(SmallImmediate(42))},
		{toLiteral(0x3d000000, 1.0f/32.0f), ImmediateSupplier(SmallImmediate(43))},
		{toLiteral(0x3d800000, 1.0f/16.0f), ImmediateSupplier(SmallImmediate(44))},
		{toLiteral(0x3e000000, 1.0f/8.0f), ImmediateSupplier(SmallImmediate(45))},
		{toLiteral(0x3e800000, 1.0f/4.0f), ImmediateSupplier(SmallImmediate(46))},
		{toLiteral(0x3f000000, 1.0f/2.0f), ImmediateSupplier(SmallImmediate(47))},
		{toLiteral(0x3f800000, 1.0f), ImmediateSupplier(SmallImmediate(32))},
		{toLiteral(0x40000000, 2.0f), ImmediateSupplier(SmallImmediate(33))},
		//{Literal(static_cast<int64_t>(0x40000000)), ImmediateSupplier(OP_ROR, SmallImmediate(4))},
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

		{toLiteral(0x4e6e0000, static_cast<float>(bit_cast<float, int32_t>(1.0f/256.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(40))},
		{toLiteral(0x4e700000, static_cast<float>(bit_cast<float, int32_t>(1.0f/128.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(41))},
		{toLiteral(0x4e720000, static_cast<float>(bit_cast<float, int32_t>(1.0f/64.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(42))},
		{toLiteral(0x4e740000, static_cast<float>(bit_cast<float, int32_t>(1.0f/32.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(43))},
		{toLiteral(0x4e760000, static_cast<float>(bit_cast<float, int32_t>(1.0f/16.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(44))},
		{toLiteral(0x4e780000, static_cast<float>(bit_cast<float, int32_t>(1.0f/8.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(45))},
		{toLiteral(0x4e7a0000, static_cast<float>(bit_cast<float, int32_t>(1.0f/4.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(46))},
		{toLiteral(0x4e7c0000, static_cast<float>(bit_cast<float, int32_t>(1.0f/2.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(47))},
		{toLiteral(0x4e7e0000, static_cast<float>(bit_cast<float, int32_t>(1.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(32))},
		{toLiteral(0x4e800000, static_cast<float>(bit_cast<float, int32_t>(2.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(33))},
		{toLiteral(0x4e810000, static_cast<float>(bit_cast<float, int32_t>(4.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(34))},
		{toLiteral(0x4e820000, static_cast<float>(bit_cast<float, int32_t>(8.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(35))},
		{toLiteral(0x4e830000, static_cast<float>(bit_cast<float, int32_t>(16.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(36))},
		{toLiteral(0x4e840000, static_cast<float>(bit_cast<float, int32_t>(32.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(37))},
		{toLiteral(0x4e850000, static_cast<float>(bit_cast<float, int32_t>(64.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(38))},
		{toLiteral(0x4e860000, static_cast<float>(bit_cast<float, int32_t>(128.0f))), ImmediateSupplier(OP_ITOF, SmallImmediate(39))},

		{Literal(static_cast<int64_t>(0x60000000)), ImmediateSupplier(OP_ROR, SmallImmediate(3))},

		{Literal(static_cast<int64_t>(0x76ff0000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(40))},
		{toLiteral(0x77000000, std::pow(2.0f, 111.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(40))},
		{Literal(static_cast<int64_t>(0x78000000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(41))},
		//{toLiteral(0x78000000, std::pow(2.0f, 113.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(41))},
		{Literal(static_cast<int64_t>(0x78ff0000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(42))},
		{toLiteral(0x79000000, std::pow(2.0f, 115.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(42))},
		{Literal(static_cast<int64_t>(0x7a000000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(43))},
		//{toLiteral(0x7a000000, std::pow(2.0f, 117.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(43))},
		{Literal(static_cast<int64_t>(0x7aff0000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(44))},
		{toLiteral(0x7b000000, std::pow(2.0f, 119.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(44))},
		{Literal(static_cast<int64_t>(0x7c000000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(45))},
		//{toLiteral(0x7c000000, std::pow(2.0f, 121.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(45))},
		{Literal(static_cast<int64_t>(0x7cff0000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(46))},
		{toLiteral(0x7d000000, std::pow(2.0f, 123.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(46))},
		{Literal(static_cast<int64_t>(0x7e000000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(47))},
		//{toLiteral(0x7e000000, std::pow(2.0f, 125.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(47))},
		{Literal(static_cast<int64_t>(0x7eff0000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(32))},
		{toLiteral(0x7f000000, std::pow(2.0f, 127.0f)), ImmediateSupplier(OP_ADD, SmallImmediate(32))},
		{Literal(static_cast<int64_t>(0x80000000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(33))},
		//-2^-126 (a denormal number), pow results in -0.0
		//{Literal(static_cast<int64_t>(0x80000000)), ImmediateSupplier(OP_ADD, SmallImmediate(33))},
		{Literal(static_cast<int64_t>(0x80ff0000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(34))},
		//-2^-125, pow results in -0.0
		{Literal(static_cast<int64_t>(0x81000000)), ImmediateSupplier(OP_ADD, SmallImmediate(34))},
		{Literal(static_cast<int64_t>(0x82000000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(35))},
		//-2^-123, pow results in -0.0
		//{Literal(static_cast<int64_t>(0x82000000)), ImmediateSupplier(OP_ADD, SmallImmediate(35))},
		{Literal(static_cast<int64_t>(0x82ff0000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(36))},
		//-2^-121, pow results in -0.0
		{Literal(static_cast<int64_t>(0x83000000)), ImmediateSupplier(OP_ADD, SmallImmediate(36))},
		{Literal(static_cast<int64_t>(0x84000000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(37))},
		//-2^-119, pow results in -0.0
		//{Literal(static_cast<int64_t>(0x84000000)), ImmediateSupplier(OP_ADD, SmallImmediate(37))},
		{Literal(static_cast<int64_t>(0x84ff0000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(38))},
		//-2^-117, pow results in -0.0
		{Literal(static_cast<int64_t>(0x85000000)), ImmediateSupplier(OP_ADD, SmallImmediate(38))},
		{Literal(static_cast<int64_t>(0x86000000)), ImmediateSupplier(OP_V8ADDS, SmallImmediate(39))},
		//-2^-115, pow results in -0.0
		//{Literal(static_cast<int64_t>(0x86000000)), ImmediateSupplier(OP_ADD, SmallImmediate(39))},

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

		{toLiteral(0xe0000100, static_cast<int32_t>(static_cast<int64_t>(-16 & 0xFFFFFF) * static_cast<int64_t>(-16 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(16))},
		{toLiteral(0xe20000e1, static_cast<int32_t>(static_cast<int64_t>(-15 & 0xFFFFFF) * static_cast<int64_t>(-15 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(17))},
		{toLiteral(0xe40000c4, static_cast<int32_t>(static_cast<int64_t>(-14 & 0xFFFFFF) * static_cast<int64_t>(-14 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(18))},
		{toLiteral(0xe60000a9, static_cast<int32_t>(static_cast<int64_t>(-13 & 0xFFFFFF) * static_cast<int64_t>(-13 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(18))},
		{toLiteral(0xe8000090, static_cast<int32_t>(static_cast<int64_t>(-12 & 0xFFFFFF) * static_cast<int64_t>(-12 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(20))},
		{toLiteral(0xea000079, static_cast<int32_t>(static_cast<int64_t>(-11 & 0xFFFFFF) * static_cast<int64_t>(-11 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(21))},
		{toLiteral(0xec000064, static_cast<int32_t>(static_cast<int64_t>(-10 & 0xFFFFFF) * static_cast<int64_t>(-10 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(22))},
		{toLiteral(0xee000051, static_cast<int32_t>(static_cast<int64_t>(-9 & 0xFFFFFF) * static_cast<int64_t>(-9 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(23))},
		{toLiteral(0xf0000040, static_cast<int32_t>(static_cast<int64_t>(-8 & 0xFFFFFF) * static_cast<int64_t>(-8 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(24))},
		{toLiteral(0xf2000031, static_cast<int32_t>(static_cast<int64_t>(-7 & 0xFFFFFF) * static_cast<int64_t>(-7 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(25))},
		{toLiteral(0xf4000024, static_cast<int32_t>(static_cast<int64_t>(-6 & 0xFFFFFF) * static_cast<int64_t>(-6 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(26))},
		{toLiteral(0xf6000019, static_cast<int32_t>(static_cast<int64_t>(-5 & 0xFFFFFF) * static_cast<int64_t>(-5 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(27))},
		{toLiteral(0xf8000010, static_cast<int32_t>(static_cast<int64_t>(-4 & 0xFFFFFF) * static_cast<int64_t>(-4 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(28))},
		{toLiteral(0xfa000009, static_cast<int32_t>(static_cast<int64_t>(-3 & 0xFFFFFF) * static_cast<int64_t>(-3 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(29))},
		{toLiteral(0xfc000004, static_cast<int32_t>(static_cast<int64_t>(-2 & 0xFFFFFF) * static_cast<int64_t>(-2 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(30))},
		{toLiteral(0xfe000001, static_cast<int32_t>(static_cast<int64_t>(-1 & 0xFFFFFF) * static_cast<int64_t>(-1 & 0xFFFFFF))), ImmediateSupplier(OP_MUL24, SmallImmediate(31))},

		{Literal(static_cast<int64_t>(0xfff0ffff)), ImmediateSupplier(OP_ROR, SmallImmediate(16))},
		{Literal(static_cast<int64_t>(0xfff8ffff)), ImmediateSupplier(OP_ROR, SmallImmediate(17))},
		{Literal(static_cast<int64_t>(0xfffcbfff)), ImmediateSupplier(OP_ROR, SmallImmediate(18))},
		{Literal(static_cast<int64_t>(0xfffe7fff)), ImmediateSupplier(OP_ROR, SmallImmediate(19))},
		{Literal(static_cast<int64_t>(0xffff4fff)), ImmediateSupplier(OP_ROR, SmallImmediate(20))},
		{Literal(static_cast<int64_t>(0xffffafff)), ImmediateSupplier(OP_ROR, SmallImmediate(21))},
		{Literal(static_cast<int64_t>(0xffffdbff)), ImmediateSupplier(OP_ROR, SmallImmediate(22))},
		{Literal(static_cast<int64_t>(0xffffefff)), ImmediateSupplier(OP_ROR, SmallImmediate(23))},
		{Literal(static_cast<int64_t>(0xfffff8ff)), ImmediateSupplier(OP_ROR, SmallImmediate(24))},
		{Literal(static_cast<int64_t>(0xfffffcff)), ImmediateSupplier(OP_ROR, SmallImmediate(25))},
		{Literal(static_cast<int64_t>(0xfffffebf)), ImmediateSupplier(OP_ROR, SmallImmediate(26))},

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
 * - is slightly faster than old version (complete SingleSteps slower than before, probably due to reversion via precalculate)
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

	Literal index(false);
	if(source.type == LiteralType::REAL)
		index = Literal(static_cast<int64_t>(bit_cast<float, int32_t>(static_cast<float>(source.real()))) & 0xFFFFFFFF);
	else
		index = Literal(source.integer & 0xFFFFFFFF);
	auto it = immediateMappings.find(index);
	if(it != immediateMappings.end())
	{
		handler.immediate = it->second.immediate;
		if(it->second.opCode == OP_NOP)
			//directly use value
			return handler;
		else
			handler.opCode = it->second.opCode;
		return handler;
	}
	handler.loadImmediate = true;
	return handler;
}

InstructionWalker optimizations::handleImmediate(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
	intermediate::MoveOperation* move = it.get<intermediate::MoveOperation>();
	if(move != nullptr)
	{
		Value source = move->getSource();
		if(source.hasType(ValueType::LITERAL))
		{
			PROFILE_START(mapImmediateValue);
			ImmediateHandler mapped = mapImmediateValue(source.literal);
			PROFILE_END(mapImmediateValue);
			if(mapped.changeValue)
			{
				//value was changed
				if(mapped.loadImmediate)
				{
					//requires load immediate
					logging::debug() << "Loading immediate value: " << source.literal.to_string() << logging::endl;
					it.reset((new intermediate::LoadImmediate(move->getOutput(), source.literal))->copyExtrasFrom(move));
				}
				else if(mapped.opCode != OP_NOP)
				{
					if(mapped.opCode.numOperands == 1)
						it.reset((new intermediate::Operation(mapped.opCode, move->getOutput(), Value(mapped.immediate, move->getSource().type)))->copyExtrasFrom(move));
					else
						it.reset((new intermediate::Operation(mapped.opCode, move->getOutput(), Value(mapped.immediate, move->getSource().type), Value(mapped.immediate, move->getSource().type)))->copyExtrasFrom(move));
				}
				else
				{
					logging::debug() << "Mapping constant for immediate value " << source.literal.to_string() << " to: " << mapped.immediate.toString() << logging::endl;
					move->setSource(Value(mapped.immediate, source.type));
				}
			}
		}
	}
	intermediate::Operation* op = it.get<intermediate::Operation>();
	if(op != nullptr)
	{
		//check for both arguments
		Value source = op->getFirstArg();
		if(source.hasType(ValueType::LITERAL))
		{
			PROFILE_START(mapImmediateValue);
			ImmediateHandler mapped = mapImmediateValue(source.literal);
			PROFILE_END(mapImmediateValue);
			if(mapped.changeValue)
			{
				const Value tmp = method.addNewLocal(source.type, "%immediate");

				//value was changed
				if(mapped.loadImmediate)
				{
					//requires load immediate
					logging::debug() << "Loading immediate value: " << source.literal.to_string() << logging::endl;
					it.emplace(new intermediate::LoadImmediate(tmp, source.literal, op->conditional));
					it.nextInBlock();
					op->setArgument(0, tmp);
				}
				else if(mapped.opCode != OP_NOP)
				{
					DataType type = mapped.immediate.getFloatingValue().hasValue ? TYPE_FLOAT : TYPE_INT32;
					if(mapped.opCode.numOperands == 1)
						it.emplace(new intermediate::Operation(mapped.opCode, tmp, Value(mapped.immediate, type), op->conditional));
					else
						it.emplace(new intermediate::Operation(mapped.opCode, tmp, Value(mapped.immediate, type), Value(mapped.immediate, type), op->conditional));
					it.nextInBlock();
					op->setArgument(0, tmp);
				}
				else
				{
					logging::debug() << "Mapping constant for immediate value " << source.literal.to_string() << " to: " << mapped.immediate.toString() << logging::endl;
					op->setArgument(0, Value(mapped.immediate, source.type));
				}
			}
		}
		if(op->getSecondArg())
		{
			source = op->getSecondArg();
			if(source.hasType(ValueType::LITERAL))
			{
				PROFILE_START(mapImmediateValue);
				ImmediateHandler mapped = mapImmediateValue(source.literal);
				PROFILE_END(mapImmediateValue);
				if(mapped.changeValue)
				{
					Value tmp = method.addNewLocal(source.type, "%immediate");
					//value was changed
					if(mapped.loadImmediate)
					{
						//requires load immediate
						logging::debug() << "Loading immediate value: " << source.literal.to_string() << logging::endl;
						it.emplace(new intermediate::LoadImmediate(tmp, source.literal, op->conditional));
						it.nextInBlock();
						op->setArgument(1, tmp);
					}
					else if(mapped.opCode != OP_NOP)
					{
						DataType type = mapped.immediate.getFloatingValue().hasValue ? TYPE_FLOAT : TYPE_INT32;
						if(mapped.opCode.numOperands == 1)
							it.emplace(new intermediate::Operation(mapped.opCode, tmp, Value(mapped.immediate, type), op->conditional));
						else
							it.emplace(new intermediate::Operation(mapped.opCode, tmp, Value(mapped.immediate, type), Value(mapped.immediate, type), op->conditional));
						it.nextInBlock();
						op->setArgument(1, tmp);
					}
					else
					{
						logging::debug() << "Mapping constant for immediate value " << source.literal.to_string() << " to: " << mapped.immediate.toString() << logging::endl;
						op->setArgument(1, Value(mapped.immediate, source.type));
					}
				}
			}
		}
	}
	return it;
}

static InstructionWalker findWriteOfLocal(InstructionWalker it, const Local* loc)
{
	//TODO could already abort after X steps (X being the accumulator threshold)
	while(!it.isStartOfBlock() && !(it->hasValueType(ValueType::LOCAL) && it->getOutput().get().hasLocal(loc)))
	{
		it.previousInBlock();
	}
	return it;
}

InstructionWalker optimizations::handleUseWithImmediate(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
	//- for all locals used together with small immediate values
	//- if the range of the local exceeds the accumulator threshold
	//-> copy the local before the use into a new temporary, which is used with the immediate instead
	//-> fixes blocked register-file B for long-living locals
	intermediate::Operation* op = it.get<intermediate::Operation>();
	if(op != nullptr)
	{
		const auto& args = op->getArguments();
		if(std::any_of(args.begin(), args.end(), [](const Value& arg) -> bool { return arg.hasType(ValueType::LITERAL) || arg.hasType(ValueType::SMALL_IMMEDIATE);}))
		{
			//at least one immediate value is used
			const auto localIt = std::find_if(args.begin(), args.end(), [](const Value& arg) -> bool {return arg.hasType(ValueType::LOCAL);});
			if(localIt != args.end() && !it.getBasicBlock()->isLocallyLimited(findWriteOfLocal(it, localIt->local), localIt->local))
			{
				//one other local is used and its range is greater than the accumulator threshold
				logging::debug() << "Inserting temporary to split up use of long-living local with immediate value: " << op->to_string() << logging::endl;
				const Value tmp = method.addNewLocal(localIt->type, "%use_with_literal");
				it.emplace(new intermediate::MoveOperation(tmp, *localIt));
				it.nextInBlock();
				const Local* oldLocal = localIt->local;
				op->replaceLocal(oldLocal, tmp.local, LocalUser::Type::READER);
			}
		}
	}

	return it;
}
