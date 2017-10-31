/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LiteralValues.h"
#include "log.h"
#include "../Profiler.h"
#include "../InstructionWalker.h"

#include <cmath>
#include <algorithm>

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
    	//copy first element without test for flags, so the register allocator finds an unconditional write ot the container
        //1) set flags for element i
    	if(i > 0)
    	{
			it.emplace(new intermediate::Operation("xor", NOP_REGISTER, ELEMENT_NUMBER_REGISTER, Value(SmallImmediate(i), TYPE_INT8), COND_ALWAYS, SetFlag::SET_FLAGS));
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
	if(rot != nullptr && rot->getSource().hasType(ValueType::CONTAINER) && rot->getOffset().hasType(ValueType::LITERAL))
	{
		Value src = rot->getSource();
		//vector rotation -> rotate container (if static offset)
		std::rotate(src.container.elements.begin(), src.container.elements.end(), src.container.elements.begin() + rot->getOffset().literal.integer);
		rot->setSource(src);
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
    const OpAdd* opAdd = &OPADD_NOP;
    //if set to MUL24/FMUL, value is the square (y * y) of the given immediate
    const OpMul* opMul = &OPMUL_NOP;
    //the immediate to load/use
    SmallImmediate immediate = SmallImmediate(0);
};

static bool contains(const std::vector<float>& container, const float val)
{
	for(const float v : container)
	{
		if(val == v)
			return true;
	}
	return false;
}

static int index(const std::vector<float>& container, const float val)
{
	for(std::size_t i = 0; i < container.size(); ++i)
	{
		if(val == container[i])
			return i;
	}
	return -1;
}

static ImmediateHandler mapImmediateValue(const Literal& source)
{
    static const std::vector<int> intAdds = {0 +0, 1+1, 2+2, 3+3, 4+4, 5+5, 6+6, 7+7, 8+8, 9+9, 10+10, 11+11, 12+12, 13+13, 14+14, 15+15, -16-16, -15-15, -14-14, -13-13, -12-12, -11-11, -10-10, -9-9, -8-8, -7-7, -6-6, -5-5, -4-4, -3-3, -2-2, -1-1};
    static const std::vector<int> shifts = {0 << 0, 1 << 1, 2 << 2, 3 << 3, 4 << 4, 5 << 5, 6 << 6, 7 << 7, 8 << 8, 9 << 9, 10 << 10, 11 << 11, 12 << 12, 13 << 13, 14 << 14, 15 << 15};
    static const std::vector<float> powersOf2 = {1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0, 128.0};
    static const std::vector<float> partsOf2 = {1.0/256.0, 1.0/128.0, 1.0/64.0, 1.0/32.0, 1.0/16.0, 1.0/8.0, 1.0/4.0, 1.0/2.0};
    ImmediateHandler handler;
    switch(source.type)
    {
    case LiteralType::BOOL:
        //no need to create a load-instruction
    	handler.changeValue = true;
    	handler.immediate.value = source.isTrue();
        return handler;
    case LiteralType::INTEGER:
    {
    	//to convert to real negative values
    	const int32_t integer = static_cast<int32_t>(source.integer);
        //ALU with immediate supports -16 <= x <= 15
        if(integer >= -16 && integer <= 15)
        {
            if(integer < 0)
                //-16 <= x < 0
                //need to move values, because bit-value 16 means -16, 17 -> -15, ..., 31 -> -1
                handler.immediate.value = 32 + integer /* + since integer is negative */;
            else    //0 <= x <= 15 -> keep as is
            	handler.immediate.value = integer;
            handler.changeValue = true;
            return handler;
        }
        // integer value x < -16 || x > 15
        else
        {
        	//FIXME sanitizer fails for -1 * -1 (aka. -9223372036854775808 * -9223372036854775808) not fitting into long (./example/SHA-256.cl)
            long root = static_cast<long>(sqrt(integer));
            if(root * root == integer && root >= 0 && root <= 15 )
            {
                handler.changeValue = true;
                handler.immediate.value = root;
                handler.opMul = &OPMUL_MUL24;
                return handler;
            }
        }
        for(std::size_t i = 0; i < intAdds.size(); ++i)
        {
            if(intAdds[i] == integer)
            {
                handler.changeValue = true;
                handler.immediate.value = i;
                handler.opAdd = &OPADD_ADD;
                return handler;
            }
        }
        for(std::size_t i = 0; i < shifts.size(); ++i)
        {
            if(shifts[i] == integer)
            {
                handler.changeValue = true;
                handler.immediate.value = i;
                handler.opAdd = &OPADD_SHL;
                return handler;
            }
        }
        //need load immediate instruction
        handler.changeValue = true;
        handler.loadImmediate = true;
        return handler;
    }
    case LiteralType::REAL:
        //handle values representable with integer small immediates
        const int integer = bit_cast<float, int>(static_cast<float>(source.real()));
        if(integer >= 0 && integer <= 15)
        {
            //0 <= x <= 15 -> keep as is
        	handler.changeValue = true;
        	handler.immediate.value = integer;
            return handler;
        }
        //ALU with immediate supports
        //1.0, 2.0, 4.0, ..., 128.0
        //1/256, 1/128, ..., 1/2
        int powerOfTwo = -1;
        int partOfTwo = -1;
        for(std::size_t i = 0; i < powersOf2.size(); ++i)
        {
            if(static_cast<float>(source.real()) == powersOf2[i])
            {
                powerOfTwo = i;
                break;
            }
        }
        for(std::size_t i = 0; i < partsOf2.size(); ++i)
        {
            if(static_cast<float>(source.real()) == partsOf2[i])
            {
                partOfTwo = i;
                break;
            }
        }
        if(powerOfTwo != -1)
        {
            handler.changeValue = true;
            handler.immediate.value = 32 + powerOfTwo;
            return handler;
        }
        if(partOfTwo != -1)
        {
            handler.changeValue = true;
            handler.immediate.value = 40 + partOfTwo;
            return handler;
        }
		float root = static_cast<float>(sqrt(source.real()));
		if(static_cast<float>(root * root) == source.real() && (contains(powersOf2, root) || contains(partsOf2, root)))
		{
			//need to map to the constant representing the power/part of two, not the value itself
			int i = index(powersOf2, root);
			if(i >= 0)
				i += 32;
			if(i < 0)
				i = 40 + index(partsOf2, root);
			handler.changeValue = true;
			handler.immediate.value = i;
			handler.opMul = &OPMUL_FMUL;
			return handler;
		}
        //any other float, load-immediate bit-casted integer
        handler.changeValue = true;
        handler.loadImmediate = true;
        return handler;
    }
    throw CompilationError(CompilationStep::OPTIMIZER, "Unknown literal-type", source.to_string());
}

InstructionWalker optimizations::handleImmediate(const Module& module, Method& method, InstructionWalker it, const Configuration& config)
{
	//TODO see http://maazl.de/project/vc4asm/doc/instructions.html#mov
	//http://maazl.de/project/vc4asm/doc/smallimmediate.html
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
				else if(*mapped.opAdd != OPADD_NOP)
				{
					it.reset((new intermediate::Operation(mapped.opAdd->name, move->getOutput(), Value(mapped.immediate, move->getSource().type), Value(mapped.immediate, move->getSource().type)))->copyExtrasFrom(move));
				}
				else if(*mapped.opMul != OPMUL_NOP)
				{
					it.reset((new intermediate::Operation(mapped.opMul->name, move->getOutput(), Value(mapped.immediate, move->getSource().type), Value(mapped.immediate, move->getSource().type)))->copyExtrasFrom(move));
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
				else if(*mapped.opAdd != OPADD_NOP)
				{
					DataType type = mapped.immediate.getFloatingValue().hasValue ? TYPE_FLOAT : TYPE_INT32;
					it.emplace(new intermediate::Operation(mapped.opAdd->name, tmp, Value(mapped.immediate, type), Value(mapped.immediate, type), op->conditional));
					it.nextInBlock();
					op->setArgument(0, tmp);
				}
				else if(*mapped.opMul != OPMUL_NOP)
				{
					DataType type = mapped.immediate.getFloatingValue().hasValue ? TYPE_FLOAT : TYPE_INT32;
					it.emplace(new intermediate::Operation(mapped.opMul->name, tmp, Value(mapped.immediate, type), Value(mapped.immediate, type), op->conditional));
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
					else if(*mapped.opAdd != OPADD_NOP)
					{
						DataType type = mapped.immediate.getFloatingValue().hasValue ? TYPE_FLOAT : TYPE_INT32;
						it.emplace(new intermediate::Operation(mapped.opAdd->name, tmp, Value(mapped.immediate, type), Value(mapped.immediate, type), op->conditional));
						it.nextInBlock();
						op->setArgument(1, tmp);
					}
					else if(*mapped.opMul != OPMUL_NOP)
					{
						DataType type = mapped.immediate.getFloatingValue().hasValue ? TYPE_FLOAT : TYPE_INT32;
						it.emplace(new intermediate::Operation(mapped.opMul->name, tmp, Value(mapped.immediate, type), Value(mapped.immediate, type), op->conditional));
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
