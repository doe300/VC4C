/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "LLVMInstruction.h"

#include "../intermediate/Helper.h"
#include "../intermediate/IntermediateInstruction.h"
#include "../intermediate/TypeConversions.h"
#include "../intermediate/VectorHelper.h"
#include "config.h"
#include "log.h"

#include <algorithm>
#include <regex>

using namespace vc4c;
using namespace vc4c::llvm2qasm;

LLVMInstruction::LLVMInstruction() : decorations(intermediate::InstructionDecorations::NONE) {}

LLVMInstruction::~LLVMInstruction() noexcept = default;

LLVMInstruction* LLVMInstruction::setDecorations(const intermediate::InstructionDecorations decorations)
{
    this->decorations = decorations;
    return this;
}

CallSite::CallSite(Value&& dest, std::string&& methodName, std::vector<Value>&& args) :
    dest(dest), methodName(methodName), arguments(args)
{
}

CallSite::CallSite(Value&& dest, const Method& method, std::vector<Value>&& args, bool isVarArg) :
    dest(dest), methodName(method.name), arguments(args)
{
    if(!isVarArg && method.parameters.size() != args.size())
    {
        throw CompilationError(CompilationStep::PARSER, "Invalid numbers of method arguments",
            std::string("Got ") + (std::to_string(args.size()) + ", expected ") +
                std::to_string(method.parameters.size()));
    }
}

bool CallSite::mapInstruction(Method& method)
{
    // map calls to @llvm.lifetime.start / @llvm.lifetime.end to lifetime-instructions
    if(methodName.find("llvm.lifetime.start") == 0 || methodName.find("llvm.lifetime.end") == 0)
    {
        Value pointer = arguments.at(1);
        if(!pointer.local()->is<StackAllocation>())
        {
            if(auto move = dynamic_cast<const intermediate::MoveOperation*>(pointer.getSingleWriter()))
                // the source of the life-time intrinsic could be bit-cast from an alloca-instruction
                pointer = move->getSource();
            else if(auto alloc = pointer.local()->getBase(true)->as<StackAllocation>())
                // it also could be a getelementptr (to the index 0)
                pointer = alloc->createReference();
        }
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Converting life-time instrinsic to life-time instruction" << logging::endl);
        if(arguments.at(0).getLiteralValue() && arguments.at(0).getLiteralValue()->signedInt() > 0)
        {
            //"The first argument is a constant integer representing the size of the object, or -1 if it is variable
            // sized"
            StackAllocation* alloc = pointer.local()->as<StackAllocation>();
            if(alloc == nullptr)
                throw CompilationError(CompilationStep::LLVM_2_IR,
                    "Cannot start life-time of object not located on stack", pointer.to_string());
        }
        //"The second argument is a pointer to the object."
        method.appendToEnd(new intermediate::LifetimeBoundary(pointer, methodName.find("llvm.lifetime.end") == 0));
        return true;
    }
    Value output = dest.checkLocal() ? dest : NOP_REGISTER;
    // handle other llvm.* intrinsics
    if(methodName.find("llvm.fmuladd") == 0)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Converting intrinsic method call '" << methodName << "' to operations" << logging::endl);
        const Value tmp = method.addNewLocal(dest.type, "%fmuladd");
        method.appendToEnd(new intermediate::Operation(OP_FMUL, tmp, arguments.at(0), arguments.at(1)));
        method.appendToEnd(new intermediate::Operation(OP_FADD, output, tmp, arguments.at(2)));
        return true;
    }
    if(methodName.find("llvm.memcpy") == 0)
    {
        //@llvm.memcpy.p0i8.p0i8.i32(i8* <dest>, i8* <src>, i32 <len>, i32 <align>, i1 <isvolatile>)
        /*
         * For later LLVM versions (7.0+), this syntax changes, see
         * https://releases.llvm.org/7.0.0/docs/ReleaseNotes.html#changes-to-the-llvm-ir
         * declare void @llvm.memcpy.p0i8.p0i8.i32(i8* <dest>, i8* <src>, i32 <len>, i1 <isvolatile>)
         */
        // the type of llvm.memcpy is always i8*, so the number of bytes (<len>) always matches the number of entries
        // (as expected for MemoryInstruction())
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying llvm.memcpy function-call" << logging::endl);
        method.appendToEnd(new intermediate::MemoryInstruction(intermediate::MemoryOperation::COPY,
            std::move(arguments.at(0)), std::move(arguments.at(1)), std::move(arguments.at(2))));
        return true;
    }
    if(methodName.find("llvm.memset") == 0)
    {
        // declare void @llvm.memset.p0i8.i32|i.64(i8* <dest>, i8 <val>, i32|i64 <len>, i32 <align>, i1 <isvolatile>)
        /*
         * For later LLVM versions(7.0+), this syntax changes, see
         * https://releases.llvm.org/7.0.0/docs/ReleaseNotes.html#changes-to-the-llvm-ir
         * declare void @llvm.memset.p0i8.i32|i64(i8* <dest>, i8 <val>, i32 <len>, i1 <isvolatile>)
         */
        // the type of llvm.memset is always i8*, so the number of bytes (<len>) always matches the number of entries
        // (as expected for MemoryInstruction())
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Intrinsifying llvm.memset with DMA writes" << logging::endl);
        Value& memAddr = arguments.at(0);
        Value& fillByte = arguments.at(1);
        Value& numBytes = arguments.at(2);
        const Value& volatileAccess = arguments.back();
        if(volatileAccess.getLiteralValue() && volatileAccess.getLiteralValue()->isTrue() && memAddr.checkLocal() &&
            memAddr.local()->getBase(true)->is<Parameter>())
        {
            // set parameter to volatile
            const_cast<Local*>(memAddr.local()->getBase(true))->as<Parameter>()->decorations =
                add_flag(memAddr.local()->getBase(true)->as<Parameter>()->decorations, ParameterDecorations::VOLATILE);
        }
        method.appendToEnd(new intermediate::MemoryInstruction(
            intermediate::MemoryOperation::FILL, std::move(memAddr), std::move(fillByte), std::move(numBytes)));
        return true;
    }
    if(methodName.find("llvm.bswap") == 0)
    {
        /*
         * declare i16 @llvm.bswap.i16(i16 <id>)
         * declare i32 @llvm.bswap.i32(i32 <id>)
         */
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Intrinsifying llvm.bswap with manual byte-swapping" << logging::endl);
        ignoreReturnValue(intermediate::insertByteSwap(method.appendToEnd(), method, arguments.at(0), output));
        return true;
    }
    if(methodName.find("llvm.fshl") == 0)
    {
        /*
         * Funnel shift:
         * declare i8  @llvm.fshl.i8 (i8 %a, i8 %b, i8 %c)
         * declare <2 x i32> @llvm.fshl.v2i32(<2 x i32> %a, <2 x i32> %b, <2 x i32> %c)
         *
         * Concatenates the first two values, shifts left by the third value and extracts the most significant bits to
         * match the output (=input) type. If the first two operands are the same, this is a left rotation.
         *
         * => ((((a << sizeof(type)) | (b & mask(type))) << (c % sizeof(type)) >> sizeof(type)) & mask(type)
         */
        if(arguments.at(0) == arguments.at(1) && arguments[0].type.getScalarBitCount() == 32)
        {
            // funnel-shift left with identical operands is rotation left
            // rotation left by X is rotation right by sizeof(type)-X
            // NOTE: Since we only support full 32-bit rotation, we only apply this for 32-bit types!
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying llvm.fshl with identical operands with rotate right" << logging::endl);
            auto truncatedOffset = method.addNewLocal(arguments.at(2).type, "%fshl.offset");
            auto offset = method.addNewLocal(arguments.at(2).type, "%fshl.offset");
            method.appendToEnd(new intermediate::Operation(OP_AND, truncatedOffset, arguments[2],
                Value(Literal(arguments.at(2).type.getScalarBitCount() - 1u), TYPE_INT8)));
            method.appendToEnd(
                new intermediate::Operation(OP_SUB, offset, Value(Literal(32u), TYPE_INT8), truncatedOffset));
            method.appendToEnd(new intermediate::Operation(OP_ROR, output, arguments[0], offset));
            return true;
        }
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Intrinsifying llvm.fshl with manual funnel-shifting" << logging::endl);
        auto upper = method.addNewLocal(arguments.at(0).type.getUnionType(TYPE_INT32), "%fshl.upper");
        auto lower = method.addNewLocal(arguments.at(1).type, "%fshl.lower");
        auto offset = method.addNewLocal(arguments.at(2).type, "%fshl.offset");
        method.appendToEnd(new intermediate::Operation(
            OP_SHL, upper, arguments.at(0), Value(Literal(arguments.at(0).type.getScalarBitCount()), TYPE_INT8)));
        method.appendToEnd(new intermediate::Operation(
            OP_AND, lower, arguments.at(1), Value(Literal(arguments.at(1).type.getScalarWidthMask()), TYPE_INT32)));
        method.appendToEnd(new intermediate::Operation(
            OP_AND, offset, arguments.at(2), Value(Literal(arguments.at(2).type.getScalarBitCount() - 1u), TYPE_INT8)));

        auto combined = method.addNewLocal(upper.type, "%fshl.combined");
        method.appendToEnd(new intermediate::Operation(OP_OR, combined, upper, lower));
        auto shifted = method.addNewLocal(upper.type, "%fshl.shifted");
        method.appendToEnd(new intermediate::Operation(OP_SHL, shifted, combined, offset));
        upper = method.addNewLocal(shifted.type, "%fshl.upper");
        method.appendToEnd(new intermediate::Operation(
            OP_SHR, upper, shifted, Value(Literal(arguments.at(0).type.getScalarBitCount()), TYPE_INT8)));
        method.appendToEnd(new intermediate::Operation(
            OP_AND, output, upper, Value(Literal(arguments.at(0).type.getScalarWidthMask()), TYPE_INT32)));
        return true;
    }
    if(methodName.find("llvm.fshr") == 0)
    {
        /*
         * Funnel shift:
         * declare i8  @llvm.fshr.i8 (i8 %a, i8 %b, i8 %c)
         * declare <2 x i32> @llvm.fshr.v2i32(<2 x i32> %a, <2 x i32> %b, <2 x i32> %c)
         *
         * Concatenates the first two values, shifts right by the third value and extracts the least significant bits to
         * match the output (=input) type. If the first two operands are the same, this is a right rotation.
         *
         * => ((((a << sizeof(type)) | (b & mask(type))) >> (c % sizeof(type))) & mask(type)
         */
        if(arguments.at(0) == arguments.at(1) && arguments[0].type.getScalarBitCount() == 32)
        {
            // funnel-shift right with identical operands is rotation right
            // NOTE: Since we only support full 32-bit rotation, we only apply this for 32-bit types!
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Intrinsifying llvm.fshr with identical operands with rotate right" << logging::endl);
            method.appendToEnd(new intermediate::Operation(OP_ROR, output, arguments[0], arguments.at(2)));
            return true;
        }
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Intrinsifying llvm.fshr with manual funnel-shifting" << logging::endl);
        auto upper = method.addNewLocal(arguments.at(0).type.getUnionType(TYPE_INT32), "%fshr.upper");
        auto lower = method.addNewLocal(arguments.at(1).type, "%fshr.lower");
        auto offset = method.addNewLocal(arguments.at(2).type, "%fshr.offset");
        method.appendToEnd(new intermediate::Operation(
            OP_SHL, upper, arguments.at(0), Value(Literal(arguments.at(0).type.getScalarBitCount()), TYPE_INT8)));
        method.appendToEnd(new intermediate::Operation(
            OP_AND, lower, arguments.at(1), Value(Literal(arguments.at(1).type.getScalarWidthMask()), TYPE_INT32)));
        method.appendToEnd(new intermediate::Operation(
            OP_AND, offset, arguments.at(2), Value(Literal(arguments.at(2).type.getScalarBitCount() - 1u), TYPE_INT8)));

        auto combined = method.addNewLocal(upper.type, "%fshr.combined");
        method.appendToEnd(new intermediate::Operation(OP_OR, combined, upper, lower));
        auto shifted = method.addNewLocal(upper.type, "%fshr.shifted");
        method.appendToEnd(new intermediate::Operation(OP_SHR, shifted, combined, offset));
        method.appendToEnd(new intermediate::Operation(
            OP_AND, output, shifted, Value(Literal(arguments.at(0).type.getScalarWidthMask()), TYPE_INT32)));
        return true;
    }
    if(methodName.find("llvm.sadd.sat") == 0)
    {
        if(arguments.at(0).type.getScalarBitCount() == 32)
        {
            // 32-bit signed add_sat intrinsic function, map to same implementation as in _integer.h std-lib header
            method.appendToEnd((new intermediate::MethodCall(
                                    std::move(output), "vc4cl_saturated_add", {arguments.at(0), arguments.at(1)}))
                                   ->addDecorations(decorations));
            return true;
        }
    }
    if(methodName.find("llvm.uadd.sat") == 0)
    {
        // 16-/32-bit unsigned add_sat intrinsic function, map to same implementation as in _integer.h std-lib header:
        // x > ((result_t)UINT_MAX) - y ? UINT_MAX : x + y
        auto tmp = method.addNewLocal(arguments.at(1).type);
        method.appendToEnd(new intermediate::Operation(OP_SUB, Value(tmp), INT_MINUS_ONE, Value(arguments.at(1))));
        auto cond = method.addNewLocal(TYPE_BOOL.toVectorType(output.type.getVectorWidth()));
        method.appendToEnd(new intermediate::Comparison(
            intermediate::COMP_UNSIGNED_GT, Value(cond), Value(arguments.at(0)), std::move(tmp)));
        method.appendToEnd(new intermediate::MoveOperation(NOP_REGISTER, cond, COND_ALWAYS, SetFlag::SET_FLAGS));
        method.appendToEnd((new intermediate::MoveOperation(Value(dest), INT_MINUS_ONE, COND_ZERO_CLEAR))
                               ->addDecorations(decorations));
        method.appendToEnd((new intermediate::Operation(
                                OP_ADD, std::move(dest), Value(arguments.at(0)), Value(arguments.at(1)), COND_ZERO_SET))
                               ->addDecorations(decorations));
        return true;
    }
    if(methodName.find("llvm.ssub.sat") == 0)
    {
        if(arguments.at(0).type.getScalarBitCount() == 32)
        {
            // 32-bit signed sub_sat intrinsic function, map to same implementation as in _integer.h std-lib header
            method.appendToEnd((new intermediate::MethodCall(
                                    std::move(output), "vc4cl_saturated_sub", {arguments.at(0), arguments.at(1)}))
                                   ->addDecorations(decorations));
            return true;
        }
    }
    if(methodName.find("llvm.usub.sat") == 0)
    {
        // 16-/32-bit unsigned sub_sat intrinsic function, map to same implementation as in _integer.h std-lib header:
        // x < y ? (result_t)0 : x - y
        auto cond = method.addNewLocal(TYPE_BOOL.toVectorType(output.type.getVectorWidth()));
        method.appendToEnd(new intermediate::Comparison(
            intermediate::COMP_UNSIGNED_LT, Value(cond), Value(arguments.at(0)), Value(arguments.at(1))));
        method.appendToEnd(new intermediate::MoveOperation(NOP_REGISTER, cond, COND_ALWAYS, SetFlag::SET_FLAGS));
        method.appendToEnd(
            (new intermediate::MoveOperation(Value(dest), INT_ZERO, COND_ZERO_CLEAR))->addDecorations(decorations));
        method.appendToEnd((new intermediate::Operation(
                                OP_SUB, std::move(dest), Value(arguments.at(0)), Value(arguments.at(1)), COND_ZERO_SET))
                               ->addDecorations(decorations));
        return true;
    }
    if(methodName.find("shuffle2") == 0)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying OpenCL shuffle2 function with " << arguments.at(0).to_string() << ", "
                << arguments.at(1).to_string() << " and mask " << arguments.at(2).to_string(false, true)
                << logging::endl);
        ignoreReturnValue(intermediate::insertVectorShuffle(
            method.appendToEnd(), method, output, arguments.at(0), arguments.at(1), arguments.at(2)));
        return true;
    }
    if(methodName.find("shuffle") == 0)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Intrinsifying OpenCL shuffle function with " << arguments.at(0).to_string() << " and mask "
                << arguments.at(1).to_string(false, true) << logging::endl);
        ignoreReturnValue(intermediate::insertVectorShuffle(
            method.appendToEnd(), method, output, arguments.at(0), UNDEFINED_VALUE, arguments.at(1)));
        return true;
    }
    static std::regex readFencePattern("_Z\\d*read_mem_fence.*");
    static std::regex writeFencePattern("_Z\\d*write_mem_fence.*");
    if(methodName.find("mem_fence") == 0 || methodName.find("read_mem_fence") == 0 ||
        methodName.find("write_mem_fence") == 0 || std::regex_match(methodName, readFencePattern) ||
        std::regex_match(methodName, writeFencePattern))
    {
        /*
         * NOTE: vc4cl-stdlib implements read_mem_fence() and write_mem_fence() as calls to mem_fence().
         * Since this is a function call passing the parameter as local, the memory scope is not a constant value.
         * Directly mapping the function call to read_mem_fence() and write_mem_fence() to a memory fence instruction
         * still created the methods for these calls, but they will never be executed.
         */
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Intrinsifying '" << methodName << "' with memory barrier" << logging::endl);
        method.appendToEnd(new intermediate::MemoryBarrier(
            static_cast<intermediate::MemoryScope>(arguments.at(0).getLiteralValue()->unsignedInt()),
            intermediate::MemorySemantics::ACQUIRE_RELEASE));
        return true;
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating immediate call to " << methodName << " -> " << dest.type.to_string() << logging::endl);
    if(dest.checkLocal())
        method.appendToEnd(
            (new intermediate::MethodCall(std::move(output), std::move(methodName), std::move(arguments)))
                ->addDecorations(decorations));
    else
        method.appendToEnd(
            (new intermediate::MethodCall(std::move(methodName), std::move(arguments)))->addDecorations(decorations));
    return true;
}

Copy::Copy(Value&& dest, Value&& orig, const bool isLoadStore, const bool isRead, bool isBitcast) :
    dest(dest), orig(orig), isLoadStore(isLoadStore), isRead(isRead), isBitcast(isBitcast)
{
}

bool Copy::mapInstruction(Method& method)
{
    if(isBitcast)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating bit-cast from " << orig.to_string() << " into " << dest.to_string() << logging::endl);
        ignoreReturnValue(intermediate::insertBitcast(method.appendToEnd(), method, orig, dest, decorations));
    }
    else if(isLoadStore)
    {
        if(isRead)
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Generating reading from " << orig.to_string() << " into " << dest.to_string() << logging::endl);
            method.appendToEnd((new intermediate::MemoryInstruction(
                                    intermediate::MemoryOperation::READ, std::move(dest), std::move(orig)))
                                   ->addDecorations(decorations));
        }
        else
        {
            CPPLOG_LAZY(logging::Level::DEBUG,
                log << "Generating writing of " << orig.to_string() << " into " << dest.to_string() << logging::endl);
            method.appendToEnd((new intermediate::MemoryInstruction(
                                    intermediate::MemoryOperation::WRITE, std::move(dest), std::move(orig)))
                                   ->addDecorations(decorations));
        }
    }
    else
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating copy of " << orig.to_string() << " into " << dest.to_string() << logging::endl);
        method.appendToEnd(
            (new intermediate::MoveOperation(std::move(dest), std::move(orig)))->addDecorations(decorations));
    }
    return true;
}

UnaryOperator::UnaryOperator(std::string&& opCode, Value&& dest, Value&& arg) : dest(dest), opCode(opCode), arg(arg) {}

bool UnaryOperator::mapInstruction(Method& method)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating unary operation " << opCode << " with " << arg.to_string() << " into " << dest.to_string()
            << logging::endl);
    auto& op = OpCode::findOpCode(opCode);
    if(op != OP_NOP)
        method.appendToEnd(
            (new intermediate::Operation(op, std::move(dest), std::move(arg)))->addDecorations(decorations));
    else
        method.appendToEnd((new intermediate::IntrinsicOperation(std::move(opCode), std::move(dest), std::move(arg)))
                               ->addDecorations(decorations));
    return true;
}

BinaryOperator::BinaryOperator(std::string&& opCode, Value&& dest, Value&& arg0, Value&& arg1) :
    UnaryOperator(std::forward<std::string>(opCode), std::forward<Value>(dest), std::forward<Value>(arg0)), arg2(arg1)
{
}

bool BinaryOperator::mapInstruction(Method& method)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating binary operation " << opCode << " with " << arg.to_string() << " and " << arg2.to_string()
            << " into " << dest.to_string() << logging::endl);
    auto& op = OpCode::findOpCode(opCode);
    if(op != OP_NOP)
        method.appendToEnd((new intermediate::Operation(op, std::move(dest), std::move(arg), std::move(arg2)))
                               ->addDecorations(decorations));
    else if(opCode == "lshr")
        // logical shift right
        method.appendToEnd((new intermediate::Operation(OP_SHR, std::move(dest), std::move(arg), std::move(arg2)))
                               ->addDecorations(decorations));
    else
        method.appendToEnd(
            (new intermediate::IntrinsicOperation(std::move(opCode), std::move(dest), std::move(arg), std::move(arg2)))
                ->addDecorations(decorations));
    return true;
}

IndexOf::IndexOf(Value&& dest, Value&& container, std::vector<Value>&& indices) :
    dest(dest), container(container), indices(indices)
{
}

bool IndexOf::mapInstruction(Method& method)
{
    // need to get pointer/address -> reference to content
    // a[i] of type t is at position &a + i * sizeof(t)
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating calculating index " << to_string<Value>(indices) << " of " << container.to_string()
            << " into " << dest.to_string() << logging::endl);

    /*
     * LLVM explicitely states for "getelementptr": "The first index always indexes the pointer value given as the
     * second argument, the second index indexes a value of the type pointed to [...]"
     */
    ignoreReturnValue(
        intermediate::insertCalculateIndices(method.appendToEnd(), method, container, dest, indices, false));
    return true;
}

Comparison::Comparison(Value&& dest, std::string&& comp, Value&& op1, Value&& op2, const bool isFloat) :
    dest(dest), comp(comp), isFloat(isFloat), op1(op1), op2(op2)
{
}

bool Comparison::mapInstruction(Method& method)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating comparison " << comp << " with " << op1.to_string() << " and " << op2.to_string() << " into "
            << dest.to_string() << logging::endl);
    method.appendToEnd((new intermediate::Comparison(std::move(comp), std::move(dest), std::move(op1), std::move(op2)))
                           ->addDecorations(decorations));
    return true;
}

ContainerInsertion::ContainerInsertion(Value&& dest, Value&& container, Value&& newValue, Value&& index) :
    dest(dest), container(container), newValue(newValue), index(index)
{
}

bool ContainerInsertion::mapInstruction(Method& method)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating insertion of " << newValue.to_string() << " at " << index.to_string() << " into "
            << container.to_string() << " into " << dest.to_string() << logging::endl);
    // 1. copy whole container
    method.appendToEnd(new intermediate::MoveOperation(std::move(dest), std::move(container)));
    // 2. insert new element
    // either into vector or into scalar at "element 0"
    if(container.type.isVectorType() || index.hasLiteral(Literal(0u)))
    {
        // insert element at given index into vector
        ignoreReturnValue(intermediate::insertVectorInsertion(method.appendToEnd(), method, dest, index, newValue));
    }
    else
    {
        throw CompilationError(CompilationStep::LLVM_2_IR, "Container insertion into arrays is not yet implemented",
            container.to_string());
    }
    return true;
}

ContainerExtraction::ContainerExtraction(Value&& dest, Value&& container, Value&& index) :
    dest(dest), container(container), index(index)
{
}

bool ContainerExtraction::mapInstruction(Method& method)
{
    const DataType elementType = container.type.getElementType();
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generation extraction of " << elementType.to_string() << " at " << index.to_string() << " from "
            << container.to_string() << " into " << dest.to_string() << logging::endl);

    if(container.type.isVectorType() || index.hasLiteral(Literal(0u)))
    {
        ignoreReturnValue(intermediate::insertVectorExtraction(method.appendToEnd(), method, container, index, dest));
    }
    else
    {
        throw CompilationError(CompilationStep::LLVM_2_IR, "Container extraction from arrays is not yet implemented",
            container.to_string());
    }

    return true;
}

ValueReturn::ValueReturn() : hasValue(false), val(Literal(false), TYPE_VOID) {}

ValueReturn::ValueReturn(Value&& val) : hasValue(true), val(val) {}

bool ValueReturn::mapInstruction(Method& method)
{
    if(hasValue)
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Generating return of " << val.to_string() << logging::endl);
        method.appendToEnd((new intermediate::Return(std::move(val)))->addDecorations(decorations));
    }
    else
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Generating return nothing" << logging::endl);
        method.appendToEnd((new intermediate::Return())->addDecorations(decorations));
    }
    return true;
}

ShuffleVector::ShuffleVector(Value&& dest, Value&& v1, Value&& v2, Value&& mask) :
    dest(dest), v1(v1), v2(v2), mask(mask)
{
}

bool ShuffleVector::mapInstruction(Method& method)
{
    // shuffling = iteration over all elements in both vectors and re-ordering in order given
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating operations mixing " << v1.to_string() << " and " << v2.to_string() << " into "
            << dest.to_string() << logging::endl);
    ignoreReturnValue(intermediate::insertVectorShuffle(method.appendToEnd(), method, dest, v1, v2, mask));
    return true;
}

LLVMLabel::LLVMLabel(Value&& label) : label(label) {}

bool LLVMLabel::mapInstruction(Method& method)
{
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Generating label " << label.to_string() << logging::endl);
    method.appendToEnd((new intermediate::BranchLabel(*label.local()))->addDecorations(decorations));
    return true;
}

PhiNode::PhiNode(Value&& dest, std::vector<std::pair<Value, const Local*>>&& labels) : dest(dest), labels(labels) {}

bool PhiNode::mapInstruction(Method& method)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating Phi-Node with " << labels.size() << " options into " << dest.to_string() << logging::endl);
    method.appendToEnd((new intermediate::PhiNode(std::move(dest), std::move(labels)))->addDecorations(decorations));
    return true;
}

Selection::Selection(Value&& dest, Value&& cond, Value&& opt1, Value&& opt2) :
    dest(dest), cond(cond), opt1(opt1), opt2(opt2)
{
}

bool Selection::mapInstruction(Method& method)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating moves for selection " << opt1.to_string() << " or " << opt2.to_string() << " according to "
            << cond.to_string() << logging::endl);
    // if cond == 1 -> first else second
    // makes sure, the flags are set for the correction value

    if(cond.type.isScalarType() && (!opt1.type.isScalarType() || !opt2.type.isScalarType()))
    {
        /*
         * LLVM language reference, section 'select' semantics:
         * "If the condition is an i1 and the value arguments are vectors of the same size, then an entire vector is
         * selected."
         */
        auto it = intermediate::insertReplication(method.appendToEnd(), cond, NOP_REGISTER, true);
        it.previousInBlock()->setFlags = SetFlag::SET_FLAGS;
    }
    else
        method.appendToEnd(new intermediate::MoveOperation(NOP_REGISTER, cond, COND_ALWAYS, SetFlag::SET_FLAGS));

    method.appendToEnd(new intermediate::MoveOperation(Value(dest), std::move(opt1), COND_ZERO_CLEAR));
    method.appendToEnd(new intermediate::MoveOperation(std::move(dest), std::move(opt2), COND_ZERO_SET));
    return true;
}

Branch::Branch(Value&& label) : thenLabel(label), elseLabel(UNDEFINED_VALUE), cond(BOOL_TRUE) {}

Branch::Branch(Value&& cond, Value&& thenLabel, Value&& elseLabel) :
    thenLabel(thenLabel), elseLabel(elseLabel), cond(cond)
{
}

bool Branch::mapInstruction(Method& method)
{
    if(cond == BOOL_TRUE)
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating unconditional branch to " << thenLabel.to_string() << logging::endl);
        method.appendToEnd(
            (new intermediate::Branch(thenLabel.local(), COND_ALWAYS, BOOL_TRUE))->addDecorations(decorations));
    }
    else
    {
        CPPLOG_LAZY(logging::Level::DEBUG,
            log << "Generating branch on condition " << cond.to_string() << " to either " << thenLabel.to_string()
                << " or " << elseLabel.to_string() << logging::endl);
        method.appendToEnd((new intermediate::Branch(thenLabel.local(), COND_ZERO_CLEAR /* condition is true */, cond))
                               ->addDecorations(decorations));
        method.appendToEnd((new intermediate::Branch(elseLabel.local(), COND_ZERO_SET /* condition is false */, cond))
                               ->addDecorations(decorations));
    }

    return true;
}

Switch::Switch(Value&& cond, Value&& defaultLabel, FastMap<int, Value>&& cases) :
    cond(cond), defaultLabel(defaultLabel), jumpLabels(cases)
{
}

bool Switch::mapInstruction(Method& method)
{
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Generating branches for switch on " << cond.to_string() << " with " << jumpLabels.size()
            << " options and the default " << defaultLabel.to_string() << logging::endl);
    for(const auto& option : jumpLabels)
    {
        // for every case, if equal,branch to given label
        Value tmp = method.addNewLocal(TYPE_BOOL, "%switch");
        method.appendToEnd(new intermediate::Comparison(
            intermediate::COMP_EQ, Value(tmp), std::move(cond), Value(Literal(option.first), TYPE_INT32)));
        method.appendToEnd(new intermediate::Branch(option.second.local(), COND_ZERO_CLEAR, tmp));
    }
    // branch default label
    method.appendToEnd(new intermediate::Branch(defaultLabel.local(), COND_ALWAYS, BOOL_TRUE));

    return true;
}
