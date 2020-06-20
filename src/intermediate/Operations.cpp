/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "IntermediateInstruction.h"

#include "../asm/ALUInstruction.h"
#include "../periphery/VPM.h"
#include "log.h"

#include <cmath>

using namespace vc4c;
using namespace vc4c::intermediate;

Operation::Operation(
    const OpCode& opCode, const Value& dest, const Value& arg0, const ConditionCode cond, const SetFlag setFlags) :
    UnpackingInstruction(SIGNAL_NONE, cond, setFlags, PACK_NOP, UNPACK_NOP, dest),
    op(opCode), parent(nullptr)
{
    if(opCode.numOperands != 1)
        throw CompilationError(
            CompilationStep::GENERAL, "Passing a single argument to a non-unary operation", opCode.name);
    setArgument(0, arg0);
}

Operation::Operation(OpCode opCode, Value&& dest, Value&& arg0, ConditionCode cond, SetFlag setFlags) :
    UnpackingInstruction(SIGNAL_NONE, cond, setFlags, PACK_NOP, UNPACK_NOP, std::move(dest)), op(std::move(opCode)),
    parent(nullptr)
{
    if(opCode.numOperands != 1)
        throw CompilationError(
            CompilationStep::GENERAL, "Passing a single argument to a non-unary operation", opCode.name);
    setArgument(0, std::move(arg0));
}

Operation::Operation(const OpCode& opCode, const Value& dest, const Value& arg0, const Value& arg1,
    const ConditionCode cond, const SetFlag setFlags) :
    UnpackingInstruction(SIGNAL_NONE, cond, setFlags, PACK_NOP, UNPACK_NOP, dest),
    op(opCode), parent(nullptr)
{
    if(opCode.numOperands != 2)
        throw CompilationError(
            CompilationStep::GENERAL, "Passing two arguments to a non-binary operation", opCode.name);
    setArgument(0, arg0);
    setArgument(1, arg1);
}

Operation::Operation(OpCode opCode, Value&& dest, Value&& arg0, Value&& arg1, ConditionCode cond, SetFlag setFlags) :
    UnpackingInstruction(SIGNAL_NONE, cond, setFlags, PACK_NOP, UNPACK_NOP, std::move(dest)), op(opCode),
    parent(nullptr)
{
    if(opCode.numOperands != 2)
        throw CompilationError(
            CompilationStep::GENERAL, "Passing two arguments to a non-binary operation", opCode.name);
    setArgument(0, std::move(arg0));
    setArgument(1, std::move(arg1));
}

LCOV_EXCL_START
std::string Operation::to_string() const
{
    return (getOutput()->to_string(true) + " = ") + (std::string(op.name) + " ") + getFirstArg().to_string() +
        (getSecondArg() ? std::string(", ") + assertArgument(1).to_string() : "") + createAdditionalInfoString();
}
LCOV_EXCL_STOP

IntermediateInstruction* Operation::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    if(!getSecondArg())
        return (new Operation(op, renameValue(method, getOutput().value(), localPrefix, localMapping),
                    renameValue(method, getFirstArg(), localPrefix, localMapping), conditional, setFlags))
            ->copyExtrasFrom(this);
    return (new Operation(op, renameValue(method, getOutput().value(), localPrefix, localMapping),
                renameValue(method, getFirstArg(), localPrefix, localMapping),
                renameValue(method, assertArgument(1), localPrefix, localMapping), conditional, setFlags))
        ->copyExtrasFrom(this);
}

static InputMultiplex getInputMux(Register reg, const bool isExplicitRegister,
    const Optional<SmallImmediate>& immediate, const bool isABlocked = false, const bool isBBlocked = false)
{
    if(immediate)
        return InputMultiplex::REGB;
    if(!isExplicitRegister && reg.isAccumulator())
    {
        return static_cast<InputMultiplex>(reg.getAccumulatorNumber());
    }
    if(isExplicitRegister && reg.isAccumulator() &&
        (reg.getAccumulatorNumber() == 5 || reg.getAccumulatorNumber() == 4))
    {
        // special case: is handled as register, but need to access the accumulator
        // reason: the value is actually read from the accumulator, not from the register with the same index
        return static_cast<InputMultiplex>(reg.getAccumulatorNumber());
    }
    if(reg.file == RegisterFile::PHYSICAL_A)
        return InputMultiplex::REGA;
    if(reg.file == RegisterFile::PHYSICAL_B)
        return InputMultiplex::REGB;
    if(!isABlocked)
        return InputMultiplex::REGA;
    if(!isBBlocked)
        return InputMultiplex::REGB;
    throw CompilationError(CompilationStep::CODE_GENERATION, "Invalid input getSource()", reg.to_string(true, true));
}

static Register getRegister(const RegisterFile file, const Register reg0, const Register reg1, const Operation* op)
{
    if(reg0.file == file && reg1.file == file)
    {
        if(reg0.isAccumulator())
            return reg1;
        if(reg1.isAccumulator())
            return reg0;
        if(reg0 == reg1)
            return reg0;
        if(reg0.num == REG_NOP.num)
            return reg1;
        if(reg1.num == REG_NOP.num)
            return reg0;
        logging::error() << "Error in assigning registers for " << op->to_string() << logging::endl;
        throw CompilationError(CompilationStep::CODE_GENERATION, "Can't access two registers from the same file",
            (reg0.to_string(true, true) + " and ") + reg1.to_string(true, true));
    }
    else if(reg0.file == file)
        return reg0;
    else if(reg1.file == file)
        return reg1;
    else if(has_flag(reg0.file, RegisterFile::PHYSICAL_ANY))
        return reg0;
    else if(has_flag(reg1.file, RegisterFile::PHYSICAL_ANY))
        return reg1;
    return REG_NOP;
}

static std::pair<Register, Optional<SmallImmediate>> getInputValue(
    const Value& val, const FastMap<const Local*, Register>& registerMapping, const IntermediateInstruction* instr)
{
    if(auto reg = val.checkRegister())
        return std::make_pair(*reg, Optional<SmallImmediate>{});
    if(auto loc = val.checkLocal())
    {
        const Register reg = registerMapping.at(loc);
        return std::make_pair(reg, Optional<SmallImmediate>{});
    }
    if(auto imm = val.checkImmediate())
        return std::make_pair(Register{RegisterFile::PHYSICAL_B, imm->value}, *imm);
    throw CompilationError(CompilationStep::CODE_GENERATION, "Unhandled value", instr->to_string());
}

qpu_asm::DecoratedInstruction Operation::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    const Register outReg = getOutput()->checkLocal() ? registerMapping.at(getOutput()->local()) : getOutput()->reg();

    Unpack unpack = unpackMode;
    Pack pack = packMode;
    if(pack.isPMBitSet() != unpack.isPMBitSet())
    {
        if(pack.hasEffect() && unpack.hasEffect())
            throw CompilationError(
                CompilationStep::CODE_GENERATION, "PM bit of pack and unpack modes do not match", to_string());
        // align pm bit for not-set mode to the set one (so thy don't re-set the pm bit)
        if(pack.hasEffect())
            unpack = pack.isPMBitSet() ? UNPACK_NOP_PM : UNPACK_NOP;
        else if(unpack.hasEffect())
            pack = unpack.isPMBitSet() ? PACK_NOP_PM : PACK_NOP;
    }
    // make sure, only mul pack modes are used for mul ALU writing to register file A
    if(!op.runsOnAddALU() && pack.hasEffect() && !pack.supportsMulALU())
        throw CompilationError(
            CompilationStep::CODE_GENERATION, "Cannot use add ALU only pack mode with mul ALU operation", to_string());

    auto input0 = getInputValue(getFirstArg(), registerMapping, this);
    auto input1 = getSecondArg() ? getInputValue(assertArgument(1), registerMapping, this) :
                                   std::make_pair(REG_NOP, Optional<SmallImmediate>{});

    if(unpack.hasEffect() && unpack.isUnpackFromR4() && !getFirstArg().hasRegister(REG_SFU_OUT) &&
        !(getSecondArg() && assertArgument(1).hasRegister(REG_SFU_OUT)))
        throw CompilationError(
            CompilationStep::CODE_GENERATION, "Can only apply r4 unpack mode when reading r4", to_string());

    // make sure the 32-bit saturation pack mode is only used for iadd/isub operations
    if(pack == PACK_32_32 && op.opAdd != OP_ADD.opAdd && op.opAdd != OP_SUB.opAdd)
        throw CompilationError(CompilationStep::CODE_GENERATION,
            "32-bit saturation is only applicable for integer addition/subtraction", to_string());

    InputMultiplex inMux0 = getInputMux(input0.first, getFirstArg().checkRegister(), input0.second);
    if(!input0.second)
    {
        if(input0.first.isAccumulator() && inMux0 != InputMultiplex::REGA && inMux0 != InputMultiplex::REGB)
            input0.first.num = REG_NOP.num; // Cosmetics, so vc4asm does not print "maybe reading reg xx"
        else if(has_flag(input0.first.file, RegisterFile::PHYSICAL_ANY) &&
            (!getSecondArg() || input1.first.file != RegisterFile::PHYSICAL_A))
            // this is only correct, if the second value is not fixed to file A
            input0.first.file = RegisterFile::PHYSICAL_A;
    }

    // for instructions, which can be mapped to both ALUs, map to add ALU
    bool writeSwap = false;
    bool isAddALU = false;
    bool isMulALU = false;
    if(op.runsOnAddALU())
    {
        writeSwap = outReg.file == RegisterFile::PHYSICAL_B;
        isAddALU = true;
    }
    else if(op.runsOnMulALU())
    {
        writeSwap = outReg.file == RegisterFile::PHYSICAL_A;
        isMulALU = true;
    }

    const WriteSwap swap = writeSwap ? WriteSwap::SWAP : WriteSwap::DONT_SWAP;

    if(auto arg1 = getSecondArg())
    {
        InputMultiplex inMux1 = getInputMux(input1.first, arg1->checkRegister(), input1.second,
            input0.first.file == RegisterFile::PHYSICAL_A, input0.first.file == RegisterFile::PHYSICAL_B);

        // one of the values is a literal immediate
        if(input0.second || input1.second)
        {
            SmallImmediate imm = (input0.second ? input0.second : input1.second).value();
            Register regA = input0.second ? input1.first : input0.first;
            if(input0.second && input1.second)
            {
                // both operands have literal
                if(input0.second.value() != input1.second.value())
                    throw CompilationError(CompilationStep::CODE_GENERATION,
                        "Can't perform operation with two distinguish literals", to_string());
                regA = REG_NOP;
            }
            else if(input0.second && input1.first.file == RegisterFile::ACCUMULATOR)
                // don't read register overlapped by accumulator, e.g. UNIFORM
                regA = REG_NOP;
            else if(input1.second && input0.first.file == RegisterFile::ACCUMULATOR)
                regA = REG_NOP;
            if(signal.hasSideEffects())
            {
                throw CompilationError(CompilationStep::CODE_GENERATION,
                    "Signal is discarded, since the operation takes an immediate", signal.to_string());
            }

            if(isAddALU)
            {
                return qpu_asm::ALUInstruction(unpack, pack, conditional, COND_NEVER, setFlags, swap, outReg.num,
                    REG_NOP.num, OP_NOP, op, regA.num, imm, inMux0, inMux1, MULTIPLEX_NONE, MULTIPLEX_NONE);
            }
            else if(isMulALU)
            {
                return qpu_asm::ALUInstruction(unpack, pack, COND_NEVER, conditional, setFlags, swap, REG_NOP.num,
                    outReg.num, op, OP_NOP, regA.num, imm, MULTIPLEX_NONE, MULTIPLEX_NONE, inMux0, inMux1);
            }
            throw CompilationError(CompilationStep::CODE_GENERATION, "No instruction set", to_string());
        }
        // both are registers
        else
        {
            if(input1.first.isAccumulator() && inMux1 != InputMultiplex::REGA && inMux1 != InputMultiplex::REGB)
                input1.first.num = REG_NOP.num; // Cosmetics, so vc4asm does not print "maybe reading reg xx"
            else if(has_flag(input1.first.file, RegisterFile::PHYSICAL_ANY))
            {
                if(getInputValue(getFirstArg(), registerMapping, this).first ==
                    getInputValue(*arg1, registerMapping, this).first)
                {
                    // same inputs - this allows e.g. vc4asm to recognize "mov x, uniform" correctly
                    input1.first.file = input0.first.file;
                    inMux1 = inMux0;
                }
                else if(input0.first.file == RegisterFile::ACCUMULATOR && inMux1 == InputMultiplex::REGA)
                    // the above instruction ( InputMultiplex inMux1 = ...) selects the input mux for register-file A
                    // if it is not explicitly blocked by the first argument (which it is not for accumulators), so we
                    // need to set the register-file A here too if possible (which it is for registers located on both
                    // physical files)
                    input1.first.file = RegisterFile::PHYSICAL_A;
                else if(input0.first.file == RegisterFile::PHYSICAL_B)
                    input1.first.file = RegisterFile::PHYSICAL_A;
                else
                    input1.first.file = RegisterFile::PHYSICAL_B;
            }
            if(inMux0 == inMux1 && input0.first != input1.first)
            {
                // "any physical file" for second argument is handled above. Since the first argument for "any physical
                // file" defaults to file A, we need to explicitly select it from B if the second argument is fixed to
                // A.
                if(has_flag(input0.first.file, RegisterFile::PHYSICAL_ANY))
                {
                    inMux0 = inMux1 == InputMultiplex::REGA ? InputMultiplex::REGB : InputMultiplex::REGA;
                }
                else
                    throw CompilationError(CompilationStep::CODE_GENERATION,
                        "Two distinct inputs use the same input multiplex", to_string());
            }
            if(isAddALU)
            {
                return qpu_asm::ALUInstruction(signal, unpack, pack, conditional, COND_NEVER, setFlags, swap,
                    outReg.num, REG_NOP.num, OP_NOP, op,
                    getRegister(RegisterFile::PHYSICAL_A, input0.first, input1.first, this).num,
                    getRegister(RegisterFile::PHYSICAL_B, input0.first, input1.first, this).num, inMux0, inMux1,
                    MULTIPLEX_NONE, MULTIPLEX_NONE);
            }
            else if(isMulALU)
            {
                return qpu_asm::ALUInstruction(signal, unpack, pack, COND_NEVER, conditional, setFlags, swap,
                    REG_NOP.num, outReg.num, op, OP_NOP,
                    getRegister(RegisterFile::PHYSICAL_A, input0.first, input1.first, this).num,
                    getRegister(RegisterFile::PHYSICAL_B, input0.first, input1.first, this).num, MULTIPLEX_NONE,
                    MULTIPLEX_NONE, inMux0, inMux1);
            }
            throw CompilationError(CompilationStep::CODE_GENERATION, "No instruction set", to_string());
        }
    }
    // only 1 input
    else
    {
        if(input0.second)
        {
            // unary operation with immediate
            const SmallImmediate imm = input0.second.value();
            if(signal.hasSideEffects())
            {
                throw CompilationError(CompilationStep::CODE_GENERATION,
                    "Signal is discarded, since the operation takes an immediate", signal.to_string());
            }

            if(isAddALU)
            {
                return qpu_asm::ALUInstruction(unpack, pack, conditional, COND_NEVER, setFlags, swap, outReg.num,
                    REG_NOP.num, OP_NOP, op, REG_NOP.num, imm, inMux0, MULTIPLEX_NONE, MULTIPLEX_NONE, MULTIPLEX_NONE);
            }
            else if(isMulALU)
            {
                return qpu_asm::ALUInstruction(unpack, pack, COND_NEVER, conditional, setFlags, swap, REG_NOP.num,
                    outReg.num, op, OP_NOP, REG_NOP.num, imm, MULTIPLEX_NONE, MULTIPLEX_NONE, inMux0, MULTIPLEX_NONE);
            }
        }
        else
        {
            if(isAddALU)
            {
                return qpu_asm::ALUInstruction(signal, unpack, pack, conditional, COND_NEVER, setFlags, swap,
                    outReg.num, REG_NOP.num, OP_NOP, op,
                    getRegister(RegisterFile::PHYSICAL_A, input0.first, REG_NOP, this).num,
                    getRegister(RegisterFile::PHYSICAL_B, input0.first, REG_NOP, this).num, inMux0, MULTIPLEX_NONE,
                    MULTIPLEX_NONE, MULTIPLEX_NONE);
            }
            else if(isMulALU)
            {
                return qpu_asm::ALUInstruction(signal, unpack, pack, COND_NEVER, conditional, setFlags, swap,
                    REG_NOP.num, outReg.num, op, OP_NOP,
                    getRegister(RegisterFile::PHYSICAL_A, input0.first, REG_NOP, this).num,
                    getRegister(RegisterFile::PHYSICAL_B, input0.first, REG_NOP, this).num, MULTIPLEX_NONE,
                    MULTIPLEX_NONE, inMux0, MULTIPLEX_NONE);
            }
        }
        throw CompilationError(CompilationStep::CODE_GENERATION, "No instruction set", to_string());
    }
}

bool Operation::mapsToASMInstruction() const
{
    // this instruction is not mapped to an ASM instruction, if either of the operands is undefined, since then the
    // result is undefined too
    if(getFirstArg().isUndefined())
        return false;
    if(op.numOperands > 1 && assertArgument(1).isUndefined())
        return false;
    return true;
}

bool Operation::isNormalized() const
{
    //"normalized" NOPs are handled via the NOP instruction class.
    // If an operation has the NOP op-code, it has an op-code name which cannot be mapped to machine code -> not
    // normalized
    return op != OP_NOP;
}

const Value& Operation::getFirstArg() const
{
    return assertArgument(0);
}

const Optional<Value> Operation::getSecondArg() const
{
    return getArgument(1);
}

PrecalculatedValue Operation::precalculate(const std::size_t numIterations) const
{
    Optional<Value> arg0 = getPrecalculatedValueForArg(0, numIterations);
    if(!arg0)
        return PrecalculatedValue{NO_VALUE, {}};
    Optional<Value> arg1 = NO_VALUE;
    if(getSecondArg())
    {
        arg1 = getPrecalculatedValueForArg(1, numIterations);
        if(!arg1)
            return PrecalculatedValue{NO_VALUE, {}};
    }

    return op(*arg0, arg1);
}

bool Operation::isSimpleOperation() const
{
    return !hasSideEffects() && !unpackMode.hasEffect() && !packMode.hasEffect();
}

bool Operation::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherOp = dynamic_cast<const Operation*>(&other))
        return op == otherOp->op;
    return false;
}

IntrinsicOperation::IntrinsicOperation(std::string&& opCode, Value&& dest, Value&& arg0) :
    IntermediateInstruction(std::move(dest)), opCode(opCode)
{
    setArgument(0, std::move(arg0));
}

IntrinsicOperation::IntrinsicOperation(std::string&& opCode, Value&& dest, Value&& arg0, Value&& arg1) :
    IntermediateInstruction(std::move(dest)), opCode(opCode)
{
    setArgument(0, std::move(arg0));
    setArgument(1, std::move(arg1));
}

LCOV_EXCL_START
std::string IntrinsicOperation::to_string() const
{
    return (getOutput()->to_string(true) + " = ") + (opCode + " ") + getFirstArg().to_string() +
        (getSecondArg() ? std::string(", ") + assertArgument(1).to_string() : "") + createAdditionalInfoString();
}
LCOV_EXCL_STOP

IntermediateInstruction* IntrinsicOperation::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    if(!getSecondArg())
        return (new IntrinsicOperation(std::string(opCode),
                    renameValue(method, getOutput().value(), localPrefix, localMapping),
                    renameValue(method, getFirstArg(), localPrefix, localMapping)))
            ->copyExtrasFrom(this);
    return (
        new IntrinsicOperation(std::string(opCode), renameValue(method, getOutput().value(), localPrefix, localMapping),
            renameValue(method, getFirstArg(), localPrefix, localMapping),
            renameValue(method, assertArgument(1), localPrefix, localMapping)))
        ->copyExtrasFrom(this);
}

LCOV_EXCL_START
qpu_asm::DecoratedInstruction IntrinsicOperation::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, std::size_t instructionIndex) const
{
    throw CompilationError(CompilationStep::OPTIMIZER, "There should be no more intrinsic operations", to_string());
}

bool IntrinsicOperation::mapsToASMInstruction() const
{
    return false;
}

bool IntrinsicOperation::isNormalized() const
{
    return false;
}
LCOV_EXCL_STOP

const Value& IntrinsicOperation::getFirstArg() const
{
    return assertArgument(0);
}

const Optional<Value> IntrinsicOperation::getSecondArg() const
{
    return getArgument(1);
}

bool IntrinsicOperation::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherOp = dynamic_cast<const IntrinsicOperation*>(&other))
        return opCode == otherOp->opCode;
    return false;
}

MoveOperation::MoveOperation(const Value& dest, const Value& arg, const ConditionCode cond, const SetFlag setFlags) :
    UnpackingInstruction(SIGNAL_NONE, cond, setFlags, PACK_NOP, UNPACK_NOP, dest)
{
    setArgument(0, arg);
}

MoveOperation::MoveOperation(Value&& dest, Value&& arg, const ConditionCode cond, const SetFlag setFlags) :
    UnpackingInstruction(SIGNAL_NONE, cond, setFlags, PACK_NOP, UNPACK_NOP, std::move(dest))
{
    setArgument(0, std::move(arg));
}

LCOV_EXCL_START
std::string MoveOperation::to_string() const
{
    if(getSource().getLiteralValue() &&
        (getOutput()->hasRegister(REG_VPM_IN_SETUP) ||
            has_flag(decoration, InstructionDecorations::VPM_READ_CONFIGURATION)))
        return (getOutput()->to_string(true) + " = ") +
            periphery::VPRSetup::fromLiteral(getSource().getLiteralValue()->unsignedInt()).to_string() +
            createAdditionalInfoString();
    if(getSource().getLiteralValue() &&
        (getOutput()->hasRegister(REG_VPM_OUT_SETUP) ||
            has_flag(decoration, InstructionDecorations::VPM_WRITE_CONFIGURATION)))
        return (getOutput()->to_string(true) + " = ") +
            periphery::VPWSetup::fromLiteral(getSource().getLiteralValue()->unsignedInt()).to_string() +
            createAdditionalInfoString();
    return (getOutput()->to_string(true) + " = ") + getSource().to_string() + createAdditionalInfoString();
}
LCOV_EXCL_STOP

IntermediateInstruction* MoveOperation::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    return (new MoveOperation(renameValue(method, getOutput().value(), localPrefix, localMapping),
                renameValue(method, getSource(), localPrefix, localMapping), conditional, setFlags))
        ->copyExtrasFrom(this);
}

qpu_asm::DecoratedInstruction MoveOperation::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    // simply call this method for intermediate-operation
    Operation op(OP_OR, getOutput().value(), getSource(), getSource(), conditional, setFlags);
    op.setPackMode(packMode);
    op.setSignaling(signal);
    op.setUnpackMode(unpackMode);
    return op.convertToAsm(registerMapping, labelMapping, instructionIndex);
}

Operation* MoveOperation::combineWith(const OpCode& otherOpCode) const
{
    if(otherOpCode == OP_NOP)
        throw CompilationError(CompilationStep::GENERAL,
            "Cannot combine move-operation with operation without a valid ALU instruction", to_string());
    Operation* op = nullptr;
    if(otherOpCode.runsOnMulALU() && (!packMode.hasEffect() || !packMode.supportsMulALU()))
    {
        // use ADD ALU
        op = new Operation(OP_OR, getOutput().value(), getSource(), getSource(), conditional, setFlags);
    }
    else if(otherOpCode.runsOnAddALU() && (!packMode.hasEffect() || packMode.supportsMulALU()) &&
        setFlags == SetFlag::DONT_SET)
    {
        // use MUL ALU
        op = new Operation(OP_V8MIN, getOutput().value(), getSource(), getSource(), conditional, setFlags);
    }
    if(op != nullptr)
    {
        op->setPackMode(packMode);
        op->setUnpackMode(unpackMode);
        op->setSignaling(signal);
        op->decoration = decoration;
    }
    // if the combination failed, the instructions remain separate
    return op;
}

bool MoveOperation::mapsToASMInstruction() const
{
    // this instruction is not mapped to an ASM instruction, if the source is undefined, since then the result is
    // undefined too
    return !getSource().isUndefined();
}

bool MoveOperation::isNormalized() const
{
    return true;
}

PrecalculatedValue MoveOperation::precalculate(const std::size_t numIterations) const
{
    if(auto source = getPrecalculatedValueForArg(0, numIterations))
        // this gives better result (in regards to flags set) then just returning the possible constant source
        return OP_OR(*source, *source);
    return PrecalculatedValue{NO_VALUE, VectorFlags{}};
}

void MoveOperation::setSource(Value&& value)
{
    setArgument(0, std::move(value));
}

const Value& MoveOperation::getSource() const
{
    return assertArgument(0);
}

bool MoveOperation::isSimpleMove() const
{
    return !hasSideEffects() && !unpackMode.hasEffect() && !packMode.hasEffect();
}

bool MoveOperation::innerEquals(const IntermediateInstruction& other) const
{
    // no extra fields to check
    return dynamic_cast<const MoveOperation*>(&other);
}

VectorRotation::VectorRotation(const Value& dest, const Value& src, const SmallImmediate& offset, RotationType type,
    const ConditionCode cond, const SetFlag setFlags) :
    MoveOperation(dest, src, cond, setFlags),
    type(type)
{
    signal = SIGNAL_ALU_IMMEDIATE;
    if(offset == VECTOR_ROTATE_R5)
        // required to track the readers of the rotation registers
        setArgument(1, ROTATION_REGISTER);
    else
        setArgument(1, Value(offset, TYPE_INT8));
    if(!offset.isVectorRotation())
        throw CompilationError(
            CompilationStep::GENERAL, "Can only rotate by one of the rotation immediate values", to_string());
}

VectorRotation::VectorRotation(Value&& dest, Value&& src, SmallImmediate&& offset, RotationType type,
    const ConditionCode cond, const SetFlag setFlags) :
    MoveOperation(std::move(dest), std::move(src), cond, setFlags),
    type(type)
{
    signal = SIGNAL_ALU_IMMEDIATE;
    if(offset == VECTOR_ROTATE_R5)
        // required to track the readers of the rotation registers
        setArgument(1, ROTATION_REGISTER);
    else
        setArgument(1, Value(offset, TYPE_INT8));
    if(!offset.isVectorRotation())
        throw CompilationError(
            CompilationStep::GENERAL, "Can only rotate by one of the rotation immediate values", to_string());
}

LCOV_EXCL_START
std::string VectorRotation::to_string() const
{
    // this is only for display purposes, the rotation register is handled correctly
    return (getOutput()->to_string(true) + " = ") + (getSource().to_string() + " ") + getOffset().to_string() +
        (type == RotationType::FULL ? " (full)" : (type == RotationType::PER_QUAD ? "(quad)" : "(full/quad)")) +
        createAdditionalInfoString();
}
LCOV_EXCL_STOP

IntermediateInstruction* VectorRotation::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    return (new VectorRotation(renameValue(method, getOutput().value(), localPrefix, localMapping),
                renameValue(method, getSource(), localPrefix, localMapping), getOffset(), type, conditional, setFlags))
        ->copyExtrasFrom(this);
}

qpu_asm::DecoratedInstruction VectorRotation::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    const Register outReg = getOutput()->checkLocal() ? registerMapping.at(getOutput()->local()) : getOutput()->reg();

    auto input = getInputValue(getSource(), registerMapping, this);
    const InputMultiplex inMux = getInputMux(input.first, false, Optional<SmallImmediate>{});
    if(inMux == InputMultiplex::REGB)
        // not allowed in either case, since input B is used for rotation offset
        throw CompilationError(CompilationStep::CODE_GENERATION, "Can't rotate register-file B as input", to_string());
    if(!isPerQuadRotationAllowed() && inMux == InputMultiplex::REGA)
        throw CompilationError(CompilationStep::CODE_GENERATION,
            "Can't rotate across full SIMD-vector from physical register as input", to_string());
    if(!isFullRotationAllowed() && inMux != InputMultiplex::REGA)
        throw CompilationError(
            CompilationStep::CODE_GENERATION, "Per-quad rotate must take register-file A as input", to_string());

    // make sure the 32-bit saturation pack mode is only used for iadd/isub operations
    if(packMode == PACK_32_32)
        throw CompilationError(CompilationStep::CODE_GENERATION,
            "32-bit saturation is only applicable for integer addition/subtraction", to_string());

    if(input.first.isAccumulator() && inMux != InputMultiplex::REGA)
        input.first.num = REG_NOP.num; // Cosmetics, so vc4asm does not print "maybe reading reg xx"
    else if(has_flag(input.first.file, RegisterFile::PHYSICAL_ANY))
        input.first.file = RegisterFile::PHYSICAL_A;

    const bool writeSwap = outReg.file == RegisterFile::PHYSICAL_A;
    const WriteSwap swap = writeSwap ? WriteSwap::SWAP : WriteSwap::DONT_SWAP;

    return qpu_asm::ALUInstruction(unpackMode, packMode, COND_NEVER, conditional, setFlags, swap, REG_NOP.num,
        outReg.num, OP_V8MIN, OP_NOP, input.first.num, getOffset(), MULTIPLEX_NONE, MULTIPLEX_NONE, inMux, inMux);
}

Operation* VectorRotation::combineWith(const std::string& otherOpCode) const
{
    // for now, don't support this
    return nullptr;
}

PrecalculatedValue VectorRotation::precalculate(const std::size_t numIterations) const
{
    // for now, don't precalculate
    return PrecalculatedValue{NO_VALUE, {}};
}

const SmallImmediate& VectorRotation::getOffset() const
{
    auto& val = assertArgument(1);
    if(val == ROTATION_REGISTER)
        return VECTOR_ROTATE_R5;
    return assertArgument(1).immediate();
}

bool VectorRotation::isSimpleMove() const
{
    return false;
}

bool VectorRotation::isPerQuadRotationAllowed() const
{
    return type != RotationType::FULL;
}

bool VectorRotation::isFullRotationAllowed() const
{
    return type != RotationType::PER_QUAD;
}

bool VectorRotation::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherRot = dynamic_cast<const VectorRotation*>(&other))
        return type == otherRot->type;
    return false;
}

LCOV_EXCL_START
static std::string toTypeString(DelayType delay)
{
    switch(delay)
    {
    case DelayType::BRANCH_DELAY:
        return "branch";
    case DelayType::THREAD_END:
        return "end";
    case DelayType::WAIT_REGISTER:
        return "register";
    case DelayType::WAIT_SFU:
        return "sfu";
    case DelayType::WAIT_TMU:
        return "tmu";
    case DelayType::WAIT_UNIFORM:
        return "uniform_address";
    case DelayType::WAIT_VPM:
        return "vpm_dma";
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Invalid nop delay type", std::to_string(static_cast<unsigned>(delay)));
}
LCOV_EXCL_STOP

Nop::Nop(const DelayType type, const Signaling signal) : SignalingInstruction(signal), type(type)
{
    // add mandatory-delay marker for delays which cannot be removed
    if(type == DelayType::BRANCH_DELAY || type == DelayType::WAIT_REGISTER || type == DelayType::WAIT_SFU ||
        type == DelayType::WAIT_UNIFORM)
        decoration = InstructionDecorations::MANDATORY_DELAY;
}

LCOV_EXCL_START
std::string Nop::to_string() const
{
    return std::string("nop (") + toTypeString(type) + ")" + createAdditionalInfoString();
}
LCOV_EXCL_STOP

IntermediateInstruction* Nop::copyFor(Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    return (new Nop(type))->copyExtrasFrom(this);
}

qpu_asm::DecoratedInstruction Nop::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    return qpu_asm::ALUInstruction(signal, UNPACK_NOP, PACK_NOP, COND_NEVER, COND_NEVER, SetFlag::DONT_SET,
        WriteSwap::DONT_SWAP, REG_NOP.num, REG_NOP.num, OP_NOP, OP_NOP, REG_NOP.num, REG_NOP.num, InputMultiplex::ACC0,
        InputMultiplex::ACC0, InputMultiplex::ACC0, InputMultiplex::ACC0);
}

bool Nop::isNormalized() const
{
    return true;
}

bool Nop::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherNop = dynamic_cast<const Nop*>(&other))
        return type == otherNop->type;
    return false;
}

Comparison::Comparison(std::string&& comp, Value&& dest, Value&& val0, Value&& val1) :
    IntrinsicOperation(std::move(comp), std::move(dest), std::move(val0), std::move(val1))
{
}

IntermediateInstruction* Comparison::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    return (new Comparison(std::string(opCode), renameValue(method, getOutput().value(), localPrefix, localMapping),
                renameValue(method, getFirstArg(), localPrefix, localMapping),
                renameValue(method, assertArgument(1), localPrefix, localMapping)))
        ->copyExtrasFrom(this);
}

CombinedOperation::CombinedOperation(Operation* op1, Operation* op2) :
    IntermediateInstruction(Optional<Value>{}), op1((op1 && op1->op.runsOnAddALU()) ? op1 : op2),
    op2((op1 && op1->op.runsOnAddALU()) ? op2 : op1)
{
    if(!op1 || !op2)
        throw CompilationError(CompilationStep::GENERAL, "Cannot combine NULL operation!");
    op1->parent = this;
    op2->parent = this;
}

FastMap<const Local*, LocalUse::Type> CombinedOperation::getUsedLocals() const
{
    FastMap<const Local*, LocalUse::Type> res =
        op1 != nullptr ? op1->getUsedLocals() : FastMap<const Local*, LocalUse::Type>{};
    const FastMap<const Local*, LocalUse::Type> tmp =
        op2 != nullptr ? op2->getUsedLocals() : FastMap<const Local*, LocalUse::Type>{};
    res.insert(tmp.begin(), tmp.end());
    return res;
}

void CombinedOperation::forUsedLocals(
    const std::function<void(const Local*, LocalUse::Type, const IntermediateInstruction&)>& consumer) const
{
    if(op1)
        op1->forUsedLocals(consumer);
    if(op2)
        op2->forUsedLocals(consumer);
}

bool CombinedOperation::readsLocal(const Local* local) const
{
    return (op1 && op1->readsLocal(local)) || (op2 && op2->readsLocal(local));
}

bool CombinedOperation::writesLocal(const Local* local) const
{
    return (op1 && op1->writesLocal(local)) || (op2 && op2->writesLocal(local));
}

void CombinedOperation::replaceLocal(const Local* oldLocal, const Local* newLocal, const LocalUse::Type type)
{
    op1->replaceLocal(oldLocal, newLocal, type);
    op2->replaceLocal(oldLocal, newLocal, type);
}

LCOV_EXCL_START
std::string CombinedOperation::to_string() const
{
    return (op1->to_string() + " and ") + op2->to_string();
}
LCOV_EXCL_STOP

qpu_asm::DecoratedInstruction CombinedOperation::convertToAsm(const FastMap<const Local*, Register>& registerMapping,
    const FastMap<const Local*, std::size_t>& labelMapping, const std::size_t instructionIndex) const
{
    if((getFirstOp()->op.runsOnAddALU() && getFirstOp()->op.runsOnMulALU()) ||
        (getSecondOP()->op.runsOnAddALU() && getSecondOP()->op.runsOnMulALU()))
        throw CompilationError(
            CompilationStep::CODE_GENERATION, "Cannot combine operations, where one can run on both ALUs", to_string());
    const IntermediateInstruction* addOp = getFirstOp()->op.runsOnAddALU() ? op1.get() : op2.get();
    const IntermediateInstruction* mulOp = !getFirstOp()->op.runsOnAddALU() ? op1.get() : op2.get();

    if(addOp == nullptr || mulOp == nullptr)
        throw CompilationError(
            CompilationStep::CODE_GENERATION, "One of the combined operations ins NULL", to_string());

    if(addOp->getOutput() && mulOp->getOutput() && addOp->getOutput().value() != mulOp->getOutput().value())
    {
        const Register addOut = addOp->getOutput()->checkLocal() ? registerMapping.at(addOp->getOutput()->local()) :
                                                                   addOp->getOutput()->reg();
        const Register mulOut = mulOp->getOutput()->checkLocal() ? registerMapping.at(mulOp->getOutput()->local()) :
                                                                   mulOp->getOutput()->reg();
        if(addOut != mulOut && !addOut.isAccumulator() && addOut.file == mulOut.file)
            throw CompilationError(CompilationStep::CODE_GENERATION,
                "Can't map outputs of a combined instruction to two distinct registers in the same file", to_string());
    }

    qpu_asm::ALUInstruction addInstr =
        *addOp->convertToAsm(registerMapping, labelMapping, instructionIndex).instruction.as<qpu_asm::ALUInstruction>();
    qpu_asm::ALUInstruction mulInstr =
        *mulOp->convertToAsm(registerMapping, labelMapping, instructionIndex).instruction.as<qpu_asm::ALUInstruction>();

    addInstr.setMulCondition(mulInstr.getMulCondition());
    addInstr.setMulMultiplexA(mulInstr.getMulMultiplexA());
    addInstr.setMulMultiplexB(mulInstr.getMulMultiplexB());
    addInstr.setMulOut(mulInstr.getMulOut());
    // an instruction writing to accumulator or register-file A does not set the swap, only an instruction writing to
    // file B does  so if any of the two instructions want to swap, we need to swap
    addInstr.setWriteSwap(add_flag(addInstr.getWriteSwap(), mulInstr.getWriteSwap()));
    addInstr.setMultiplication(mulInstr.getMultiplication());
    if(addInstr.getInputA() == REG_NOP.num)
        addInstr.setInputA(mulInstr.getInputA());
    if(addInstr.getInputB() == REG_NOP.num)
    {
        addInstr.setInputB(mulInstr.getInputB());
        // if ADD doesn't use register-file B and no small immediate, set signaling to value of MUL
        if(addInstr.getSig() == SIGNAL_NONE)
            addInstr.setSig(mulInstr.getSig());
    }
    if(addInstr.getUnpack().hasEffect() && mulInstr.getUnpack().hasEffect() &&
        addInstr.getUnpack() != mulInstr.getUnpack())
        throw CompilationError(
            CompilationStep::CODE_GENERATION, "Cannot combine two different unpack modes", to_string());
    if(!addInstr.getUnpack().hasEffect() && mulInstr.getUnpack().hasEffect())
        addInstr.setUnpack(mulInstr.getUnpack());

    if(addInstr.getPack().hasEffect() && mulInstr.getPack().hasEffect() && addInstr.getPack() != mulInstr.getPack())
        throw CompilationError(
            CompilationStep::CODE_GENERATION, "Cannot combine two different pack modes", to_string());
    if(!addInstr.getPack().hasEffect() && mulInstr.getPack().hasEffect())
        addInstr.setPack(mulInstr.getPack());

    if(mulInstr.getSetFlag() == SetFlag::SET_FLAGS && addInstr.getAddition() != OP_NOP.opAdd)
        throw CompilationError(CompilationStep::CODE_GENERATION,
            "Cannot set flag on multiplication ALU if addition ALU executes non-nop", to_string());

    return addInstr;
}

bool CombinedOperation::isNormalized() const
{
    return (!op1 || op1->isNormalized()) && (!op2 || op2->isNormalized());
}

SideEffectType CombinedOperation::getSideEffects() const
{
    return add_flag(
        op1 ? op1->getSideEffects() : SideEffectType::NONE, op2 ? op2->getSideEffects() : SideEffectType::NONE);
}

IntermediateInstruction* CombinedOperation::copyFor(
    Method& method, const std::string& localPrefix, InlineMapping& localMapping) const
{
    return (new CombinedOperation(dynamic_cast<Operation*>(op1->copyFor(method, localPrefix, localMapping)),
                dynamic_cast<Operation*>(op2->copyFor(method, localPrefix, localMapping))))
        ->copyExtrasFrom(this);
}

bool CombinedOperation::mapsToASMInstruction() const
{
    return (op1 && op1->mapsToASMInstruction()) || (op2 && op2->mapsToASMInstruction());
}

const Operation* CombinedOperation::getFirstOp() const
{
    return dynamic_cast<const Operation*>(op1.get());
}

const Operation* CombinedOperation::getSecondOP() const
{
    return dynamic_cast<const Operation*>(op2.get());
}

bool CombinedOperation::innerEquals(const IntermediateInstruction& other) const
{
    if(auto otherComb = dynamic_cast<const CombinedOperation*>(&other))
    {
        if(op1 && (!otherComb->op1 || *op1 != *otherComb->op1))
            return false;
        if(op2 && (!otherComb->op2 || *op2 != *otherComb->op2))
            return false;
    }
    return true;
}
