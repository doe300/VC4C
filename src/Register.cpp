/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Register.h"

#include "Values.h"

#include <iomanip>
#include <sstream>

using namespace vc4c;

Literal BitMask::operator()(Literal newValue, Literal oldValue) const noexcept
{
    return Literal((newValue.unsignedInt() & value) | (oldValue.unsignedInt() & ~value));
}

LCOV_EXCL_START
std::string BitMask::to_string() const
{
    std::ostringstream ss;
    ss << std::setfill('0') << std::setw(8) << std::hex << value;
    return ss.str();
}

std::string vc4c::toString(const RegisterFile file)
{
    std::string fileName;
    if(file == RegisterFile::ANY)
        return "any";
    if(file == RegisterFile::NONE)
        return "none";
    if(has_flag(file, RegisterFile::ACCUMULATOR))
        fileName.append("acc");
    if(has_flag(file, RegisterFile::PHYSICAL_A))
        fileName.append(fileName.empty() ? "" : ",").append("A");
    if(has_flag(file, RegisterFile::PHYSICAL_B))
        fileName.append(fileName.empty() ? "" : ",").append("B");

    return fileName;
}
LCOV_EXCL_STOP

bool vc4c::isFixed(const RegisterFile file) noexcept
{
    return file == RegisterFile::ACCUMULATOR || file == RegisterFile::PHYSICAL_A || file == RegisterFile::PHYSICAL_B;
}

bool Register::isGeneralPurpose() const noexcept
{
    return num < 32;
}

LCOV_EXCL_START
std::string Register::to_string(bool specialNames, bool readAccess) const
{
    if(specialNames)
    {
        if(readAccess && file != RegisterFile::ACCUMULATOR)
        {
            if(num == 32)
                return "unif";
            if(num == 35)
                return "varying";
            if(num == 36)
                return "sfu_tmu_out";
            if(num == 37)
                return "rep";
            if(num == 38)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "elem_num";
                if(file == RegisterFile::PHYSICAL_B)
                    return "qpu_num";
            }
            if(num == 39)
                return "-";
            if(num == 41)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "x_coord";
                if(file == RegisterFile::PHYSICAL_B)
                    return "y_coord";
            }
            if(num == 42)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "ms_mask";
                if(file == RegisterFile::PHYSICAL_B)
                    return "rev_flag";
            }
            if(num == 48)
                return "vpm";
            if(num == 49)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "vpr_busy";
                if(file == RegisterFile::PHYSICAL_B)
                    return "vpw_busy";
            }
            if(num == 50)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "vpr_wait";
                if(file == RegisterFile::PHYSICAL_B)
                    return "vpw_wait";
            }
            if(num == 51)
                return "mutex_acq";
        }
        else
        {
            if(num == 36 && file != RegisterFile::ACCUMULATOR)
                return "tmu_noswap";
            if(num == 37)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "rep_quad|r5";
                if(file == RegisterFile::PHYSICAL_B)
                    return "rep_all|r5";
            }
            if(num == 38)
                return "irq";
            if(num == 39)
                return "-";
            if(num == 40)
                return "unif_addr";
            if(num == 42)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "ms_mask";
                if(file == RegisterFile::PHYSICAL_B)
                    return "rev_flag";
            }
            if(num == 48)
                return "vpm";
            if(num == 49)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "vpr_setup";
                if(file == RegisterFile::PHYSICAL_B)
                    return "vpw_setup";
            }
            if(num == 50)
            {
                if(file == RegisterFile::PHYSICAL_A)
                    return "vpr_addr";
                if(file == RegisterFile::PHYSICAL_B)
                    return "vpw_addr";
            }
            if(num == 51)
                return "mutex_rel";
            if(num == 52)
                return "sfu_recip";
            if(num == 53)
                return "sfu_rsqrt";
            if(num == 54)
                return "sfu_exp";
            if(num == 55)
                return "sfu_log";
            if(num == 56)
                return "tmu0s";
            if(num == 57)
                return "tmu0t";
            if(num == 58)
                return "tmu0r";
            if(num == 59)
                return "tmu0b";
            if(num == 60)
                return "tmu1s";
            if(num == 61)
                return "tmu1t";
            if(num == 62)
                return "tmu1r";
            if(num == 63)
                return "tmu1b";
        }
        if(getAccumulatorNumber() != INVALID_ACCUMULATOR)
        {
            return std::string("r") + std::to_string(getAccumulatorNumber());
        }
    }
    return std::string(file == RegisterFile::PHYSICAL_A ? "ra" : (file == RegisterFile::PHYSICAL_B ? "rb" : "rx")) +
        std::to_string(num);
}
LCOV_EXCL_STOP

int Register::getAccumulatorNumber() const noexcept
{
    switch(num)
    {
    case 32:
        return 0;
    case 33:
        return 1;
    case 34:
        return 2;
    case 35:
        return 3;
    case 36:
        return 4;
    case 37:
        return 5;
    default:
        return -1;
    }
}

bool Register::operator<(Register right) const noexcept
{
    if(static_cast<unsigned char>(file) < static_cast<unsigned char>(right.file))
        return true;
    if(file == right.file)
        return num < right.num;
    return false;
}

bool Register::operator==(Register right) const noexcept
{
    if(this == &right)
        return true;
    return num == right.num && file == right.file;
}

bool Register::isAccumulator() const noexcept
{
    return (num >= 32 && num <= 37) || file == RegisterFile::ACCUMULATOR;
}

bool Register::isTileBuffer() const noexcept
{
    return num >= 43 && num <= 47;
}

bool Register::isVertexPipelineMemory() const noexcept
{
    return num >= 48 && num <= 50;
}

bool Register::isSpecialFunctionsUnit() const noexcept
{
    return num >= 52 && num <= 55;
}

bool Register::isTextureMemoryUnit() const noexcept
{
    return num >= 56;
}

bool Register::hasSideEffectsOnRead() const noexcept
{
    if(!isReadable())
        return false;
    if(num == 32 || num == 35 || num == 36) /* UNIFORM, VARYING and SFU/TMU read */
        return true;
    if(num >= 48 && num <= 50) /* VPM read, busy, wait */
        return true;
    if(num == 51) /* mutex acquire */
        return true;
    return false;
}

bool Register::hasSideEffectsOnWrite() const noexcept
{
    if(!isWriteable())
        return false;
    if(num == 36) /* TMU noswap */
        return true;
    if(num == 38) /* host interrupt */
        return true;
    if(num == 40) /* UNIFORM address */
        return true;
    if(num >= 41 && num <= 47) /* Tile buffer setup and value writes */
        return true;
    if(num >= 48 && num <= 50) /* VPM setup */
        return true;
    if(num == 51) /* mutex release */
        return true;
    if(num >= 52 && num <= 55) /* SFU calls */
        return true;
    if(num >= 56 && num <= 63) /* TMU setup */
        return true;
    return false;
}

bool Register::isReadable() const noexcept
{
    if(num == 40 /* UNIFORM address */ || (num >= 43 && num <= 47) /* TLB setup */ || num >= 52 /* SFU, TMU write */)
        return false;
    return true;
}

bool Register::isWriteable() const noexcept
{
    return true;
}

bool Register::triggersReadOfR4() const noexcept
{
    return isSpecialFunctionsUnit();
    // TODO TMU S coordinates trigger the loading/processing of the (texture) value, but the signal  is needed to
    // load it into r4
    // || (num == 56 || num == 60) /* TMU S coordinates */;
}

bool Register::isUnsignedInteger() const noexcept
{
    return *this == REG_QPU_NUMBER || *this == REG_ELEMENT_NUMBER || *this == REG_X_COORDS || *this == REG_Y_COORDS ||
        *this == REG_REV_FLAG || *this == REG_MS_MASK || *this == REG_MUTEX;
}

BitMask Register::getReadMask() const noexcept
{
    if(*this == REG_ELEMENT_NUMBER || *this == REG_QPU_NUMBER || *this == REG_MS_MASK)
        // values 0 to 15 or 0 to 12 respectively
        return BitMask{0x0000000F};
    if(*this == REG_X_COORDS || *this == REG_Y_COORDS || *this == REG_REV_FLAG)
        return BitMask{0x00000001};

    return BITMASK_ALL;
}

BitMask Register::getWriteMask() const noexcept
{
    if(*this == REG_MS_MASK)
        return BitMask{0x0000000F};
    if(*this == REG_REV_FLAG)
        return BitMask{0x00000001};

    return BITMASK_ALL;
}
