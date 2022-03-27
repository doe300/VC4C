// This file is auto generated from code run on the actual hardware
#include <cstdint>
#include <vector>

struct UnpackTestEntry { int32_t arg; int32_t result;
    int32_t result16BitASignExtended; int32_t result16BitBSignExtended; int32_t result8BitReplicated;
    int32_t result8BitAZeroExtended; int32_t result8BitBZeroExtended; int32_t result8BitCZeroExtended; int32_t result8BitDZeroExtended;    int32_t result16BitAFloatExtended; int32_t result16BitBFloatExtended; int32_t result8BitFloatReplicated;
    int32_t result8BitAFloatExtended; int32_t result8BitBFloatExtended; int32_t result8BitCFloatExtended; int32_t result8BitDFloatExtended; };

static const std::vector<UnpackTestEntry> TEST_UNPACK = {
    UnpackTestEntry{-2147483648, -2147483648, 0, -32768, -2139062144, 0, 0, 0, 128, 0, -2147483648, -2139062144, 0, 0, 0, 1056997504},
    UnpackTestEntry{2147483647, 2147483647, -1, 32767, 2139062143, 255, 255, 255, 127, -8192, 2147475456, 2139062143, 1065353216, 1065353216, 1065353216, 1056898814},
    UnpackTestEntry{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    UnpackTestEntry{-1, -1, -1, -1, -1, 255, 255, 255, 255, -8192, -8192, -1, 1065353216, 1065353216, 1065353216, 1065353216},
    UnpackTestEntry{1, 1, 1, 0, 0, 1, 0, 0, 0, 8192, 0, 0, 998277248, 0, 0, 0},
    UnpackTestEntry{1879048192, 1879048192, 0, 28672, 1886417008, 0, 0, 0, 112, 0, 1174405120, 1886417008, 0, 0, 0, 1054925024},
    UnpackTestEntry{-1879048192, -1879048192, 0, -28672, -1869574000, 0, 0, 0, 144, 0, -1174405120, -1869574000, 0, 0, 0, 1058050192},
    UnpackTestEntry{32767, 32767, 32767, 0, 0, 255, 127, 0, 0, 2147475456, 0, 0, 1065353216, 1056898814, 0, 0},
    UnpackTestEntry{32768, 32768, -32768, 0, 0, 0, 128, 0, 0, -2147483648, 0, 0, 0, 1056997504, 0, 0},
    UnpackTestEntry{127, 127, 127, 0, 0, 127, 0, 0, 0, 1040384, 0, 0, 1056898814, 0, 0, 0},
    UnpackTestEntry{128, 128, 128, 0, 0, 128, 0, 0, 0, 1048576, 0, 0, 1056997504, 0, 0, 0},
    UnpackTestEntry{1065353216, 1065353216, 0, 16256, 1061109567, 0, 0, 128, 63, 0, 1072693248, 1061109567, 0, 0, 1056997504, 1048378620},
    UnpackTestEntry{-1082130432, -1082130432, 0, -16512, -1077952577, 0, 0, 128, 191, 0, -1074790400, -1077952577, 0, 0, 1056997504, 1061142463},
    UnpackTestEntry{1073741824, 1073741824, 0, 16384, 1077952576, 0, 0, 0, 64, 0, 1073741824, 1077952576, 0, 0, 0, 1048608896},
    UnpackTestEntry{-268435456, -268435456, 0, -4096, -252645136, 0, 0, 0, 240, 0, -973078528, -252645136, 0, 0, 0, 1064366320},
};

