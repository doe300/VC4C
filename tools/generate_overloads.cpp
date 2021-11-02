#include <cstdint>
#include <iostream>
#include <string>
#include <vector>

static std::vector<std::string> spaceNames = {"", "__global", "", "__local"};

int main(int argc, char** argv)
{
    std::vector<std::pair<std::string, std::string>> types = {
        {"c", "char"},
        {"h", "uchar"},
        {"i", "int"},
        {"j", "uint"},
        {"s", "short"},
        {"t", "ushort"},
        {"f", "float"},
        {"l", "long"},
        {"m", "ulong"},
    };

    std::vector<std::pair<unsigned, unsigned>> addressSpaces = {
        {1, 3}, // global to local
        {3, 1}, // local to global
    };

    std::vector<std::pair<std::string, std::string>> vectors = {
        {"", ""},
        {"2", "Dv2_"},
        {"3", "Dv3_"},
        {"4", "Dv4_"},
        {"8", "Dv8_"},
        {"16", "Dv16_"},
    };

    for(auto type : types)
    {
        for(auto space : addressSpaces)
        {
            for(auto vector : vectors)
            {
                std::cout << "event_t _Z21async_work_group_copyPU3AS" << space.first << vector.second << type.first
                          << "PKU3AS" << space.second << vector.second << type.first << "j9ocl_event("
                          << spaceNames[space.first] << " " << type.second << vector.first << " *, const "
                          << spaceNames[space.second] << " " << type.second << vector.first
                          << " *, size_t, event_t) __attribute((weak, "
                             "alias(\"_Z21async_work_group_copyPU3AS"
                          << space.first << vector.second << type.first << "PU3AS" << space.second << "K"
                          << (vector.second.empty() ? type.first : "S_") << "j9ocl_event\")));" << std::endl;
                std::cout << "event_t _Z21async_work_group_copyPU3AS" << space.first << vector.second << type.first
                          << "PKU3AS" << space.second << "S_j9ocl_event(" << spaceNames[space.first] << " "
                          << type.second << vector.first << " *, const " << spaceNames[space.second] << " "
                          << type.second << vector.first
                          << " *, size_t, event_t) __attribute((weak, "
                             "alias(\"_Z21async_work_group_copyPU3AS"
                          << space.first << vector.second << type.first << "PU3AS" << space.second << "K"
                          << (vector.second.empty() ? type.first : "S_") << "j9ocl_event\")));" << std::endl;
            }
        }
    }
    for(auto type : types)
    {
        for(auto space : addressSpaces)
        {
            for(auto vector : vectors)
            {
                std::cout << "event_t _Z29async_work_group_strided_copyPU3AS" << space.first << vector.second
                          << type.first << "PKU3AS" << space.second << vector.second << type.first << "jj9ocl_event("
                          << spaceNames[space.first] << " " << type.second << vector.first << " *, const "
                          << spaceNames[space.second] << " " << type.second << vector.first
                          << " *, size_t, size_t, event_t) __attribute((weak, "
                             "alias(\"_Z29async_work_group_strided_copyPU3AS"
                          << space.first << vector.second << type.first << "PU3AS" << space.second << "K"
                          << (vector.second.empty() ? type.first : "S_") << "jj9ocl_event\")));" << std::endl;
                std::cout << "event_t _Z29async_work_group_strided_copyPU3AS" << space.first << vector.second
                          << type.first << "PKU3AS" << space.second << "S_jj9ocl_event(" << spaceNames[space.first]
                          << " " << type.second << vector.first << " *, const " << spaceNames[space.second] << " "
                          << type.second << vector.first
                          << " *, size_t, size_t, event_t) __attribute((weak, "
                             "alias(\"_Z29async_work_group_strided_copyPU3AS"
                          << space.first << vector.second << type.first << "PU3AS" << space.second << "K"
                          << (vector.second.empty() ? type.first : "S_") << "jj9ocl_event\")));" << std::endl;
            }
        }
    }

    return EXIT_SUCCESS;
}