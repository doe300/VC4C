/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */
#ifndef VC4C_TEST_REGISTER_FIXES_H
#define VC4C_TEST_REGISTER_FIXES_H

#include "cpptest.h"

#include "Precompiler.h"
#include "config.h"
#include "performance.h"

#include <string>

class TestRegisterFixes : public Test::Suite
{
public:
    TestRegisterFixes(const vc4c::Configuration& config);
    ~TestRegisterFixes() override;

    void testRegisterFix(std::string entryName, std::string stepName);
    void checkTestQuality();

private:
    vc4c::Configuration config;
    vc4c::SortedMap<std::string, std::pair<std::size_t, std::size_t>> fixupPasses;
    vc4c::FastMap<std::string, vc4c::CompilationData> precompilationCache;
};

#endif /* VC4C_TEST_REGISTER_FIXES_H */
