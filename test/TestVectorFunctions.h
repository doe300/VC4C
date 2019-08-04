/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_VECTOR_FUNCTIONS_H
#define VC4C_TEST_VECTOR_FUNCTIONS_H

#include "cpptest.h"

#include "config.h"

class TestVectorFunctions : public Test::Suite
{
public:
    TestVectorFunctions(const vc4c::Configuration& config = {});
    ~TestVectorFunctions() override;

    void testVectorLoad2Int();
    void testVectorLoad2Short();
    void testVectorLoad2Char();
    void testVectorLoad3Int();
    void testVectorLoad3Short();
    void testVectorLoad3Char();
    void testVectorLoad4Int();
    void testVectorLoad4Short();
    void testVectorLoad4Char();
    void testVectorLoad8Int();
    void testVectorLoad8Short();
    void testVectorLoad8Char();
    void testVectorLoad16Int();
    void testVectorLoad16Short();
    void testVectorLoad16Char();

    void testVectorStore2Int();
    void testVectorStore2Short();
    void testVectorStore2Char();
    void testVectorStore3Int();
    void testVectorStore3Short();
    void testVectorStore3Char();
    void testVectorStore4Int();
    void testVectorStore4Short();
    void testVectorStore4Char();
    void testVectorStore8Int();
    void testVectorStore8Short();
    void testVectorStore8Char();
    void testVectorStore16Int();
    void testVectorStore16Short();
    void testVectorStore16Char();

    void testVectorLoad3IntUneven();
    void testVectorLoad3ShortUneven();
    void testVectorLoad3CharUneven();
    void testVectorStore3IntUneven();
    void testVectorStore3ShortUneven();
    void testVectorStore3CharUneven();

    void testShuffleVector2();
    void testShuffleVector4();
    void testShuffleVector8();
    void testShuffleVector16();
    void testShuffle2Vector2();
    void testShuffle2Vector4();
    void testShuffle2Vector8();
    void testShuffle2Vector16();

    void testVectorReorder2();
    void testVectorReorder4();
    void testVectorReorder8();
    void testVectorReorder16();
    void testExtractElement();
    void testInsertElement();

    void testVectorAssembly16(std::string source);
    void testVectorAssembly8(std::string source);
    void testVectorAssembly4(std::string source);
    void testVectorAssembly2(std::string source);
    void testVectorAssemblyScalar(std::string source);

private:
    vc4c::Configuration config;

    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_VECTOR_FUNCTIONS_H */
