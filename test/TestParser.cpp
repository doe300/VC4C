/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestParser.h"
#include "../lib/cpplog/include/log.h"
#include "../lib/cpplog/include/logger.h"

#include <fstream>

using namespace vc4c;

static std::ifstream source1("./example/hello_world_vector.ir");
static std::ifstream source2("./example/hello_world_vector_rpi.ir");

TestParser::TestParser() : parser1(source1), parser2(source2)
{
    TEST_ADD(TestParser::testKernelMethod);
    TEST_ADD(TestParser::testGlobalData);
    TEST_ADD(TestParser::testStructDefinition);
    TEST_ADD(TestParser::testUnionDefinition);
}

bool TestParser::setup()
{
//    parser1.parse();
//    parser2.parse();
    
    return true;
}

void TestParser::testKernelMethod()
{
//    TEST_ASSERT_EQUALS(1, parser1.getKernelFunctions().size());
//    TEST_ASSERT_EQUALS(1, parser2.getKernelFunctions().size());
//
//    const Method* kernel1 = parser1.getKernelFunctions()[0];
//    const Method* kernel2 = parser2.getKernelFunctions()[0];
//    TEST_ASSERT_EQUALS("@hello_world", kernel1->name);
//    TEST_ASSERT_EQUALS("@hello_world", kernel2->name);
//    TEST_ASSERT_EQUALS(TYPE_VOID.typeName, kernel1->returnType.typeName);
//    TEST_ASSERT_EQUALS(TYPE_VOID.typeName, kernel2->returnType.typeName);
//    TEST_ASSERT_EQUALS(2, kernel1->parameters.size());
//    TEST_ASSERT_EQUALS(2, kernel2->parameters.size());
//
//    TEST_ASSERT_EQUALS(2, kernel1->metaData.at(MetaDataType::ARG_ACCESS_QUALIFIERS).size());
//    TEST_ASSERT_EQUALS(2, kernel2->metaData.at(MetaDataType::ARG_ACCESS_QUALIFIERS).size());
//
//    TEST_ASSERT_EQUALS("none", kernel1->metaData.at(MetaDataType::ARG_ACCESS_QUALIFIERS)[0]);
//    TEST_ASSERT_EQUALS("none", kernel2->metaData.at(MetaDataType::ARG_ACCESS_QUALIFIERS)[0]);
//    TEST_ASSERT_EQUALS("char16*", kernel1->metaData.at(MetaDataType::ARG_TYPE_NAMES)[0]);
//    TEST_ASSERT_EQUALS("char16*", kernel2->metaData.at(MetaDataType::ARG_TYPE_NAMES)[0]);
//
//    TEST_ASSERT_EQUALS("const", kernel1->metaData.at(MetaDataType::ARG_TYPE_QUALIFIERS)[0]);
//    TEST_ASSERT_EQUALS("const", kernel2->metaData.at(MetaDataType::ARG_TYPE_QUALIFIERS)[0]);
//    TEST_ASSERT(kernel1->metaData.at(MetaDataType::ARG_TYPE_QUALIFIERS)[1].empty());
//    TEST_ASSERT(kernel2->metaData.at(MetaDataType::ARG_TYPE_QUALIFIERS)[1].empty());
}

void TestParser::testGlobalData()
{

}

void TestParser::testStructDefinition()
{

}

void TestParser::testUnionDefinition()
{

}
