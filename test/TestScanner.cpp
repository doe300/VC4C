/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include <string.h>

#include "TestScanner.h"

using namespace vc4c;
using namespace vc4c::llvm2qasm;

TestScanner::TestScanner()
{
    TEST_ADD(TestScanner::testEnd);
    TEST_ADD(TestScanner::testInteger);
    TEST_ADD(TestScanner::testFloat);
    TEST_ADD(TestScanner::testString);
    TEST_ADD(TestScanner::testBool);
}

void TestScanner::testEnd()
{
    std::stringstream stream;
    Scanner s(stream);
    
    TEST_ASSERT_EQUALS(TokenType::EMPTY, s.peek().type);
}

void TestScanner::testInteger()
{
    std::stringstream stream;
    Scanner s(stream);
    
    stream << "123 0x123 0123";
    
    TEST_ASSERT_EQUALS(TokenType::NUMBER, s.peek().type);
    TEST_ASSERT_EQUALS(123, s.pop().integer);
    
    TEST_ASSERT_EQUALS(TokenType::NUMBER, s.peek().type);
    TEST_ASSERT_EQUALS(0x123, s.pop().integer);
    
    TEST_ASSERT_EQUALS(TokenType::NUMBER, s.peek().type);
    TEST_ASSERT_EQUALS(0123, s.pop().integer);
}

void TestScanner::testFloat()
{
    std::stringstream stream;
    Scanner s(stream);
    
    stream << "-0.000000e+00  123.123 0x1p+8 0x47EFFFFFE0000000";
    
    TEST_ASSERT_EQUALS(TokenType::NUMBER, s.peek().type);
    TEST_ASSERT_EQUALS(0, s.pop().real);
    
    TEST_ASSERT_EQUALS(TokenType::NUMBER, s.peek().type);
    TEST_ASSERT_EQUALS(123.123, s.pop().real);
    
    TEST_ASSERT_EQUALS(TokenType::NUMBER, s.peek().type);
    TEST_ASSERT_EQUALS(1 << 8, s.pop().real);
    
    uint64_t dummy = 0x47EFFFFFE0000000L;
    TEST_ASSERT_EQUALS(TokenType::NUMBER, s.peek().type);
    TEST_ASSERT_EQUALS(*reinterpret_cast<double*>(&dummy), s.pop().integer);
}

void TestScanner::testString()
{
    std::stringstream stream;
    Scanner s(stream);
    
    stream << "apple.and.an_eye \"tree with space\" !\"stump is cut off\"";
    
    TEST_ASSERT_EQUALS(TokenType::STRING, s.peek().type);
    TEST_ASSERT(strcmp("apple.and.an_eye", s.pop().getText().get().data()) == 0);
    
    TEST_ASSERT_EQUALS(TokenType::STRING, s.peek().type);
    TEST_ASSERT(strcmp("\"tree with space\"", s.pop().getText().get().data()) == 0);
    
    TEST_ASSERT_EQUALS(TokenType::STRING, s.peek().type);
    TEST_ASSERT(strcmp("!\"stump is cut off\"", s.pop().getText().get().data()) == 0);
}

void TestScanner::testBool()
{
    std::stringstream stream;
    Scanner s(stream);
    
    stream << "true 1 0 false";
    
    TEST_ASSERT_EQUALS(TokenType::BOOLEAN, s.peek().type);
    TEST_ASSERT_EQUALS(true, s.pop().flag);
    
    TEST_ASSERT_EQUALS(TokenType::NUMBER, s.peek().type);
    TEST_ASSERT_EQUALS(true, s.pop().flag);
    
    TEST_ASSERT_EQUALS(TokenType::NUMBER, s.peek().type);
    TEST_ASSERT_EQUALS(false, s.pop().flag);
    
    TEST_ASSERT_EQUALS(TokenType::BOOLEAN, s.peek().type);
    TEST_ASSERT_EQUALS(false, s.pop().flag);
}
