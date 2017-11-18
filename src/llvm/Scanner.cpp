/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */


#include "Scanner.h"

#include <algorithm>
#include <cmath>
#include <cstring>

using namespace vc4c;
using namespace vc4c::llvm2qasm;

Scanner::Scanner(std::istream& input) : lineNumber(0), rowNumber(0), input(input), lookAhead(false,{})
{
}

const Token Scanner::peek()
{
    if (lookAhead.first) {
        return lookAhead.second;
    }
    const Token nextToken = readToken();
    if (nextToken.type != TokenType::EMPTY) {
        lookAhead.first = true;
        lookAhead.second = nextToken;
    }
    return nextToken;
}

const Token Scanner::pop()
{
    if (lookAhead.first) {
        lookAhead.first = false;
        return lookAhead.second;
    }
    return readToken();
}

bool Scanner::hasInput()
{
    return lookAhead.first || (input.peek() != std::istream::traits_type::eof() && input.peek() != '\0');
}

std::string Scanner::getErrorPosition() const
{
	std::array<char, 1024> buffer{};
    //TODO always points to the end of the error (since we read until there)
    sprintf(buffer.data(), "Error in line %u, row %u: ", lineNumber, rowNumber);
    return buffer.data();
}

unsigned int Scanner::getLineNumber() const
{
    return lineNumber;
}

int Scanner::skipChar()
{
    ++rowNumber;
    return input.get();
}

inline bool isStringCharacter(char c)
{
    return std::isalnum(c) || c == '@' || c == '%' || c == '"' || c == '_' || c == '.' || c == '#' || c == '/' || c == '*' || c == '!';
}

const Token Scanner::readToken()
{
    Token result{};
    //skip all leading white-spaces
    std::iostream::traits_type::int_type c;
    while (std::isspace(c = input.peek())) {
        skipChar();
        if(c == '\n')   //end statement on line break
        {
            ++lineNumber;
            rowNumber = 0;
            result.type = TokenType::END;
            return result;
        }
    }

    if(c == std::iostream::traits_type::eof() || c == '\0')
    {
        result.type = TokenType::EMPTY;
        return result;
    }
    else if(c == ';')    //skip comments
    {
        //skip complete line
        const Token t = readLine();
        if(t.to_string().find("<label>") != std::string::npos)
            return t;
        Token end{};
        end.type = TokenType::END;
        return end;
    }
    else if (std::isdigit(c))    //number -> integer or real
    {
        return readNumber();
    }
    else if (isStringCharacter(c))   //text -> text or bool
    {
        bool inStringLiteral = c == '"';
        std::array<char, TOKEN_BUFFER_SIZE> buffer{};
        buffer.fill(0);
        unsigned i = 0;
        for(; i < TOKEN_BUFFER_SIZE; ++i)
        {
            c = input.peek();
            if(!inStringLiteral && i == 1 && (buffer[0] == '!' || buffer[0] == 'c') && c == '"')
                //some strings in LLVM start with '!"', others (string-constants) with 'c"'
                inStringLiteral = true;
            if(inStringLiteral)
            {
                //end string literal only after next '"'
                //XXX improve by testing for \"
                //test to not read string '!"' for a string starting with '!"'
                if((buffer[0] == '"' ? i > 0 : i > 1) && c =='"')
                {
                    //include closing '"'
                    buffer.at(i) = static_cast<char>(skipChar());
                    ++i;
                    break;
                }
            }
            else if(!isStringCharacter(c) && c != '-')
            {
                break;
            }
            buffer.at(i) = static_cast<char>(skipChar());
        }
        if (strcasecmp("true", buffer.data()) == 0) // boolean true
        {
            result.type = TokenType::BOOLEAN;
            result.flag = true;
        }
        else if (strcasecmp("false", buffer.data()) == 0) // boolean false
        {
            result.type = TokenType::BOOLEAN;
            result.flag = false;
        }
        else // some other text
        {
            result.type = TokenType::STRING;
            memcpy(result.text.data(), buffer.data(), i);
        }
        return result;
    }
        //special character
    else {
        result.type = TokenType::STRING;
        //special treatment for '+' and '-' -> could start number
        if (c == '+' || c == '-') {
            skipChar();
            auto d = input.peek();
            if (std::isdigit(d)) {
                //start of number
                input.putback(c);
                return readNumber();
            }
            else if(d == c)     //++ or --
            {
                skipChar();
                result.text[0] = static_cast<char>(c);
                result.text[1] = static_cast<char>(c);
            }
            else //operator
            {
                result.text[0] = static_cast<char>(c);
            }
        }
            //other single character tokens
        else if (c == '(' || c == ')' || c == '*' || c == ':' || c == ',' || c == '[' || c == ']' 
                 || c == '=' || c == '{' || c == '}' || c == '<' || c == '>') {
            skipChar();
            result.text[0] = c;
        }
        return result;
    }
    throw CompilationError(CompilationStep::SCANNER, lineNumber, std::string("Invalid character:") + static_cast<char>(c));
}

const Token Scanner::readNumber()
{
    Token result{};
    std::string numberToken;
    while(std::isalnum(input.peek()) || input.peek() == '.' || input.peek() == '-' || input.peek() == '+')
    {
        ++rowNumber;
        numberToken.push_back(input.get());
    }
    result.type = TokenType::NUMBER;
    if(numberToken.find_first_of("e.p") != std::string::npos)
    {
        //floating literal
        result.real = std::strtod(numberToken.data(), nullptr);
    }
    else
    {
        //integer literal
        result.integer = std::strtoll(numberToken.data(), nullptr, 0 /* let method decide */);
    }
    //so our index is correct again
    result.type = TokenType::NUMBER;
    return result;
}

const Token Scanner::readLine()
{
    std::iostream::traits_type::int_type c;
    Token line{};
    line.type = TokenType::STRING;
    memset(line.text.data(), '\0', TOKEN_BUFFER_SIZE);
    std::size_t i = 0;
    while ((c = input.peek()) != std::iostream::traits_type::eof() && c != '\0')
    {
        if(c == '\n')   //end statement on line break
        {
            ++lineNumber;
            rowNumber = 0;
            break;
        }
        line.text.at(i++) = skipChar();
    }
    return line;
}
