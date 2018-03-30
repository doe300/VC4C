/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SCANNER_H
#define SCANNER_H

#include "Token.h"

#include <iostream>
#include <string>
#include <utility>

namespace vc4c
{
    namespace llvm2qasm
    {
        class Scanner
        {
        public:
            explicit Scanner(std::istream& input = std::cin);
            Scanner(const Scanner& orig) = default;
            ~Scanner() = default;

            const Token peek();
            const Token pop();
            const Token readLine();

            bool hasInput();

            std::string getErrorPosition() const;

            unsigned int getLineNumber() const;

        private:
            unsigned int lineNumber;
            unsigned int rowNumber;

            std::istream& input;
            std::pair<bool, Token> lookAhead;

            const Token readToken();

            const Token readNumber();

            int skipChar();
        };
    } // namespace llvm2qasm
} // namespace vc4c
#endif /* SCANNER_H */
