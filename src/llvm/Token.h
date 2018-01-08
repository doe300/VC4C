/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TOKEN_H
#define TOKEN_H

#include "CompilationError.h"
#include "helper.h"

#include <array>

namespace vc4c
{

	namespace llvm2qasm
	{
		static constexpr unsigned int TOKEN_BUFFER_SIZE { 2048 };

		enum class TokenType
			: unsigned char
			{
				EMPTY, NUMBER, STRING, BOOLEAN, END
		};

		class Token
		{
		public:
			TokenType type;

			union
			{
				int64_t integer;
				double real;
				bool flag;
			};

			bool isEnd() const
			{
				return type == TokenType::END || type == TokenType::EMPTY;
			}

			friend std::ostream& operator<<(std::ostream& stream, const Token& token)
			{
				return stream << token.to_string();
			}

			const std::string to_string() const
			{
				switch (type)
				{
					case TokenType::BOOLEAN:
						return (flag ? "true" : "false");
					case TokenType::NUMBER:
						return std::to_string(integer);
					case TokenType::STRING:
						return text.data();
					case TokenType::EMPTY:
						return "(empty)";
					case TokenType::END:
						return "(end)";
					default:
						throw CompilationError(CompilationStep::SCANNER, "Invalid type!");
				}
			}

			bool hasValue(const std::string& val) const
			{
				return type == TokenType::STRING && val.compare(text.data()) == 0;
			}

			bool hasValue(char val) const
			{
				return type == TokenType::STRING && text[0] == val;
			}

			Optional<std::string> getText() const
			{
				if (type == TokenType::STRING)
					return std::string(text.data());
				return {};
			}
		private:
			std::array<char, TOKEN_BUFFER_SIZE> text;

			friend class Scanner;
			friend class IRParser;
		};
	} // namespace llvm2qasm
} // namespace vc4c

#endif /* TOKEN_H */

