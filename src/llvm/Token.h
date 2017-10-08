/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef TOKEN_H
#define TOKEN_H

#include "CompilationError.h"
#include "helper.h"

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
						return text;
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
				return type == TokenType::STRING && val.compare(text) == 0;
			}

			bool hasValue(const char val) const
			{
				return type == TokenType::STRING && *text == val;
			}

			Optional<std::string> getText() const
			{
				if (type == TokenType::STRING)
					return std::string(text);
				return
				{};
			}
		private:
			char text[TOKEN_BUFFER_SIZE];

			friend class Scanner;
			friend class IRParser;
		};
	}
}

#endif /* TOKEN_H */

