/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef PARSER_H
#define PARSER_H

#include "Module.h"

namespace vc4c
{
	class Parser
	{
	public:
		Parser()
		{

		}

		virtual ~Parser()
		{

		}

		virtual void parse(Module& module) = 0;
	};
}



#endif /* PARSER_H */

