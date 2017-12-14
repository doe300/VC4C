/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "DebugGraph.h"

#ifdef DEBUG_MODE

using namespace vc4c;

static const std::string STYLE_EDGE_STRONG = "";
static const std::string STYLE_EDGE_WEAK = "[style=\"dashed\"]";

static std::string cleanName(const std::string& name)
{
	std::string copy(name);
	while(copy.find('%') != std::string::npos)
	{
		copy = copy.replace(copy.find('%'), 1, "");
	}
	while(copy.find('.') != std::string::npos)
	{
		copy = copy.replace(copy.find('.'), 1, "_");
	}
	return std::string("\"") + copy + "\"";
}

static std::string createEdge(bool isDirected)
{
	return isDirected ? " -> " : " -- ";
}

void vc4c::printEdge(std::ofstream& file, const std::string& name1, const std::string& name2, bool weakEdge, bool isDirected)
{
	file << cleanName(name1) << createEdge(isDirected) << cleanName(name2) << (weakEdge ? STYLE_EDGE_STRONG : STYLE_EDGE_WEAK) << ";" << std::endl;
}

#endif
