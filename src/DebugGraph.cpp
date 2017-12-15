/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "DebugGraph.h"

#ifdef DEBUG_MODE

using namespace vc4c;

static const std::string STYLE_EDGE_STRONG = "";
static const std::string STYLE_EDGE_WEAK = "style=\"dashed\"";

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

static std::string toEdgeAttributes(bool weakEdge, const std::string& label)
{
	std::vector<std::string> attrs;
	if(weakEdge)
		attrs.emplace_back(STYLE_EDGE_WEAK);
	if(!label.empty())
		attrs.emplace_back(std::string("label=\"") + label + "\"");
	if(attrs.empty())
		return "";
	return std::string("[") + to_string<std::string>(attrs) + "]";
}

void vc4c::printEdge(std::ofstream& file, const std::string& name1, const std::string& name2, bool weakEdge, bool isDirected, const std::string& edgeLabel)
{
	file << cleanName(name1) << createEdge(isDirected) << cleanName(name2) << toEdgeAttributes(weakEdge, edgeLabel) << ";" << std::endl;
}

#endif
