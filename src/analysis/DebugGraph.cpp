/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "DebugGraph.h"

#include "../helper.h"

#ifdef DEBUG_MODE
LCOV_EXCL_START

using namespace vc4c;

static const std::string STYLE_EDGE_STRONG{};
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

static std::string createEdge(Direction direction)
{
    return direction == Direction::NONE ? " -- " : " -> ";
}

static std::string toEdgeAttributes(bool weakEdge, const std::string& label, Direction direction)
{
    std::vector<std::string> attrs;
    if(weakEdge)
        attrs.emplace_back(STYLE_EDGE_WEAK);
    if(!label.empty())
        attrs.emplace_back(std::string("label=\"") + label + "\"");
    if(direction == Direction::BOTH)
        attrs.emplace_back("dir=\"both\"");
    if(attrs.empty())
        return "";
    return std::string("[") + to_string<std::string>(attrs) + "]";
}

void vc4c::printEdge(
    std::ofstream& file, uintptr_t id1, uintptr_t id2, bool weakEdge, Direction direction, const std::string& edgeLabel)
{
    file << id1 << createEdge(direction) << id2 << toEdgeAttributes(weakEdge, edgeLabel, direction) << ";" << std::endl;
}

void vc4c::printNode(std::ofstream& file, uintptr_t ID, const std::string& name)
{
    file << ID << " [label=" << cleanName(name) << "];" << std::endl;
}
LCOV_EXCL_STOP
#endif
