/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef DEBUG_GRAPH_H
#define DEBUG_GRAPH_H

#include "Graph.h"

#include <fstream>
#include <functional>

#ifdef DEBUG_MODE

namespace vc4c
{
	void printEdge(std::ofstream& file, const std::string& name1, const std::string& name2, bool weakEdge, bool isDirected, const std::string& edgeLabel);

	/*
	 * Generates a Graphviz (http://graphviz.org/) graph out of the colored graph/basic block graph
	 *
	 * Generate SVG with: "sfdp -Tsvg <input>.dot -o <output>.svg"
	 */
	template<typename T, typename R>
	class DebugGraph
	{
	public:

		template<typename U>
		using NameFunc = std::function<std::string(const U&)>;

		explicit DebugGraph(const std::string& fileName, bool isDirected = false) : isDirected(isDirected), file(fileName)
		{
			//strict: at most one edge can connect two nodes, multiple same connections are merged (including their attributes)
			//graph: undirected graph, digraph: directed graph
			if(isDirected)
				file << "strict digraph {" << std::endl;
			else
				file << "strict graph {" << std::endl;
			//global graph settings: draw edges as splines and remove overlap between edges and nodes, draw nodes over edges
			file << "graph [splines=true, overlap=\"prism\", outputorder=\"edgesfirst\"];" << std::endl;
			//fill background of nodes white
			file << "node [style=\"filled\", fillcolor=\"white\"];" << std::endl;
		}

		~DebugGraph()
		{
			file << "}" << std::endl;
			file.flush();
			file.close();
		}

		template<typename NodeType = Node<T, R>>
		void addNodeWithNeighbors(const NodeType& node, const NameFunc<T>& nameFunc = NodeType::to_string, const std::function<bool(const R&)>& weakEdgeFunc = [](const R& r) -> bool{return false;}, const NameFunc<R>& edgeLabelFunc = [](const R& r) -> std::string{ return "";})
		{
			processedNodes.emplace(reinterpret_cast<const Node<T, R>*>(&node));
			for(const auto& pair : node.getNeighbors())
			{
				//making sure every edge is printed just once is not necessary because of the "strict" keyword,
				//but it saves a lot of processing time in Graphviz
				if(!isDirected && processedNodes.find(reinterpret_cast<const Node<T, R>*>(pair.first)) != processedNodes.end())
					return;
				printEdge(file, nameFunc(const_cast<T&>(node.key)), nameFunc(const_cast<T&>(pair.first->key)), weakEdgeFunc(pair.second), isDirected, edgeLabelFunc(pair.second));
			}
		}

		template<typename G = Graph<T, Node<T, R>>>
		static void dumpGraph(const G& graph, const std::string& fileName, bool isDirected, const NameFunc<T>& nameFunc = Node<T, R>::to_string, const std::function<bool(const R&)>& weakEdgeFunc = [](const R& r) -> bool{return false;}, const NameFunc<R>& edgeLabelFunc = [](const R& r) -> std::string{ return "";})
		{
			DebugGraph<T, R> debugGraph(fileName, isDirected);
			for(const auto& node : graph)
			{
				debugGraph.addNodeWithNeighbors(node.second, nameFunc, weakEdgeFunc, edgeLabelFunc);
			}
		}

	private:
		const bool isDirected;
		std::ofstream file;
		FastSet<const Node<T, R>*> processedNodes;
	};
} /* namespace vc4c */
#endif /* DEBUG_MODE */
#endif /* DEBUG_GRAPH_H */
