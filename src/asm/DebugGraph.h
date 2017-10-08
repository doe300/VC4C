/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef DEBUG_GRAPH_H
#define DEBUG_GRAPH_H

#include <fstream>
#include <functional>

#include "GraphColoring.h"

namespace vc4c
{
	namespace qpu_asm
	{

		void printEdge(std::ofstream& file, const std::string& name1, const std::string& name2, bool weakEdge, bool isDirected);

		/*
		 * Generates a Graphviz (http://graphviz.org/) graph out of the colored graph/basic block graph
		 *
		 * Generate SVG with: "sfdp -Tsvg <input>.dot -o <output>.svg"
		 */
		template<typename T, typename R>
		class DebugGraph
		{
		public:
			DebugGraph(const std::string& fileName, bool isDirected = false) : isDirected(isDirected), file(fileName)
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
			void addNodeWithNeighbors(const NodeType& node, const std::function<std::string(const T&)> nameFunc, const std::function<bool(const R&)> weakEdgeFunc = [](const R& r) -> bool{return false;})
			{
				processedNodes.emplace(reinterpret_cast<const Node<T, R>*>(&node));
				for(const auto& pair : node.getNeighbors())
				{
					//making sure, every edge is printed just once is not necessary because of the "strict" keyword,
					//but it saves a lot of processing time in Graphviz
					if(processedNodes.find(reinterpret_cast<const Node<T, R>*>(pair.first)) != processedNodes.end())
						return;
					printEdge(file, nameFunc(const_cast<T&>(node.key)), nameFunc(const_cast<T&>(pair.first->key)), weakEdgeFunc(pair.second), isDirected);
				}
			}

		private:
			const bool isDirected;
			std::ofstream file;
			FastSet<const Node<T, R>*> processedNodes;
		};

	} /* namespace qpu_asm */
} /* namespace vc4c */

#endif /* DEBUG_GRAPH_H */
