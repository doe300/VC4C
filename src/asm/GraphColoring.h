/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef GRAPH_COLORING_H
#define GRAPH_COLORING_H

#include "../Graph.h"
#include "../InstructionWalker.h"
#include "../performance.h"

#include <bitset>

namespace vc4c
{
	namespace qpu_asm
	{
		enum class LocalRelation
		{
			// tow locals are in use at the same time, but do not block each other's register-file (only the specific register)
			USED_SIMULTANEOUSLY = 1,
			//two locals are in use (as inputs or outputs) by the same instructions, so they block each other's register-file (at least for physical files A and B)
			USED_TOGETHER = 2
		};

		struct LocalUsage
		{
			//the first use of the local
			InstructionWalker firstOccurrence;
			//the last use of the local
			InstructionWalker lastOccurrence;
			//the list of possible register-file this local can be on
			RegisterFile possibleFiles;
			//register files which are definitively blocked (e.g. by literals, fixed registers)
			RegisterFile blockedFiles;
			//list of iterators to the associated instructions
			//NOTE: this is the instruction-iterator, so it points to the combined operation not the single operation actually using the local
			FastSet<InstructionWalker> associatedInstructions;

			LocalUsage(InstructionWalker first, InstructionWalker last);
		};

		class ColoredNode : public Node<const Local*, LocalRelation>
		{
		public:
			explicit ColoredNode(const Local* local, RegisterFile possibleFiles = RegisterFile::ANY);

			/*!
			 * Blocks a single register or a whole register-file.
			 * Any blocked register (file) is no longer available to be used by this node
			 */
			void blockRegister(RegisterFile file, std::size_t index);
			bool hasFreeRegisters(RegisterFile file) const;
			std::size_t countFreeRegisters(RegisterFile file) const;
			void takeValues(const ColoredNode& other);

			/*
			 * \return The fixed register, this node has
			 */
			Register getRegisterFixed() const;

			/*!
			 * Fixes this node to a specific register by discarding all other possibilities.
			 * \return the register-index in the corresponding bit-set
			 */
			std::size_t fixToRegister();

			std::string to_string(bool longDescription = false) const;

			RegisterFile initialFile;
			RegisterFile possibleFiles;

		private:
			std::bitset<32> availableA = 0xFFFFFFFFUL;
			std::bitset<32> availableB = 0xFFFFFFFFUL;
			std::bitset<4> availableAcc = 0xFUL;
		};

		using ColoredGraph = Graph<const Local*, ColoredNode>;

		/*
		 * Graph coloring
		 * - create graph of locals used (mark in-the-same-instruction (A) and at-the-same time (B) separately) together
		 *   - also assign every local to closed-set (fixed to regA/regB) or open-set (else)
		 * - keep copy of original graph for future iterations (for conflict resolver)?
		 * - use with fixed locals to create colored graph (acc, regA, regB)
		 *   - fix parameters to non-accumulator
		 *   1) fixed locals
		 * 	 - keep open-set to track all locals not yet processed
		 * 	   - from all E in closed-set, update all neighbors with relation (A), remove E from closed-set
		 * 	   - if one neighbor becomes fixed to regA/regB, add to closed-set, remove from open-set
		 * 	   - until closed-set empty or local which cannot be assigned (-> abort)
		 * 	 2) "freely" assignable locals
		 * 	 - for all entries in open-set
		 * 	   - if element E has more than 4 (including self) neighbors which must be assigned to acc, abort
		 * 	   - if less then 4 neighbors (A/B) (including self) which can/must be on acc, assign all to acc, remove acc from all other neighbors (including self)
		 * 	   - if assignment is fixed, remove E from open-list
		 * - assign all locals to first free (neither used in A nor B relations) register on fixed register-file
		 *   - if there are no more registers for a file, abort
		 *
		 *
		 * - remove keeping track of all used register / locals used together
		 */
		class GraphColoring
		{
		public:
			/*!
			 * Initializes all internal data structures with a single iteration over all instructions
			 */
			GraphColoring(Method& method, InstructionWalker it);

			bool colorGraph();

			/*!
			 * \return Whether all errors have been fixed
			 */
			bool fixErrors();

			FastMap<const Local*, Register> toRegisterMap() const;
		private:
			Method& method;
			FastSet<const Local*> closedSet;
			FastSet<const Local*> openSet;
			FastMap<const Local*, LocalUsage> localUses;

			ColoredGraph graph;
			FastSet<const Local*> errorSet;

			void createGraph();
			void resetGraph();
		};
	} // namespace qpu_asm
} // namespace vc4c

#endif /* GRAPH_COLORING_H */
