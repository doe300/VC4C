/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_METHOD_H
#define VC4C_METHOD_H

#include "BasicBlock.h"
#include "KernelMetaData.h"

namespace vc4c
{
	namespace periphery
	{
		class VPM;
	} // namespace periphery
	class ControlFlowGraph;
	class Module;

	/*
	 * Base class representing a function (e.g. an OpenCL kernel)
	 */
	class Method : private NonCopyable
	{
		using BasicBlockList = RandomModificationList<BasicBlock>;

	public:
		static const std::string WORK_DIMENSIONS;
		static const std::string LOCAL_SIZES;
		static const std::string LOCAL_IDS;
		static const std::string NUM_GROUPS_X;
		static const std::string NUM_GROUPS_Y;
		static const std::string NUM_GROUPS_Z;
		static const std::string GROUP_ID_X;
		static const std::string GROUP_ID_Y;
		static const std::string GROUP_ID_Z;
		static const std::string GLOBAL_OFFSET_X;
		static const std::string GLOBAL_OFFSET_Y;
		static const std::string GLOBAL_OFFSET_Z;
		static const std::string GLOBAL_DATA_ADDRESS;
		// the number of (remaining) work-groups to execute in loop
		static const std::string GROUP_LOOP_SIZE;

		/*
		 * Whether this function is a kernel-function
		 */
		bool isKernel;
		/*
		 * The function-name
		 */
		std::string name;
		/*
		 * The return-type (e.g. TYPE_VOID for kernel-functions)
		 */
		DataType returnType;
		/*
		 * The list of parameters
		 */
		std::vector<Parameter> parameters;
		/*
		 * The list of stack-allocations from within that method, sorted by descending alignment value
		 */
		OrderedSet<StackAllocation, order_by_alignment_and_name> stackAllocations;
		/*
		 * Additional meta-data for kernel-functions
		 */
		KernelMetaData metaData;
		/*
		 * The VPM object to manage the use of the VPM cache
		 */
		std::unique_ptr<periphery::VPM> vpm;

		explicit Method(const Module &module);
		Method(const Method &) = delete;
		Method(Method &&) = delete;
		~Method();

		Method &operator=(const Method &) = delete;
		Method &operator=(Method &&) = delete;

		/*
		 * Creates a new local for the given type and returns a value pointing to it.
		 *
		 * If neither prefix nor postfix are set, the name is chosen randomly.
		 * If the prefix is set, a random postfix is appended.
		 * If only the postfix is set, the local has this exact name.
		 * If both pre- and postfix are set, the local has the name "prefix.postfix"
		 *
		 * NOTE: The name of a local must be unique within a method (for parameter, globals, stack-allocations too)
		 */
		const Value addNewLocal(const DataType &type, const std::string &prefix = "", const std::string &postfix = "");

		/*
		 * Looks for a local with the given name and returns it.
		 */
		const Local *findLocal(const std::string &name) const;
		/*
		 * Looks for a parameter with the given name and returns it.
		 */
		const Parameter *findParameter(const std::string &name) const;
		/*
		 * Looks for a global with the given name and returns it.
		 */
		const Global *findGlobal(const std::string &name) const;
		/*
		 * Looks for a stack-allocation with the given name and returns it.
		 */
		const StackAllocation *findStackAllocation(const std::string &name) const;
		/*
		 * Returns a local with the given type and name.
		 *
		 * If a local, parameter, global or stack-allocation with such a name already exists, it is returned.
		 * Otherwise a new local is created
		 */
		const Local *findOrCreateLocal(const DataType &type, const std::string &name) __attribute__((returns_nonnull));

		/*!
		 * Checks if all usages of this local are within a certain range from the current instruction, but following
		 * branches
		 */
		bool isLocallyLimited(
			InstructionWalker curIt, const Local *locale, std::size_t threshold = ACCUMULATOR_THRESHOLD_HINT) const;

		/*
		 * Returns an iterator to the beginning of the first basic block
		 *
		 * NOTE: The order the instructions are traversed in does not reflect their final order!
		 */
		InstructionWalker walkAllInstructions();
		/*
		 * Executes the given consumer for all instructions
		 *
		 * NOTE: Users of this function should assume the order of traversal to be arbitrary!
		 */
		void forAllInstructions(
			const std::function<void(const intermediate::IntermediateInstruction *)> &consumer) const;
		/*
		 * Calculates the number of instructions within the method from the sizes of the basic blocks
		 */
		std::size_t countInstructions() const;
		/*
		 * Deletes all positions not pointing to a valid instructions and returns the number of positions removed
		 */
		std::size_t cleanEmptyInstructions();
		/*
		 * Inserts the instruction at the end of the method (behind the last instruction in the last basic-block)
		 *
		 * NOTE: This method allows for insertion of labels and also creates a default label, if no basic-block exists
		 * yet
		 */
		void appendToEnd(intermediate::IntermediateInstruction *instr);
		/*
		 * Returns an iterator pointing one after the last instruction in this method
		 */
		InstructionWalker appendToEnd();

		const OrderedMap<std::string, Local> &readLocals() const;
		/*
		 * Removes all locals without any usages left
		 */
		void cleanLocals();

		/*
		 * Prints all instruction to the logging-stream
		 */
		void dumpInstructions() const;
		
		/*
		 * The following functions are for traversal only and do not allow the basic blocks themselves to be modified.
		 *
		 * To modify the relations of basic blocks, use the CFG
		 */
		inline BasicBlockList::iterator begin()
		{
			return basicBlocks.begin();
		}

		inline BasicBlockList::const_iterator begin() const
		{
			return basicBlocks.begin();
		}

		inline BasicBlockList::iterator end()
		{
			return basicBlocks.end();
		}

		inline BasicBlockList::const_iterator end() const
		{
			return basicBlocks.end();
		}

		/*
		 * Searches for the basic-block belonging to the given label
		 */
		BasicBlock *findBasicBlock(const Local *label);
		const BasicBlock *findBasicBlock(const Local *label) const;
		
		/*
		 * Clears and removes the given basic block.
		 *
		 * If overwriteUsages is not set, the deletion fails if this block has instructions (other than the label) or is target of an explicit jump
		 *
		 * Returns whether the operation was executed.
		 */
		bool removeBlock(BasicBlock& block, bool overwriteUsages = false);

		/*
		 * Inserts the given label at the position and returns a iterator to it.
		 *
		 * This function splits the current basic block, moving all following instructions to the newly created basic
		 * block
		 */
		InstructionWalker emplaceLabel(InstructionWalker it, intermediate::BranchLabel *label);

		/*
		 * Calculates the offsets (within a stack-frame) of the single stack-items
		 *
		 * The total offset (StackAllocation's offset added to the stack base-offset) is aligned to the alignment of the
		 * stack-allocation
		 */
		void calculateStackOffsets();

		/*
		 * Calculates the maximum size used by all stack allocations for a single execution
		 *
		 * The stack-size is aligned to the maximum alignment of any stack entry (the alignment of the first entry),
		 * to make sure all other stack-frames (for 2nd, 3rd, ... QPU) are aligned correctly
		 */
		std::size_t calculateStackSize() const;

		/*
		 * Calculates the base offset of the (first) stack-frame from the beginning of the global data segment.
		 *
		 * The stack base offset is aligned to the maximum alignment of any stack-entry (alignment of first stack-entry)
		 */
		std::size_t getStackBaseOffset() const;
		
		/*
		 * Returns the currently valid CFG for this function.
		 *
		 * The CFG is dropped (and needs to be re-created) if:
		 * - a basic block is added or removed
		 * - a branch or branch-label is released/erased/replaced/emplaced or replaces another instruction
		 *
		 * NOTE: Depending on the state of the function, the CFG may be (re)created in this method-call
		 */
		ControlFlowGraph& getCFG();

		/*
		 * The module the method belongs to
		 */
		const Module &module;

	private:
		/*
		 * The list of basic blocks
		 */
		BasicBlockList basicBlocks;
		/*
		 * The list of locals
		 */
		OrderedMap<std::string, Local> locals;
		/*
		 * The currently valid CFG
		 *
		 * We cannot use unique_ptr here, since the type ControlFlowGraph is not complete here
		 */
		std::shared_ptr<ControlFlowGraph> cfg;

		std::string createLocalName(const std::string &prefix = "", const std::string &postfix = "");

		BasicBlock *getNextBlockAfter(const BasicBlock *block);
		BasicBlock *getPreviousBlock(const BasicBlock *block);

		void checkAndCreateDefaultBasicBlock();

		friend class BasicBlock;
		friend class InstructionWalker;
		friend class ConstInstructionWalker;
	};
	
	using MethodIterator = ScopedInstructionWalker<Method>;
}

#endif /* VC4C_METHOD_H */