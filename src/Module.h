/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SRC_MODULE_H_
#define SRC_MODULE_H_

#include "config.h"
#include "Locals.h"
#include "performance.h"
#include "Types.h"

namespace vc4c
{
	namespace intermediate
	{
		class IntermediateInstruction;
		struct BranchLabel;
	} // namespace intermediate

	namespace periphery
	{
		class VPM;
	} // namespace periphery

	/*
	 * Container for additional meta-data of kernel-functions
	 */
	struct KernelMetaData
	{
		/*
		 * The compilation-time work-group size, specified by the reqd_work_group_size attribute
		 */
		std::array<uint32_t, 3> workGroupSizes;
		/*
		 * The compilation-time preferred work-group size, specified by the work_group_size_hint attribute
		 */
		std::array<uint32_t, 3> workGroupSizeHints;

		KernelMetaData()
		{
			workGroupSizes.fill(0);
			workGroupSizeHints.fill(0);
		}

		/*
		 * Retuns whether the explicit work-group size is set
		 */
		bool isWorkGroupSizeSet() const;
	};

	class InstructionWalker;
	class BasicBlock;
	class Module;
	class Method;
	class ControlFlowGraph;

	template<typename Scope>
	struct ScopedInstructionWalker;
	using BlockIterator = ScopedInstructionWalker<BasicBlock>;
	using MethodIterator = ScopedInstructionWalker<Method>;

	/*
	 * A basic-block is a sequence of continuous instructions within a function body.
	 *
	 * If an instruction within a basic-block is executed, all instructions within that block are executed.
	 * A basic-block always starts with a label, can only contain that one label and cannot have any non-branch instructions behind the first branch.
	 */
	class BasicBlock : private NonCopyable
	{
	public:
		static const std::string DEFAULT_BLOCK;
		static const std::string LAST_BLOCK;

		BasicBlock(Method& method, intermediate::BranchLabel* label);
		BasicBlock(const BasicBlock&) = delete;
		BasicBlock(BasicBlock&&) = delete;
		~BasicBlock() = default;

		BasicBlock& operator=(const BasicBlock&) = delete;
		BasicBlock& operator=(BasicBlock&&) = delete;

		/*
		 * Whether this basic block has no instructions (except its label)
		 */
		bool empty() const;
		/*
		 * Returns an iterator to the start of this basic block (points to the label)
		 */
		InstructionWalker begin();
		/*
		 * Returns an iterator to the end of the basic block (points one past the last instruction)
		 */
		InstructionWalker end();
		/*
		 * Returns the number of instructions within this block
		 */
		std::size_t size() const;

		/*!
		 * Checks if all usages of this local are within a certain range from the current instruction within a single basic block
		 */
		bool isLocallyLimited(InstructionWalker curIt, const Local* locale, std::size_t threshold = ACCUMULATOR_THRESHOLD_HINT) const;

		/*
		 * Returns the label for this block
		 */
		const intermediate::BranchLabel* getLabel() const;
		/*
		 * Runs the consumer function for every block directly following this
		 *
		 * NOTE: the consumer may be called several times for a single block
		 */
		void forSuccessiveBlocks(const std::function<void(BasicBlock&)>& consumer) const;
		/*
		 * Runs the consumer function for every block directly preceding this
		 *
		 * NOTE: the consumer may be called several times for a single block
		 */
		void forPredecessors(const std::function<void(InstructionWalker)>& consumer) const;
		/*
		 * Returns whether the control-flow falls through to the next basic block, e.g. there are no unconditional branches away
		 */
		bool fallsThroughToNextBlock() const;
		/*
		 * Returns the InstructionWalker for the given instruction, if any
		 */
		Optional<InstructionWalker> findWalkerForInstruction(const intermediate::IntermediateInstruction* instr, InstructionWalker start);
		/*
		 * Returns the InstructionWalker for the last (as in prior to the given instruction) instruction setting flags within this basic block
		 */
		Optional<InstructionWalker> findLastSettingOfFlags(InstructionWalker start);
		/*
		 * Returns whether this basic-block is the start of the method body
		 */
		bool isStartOfMethod() const;
	private:
		Method& method;
		RandomModificationList<std::unique_ptr<intermediate::IntermediateInstruction>> instructions;

		friend class ControlFlowGraph;
		friend class InstructionWalker;
		friend struct InstructionVisitor;
		friend class Method;
	};

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
		//the number of (remaining) work-groups to execute in loop
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

		explicit Method(const Module& module);
		Method(const Method&) = delete;
		Method(Method&&) = delete;
		~Method();

		Method& operator=(const Method&) = delete;
		Method& operator=(Method&&) = delete;

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
		const Value addNewLocal(const DataType& type, const std::string& prefix = "", const std::string& postfix = "");

		/*
		 * Looks for a local with the given name and returns it.
		 */
		const Local* findLocal(const std::string& name) const;
		/*
		 * Looks for a parameter with the given name and returns it.
		 */
		const Parameter* findParameter(const std::string& name) const;
		/*
		 * Looks for a global with the given name and returns it.
		 */
		const Global* findGlobal(const std::string& name) const;
		/*
		 * Looks for a stack-allocation with the given name and returns it.
		 */
		const StackAllocation* findStackAllocation(const std::string& name) const;
		/*
		 * Returns a local with the given type and name.
		 *
		 * If a local, parameter, global or stack-allocation with such a name already exists, it is returned.
		 * Otherwise a new local is created
		 */
		const Local* findOrCreateLocal(const DataType& type, const std::string& name) __attribute__((returns_nonnull));

		/*!
		 * Checks if all usages of this local are within a certain range from the current instruction, but following branches
		 */
		bool isLocallyLimited(InstructionWalker curIt, const Local* locale, std::size_t threshold = ACCUMULATOR_THRESHOLD_HINT) const;

		/*
		 * Returns an iterator to the beginning of the first basic block
		 */
		InstructionWalker walkAllInstructions();
		/*
		 * Executes the given consumer for all instructions
		 */
		void forAllInstructions(const std::function<void(const intermediate::IntermediateInstruction*)>& consumer) const;
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
		 * NOTE: This method allows for insertion of labels and also creates a default label, if no basic-block exists yet
		 */
		void appendToEnd(intermediate::IntermediateInstruction* instr);
		/*
		 * Returns an iterator pointing one after the last instruction in this method
		 */
		InstructionWalker appendToEnd();

		const OrderedMap<std::string, Local>& readLocals() const;
		/*
		 * Removes all locals without any usages left
		 */
		void cleanLocals();

		/*
		 * Prints all instruction to the logging-stream
		 */
		void dumpInstructions() const;
		/*
		 * Returns the basic-blocks within this method
		 */
		BasicBlockList& getBasicBlocks();

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
		BasicBlock* findBasicBlock(const Local* label);

		/*
		 * Inserts the given label at the position and returns a iterator to it.
		 *
		 * This function splits the current basic block, moving all following instructions to the newly created basic block
		 */
		InstructionWalker emplaceLabel(InstructionWalker it, intermediate::BranchLabel* label);

		/*
		 * Calculates the offsets (within a stack-frame) of the single stack-items
		 *
		 * The total offset (StackAllocation's offset added to the stack base-offset) is aligned to the alignment of the stack-allocation
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
		 * The module the method belongs to
		 */
		const Module& module;

	private:
		/*
		 * The list of basic blocks
		 */
		BasicBlockList basicBlocks;
		/*
		 * The list of locals
		 */
		OrderedMap<std::string, Local> locals;

		std::string createLocalName(const std::string& prefix = "", const std::string& postfix = "");

		BasicBlock* getNextBlockAfter(const BasicBlock* block);
		BasicBlock* getPreviousBlock(const BasicBlock* block);

		void checkAndCreateDefaultBasicBlock();

		friend class BasicBlock;
		friend class ControlFlowGraph;
		friend class InstructionWalker;
	};

	/*
	 * A module represents a compilation unit (e.g. a compilation of one source file).
	 *
	 * The module-class manages shared data, like globals and contains the list of methods
	 */
	class Module : private NonCopyable
	{
		using MethodList = std::vector<std::unique_ptr<Method>>;

	public:
		explicit Module(const Configuration& compilationConfig);
		Module(const Module&) = delete;
		Module(Module&&) = delete;
		~Module() = default;

		Module& operator=(const Module&) = delete;
		Module& operator=(Module&&) = delete;

		/*
		 * The global data within this module
		 */
		ReferenceRetainingList<Global> globalData;
		/*
		 * The module's methods
		 */
		MethodList methods;

		inline MethodList::iterator begin()
		{
			return methods.begin();
		}

		inline MethodList::const_iterator begin() const
		{
			return methods.begin();
		}

		inline MethodList::iterator end()
		{
			return methods.end();
		}

		inline MethodList::const_iterator end() const
		{
			return methods.end();
		}

		/*
		 * Returns the methods marked as OpenCL kernels
		 */
		std::vector<Method*> getKernels();
		/*
		 * Calculates the offset (in bytes) from the start of the global-data segment for the given local.
		 *
		 * If the local is a Global, the correctly aligned position of this global is returned,
		 * otherwise the complete size of the global-data segment (aligned to 8 Byte) is returned.
		 */
		Optional<unsigned int> getGlobalDataOffset(const Local* local) const;

		/*
		 * Looks for a global with the given name and returns it.
		 *
		 * Returns nullptr otherwise
		 */
		const Global* findGlobal(const std::string& name) const;

		const Configuration& compilationConfig;
	};
} // namespace vc4c

#endif /* SRC_MODULE_H_ */
