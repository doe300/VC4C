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

	struct KernelMetaData
	{
		std::array<uint32_t, 3> workGroupSizes;
		std::array<uint32_t, 3> workGroupSizeHints;

		KernelMetaData()
		{
			workGroupSizes.fill(0);
			workGroupSizeHints.fill(0);
		}
	};

	class InstructionWalker;
	class Module;
	class Method;

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

		bool empty() const;
		InstructionWalker begin();
		InstructionWalker end();

		/*!
		 * Checks if all usages of this local are within a certain range from the current instruction within a single basic block
		 */
		bool isLocallyLimited(InstructionWalker curIt, const Local* locale, std::size_t threshold = ACCUMULATOR_THRESHOLD_HINT) const;

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
		 * Returns the InstructionWalker for the last (as in prior to the given instruction) instruction setting flags within this basoc block
		 */
		Optional<InstructionWalker> findLastSettingOfFlags(InstructionWalker start);
	private:
		Method& method;
		RandomModificationList<std::unique_ptr<intermediate::IntermediateInstruction>> instructions;

		friend class InstructionWalker;
		friend class InstructionVisitor;
		friend class Method;
	};

	class Method : private NonCopyable
	{
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

		bool isKernel;
		std::string name;
		DataType returnType;
		std::vector<Parameter> parameters;
		ReferenceRetainingList<StackAllocation> stackAllocations;
		KernelMetaData metaData;
		std::unique_ptr<periphery::VPM> vpm;

		explicit Method(const Module& module);
		Method(const Method&) = delete;
		Method(Method&&) = delete;
		~Method();

		Method& operator=(const Method&) = delete;
		Method& operator=(Method&&) = delete;

		const Value addNewLocal(const DataType& type, const std::string& prefix = "", const std::string& postfix = "");

		const Local* findLocal(const std::string& name) const;
		const Parameter* findParameter(const std::string& name) const;
		const Global* findGlobal(const std::string& name) const;
		const StackAllocation* findStackAllocation(const std::string& name) const;
		const Local* findOrCreateLocal(const DataType& type, const std::string& name) __attribute__((returns_nonnull));

		/*!
		 * Checks if all usages of this local are within a certain range from the current instruction, but following branches
		 */
		bool isLocallyLimited(InstructionWalker curIt, const Local* locale, std::size_t threshold = ACCUMULATOR_THRESHOLD_HINT) const;

		InstructionWalker walkAllInstructions();
		void forAllInstructions(const std::function<void(const intermediate::IntermediateInstruction*)>& consumer) const;
		std::size_t countInstructions() const;
		std::size_t cleanEmptyInstructions();
		void appendToEnd(intermediate::IntermediateInstruction* instr);
		InstructionWalker appendToEnd();

		const OrderedMap<std::string, Local>& readLocals() const;
		void cleanLocals();

		void dumpInstructions() const;
		RandomModificationList<BasicBlock>& getBasicBlocks();
		BasicBlock* findBasicBlock(const Local* label);

		InstructionWalker emplaceLabel(InstructionWalker it, intermediate::BranchLabel* label);

		/*
		 * Calculates the maximum size used by all stack allocations for a single execution
		 */
		std::size_t calculateStackSize() const;

	private:
		const Module& module;
		RandomModificationList<BasicBlock> basicBlocks;
		OrderedMap<std::string, Local> locals;

		std::string createLocalName(const std::string& prefix = "", const std::string& postfix = "");

		BasicBlock* getNextBlockAfter(const BasicBlock* block);
		BasicBlock* getPreviousBlock(const BasicBlock* block);

		void checkAndCreateDefaultBasicBlock();

		friend class BasicBlock;
		friend class InstructionWalker;
	};

	class Module : private NonCopyable
	{
	public:
		explicit Module(const Configuration& compilationConfig);
		Module(const Module&) = delete;
		Module(Module&&) = default;
		~Module() = default;

		Module& operator=(const Module&) = delete;
		Module& operator=(Module&&) = delete;

		ReferenceRetainingList<Global> globalData;
		std::vector<std::unique_ptr<Method>> methods;

		std::vector<Method*> getKernels();
		Optional<unsigned int> getGlobalDataOffset(const Local* local) const;

		const Configuration& compilationConfig;
	};
} // namespace vc4c

#endif /* SRC_MODULE_H_ */
