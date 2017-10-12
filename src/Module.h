/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SRC_MODULE_H_
#define SRC_MODULE_H_

#include "config.h"
#include "Types.h"
#include "Values.h"
#include "Locals.h"
#include "performance.h"

namespace vc4c
{
	namespace intermediate
	{
		class IntermediateInstruction;
		class BranchLabel;
	}

	namespace periphery
	{
		class VPM;
	}

	enum class MetaDataType
	{
		//TODO remove most of them (except work-group-sizes and size-hint)
		ARG_ADDR_SPACES, ARG_ACCESS_QUALIFIERS, ARG_TYPE_NAMES, ARG_TYPE_QUALIFIERS, ARG_NAMES, WORK_GROUP_SIZES, WORK_GROUP_SIZES_HINT, OTHER
	};

	template<>
	struct hash<MetaDataType> : public std::hash<uint8_t>
	{
		size_t operator()(const MetaDataType& ) const noexcept;
	};

	struct InstructionWalker;
	class Module;
	class Method;


	class BasicBlock : private NonCopyable
	{
	public:
		static const std::string DEFAULT_BLOCK;
		static const std::string LAST_BLOCK;

		BasicBlock(Method& method, intermediate::BranchLabel* label);
		BasicBlock(BasicBlock&&) = delete;

		BasicBlock& operator=(BasicBlock&&) = delete;

		bool empty() const;
		InstructionWalker begin();
		InstructionWalker end();

		/*!
		 * Checks if all usages of this local are within a certain range from the current instruction within a single basic block
		 */
		bool isLocallyLimited(InstructionWalker curIt, const Local* locale, const std::size_t threshold = ACCUMULATOR_THRESHOLD_HINT) const;

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
		std::map<MetaDataType, std::vector<std::string>> metaData;
		std::unique_ptr<periphery::VPM> vpm;

		Method(const Module& module);
		~Method();

		Method(Method&& old) = delete;
		Method& operator=(Method&& old) = delete;

		const Value addNewLocal(const DataType& type, const std::string& prefix = "", const std::string& postfix = "");

		const Local* findLocal(const std::string& name) const;
		const Parameter* findParameter(const std::string& name) const;
		const Global* findGlobal(const std::string& name) const;
		const Local* findOrCreateLocal(const DataType& type, const std::string& name) __attribute__((returns_nonnull));

		/*!
		 * Checks if all usages of this local are within a certain range from the current instruction, but following branches
		 */
		bool isLocallyLimited(InstructionWalker curIt, const Local* locale, const std::size_t threshold = ACCUMULATOR_THRESHOLD_HINT) const;

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

	private:
		const Module& module;
		RandomModificationList<BasicBlock> basicBlocks;
		OrderedMap<std::string, Local> locals;

		std::string createLocalName(const std::string& prefix = "", const std::string& postfix = "");

		BasicBlock* getNextBlockAfter(const BasicBlock* block);
		BasicBlock* getPreviousBlock(const BasicBlock* block);

		friend class BasicBlock;
		friend class InstructionWalker;
	};

	class Module : private NonCopyable
	{
	public:
		Module(const Configuration& compilationConfig);
		~Module();

		Module(Module&& old) = default;
		Module& operator=(Module&& old) = default;

		ReferenceRetainingList<Global> globalData;
		std::vector<std::unique_ptr<Method>> methods;

		std::vector<Method*> getKernels();
		Optional<unsigned int> getGlobalDataOffset(const Local* local) const;

		const Configuration& compilationConfig;
	};
}



#endif /* SRC_MODULE_H_ */
