/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef INSTRUCTION_WALKER_H
#define INSTRUCTION_WALKER_H

#include <functional>

#include "Module.h"
#include "intermediate/IntermediateInstruction.h"
#include "Graph.h"

namespace vc4c
{
	enum class InstructionVisitResult
	{
		//continue to visit the next instruction
		CONTINUE,
		//stop visiting this branch, continue with others, if any
		STOP_BRANCH,
		//stop completely
		STOP_ALL
	};

	struct InstructionVisitor
	{
		const std::function<InstructionVisitResult(InstructionWalker&)> op;
		const bool stopAtBlock;
		const bool followJumps;

		/*
		 * Visits start and all following instructions, according to the settings
		 *
		 * \return true, if the end of the block/method was reached, false, if the visiting operation aborted with STOP_ALL
		 */
		bool visit(const InstructionWalker& start) const;
		/*
		 * Visits start and all preceding instructions, according to the settings
		 *
		 * \return true, if the beginning of the block/method was reached, false, of the visiting operation aborted with STOP_ALL
		 */
		bool visitReverse(const InstructionWalker& start, Graph<InstructionWalker, Node<InstructionWalker, bool>>* blockGraph = nullptr) const;
	};

	class InstructionWalker
	{
	public:
		explicit InstructionWalker();
		InstructionWalker(BasicBlock* basicBlock, intermediate::InstructionsIterator pos);
		InstructionWalker(const InstructionWalker& other) = default;
		InstructionWalker(InstructionWalker&& other) = default;

		BasicBlock* getBasicBlock();

		InstructionWalker& operator=(const InstructionWalker& other) = default;
		InstructionWalker& operator=(InstructionWalker&& other) = default;

		InstructionWalker& nextInBlock();
		InstructionWalker& previousInBlock();
		bool isEndOfBlock() const;
		bool isStartOfBlock() const;

		InstructionWalker& nextInMethod();
		InstructionWalker& previousInMethod();
		bool isEndOfMethod() const;
		bool isStartOfMethod() const;

		InstructionWalker copy() const;

		bool operator==(const InstructionWalker& other) const;
		inline bool operator!=(const InstructionWalker& other) const
		{
			return !(*this == other);
		}

		template<typename T>
		inline T* get()
		{
			return dynamic_cast<T*>(get());
		}

		template<typename T>
		inline const T* get() const
		{
			return dynamic_cast<T*>(get());
		}

		inline bool has() const
		{
			return get() != nullptr;
		}

		template<typename T>
		inline bool has() const
		{
			return get<const T>() != nullptr;
		}

		inline intermediate::IntermediateInstruction* operator->()
		{
			return get();
		}

		inline const intermediate::IntermediateInstruction* operator->() const
		{
			return get();
		}

		intermediate::IntermediateInstruction* get();
		const intermediate::IntermediateInstruction* get() const;
		intermediate::IntermediateInstruction* release();

		InstructionWalker& reset(intermediate::IntermediateInstruction* instr);
		InstructionWalker& erase();
		InstructionWalker& emplace(intermediate::IntermediateInstruction* instr);

		inline void forAllInstructions(const std::function<void(const intermediate::IntermediateInstruction*)>& func) const
		{
			const intermediate::CombinedOperation* combined = get<const intermediate::CombinedOperation>();
			if(combined != nullptr)
			{
				func(combined->getFirstOp());
				func(combined->getSecondOP());
			}
			else
				func(get());
		}

		inline bool allInstructionMatches(const std::function<bool(const intermediate::IntermediateInstruction*)>& func) const
		{
			const intermediate::CombinedOperation* combined = get<const intermediate::CombinedOperation>();
			if(combined != nullptr)
			{
				return func(combined->getFirstOp()) && func(combined->getSecondOP());
			}
			return func(get());
		}

		inline bool anyInstructionMatches(const std::function<bool(const intermediate::IntermediateInstruction*)>& func) const
		{
			const intermediate::CombinedOperation* combined = get<const intermediate::CombinedOperation>();
			if(combined != nullptr)
			{
				return func(combined->getFirstOp()) || func(combined->getSecondOP());
			}
			return func(get());
		}

	private:
		BasicBlock* basicBlock;
		intermediate::InstructionsIterator pos;

		friend class Method;
	};

	template<>
	struct hash<InstructionWalker>
	{
		size_t operator()(const InstructionWalker& ) const noexcept;
	};

} /* namespace vc4c */

#endif /* INSTRUCTION_WALKER_H */
