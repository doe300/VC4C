/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_METHOD_H
#define VC4C_METHOD_H

#include "BasicBlock.h"
#include "KernelMetaData.h"
#include "Locals.h"
#include "Optional.h"

#include <memory>

namespace vc4c
{
    namespace periphery
    {
        class VPM;
    } // namespace periphery
    namespace analysis
    {
        class ControlFlowGraph;
    } // namespace analysis
    class Module;
    struct Global;

    /**
     * Additional flags set for functions.
     *
     * This is a bitmask and the single flags can therefore be combined.
     */
    enum class MethodFlags : uint16_t
    {
        NONE = 0,
        /**
         * This method is a kernel function
         */
        KERNEL = 1 << 0,
        /**
         * This kernel function has been enclosed by a work-group loop
         */
        WORK_GROUP_LOOP = 1 << 1,
        /**
         * This kernel function has a leading work-group synchronization (control-flow barrier)
         */
        LEADING_CONTROL_FLOW_BARRIER = 1 << 2,
        /**
         * This kernel function has a trailing work-group synchronization (control-flow barrier)
         */
        TRAILING_CONTROL_FLOW_BARRIER = 1 << 3,
        /**
         * This kernel function is guaranteed to have no cross-item memory access within the same work-group (i.e. a
         * work-item never writes memory read by another work-item within the same work-group) or such accesses are
         * otherwise guarded, so that no data race might occur between work-items of different work-groups executed
         * serially. This property allows us to do some more memory access optimizations.
         */
        NO_UNGUARDED_CROSS_ITEM_MEMORY_DEPENDENCIES = 1 << 4
    };

    /*
     * Base class representing a function (e.g. an OpenCL kernel)
     */
    class Method : private NonCopyable
    {
        using BasicBlockList = FastModificationList<BasicBlock>;

    public:
        /**
         * Additional flags
         */
        MethodFlags flags;
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
        SortedSet<StackAllocation, order_by_alignment_and_name> stackAllocations;
        /*
         * Additional meta-data for kernel-functions
         */
        KernelMetaData metaData;
        /*
         * The VPM object to manage the use of the VPM cache
         */
        std::unique_ptr<periphery::VPM> vpm;

        explicit Method(Module& module);
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
        NODISCARD Value addNewLocal(DataType type, const std::string& prefix = "", const std::string& postfix = "");

        /*
         * Returns a newly created local with the given type and name.
         *
         * NOTE: In contrast to #addNewLocal(...), this does not create an unique name, but takes the given name as-is!
         * NOTE: The name of a local must be unique within a method (for parameter, globals, stack-allocations too)
         */
        const Local* createLocal(DataType type, const std::string& name) __attribute__((returns_nonnull));

        /**
         * Adds the given parameter to the list of tracked parameters for this function.
         *
         * NOTE: Since this function also executes some more logic (e.g. setting additional LocalData), this is to be
         * preferred over adding parameters manually!
         */
        Parameter& addParameter(Parameter&& param);

        /**
         * Looks for a builtin local with the given name and returns it.
         */
        const BuiltinLocal* findBuiltin(BuiltinLocal::Type type) const;
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
        /**
         * Looks for a builtin local for the given type. Create the builtin local if it does not exists yet.
         */
        const BuiltinLocal* findOrCreateBuiltin(BuiltinLocal::Type type) __attribute__((returns_nonnull));

        /*!
         * Checks if all usages of this local are within a certain range from the current instruction, but following
         * branches
         */
        bool isLocallyLimited(InstructionWalker curIt, const Local* local, std::size_t threshold) const;

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
            const std::function<void(const intermediate::IntermediateInstruction&)>& consumer) const;
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
        intermediate::IntermediateInstruction& appendToEnd(
            std::unique_ptr<intermediate::IntermediateInstruction>&& instr);
        /*
         * Returns an iterator pointing one after the last instruction in this method
         */
        InstructionWalker appendToEnd();

        std::size_t getNumLocals() const;

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
        BasicBlock* findBasicBlock(const Local* label);
        const BasicBlock* findBasicBlock(const Local* label) const;
        BasicBlock* findBasicBlock(const std::string& label);
        const BasicBlock* findBasicBlock(const std::string& label) const;

        /*
         * Clears and removes the given basic block.
         *
         * If overwriteUsages is not set, the deletion fails if this block has instructions (other than the label) or is
         * target of an explicit jump
         *
         * Returns whether the operation was executed.
         */
        NODISCARD bool removeBlock(BasicBlock& block, bool overwriteUsages = false);

        /*
         * Create new basic block and Insert it into position.
         */
        BasicBlock& createAndInsertNewBlock(BasicBlockList::iterator position, const std::string& labelName);

        /*
         * Returns the number of blocks in this method
         */
        std::size_t size() const
        {
            return basicBlocks.size();
        }

        /**
         * @return whether this method contains no instructions (except possibly a single label)
         */
        bool empty() const
        {
            return size() == 0 || (size() == 1 && begin()->empty());
        }

        /*
         * Inserts the given label at the position and returns a iterator to it.
         *
         * This function splits the current basic block, moving all following instructions to the newly created basic
         * block
         */
        NODISCARD InstructionWalker emplaceLabel(
            InstructionWalker it, std::unique_ptr<intermediate::BranchLabel>&& label);

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
         * NOTE: Depending on the state of the function, the CFG may be (re)created in this method-call
         */
        analysis::ControlFlowGraph& getCFG();

        /*
         * The module the method belongs to
         */
        Module& module;

        /*
         * Moves the block from origin to destination (is inserted before destination)
         * NOTE: This function MUST NOT change the control flow of the method
         * and therefore can only be called if the blocks predecessor does not fall-through.
         */
        void moveBlock(BasicBlockList::iterator origin, BasicBlockList::iterator dest);

        // NOTE: The Method does not hold any types, since it might be destroyed (inlined), it only redirects these
        // calls to the Module
        DataType createPointerType(
            DataType elementType, AddressSpace addressSpace = AddressSpace::PRIVATE, unsigned alignment = 0);
        DataType createStructType(
            const std::string& name, const std::vector<DataType>& elementTypes, bool isPacked = false);
        DataType createArrayType(DataType elementType, unsigned int size);
        DataType createImageType(
            uint8_t dimensions, bool isImageArray = false, bool isImageBuffer = false, bool isSampled = false);

    private:
        /*
         * The list of basic blocks
         */
        BasicBlockList basicBlocks;
        /*
         * The list of locals
         */
        StableList<Local> locals;

        /*
         * The builtin locals which are statically named
         */
        std::vector<std::unique_ptr<BuiltinLocal>> builtinLocals;

        /*
         * The currently valid CFG
         *
         * We cannot use unique_ptr here, since the type ControlFlowGraph is not complete here
         */
        std::unique_ptr<analysis::ControlFlowGraph> cfg;

        std::string createLocalName(const std::string& prefix = "", const std::string& postfix = "");

        BasicBlock* getNextBlockAfter(const BasicBlock* block);
        BasicBlock* getPreviousBlock(const BasicBlock* block);

        void checkAndCreateDefaultBasicBlock();

        void updateCFGOnBlockInsertion(BasicBlock* block);
        void updateCFGOnBlockRemoval(BasicBlock* block);
        void updateCFGOnBranchInsertion(InstructionWalker it);
        void updateCFGOnBranchRemoval(BasicBlock& affectedBlock, const FastSet<const Local*>& branchTargets);

        void addLocalData(Local& loc);

        friend class BasicBlock;
        friend class InstructionWalker;
        friend class ConstInstructionWalker;
    };

    using MethodIterator = ScopedInstructionWalker<Method>;
} // namespace vc4c

#endif /* VC4C_METHOD_H */
