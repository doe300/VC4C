/* 
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_CONFIG_H
#define VC4C_CONFIG_H

#include <stdint.h>
#include <cstddef>

namespace vc4c
{
	enum class MathType
	{
	    FAST = 0,
	    EXACT = 1,
	    STRICT = 2
	};

	/*
	 * The kind of code to generate
	 */
	enum class OutputMode
	{
		/*
		 * Generate binary code (as used in the VC4CL host-library)
		 */
	    BINARY = 0,
		/*
		 * Generate hexadecimal code (e.g. to be included on source-files)
		 */
	    HEX = 1,
		/*
		 * Generate custom assembler code (for debugging/analysis purposes only)
		 */
	    ASSEMBLER = 2
	};

	/*
	 * Specifies which compiler front-end to use.
	 *
	 * NOTE: Forcing a specific front-end will cause compilation-errors, if it is not available
	 */
	enum class Frontend
	{
		/*
		 * Use the default front-end, depending on how this compiler was configured on build
		 */
		DEFAULT = 0,
		/*
		 * Force the use of the LLVM IR front-end
		 */
		LLVM_IR = 1,
		/*
		 * Force the use of the SPIR-V front-end
		 */
		SPIR_V = 2
	};

	/*
	 * The maximum VPM size to be used (in bytes).
	 *
	 * According to tests the configured VPM size (at least for the Raspberry Pi 2) is 12 KB.
	 * but there is another register "VPM memory reserved for user programs", which could be configured and has a default size of 4KB (according to tests).
	 */
	constexpr unsigned VPM_DEFAULT_SIZE = 4 * 1024;

	/*
	 * Container for user-defined configuration
	 */
	struct Configuration
	{
	    MathType mathType = MathType::FAST;
	    /*
	     * The output-mode to write the generated code in
	     */
	    OutputMode outputMode = OutputMode::BINARY;
	    /*
	     * Whether to prepend the kernel meta-data block at the start of the output.
	     *
	     * NOTE: This option is required for being able to load the generated binary code with the VC4CL host-library
	     */
	    bool writeKernelInfo = true;
	    /*
	     * The maximum size of the VPM available to be used as cache.
	     *
	     * NOTE: Setting this to a value not available hardware-side may hang the execution/system
	     */
	    unsigned availableVPMSize = VPM_DEFAULT_SIZE;
	    /*
	     * The front-end to be used
	     */
	    Frontend frontend = Frontend::DEFAULT;
	    /*
	     * Whether to turn on auto-vectorization of loops
	     */
	    bool autoVectorization = false;
	};

	/*
	 * The instructions limit to use accumulators.
	 * This is used as a hint for optimizations and is interpreted as follows:
	 * - Any local with a usage-range lower than this limit is assumed to be mapped to an accumulator. E.g. there is no need to split writes and reads
	 * - Any local with a usage-range higher than this threshold is assumed to be on a physical register, limitations for physical register apply
	 *
	 * NOTE: This should not be less than 5, otherwise, for all conditional jumps there is a NOP inserted
	 */
	constexpr std::size_t ACCUMULATOR_THRESHOLD_HINT{6};
	/*
	 * Numbers of elements for a native SIMD vector
	 */
	constexpr std::size_t NATIVE_VECTOR_SIZE{16};

	/*
	 * Maximum number of instructions to check for reordering.
	 * This prevents long runs for huge linear programs at the cost of less performant code
	 */
	constexpr std::size_t REPLACE_NOP_MAX_INSTRUCTIONS_TO_CHECK{64};

	/*
	 * Maximum number of rounds the register-checker tries to resolve conflicts
	 */
	constexpr std::size_t REGISTER_RESOLVER_MAX_ROUNDS{6};

	/*
	 * Magic number to identify QPU assembler code (machine code)
	 */
	constexpr uint32_t QPUASM_MAGIC_NUMBER = 0xDEADBEAF;
	constexpr uint32_t QPUASM_NUMBER_MAGIC = 0xAFBEADDE;
}

#endif /* VC4C_CONFIG_H */

