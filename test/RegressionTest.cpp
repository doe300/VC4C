/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "RegressionTest.h"
#include "../src/Profiler.h"
#include "../src/helper.h"
#include "Precompiler.h"

using namespace vc4c;

// TODO
// Implements this: ./testing/JohnTheRipper/pbkdf2_hmac_md4_kernel.cl, ./testing/JohnTheRipper/pbkdf2_hmac_md5_kernel.cl

// TODO add emulation tests for some more of these (esp. more complex kernels)

/**
 * Bit-set to combine the regression status
 */
enum RegressionStatus : uint8_t
{
    PASSED = 1u << 0u,
    PENDING_LLVM = 1u << 1u,
    PENDING_SPIRV = 1u << 2u,
    DISABLED_IMAGE = 1u << 3u,
    DISABLED_GROUP_SIZE = 1u << 4u
};

constexpr RegressionStatus operator|(RegressionStatus a, RegressionStatus b) noexcept
{
    return static_cast<RegressionStatus>(static_cast<uint8_t>(a) | static_cast<uint8_t>(b));
}

constexpr bool isSupported(RegressionStatus status, Frontend frontend)
{
    if(frontend == Frontend::LLVM_IR)
        return !has_flag(status, RegressionStatus::PENDING_LLVM);
    if(frontend == Frontend::SPIR_V)
        return !has_flag(status, RegressionStatus::PENDING_SPIRV);
    return status == PASSED;
}

static constexpr auto EMULATED = PASSED;
// Fails due to errors in the llvm-spirv translation
static constexpr auto PENDING_SPIRV_PRECOMPILER = PENDING_SPIRV;
// The test requires image functions/types
static constexpr auto PENDING_IMAGE = PENDING_LLVM | PENDING_SPIRV | DISABLED_IMAGE;
// The test requires not supported work-group size of more than 12 work-items
static constexpr auto PENDING_WORK_GROUP_SIZE = PENDING_LLVM | PENDING_SPIRV | DISABLED_GROUP_SIZE;
// Fails on the corresponding front-end in the CI but not locally
static constexpr auto PENDING_LLVM_CI = PENDING_LLVM;
static constexpr auto PENDING_SPIRV_CI = PENDING_SPIRV;
static constexpr uint8_t SLOW = true;
static constexpr uint8_t FAST = false;

using Entry = std::tuple<RegressionStatus, uint8_t, std::string, std::string>;

static std::vector<Entry> allKernels = {
    Entry{PASSED, FAST, EXAMPLE_FILES "fft2_2.cl", ""},
    Entry{EMULATED, FAST, EXAMPLE_FILES "fibonacci.cl", ""},
    Entry{EMULATED, FAST, EXAMPLE_FILES "hello_world.cl", ""},
    Entry{EMULATED, FAST, EXAMPLE_FILES "hello_world_vector.cl", ""},
    Entry{EMULATED, FAST, EXAMPLE_FILES "test.cl", ""},
    Entry{EMULATED | PENDING_SPIRV_PRECOMPILER, FAST, EXAMPLE_FILES "test_instructions.cl", ""},
    Entry{EMULATED, FAST, EXAMPLE_FILES "test_prime.cl", ""},
    Entry{EMULATED | PENDING_SPIRV_PRECOMPILER, FAST, EXAMPLE_FILES "md5.cl", ""},
    Entry{EMULATED | PENDING_SPIRV_PRECOMPILER, FAST, EXAMPLE_FILES "SHA-256.cl", ""},
    Entry{PASSED, FAST, EXAMPLE_FILES "test_cl.cl", ""},
    Entry{EMULATED, FAST, EXAMPLE_FILES "histogram.cl", "-DFLOATING_TYPE=1 -DTYPE=float -DMIN_VALUE=16 -DMAX_VALUE=0"},
    Entry{PASSED, FAST, EXAMPLE_FILES "histogram.cl", "-DTYPE=short -DMAX_VALUE=32767 -DMIN_VALUE=-32768"},

    Entry{EMULATED, FAST, TESTING_FILES "test_async_copy.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "test_atomic.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_barrier_fence.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "test_barrier.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "test_branches.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_builtins.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_common.cl", ""},
    // XXX unsupported explicit rounding modes on type conversions
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "test_conversions.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_float.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_geometric.cl", ""},
    Entry{PENDING_SPIRV_CI, FAST, TESTING_FILES "test_global_data.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "test_hashes.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "test_images.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_immediates.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_int.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_integer.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_math.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "test_other.cl", ""},
    Entry{EMULATED | PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "test_partial_md5.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_sfu.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "test_shuffle.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "test_struct.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "test_vector.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_vector3_layout.cl", ""},
    // XXX LLVM-SPIRV Translator does not support i3 type used for switch in test19
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "test_vectorization.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_vpm_read.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "test_vpm_write.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "test_work_item.cl", ""},

    Entry{PASSED, FAST, TESTING_FILES "deepCL/activate.cl", "-DLINEAR"},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/addscalar.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/applyActivationDeriv.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "deepCL/backpropweights.cl",
        "-DgNumFilters=4 -DgInputPlanes=2 -DgOutputPlanes=2 -DgOutputSize=16 -DgInputSize=16 -DgFilterSize=4 "
        "-DgFilterSizeSquared=16 -DgMargin=1"},
    Entry{EMULATED, FAST, TESTING_FILES "deepCL/copy.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/forward_fc.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "deepCL/inv.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "deepCL/memset.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/per_element_add.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/per_element_mult.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/SGD.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/sqrt.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/squared.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/backpropweights_byrow.cl",
        "-DgInputSize=16 -DgOutputSize=16 -DgFilterSize=4 -DgFilterSizeSquared=16 -DgNumOutputPlanes=4 -DgMargin=1 "
        "-DgNumInputPlanes=4 -DinputRow=0"},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "deepCL/BackpropWeightsScratchLarge.cl",
        "-DgFilterSize=4 -DgFilterSizeSquared=16 -DgOutputSize=16 -DgInputSize=16 -DgMargin=1 "
        "-DgInputStripeInnerSize=2 -DgInputStripeOuterSize=3 -DgOutputSizeSquared=16 -DgInputStripeMarginSize=1 "
        "-DgNumStripes=16 -DgOutputStripeSize=16 -DgOutputStripeNumRows=4 -DgNumFilters=4 -DgInputPlanes=4 "
        "-DgInputSizeSquared=16"},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "deepCL/backward.cl",
        "-DgFilterSize=4 -DgFilterSizeSquared=16 -DgOutputSize=16 -DgInputSize=16 -DgMargin=1 "
        "-DgInputStripeInnerSize=2 -DgInputStripeOuterSize=3 -DgOutputSizeSquared=16 -DgInputStripeMarginSize=1 "
        "-DgNumStripes=16 -DgOutputStripeSize=16 -DgOutputStripeNumRows=4 -DgNumFilters=4 -DgInputPlanes=4 "
        "-DgInputSizeSquared=16"},
    Entry{PENDING_LLVM_CI, FAST, TESTING_FILES "deepCL/backward_cached.cl",
        "-DgFilterSize=4 -DgFilterSizeSquared=16 -DgOutputSize=16 -DgInputSize=16 -DgMargin=1 "
        "-DgInputStripeInnerSize=2 -DgInputStripeOuterSize=3 -DgOutputSizeSquared=16 -DgInputStripeMarginSize=1 "
        "-DgNumStripes=16 -DgOutputStripeSize=16 -DgOutputStripeNumRows=4 -DgNumFilters=4 -DgInputPlanes=4 "
        "-DgInputSizeSquared=16"},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/copyBlock.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/copyLocal.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "deepCL/forward.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "deepCL/forward1.cl",
        "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 "
        "-DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16"},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/forward2.cl", "-DgWorkgroupSize=8"},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "deepCL/forward3.cl",
        "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 "
        "-DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16 "
        "-DgPadZeros=true -DgInputPlanes=8"},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/forward4.cl", ""},
    Entry{PENDING_SPIRV_CI, FAST, TESTING_FILES "deepCL/forward_byinputplane.cl",
        "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 "
        "-DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16 "
        "-DgPadZeros=true -DgInputPlanes=8"},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/forward_fc_wgperrow.cl",
        "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 "
        "-DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16 "
        "-DgPadZeros=true -DgInputPlanes=8"},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "deepCL/forwardfc_workgroupperfilterplane.cl",
        "-DgFilterSizeSquared=16 -DgNumInputPlanes=8"},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/ids.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "deepCL/pooling.cl",
        "-DgOutputSize=16 -DgOutputSizeSquared=64 -DgNumPlanes=4 -DgPoolingSize=8 -DgInputSize=16 "
        "-DgInputSizeSquared=64"},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "deepCL/PoolingBackwardGpuNaive.cl",
        "-DgOutputSize=16 -DgOutputSizeSquared=64 -DgNumPlanes=4 -DgPoolingSize=8 -DgInputSize=16 "
        "-DgInputSizeSquared=64"},
    Entry{PASSED, FAST, TESTING_FILES "deepCL/reduce_segments.cl", ""},

    Entry{PASSED, FAST, TESTING_FILES "CLTune/conv_reference.opencl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "CLTune/gemm.opencl",
        "-DVWM=16 -DVWN=16 -DMWG=1 -DNWG=1 -DMDIMC=1 -DNDIMC=1 -DKWG=1 -DKWI=1 -Dreal16=float16 -Dreal=float "
        "-DZERO=0.0f"},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "CLTune/gemm_reference.opencl", ""},
    Entry{PASSED, FAST, TESTING_FILES "CLTune/multiple_kernels_reference.opencl", ""},
    Entry{PASSED, FAST, TESTING_FILES "CLTune/multiple_kernels_unroll.opencl", ""},
    Entry{PASSED, FAST, TESTING_FILES "CLTune/simple_kernel.opencl", ""},
    // TODO freezes/hangs llvm-spirv/the calling of it
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "CLTune/conv.opencl",
        "-DVECTOR=16 -DWPTX=256 -DWPTY=64 -DTBX=2 -DTBY=2 -DUNROLL_FACTOR -Dfloatvec=float16"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "CLTune/conv_simple_kernel.opencl",
        "-DVECTOR=16 -DWPTX=256 -DWPTY=64 -DTBX=2 -DTBY=2 -DUNROLL_FACTOR -Dfloatvec=float16"},
    Entry{PASSED, FAST, TESTING_FILES "CLTune/multiple_kernels_tiled.opencl",
        "-DVECTOR=16 -DWPTX=256 -DWPTY=64 -DTBX=2 -DTBY=2 -DUNROLL_FACTOR -Dfloatvec=float16 -DTS=8"},

    Entry{PASSED, FAST, TESTING_FILES "bullet/jointSolver.cl", ""},
    Entry{PENDING_WORK_GROUP_SIZE, FAST, TESTING_FILES "bullet/solveContact.cl", ""},
    Entry{PENDING_WORK_GROUP_SIZE, FAST, TESTING_FILES "bullet/solveFriction.cl", ""},

    Entry{PASSED, FAST, TESTING_FILES "clpeak/compute_integer_kernels.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "clpeak/compute_sp_kernels.cl", ""},
    // XXX Entry{PASSED, FAST, TESTING_FILES "clpeak/compute_hp_kernels.cl", "-DHALF_AVAILABLE"},
    Entry{PASSED, FAST, TESTING_FILES "clpeak/global_bandwidth_kernels.cl", ""},

    Entry{PASSED, FAST, TESTING_FILES "vattenoverhuvudet/calculate_voxel_grid.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "vattenoverhuvudet/integrate_particle_states.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "vattenoverhuvudet/simple_voxel_grid_move.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "vattenoverhuvudet/taskParallel.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "vattenoverhuvudet/update_particle_positions.cl", ""},

    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/bilateral2.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/bilateral3.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/bilateralAdapt.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/bilateral_shared.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "gputools/convolve.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/convolve1.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/convolve2.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/convolve3.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "gputools/convolve_sep.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/correlate_kernels.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/dct_8x8.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/dct_8x8_new.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/dct_8x8x8.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "gputools/minmax_filter.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/nlm2.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/nlm3.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/nlm3_thresh.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/nlmeans.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/nlmeans3d.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/nlmeans_projected.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/nlm_fast.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/nlm_fast3.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/patch_kernel.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "gputools/perlin.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/scale.cl", "-DTYPENAME=float4 -DREAD_IMAGE=read_imagef"},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/transformations.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "gputools/tv_chambolle.cl", ""},

    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "clNN/im2col.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "clNN/SoftMax.cl", "-DSOFTMAX_THREADS=4"},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "clNN/SpatialAveragePooling.cl",
        "-DDtype=float -DCOUNT_INCLUDE_PAD=true"},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "clNN/SpatialMaxPooling.cl", "-DDtype=float"},

    // all kernels
    // Entry{PASSED, FAST, TESTING_FILES "HandBrake/openclkernels.cl", ""},
    // kernels split up
    Entry{PASSED, FAST, TESTING_FILES "HandBrake/frame_h_scale.cl", ""},
    // requires work-group size of 64
    Entry{PENDING_WORK_GROUP_SIZE, FAST, TESTING_FILES "HandBrake/frame_scale.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "HandBrake/hscale_all_opencl.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "HandBrake/hscale_fast_opencl.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "HandBrake/nv12toyuv.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "HandBrake/vscale_all_dither_opencl.cl", ""},
    Entry{PENDING_LLVM_CI, FAST, TESTING_FILES "HandBrake/vscale_all_nodither_opencl.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "HandBrake/vscale_fast_opencl.cl", ""},
    Entry{PENDING_LLVM_CI, FAST, TESTING_FILES "HandBrake/yaif_filter.cl", ""},

    Entry{PENDING_SPIRV_CI, SLOW, TESTING_FILES "bfgminer/diablo.cl", "-DWORKSIZE=8"},
    Entry{PENDING_SPIRV_CI, SLOW, TESTING_FILES "bfgminer/diakgcn.cl", "-DWORKSIZE=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "bfgminer/keccak.cl", "-DWORKSIZE=8"},
    Entry{PASSED, SLOW, TESTING_FILES "bfgminer/phatk.cl", "-DWORKSIZE=8"},
    Entry{PASSED, SLOW, TESTING_FILES "bfgminer/poclbm.cl", "-DWORKSIZE=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "bfgminer/psw.cl",
        "-DWORKSIZE=8 -DCONCURRENT_THREADS=1 -DLOOKUP_GAP=0"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "bfgminer/scrypt.cl",
        "-DWORKSIZE=8 -DCONCURRENT_THREADS=1 -DLOOKUP_GAP=0"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "bfgminer/zuikkis.cl",
        "-DWORKSIZE=8 -DCONCURRENT_THREADS=1 -DLOOKUP_GAP=0"},

    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "rendergirl/FXAA.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "rendergirl/Raytracer.cl", ""},

    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/7z_kernel.cl",
        "-DPLAINTEXT_LENGTH=16 -DHASH_LOOPS=4"},
    // TODO removes too many instructions
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "JohnTheRipper/agile_kernel.cl",
        "-DKEYLEN=16 -DSALTLEN=32 -DOUTLEN=16"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "JohnTheRipper/bf_kernel.cl", "-DWORK_GROUP_SIZE=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "JohnTheRipper/bitlocker_kernel.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "JohnTheRipper/cryptmd5_kernel.cl",
        "-DPLAINTEXT_LENGTH=32"},
    Entry{PASSED, FAST, TESTING_FILES "JohnTheRipper/DES_bs_finalize_keys_kernel.cl", "-DITER_COUNT=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/DES_bs_kernel.cl", "-DITER_COUNT=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/enpass_kernel.cl",
        "-DHASH_LOOPS=4 -DOUTLEN=16"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/gpg_kernel.cl",
        "-DPLAINTEXT_LENGTH=32 -DSALT_LENGTH=32"},
    Entry{
        PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/iwork_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PASSED, FAST, TESTING_FILES "JohnTheRipper/keystore_kernel.cl", "-DPASSLEN=8 -DSALTLEN=16"},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "JohnTheRipper/lotus5_kernel.cl", ""},
    Entry{PENDING_SPIRV_CI, FAST, TESTING_FILES "JohnTheRipper/o5logon_kernel.cl", ""},
    // TODO freezes/hangs llvm-spirv/the calling of it
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "JohnTheRipper/odf_aes_kernel.cl",
        "-DKEYLEN=128 -DOUTLEN=32 -DSALTLEN=16 -DPLAINTEXT_LENGTH=32 -DAES_LEN=32"},
    Entry{PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/pbkdf1_hmac_sha1_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/pbkdf2_hmac_md4_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/pbkdf2_hmac_md5_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/pbkdf2_hmac_sha1_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "JohnTheRipper/pbkdf2_hmac_sha1_unsplit_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4 -DKEYLEN=8 -DSALTLEN=16"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/pbkdf2_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/pbkdf2_ripemd160_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4 -DKEYLEN=8 -DSALTLEN=16"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/pwsafe_kernel.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/rakp_kernel.cl", "-DV_WIDTH=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "JohnTheRipper/rar_kernel.cl",
        "-DPLAINTEXT_LENGTH=16 -DHASH_LOOPS=4 -DUNICODE_LENGTH=1"},
    // TODO see above
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "JohnTheRipper/wpapsk_kernel.cl", "-DHASH_LOOPS=4"},

    Entry{PASSED, FAST, TESTING_FILES "rodinia/backprop_kernel.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "rodinia/bfs-Kernels.cl", ""},
    // XXX sporadically register association errors
    Entry{PASSED, FAST, TESTING_FILES "rodinia/cfd-Kernels.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "rodinia/find_ellipse_kernel.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "rodinia/gaussianElim_kernels.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "rodinia/heartwall_kernel_gpu_opencl.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "rodinia/hotspot_kernel.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "rodinia/kmeans.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "rodinia/lud_kernel.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "rodinia/myocyte_kernel_gpu_opencl.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "rodinia/nearestNeighbor_kernel.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "rodinia/nw.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "rodinia/particle_single.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "rodinia/pathfinder_kernels.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "rodinia/srad_kernel_gpu_opencl.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "rodinia/streamcluster-Kernels.cl", ""}, // 64-bit integer
    Entry{PENDING_LLVM | PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "rodinia/track_ellipse_kernel.cl", ""},

    Entry{PENDING_SPIRV, FAST, TESTING_FILES "NVIDIA/BitonicSort.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_SPIRV, FAST, TESTING_FILES "NVIDIA/BitonicSort_b.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/BlackScholes.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/BoxFilter.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_WORK_GROUP_SIZE, FAST, TESTING_FILES "NVIDIA/ConvolutionSeparable.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DKERNEL_RADIUS=8 -DROWS_BLOCKDIM_X=16 -DROWS_BLOCKDIM_Y=4 -DCOLUMNS_BLOCKDIM_X=16 "
        "-DCOLUMNS_BLOCKDIM_Y=8 -DROWS_RESULT_STEPS=4 -DROWS_HALO_STEPS=1 -DCOLUMNS_RESULT_STEPS=4 "
        "-DCOLUMNS_HALO_STEPS=1"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/cyclic_kernels.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_WORK_GROUP_SIZE, FAST, TESTING_FILES "NVIDIA/DCT8x8.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/DotProduct.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, TESTING_FILES "NVIDIA/DXTCompression.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/FDTD3d.cl", "-DLOCAL_SIZE_LIMIT=8 -DRADIUS=8 -DMAXWORKY=2 -DMAXWORKX=4"},
    Entry{PENDING_WORK_GROUP_SIZE, FAST, TESTING_FILES "NVIDIA/Histogram64.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DHISTOGRAM64_WORKGROUP_SIZE=32 -DLOCAL_MEMORY_BANKS=8 -DMERGE_WORKGROUP_SIZE=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/Histogram256.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DLOG2_WARP_SIZE=2U -DWARP_COUNT=3 -DMERGE_WORKGROUP_SIZE=8"},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "NVIDIA/marchingCubes_kernel.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/matrixMul.cl", "-DLOCAL_SIZE_LIMIT=8 -DBLOCK_SIZE=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/MedianFilter.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/MersenneTwister.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/oclMatVecMul.cl", "-DLOCAL_SIZE_LIMIT=8"},
    // TODO has problems with command line parsing
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, TESTING_FILES "NVIDIA/oclNbodyKernel.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DREAL3=float3 -DREAL4=float4 -DREAL=float \"-DZERO3=(float3)0\""},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/oclReduction_kernel.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DT=float -DblockSize=128 -DnIsPow2=1"},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "NVIDIA/oclSimpleTexture3D_kernel.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_LLVM_CI, FAST, TESTING_FILES "NVIDIA/Particles.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/pcr_kernels.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/PostprocessGL.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/QuasirandomGenerator.cl", "-DLOCAL_SIZE_LIMIT=8"},
    // TODO crashes the tests with "corrupted double-linked list" almost every time
    // Entry{PASSED, FAST, TESTING_FILES "NVIDIA/RadixSort.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/RecursiveGaussian.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_LLVM_CI | PENDING_SPIRV_CI, FAST, TESTING_FILES "NVIDIA/Scan.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DWORKGROUP_SIZE=8"},
    Entry{PENDING_WORK_GROUP_SIZE, FAST, TESTING_FILES "NVIDIA/Scan_b.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/simpleGL.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/simpleMultiGPU.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/SobelFilter.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/sweep_kernels.cl", "-DLOCAL_SIZE_LIMIT=8 -Dsystem_size=16 -DBLOCK_DIM=3"},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "NVIDIA/texture_2d.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "NVIDIA/texture_cube.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/texture_volume.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/transpose.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{EMULATED, FAST, TESTING_FILES "NVIDIA/VectorAdd.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/VectorHypot.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/Viterbi.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, TESTING_FILES "NVIDIA/volumeRender.cl", "-DLOCAL_SIZE_LIMIT=8"},

    Entry{PASSED, FAST, TESTING_FILES "mixbench/mix_kernels_ro.cl",
        "-Dblockdim=8 -Dclass_T=float -DELEMENTS_PER_THREAD=32 -DCOMPUTE_ITERATIONS=32 -DFUSION_DEGREE=8"},
    Entry{PENDING_LLVM_CI, FAST, TESTING_FILES "mixbench/mix_kernels.cl",
        "-Dblockdim=8 -Dclass_T=float -DELEMENTS_PER_THREAD=32 -DCOMPUTE_ITERATIONS=32 -DFUSION_DEGREE=8 "
        "-Dmemory_ratio=8"},

    Entry{PASSED, FAST, TESTING_FILES "OpenCV/convert.cl",
        "-DNO_SCALE -DsrcT=uint8 -DdstT=float8 -DconvertToDT=convert_float8"},
    Entry{PASSED, FAST, TESTING_FILES "OpenCV/copymakeborder.cl",
        "-Dcn=4 -DBORDER_REPLICATE -DST=uint -DrowsPerWI=16 -DT=int4"},
    Entry{PASSED, FAST, TESTING_FILES "OpenCV/copyset.cl", "-Dcn=8 -DdstT=float8 -DrowsPerWI=32 -DdstT1=float4"},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "OpenCV/cvtclr_dx.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "OpenCV/flip.cl", "-DT=short16 -DPIX_PER_WI_Y=4"},
    Entry{PASSED, FAST, TESTING_FILES "OpenCV/gemm.cl", "-DT=float8 -DLOCAL_SIZE=16 -DWT=float8 -DT1=float"},
    Entry{PASSED, FAST, TESTING_FILES "OpenCV/inrange.cl", "-Dcn=4 -DsrcT1=uint -Dkercn=4 -DHAVE_SCALAR -DcolsPerWI=8"},
    Entry{PASSED, FAST, TESTING_FILES "OpenCV/lut.cl", "-Dlcn=1 -Ddcn=3 -DdstT=uint8 -DsrcT=short16"},
    Entry{EMULATED | PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "OpenCV/meanstddev.cl",
        "-DdstT=float -DWGS2_ALIGNED=4 -Dcn=4 -DsqdstT=float -DsrcT=uint -DconvertToDT=convert_float "
        "-DconvertToSDT=convert_float -DWGS=8"},
    Entry{PASSED, FAST, TESTING_FILES "OpenCV/minmaxloc.cl",
        "-DWGS2_ALIGNED=3 -DMINMAX_STRUCT_ALIGNMENT=16 -Dkercn=4 -DdstT=float4 -DWGS=8 -DsrcT=int4 "
        "-DconvertToDT=convert_float4 -DsrcT1=int"},
    // TODO OpenCV, cltorch, blender, x264, hashcat

    Entry{PASSED, FAST, TESTING_FILES "boost-compute/adjacent_difference.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "boost-compute/adjacent_find.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/copy_on_device.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/linear_congruential_engine.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, TESTING_FILES "boost-compute/test_accumulate.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_any_all_none_of.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_binary_search.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_closure.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_count.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_extrema.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_function.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_gather.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_inner_product.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "boost-compute/test_insertion_sort.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_iota.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_lambda.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_radix_sort.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_reduce_by_key.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_search.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_transform1.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_transform2.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_valarray.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/test_vector.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/threefry_engine.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "boost-compute/user_defined_types.cl", ""},

    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/abs_diff.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "OpenCL-CTS/clamp.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "OpenCL-CTS/cross_product.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/explicit_s2v_char8.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/kernel_memory_alignments.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/parameter_types.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "OpenCL-CTS/pointer_cast.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/quick_1d_explicit_load.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/shuffle_builtin_dual_input.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/shuffle_copy.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "OpenCL-CTS/sub_sat.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/test_vload.cl", ""},
    Entry{EMULATED, FAST, TESTING_FILES "OpenCL-CTS/uchar_compare.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/vload_private.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/vstore_private.cl", ""},
    Entry{PASSED, FAST, TESTING_FILES "OpenCL-CTS/work_item_functions.cl", ""},

    Entry{PENDING_IMAGE, FAST, TESTING_FILES "FFmpeg/overlay.cl", ""},
    Entry{PENDING_IMAGE, FAST, TESTING_FILES "FFmpeg/unsharp.cl", ""},

    Entry{PASSED, FAST, TESTING_FILES "OpenCL-caffe/caffe_gpu_memset.cl", ""},
};

RegressionTest::RegressionTest(
    const vc4c::Configuration& config, vc4c::Frontend frontend, bool onlyRegressions, bool onlyFast) :
    config(config)
{
    for(const auto& tuple : allKernels)
    {
        if((std::get<0>(tuple) & DISABLED_IMAGE) || std::get<0>(tuple) & DISABLED_GROUP_SIZE)
            ; // skip
        else if(isSupported(std::get<0>(tuple), frontend) && (!onlyFast || std::get<1>(tuple) == FAST))
        {
            TEST_ADD_THREE_ARGUMENTS(RegressionTest::testRegression, static_cast<std::string>(std::get<2>(tuple)),
                static_cast<std::string>(std::get<3>(tuple)), static_cast<vc4c::Frontend>(frontend));
        }
        else if(!onlyRegressions && std::get<1>(tuple) == FAST)
        {
            TEST_ADD_THREE_ARGUMENTS(RegressionTest::testPending, static_cast<std::string>(std::get<2>(tuple)),
                static_cast<std::string>(std::get<3>(tuple)), static_cast<vc4c::Frontend>(frontend));
        }
        else if(!onlyRegressions && !onlyFast && std::get<1>(tuple) == SLOW)
        {
            // TEST_ADD_THREE_ARGUMENTS(RegressionTest::testSlowPending, static_cast<std::string>(std::get<2>(tuple)),
            // static_cast<std::string>(std::get<3>(tuple)), static_cast<vc4c::Frontend>(frontend));
        }
    }
    TEST_ADD(RegressionTest::printProfilingInfo);
}

RegressionTest::~RegressionTest()
{
    // out-of-line virtual destructor
}

void RegressionTest::testRegression(std::string clFile, std::string options, vc4c::Frontend frontend)
{
    if(frontend != Frontend::DEFAULT)
        config.frontend = frontend;
    CompilationData in(clFile, SourceType::OPENCL_C);
    printf("%s\n", clFile.data());

    const std::size_t numBytes = Compiler::compile(in, config, options).second;
    TEST_ASSERT(numBytes > 0)
}

void RegressionTest::testPending(std::string clFile, std::string options, vc4c::Frontend frontend)
{
    testRegression(clFile, options, frontend);
}

void RegressionTest::testSlowPending(std::string clFile, std::string options, vc4c::Frontend frontend)
{
    testRegression(clFile, options, frontend);
}

void RegressionTest::printProfilingInfo()
{
#ifndef NDEBUG
    vc4c::profiler::dumpProfileResults(true);
#endif
}
