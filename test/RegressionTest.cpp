/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "RegressionTest.h"
#include "../src/Profiler.h"
#include "../src/helper.h"

#include <fstream>

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
    PENDING_SPIRV = 1u << 2u
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
static constexpr auto PENDING_IMAGE = PENDING_LLVM | PENDING_SPIRV;
// Fails on the corresponding front-end in the CI but not locally
static constexpr auto PENDING_LLVM_CI = PENDING_LLVM;
static constexpr auto PENDING_SPIRV_CI = PENDING_SPIRV;
static constexpr uint8_t SLOW = true;
static constexpr uint8_t FAST = false;

using Entry = std::tuple<RegressionStatus, uint8_t, std::string, std::string>;

static std::vector<Entry> allKernels = {
    Entry{PASSED, FAST, "./example/fft2_2.cl", ""},
    Entry{EMULATED, FAST, "./example/fibonacci.cl", ""},
    Entry{EMULATED, FAST, "./example/hello_world.cl", ""},
    Entry{EMULATED, FAST, "./example/hello_world_vector.cl", ""},
    Entry{EMULATED, FAST, "./example/test.cl", ""},
    Entry{EMULATED, FAST, "./example/test_instructions.cl", ""},
    Entry{EMULATED, FAST, "./example/test_prime.cl", ""},
    Entry{EMULATED | PENDING_SPIRV_PRECOMPILER, FAST, "./example/md5.cl", ""},
    Entry{EMULATED | PENDING_SPIRV_PRECOMPILER, FAST, "./example/SHA-256.cl", ""},
    Entry{PASSED, FAST, "./example/test_cl.cl", ""},
    Entry{PASSED, FAST, "./example/histogram.cl", "-DTYPE=char -DMAX_VALUE=127 -DMIN_VALUE=-128"},
    Entry{PASSED, FAST, "./example/histogram.cl", "-DTYPE=uchar -DMAX_VALUE=255 -DMIN_VALUE=0"},
    Entry{PASSED, FAST, "./example/histogram.cl", "-DTYPE=short -DMAX_VALUE=32767 -DMIN_VALUE=-32768"},

    Entry{EMULATED, FAST, "./testing/test_async_copy.cl", ""},
    Entry{PASSED, FAST, "./testing/test_atomic.cl", ""},
    Entry{PASSED, FAST, "./testing/test_barrier_fence.cl", ""},
    Entry{EMULATED, FAST, "./testing/test_barrier.cl", ""},
    Entry{EMULATED, FAST, "./testing/test_branches.cl", ""},
    Entry{PASSED, FAST, "./testing/test_builtins.cl", ""},
    Entry{PASSED, FAST, "./testing/test_common.cl", ""},
    // XXX unsupported explicit rounding modes on type conversions
    Entry{PENDING_SPIRV, FAST, "./testing/test_conversions.cl", ""},
    Entry{PASSED, FAST, "./testing/test_float.cl", ""},
    Entry{PASSED, FAST, "./testing/test_geometric.cl", ""},
    Entry{PENDING_SPIRV_CI, FAST, "./testing/test_global_data.cl", ""},
    Entry{EMULATED, FAST, "./testing/test_hashes.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/test_images.cl", ""},
    Entry{PASSED, FAST, "./testing/test_immediates.cl", ""},
    Entry{PASSED, FAST, "./testing/test_int.cl", ""},
    Entry{PASSED, FAST, "./testing/test_integer.cl", ""},
    Entry{PASSED, FAST, "./testing/test_math.cl", ""},
    Entry{EMULATED, FAST, "./testing/test_other.cl", ""},
    Entry{EMULATED | PENDING_SPIRV_PRECOMPILER, FAST, "./testing/test_partial_md5.cl", ""},
    Entry{PASSED, FAST, "./testing/test_sfu.cl", ""},
    Entry{PASSED, FAST, "./testing/test_shuffle.cl", ""},
    Entry{EMULATED, FAST, "./testing/test_struct.cl", ""},
    Entry{EMULATED, FAST, "./testing/test_vector.cl", ""},
    Entry{PASSED, FAST, "./testing/test_vector3_layout.cl", ""},
    Entry{EMULATED, FAST, "./testing/test_vectorization.cl", ""},
    Entry{PASSED, FAST, "./testing/test_vpm_read.cl", ""},
    Entry{PASSED, FAST, "./testing/test_vpm_write.cl", ""},
    Entry{EMULATED, FAST, "./testing/test_work_item.cl", ""},

    Entry{PASSED, FAST, "./testing/deepCL/activate.cl", "-DLINEAR"},
    Entry{PASSED, FAST, "./testing/deepCL/addscalar.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/applyActivationDeriv.cl", ""},
    Entry{PASSED, SLOW, "./testing/deepCL/backpropweights.cl",
        "-DgNumFilters=4 -DgInputPlanes=2 -DgOutputPlanes=2 -DgOutputSize=16 -DgInputSize=16 -DgFilterSize=4 "
        "-DgFilterSizeSquared=16 -DgMargin=1"},
    Entry{PASSED, FAST, "./testing/deepCL/copy.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/forward_fc.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/inv.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/memset.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/per_element_add.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/per_element_mult.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/SGD.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/sqrt.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/squared.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/backpropweights_byrow.cl",
        "-DgInputSize=16 -DgOutputSize=16 -DgFilterSize=4 -DgFilterSizeSquared=16 -DgNumOutputPlanes=4 -DgMargin=1 "
        "-DgNumInputPlanes=4 -DinputRow=0"},
    Entry{PASSED, FAST, "./testing/deepCL/BackpropWeightsScratchLarge.cl",
        "-DgFilterSize=4 -DgFilterSizeSquared=16 -DgOutputSize=16 -DgInputSize=16 -DgMargin=1 "
        "-DgInputStripeInnerSize=2 -DgInputStripeOuterSize=3 -DgOutputSizeSquared=16 -DgInputStripeMarginSize=1 "
        "-DgNumStripes=16 -DgOutputStripeSize=16 -DgOutputStripeNumRows=4 -DgNumFilters=4 -DgInputPlanes=4 "
        "-DgInputSizeSquared=16"},
    Entry{PASSED, FAST, "./testing/deepCL/backward.cl",
        "-DgFilterSize=4 -DgFilterSizeSquared=16 -DgOutputSize=16 -DgInputSize=16 -DgMargin=1 "
        "-DgInputStripeInnerSize=2 -DgInputStripeOuterSize=3 -DgOutputSizeSquared=16 -DgInputStripeMarginSize=1 "
        "-DgNumStripes=16 -DgOutputStripeSize=16 -DgOutputStripeNumRows=4 -DgNumFilters=4 -DgInputPlanes=4 "
        "-DgInputSizeSquared=16"},
    Entry{PASSED, FAST, "./testing/deepCL/backward_cached.cl",
        "-DgFilterSize=4 -DgFilterSizeSquared=16 -DgOutputSize=16 -DgInputSize=16 -DgMargin=1 "
        "-DgInputStripeInnerSize=2 -DgInputStripeOuterSize=3 -DgOutputSizeSquared=16 -DgInputStripeMarginSize=1 "
        "-DgNumStripes=16 -DgOutputStripeSize=16 -DgOutputStripeNumRows=4 -DgNumFilters=4 -DgInputPlanes=4 "
        "-DgInputSizeSquared=16"},
    Entry{PASSED, FAST, "./testing/deepCL/copyBlock.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/copyLocal.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/deepCL/forward.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/forward1.cl",
        "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 "
        "-DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16"},
    Entry{PASSED, FAST, "./testing/deepCL/forward2.cl", "-DgWorkgroupSize=8"},
    Entry{PASSED, FAST, "./testing/deepCL/forward3.cl",
        "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 "
        "-DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16 "
        "-DgPadZeros=true -DgInputPlanes=8"},
    Entry{PASSED, FAST, "./testing/deepCL/forward4.cl", ""},
    Entry{PENDING_SPIRV_CI, FAST, "./testing/deepCL/forward_byinputplane.cl",
        "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 "
        "-DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16 "
        "-DgPadZeros=true -DgInputPlanes=8"},
    Entry{PASSED, FAST, "./testing/deepCL/forward_fc_wgperrow.cl",
        "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 "
        "-DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16 "
        "-DgPadZeros=true -DgInputPlanes=8"},
    Entry{PASSED, FAST, "./testing/deepCL/forwardfc_workgroupperfilterplane.cl",
        "-DgFilterSizeSquared=16 -DgNumInputPlanes=8"},
    Entry{PASSED, FAST, "./testing/deepCL/ids.cl", ""},
    Entry{PASSED, FAST, "./testing/deepCL/pooling.cl",
        "-DgOutputSize=16 -DgOutputSizeSquared=64 -DgNumPlanes=4 -DgPoolingSize=8 -DgInputSize=16 "
        "-DgInputSizeSquared=64"},
    Entry{PASSED, FAST, "./testing/deepCL/PoolingBackwardGpuNaive.cl",
        "-DgOutputSize=16 -DgOutputSizeSquared=64 -DgNumPlanes=4 -DgPoolingSize=8 -DgInputSize=16 "
        "-DgInputSizeSquared=64"},
    Entry{PASSED, FAST, "./testing/deepCL/reduce_segments.cl", ""},

    Entry{PASSED, FAST, "./testing/CLTune/conv_reference.opencl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, "./testing/CLTune/gemm.opencl",
        "-DVWM=16 -DVWN=16 -DMWG=1 -DNWG=1 -DMDIMC=1 -DNDIMC=1 -DKWG=1 -DKWI=1 -Dreal16=float16 -Dreal=float "
        "-DZERO=0.0f"},
    Entry{PASSED, FAST, "./testing/CLTune/gemm_reference.opencl", ""},
    Entry{PASSED, FAST, "./testing/CLTune/multiple_kernels_reference.opencl", ""},
    Entry{PASSED, FAST, "./testing/CLTune/multiple_kernels_unroll.opencl", ""},
    Entry{PASSED, FAST, "./testing/CLTune/simple_kernel.opencl", ""},
    // TODO freezes/hangs llvm-spirv/the calling of it
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/CLTune/conv.opencl",
        "-DVECTOR=16 -DWPTX=256 -DWPTY=64 -DTBX=2 -DTBY=2 -DUNROLL_FACTOR -Dfloatvec=float16"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/CLTune/conv_simple_kernel.opencl",
        "-DVECTOR=16 -DWPTX=256 -DWPTY=64 -DTBX=2 -DTBY=2 -DUNROLL_FACTOR -Dfloatvec=float16"},
    Entry{PASSED, FAST, "./testing/CLTune/multiple_kernels_tiled.opencl",
        "-DVECTOR=16 -DWPTX=256 -DWPTY=64 -DTBX=2 -DTBY=2 -DUNROLL_FACTOR -Dfloatvec=float16 -DTS=8"},

    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/bullet/jointSolver.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/bullet/solveContact.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/bullet/solveFriction.cl", ""},

    Entry{PASSED, SLOW, "./testing/clpeak/compute_integer_kernels.cl", ""},
    Entry{PASSED, FAST, "./testing/clpeak/compute_sp_kernels.cl", ""},
    // XXX Entry{PASSED, FAST, "./testing/clpeak/compute_hp_kernels.cl", "-DHALF_AVAILABLE"},
    Entry{PASSED, FAST, "./testing/clpeak/global_bandwidth_kernels.cl", ""},

    Entry{PASSED, FAST, "./testing/vattenoverhuvudet/calculate_voxel_grid.cl", ""},
    Entry{PASSED, FAST, "./testing/vattenoverhuvudet/integrate_particle_states.cl", ""},
    Entry{PASSED, FAST, "./testing/vattenoverhuvudet/simple_voxel_grid_move.cl", ""},
    Entry{PASSED, FAST, "./testing/vattenoverhuvudet/taskParallel.cl", ""},
    Entry{PASSED, FAST, "./testing/vattenoverhuvudet/update_particle_positions.cl", ""},

    Entry{PENDING_IMAGE, FAST, "./testing/gputools/bilateral2.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/bilateral3.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/bilateralAdapt.cl", ""},
    Entry{PASSED, FAST, "./testing/gputools/bilateral_shared.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/gputools/convolve.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/convolve1.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/convolve2.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/convolve3.cl", ""},
    Entry{PASSED, FAST, "./testing/gputools/convolve_sep.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/correlate_kernels.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/dct_8x8.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/dct_8x8_new.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/dct_8x8x8.cl", ""},
    Entry{PASSED, FAST, "./testing/gputools/minmax_filter.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/nlm2.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/nlm3.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/nlm3_thresh.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/nlmeans.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/nlmeans3d.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/nlmeans_projected.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/nlm_fast.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/nlm_fast3.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/patch_kernel.cl", ""},
    Entry{PASSED, FAST, "./testing/gputools/perlin.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/scale.cl", "-DTYPENAME=float4 -DREAD_IMAGE=read_imagef"},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/transformations.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/gputools/tv_chambolle.cl", ""},

    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/clNN/im2col.cl", ""},
    Entry{PASSED, FAST, "./testing/clNN/SoftMax.cl", "-DSOFTMAX_THREADS=4"},
    Entry{PASSED, FAST, "./testing/clNN/SpatialAveragePooling.cl", "-DDtype=float -DCOUNT_INCLUDE_PAD=true"},
    Entry{PENDING_LLVM, FAST, "./testing/clNN/SpatialMaxPooling.cl", "-DDtype=float"},

    // all kernels
    // Entry{PASSED, FAST, "./testing/HandBrake/openclkernels.cl", ""},
    // kernels split up
    Entry{PASSED, FAST, "./testing/HandBrake/frame_h_scale.cl", ""},
    // requires work-group size of 64
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/HandBrake/frame_scale.cl", ""},
    Entry{PASSED, FAST, "./testing/HandBrake/hscale_all_opencl.cl", ""},
    Entry{PASSED, FAST, "./testing/HandBrake/hscale_fast_opencl.cl", ""},
    Entry{PASSED, FAST, "./testing/HandBrake/nv12toyuv.cl", ""},
    Entry{PENDING_SPIRV, FAST, "./testing/HandBrake/vscale_all_dither_opencl.cl", ""},
    Entry{PENDING_LLVM_CI, FAST, "./testing/HandBrake/vscale_all_nodither_opencl.cl", ""},
    Entry{PASSED, FAST, "./testing/HandBrake/vscale_fast_opencl.cl", ""},
    Entry{PASSED, FAST, "./testing/HandBrake/yaif_filter.cl", ""},

    Entry{PASSED, SLOW, "./testing/bfgminer/diablo.cl", "-DWORKSIZE=8"},
    Entry{PASSED, SLOW, "./testing/bfgminer/diakgcn.cl", "-DWORKSIZE=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/bfgminer/keccak.cl", "-DWORKSIZE=8"},
    Entry{PASSED, SLOW, "./testing/bfgminer/phatk.cl", "-DWORKSIZE=8"},
    Entry{PASSED, SLOW, "./testing/bfgminer/poclbm.cl", "-DWORKSIZE=8"},
    // FIXME SEGFAULTs (stack overflow in #markDepthFirst())
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/bfgminer/psw.cl",
        "-DWORKSIZE=8 -DCONCURRENT_THREADS=1 -DLOOKUP_GAP=0"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/bfgminer/scrypt.cl",
        "-DWORKSIZE=8 -DCONCURRENT_THREADS=1 -DLOOKUP_GAP=0"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/bfgminer/zuikkis.cl",
        "-DWORKSIZE=8 -DCONCURRENT_THREADS=1 -DLOOKUP_GAP=0"},

    Entry{PASSED, FAST, "./testing/rendergirl/FXAA.cl", ""},
    Entry{PENDING_SPIRV_CI, FAST, "./testing/rendergirl/Raytracer.cl", ""},

    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/7z_kernel.cl",
        "-DPLAINTEXT_LENGTH=16 -DHASH_LOOPS=4"},
    // TODO removes too many instructions
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/JohnTheRipper/agile_kernel.cl",
        "-DKEYLEN=16 -DSALTLEN=32 -DOUTLEN=16"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/JohnTheRipper/bf_kernel.cl", "-DWORK_GROUP_SIZE=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/JohnTheRipper/bitlocker_kernel.cl", ""},
    // FIXME SEGFAULTs
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/cryptmd5_kernel.cl", "-DPLAINTEXT_LENGTH=32"},
    Entry{PASSED, FAST, "./testing/JohnTheRipper/DES_bs_finalize_keys_kernel.cl", "-DITER_COUNT=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/DES_bs_kernel.cl", "-DITER_COUNT=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/enpass_kernel.cl", "-DHASH_LOOPS=4 -DOUTLEN=16"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/gpg_kernel.cl",
        "-DPLAINTEXT_LENGTH=32 -DSALT_LENGTH=32"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/iwork_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/keystore_kernel.cl", "-DPASSLEN=8 -DSALTLEN=16"},
    Entry{PENDING_SPIRV, FAST, "./testing/JohnTheRipper/lotus5_kernel.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, "./testing/JohnTheRipper/o5logon_kernel.cl", ""},
    // TODO freezes/hangs llvm-spirv/the calling of it
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/JohnTheRipper/odf_aes_kernel.cl",
        "-DKEYLEN=128 -DOUTLEN=32 -DSALTLEN=16 -DPLAINTEXT_LENGTH=32 -DAES_LEN=32"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/pbkdf1_hmac_sha1_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/pbkdf2_hmac_md4_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/pbkdf2_hmac_md5_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/pbkdf2_hmac_sha1_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/pbkdf2_hmac_sha1_unsplit_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4 -DKEYLEN=8 -DSALTLEN=16"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/pbkdf2_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/pbkdf2_ripemd160_kernel.cl",
        "-DOUTLEN=8 -DHASH_LOOPS=4 -DKEYLEN=8 -DSALTLEN=16"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/pwsafe_kernel.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/rakp_kernel.cl", "-DV_WIDTH=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/JohnTheRipper/rar_kernel.cl",
        "-DPLAINTEXT_LENGTH=16 -DHASH_LOOPS=4 -DUNICODE_LENGTH=1"},
    // TODO see above
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/JohnTheRipper/wpapsk_kernel.cl", "-DHASH_LOOPS=4"},

    Entry{PASSED, FAST, "./testing/rodinia/backprop_kernel.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/bfs-Kernels.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/cfd-Kernels.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/find_ellipse_kernel.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/gaussianElim_kernels.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/rodinia/heartwall_kernel_gpu_opencl.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/hotspot_kernel.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/kmeans.cl", ""},
    /// XXX error in precompilation
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/rodinia/lavaMD_kernel_gpu_opencl.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/lud_kernel.cl", ""},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/rodinia/myocyte_kernel_gpu_opencl.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/nearestNeighbor_kernel.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/nw.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/rodinia/particle_single.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/pathfinder_kernels.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/srad_kernel_gpu_opencl.cl", ""},
    Entry{PASSED, FAST, "./testing/rodinia/streamcluster-Kernels.cl", ""}, // 64-bit integer
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/rodinia/track_ellipse_kernel.cl", ""},

    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/NVIDIA/BitonicSort.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_SPIRV, FAST, "./testing/NVIDIA/BitonicSort_b.cl", ""},
    Entry{PASSED, FAST, "./testing/NVIDIA/BlackScholes.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/BoxFilter.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/NVIDIA/ConvolutionSeparable.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DKERNEL_RADIUS=8 -DROWS_BLOCKDIM_X=16 -DROWS_BLOCKDIM_Y=4 -DCOLUMNS_BLOCKDIM_X=16 "
        "-DCOLUMNS_BLOCKDIM_Y=8 -DROWS_RESULT_STEPS=4 -DROWS_HALO_STEPS=1 -DCOLUMNS_RESULT_STEPS=4 "
        "-DCOLUMNS_HALO_STEPS=1"},
    Entry{PASSED, FAST, "./testing/NVIDIA/cyclic_kernels.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/NVIDIA/DCT8x8.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/DotProduct.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, SLOW, "./testing/NVIDIA/DXTCompression.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/NVIDIA/FDTD3d.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DRADIUS=8 -DMAXWORKY=2 -DMAXWORKX=4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/NVIDIA/Histogram64.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DHISTOGRAM64_WORKGROUP_SIZE=32 -DLOCAL_MEMORY_BANKS=8 -DMERGE_WORKGROUP_SIZE=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/Histogram256.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DLOG2_WARP_SIZE=2U -DWARP_COUNT=3 -DMERGE_WORKGROUP_SIZE=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/NVIDIA/marchingCubes_kernel.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/matrixMul.cl", "-DLOCAL_SIZE_LIMIT=8 -DBLOCK_SIZE=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/MedianFilter.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/MersenneTwister.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/oclMatVecMul.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/NVIDIA/oclNbodyKernel.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DREAL3=float3 -DREAL4=float4 -DREAL=float -DZERO3=(float3)0"},
    Entry{PASSED, FAST, "./testing/NVIDIA/oclReduction_kernel.cl",
        "-DLOCAL_SIZE_LIMIT=8 -DT=float -DblockSize=128 -DnIsPow2=1"},
    Entry{PENDING_IMAGE, FAST, "./testing/NVIDIA/oclSimpleTexture3D_kernel.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_LLVM_CI, FAST, "./testing/NVIDIA/Particles.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/pcr_kernels.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/PostprocessGL.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/QuasirandomGenerator.cl", "-DLOCAL_SIZE_LIMIT=8"},
    // TODO crashes the tests with "corrupted double-linked list" almost every time
    // Entry{PASSED, FAST, "./testing/NVIDIA/RadixSort.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/RecursiveGaussian.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/Scan.cl", "-DLOCAL_SIZE_LIMIT=8 -DWORKGROUP_SIZE=8"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/NVIDIA/Scan_b.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/simpleGL.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/simpleMultiGPU.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/SobelFilter.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_SPIRV, FAST, "./testing/NVIDIA/sweep_kernels.cl",
        "-DLOCAL_SIZE_LIMIT=8 -Dsystem_size=16 -DBLOCK_DIM=3"},
    Entry{PENDING_IMAGE, FAST, "./testing/NVIDIA/texture_2d.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PENDING_IMAGE, FAST, "./testing/NVIDIA/texture_cube.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/texture_volume.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/transpose.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{EMULATED, FAST, "./testing/NVIDIA/VectorAdd.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/VectorHypot.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/Viterbi.cl", "-DLOCAL_SIZE_LIMIT=8"},
    Entry{PASSED, FAST, "./testing/NVIDIA/volumeRender.cl", "-DLOCAL_SIZE_LIMIT=8"},

    Entry{PENDING_SPIRV, FAST, "./testing/mixbench/mix_kernels_ro.cl",
        "-Dblockdim=8 -Dclass_T=float -DELEMENTS_PER_THREAD=32 -DCOMPUTE_ITERATIONS=32 -DFUSION_DEGREE=8"},
    Entry{PASSED, FAST, "./testing/mixbench/mix_kernels.cl",
        "-Dblockdim=8 -Dclass_T=float -DELEMENTS_PER_THREAD=32 -DCOMPUTE_ITERATIONS=32 -DFUSION_DEGREE=8 "
        "-Dmemory_ratio=8"},

    Entry{PASSED, FAST, "./testing/OpenCV/convert.cl",
        "-DNO_SCALE -DsrcT=uint8 -DdstT=float8 -DconvertToDT=convert_float8"},
    Entry{PASSED, FAST, "./testing/OpenCV/copymakeborder.cl",
        "-Dcn=4 -DBORDER_REPLICATE -DST=uint -DrowsPerWI=16 -DT=int4"},
    Entry{PASSED, FAST, "./testing/OpenCV/copyset.cl", "-Dcn=8 -DdstT=float8 -DrowsPerWI=32 -DdstT1=float4"},
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/OpenCV/cvtclr_dx.cl", ""},
    // XXX indices in access chain do not match
    Entry{PENDING_LLVM | PENDING_SPIRV, FAST, "./testing/OpenCV/fft.cl",
        "-DFT=float -DCT=float2 -DLOCAL_SIZE=32 -Dkercn=4 -DRADIX_PROCESS"},
    Entry{PASSED, FAST, "./testing/OpenCV/flip.cl", "-DT=short16 -DPIX_PER_WI_Y=4"},
    Entry{PASSED, FAST, "./testing/OpenCV/gemm.cl", "-DT=float8 -DLOCAL_SIZE=16 -DWT=float8 -DT1=float"},
    Entry{PASSED, FAST, "./testing/OpenCV/inrange.cl", "-Dcn=4 -DsrcT1=uint -Dkercn=4 -DHAVE_SCALAR -DcolsPerWI=8"},
    Entry{PASSED, FAST, "./testing/OpenCV/lut.cl", "-Dlcn=1 -Ddcn=3 -DdstT=uint8 -DsrcT=short16"},
    Entry{PASSED, FAST, "./testing/OpenCV/meanstddev.cl",
        "-DdstT=float -DWGS2_ALIGNED=4 -Dcn=4 -DsqdstT=float -DsrcT=uint -DconvertToDT=convert_float "
        "-DconvertToSDT=convert_float -DWGS=8"},
    Entry{PASSED, FAST, "./testing/OpenCV/minmaxloc.cl",
        "-DWGS2_ALIGNED=3 -DMINMAX_STRUCT_ALIGNMENT=16 -Dkercn=4 -DdstT=float4 -DWGS=8 -DsrcT=int4 "
        "-DconvertToDT=convert_float4 -DsrcT1=int"},
    // TODO OpenCV, cltorch, blender, x264, hashcat

    Entry{PASSED, FAST, "./testing/boost-compute/adjacent_difference.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/adjacent_find.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/copy_on_device.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/linear_congruential_engine.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, "./testing/boost-compute/test_accumulate.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_any_all_none_of.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_binary_search.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_closure.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_count.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_extrema.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_function.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_gather.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_inner_product.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_insertion_sort.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_iota.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_lambda.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_radix_sort.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_reduce_by_key.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_search.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_transform1.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_transform2.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_valarray.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/test_vector.cl", ""},
    Entry{PENDING_SPIRV_PRECOMPILER, FAST, "./testing/boost-compute/threefry_engine.cl", ""},
    Entry{PASSED, FAST, "./testing/boost-compute/user_defined_types.cl", ""},

    Entry{PASSED, FAST, "./testing/OpenCL-CTS/abs_diff.cl", ""},
    Entry{EMULATED, FAST, "./testing/OpenCL-CTS/clamp.cl", ""},
    Entry{EMULATED, FAST, "./testing/OpenCL-CTS/cross_product.cl", ""},
    Entry{PASSED, FAST, "./testing/OpenCL-CTS/explicit_s2v_char8.cl", ""},
    Entry{PASSED, FAST, "./testing/OpenCL-CTS/kernel_memory_alignments.cl", ""},
    Entry{PASSED, FAST, "./testing/OpenCL-CTS/parameter_types.cl", ""},
    Entry{EMULATED, FAST, "./testing/OpenCL-CTS/pointer_cast.cl", ""},
    Entry{PASSED, FAST, "./testing/OpenCL-CTS/quick_1d_explicit_load.cl", ""},
    Entry{PASSED, FAST, "./testing/OpenCL-CTS/shuffle_builtin_dual_input.cl", ""},
    Entry{PASSED, FAST, "./testing/OpenCL-CTS/shuffle_copy.cl", ""},
    Entry{EMULATED, FAST, "./testing/OpenCL-CTS/sub_sat.cl", ""},
    Entry{PASSED, FAST, "./testing/OpenCL-CTS/test_vload.cl", ""},
    Entry{EMULATED, FAST, "./testing/OpenCL-CTS/uchar_compare.cl", ""},
    Entry{PASSED, FAST, "./testing/OpenCL-CTS/vload_private.cl", ""},
    Entry{PASSED, FAST, "./testing/OpenCL-CTS/vstore_private.cl", ""},
    Entry{PASSED, FAST, "./testing/OpenCL-CTS/work_item_functions.cl", ""},

    Entry{PENDING_IMAGE, FAST, "./testing/FFmpeg/overlay.cl", ""},
    Entry{PENDING_IMAGE, FAST, "./testing/FFmpeg/unsharp.cl", ""},

    Entry{PASSED, FAST, "./testing/OpenCL-caffe/caffe_gpu_memset.cl", ""},
};

RegressionTest::RegressionTest(
    const vc4c::Configuration& config, vc4c::Frontend frontend, bool onlyRegressions, bool onlyFast) :
    config(config)
{
    for(const auto& tuple : allKernels)
    {
        if(isSupported(std::get<0>(tuple), frontend) && (!onlyFast || std::get<1>(tuple) == FAST))
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
    std::ostringstream out;
    std::ifstream in(clFile);
    printf("%s\n", clFile.data());

    const std::size_t numBytes = Compiler::compile(in, out, config, options, clFile);

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
#if DEBUG_MODE
    vc4c::profiler::dumpProfileResults(true);
#endif
}
