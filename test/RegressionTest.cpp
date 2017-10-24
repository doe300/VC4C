/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */


#include "RegressionTest.h"
#include "../src/Profiler.h"

#include <fstream>

using namespace vc4c;

static constexpr uint8_t PASSED = true;
static constexpr uint8_t PENDING = false;
static constexpr uint8_t SLOW = true;
static constexpr uint8_t FAST = false;

static std::vector<std::tuple<uint8_t, uint8_t, std::string, std::string>> allKernels =
{
		{PASSED, FAST, "./example/fft2_2.cl", ""},
		{PASSED, FAST, "./example/fibonacci.cl", ""},
		{PASSED, FAST, "./example/fibonacci.spt", ""},
		{PASSED, FAST, "./example/fibonacci_vector.ir", ""},
		{PASSED, FAST, "./example/hello_world.cl", ""},
		{PASSED, FAST, "./example/hello_world_vector.cl", ""},
		{PASSED, FAST, "./example/test.cl", ""},
		{PASSED, FAST, "./example/test_instructions.cl", ""},
		{PASSED, FAST, "./example/test_prime.cl", ""},
		{PASSED, FAST, "./example/md5.cl", ""},
		{PASSED, FAST, "./example/SHA-256.cl", ""},
		{PASSED, FAST, "./example/test_cl.cl", ""},
    
		{PASSED, FAST, "./testing/test_barrier_fence.cl", ""},
		{PASSED, FAST, "./testing/test_branches.cl", ""},
		{PENDING, FAST, "./testing/test_builtins.cl", ""},
		{PASSED, FAST, "./testing/test_float.cl", ""},
		{PENDING, FAST, "./testing/test_images.cl", ""},
		{PASSED, FAST, "./testing/test_int.cl", ""},
		{PASSED, FAST, "./testing/test_other.cl", ""},
		{PASSED, FAST, "./testing/test_sfu.cl", ""},
		{PASSED, FAST, "./testing/test_struct.cl", ""},
		{PASSED, FAST, "./testing/test_vector.cl", ""},
		{PASSED, FAST, "./testing/test_vector3_layout.cl", ""},
		{PASSED, FAST, "./testing/test_vpm_read.cl", ""},
		{PASSED, FAST, "./testing/test_vpm_write.cl", ""},
		{PASSED, FAST, "./testing/test_work_item.cl", ""},
    
		{PASSED, FAST, "./testing/deepCL/activate.cl", "-DLINEAR"},
		{PASSED, FAST, "./testing/deepCL/addscalar.cl", ""},
		{PASSED, FAST, "./testing/deepCL/applyActivationDeriv.cl", ""},
		{PASSED, SLOW, "./testing/deepCL/backpropweights.cl", "-DgNumFilters=4 -DgInputPlanes=2 -DgOutputPlanes=2 -DgOutputSize=16 -DgInputSize=16 -DgFilterSize=4 -DgFilterSizeSquared=16 -DgMargin=1"},
		{PASSED, FAST, "./testing/deepCL/copy.cl", ""},
		{PASSED, FAST, "./testing/deepCL/forward_fc.cl", ""},
		{PASSED, FAST, "./testing/deepCL/inv.cl", ""},
		{PASSED, FAST, "./testing/deepCL/memset.cl", ""},
		{PASSED, FAST, "./testing/deepCL/per_element_add.cl", ""},
		{PASSED, FAST, "./testing/deepCL/per_element_mult.cl", ""},
		{PASSED, FAST, "./testing/deepCL/SGD.cl", ""},
		{PASSED, FAST, "./testing/deepCL/sqrt.cl", ""},
		{PASSED, FAST, "./testing/deepCL/squared.cl", ""},
		{PASSED, FAST, "./testing/deepCL/backpropweights_byrow.cl", "-DgInputSize=16 -DgOutputSize=16 -DgFilterSize=4 -DgFilterSizeSquared=16 -DgNumOutputPlanes=4 -DgMargin=1 -DgNumInputPlanes=4 -DinputRow=0"},
		{PENDING, FAST, "./testing/deepCL/BackpropWeightsScratchLarge.cl", "-DgFilterSize=4 -DgFilterSizeSquared=16 -DgOutputSize=16 -DgInputSize=16 -DgMargin=1 -DgInputStripeInnerSize=2 -DgInputStripeOuterSize=3 -DgOutputSizeSquared=16 -DgInputStripeMarginSize=1 -DgNumStripes=16 -DgOutputStripeSize=16 -DgOutputStripeNumRows=4 -DgNumFilters=4 -DgInputPlanes=4 -DgInputSizeSquared=16"},
		{PASSED, FAST, "./testing/deepCL/backward.cl", "-DgFilterSize=4 -DgFilterSizeSquared=16 -DgOutputSize=16 -DgInputSize=16 -DgMargin=1 -DgInputStripeInnerSize=2 -DgInputStripeOuterSize=3 -DgOutputSizeSquared=16 -DgInputStripeMarginSize=1 -DgNumStripes=16 -DgOutputStripeSize=16 -DgOutputStripeNumRows=4 -DgNumFilters=4 -DgInputPlanes=4 -DgInputSizeSquared=16"},
		{PASSED, FAST, "./testing/deepCL/backward_cached.cl", "-DgFilterSize=4 -DgFilterSizeSquared=16 -DgOutputSize=16 -DgInputSize=16 -DgMargin=1 -DgInputStripeInnerSize=2 -DgInputStripeOuterSize=3 -DgOutputSizeSquared=16 -DgInputStripeMarginSize=1 -DgNumStripes=16 -DgOutputStripeSize=16 -DgOutputStripeNumRows=4 -DgNumFilters=4 -DgInputPlanes=4 -DgInputSizeSquared=16"},
		{PASSED, FAST, "./testing/deepCL/copyBlock.cl", ""},
		{PASSED, FAST, "./testing/deepCL/copyLocal.cl", ""},
		{PASSED, FAST, "./testing/deepCL/forward.cl", ""},
		{PASSED, FAST, "./testing/deepCL/forward1.cl", "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 -DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16"},
		{PASSED, FAST, "./testing/deepCL/forward2.cl", "-DgWorkgroupSize=8"},
		{PASSED, FAST, "./testing/deepCL/forward3.cl", "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 -DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16 -DgPadZeros=true -DgInputPlanes=8"},
		{PASSED, FAST, "./testing/deepCL/forward4.cl", ""},
		{PENDING, FAST, "./testing/deepCL/forward_byinputplane.cl", "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 -DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16 -DgPadZeros=true -DgInputPlanes=8"},
		{PASSED, FAST, "./testing/deepCL/forward_fc_wgperrow.cl", "-DgHalfFilterSize=8 -DgInputSize=16 -DgOutputSize=16 -DgFilterSizeSquared=64 -DgNumFilters=4 -DgOutputSizeSquared=64 -DgInputSizeSquared=64 -DgNumInputPlanes=4 -DgEven=2 -DgFilterSize=16 -DgPadZeros=true -DgInputPlanes=8"},
		{PASSED, FAST, "./testing/deepCL/forwardfc_workgroupperfilterplane.cl", "-DgFilterSizeSquared=16 -DgNumInputPlanes=8"},
		{PASSED, FAST, "./testing/deepCL/ids.cl", ""},
		{PASSED, FAST, "./testing/deepCL/pooling.cl", "-DgOutputSize=16 -DgOutputSizeSquared=64 -DgNumPlanes=4 -DgPoolingSize=8 -DgInputSize=16 -DgInputSizeSquared=64"},
		{PASSED, FAST, "./testing/deepCL/PoolingBackwardGpuNaive.cl", "-DgOutputSize=16 -DgOutputSizeSquared=64 -DgNumPlanes=4 -DgPoolingSize=8 -DgInputSize=16 -DgInputSizeSquared=64"},
		{PASSED, FAST, "./testing/deepCL/reduce_segments.cl", ""},
    
		{PASSED, FAST, "./testing/CLTune/conv_reference.opencl", ""},
		{PASSED, FAST, "./testing/CLTune/gemm.opencl", "-DVWM=16 -DVWN=16 -DMWG=1 -DNWG=1 -DMDIMC=1 -DNDIMC=1 -DKWG=1 -DKWI=1 -Dreal16=float16 -Dreal=float -DZERO=0.0f"},
		{PASSED, FAST, "./testing/CLTune/gemm_reference.opencl", ""},
		{PASSED, FAST, "./testing/CLTune/multiple_kernels_reference.opencl", ""},
		{PASSED, FAST, "./testing/CLTune/multiple_kernels_unroll.opencl", ""},
		{PASSED, FAST, "./testing/CLTune/simple_kernel.opencl", ""},
		//TODO freezes/hangs llvm-spirv/the calling of it
		{PENDING, SLOW, "./testing/CLTune/conv.opencl", "-DVECTOR=16 -DWPTX=256 -DWPTY=64 -DTBX=2 -DTBY=2 -DUNROLL_FACTOR -Dfloatvec=float16"},
		{PENDING, SLOW, "./testing/CLTune/conv_simple_kernel.opencl", "-DVECTOR=16 -DWPTX=256 -DWPTY=64 -DTBX=2 -DTBY=2 -DUNROLL_FACTOR -Dfloatvec=float16"},
		{PASSED, FAST, "./testing/CLTune/multiple_kernels_tiled.opencl", "-DVECTOR=16 -DWPTX=256 -DWPTY=64 -DTBX=2 -DTBY=2 -DUNROLL_FACTOR -Dfloatvec=float16 -DTS=8"},

		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/CodeGenOpenCL/spir/metadata/access_qualifier/images/read_only.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/CodeGenOpenCL/addr-space-struct-arg.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/CodeGenOpenCL/astype.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/CodeGenOpenCL/constant-addr-space-globals.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/CodeGenOpenCL/kernel-attributes.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/CodeGenOpenCL/opencl_types.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/CodeGenSPIRV/sampler_t.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/CodeGenOpenCL/vector_odd.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/SemaOpenCL/array-parameters.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/SemaOpenCL/extern.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/SemaOpenCL/str_literals.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/SemaOpenCL/warn-missing-prototypes.cl", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/tools/clang/test/SemaOpenCL/warn-potential-abiguity.cl", ""},
		{PENDING, FAST, "/opt/SPIRV-LLVM/test/SPIRV/transcoding/extract_insert_value.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/transcoding/OpConstantBool.ll", ""},
		{PENDING, FAST, "/opt/SPIRV-LLVM/test/SPIRV/transcoding/OpPhi_ArgumentsPlaceholders.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/transcoding/RecursiveType.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/builtin_vars-decorate.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/empty.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/ExecutionMode.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/ExecutionMode_SPIR_to_SPIRV.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/group-decorate.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/image_decl_func_arg.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/image_dim.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/store.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/linked-list.ll", ""},
		//TODO produces completely wrong machine code?! (e.g. discards all writes/reads from parameters/globals)
		{PENDING, FAST, "/opt/SPIRV-LLVM/test/SPIRV/simple.ll", ""},
		{PASSED, FAST, "/opt/SPIRV-LLVM/test/SPIRV/no_capability_shader.ll", ""},

		{PENDING, FAST, "./testing/bullet/jointSolver.cl", ""},
		{PENDING, FAST, "./testing/bullet/solveContact.cl", ""},
		{PENDING, FAST, "./testing/bullet/solveFriction.cl", ""},

		/* TODO require -cl-std=c++
		"./testing/OpenCL-caffe/bnll_layer.cl",
		"./testing/OpenCL-caffe/concat_layer.cl",
		"./testing/OpenCL-caffe/contrastive_loss_layer.cl",
		"./testing/OpenCL-caffe/dropout_layer.cl",
		"./testing/OpenCL-caffe/eltwise_layer.cl",
		"./testing/OpenCL-caffe/im2col.cl",
		"./testing/OpenCL-caffe/lrn_layer.cl",
		"./testing/OpenCL-caffe/pooling_layer.cl",
		"./testing/OpenCL-caffe/prelu_layer.cl",
		"./testing/OpenCL-caffe/random.cl",
		"./testing/OpenCL-caffe/relu_layer.cl",
		"./testing/OpenCL-caffe/sigmoid_layer.cl",
		"./testing/OpenCL-caffe/slice_layer.cl",
		"./testing/OpenCL-caffe/softmax_layer.cl",
		"./testing/OpenCL-caffe/softmaxwithloss_layer.cl",
		"./testing/OpenCL-caffe/tanh_layer.cl",
		"./testing/OpenCL-caffe/threshold_layer.cl",
		"./testing/OpenCL-caffe/util.cl",
		 */

		{PASSED, SLOW, "./testing/clpeak/compute_integer_kernels.cl", ""},
		{PASSED, FAST, "./testing/clpeak/compute_sp_kernels.cl", ""},
		//XXX {PENDING, FAST, "./testing/clpeak/compute_hp_kernels.cl", "-DHALF_AVAILABLE"},
		{PASSED, FAST, "./testing/clpeak/global_bandwidth_kernels.cl", ""},

		{PASSED, FAST, "./testing/vattenoverhuvudet/calculate_voxel_grid.cl", ""},
		{PENDING, FAST, "./testing/vattenoverhuvudet/integrate_particle_states.cl", ""},
		{PASSED, FAST, "./testing/vattenoverhuvudet/simple_voxel_grid_move.cl", ""},
		{PASSED, FAST, "./testing/vattenoverhuvudet/taskParallel.cl", ""},
		{PASSED, FAST, "./testing/vattenoverhuvudet/update_particle_positions.cl", ""},

		{PENDING, FAST, "./testing/gputools/bilateral2.cl", ""},
		{PENDING, FAST, "./testing/gputools/bilateral3.cl", ""},
		{PENDING, FAST, "./testing/gputools/bilateralAdapt.cl", ""},
		{PENDING, FAST, "./testing/gputools/bilateral_shared.cl", ""},
		{PASSED, FAST, "./testing/gputools/convolve.cl", ""},
		{PENDING, FAST, "./testing/gputools/convolve1.cl", ""},
		{PENDING, FAST, "./testing/gputools/convolve2.cl", ""},
		{PENDING, FAST, "./testing/gputools/convolve3.cl", ""},
		{PASSED, FAST, "./testing/gputools/convolve_sep.cl", ""},
		{PENDING, FAST, "./testing/gputools/correlate_kernels.cl", ""},
		{PENDING, FAST, "./testing/gputools/dct_8x8.cl", ""},
		{PENDING, FAST, "./testing/gputools/dct_8x8_new.cl", ""},
		{PENDING, FAST, "./testing/gputools/dct_8x8x8.cl", ""},
		{PASSED, FAST, "./testing/gputools/minmax_filter.cl", ""},
		{PENDING, FAST, "./testing/gputools/nlm2.cl", ""},
		{PENDING, FAST, "./testing/gputools/nlm3.cl", ""},
		{PENDING, FAST, "./testing/gputools/nlm3_thresh.cl", ""},
		{PENDING, FAST, "./testing/gputools/nlmeans.cl", ""},
		{PENDING, FAST, "./testing/gputools/nlmeans3d.cl", ""},
		{PENDING, FAST, "./testing/gputools/nlmeans_projected.cl", ""},
		{PENDING, FAST, "./testing/gputools/nlm_fast.cl", ""},
		{PENDING, FAST, "./testing/gputools/nlm_fast3.cl", ""},
		{PENDING, FAST, "./testing/gputools/patch_kernel.cl", ""},
		{PENDING, FAST, "./testing/gputools/perlin.cl", ""},
		{PENDING, FAST, "./testing/gputools/scale.cl", "-DTYPENAME=float4 -DREAD_IMAGE=read_imagef"},
		{PENDING, FAST, "./testing/gputools/transformations.cl", ""},
		{PENDING, FAST, "./testing/gputools/tv_chambolle.cl", ""},

		{PASSED, FAST, "./testing/clNN/im2col.cl", ""},
		{PASSED, FAST, "./testing/clNN/SoftMax.cl", "-DSOFTMAX_THREADS=4"},
		{PASSED, FAST, "./testing/clNN/SpatialAveragePooling.cl", "-DDtype=float -DCOUNT_INCLUDE_PAD=true"},
		{PASSED, FAST, "./testing/clNN/SpatialMaxPooling.cl", "-DDtype=float"},

		{PASSED, FAST, "./testing/boost-compute/linear_congruential_engine.cl", ""},

		//all kernels
		//{PASSED, FAST, "./testing/HandBrake/openclkernels.cl", ""},
		//kernels split up
		{PASSED, FAST, "./testing/HandBrake/frame_h_scale.cl", ""},
		{PASSED, FAST, "./testing/HandBrake/frame_scale.cl", ""},
		{PASSED, FAST, "./testing/HandBrake/hscale_all_opencl.cl", ""},
		{PASSED, FAST, "./testing/HandBrake/hscale_fast_opencl.cl", ""},
		{PASSED, FAST, "./testing/HandBrake/nv12toyuv.cl", ""},
		{PASSED, FAST, "./testing/HandBrake/vscale_all_dither_opencl.cl", ""},
		{PASSED, FAST, "./testing/HandBrake/vscale_all_nodither_opencl.cl", ""},
		{PASSED, FAST, "./testing/HandBrake/vscale_fast_opencl.cl", ""},
		{PASSED, FAST, "./testing/HandBrake/yaif_filter.cl", ""},

		{PASSED, SLOW, "./testing/bfgminer/diablo.cl", "-DWORKSIZE=8"},
		{PASSED, SLOW, "./testing/bfgminer/diakgcn.cl", "-DWORKSIZE=8"},
		{PASSED, SLOW, "./testing/bfgminer/keccak.cl", "-DWORKSIZE=8"},
		{PASSED, SLOW, "./testing/bfgminer/phatk.cl", "-DWORKSIZE=8"},
		{PASSED, SLOW, "./testing/bfgminer/poclbm.cl", "-DWORKSIZE=8"},
		{PENDING, SLOW, "./testing/bfgminer/psw.cl", "-DWORKSIZE=8 -DCONCURRENT_THREADS=1 -DLOOKUP_GAP=0"},
		{PENDING, SLOW, "./testing/bfgminer/scrypt.cl", "-DWORKSIZE=8 -DCONCURRENT_THREADS=1 -DLOOKUP_GAP=0"},
		{PENDING, SLOW, "./testing/bfgminer/zuikkis.cl", "-DWORKSIZE=8 -DCONCURRENT_THREADS=1 -DLOOKUP_GAP=0"},

		{PASSED, FAST, "./testing/rendergirl/FXAA.cl", ""},
		{PENDING, FAST, "./testing/rendergirl/Raytracer.cl", ""},

		{PENDING, FAST, "./testing/JohnTheRipper/7z_kernel.cl", "-DPLAINTEXT_LENGTH=16 -DHASH_LOOPS=4"},
		//TODO removes too many instructions
		{PENDING, FAST, "./testing/JohnTheRipper/agile_kernel.cl", "-DKEYLEN=16 -DSALTLEN=32 -DOUTLEN=16"},
		{PENDING, SLOW, "./testing/JohnTheRipper/bf_kernel.cl", "-DWORK_GROUP_SIZE=8"},
		{PENDING, FAST, "./testing/JohnTheRipper/bitlocker_kernel.cl", ""},
		{PENDING, FAST, "./testing/JohnTheRipper/cryptmd5_kernel.cl", "-DPLAINTEXT_LENGTH=32"},
		{PENDING, FAST, "./testing/JohnTheRipper/DES_bs_finalize_keys_kernel.cl", "-DITER_COUNT=4"},
		{PENDING, FAST, "./testing/JohnTheRipper/DES_bs_kernel.cl", "-DITER_COUNT=4"},
		{PENDING, FAST, "./testing/JohnTheRipper/enpass_kernel.cl", "-DHASH_LOOPS=4 -DOUTLEN=16"},
		{PENDING, FAST, "./testing/JohnTheRipper/gpg_kernel.cl", "-DPLAINTEXT_LENGTH=32 -DSALT_LENGTH=32"},
		{PENDING, FAST, "./testing/JohnTheRipper/iwork_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4"},
		{PENDING, FAST, "./testing/JohnTheRipper/keystore_kernel.cl", "-DPASSLEN=8 -DSALTLEN=16"},
		{PASSED, FAST, "./testing/JohnTheRipper/lotus5_kernel.cl", ""},
		{PENDING, FAST, "./testing/JohnTheRipper/o5logon_kernel.cl", ""},
		//TODO freezes/hangs llvm-spirv/the calling of it
		{PENDING, SLOW, "./testing/JohnTheRipper/odf_aes_kernel.cl", "-DKEYLEN=128 -DOUTLEN=32 -DSALTLEN=16 -DPLAINTEXT_LENGTH=32 -DAES_LEN=32"},
		{PENDING, FAST, "./testing/JohnTheRipper/pbkdf1_hmac_sha1_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4"},
		{PENDING, FAST, "./testing/JohnTheRipper/pbkdf2_hmac_md4_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4"},
		{PENDING, FAST, "./testing/JohnTheRipper/pbkdf2_hmac_md5_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4"},
		{PENDING, FAST, "./testing/JohnTheRipper/pbkdf2_hmac_sha1_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4"},
		{PENDING, FAST, "./testing/JohnTheRipper/pbkdf2_hmac_sha1_unsplit_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4 -DKEYLEN=8 -DSALTLEN=16"},
		{PENDING, FAST, "./testing/JohnTheRipper/pbkdf2_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4"},
		{PENDING, FAST, "./testing/JohnTheRipper/pbkdf2_ripemd160_kernel.cl", "-DOUTLEN=8 -DHASH_LOOPS=4 -DKEYLEN=8 -DSALTLEN=16"},
		{PENDING, FAST, "./testing/JohnTheRipper/pwsafe_kernel.cl", ""},
		{PENDING, FAST, "./testing/JohnTheRipper/rakp_kernel.cl", "-DV_WIDTH=4"},
		{PENDING, FAST, "./testing/JohnTheRipper/rar_kernel.cl", "-DPLAINTEXT_LENGTH=16 -DHASH_LOOPS=4 -DUNICODE_LENGTH=1"},
		//TODO see above
		{PENDING, SLOW, "./testing/JohnTheRipper/wpapsk_kernel.cl", "-DHASH_LOOPS=4"},

		{PASSED, FAST, "./testing/rodinia/backprop_kernel.cl", ""},
		{PASSED, FAST, "./testing/rodinia/bfs-Kernels.cl", ""},
		{PENDING, FAST, "./testing/rodinia/cfd-Kernels.cl", ""},
		{PASSED, FAST, "./testing/rodinia/find_ellipse_kernel.cl", ""},
		{PASSED, FAST, "./testing/rodinia/gaussianElim_kernels.cl", ""},
		{PENDING, SLOW, "./testing/rodinia/heartwall_kernel_gpu_opencl.cl", ""},
		{PASSED, FAST, "./testing/rodinia/hotspot_kernel.cl", ""},
		{PASSED, FAST, "./testing/rodinia/kmeans.cl", ""},
		{PASSED, FAST, "./testing/rodinia/lud_kernel.cl", ""},
		{PENDING, FAST, "./testing/rodinia/myocyte_kernel_gpu_opencl.cl", ""},
		{PASSED, FAST, "./testing/rodinia/nearestNeighbor_kernel.cl", ""},
		{PASSED, FAST, "./testing/rodinia/nw.cl", ""},
		{PENDING, FAST, "./testing/rodinia/particle_single.cl", ""},
		{PASSED, FAST, "./testing/rodinia/pathfinder_kernels.cl", ""},
		{PASSED, FAST, "./testing/rodinia/track_ellipse_kernel.cl", ""},

		{PASSED, FAST, "./testing/NVIDIA/BitonicSort.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/BitonicSort_b.cl", ""},
		{PASSED, FAST, "./testing/NVIDIA/BlackScholes.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/BoxFilter.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/ConvolutionSeparable.cl", "-DLOCAL_SIZE_LIMIT=8 -DKERNEL_RADIUS=8 -DROWS_BLOCKDIM_X=16 -DROWS_BLOCKDIM_Y=4 -DCOLUMNS_BLOCKDIM_X=16 -DCOLUMNS_BLOCKDIM_Y=8 -DROWS_RESULT_STEPS=4 -DROWS_HALO_STEPS=1 -DCOLUMNS_RESULT_STEPS=4 -DCOLUMNS_HALO_STEPS=1"},
		{PASSED, FAST, "./testing/NVIDIA/cyclic_kernels.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/DCT8x8.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/DotProduct.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PENDING, SLOW, "./testing/NVIDIA/DXTCompression.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PENDING, FAST, "./testing/NVIDIA/FDTD3d.cl", "-DLOCAL_SIZE_LIMIT=8 -DRADIUS=8 -DMAXWORKY=2 -DMAXWORKX=4"},
		{PASSED, FAST, "./testing/NVIDIA/Histogram64.cl", "-DLOCAL_SIZE_LIMIT=8 -DHISTOGRAM64_WORKGROUP_SIZE=32 -DLOCAL_MEMORY_BANKS=8 -DMERGE_WORKGROUP_SIZE=8"},
		{PASSED, FAST, "./testing/NVIDIA/Histogram256.cl", "-DLOCAL_SIZE_LIMIT=8 -DLOG2_WARP_SIZE=2U -DWARP_COUNT=3 -DMERGE_WORKGROUP_SIZE=8"},
		{PENDING, FAST, "./testing/NVIDIA/marchingCubes_kernel.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/matrixMul.cl", "-DLOCAL_SIZE_LIMIT=8 -DBLOCK_SIZE=8"},
		{PASSED, FAST, "./testing/NVIDIA/MedianFilter.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/MersenneTwister.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/oclMatVecMul.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PENDING, FAST, "./testing/NVIDIA/oclNbodyKernel.cl", "-DLOCAL_SIZE_LIMIT=8 -DREAL3=float3 -DREAL4=float4 -DREAL=float -DZERO3=(float3)0"},
		{PASSED, FAST, "./testing/NVIDIA/oclReduction_kernel.cl", "-DLOCAL_SIZE_LIMIT=8 -DT=float -DblockSize=128 -DnIsPow2=1"},
		{PENDING, FAST, "./testing/NVIDIA/oclSimpleTexture3D_kernel.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/Particles.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/pcr_kernels.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/PostprocessGL.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/QuasirandomGenerator.cl", "-DLOCAL_SIZE_LIMIT=8"},
		//TODO crashes the tests with "corrupted double-linked list" almost every time
		//{PENDING, FAST, "./testing/NVIDIA/RadixSort.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/RecursiveGaussian.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/Scan.cl", "-DLOCAL_SIZE_LIMIT=8 -DWORKGROUP_SIZE=8"},
		{PASSED, FAST, "./testing/NVIDIA/Scan_b.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/simpleGL.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/simpleMultiGPU.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/SobelFilter.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PENDING, FAST, "./testing/NVIDIA/sweep_kernels.cl", "-DLOCAL_SIZE_LIMIT=8 -Dsystem_size=16 -DBLOCK_DIM=3"},
		{PENDING, FAST, "./testing/NVIDIA/texture_2d.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PENDING, FAST, "./testing/NVIDIA/texture_cube.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PENDING, FAST, "./testing/NVIDIA/texture_volume.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/transpose.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/VectorAdd.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/VectorHypot.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/Viterbi.cl", "-DLOCAL_SIZE_LIMIT=8"},
		{PASSED, FAST, "./testing/NVIDIA/volumeRender.cl", "-DLOCAL_SIZE_LIMIT=8"},

		{PASSED, FAST, "./testing/mixbench/mix_kernels_ro.cl", "-Dblockdim=8 -Dclass_T=float -DELEMENTS_PER_THREAD=32 -DCOMPUTE_ITERATIONS=32 -DFUSION_DEGREE=8"},
		{PASSED, FAST, "./testing/mixbench/mix_kernels.cl", "-Dblockdim=8 -Dclass_T=float -DELEMENTS_PER_THREAD=32 -DCOMPUTE_ITERATIONS=32 -DFUSION_DEGREE=8 -Dmemory_ratio=8"},

		{PASSED, FAST, "./testing/OpenCV/convert.cl", "-DNO_SCALE -DsrcT=uint8 -DdstT=float8 -DconvertToDT=convert_float8"},
		{PASSED, FAST, "./testing/OpenCV/copymakeborder.cl", "-Dcn=4 -DBORDER_REPLICATE -DST=uint -DrowsPerWI=16 -DT=int4"},
		{PASSED, FAST, "./testing/OpenCV/copyset.cl", "-Dcn=8 -DdstT=float8 -DrowsPerWI=32 -DdstT1=float4"},
		{PENDING, FAST, "./testing/OpenCV/cvtclr_dx.cl", ""},
		{PENDING, FAST, "./testing/OpenCV/fft.cl", "-DFT=float -DCT=float2 -DLOCAL_SIZE=32 -Dkercn=4 -DRADIX_PROCESS"},
		{PASSED, FAST, "./testing/OpenCV/flip.cl", "-DT=short16 -DPIX_PER_WI_Y=4"},
		{PASSED, FAST, "./testing/OpenCV/gemm.cl", "-DT=float8 -DLOCAL_SIZE=16 -DWT=float8 -DT1=float"},
		{PASSED, FAST, "./testing/OpenCV/inrange.cl", "-Dcn=4 -DsrcT1=uint -Dkercn=4 -DHAVE_SCALAR -DcolsPerWI=8"},
		{PASSED, FAST, "./testing/OpenCV/lut.cl", "-Dlcn=1 -Ddcn=3 -DdstT=uint8 -DsrcT=short16 -DLUT_OP"},
		{PASSED, FAST, "./testing/OpenCV/meanstddev.cl", "-DdstT=float -DWGS2_ALIGNED=4 -Dcn=4 -DsqdstT=float -DsrcT=uint -DconvertToDT=convert_float -DconvertToSDT=convert_float -DWGS=8"},
		{PASSED, FAST, "./testing/OpenCV/minmaxloc.cl", "-DWGS2_ALIGNED=3 -DMINMAX_STRUCT_ALIGNMENT=16 -Dkercn=4 -DdstT=float4 -DWGS=8 -DsrcT=int4 -DconvertToDT=convert_float4 -DsrcT1=int"},
		//TODO OpenCV, cltorch, blender, x264, hashcat

		{PASSED, FAST, "./testing/boost-compute/test_transform1.cl", ""},
		{PASSED, FAST, "./testing/boost-compute/test_transform2.cl", ""}
};

RegressionTest::RegressionTest()
{
    for(const auto& tuple : allKernels)
    {
    	if(std::get<0>(tuple) == PASSED)
    	{
    		TEST_ADD_TWO_ARGUMENTS(RegressionTest::testRegression, static_cast<std::string>(std::get<2>(tuple)), static_cast<std::string>(std::get<3>(tuple)));
    	}
    	else if(std::get<0>(tuple) == PENDING && std::get<1>(tuple) == FAST)
    	{
    		TEST_ADD_TWO_ARGUMENTS(RegressionTest::testPending, static_cast<std::string>(std::get<2>(tuple)), static_cast<std::string>(std::get<3>(tuple)));
    	}
    	else if(std::get<0>(tuple) == PENDING && std::get<1>(tuple) == SLOW)
    	{
			//TODO TEST_ADD_TWO_ARGUMENTS(RegressionTest::testSlowPending, static_cast<std::string>(std::get<2>(tuple)), static_cast<std::string>(std::get<3>(tuple)));
    	}
	}
    TEST_ADD(RegressionTest::printProfilingInfo);
}

void RegressionTest::testRegression(std::string clFile, std::string options)
{
    Configuration config;
    std::ostringstream out;
    std::ifstream in(clFile);
    printf("%s\n", clFile.data());
    
    const std::size_t numBytes = Compiler::compile(in, out, config, options, clFile);
    
    TEST_ASSERT(numBytes > 0);
}

void RegressionTest::testPending(std::string clFile, std::string options)
{
    testRegression(clFile, options);
}

void RegressionTest::testSlowPending(std::string clFile, std::string options)
{
    testRegression(clFile, options);
}

void RegressionTest::printProfilingInfo()
{
	//TODO is not executed?! DEBUG_MODE not set? Or just hidden from logger?
#if DEBUG_MODE
	vc4c::profiler::dumpProfileResults(true);
#endif
}
