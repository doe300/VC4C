#define int8_t char
#define int16_t short
#define int32_t int
#define int64_t long
#define uint8_t uchar
#define uint16_t ushort
#define uint32_t uint
#define uint64_t ulong
#if defined(cl_khr_fp64)
#pragma OPENCL EXTENSION cl_khr_fp64 : enable
#define DOUBLE_SUPPORT_AVAILABLE
#elif defined(cl_amd_fp64)
#pragma OPENCL EXTENSION cl_amd_fp64 : enable
#define DOUBLE_SUPPORT_AVAILABLE
#endif
#if defined(cl_khr_fp16)
#pragma OPENCL EXTENSION cl_khr_fp16 : enable
#define HALF_SUPPORT_AVAILABLE
#endif
#ifdef int_tp
#undef int_tp
#endif  //int_tp
#define int_tp int32_t
#ifdef uint_tp
#undef uint_tp
#endif  //uint_tp
#define uint_tp uint32_t
#ifdef int_tpc
#undef int_tpc
#endif  //int_tpc
#define int_tpc int32_t
#ifdef uint_tpc
#undef uint_tpc
#endif  //uint_tpc
#define uint_tpc uint32_t
__kernel void caffe_gpu_memset(const uint32_t n, const char alpha, __global char* y_raw_ptr, const uint_tp y_offset) {
__global char* y = y_raw_ptr + y_offset;
for (uint_tp index = get_global_id(0); index < (n); index += get_global_size(0)) {
y[index] = alpha;
}
}
__kernel void caffe_gpu_null_kernel(float arg) {
float out = arg;
}

