
#define TEST_KERNEL(name)                                                                                              \
	__kernel void test_##name(const __global int *in, __global int *out)                                               \
	{                                                                                                                  \
		uint gid = get_global_id(0);                                                                                   \
		out[gid] = (int) name((short) in[gid]);                                                                        \
	}

#define TEST_KERNEL2(name)                                                                                             \
	__kernel void test_##name(const __global int *in0, const __global int *in1, __global int *out)                     \
	{                                                                                                                  \
		uint gid = get_global_id(0);                                                                                   \
		out[gid] = (int) name((short) in0[gid], (short) in1[gid]);                                                     \
	}

#define TEST_KERNEL3(name)                                                                                             \
	__kernel void test_##name(                                                                                         \
		const __global int *in0, const __global int *in1, const __global int *in2, __global int *out)                 \
	{                                                                                                                  \
		uint gid = get_global_id(0);                                                                                   \
		out[gid] = (int) name((short) in0[gid], (short) in1[gid], (short) in2[gid]);                                   \
	}

TEST_KERNEL(abs)
TEST_KERNEL2(abs_diff)
TEST_KERNEL2(add_sat)
TEST_KERNEL2(hadd)
TEST_KERNEL2(rhadd)
TEST_KERNEL3(clamp)
TEST_KERNEL(clz)
TEST_KERNEL3(mad_hi)
TEST_KERNEL3(mad_sat)
TEST_KERNEL2(max)
TEST_KERNEL2(min)
TEST_KERNEL2(mul_hi)
TEST_KERNEL2(rotate)
TEST_KERNEL2(sub_sat)
TEST_KERNEL2(upsample)
TEST_KERNEL(popcount)
TEST_KERNEL3(mad24)
TEST_KERNEL2(mul24)