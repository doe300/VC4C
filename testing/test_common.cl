#define TEST_KERNEL(name)                                                                                              \
	__kernel void test_##name(const __global float *in, __global float *out)                                           \
	{                                                                                                                  \
		uint gid = get_global_id(0);                                                                                   \
		out[gid] = name(in[gid]);                                                                                      \
	}

#define TEST_KERNEL2(name)                                                                                             \
	__kernel void test_##name(const __global float *x, const __global float *y, __global float *out)                   \
	{                                                                                                                  \
		uint gid = get_global_id(0);                                                                                   \
		out[gid] = name(x[gid], y[gid]);                                                                               \
	}

#define TEST_KERNEL3(name)                                                                                             \
	__kernel void test_##name(                                                                                         \
		const __global float *x, const __global float *y, const __global float *z, __global float *out)                \
	{                                                                                                                  \
		uint gid = get_global_id(0);                                                                                   \
		out[gid] = name(x[gid], y[gid], z[gid]);                                                                       \
	}

TEST_KERNEL3(clamp)
TEST_KERNEL(degrees)
TEST_KERNEL2(max)
TEST_KERNEL2(min)
TEST_KERNEL3(mix)
TEST_KERNEL(radians)
TEST_KERNEL2(step)
TEST_KERNEL3(smoothstep)
TEST_KERNEL(sign)