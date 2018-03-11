
#define TEST_KERNEL(name)                                                                                              \
	__kernel void test_##name(const __global float *in, __global float *out)                                           \
	{                                                                                                                  \
		uint gid = get_global_id(0);                                                                                   \
		vstore3(name(vload3(gid, in)), gid, out);                                                                      \
	}

#define TEST_KERNEL2(name)                                                                                             \
	__kernel void test_##name(const __global float *x, const __global float *y, __global float *out)                   \
	{                                                                                                                  \
		uint gid = get_global_id(0);                                                                                   \
		vstore3(name(vload3(gid, x), vload3(gid, y)), gid, out);                                                       \
	}

TEST_KERNEL2(cross)
TEST_KERNEL2(dot)
TEST_KERNEL(length)
TEST_KERNEL2(distance)
TEST_KERNEL(normalize)
TEST_KERNEL(fast_length)
TEST_KERNEL2(fast_distance)
TEST_KERNEL(fast_normalize)