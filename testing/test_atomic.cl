
#define TEST_KERNEL(name)                                                                                              \
	__kernel void test_##name(__global unsigned *ptr)                                                                  \
	{                                                                                                                  \
		name(ptr);                                                                                                     \
	}

#define TEST_KERNEL2(name)                                                                                             \
	__kernel void test_##name(__global unsigned *ptr, unsigned val)                                                    \
	{                                                                                                                  \
		name(ptr, val);                                                                                                \
	}

TEST_KERNEL2(atomic_add)
TEST_KERNEL2(atomic_sub)
TEST_KERNEL2(atomic_xchg)
TEST_KERNEL(atomic_inc)
TEST_KERNEL(atomic_dec)
TEST_KERNEL2(atomic_min)
TEST_KERNEL2(atomic_max)
TEST_KERNEL2(atomic_and)
TEST_KERNEL2(atomic_or)
TEST_KERNEL2(atomic_xor)

__kernel void test_atomic_cmpxchg(__global unsigned *ptr, unsigned compare, unsigned val)
{
    atomic_cmpxchg(ptr, compare, val);
}

__kernel void test_atomic_xchg_float(__global float *ptr, float val)
{
    atomic_xchg(ptr, val);
}
