/*
 * Contains test routines for the OpenCL C standard library math routines
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#define TEST_KERNEL(func)                                                                                              \
	__kernel void test_##func(const __global float *in, __global float *out)                                           \
	{                                                                                                                  \
		size_t gid = get_global_id(0);                                                                                 \
		out[gid] = func(in[gid]);                                                                                      \
	}

#define TEST_KERNEL2(func)                                                                                             \
	__kernel void test_##func(const __global float *in0, const __global float *in1, __global float *out)               \
	{                                                                                                                  \
		size_t gid = get_global_id(0);                                                                                 \
		out[gid] = func(in0[gid], in1[gid]);                                                                           \
	}
TEST_KERNEL(acos)
TEST_KERNEL(acosh)
TEST_KERNEL(acospi)
TEST_KERNEL(asin)
TEST_KERNEL(asinh)
TEST_KERNEL(asinpi)
TEST_KERNEL(atan)
TEST_KERNEL2(atan2)
TEST_KERNEL(atanh)
TEST_KERNEL(atanpi)
TEST_KERNEL2(atan2pi)
TEST_KERNEL(cbrt)
TEST_KERNEL(ceil)
TEST_KERNEL2(copysign)
TEST_KERNEL(cos)
TEST_KERNEL(cosh)
TEST_KERNEL(cospi)
TEST_KERNEL(erfc)
TEST_KERNEL(erf)
TEST_KERNEL(exp)
TEST_KERNEL(exp2)
TEST_KERNEL(exp10)
TEST_KERNEL(expm1)
TEST_KERNEL(fabs)
TEST_KERNEL2(fdim)
TEST_KERNEL(floor)
// TODO fma
TEST_KERNEL2(fmax)
TEST_KERNEL2(fmin)
TEST_KERNEL2(fmod)
//TODO fract, frexp
TEST_KERNEL2(hypot)
TEST_KERNEL(ilogb)
TEST_KERNEL2(ldexp)
TEST_KERNEL(lgamma)
// TODO lgamma_r
TEST_KERNEL(log)
TEST_KERNEL(log2)
TEST_KERNEL(log10)
TEST_KERNEL(log1p)
TEST_KERNEL(logb)
// TODO mad
TEST_KERNEL2(maxmag)
TEST_KERNEL2(minmag)
//TODO modf, nan
TEST_KERNEL2(nextafter)
TEST_KERNEL2(pow)
TEST_KERNEL2(pown)
TEST_KERNEL2(powr)
TEST_KERNEL2(remainder)
// TODO remquo
TEST_KERNEL(rint)
TEST_KERNEL2(rootn)
TEST_KERNEL(round)
TEST_KERNEL(rsqrt)
TEST_KERNEL(sin)
// TODO sincos
TEST_KERNEL(sinh)
TEST_KERNEL(sinpi)
TEST_KERNEL(sqrt)
TEST_KERNEL(tan)
TEST_KERNEL(tanh)
TEST_KERNEL(tanpi)
TEST_KERNEL(tgamma)
TEST_KERNEL(trunc)