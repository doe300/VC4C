
/*
 * Tests floating to integer conversions
 *
 * val_down is a floating value which would be rounded downwards (e.g. 1.1)
 * val_mid is a floating value in the middle of two integers (e.g. 1.5)
 * val_up is floating value which would be rounded upwards (e.g. 2.9)
 *
 */
__kernel void test_f2i(const float val_exact, const float val_down, const float val_mid, const float val_up, __global int* out)
{
	size_t offset = 32 * get_global_id(0);
	out[offset] = (int) val_exact;
	out[offset + 1] = (int) val_down;
	out[offset + 2] = (int) val_mid;
	out[offset + 3] = (int) val_up;
	out[offset + 4] = (int) -val_exact;
	out[offset + 5] = (int) -val_down;
	out[offset + 6] = (int) -val_mid;
	out[offset + 7] = (int) -val_up;
	out[offset + 8] = (int) ceil(val_exact);
	out[offset + 9] = (int) ceil(-val_exact);
	out[offset + 10] = (int) ceil(val_mid);
	out[offset + 11] = (int) ceil(-val_mid);
	out[offset + 12] = (int) floor(val_exact);
	out[offset + 13] = (int) floor(-val_exact);
	out[offset + 14] = (int) floor(val_mid);
	out[offset + 15] = (int) floor(-val_mid);
	out[offset + 16] = (int) rint(val_exact);
	out[offset + 17] = (int) rint(-val_exact);
	out[offset + 18] = (int) rint(val_mid);
	out[offset + 19] = (int) rint(-val_mid);
	out[offset + 20] = (int) round(val_exact);
	out[offset + 21] = (int) round(val_down);
	out[offset + 22] = (int) round(val_mid);
	out[offset + 23] = (int) round(val_up);
	out[offset + 24] = (int) round(-val_exact);
	out[offset + 25] = (int) round(-val_down);
	out[offset + 26] = (int) round(-val_mid);
	out[offset + 27] = (int) round(-val_up);
	out[offset + 28] = (int) trunc(val_mid);
	out[offset + 29] = (int) trunc(-val_mid);
}

/*
 * Tests handling of vector-parameters
 *
 * - see, what LLVM generates from them
 * - see, how we have to handle the IR
 *
 */
__kernel void test_vector_param(const float2 vec2, const float4 vec4, const float16 vec16, __global float16* out)
{
	vec4.x += vec2.x;
	vec4.y += vec2.y;

	vec16.x *= vec4.x;
	vec16.y *= vec4.y;
	vec16.z *= vec4.z;
	vec16.w *= vec4.w;

	*out = vec16;
}

/*
 * Tests LLVM conversion of atomic operations
 *
 * - see, what LLVM generates from them
 * - see, how we have to handle the IR
 *
 */
__kernel void test_atomics(__global const int* in, __global int* out)
{
	size_t offset = get_global_id(0);
	size_t size = get_global_size(0);
	atomic_add(&out[offset], in[offset]);
	atomic_sub(&out[offset + size], in[offset + size]);
	atomic_xchg(&out[offset + 2 * size], in[offset + 2 * size]);
	atomic_inc(&out[offset + 3 * size]);
	atomic_dec(&out[offset + 4 * size]);
	atomic_cmpxchg(&out[offset + 5 * size], in[offset + 5 * size], in[offset + 5 * size]);
	atomic_min(&out[offset + 6 * size], in[offset + 6 * size]);
	atomic_max(&out[offset + 7 * size], in[offset + 7 * size]);
	atomic_and(&out[offset + 8 * size], in[offset + 8 * size]);
	atomic_or(&out[offset + 9 * size], in[offset + 9 * size]);
	atomic_xor(&out[offset + 10 * size], in[offset + 10 * size]);
}

/*
 * Tests access to global data
 *
 * - LLVM-SPIRV generates OpVariables out of both arrays (at least, if both constant)
 */
__constant int globalData[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};

__kernel void test_global_data(const int index, __global int* out)
{
	const int localData[] = {11,21,31,41,51,61,71,81,91,101,111,121,131,141,151,161,171,181,191,201};

	out[0] = globalData[index];
	out[1] = localData[index];
}
