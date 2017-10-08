/*
 * Tests the result of 24-bit muliplication
 *
 * According to maazl (http://maazl.de/project/vc4asm/doc/instructions.html#ALU), mul24 accepts two 24-bit integers and returns a 32-bit integer. if that is the case, we could improve the 32-bit mulitplication performance
 */
__kernel void test_mul24(const int num, const int in, __global int* out)
{
	size_t offset = num * get_global_id(0);
	//for(int i = 0;i < num; ++i)
	{
		//for(int j = 0;j < num; ++j)
		{
			//out[offset + i * num + j] = mul24(in[i], in[j]);
		}
	}
	out[offset] = mul24(in, in);
	//It works!! mul24(0xFF008000, 0xFF008000) results in 0x40000000
	//which is the result of 0x8000 * 0x8000, so the leading 0xFF (8-bits) are simply ignored
	// -> problem: mul24 is unsigned ;(

	//Just to test, whether add/sub is signed or not!
	//Seems to be working for signed integer
	out[offset + 1] = 10 + in;
	out[offset + 2] = 10 - in;
}

/*
 * Tests the error for unsigned division
 *
 * The compiler emulates unsigned division by long division. This test calculates those results, which then have to be compared to "real" integer division
 * The output memory must have space for num * num items
 */
__kernel void test_udiv(const int num, __global const uint* in, __global uint* out)
{
	int offset = num * get_global_id(0);
	for(int i = 0;i < num; ++i)
	{
		for(int j = 0;j < num; ++j)
		{
			out[offset + i * num + j] = in[i] / in[j];
		}
	}
}

/*
 * Tests the error for signed division
 *
 * The compiler emulates signed division by long division. This test calculates those results, which then have to be compared to "real" integer division
 * The output memory must have space for num * num items
 */
__kernel void test_sdiv(const int num, __global const int* in, __global int* out)
{
	int offset = num * get_global_id(0);
	for(int i = 0;i < num; ++i)
	{
		for(int j = 0;j < num; ++j)
		{
			out[offset + i * num + j] = in[i] / in[j];
		}
	}
}

/*
 * Tests the error for unsigned division with remainder
 *
 * The compiler emulates unsigned division by long division. This test calculates those results, which then have to be compared to "real" integer division
 * The output memory must have space for num * num items
 */
__kernel void test_urem(const int num, __global const uint* in, __global uint* out)
{
	int offset = num * get_global_id(0);
	for(int i = 0;i < num; ++i)
	{
		for(int j = 0;j < num; ++j)
		{
			out[offset + i * num + j] = in[i] % in[j];
		}
	}
}

/*
 * Tests the error for signed remainder-division
 *
 * The compiler emulates unsigned division by long division. This test calculates those results, which then have to be compared to "real" integer division
 * The output memory must have space for num * num items
 */
__kernel void test_srem(const int num, __global const int* in, __global int* out)
{
	int offset = num * get_global_id(0);
	for(int i = 0;i < num; ++i)
	{
		for(int j = 0;j < num; ++j)
		{
			out[offset + i * num + j] = in[i] % in[j];
		}
	}
}
