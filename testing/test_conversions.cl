
#define CALCULATE(x, type) \
	out[0] = x; \
	out[1] = x + x; \
	out[2] = x - x; \
	out[3] = x * x; \
	out[4] = x / (type)100; \
	out[5] = x % (type)100;

__kernel void test_sign_truncate(int2 source, __global short2* out)
{
	short2 x = convert_short2(source);
	CALCULATE(x, short2)
}

__kernel void test_sign_extend(short2 source, __global int2* out)
{
	int2 x = convert_int2(source);
	CALCULATE(x, int2)
}

__kernel void test_unsign_truncate(uint2 source, __global ushort2* out)
{
	ushort2 x = convert_ushort2(source);
	CALCULATE(x, ushort2)
}

__kernel void test_unsign_extend(ushort2 source, __global uint2* out)
{
	uint2 x = convert_uint2(source);
	CALCULATE(x, uint2)
}
