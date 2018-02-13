
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

short4 custom_as_short4(int2 val)
{
	return ((union { int2 src; short4 dst; }) { .src = val}).dst;
}

uchar8 custom_as_uchar8(int2 val)
{
	return ((union { int2 src; uchar8 dst; }) { .src = val}).dst;
}

/*
 * Built-in and custom as_type implementations generate the same code
 */
__kernel void test_as_type(int2 source, __global short4* out1, __global uchar8* out2)
{
	uint gid = get_global_id(0) * 2;
	out1[gid] = __builtin_astype((source),  short4);
	out2[gid] = __builtin_astype((source), uchar8);
	out1[gid + 1] = custom_as_short4(source);
	out2[gid + 1] = custom_as_uchar8(source);
}