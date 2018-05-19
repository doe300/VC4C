
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

#define CONVERT_FUNCTIONS(out, in, type) \
	out.s0 = convert_##type(in.s0); \
	out.s1 = convert_##type##_rte(in.s1); \
	out.s2 = convert_##type##_rtz(in.s2); \
	out.s3 = convert_##type##_rtp(in.s3); \
	out.s4 = convert_##type##_rtn(in.s4); \
	out.s5 = convert_##type##_sat(in.s5); \
	out.s6 = convert_##type##_sat_rte(in.s6); \
	out.s7 = convert_##type##_sat_rtz(in.s7); \
	out.s8 = convert_##type##_sat_rtp(in.s8); \
	out.s9 = convert_##type##_sat_rtn(in.s9);

/*
 * Tests converting through all types 
 */
__kernel void test_conversion_functions(const __global float16* in, __global float16* out)
{
	float16 f = (*in);
	//convert_float_sat is not in the LLVM OpenCL C headers
//	CONVERT_FUNCTIONS(f, (*in), float)
	int16 i;
	CONVERT_FUNCTIONS(i, f, int)
	int16 j;
	CONVERT_FUNCTIONS(j, i, int)
	short16 s;
	CONVERT_FUNCTIONS(s, j, short)
	short16 t;
	CONVERT_FUNCTIONS(t, s, short)
	char16 c;
	CONVERT_FUNCTIONS(c, t, char)
	char16 d;
	CONVERT_FUNCTIONS(d, c, char)

	uchar16 uc;
	CONVERT_FUNCTIONS(uc, d, uchar)
	uchar16 ud;
	CONVERT_FUNCTIONS(ud, uc, uchar)
	ushort16 us;
	CONVERT_FUNCTIONS(us, ud, ushort)
	ushort16 ut;
	CONVERT_FUNCTIONS(ut, us, ushort)
	uint16 ui;
	CONVERT_FUNCTIONS(ui, ut, uint)
	uint16 uj;
	CONVERT_FUNCTIONS(uj, ui, uint)
	
//	CONVERT_FUNCTIONS((*out), uj, float)
	*out = convert_float16(uj);
}
