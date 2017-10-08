__kernel 
__attribute__((reqd_work_group_size(1,1,1)))
__attribute__((work_group_size_hint(1, 1, 1)))
void test_arithm(const float offset, __constant const float16* in, __global float16* out)
{
	size_t id = get_global_id(0);
	float16 tmp = in[id] + offset;
	out[id] = tmp;
}

__kernel void test_copy(__global const int16* in, __global int16* out)
{
	size_t id = get_global_id(0);
	const int16 data = *in;
	out[id] = data;
	out[id + 1] = data;
}

__kernel void test_add(__global const uchar16* in0, __global const ushort16* in1, __global uchar16* outTest, __global ushort16* out0, __global int16* out1)
{
	size_t id = get_global_id(0);
	*outTest = *in0;
//	*outTest = (uchar16)(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
	out0[id] = convert_ushort16(*in0);
//	out0[id] = convert_ushort16((uchar16)(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15));
	out0[id + 2] = *in1;
//	out0[id + 2] = (ushort16)(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
	*out1 = convert_int16(*in0) + convert_int16(*in1);
//	*out1 = convert_int16((uchar16)(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + convert_int16((ushort16)(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15));
}

__kernel void test_param(const uchar16 in1, const int4 in2, __global int4* out)
{
	*out = in2 + vc4cl_bitcast_int(vc4cl_extend(in1.xyzw));
}
