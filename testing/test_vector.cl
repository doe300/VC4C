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
	*out = in2 + convert_int4(in1.xyzw);
}

__kernel void test_vector_load(const __global char* in, __global char3* out)
{
	const uchar offset = 5;
	char3 tmp0 = vload3(offset, in);
	char3 tmp1 = ((const __global char3*)in)[offset];	//TODO this is wrong for char3, uses 4-byte stride
	char3 tmp2 = *((const __global char3*)(in + offset * 3));
	char3 tmp3 = (char3)(in[offset * 3], in[offset * 3 + 1], in[offset * 3 + 2]);	//pocl does this, but requires n loads for vloadn (see: https://github.com/pocl/pocl/blob/master/lib/kernel/vload.cl)
	char3 tmp4 = *((const __global char3*) (&in[offset * 3]));	//libclc does this (see: https://llvm.org/viewvc/llvm-project/libclc/trunk/generic/lib/shared/vload.cl?view=markup)

	//for char2 vector:
	//LLVM recognizes tmp0, tmp1, tmp2 and tmp4 as identical, creates only one load
	//tmp3 generates different code (as expected)
	//all generate same result

	//for char3 vector:
	//tmp0, tmp2, tmp3 and tmp4 generate same result

	out[0] = tmp0;
	out[1] = tmp1;
	out[2] = tmp2;
	out[3] = tmp3;
	out[4] = tmp4;
}
