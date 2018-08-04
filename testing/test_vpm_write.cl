short16 vc4cl_bitcast_short(int16) __attribute__((overloadable));
char16 vc4cl_bitcast_char(int16) __attribute__((overloadable));

__kernel void test_vpm_write(__global const int16* in, __global int16* out1, __global short16* out2, __global char16* out3, __global int16* out4)
{
	int16 val = *in;
	#pragma unroll
	for(int i = 0; i < 10; ++i)
		out1[i] = val;
	for(int i = 0; i < 10; ++i)
		out2[i] = vc4cl_bitcast_short(val);
	for(int i = 0; i < 10; ++i)
		out3[i] = vc4cl_bitcast_char(val);

	// tests optimization with strided writes
	int easyStride = 3;
	for(int i = 0; i < 10; ++i)
		out4[easyStride * i] = val;

	//TODO test optimization with strided writes and unknown stride - possible?
}
