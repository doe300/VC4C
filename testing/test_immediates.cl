
__kernel void test_small_immediates(__global int* out0, __global float* out1)
{
	out0[0] = 3;
	out0[1] = 7;
	out0[2] = out1[0] + 31.0f;
	out0[3] = out0[3] - 26;

	out1[0] = out1[0] + 7.0f;

}
