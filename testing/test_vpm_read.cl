__kernel void test_vpm_read(__global const int4* in1, __global const short4* in2, __global const char4* in3, __global int4* out)
{
	int4 val = 0, sum1 = 0, sum2 = 0, sum3 = 0;
	for(int i = 0; i < 2; ++i)
		sum1 += in1[i];
	for(int i = 0; i < 2; ++i)
		sum2 += convert_int4(in2[i]);
	for(int i = 0; i < 2; ++i)
		sum3 += convert_int4(in3[i]);
	out[0] = sum1 + sum2 + sum3;
	out[1] = sum1;
	out[2] = sum2;
	out[3] = sum3;
}
