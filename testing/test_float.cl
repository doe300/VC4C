/*
 * Tests the error for floating point division
 *
 * The compiler emulates floating point division by multipling with the inverse. This test calculates those results, which then have to be compared to "real" floating point division
 * The output memory must have space for num * num items
 */
__kernel void test_fdiv(const int num, __global const float* in, __global float * out)
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
 * Tests the error for the sqrt function
 *
 * The compiler intrinsifies sqrt() by returning the inverse of the inverse-sqrt. This test calculates those results, which then have to be compared to the results of an "exact" algorithm
 */
__kernel void test_sqrt(const int num, __global const float* in, __global float* out)
{
	int offset = num * get_global_id(0);
	for(int i = 0;i < num; ++i)
	{
		out[offset + i] = sqrt(in[i]);
	}
}

/*
 * Tests the error for the sine function
 *
 * The compiler approximates the sine-function by calculating its Tailer-series. This test calculates those results, which then have to be compared to the results of an "exact" algorithm
 */
__kernel void test_sin(const int num, __global const float* in, __global float* out)
{
	int offset = num * get_global_id(0);
	for(int i = 0;i < num; ++i)
	{
		out[offset + i] = sin(in[i]);
	}
}

/*
 * Tests the error for the cosine function
 *
 * The compiler approximates the cosine-function by calculating its Tailer-series. This test calculates those results, which then have to be compared to the results of an "exact" algorithm
 */
__kernel void test_cos(const int num, __global const float* in, __global float* out)
{
	int offset = num * get_global_id(0);
	for(int i = 0;i < num; ++i)
	{
		out[offset + i] = cos(in[i]);
	}
}

/*
 * Tests the rounding-mode for itof/ftoi
 */
__kernel void test_round(const int num, __global const int* in, __global float* out)
{
	int offset = num * get_global_id(0);
	for(int i = 0;i < num; ++i)
	{
		out[offset + i] = (int)(float)in[i];
	}
}
