
/*
 * Calculates the next 10 fibonacci numbers after start0, start1
 * E.g. set start0 = start1 = 1 for the first 10 fibonacci numbers:
 * 2, 3, 5, 8, 13, 21, 34, 55, 89, 144
 */
__kernel void fibonacci(const int start0, const int start1, __global int * out)
{
	int fp = start0;
	int fc = start1;
	int tmp;
	
	#pragma loop unroll
	for(int i = 0; i < 10; i++)
	{
		tmp = fc + fp;
		fp = fc;
		fc = tmp;
		out[i] = fc;
	}
}
