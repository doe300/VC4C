
__kernel void test_instructions(const int i0, const int i1, const float f0, const float f1, __global int* intOut, __global float* floatOut)
{
	intOut[0] = i0 + i1;
	intOut[1] = i0 - i1;
	intOut[2] = i0 * i1;
	intOut[3] = i0 / i1;
	intOut[4] = i0 % i1;
	intOut[5] = max(i0, i1);
	intOut[6] = min(i0, i1);
	intOut[7] = (int) f0;
	intOut[8] = i0 << i1;
	intOut[9] = i0 >> i1;
	intOut[10] = i0 & i1;
	intOut[11] = i0 | i1;
	intOut[12] = i0 ^ i1;
	intOut[13] = ~i0;
	intOut[14] = clz(i0);
	int j = i0;
	intOut[15] = ++j;
	intOut[16] = j--;
	intOut[17] = i0 < i1;
	intOut[18] = i0 <= i1;
	intOut[19] = i0 == i1;
	intOut[20] = i0 >= i1;
	intOut[21] = i0 > i1;
	intOut[22] = i0 != i1;
	intOut[23] = i0 && i1;
	intOut[24] = i0 || i1;
	intOut[25] = !i0;
	intOut[26] = sizeof(i0);
	intOut[27] = ((char) i0) * ((char) i1);

	/*
	char4 c0 = {
		(char) i0 >> 24,
		(char) i0 >> 16,
		(char) i0 >> 8,
		(char) i0
	};
	char4 c1 = {
		(char) i1 >> 24,
		(char) i1 >> 16,
		(char) i1 >> 8,
		(char) i1
	};

	intOut[28] = c0 + c1;
	intOut[29] = c0 - c1;
	intOut[30] = min(c0, c1);
	intOut[31] = max(c0, c1);
	*/

	floatOut[0] = f0 + f1;
	floatOut[1] = f0 - f1;
	floatOut[2] = f0 * f1;
	floatOut[3] = f0 / f1;
	floatOut[4] = max(f0, f1);
	floatOut[5] = min(f0, f1);
	floatOut[6]= (float) i0;
}
