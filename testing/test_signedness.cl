__kernel void test_signed_char(char c, char4 c2, __global char* out)
{
	*out = c;
	vstore4(c2, 4, out);
}

__kernel void test_signed_short(short s, short4 s2, __global short* out)
{
	*out = s;
	vstore4(s2, 4, out);
}

__kernel void test_signed_int(int i, int4 i2, __global int* out)
{
	*out = i;
	vstore4(i2, 4, out);
}

__kernel void test_unsigned_char(uchar u, uchar4 u2, __global uchar* out)
{
	*out = u;
	vstore4(u2, 4, out);
}

__kernel void test_unsigned_short(ushort u, ushort4 u2, __global ushort* out)
{
	*out = u;
	vstore4(u2, 4, out);
}

__kernel void test_unsigned_int(uint u, uint4 u2, __global uint* out)
{
	*out = u;
	vstore4(u2, 4, out);
}

__kernel void test_float(float f, float4 f2, __global float* out)
{
	*out = f;
	vstore4(f2, 4, out);
}
