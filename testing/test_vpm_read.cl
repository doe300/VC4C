__kernel void test_vpm_read(__global int4* inout1, __global int4* inout2)
{
	int4 a, b, c, d, e, f, g, h, i, j;
	//1. read and write blocks with stride
	//TODO allow for initial offset
	int easyStride = 3;
	a = inout2[0 * easyStride];
	b = inout2[1 * easyStride];
	c = inout2[2 * easyStride];
	d = inout2[3 * easyStride];
	e = inout2[4 * easyStride];
	f = inout2[5 * easyStride];
	g = inout2[6 * easyStride];
	h = inout2[7 * easyStride];
	i = inout2[8 * easyStride];
	j = inout2[9 * easyStride];

	inout1[0] = (a);
	inout1[1] = (b);
	inout1[2] = (c);
	inout1[3] = (d);
	inout1[4] = (e);
	inout1[5] = (f);
	inout1[6] = (g);
	inout1[7] = (h);
	inout1[8] = (i);
	inout1[9] = (j);

	//2. read and write blocks
	a = (inout1[0]);
	b = (inout1[1]);
	c = (inout1[2]);
	d = (inout1[3]);
	e = (inout1[4]);
	f = (inout1[5]);
	g = (inout1[6]);
	h = (inout1[7]);
	i = (inout1[8]);
	j = (inout1[9]);

	inout2[0] = a;
	inout2[1] = b;
	inout2[2] = c;
	inout2[3] = d;
	inout2[4] = e;
	inout2[5] = f;
	inout2[6] = g;
	inout2[7] = h;
	inout2[8] = i;
	inout2[9] = j;
}
