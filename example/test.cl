
/*
 * C-Code doing some calculations
 * 
 * Test C -> LLVM IR
 * 
 * "clang ./test.c -S -emit-llvm -o test.ir"
 * 
 * LLVM IR -> QASM (https://github.com/maazl/vc4asm)
 */

__kernel void test()
{
	int i = 0;
	int j = 5;
	int a = i & j;
	int b = i && j;
	int c = i - j;
	int d = i - -j;
	int e = i | j;
	int f = i || j;
	int g = i ^ j;
	int h = ~i;
	int k = i < j;
	int l = i >= j;
	int m = i == j;
	int n = &i;
	int o = *(int*)i;
	float p = i;
	int q = p;
	unsigned r = q;
	unsigned s = p;
	long t = i;
	short u = i;
	char v = j;
	int w = ~i;
}

__constant int num = 5;

int testRet()
{
	return 42;
}

int inc(int i)
{
	return i + 1;
}

__kernel void test_llvm_ir(__global int* out)
{
	float res[3];
	float vec1[3] = { 1.0f, 1.0f, 1.0f};
	float vec2[3] = { 1.5f, 2.5f, 3.5f};
	float vec3[3] = { 2.0f, 4.0f, 6.0f};
	
	//Vector 1 + Vector 2
	res[0] = vec1[0] + vec2[0];
	res[1] = vec1[1] + vec2[1];
	res[2] = vec1[2] + vec2[2];
	
	res[0] = res[0] * vec3[0];
	res[1] = res[1] * vec3[1];
	res[2] = res[2] * vec3[2];
	
	float f, g, h;
	f = inc(num);
	g = 2.0f;
	h = 3.0f;
	
	test();
	
	*out = res[0] * f + res[1] * g + res[1] * h + testRet();
}
