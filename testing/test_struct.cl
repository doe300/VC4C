
struct SomeStruct
{
	//TODO crashes llvm-spirv, not allowed??
	float16 f;
	int i;
	//char c;
};

/*
 * Tests handling of structs
 */
__kernel void test_struct(const __global struct SomeStruct* in, __global struct SomeStruct* out)
{
	struct SomeStruct tmp = *in;
//	tmp.f = (float16) 16.0f;
	tmp.i = 42;
	//tmp.c = 'c';
	*out = tmp;
}
