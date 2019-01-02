
extern int get_value(const __global int* in);

__kernel void test_linker(__global int* out, const __global int* in)
{
    *out = get_value(in);
}