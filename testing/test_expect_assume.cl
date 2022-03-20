
__kernel void test_expect_assume(__global int* out, __global const int* in)
{
    size_t gid = get_global_id(0);
    __builtin_assume(in[gid]);
    int value = __builtin_expect(in[gid], 13);
    if(value == 13)
        out[gid] = -1;
    else
        out[gid] = (int) exp2((float) value);
}
