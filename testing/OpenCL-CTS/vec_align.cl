typedef struct myUnpackedStruct
{
    char c;
    char2 vec;
} testStruct;
__kernel void test_vec_align_struct(__constant char2* source, __global uint* dest)
{
    __local testStruct test;
    int tid = get_global_id(0);
    dest[tid] = (uint)((__local uchar*) &(test.vec));
}

typedef struct __attribute__((packed)) myPackedStruct
{
    char2 vec;
} testPackedStruct;
__kernel void test_vec_align_packed_struct(__constant char2* source, __global uint* dest)
{
    __private testPackedStruct test;
    int tid = get_global_id(0);
    dest[tid] = (uint)((__private uchar*) &(test.vec) - (__private uchar*) &test);
}
