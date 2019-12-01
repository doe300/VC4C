typedef struct myUnpackedStruct
{
    char2 vec;
} testStruct;
__kernel void test_vec_align_struct(__constant char2* source, __global ulong* dest)
{
    __private testStruct test;
    int tid = get_global_id(0);
    dest[tid] = (ulong)((__private uchar*) &(test.vec));
}

typedef struct __attribute__((packed)) myPackedStruct
{
    char2 vec;
} testPackedStruct;
__kernel void test_vec_align_packed_struct(__constant char2* source, __global ulong* dest)
{
    __private testPackedStruct test;
    int tid = get_global_id(0);
    dest[tid] = (ulong)((__private uchar*) &(test.vec) - (__private uchar*) &test);
}
