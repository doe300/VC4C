__kernel void test_select(__global uchar4 *srcA, __global uchar4 *srcB, __global uchar4 *dst)
{
    int  tid = get_global_id(0);

    dst[tid] = (srcA[tid].s0 < srcB[tid].s0) ? srcA[tid] : srcB[tid];
}
