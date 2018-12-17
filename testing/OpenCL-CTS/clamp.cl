__kernel void test_clamp(__global float* x, __global float* minval, __global float* maxval, __global float* dst)
{
    int tid = get_global_id(0);

    dst[tid] = clamp(x[tid], minval[tid], maxval[tid]);
}

__kernel void sample_test(
    __global short16* sourceA, __global short16* sourceB, __global short16* sourceC, __global short16* destValues)
{
    int tid = get_global_id(0);
    short16 sA = sourceA[tid];
    short16 sB = sourceB[tid];
    short16 sC = sourceC[tid];
    short16 dst = clamp(sA, sB, sC);
    destValues[tid] = dst;
}
