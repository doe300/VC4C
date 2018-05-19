__kernel void test_cross(__global float3 *x, __global float3 *y, __global float3 *dst)
{
    int  tid = get_global_id(0);

    dst[tid] = cross(x[tid], y[tid]);
}
