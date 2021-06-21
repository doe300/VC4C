__kernel void test_add_sat_int3(__global int *srcA, __global int *srcB, __global int *dst)
{
    int  tid = get_global_id(0);

    int3 tmp = add_sat(vload3(tid, srcA), vload3(tid, srcB));
    vstore3(tmp, tid, dst);
}

__kernel void test_add_sat_ushort4(__global ushort4 *srcA, __global ushort4 *srcB, __global ushort4 *dst)
{
    int  tid = get_global_id(0);

    ushort4 tmp = add_sat(srcA[tid], srcB[tid]);
    dst[tid] = tmp;
}
