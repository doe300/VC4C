__kernel void test_absdiff_char(__global char *srcA, __global char *srcB, __global uchar *dst)
{
    int  tid = get_global_id(0);

    char sA, sB;
    sA = srcA[tid];
    sB = srcB[tid];
    uchar dstVal = abs_diff(sA, sB);
	 dst[ tid ] = dstVal;
}

__kernel void test_abs_short3(__global short *srcA, __global ushort *dst)
{
    int  tid = get_global_id(0);

    ushort3 tmp = abs(vload3(tid, srcA));
    vstore3(tmp, tid, dst);
}


__kernel void test_absdiff_char2(__global char2 *srcA, __global char2 *srcB, __global uchar2 *dst)
{
    int  tid = get_global_id(0);

    char2 sA, sB;
    sA = srcA[tid];
    sB = srcB[tid];
    uchar2 dstVal = abs_diff(sA, sB);
	 dst[ tid ] = dstVal;
}
