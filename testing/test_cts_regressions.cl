__kernel void basic_vload_local_uchar3(__global uchar3* src, __local uchar3* localBuf, __global uchar3* res)
{
    int tid = get_global_id(0);
    int lid = get_local_id(0);
    vstore3(vload3(tid, (__global uchar*) src), lid, (__local uchar*) localBuf);
    barrier(CLK_LOCAL_MEM_FENCE);
    vstore3(vload3(lid, (__local uchar*) localBuf), tid, (__global uchar*) res);
}

__kernel void basic_vload_local_float3(__global float3* src, __local float3* localBuf, __global float3* res)
{
    int tid = get_global_id(0);
    int lid = get_local_id(0);
    vstore3(vload3(tid, (__global float*) src), lid, (__local float*) localBuf);
    barrier(CLK_LOCAL_MEM_FENCE);
    vstore3(vload3(lid, (__local float*) localBuf), tid, (__global float*) res);
}

kernel void basic_vstore_local_char3(local char* sSharedStorage, global char* srcValues, global uint* offsets,
    global char* destBuffer, uint alignmentOffset)
{
    int tid = get_global_id(0);
    int lid = get_local_id(0);
    sSharedStorage[3 * lid] = (char) 0;
    sSharedStorage[3 * lid + 1] = sSharedStorage[3 * lid];
    sSharedStorage[3 * lid + 2] = sSharedStorage[3 * lid];
    sSharedStorage[3 * lid + 3] = sSharedStorage[3 * lid];
    sSharedStorage[3 * lid + 4] = sSharedStorage[3 * lid];
    sSharedStorage[3 * lid + 5] = sSharedStorage[3 * lid];
    barrier(CLK_LOCAL_MEM_FENCE);

    vstore3(vload3(tid, srcValues), lid, sSharedStorage);

    barrier(CLK_LOCAL_MEM_FENCE);

    int i;
    local char* sp = (sSharedStorage + 3 * lid);
    global char* dp = (destBuffer + 3 * tid);
    for(i = 0; i < 3; i++)
        dp[i] = sp[i];
}

__kernel void relational_shuffle_copy_char3_to_char16(__global char* source, __global char16* dest)
{
    char16 tmp;
    tmp = (char16) (0);
    tmp.s2 = vload3(0, source).S0;
    dest[0] = tmp;

    tmp = (char16) (0);
    tmp.Sd5 = vload3(0, source).s10;
    dest[1] = tmp;
}

__kernel void relational_shuffle_copy_uchar1_to_uchar4(__global uchar* source, __global uchar4* dest)
{
    uchar4 tmp;
    tmp = (uchar4) (0);
    tmp.S3 = source[0];
    dest[0] = tmp;

    tmp = (uchar4) (0);
    tmp.s0 = source[0];
    dest[1] = tmp;
}

__kernel void relational_shuffle_copy_ushort4_to_ushort2(__global ushort4* source, __global ushort2* dest)
{
    ushort2 tmp;
    tmp = (ushort2) (0);
    tmp.s0 = source[0].s1;
    dest[0] = tmp;
}

__kernel void relational_shuffle_copy_uint3_to_uint4(__global uint* source, __global uint4* dest)
{
    uint4 tmp;
    tmp = (uint4) (0);
    tmp.S1 = vload3(0, source).S2;
    dest[0] = tmp;
}
