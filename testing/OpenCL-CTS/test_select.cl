__kernel void select_short_short(__global short* dest, __global short* src1, __global short* src2, __global short* cmp)
{
    size_t tid = get_global_id(0);
    if(tid < get_global_size(0))
        dest[tid] = select(src1[tid], src2[tid], cmp[tid]);
}

__kernel void select_int3_uint3(__global int* dest, __global int* src1, __global int* src2, __global uint* cmp)
{
    size_t tid = get_global_id(0);
    size_t size = get_global_size(0);
    if(tid + 1 < size) // can't run off the end
        vstore3(select(vload3(tid, src1), vload3(tid, src2), vload3(tid, cmp)), tid, dest);
    else if(tid + 1 == size)
    {
        size_t leftovers = 1 + (size & 1);
        int3 a, b;
        uint3 c;
        switch(leftovers)
        {
        case 2:
            a.y = src1[3 * tid + 1];
            b.y = src2[3 * tid + 1];
            c.y = cmp[3 * tid + 1];
        // fall through
        case 1:
            a.x = src1[3 * tid];
            b.x = src2[3 * tid];
            c.x = cmp[3 * tid];
            break;
        }
        a = select(a, b, c);
        switch(leftovers)
        {
        case 2:
            dest[3 * tid + 1] = a.y;
        // fall through
        case 1:
            dest[3 * tid] = a.x;
            break;
        }
    }
}


__kernel void select_uint2_int2(__global uint2* dest, __global uint2* src1, __global uint2* src2, __global int2* cmp)
{
    size_t tid = get_global_id(0);
    if(tid < get_global_size(0))
        dest[tid] = select(src1[tid], src2[tid], cmp[tid]);
}