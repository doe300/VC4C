__kernel void test_cross_write_simple(__global uint* out)
{
    __local uint temp[12];

    uint lid = get_local_id(0);
    temp[lid] = get_global_id(0);

    // This barrier is required, otherwise the memory contents is undefined anyway
    barrier(CLK_GLOBAL_MEM_FENCE);

    if(lid == 0)
    {
        // This code is only executed for local ID 0 and takes a lot longer than the writing of the next global ID into
        // the local buffer. Thus, if we omit inserting a work-group loop, we read some values already written by the
        // successive work-groups, which is wrong.
        // Finding this access is rather simple, since the address offset is not derived from the work-item's local ID
        // at all.
        uint sum = 0;
        for(uint i = 0; i < get_local_size(0); ++i)
        {
            sum += temp[i];
        }
        // TODO if we correctly handle group id written by a single work-item, rewrite to that
        out[get_global_id(0)] = sum;
    }
}

__kernel void test_cross_write_advanced(__global uint* out, __global uint* temp)
{
    // FIXME is lowered into 12 rows, but read below only reads 8 elements from first (and second) row,
    // independent of whether this is a __local variable or a __local parameter
    // __local uint temp[12];

    uint lid = get_local_id(0);
    temp[lid] = get_global_id(0);

    // This barrier is required, otherwise the memory contents is undefined anyway
    barrier(CLK_GLOBAL_MEM_FENCE);

    if(lid == 0)
    {
        // This code is only executed for local ID 0 and takes a lot longer than the writing of the next global ID into
        // the local buffer. Thus, if we omit inserting a work-group loop, we read some values already written by the
        // successive work-groups, which is wrong.
        // We make sure all memory offsets (accessing the temporary buffer) are derived from the local ID to trick our
        // check and see if it still can handle this case.
        uint8 sum = vload8(lid, temp);
        // TODO if we correctly handle group id written by a single work-item, rewrite to that
        out[get_global_id(0)] = sum.s0 + sum.s1 + sum.s2 + sum.s3 + sum.s4 + sum.s5 + sum.s6 + sum.s7;
    }
}
