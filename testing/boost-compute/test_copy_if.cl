#define boost_pair_type(t1, t2) _pair_##t1##_##t2##_t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y)                                                                                  \
    (boost_pair_type(t1, t2))                                                                                          \
    {                                                                                                                  \
        x, y                                                                                                           \
    }
#define boost_tuple_get(x, n) (x.v##n)

__kernel void local_scan(__global uint* block_sums, __local uint* scratch, const uint block_size, const uint count,
    const uint init, __global uint* _buf0, __global uint* _buf1)
{
    const uint gid = get_global_id(0);
    const uint lid = get_local_id(0);
    if(gid < count)
    {
        const uint local_init = (gid == 0) ? init : 0;
        if(lid == 0)
        {
            scratch[lid] = local_init;
        }
        else
        {
            scratch[lid] = _buf0[gid - 1];
        }
    }
    else
    {
        scratch[lid] = 0;
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    for(uint i = 1; i < block_size; i <<= 1)
    {
        const uint x = lid >= i ? scratch[lid - i] : 0;
        barrier(CLK_LOCAL_MEM_FENCE);
        if(lid >= i)
        {
            scratch[lid] = ((scratch[lid]) + (x));
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    if(gid < count)
    {
        _buf1[gid] = scratch[lid];
    }
    if(lid == block_size - 1)
    {
        // TODO bug?? _buf0[gid] is out-of-bounds, if block_size > count
        block_sums[get_group_id(0)] = ((_buf0[gid]) + (scratch[lid]));
    }
}