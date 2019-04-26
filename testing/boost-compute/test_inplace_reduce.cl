#define boost_pair_type(t1, t2) _pair_##t1##_##t2##_t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y)                                                                                  \
    (boost_pair_type(t1, t2))                                                                                          \
    {                                                                                                                  \
        x, y                                                                                                           \
    }
#define boost_tuple_get(x, n) (x.v##n)

__kernel void multiply_int(__global int* input, const uint input_size, __global int* output, __local int* scratch)
{
    const uint gid = get_global_id(0);
    const uint lid = get_local_id(0);
    const uint values_per_thread = 8;
    const uint index = gid * values_per_thread;
    if(index < input_size)
    {
        int sum = input[index];
        for(uint i = 1; i < values_per_thread && (index + i) < input_size; i++)
        {
            sum = ((sum) * (input[index + i]));
        }
        scratch[lid] = sum;
    }
    for(uint i = 1; i < get_local_size(0); i <<= 1)
    {
        barrier(CLK_LOCAL_MEM_FENCE);
        uint mask = (i << 1) - 1;
        uint next_index = (gid + i) * values_per_thread;
        if((lid & mask) == 0 && next_index < input_size)
        {
            scratch[lid] = ((scratch[lid]) * (scratch[lid + i]));
        }
    }
    if(lid == 0)
    {
        output[get_group_id(0)] = scratch[0];
    }
}

__kernel void sum_int(__global int* input, const uint input_size, __global int* output, __local int* scratch)
{
    const uint gid = get_global_id(0);
    const uint lid = get_local_id(0);
    const uint values_per_thread = 8;
    const uint index = gid * values_per_thread;
    if(index < input_size)
    {
        int sum = input[index];
        for(uint i = 1; i < values_per_thread && (index + i) < input_size; i++)
        {
            sum = ((sum) + (input[index + i]));
        }
        scratch[lid] = sum;
    }
    for(uint i = 1; i < get_local_size(0); i <<= 1)
    {
        barrier(CLK_LOCAL_MEM_FENCE);
        uint mask = (i << 1) - 1;
        uint next_index = (gid + i) * values_per_thread;
        if((lid & mask) == 0 && next_index < input_size)
        {
            scratch[lid] = ((scratch[lid]) + (scratch[lid + i]));
        }
    }
    if(lid == 0)
    {
        output[get_group_id(0)] = scratch[0];
    }
}

__kernel void copy(__global int* _buf0, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = (0 + index);
            index += 8;
        }
    }
}
