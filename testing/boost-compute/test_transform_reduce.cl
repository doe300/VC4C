#define boost_pair_type(t1, t2) _pair_##t1##_##t2##_t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y)                                                                                  \
    (boost_pair_type(t1, t2))                                                                                          \
    {                                                                                                                  \
        x, y                                                                                                           \
    }
#define boost_tuple_get(x, n) (x.v##n)

__kernel void extra_serial_reduce(
    uint count, uint offset, __global float* output, uint output_offset, __global float4* _buf0)
{
    float result = length(_buf0[offset]);
    for(uint i = offset + 1; i < count; i++)
        result = ((result) * (length(_buf0[i])));
    output[output_offset] = result;
}
