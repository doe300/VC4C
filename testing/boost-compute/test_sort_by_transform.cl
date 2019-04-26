#define boost_pair_type(t1, t2) _pair_##t1##_##t2##_t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y)                                                                                  \
    (boost_pair_type(t1, t2))                                                                                          \
    {                                                                                                                  \
        x, y                                                                                                           \
    }
#define boost_tuple_get(x, n) (x.v##n)

__kernel void serial_insertion_sort_by_key_int(
    __local int* keys, __local int* data, uint n, __global int* _buf0, __global int* _buf1)
{
    for(uint i = 0; i < n; i++)
    {
        keys[i] = _buf0[i];
        data[i] = _buf1[i];
    }
    for(uint i = 1; i < n; i++)
    {
        const int key = keys[i];
        const int value = data[i];
        uint pos = i;
        while(pos > 0 && ((key) < (keys[pos - 1])))
        {
            keys[pos] = keys[pos - 1];
            data[pos] = data[pos - 1];
            pos--;
        }
        keys[pos] = key;
        data[pos] = value;
    }
    for(uint i = 0; i < n; i++)
    {
        _buf0[i] = keys[i];
        _buf1[i] = data[i];
    }
}

__kernel void sort_int_by_abs(__global int* _buf0, __global int* _buf1, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = abs(_buf1[index]);
            index += 8;
        }
    }
}
__kernel void sort_vectors_by_length(__global float* _buf0, __global float4* _buf1, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = length(_buf1[index]);
            index += 8;
        }
    }
}

__kernel void sort_vectors_by_component(__global float* _buf0, __global float4* _buf1, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = _buf1[index].s1;
            index += 8;
        }
    }
}

__kernel void serial_insertion_sort_by_key_float(
    __local float* keys, __local float4* data, uint n, __global float* _buf0, __global float4* _buf1)
{
    for(uint i = 0; i < n; i++)
    {
        keys[i] = _buf0[i];
        data[i] = _buf1[i];
    }
    for(uint i = 1; i < n; i++)
    {
        const float key = keys[i];
        const float4 value = data[i];
        uint pos = i;
        while(pos > 0 && ((key) < (keys[pos - 1])))
        {
            keys[pos] = keys[pos - 1];
            data[pos] = data[pos - 1];
            pos--;
        }
        keys[pos] = key;
        data[pos] = value;
    }
    for(uint i = 0; i < n; i++)
    {
        _buf0[i] = keys[i];
        _buf1[i] = data[i];
    }
}
