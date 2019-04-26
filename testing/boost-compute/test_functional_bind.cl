#define boost_pair_type(t1, t2) _pair_##t1##_##t2##_t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y)                                                                                  \
    (boost_pair_type(t1, t2))                                                                                          \
    {                                                                                                                  \
        x, y                                                                                                           \
    }
#define boost_tuple_get(x, n) (x.v##n)

__kernel void subtract_ranges(__global int* _buf0, __global int* _buf1, __global int* _buf2, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = ((_buf1[index]) - (_buf2[index]));
            index += 8;
        }
    }
}
inline int x_if_odd_else_y(int x, int y)
{
    if(x & 1)
        return x;
    else
        return y;
}

__kernel void bind_custom_function2(__global int* _buf0, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = x_if_odd_else_y(_buf0[index], 9);
            index += 8;
        }
    }
}
__kernel void transform_pow_two(__global float* _buf0, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = pow(2.00000f, _buf0[index]);
            index += 8;
        }
    }
}

__kernel void transform_plus_two(__global int* _buf0, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = ((_buf0[index]) + (2));
            index += 8;
        }
    }
}
__kernel void find_if(__global int* index, __global int* _buf0)
{
    const uint i = get_global_id(0);
    const int value = _buf0[i];
    if(((value) == (3)))
    {
        atomic_min(index, i);
    }
}
__kernel void clamp_values(__global int* _buf0, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = clamp(_buf0[index], 2, 3);
            index += 8;
        }
    }
}

__kernel void bind_custom_function1(__global int* _buf0, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = x_if_odd_else_y(2, _buf0[index]);
            index += 8;
        }
    }
}

typedef struct __attribute__((packed))
{
    int int_value;
    float float_value;
} data_struct;

inline int add_struct_value(int x, data_struct s)
{
    return s.int_value + x;
}

__kernel void bind_struct(__global int* _buf0, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = add_struct_value(_buf0[index], (data_struct){3, 4.56000f});
            index += 8;
        }
    }
}

__kernel void compare_less_than1(uint size, __global uint* result, __global int* _buf0)
{
    uint count = 0;
    for(uint i = 0; i < size; i++)
    {
        const int value = _buf0[i];
        if(((3) < (value)))
        {
            count++;
        }
    }
    *result = count;
}

__kernel void compare_less_than2(uint size, __global uint* result, __global int* _buf0)
{
    uint count = 0;
    for(uint i = 0; i < size; i++)
    {
        const int value = _buf0[i];
        if(((value) < (3)))
        {
            count++;
        }
    }
    *result = count;
}

__kernel void copy(__global int* _buf0, __global int* _buf1, const uint count)
{
    uint index = get_local_id(0) + (32 * get_group_id(0));
    for(uint i = 0; i < 4; i++)
    {
        if(index < count)
        {
            _buf0[index] = ((5) - (_buf1[index]));
            index += 8;
        }
    }
}
