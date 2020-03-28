#define boost_pair_type(t1, t2) _pair_##t1##_##t2##_t

#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)

#define boost_make_pair(t1, x, t2, y)                                                                                  \
    (boost_pair_type(t1, t2))                                                                                          \
    {                                                                                                                  \
        x, y                                                                                                           \
    }

#define boost_tuple_get(x, n) (x.v##n)

__kernel void serial_unique_copy(const uint size, __global uint* unique_count, __global int* _buf0, __global int* _buf1)

{
    uint index = 0;

    int current = _buf0[0];

    _buf1[0] = current;

    for(uint i = 1; i < size; i++)
    {
        int next = _buf0[i];

        if(!((current) == (next)))
        {
            _buf1[++index] = next;

            current = next;
        }
    }

    *unique_count = index + 1;
}
