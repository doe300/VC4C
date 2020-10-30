#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)

//TODO check, fix and add to emulation test

inline ulong boost_popcount(const ulong x)
{
#if __OPENCL_VERSION__ >= 120
    return popcount(x);
#else
    ulong count = 0;
    for(ulong i = 0; i < sizeof(i) * CHAR_BIT; i++){
        if(x & (ulong) 1 << i){
            count++;
        }
    }
    return count;
#endif
}



__kernel void copy(__global int* _buf0, __global ulong* _buf1, const uint count)
{
uint index = get_local_id(0) + (32 * get_group_id(0));
for(uint i = 0; i < 4; i++){
    if(index < count){
_buf0[index]=boost_popcount(_buf1[index]);
       index += 8;
    }
}

}

// Additional test to narrow down popcount result error
__kernel void popcount_uint(__global uint* _buf0, __global uint* _buf1, const uint count)
{
uint index = get_local_id(0) + (32 * get_group_id(0));
for(uint i = 0; i < 4; i++){
    if(index < count){
_buf0[index]=popcount(_buf1[index]);
       index += 8;
    }
}

}
