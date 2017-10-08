#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)

inline uint nth_fibonacci(uint n){ const float golden_ratio = (1.f + sqrt(5.f)) / 2.f; return floor(pown(golden_ratio, n) / sqrt(5.f) + 0.5f); }


__kernel void copy(__global uint* _buf0, const uint count)
{
uint index = get_local_id(0) + (32 * get_group_id(0));
for(uint i = 0; i < 4; i++){
    if(index < count){
_buf0[index]=nth_fibonacci((0+index));
       index += 8;
    }
}

}

