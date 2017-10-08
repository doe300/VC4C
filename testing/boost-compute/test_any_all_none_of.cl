#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)


__kernel void find_if(__global int* index, __global float* _buf0)
{
const uint i = get_global_id(0);
const float value=_buf0[i];
if((isinf(value))||(isnan(value))){
    atomic_min(index, i);
}

}
