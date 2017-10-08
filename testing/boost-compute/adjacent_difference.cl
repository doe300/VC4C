#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)

__kernel void adjacent_difference(__global int* _buf0, __global int* _buf1)
{
const uint i = get_global_id(0);
if(i == 0){
    _buf0[0] = _buf1[0];
}
else {
    _buf0[i] = ((_buf1[i])-(_buf1[i-1]));
}

}
