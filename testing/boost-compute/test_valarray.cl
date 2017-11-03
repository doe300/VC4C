#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)


__kernel void serial_find_extrema(__global int* _buf0, __global uint* index, uint size)
{
int value = _buf0[0];
uint value_index = 0;
for(uint i = 1; i < size; i++){
  int candidate=_buf0[i];
#ifndef BOOST_COMPUTE_FIND_MAXIMUM
  if(((candidate)<(value))){
#else
  if(((value)<(candidate))){
#endif
    value = candidate;
    value_index = i;
  }
}
*index = value_index;

}
