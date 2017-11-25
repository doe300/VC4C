#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)

__kernel void search(uint p_count, __global char* _buf0, __global char* _buf1, __global uint* _buf2)
{
uint i = get_global_id(0);
const uint i1 = i;
uint j;
for(j = 0; j<p_count; j++,i++)
{
   if(_buf0[j] != _buf1[i])
       j = p_count + 1;
}
if(j == p_count)
_buf2[i1] = 1;
else
_buf2[i1] = 0;

}

__kernel void find_if(__global int* index, __global uint* _buf0)
{
const uint i = get_global_id(0);
const uint value=_buf0[i];
if(value==(1u)){
    atomic_min(index, i);
}
}
