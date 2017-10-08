__kernel void binary_find(__global uint* index, uint block, __global int* _buf0, uint needle)
{
uint i = get_global_id(0) * block;
int value=_buf0[i];
if(value>=needle) {
atomic_min(index, i);
}
}

__kernel void find_if(__global int* index, __global int* _buf0)
{
const uint i = get_global_id(0);
const int value=_buf0[1167+(i)];
if(value>=3){
    atomic_min(index, i);
}

}

