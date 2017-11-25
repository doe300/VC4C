__kernel void serial_adjacent_find(const uint size, __global uint* output, __global int* _buf0)
{
uint result = size;
for(uint i = 0; i < size - 1; i++){
    if(_buf0[i]==_buf0[i+1]){
        result = i;
        break;
    }
}
*output = result;
}

__kernel void serial_adjacent_find2(const uint size, __global uint* output, __global int2* _buf0){
uint result = size;
for(uint i = 0; i < size - 1; i++){
    if(all(_buf0[i]==_buf0[i+1])){
        result = i;
        break;
    }
}
*output = result;
}


__kernel void adjacent_find_with_atomics(__global uint* output, __global int* _buf0)
{
const uint i = get_global_id(0);
if(_buf0[i]==_buf0[i+1]){
    atomic_min(output, i);
}
}
