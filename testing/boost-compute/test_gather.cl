/*
__kernel void gather(__global int* _buf0, __global int* _buf1, __global int* _buf2)
{
const uint i = get_global_id(0);
_buf0[i]=_buf1[_buf2[i]];
}


inline bool is_odd(int x){ return x % 2 != 0; }
__kernel void transform_if_write_counts(__global uint* _buf0, __global int* _buf1)
{
_buf0[get_global_id(0)] = is_odd(_buf1[get_global_id(0)]) ? 1 : 0;
}
*/

//manual test runs fine
__kernel void local_scan(__global uint* block_sums, __local uint* scratch, const uint block_size, const uint count, const uint init, __global uint* _buf0, __global uint* _buf1)
{
const uint gid = get_global_id(0);
const uint lid = get_local_id(0);
if(gid < count){
const uint local_init= (gid == 0) ? init : 0;
if(lid == 0){ scratch[lid] = local_init; }
else { scratch[lid] = _buf0[gid-1]; }
}
else {
    scratch[lid] = 0;
}
barrier(CLK_LOCAL_MEM_FENCE);
for(uint i = 1; i < block_size; i <<= 1){
    const uint x = lid >= i ? scratch[lid-i] : 0;
    barrier(CLK_LOCAL_MEM_FENCE);
    if(lid >= i){
        scratch[lid] = ((scratch[lid])+(x));
    }
    barrier(CLK_LOCAL_MEM_FENCE);
}
if(gid < count){
_buf1[gid] = scratch[lid];
}
if(lid == block_size - 1){
    block_sums[get_group_id(0)] = ((_buf0[gid])+(scratch[lid]));
}
}

/*
__kernel void transform_if_do_copy(__global int* _buf0, __global int* _buf1, __global uint* _buf2)
{
if(is_odd(_buf0[get_global_id(0)]))
    _buf1[_buf2[get_global_id(0)]]=get_global_id(0);
}
*/
