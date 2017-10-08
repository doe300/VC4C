__kernel void serial_adjacent_find(const uint size, __global uint* output, __global char* _buf0)
{
uint result = size;
for(uint i = 0; i < size - 1; i++){
    if(((_buf0[i+1])<(_buf0[i]))){
        result = i;
        break;
    }
}
*output = result;
}


#if T2_double
#pragma OPENCL EXTENSION cl_khr_fp64 : enable
#endif

#define K2_BITS (1 << K_BITS)
#define RADIX_MASK ((((T)(1)) << K_BITS) - 1)
#define SIGN_BIT ((sizeof(T) * CHAR_BIT) - 1)
#if defined(ASC)
inline uint radix(const T x, const uint low_bit)
{
#if defined(IS_FLOATING_POINT)
    const T mask = -(x >> SIGN_BIT) | (((T)(1)) << SIGN_BIT);
    return ((x ^ mask) >> low_bit) & RADIX_MASK;
#elif defined(IS_SIGNED)
    return ((x ^ (((T)(1)) << SIGN_BIT)) >> low_bit) & RADIX_MASK;
#else
    return (x >> low_bit) & RADIX_MASK;
#endif
}
#else
inline uint radix(const T x, const uint low_bit)
{
#if defined(IS_FLOATING_POINT)
    const T mask = -(x >> SIGN_BIT) | (((T)(1)) << SIGN_BIT);
    return (((-x) ^ mask) >> low_bit) & RADIX_MASK;
#elif defined(IS_SIGNED)
    return (((-x) ^ (((T)(1)) << SIGN_BIT)) >> low_bit) & RADIX_MASK;
#else
    return (((T)(-1) - x) >> low_bit) & RADIX_MASK;
#endif
}
#endif
__kernel void count(__global const T *input, const uint input_offset, const uint input_size, __global uint *global_counts, __global uint *global_offsets, __local uint *local_counts, const uint low_bit)
{
    const uint gid = get_global_id(0);
    const uint lid = get_local_id(0);
    if(lid < K2_BITS){
        local_counts[lid] = 0;
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    if(gid < input_size){
        T value = input[input_offset+gid];
        uint bucket = radix(value, low_bit);
        atomic_inc(local_counts + bucket);
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    if(lid < K2_BITS){
        global_counts[K2_BITS*get_group_id(0) + lid] = local_counts[lid];
        if(get_group_id(0) == (get_num_groups(0) - 1)){
        global_offsets[lid] = local_counts[lid];
        }
    }
}
__kernel void scan(__global const uint *block_offsets, __global uint *global_offsets, const uint block_count)
{
    __global const uint *last_block_offsets =
        block_offsets + K2_BITS * (block_count - 1);
    uint sum = 0;
    for(uint i = 0; i < K2_BITS; i++){
        uint x = global_offsets[i] + last_block_offsets[i];
        global_offsets[i] = sum;
        sum += x;
    }
}
__kernel void scatter(__global const T *input, const uint input_offset, const uint input_size, const uint low_bit, __global const uint *counts, __global const uint *global_offsets,
#ifndef SORT_BY_KEY
__global T *output,
const uint output_offset)
#else
__global T *keys_output,
const uint keys_output_offset,
__global T2 *values_input,
const uint values_input_offset,
__global T2 *values_output,
const uint values_output_offset)
#endif
{
    const uint gid = get_global_id(0);
    const uint lid = get_local_id(0);
    T value;
    uint bucket;
    __local uint local_input[BLOCK_SIZE];
    if(gid < input_size){
        value = input[input_offset+gid];
        bucket = radix(value, low_bit);
        local_input[lid] = bucket;
    }
    __local uint local_counts[(1 << K_BITS)];
    if(lid < K2_BITS){
        local_counts[lid] = counts[get_group_id(0) * K2_BITS + lid];
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    if(gid >= input_size){
        return;
    }
    uint offset = global_offsets[bucket] + local_counts[bucket];
    uint local_offset = 0;
    for(uint i = 0; i < lid; i++){
        if(local_input[i] == bucket)
	local_offset++;
    }
#ifndef SORT_BY_KEY
    output[output_offset + offset + local_offset] = value;
#else
    keys_output[keys_output_offset+offset + local_offset] = value;
    values_output[values_output_offset+offset + local_offset] =
        values_input[values_input_offset+gid];
#endif
}
