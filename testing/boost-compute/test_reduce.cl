#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)

#define VPT 8
// This needs to be a power of two for the second loop not to access any out-of-bounds memory
#define TPB 8

__kernel void reduce(__global int* input, const uint offset, const uint count, __global int* output, const uint output_offset)
{
const uint block_offset = get_group_id(0) * VPT * TPB;
__global const int *block = input + offset + block_offset;
const uint lid = get_local_id(0);
__local int scratch[TPB];
int sum = 0;
for(uint i = 0; i < VPT; i++){
    if(block_offset + lid + i*TPB < count){
        sum = sum + block[lid+i*TPB];
    }
}
scratch[lid] = sum;
for(int i = 1; i < TPB; i <<= 1){
   barrier(CLK_LOCAL_MEM_FENCE);
   uint mask = (i << 1) - 1;
   if((lid & mask) == 0){
       scratch[lid] += scratch[lid+i];
   }
}
if(lid == 0){
    output[output_offset + get_group_id(0)] = scratch[0];
}

}

__kernel void reduce_uint(__global uint* input, const uint offset, const uint count, __global uint* output, const uint output_offset)
{
const uint block_offset = get_group_id(0) * VPT * TPB;
__global const uint *block = input + offset + block_offset;
const uint lid = get_local_id(0);
__local uint scratch[TPB];
uint sum = 0;
for(uint i = 0; i < VPT; i++){
    if(block_offset + lid + i*TPB < count){
        sum = sum + block[lid+i*TPB];
    }
}
scratch[lid] = sum;
for(int i = 1; i < TPB; i <<= 1){
   barrier(CLK_LOCAL_MEM_FENCE);
   uint mask = (i << 1) - 1;
   if((lid & mask) == 0){
       scratch[lid] += scratch[lid+i];
   }
}
if(lid == 0){
    output[output_offset + get_group_id(0)] = scratch[0];
}

}

__kernel void reduce_float(__global float2* input, const uint offset, const uint count, __global float2* output, const uint output_offset)
{
const uint block_offset = get_group_id(0) * VPT * TPB;
__global const float2 *block = input + offset + block_offset;
const uint lid = get_local_id(0);
__local float2 scratch[TPB];
float2 sum = 0;
for(uint i = 0; i < VPT; i++){
    if(block_offset + lid + i*TPB < count){
        sum = sum + block[lid+i*TPB];
    }
}
scratch[lid] = sum;
for(int i = 1; i < TPB; i <<= 1){
   barrier(CLK_LOCAL_MEM_FENCE);
   uint mask = (i << 1) - 1;
   if((lid & mask) == 0){
       scratch[lid] += scratch[lid+i];
   }
}
if(lid == 0){
    output[output_offset + get_group_id(0)] = scratch[0];
}

}

__kernel void extra_serial_reduce_as_float(uint count, uint offset, __global float2* output, uint output_offset, __global float2* _buf0)
{
float2 result = _buf0[offset];
for(uint i = offset + 1; i < count; i++)
    result = (float2)(result.x*_buf0[i].x-result.y*_buf0[i].y,result.y*_buf0[i].x+result.x*_buf0[i].y);
output[output_offset] = result;

}

__kernel void extra_serial_reduce_min_int(uint count, uint offset, __global int* output, uint output_offset, __global int* _buf0)
{
int result = _buf0[offset];
for(uint i = offset + 1; i < count; i++)
    result = min(result, _buf0[i]);
output[output_offset] = result;

}
