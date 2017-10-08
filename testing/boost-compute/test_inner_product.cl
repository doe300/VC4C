#define VPT 8
#define TPB 8


//manual test works
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

//manual tests runs, result not verified
__kernel void initial_reduce(const uint count, __global int* output, __global int* _buf0, __global int* _buf1)
{
const uint offset = get_group_id(0) * VPT * TPB;
const uint lid = get_local_id(0);
__local int scratch[TPB];
int sum = 0;
for(uint i = 0; i < VPT; i++){
    if(offset + lid + i*TPB < count){
        sum = sum + ((_buf0[offset+lid+i*TPB])*(_buf1[offset+lid+i*TPB]));
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
    output[get_group_id(0)] = scratch[0];
}
}
