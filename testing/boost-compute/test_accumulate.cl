#define T int
#define VPT 8
#define TPB 8


#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)


__kernel void reduce(__global int* input, const uint offset, const uint count, __global int* output, const uint output_offset)
{
//possbile problem 1: __local data access
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
//up to here: all values in scratch are correct!


//possible problem 2: barrier
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

__kernel void serial_accumulate(int init, uint count, __global int* _buf0)
{
int result = init;
for(uint i = 0; i < count; i++)
    result = ((result)+((0+i)));
_buf0[0] = result;

}