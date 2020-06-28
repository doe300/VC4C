// Helper kernel which is used among others in: test_count, test_discrete_distribution, test_dynamic_bitset
#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)

#define VPT 8
#define TPB 8

__kernel void initial_reduce(const uint count, __global ulong* output, __global uchar* _buf0)
{
const uint offset = get_group_id(0) * VPT * TPB;
const uint lid = get_local_id(0);
__local ulong scratch[TPB];
ulong sum = 0;
for(uint i = 0; i < VPT; i++){
    if(offset + lid + i*TPB < count){
        sum = sum + (_buf0[offset+lid+i*TPB]==(uchar)(1u) ? 1 : 0);
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
