// Taken from: https://raw.githubusercontent.com/kernhanda/opencl_dot_product/master/dot_product.cl
__kernel void dot_product(__global float4* a_vec, __global float4* b_vec,
      __global float* output, __local float4* partial_dot) {

   int gid = get_global_id(0);
   int lid = get_local_id(0);
   int group_size = get_local_size(0);

   partial_dot[lid] = a_vec[gid] * b_vec[gid];
   barrier(CLK_LOCAL_MEM_FENCE);

   for(int i = group_size/2; i>0; i >>= 1) {
      if(lid < i) {
         partial_dot[lid] += partial_dot[lid + i];
      }
      barrier(CLK_LOCAL_MEM_FENCE);
   }

   if(lid == 0) {
      output[get_group_id(0)] = dot(partial_dot[0], (float4)(1.0f));
   }
}
