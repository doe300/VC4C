//code by @harleyzhang (taken from: https://github.com/doe300/VC4C/issues/30) 
__kernel void dot3(__global float* a_vec, __global float* b_vec,__global float* output, __local float* l_vec) {
	size_t gid = get_global_id(0);
	size_t lid = get_local_id(0);
	
	l_vec[lid] = a_vec[gid] * b_vec[gid];

	barrier(CLK_LOCAL_MEM_FENCE);
	if(lid==0) {
		l_vec[lid] = l_vec[lid] + l_vec[lid+1]; //sometimes, this line is ignored.
		//l_vec[0] = l_vec[0] + l_vec[1];
	}
	
	output[gid] = l_vec[lid];
}

__kernel void dot3_local(__global float* a_vec, __global float* b_vec,__global float* output) {
	size_t gid = get_global_id(0);
	size_t lid = get_local_id(0);
	
	__local float l_vec[12];
	
	l_vec[lid] = a_vec[gid] * b_vec[gid];

	barrier(CLK_LOCAL_MEM_FENCE);
	if(lid==0) {
		l_vec[lid] = l_vec[lid] + l_vec[lid+1]; //sometimes, this line is ignored.
		//l_vec[0] = l_vec[0] + l_vec[1];
	}
	
	output[gid] = l_vec[lid];
}
