__kernel void test_barrier_fence(__global const int4* in, __global int4* out)
{
	int offset = 0 ; //TODO get_global_id(0);
	int4 val = 0;
	val += in[offset];
	val += in[offset + 1];
	val += in[offset + 2];
	val += in[offset + 3];
	//reads/writes must not be combined accross mem-fences
	read_mem_fence(CLK_LOCAL_MEM_FENCE);
	val += in[offset + 4];
	val += in[offset + 5];
	val += in[offset + 6];
	out[0] = val;
	out[1] = val;
	out[2] = val;
	write_mem_fence(CLK_LOCAL_MEM_FENCE);
	out[3] = val;
	out[4] = val;
	out[5] = val;
	mem_fence(CLK_GLOBAL_MEM_FENCE);
	out[6] = val;
	out[7] = val;
	barrier(CLK_GLOBAL_MEM_FENCE);
	out[8] = val;
	out[9] = val;
}
