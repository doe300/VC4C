__kernel void sum(__global const float *a,
__global const float *b, __global float *c)
{
	int gid = get_global_id(0);
	float a_temp;
	float b_temp;
	float c_temp;
	a_temp = a[gid]; // my a element (by global ref)
	b_temp = b[gid]; // my b element (by global ref)
	
	c_temp = a_temp+b_temp; // sum of my elements
	c_temp = c_temp * c_temp; // product of sums
	c_temp = c_temp * (a_temp/2.0f); // times 1/2 my a
	c[gid] = c_temp; // store result in global memory
}