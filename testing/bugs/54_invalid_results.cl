//See https://github.com/doe300/VC4C/issues/54 for discussion
__kernel void sum(__global float *c)
{
	int gid = get_global_id(0);
	c[gid] = 1.f;
}
