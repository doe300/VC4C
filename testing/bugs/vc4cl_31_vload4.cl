

__kernel void test_vload4(__global float* g, __local float* l, __constant float* c, __global float4* out)
{
    __private float p[4];
    vstore4(vload4(0, g), 0, p);
    barrier(0);
    out[0] = vload4(0, g);
    out[1] = vload4(0, l);
    out[2] = vload4(0, c);
    out[3] = vload4(0, p);
}
