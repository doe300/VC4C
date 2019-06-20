__kernel void test_cross(__global float *x, __global float *y, __global float *dst)
{
    int  tid = get_global_id(0);

    vstore3(cross(vload3(tid, x), vload3(tid, y)), tid, dst);
}
