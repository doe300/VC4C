__kernel void test_step_type(__global char *source, __global int *dest)
{
    int  tid = get_global_id(0);
    dest[tid] = vec_step(char);

}
