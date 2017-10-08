__kernel void test_conversion(__global char *sourceValues, __global char2 *destValues )
{
    int  tid = get_global_id(0);
    char  src = sourceValues[tid];

    destValues[tid] = (char2)src;
}
