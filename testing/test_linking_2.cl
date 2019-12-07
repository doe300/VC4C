
int get_value_inner(const __global int* in)
{
    return get_global_id(0) + *in;
}
