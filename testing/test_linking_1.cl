
extern int get_value_inner(const __global int* in);

int get_value(const __global int* in)
{
    return get_global_id(0) + get_value_inner(in) % 0xFFFF;
}
