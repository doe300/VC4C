// TODO add to optimization test
__kernel void test_simple_if_else(__global int* out, __global const int* in, __global const int* cond)
{
    uint gid = get_global_id(0);
    int val = in[gid];
    // is already converted to select(xxx) by clang
    if(cond[gid] > 42)
    {
        out[gid] = val;
    }
    else
    {
        out[gid] = -val;
    }
}

__kernel void test_simple_if(__global int* out, __global const int* in, __global const int* cond)
{
    uint gid = get_global_id(0);
    int val = in[gid];
    int factor = 1;
    // is already converted to select(xxx) by clang
    if(cond[gid] > 42)
    {
        val += 17;
        factor = -3;
    }
    out[gid] = -val * factor;
}

__kernel void test_switch_2_default(__global int* out, __global const int* in, __global const int* cond)
{
    uint gid = get_global_id(0);
    int val = in[gid];
    // is already converted to select(xxx) by clang
    switch(cond[gid])
    {
    case 17:
        out[gid] = val;
        break;
    default:
        out[gid] = -val;
    }
}

__kernel void test_switch_2(__global int* out, __global const int* in, __global const int* cond)
{
    uint gid = get_global_id(0);
    int val = in[gid];
    // is partially optimized by clang into:
    // a = load(in)
    // if(cond == 0) a = 42 - a
    // [no else clause]
    // store(a)
    switch(cond[gid])
    {
    case 0:
        out[gid] = -val + 42;
        break;
    case 17:
        out[gid] = val;
        break;
    }
}

__kernel void test_switch_more(__global int* out, __global const int* in, __global const int* cond)
{
    uint gid = get_global_id(0);
    int val = in[gid];
    switch(cond[gid])
    {
    case 0:
        out[gid] = -val;
        break;
    case -1:
        out[gid] = val * 5;
        break;
    case 1:
        out[gid] = val * 17;
        break;
    case 42:
        out[gid] = val - 42;
        break;
    }
}

__kernel void test_switch_more_default(__global int* out, __global const int* in, __global const int* cond)
{
    uint gid = get_global_id(0);
    int val = in[gid];
    switch(cond[gid])
    {
    case 0:
        out[gid] = -val;
        break;
    case -1:
        out[gid] = val ^ 45;
        break;
    case 1:
        out[gid] = val - 17;
        break;
    case 42:
        out[gid] = val + 42;
        break;
    default:
        out[gid] = val & 1404;
    }
}

// TODO more complex if-else with single output variable
// TODO if-else with multiple output variables
// TODO more complex switch with multiple cases