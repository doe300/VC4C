/*
 * Tests handling of branches
 */
__kernel void test_branches(const __global int* in, __global int* out_ptr)
{
    int tmp = in[get_local_id(0)];
    int result;
    bool test = tmp > 100;
    __global int* out = out_ptr + get_local_id(0) * 12;
    out[2] = tmp;
    if(tmp > 1000)
    {
        result = 1000;
        out[3] = result;
    }
    else if(test)
    {
        result = 100;
        out[3] = result;
    }
    else
    {
        result = 10;
        out[3] = result;
    }

    out[4] = result;
    out[5] = tmp;

    switch(tmp)
    {
    case 1024:
        result += 10;
        out[6] = result;
        break;
    case 512:
        result += 9;
        out[7] = result;
        break;
    case 256:
        result += 8;
        out[8] = result;
        break;
    case 64:
        result += 6;
        out[9] = result;
        break;
    case 32:
        result += 5;
        out[10] = result;
        break;
    default:
        result += 1;
        out[11] = result;
        break;
    }

    out[0] = result;

    while(result < 1024)
    {
        result *= 2;
        result += 7;
    }
    out[1] = result;
}

// mainly tests handling of sign-extension of case values
__kernel void test_short_switch(const __global short* in, __global short* out)
{
    uint gid = get_global_id(0);
    short val = in[gid];

    switch(val)
    {
    case 0x123:
        out[gid] = 11;
        break;
    case 0x6432:
        val = 17;
    // fall-through
    case 0x1345:
        out[gid] = val + 0x1245;
        break;
    case -0x567:
        out[gid] -= 0x0FFF;
        break;
    case -0x7777:
        out[gid] = 42;
        break;
    default:
        out[gid] = val;
    }
}

// mainly tests |case values| > 32 bit
__kernel void test_long_switch(const __global long* in, __global long* out)
{
    uint gid = get_global_id(0);
    long val = in[gid];

    switch(val)
    {
    case 0x12345678:
        out[gid] = 11;
        break;
    case 0x65432:
        val = 17;
    // fall-through
    case 0x12345:
        out[gid] = val + 0x1234500000000;
        break;
    case -0x12345671234567:
        out[gid] += 0x0FFF0000FFFF0000;
        break;
    case -0x12388887777:
        out[gid] = 42;
        break;
    default:
        out[gid] = val;
    }
}
