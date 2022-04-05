#ifdef dynamic_offset
#define OFFSET offset
#elif defined(static_offset)
#define OFFSET 3
#else
#define OFFSET 0
#endif

// Simple case, both addresses are of same type (__global parameter)
__kernel void test_select_write_address_simple(
    const __global int* in, __global int* out0, __global int* out1, uint offset)
{
    uint gid = (uint) get_global_id(0);
    __global int* ptr = (gid & 1) ? out1 + OFFSET : out0;
    int tmp = in[gid];
    ptr[gid] = tmp + 17;
}

__kernel void test_select_read_address_simple(
    const __global int* in0, const __global int* in1, __global int* out, uint offset)
{
    uint gid = (uint) get_global_id(0);
    const __global int* ptr = (gid & 1) ? in1 + OFFSET : in0;
    int tmp = ptr[gid];
    out[gid] = tmp + 17;
}

__kernel void test_select_read_write_address_simple(__global int* mem0, __global int* mem1, uint offset)
{
    uint gid = (uint) get_global_id(0);
    __global int* in = (gid & 1) ? mem0 : mem1 + OFFSET;
    __global int* out = !(gid & 1) ? mem0 : mem1 + OFFSET;
    int tmp = in[gid];
    out[gid] = tmp + 17;
}

__kernel void test_select_copy_address_simple(__global int* mem0, __global int* mem1, uint offset)
{
    uint gid = (uint) get_global_id(0);
    __global int* in = (gid & 1) ? mem0 : mem1 + OFFSET;
    __global int* out = !(gid & 1) ? mem0 : mem1 + OFFSET;
    out[gid] = in[gid];
}

// Little bit more advanced case, addresses of same type (__global parameter) are selected via phi node
__kernel void test_phi_write_address_simple(
    const __global int* in, __global int* out0, __global int* out1, __global unsigned* cnt, uint offset)
{
    unsigned count = *cnt;
    uint gid = (uint) get_global_id(0);
    uint gsize = (uint) get_global_size(0);
    int tmp = in[gid];
    __global int* ptr = (gid & 1) ? out1 + OFFSET : out0;
    for(unsigned i = 0; i < count; ++i)
    {
        ptr[gid] = tmp + 17;
        ptr += gsize;
    }
}

__kernel void test_phi_read_address_simple(
    const __global int* in0, const __global int* in1, __global int* out, __global unsigned* cnt, uint offset)
{
    unsigned count = *cnt;
    uint gid = (uint) get_global_id(0);
    uint gsize = (uint) get_global_size(0);
    const __global int* ptr = (gid & 1) ? in1 + OFFSET : in0;
    for(unsigned i = 0; i < count; ++i)
    {
        int tmp = ptr[gid];
        out[gid] = tmp + 17;
        ptr += gsize;
        out += gsize;
    }
}

__kernel void test_phi_read_write_address_simple(
    __global int* mem0, __global int* mem1, __global unsigned* cnt, uint offset)
{
    unsigned count = *cnt;
    uint gid = (uint) get_global_id(0);
    uint gsize = (uint) get_global_size(0);
    __global int* in = (gid & 1) ? mem0 : mem1 + OFFSET;
    __global int* out = !(gid & 1) ? mem0 : mem1 + OFFSET;
    for(unsigned i = 0; i < count; ++i)
    {
        int tmp = in[gid];
        out[gid] = tmp + 17;
        in += gsize;
        out += gsize;
    }
}

__kernel void test_phi_copy_address_simple(__global int* mem0, __global int* mem1, __global unsigned* cnt, uint offset)
{
    unsigned count = *cnt;
    uint gid = (uint) get_global_id(0);
    uint gsize = (uint) get_global_size(0);
    __global int* in = (gid & 1) ? mem0 : mem1 + OFFSET;
    __global int* out = !(gid & 1) ? mem0 : mem1 + OFFSET;
    for(unsigned i = 0; i < count; ++i)
    {
        out[gid] = in[gid];
        in += gsize;
        out += gsize;
    }
}

// More advanced case, addresses of different type (__local parameter vs. __local buffer) are accessed via selection
__kernel void test_select_write_address_local(const __global int* in, __local int* out0, uint offset)
{
    // fits into VPM on purpose, will still be located in RAM, since the conditional accessed memory locations need
    // to be stored in the same memory type (VPM, RAM) for now.
    __local int out1[16];
    uint gid = (uint) get_global_id(0);
    __local int* ptr = (gid & 1) ? out1 + OFFSET : out0;
    int tmp = in[gid];
    ptr[gid] = tmp + 17;
}

__kernel void test_select_read_address_local(__local int* in0, __global int* out, uint offset)
{
    // fits into VPM on purpose, will still be located in RAM, since the conditional accessed memory locations need
    // to be stored in the same memory type (VPM, RAM) for now.
    __local int in1[16];
    uint gid = (uint) get_global_id(0);
    // need to initially set some value to not be optimized away (or return undefined values)
    in0[gid] = (int) gid;
    in1[gid + OFFSET] = (int) gid;
    const __local int* ptr = (gid & 1) ? in1 + OFFSET : in0;
    int tmp = ptr[gid];
    out[gid] = tmp + 17;
}

// TODO not yet implemented
// __kernel void test_select_write_address_private(__global int* in, __global int* out)
// {
//     __private int priv0[16] = {17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17};
//     __private int priv1[16] = {42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42};
//     uint lid = (uint) get_local_id(0);
//     uint gid = (uint) get_global_id(0);

//     __private int* ptr = (gid & 1) ? priv0 : priv1;
//     ptr[lid] = in[gid] + 17;
//     out[gid] = priv0[lid] + priv1[lid];
// }

__kernel void test_select_read_address_private(__global int* in, __global int* out, uint offset)
{
    __private int priv0[16] = {17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32};
    __private int priv1[16] = {42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57};
    uint lid = (uint) get_local_id(0);
    uint gid = (uint) get_global_id(0);
    // non-conditional write, required so our data is not compile-time constant
    priv0[lid] = in[gid];
    priv1[lid] = in[gid];

    const __private int* ptr = (gid & 1) ? priv0 : priv1 + OFFSET;
    int tmp = ptr[gid];
    out[gid] = tmp + 17;
}

// // TODO not yet implemented
// __kernel void test_select_read_write_address_private(__global int* in, __global int* out)
// {
//     __private int priv0[16] = {17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17};
//     __private int priv1[16] = {42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42};
//     uint lid = (uint) get_local_id(0);
//     uint gid = (uint) get_global_id(0);

//     __private int* ptr = (gid & 1) ? priv0 : priv1;
//     ptr[lid] += in[gid] + 17;
//     out[gid] = priv0[lid] + priv1[lid];
// }

// TODO add test or address modified in loop, with base at least partially referring to itself -> test elimination of
// loop phi-nodes
// TODO add test for lowered (register, VPM private, VPM shared) memory areas
