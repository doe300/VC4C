__kernel void test_vload_half(const __global half* p, __global float* f)
{
    size_t i = get_global_id(0);
    f[i] = vload_half(i, p);
}

__kernel void test_vloada_half(const __global half* p, __global float* f)
{
    size_t i = get_global_id(0);
    f[i] = vloada_half(i, p);
}

__kernel void test_vstore_half(__global float* p, __global half* f)
{
    size_t i = get_global_id(0);
    vstore_half(p[i], i, f);
}

__kernel void test_vstorea_half2(__global float2* p, __global half* f)
{
    size_t i = get_global_id(0);
    vstorea_half2(p[i], i, f);
}

__kernel void test_vstore_half_rte(__global float* p, __global half* f)
{
    size_t i = get_global_id(0);
    vstore_half_rte(p[i], i, f);
}

__kernel void test_vstorea_half2_rte(__global float2* p, __global half* f)
{
    size_t i = get_global_id(0);
    vstorea_half2_rte(p[i], i, f);
}

__kernel void test_vstore_half_rtz(__global float* p, __global half* f)
{
    size_t i = get_global_id(0);
    vstore_half_rtz(p[i], i, f);
}

__kernel void test_vstorea_half2_rtz(__global float2* p, __global half* f)
{
    size_t i = get_global_id(0);
    vstorea_half2_rtz(p[i], i, f);
}

__kernel void test_vstore_half_rtp(__global float* p, __global half* f)
{
    size_t i = get_global_id(0);
    vstore_half_rtp(p[i], i, f);
}

__kernel void test_vstorea_half2_rtp(__global float2* p, __global half* f)
{
    size_t i = get_global_id(0);
    vstorea_half2_rtp(p[i], i, f);
}

__kernel void test_vstore_half_rtn(__global float* p, __global half* f)
{
    size_t i = get_global_id(0);
    vstore_half_rtn(p[i], i, f);
}

__kernel void test_vstorea_half2_rtn(__global float2* p, __global half* f)
{
    size_t i = get_global_id(0);
    vstorea_half2_rtn(p[i], i, f);
}

__kernel void test_vload_vstore_half(const __global half* in, __global half* out)
{
    size_t i = get_global_id(0);
    vstore_half(vload_half(i, in), i, out);
}

__kernel void test_vload_half2_private(const __global half* p, __global float2* f)
{
    __private int data[2 / 2];
    __private half* hdata_p = (__private half*) data;
    __global int* i_p = (__global int*) p;
    size_t i = get_global_id(0);
    int k;
    for(k = 0; k < 2 / 2; k++)
        data[k] = i_p[i + k];
    f[i] = vload_half2(0, hdata_p);
}

__kernel void test_vload_half3_private(const __global half* p, __global float* f, uint extra_last_thread)
{
    __private ushort data[3];
    __private half* hdata_p = (__private half*) data;
    __global ushort* i_p = (__global ushort*) p;
    size_t i = get_global_id(0);
    int k;
    size_t last_i = get_global_size(0) - 1;
    if(last_i == i && extra_last_thread != 0)
    {
        if(extra_last_thread == 2)
        {
            f[3 * i + 1] = vload_half(3 * i + 1, p);
        }
        f[3 * i] = vload_half(3 * i, p);
    }
    else
    {
        for(k = 0; k < 3; k++)
            data[k] = i_p[i * 3 + k];
        vstore3(vload_half3(0, hdata_p), i, f);
    }
}

__kernel void test_vstore_half4_private( __global float4 *p, __global half *f )
{
   __private ushort data[16];
   size_t i = get_global_id(0);
   size_t offset = 0;
   size_t vecsize = vec_step(p[i]);
   vstore_half4( p[i], 0, (__private half *)(&data[0]) );
   for(offset = 0; offset < vecsize; offset++)
   {
       vstore_half(vload_half(offset, (__private half *)data), 0, &f[vecsize*i+offset]);
   }
}