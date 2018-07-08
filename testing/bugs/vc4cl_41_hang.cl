__kernel void TestMagic(const int total, const int is_deeper_magic, const float alpha_s, const float fore_th, __global const float* gradx, __global const float* grady,
    //in/out
    __global float* BSx,  __global float* BSy, __global int* mapRes)
{
    private const size_t i        = get_global_id(0);
    private const size_t gpu_used = get_global_size(0);

    private const size_t elements_count = total / (gpu_used * 16);
    private const size_t offset = i * total / gpu_used;
 
    for (size_t k = 0; k < elements_count; ++k)
    {
        int16 mr = vload16( k , mapRes + offset);
        const int16 twos   = 2;
        mr += twos;
        vstore16(mr, k, mapRes + offset);
   }
}
