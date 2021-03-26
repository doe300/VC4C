__kernel void constant_kernel(__global float* out, __constant float* tmpF, __constant int* tmpI)
{
    int tid = get_global_id(0);

    float ftmp = tmpF[tid];
    float Itmp = tmpI[tid];
    out[tid] = ftmp * Itmp;
}

kernel void loop_constant_kernel(global float* out, constant float* i_pos, int num)
{
    int tid = get_global_id(0);
    float sum = 0;
    for(int i = 0; i < num; i++)
    {
        float pos = i_pos[i * 3];
        sum += pos;
    }
    out[tid] = sum;
}
