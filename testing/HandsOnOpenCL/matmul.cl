// Source: https://github.com/HandsOnOpenCL/Exercises-Solutions/blob/master/Solutions/Exercise06/C/matmul.c
// Original licensed under: Creative Commons Attribution 3.0 Unported License.
// Original source: https://github.com/HandsOnOpenCL/Exercises-Solutions
__kernel void mmul(const int N, __global float* A, __global float* B, __global float* C)
{
    int k;
    int i = get_global_id(0);
    int j = get_global_id(1);
    float tmp;
    if((i < N) && (j < N))
    {
        tmp = 0.0f;
        for(k = 0; k < N; k++)
            tmp += A[i * N + k] * B[k * N + j];
        C[i * N + j] = tmp;
    }
}