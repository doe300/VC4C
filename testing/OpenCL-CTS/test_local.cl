__kernel void test_local_arg_def(__global int* a, int n, __local int* tmp_sum, __global int* sum)
{
    int tid = get_local_id(0);
    int lsize = get_local_size(0);
    int i;

    tmp_sum[tid] = 0;
    for(i = tid; i < n; i += lsize)
        tmp_sum[tid] += a[i];

    if(lsize == 1)
    {
        if(tid == 0)
            *sum = tmp_sum[0];
        return;
    }

    do
    {
        barrier(CLK_LOCAL_MEM_FENCE);
        if(tid < lsize / 2)
        {
            int sum = tmp_sum[tid];
            if((lsize & 1) && tid == 0)
                sum += tmp_sum[tid + lsize - 1];
            tmp_sum[tid] = sum + tmp_sum[tid + lsize / 2];
        }
        lsize = lsize / 2;
    } while(lsize);

    if(tid == 0)
        *sum = tmp_sum[0];
}

__kernel void test_local_kernel_def(__global int* a, int n, __global int* sum)
{
    __local int tmp_sum[WORK_GROUP_SIZE];
    int tid = get_local_id(0);
    int lsize = get_local_size(0);
    int i;

    tmp_sum[tid] = 0;
    for(i = tid; i < n; i += lsize)
        tmp_sum[tid] += a[i];

    if(lsize == 1)
    {
        if(tid == 0)
            *sum = tmp_sum[0];
        return;
    }

    do
    {
        barrier(CLK_LOCAL_MEM_FENCE);
        if(tid < lsize / 2)
        {
            int sum = tmp_sum[tid];
            if((lsize & 1) && tid == 0)
                sum += tmp_sum[tid + lsize - 1];
            tmp_sum[tid] = sum + tmp_sum[tid + lsize / 2];
        }
        lsize = lsize / 2;
    } while(lsize);

    if(tid == 0)
        *sum = tmp_sum[0];
}