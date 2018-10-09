__kernel void test_kernel(int N, __global float *input, __global float *output, __global float *expected)
{
  int index = (get_group_id(0) + get_group_id(1)*get_num_groups(0)) * get_local_size(0) + get_local_id(0);

  if (index >= N) return;

  output[index] = sqrt(input[index]);

  index += 1;
  input[index] = output[index-1];
  output[index] = log(input[index]);

  index += 1;
  input[index] = output[index-1];
  output[index] = pow(input[index], output[index-2]);

  index += 1;
  input[index] = output[index-1];
  output[index] = -exp(input[index]);

  index += 1;
  input[index] = output[index-1];
  output[index] = fabs(input[index]);

  index += 1;
  input[index] = output[index-1];
  output[index] = sin(input[index]);

  index += 1;
  input[index] = output[index-1];
  output[index] = cos(input[index]);

}


