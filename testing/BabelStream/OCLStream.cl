// Taken from: https://github.com/UoB-HPC/BabelStream/blob/master/OCLStream.cpp
#define startScalar 0.4f
#define TYPE float

constant TYPE scalar = startScalar;
kernel void init(
  global TYPE * restrict a,
  global TYPE * restrict b,
  global TYPE * restrict c,
  TYPE initA, TYPE initB, TYPE initC)
{
  const size_t i = get_global_id(0);
  a[i] = initA;
  b[i] = initB;
  c[i] = initC;
}
kernel void copy(
  global const TYPE * restrict a,
  global TYPE * restrict c)
{
  const size_t i = get_global_id(0);
  c[i] = a[i];
}
kernel void mul(
  global TYPE * restrict b,
  global const TYPE * restrict c)
{
  const size_t i = get_global_id(0);
  b[i] = scalar * c[i];
}
kernel void add(
  global const TYPE * restrict a,
  global const TYPE * restrict b,
  global TYPE * restrict c)
{
  const size_t i = get_global_id(0);
  c[i] = a[i] + b[i];
}
kernel void triad(
  global TYPE * restrict a,
  global const TYPE * restrict b,
  global const TYPE * restrict c)
{
  const size_t i = get_global_id(0);
  a[i] = b[i] + scalar * c[i];
}
kernel void stream_dot(
  global const TYPE * restrict a,
  global const TYPE * restrict b,
  global TYPE * restrict sum,
  local TYPE * restrict wg_sum,
  int array_size)
{
  size_t i = get_global_id(0);
  const size_t local_i = get_local_id(0);
  wg_sum[local_i] = 0.0;
  for (; i < array_size; i += get_global_size(0))
    wg_sum[local_i] += a[i] * b[i];
  for (int offset = get_local_size(0) / 2; offset > 0; offset /= 2)
  {
    barrier(CLK_LOCAL_MEM_FENCE);
    if (local_i < offset)
    {
      wg_sum[local_i] += wg_sum[local_i+offset];
    }
  }
  if (local_i == 0)
    sum[get_group_id(0)] = wg_sum[local_i];
}
