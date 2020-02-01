void test_arrays_float(int const *const data, int const ndata)
{
  float s = 0.0f;
  for (int i=0; i<ndata; ++i) {
    s += as_float(data[i]);
  }
  printf("sum=%g\n", s);
  for (;;);
}

kernel void test_kernel(global int *input, global int* output)
{
  int const data[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11};
  int const ndata = sizeof(data) / sizeof(*data);
  test_arrays_float(data, ndata);
}
