//code by @nomaddo (see https://github.com/doe300/VC4C/issues/14)
kernel void add (global float a[], global float b[]) {
  int id = get_global_id(0);
  float8 v1 = vload8(id, a);
  float8 v2 = vload8(id, b);
  v1 += v2;
  vstore8 (v1, id, a);
}

//code by @nomaddo (see https://github.com/doe300/VC4C/issues/17 and https://github.com/doe300/VC4C/issues/28)
kernel void sum_f16 (global float a[], global float b[], global float c[]) {
  int id = get_global_id(0);
  float16 x = vload16(id, b);
  float16 y = vload16(id, c);
  vstore16 (x * y + x * y + x * y, id, a);
}

//code by @nomaddo (see https://github.com/doe300/VC4C/issues/22)
kernel void loop1 (global float a[], global float b[]) {
  for (int i = 0; i < 1000; i++) {
    a[i] = -i;
    b[i] = -i;
  }
}