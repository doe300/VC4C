// See https://github.com/doe300/VC4C/issues/50
kernel void if1 (global float a[]) {
  int id = get_global_id(0);
  if ((id & 0x01) == 0)
    a[id] = 3.0;
}

void kernel test(float global * a, const int n){
  float sum = 0.0;
  for (int i = 0; i < n; i++)
    sum += a[i];
  a[get_global_id(0)] = sum;
}
