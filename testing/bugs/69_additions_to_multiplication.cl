//for discussion, see https://github.com/doe300/VC4C/issues/69
void kernel test(float global * a, const int n){
  float sum = 0.0;
  float c = a[0];
  for (int i = 0; i < 10; i++)
    sum += 10.0 + c;
  a[get_global_id(0)] = sum;
}
