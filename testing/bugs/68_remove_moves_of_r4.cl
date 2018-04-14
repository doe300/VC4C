//for discussion,see here: https://github.com/doe300/VC4C/issues/68
void kernel test(float global * a, const int n){
  for (int i = 0; i < 10; i++)
    a[i] += 1;
}
