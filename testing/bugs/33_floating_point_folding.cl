//Sources by @long-long-float (from https://github.com/doe300/VC4C/pull/33)

__kernel void add_redundancy(__global float * a) {
  float b = *a + 0.0;
  *a = b;
}

__kernel void mul_redundancy(__global float * a) {
  float b = *a * 0.0;
  *a = b;
}