//code by @nomaddo (see https://github.com/doe300/VC4C/issues/22)
kernel void test1 (global float a[], global float b[]) {
  //this loop should be vectorized
  //XXX runs okay!
  for (int i = 0; i < 1000; i++) {
    a[i] = -i;
    b[i] = -i;
  }
}

/*
//following samples taken (slightly modified) from https://llvm.org/docs/Vectorizers.html
kernel void test2(global float *A, global float* B, float K, int start, int end) {
  //cannot be vectorized, since iteration count is unknown
  //XXX is not vectorized 
  for (int i = start; i < end; ++i)
    A[i] *= B[i] + K;
}

kernel void test3(global float *A, global float* B, float K) {
  //illegal to be vectorized, if A and B point to overlapping memory
  //-> disallow loop vectorization if output is also read?!
  //XXX loop vectorization is skipped!
  for (int i = 0; i < 800; ++i)
    A[i] *= B[i] + K;
}

kernel void test4(global int *A, global int *B) {
  unsigned sum = 0;
  //should be able to vectorize
  //-> need to make sure, i is recognized as iteration-variable, not sum
  //XXX determines iteration variable correctly!
  for (int i = 0; i < 1024; ++i)
    sum += A[i] + 5;
  //FIXME the vectorized sum values are not added together
  *B = sum;
}

kernel void test5(global float *A, float K) {
  //should be able to vectorize
  //XXX is vectorized!
  for (int i = 0; i < 1024; ++i)
    A[i] = i;
}

kernel void test6(global int *A, global int *B) {
  unsigned sum = 0;
  //should be able to vectorize?!
  //XXX cannot find loop iteration variable, is moved several times (for taking/not taking the if-block)
  for (int i = 0; i < 1024; ++i)
    if (A[i] > B[i])
      sum += A[i] + 5;
  *B = sum;
}

kernel void test7(global int *A, global int *B) {
  //should be able to vectorize
  //XXX cannot find the "upper" bound
  for (int i = 1024; i > 0; --i)
    A[i] +=1;
}

kernel void test8(global int * A, global int * B) {
  //may be able to vectorize
  //-> we can read from deviating memory locations, just not store into them
  //XXX declines vectorization for reading and writing same memory address
  for (int i = 0; i < 1024; ++i)
      A[i] += B[i * 4];
}

kernel void test9(global int * A, global int * B) {
  //cannot be vectorized
  //-> we can read from deviating memory locations, but not store into them
  //XXX declines vectorization for reading and writing same memory address
  for (int i = 0; i < 1024; ++i)
      A[i * 4] += B[i];
}

kernel void test10(global int *A, global char *B, int k) {
  //should be able to vectorize
  //XXX declines vectorization for reading and writing same memory address
  for (int i = 0; i < 1024; ++i)
    A[i] += 4 * B[i];
}

struct Foo{ int A[100], K, B[100]; };

kernel void test11(global struct Foo *out) {
  local struct Foo f;
  //may be able to vectorize
  //XXX currently fails for not being able to vectorize reading from VPM
  for (int i = 0; i < 100; ++i)
    f.A[i] = f.B[i] + 100;
  *out = f;
}
*/
