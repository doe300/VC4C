//code by @nomaddo (see https://github.com/doe300/VC4C/issues/22)
kernel void test1 (global float a[], global float b[]) {
  //Expected: this loop should be vectorized
  //Actual: loop is vectorized (factor 10) and executes with correct result!
  for (int i = 0; i < 1000; i++) {
    a[i] = -i;
    b[i] = -i;
  }
}

//following samples taken (slightly modified) from https://llvm.org/docs/Vectorizers.html
kernel void test2(global float *A, global float* B, float K, int start, int end) {
  //Expected: cannot be vectorized, since iteration count is unknown
  //Actual: loop is recognized but skipped, failed to find bounds
  for (int i = start; i < end; ++i)
    A[i] *= B[i] + K;
}

kernel void test3(global float *A, global float* B, float K) {
  //illegal to be vectorized, if A and B point to overlapping memory
  //Expected: cannot be vectorized, written to memory is also read
  //Actual: loop is recognized but skipped, output is read
  for (int i = 0; i < 800; ++i)
    A[i] *= B[i] + K;
}

kernel void test4(global int *A, global int *B) {
  unsigned sum = 0;
  //Expected: should be able to vectorize
  //Attention: need to make sure, i is recognized as iteration-variable, not sum
  //Actual: loop is recognized with correct iteration variable, fails to vectorize
  for (int i = 0; i < 1024; ++i)
    sum += A[i] + 5;
  //FIXME the vectorized sum values are not added together
  *B = sum;
}

kernel void test5(global float *A) {
  //Expected: should be able to vectorize
  //Actual: loop is recognized and vectorized (factor 16)
  for (int i = 0; i < 1024; ++i)
    A[i] = i;
}

kernel void test6(global int *A, global int *B) {
  unsigned sum = 0;
  //Expected: should be able to vectorize
  //Actual: cannot find loop iteration variable, is moved several times (for taking/not taking the if-block)
  for (int i = 0; i < 1024; ++i)
    if (A[i] > B[i])
      sum += A[i] + 5;
  *B = sum;
}

kernel void test7(global int *A) {
  //Expected: should be able to vectorize
  //Actual: cannot find the "upper" bound, comparison is more complex than checked for
  for (int i = 1024; i > 0; --i)
    A[i] +=1;
}

kernel void test8(global int * A, global int * B) {
  //Expected: may be able to vectorize
  //-> we can read from deviating memory locations, just not store into them
  //Actual: declines vectorization for reading and writing same memory address
  for (int i = 0; i < 1024; ++i)
      A[i] += B[i * 4];
}

kernel void test9(global int * A, global int * B) {
  //Expected: cannot be vectorized
  //-> we can read from deviating memory locations, but not store into them
  //Actual: declines vectorization for reading and writing same memory address
  for (int i = 0; i < 1024; ++i)
      A[i * 4] += B[i];
}

kernel void test10(global int *A, global char *B) {
  //Expected: should be able to vectorize
  //Actual: declines vectorization for reading and writing same memory address
  for (int i = 0; i < 1024; ++i)
    A[i] += 4 * B[i];
}

struct Foo{ int A[100], K, B[100]; };

kernel void test11(global struct Foo *out) {
  local struct Foo f;
  //Expected: may be able to vectorize
  //Actual: loop is vectorized (factor 10)
  for (int i = 0; i < 100; ++i)
    f.A[i] = f.B[i] + 100;
  *out = f;
}
