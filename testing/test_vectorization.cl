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
  //Expected: cannot be vectorized for now
  //Actual: declines vectorization for reading and writing same memory address
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
  //Actual: loop is recognized with correct iteration variable, vectorized version calculates correctly
  for (int i = 0; i < 1024; ++i)
    sum += A[i] + 5;
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
  //Actual: loop is recognized and vectorized (factor 16)
  for (int i = 0; i < 1024; ++i)
    if (A[i] > B[i])
      sum += A[i] + 5;
  *B = sum;
}

kernel void test7(global int *A) {
  //Expected: should be able to vectorize
  //Actual: declines vectorization for reading and writing same memory address
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
  //Actual: declines vectorization for reading and writing same memory address
  for (int i = 0; i < 100; ++i)
    f.A[i] = f.B[i] + 100;
  *out = f;
}

//following samples adapted from https://llvm.org/docs/Vectorizers.html
kernel void test12(global float *A, global float* B, float K, int start, int end) {
  //Expected: should be able to vectorize
  //Actual: loop is recognized and vectorized with dynamic active element count, calculates correctly
  for (int i = start; i < end; ++i)
    A[i] = B[i] + K;
}

kernel void test13(global int *A, global int *B, int count) {
  unsigned sum = 0;
  //Expected: should be able to vectorize
  //Attention: need to make sure, i is recognized as iteration-variable, not sum
  //Actual: loop is recognized and vectorized with dynamic active element count, calculates correctly
  for (int i = 0; i < count; ++i)
    sum += A[i] + 5;
  *B = sum;
}

kernel void test14(global int *A, global int *B, int count) {
  unsigned sum = 0;
  //Expected: should be able to vectorize
  //Actual: loop is recognized and vectorized with dynamic active element count, calculates correctly
  for (int i = 0; i < count; ++i)
    if (A[i] > B[i])
      sum += A[i] + 5;
  *B = sum;
}

kernel void test15(global int *A, global int *B) {
  unsigned sum = 17;
  //Expected: should be able to vectorize
  //Attention: need to make sure the initial sum value is vectorized correctly
  //Actual: loop is recognized with correct iteration variable, not vectorized due to non-identity initial accumulation value
  for (int i = 0; i < 1024; ++i)
    sum += A[i] + 5;
  *B = sum;
}


kernel void test16(global int *A, global float *B) {
  int sum = 0;
  //Expected: should be able to vectorize
  //Attention: need to make sure, operation on accumulated value is handled correctly
  //Actual: loop is recognized with correct iteration variable, vectorized version calculates correctly
  for (int i = 0; i < 1024; ++i)
    sum += A[i] + 5;
  *B = (float)sum;
}

kernel void test17(global unsigned *A, global unsigned *B) {
  unsigned sum = 0;
  //Expected: for now cannot be vectorized
  //Attention: need to make sure the read depending on the input is still correct
  //Actual: skipped due to divergent control flow in loop
  for (int i = 0; i < 1024; ++i) {
    unsigned tmp = 5;
    if (i % 2)
      tmp += A[i];
    sum += tmp;
  }
  *B = sum;
}

kernel void test18(global unsigned *A, global unsigned *B) {
  unsigned sum = 0;
  //Expected: should be able to be vectorized
  //Actual: skipped due to unhandled sum variable
  for (int i = 0; i < 1024; ++i) {
    unsigned tmp = 5;
    if (get_local_id(0) % 2)
      tmp = A[i];
    sum += tmp;
  }
  B[get_global_id(0)] = sum;
}

kernel void test19(global int *A, global int *B) {
  int sum = 0;
  //Expected: for now cannot be vectorized
  //Attention: need to make sure the switch-case blocks are still entered/skipped for the correct iterations
  //Actual: skipped due to containing code-address calculations (for switch-cases)
  for (int i = 0; i < 1024; ++i) {
    int tmp = 0;
    switch(i % 5) {
    case 0:
      tmp = 5;
      break;
    case 1:
      tmp = A[i+1];
      break;
    case 2:
      tmp = A[i] + 5;
      break;
    case 3:
      tmp = A[i];
      // fall-through
    case 4:
      tmp += 17;
      break;
    }
    sum += tmp;
  }
  *B = sum;
}
