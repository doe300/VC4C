__kernel void taskParallelAdd(__global float* A, __global float* B, __global float* C) {
    int base = 0;

    C[base+0] = A[base+0] + B[base+0];
    C[base+4] = A[base+4] + B[base+4];
    C[base+8] = A[base+8] + B[base+8];
    C[base+12] = A[base+12] + B[base+12];
}

__kernel void taskParallelSub(__global float* A, __global float* B, __global float* C) {
    int base = 1;

    C[base+0] = A[base+0] - B[base+0];
    C[base+4] = A[base+4] - B[base+4];
    C[base+8] = A[base+8] - B[base+8];
    C[base+12] = A[base+12] - B[base+12];
}

__kernel void taskParallelMul(__global float* A, __global float* B, __global float* C) {
    int base = 2;

    C[base+0] = A[base+0] * B[base+0];
    C[base+4] = A[base+4] * B[base+4];
    C[base+8] = A[base+8] * B[base+8];
    C[base+12] = A[base+12] * B[base+12];
}

__kernel void taskParallelDiv(__global float* A, __global float* B, __global float* C) {
    int base = 3;

    C[base+0] = A[base+0] / B[base+0];
    C[base+4] = A[base+4] / B[base+4];
    C[base+8] = A[base+8] / B[base+8];
    C[base+12] = A[base+12] / B[base+12];
}