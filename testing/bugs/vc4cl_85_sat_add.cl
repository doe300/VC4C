__kernel void brightness(__global const uchar *A, uchar const B, __global uchar *C) {
int i = get_global_id(0);

C[i] = (A[i]+B) >= 255 ? 255 : (A[i]+B);
//C[i] = A[i];
}
