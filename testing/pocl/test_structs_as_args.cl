typedef struct int_single {
    int a;
} int_single;
typedef struct int_pair {
    long a;
    long b;
} int_pair;
typedef struct test_struct {
    int elementA;
    int elementB;
    long elementC;
    char elementD;
    long elementE;
    float elementF;
    short elementG;
#ifdef cl_khr_fp64
    double elementH;
#else
    long elementH;
#endif
} test_struct;

kernel void test_single(int_single input, global int* output) {
 output[0] = input.a;
}
kernel void test_pair(int_pair input, global int* output) {
 output[0] = (int)input.a;
 output[1] = (int)input.b;
}
kernel void test_kernel(test_struct input, global int* output) {
 output[0] = input.elementA;
 output[1] = input.elementB;
 output[2] = (int)input.elementC;
 output[3] = (int)input.elementD;
 output[4] = (int)input.elementE;
 output[5] = (int)input.elementF;
 output[6] = (int)input.elementG;
 output[7] = (int)input.elementH;
 output[8] = sizeof(test_struct);
 output[9] = _Alignof(test_struct);
}
