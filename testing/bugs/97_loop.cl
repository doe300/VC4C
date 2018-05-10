// source: https://github.com/doe300/VC4C/issues/97
__kernel void loop (__global float * a){
    for (int i = 0; i < 100; i++) {
        a[i] = i * 2;
    }
}
