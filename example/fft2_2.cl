/*
 * http://www.bealto.com/gpu-fft_opencl-1.html
 */
//#include <clc/clc.h>
/*
 * /opt/spirv/build/bin/clang -cc1 -triple spir-unknown-unknown -emit-spirv -x cl -Dcl_clang_storage_class_specifiers -isystem /usr/include/clc/ -include clc/clc.h -o fft2_2.sv ./fft2_2.cl 
 */

/* Header to make Clang compatible with OpenCL */
// Complex product, multiply vectors of complex numbers

#define MUL_RE(a,b) (a.even*b.even - a.odd*b.odd)

#define MUL_IM(a,b) (a.even*b.odd + a.odd*b.even)



float2 mul_1(float2 a,float2 b)

{ float2 x; x.even = MUL_RE(a,b); x.odd = MUL_IM(a,b); return x; }



float4 mul_2(float4 a,float4 b)

{ float4 x; x.even = MUL_RE(a,b); x.odd = MUL_IM(a,b); return x; }



// Return the DFT2 of the two complex numbers in vector A

float4 dft2(float4 a) { return (float4)(a.lo+a.hi,a.lo-a.hi); }



// Return cos(alpha)+I*sin(alpha)  (3 variants)

float2 exp_alpha_1(float alpha)

{

  float cs,sn;

  // sn = sincos(alpha,&cs);  // sincos

  cs = native_cos(alpha); sn = native_sin(alpha);  // native sin+cos

  // cs = cos(alpha); sn = sin(alpha); // sin+cos

  return (float2)(cs,sn);

}

// Radix-2 kernel

__kernel void fft_radix2(__global const float2 * x,__global float2 * y,int p)

{

  int i = get_global_id(0); // number of threads

  int t = get_global_size(0); // current thread

  int k = i & (p-1); // index in input sequence, in 0..P-1

  x += i; // input offset

  y += (i<<1) - k; // output offset

  float4 u = dft2( (float4)(

    x[0],

    mul_1(exp_alpha_1(-M_PI*(float)k/(float)p),x[t]) ));

  y[0] = u.lo;

  y[p] = u.hi;

}
