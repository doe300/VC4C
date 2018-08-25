//taken from here: https://github.com/alexzk1/LitterBug-Algorithm/blob/Desktop_Litterbug/cl/kernels.cpp
uchar16 myselectuc16(uchar16 afalse, uchar16 atrue, int16 condition)
{
    //we have -1 = true in condition ...it should be so
    uchar16 one = 1;
    uchar16 cond     = convert_uchar16(condition * condition);
    uchar16 not_cond = one - cond;
    return atrue * cond + afalse * not_cond;
}
int16 myselecti16(int16 afalse, int16 atrue, int16 condition)
{
    int16 cond     = condition * condition;
    int16 not_cond = 1 - cond;
    return atrue * cond + afalse * not_cond;
}

__kernel void cartToAngle(const int total, float gpu16, float gpu1, __global const float* restrict gradx, __global const float* restrict grady, __global float* restrict radians)
{
    private const size_t i        = get_global_id(0);
    private const size_t gpu_used = get_global_size(0);
    private const int elements_count = (int)(total * gpu16); //total / (gpu_used * 16);
    private const int offset = (int)(i * total * gpu1);      //i * total / gpu_used;
    const float16 pi2 = 2 * 3.14159265f;
    for (size_t k = 0; k < elements_count; ++k)
    {
       private float16 x = vload16( k , gradx + offset);
       private float16 y = vload16( k , grady + offset);
       float16 a  = atan2(y, x);
       a = select(a, a + pi2, a < 0);
       vstore16(a, k, radians + offset);
    }
}
//https://software.intel.com/en-us/videos/optimizing-simple-opencl-kernels-sobel-kernel-optimization

#define INP_MEM __global const
//#define NORM /255.f
//#define DENORM *255.f
#define NORM
#define DENORM
//z1 z2 z3
//z4 z5 z6
//z7 z8 z9
#define Z1 ((float16)(a, b.s0123, b.s456789ab, b.scde))
#define Z2 (b)
#define Z3 ((float16) (b.s123, b.s4567, b.s89abcdef, c))
#define Z4 ((float16)(d, e.s0123, e.s456789ab, e.scde))
#define Z5 (e)
#define Z6 ((float16)(e.s123, e.s4567, e.s89abcdef, f))
#define Z7 ((float16)(g, h.s0123, h.s456789ab, h.scde))
#define Z8 (h)
#define Z9 ((float16)(h.s123, h.s4567, h.s89abcdef, i))
#define INIT_PADDED uint dstXStride = get_global_size(0); uint dstIndex = 16 * get_global_id(1) * dstXStride + get_global_id(0);uint srcXStride = dstXStride + 2;uint srcIndex = 16 * get_global_id(1) * srcXStride + get_global_id(0) + 1
#define NEXT_ROW a = d; b = e; c = f; d = g; e = h; f = i
#define INPUT (( INP_MEM uchar*)(input + srcIndex))
#define INPUT (( INP_MEM uchar*)(input + srcIndex))
__kernel void SobelAndMagicDetectorNoCanny(const int is_minus1, const int is_plus2, const int is_first_run, const float alpha_s, const float fore_th,
                                    INP_MEM uchar16* restrict input, __global float16* restrict grad_dir,
                                    __global float16* restrict BSx,  __global float16* restrict BSy, __global uchar16* restrict mapRes
                                   )
{
    INIT_PADDED;
    const float16 as  = alpha_s;
    const float16 fth = fore_th NORM;
    const float16 v19 = 19 NORM;
    float   a = *(INPUT - 1)NORM;
    float16 b = convert_float16(vload16(0, INPUT))NORM;
    float   c = INPUT[16]NORM;
    srcIndex += srcXStride;
    float   d = *(INPUT - 1)NORM;
    float16 e = convert_float16(vload16(0, INPUT))NORM;
    float   f = INPUT[16]NORM;
    int16   fr = is_first_run;
    for (int k = 0; k < 16; ++k)
    {
        uint dstAlignedIndex = srcIndex; //ok, I hope that is correct, i.e. middle of the matrix is index
        srcIndex += srcXStride;
        float   g = *(INPUT - 1)NORM;
        float16 h = convert_float16(vload16(0, INPUT))NORM;
        float   i = INPUT[16]NORM;
        float16 Gx = (Z7 + 2 * Z8 + Z9) - (Z1 + 2 * Z2 + Z3);
        float16 Gy = (Z3 + 2 * Z6 + Z9) - (Z1 + 2 * Z4 + Z7);
        float16 an  = atan2(Gy, Gx);
        an = select(an, an + 6.2831853f, isless(an, 0));
        vstore16(an, 0, ( __global float*)(grad_dir + dstIndex));
        float16 bx = select(vload16(0, ( __global float*)(BSx + dstIndex)), Gx, fr);
        float16 by = select(vload16(0, ( __global float*)(BSy + dstIndex)), Gy, fr);
        float16 D_Sx = Gx - bx;
        bx += D_Sx * as;
        vstore16(bx, 0, ( __global float*)(BSx + dstIndex));
        float16 D_Sy = Gy - by;
        by += D_Sy * as;
        vstore16(by, 0, ( __global float*)(BSy + dstIndex));
        if (is_minus1 || is_plus2)
        {
             const int16 zeros = 0;
             const int16 ones  = 1;
             const int16 twos  = 2;
             const int16 twos5 = 255;
             int16 mr           = convert_int16(vload16(0, ( __global uchar*)(mapRes + dstIndex)));
             mr -= is_minus1;
             int16 c1 = isgreater(fabs(D_Sx), fth ) && isgreater(fabs(Gx), v19 );
             int16 c2 = isgreater(fabs(D_Sy), fth ) && isgreater(fabs(Gy), v19 );
             mr += select(zeros, ones, c2 || c1) * twos;
             c1 = mr < zeros;
             c2 = select(mr, zeros, c1);//overflow protection
             c1 = c2 > twos5;
             mr = is_plus2 * select(c2, twos5, c1); //overflow protection
             vstore16(convert_uchar16(mr), 0, ( __global uchar*)(mapRes + dstIndex));
        }
        NEXT_ROW;
        dstIndex += dstXStride;
    }
}

__kernel void SobelAndMagicDetector(const int is_minus1, const int is_plus2, const int is_first_run, const float alpha_s, const float fore_th,
                                    INP_MEM uchar16* restrict input, __global float16* restrict grad_dir,
                                    __global float16* restrict BSx,  __global float16* restrict BSy, __global uchar16* restrict mapRes
                                    , __global float16* restrict alignedGMod
                                   )
{
    INIT_PADDED;
    const float16 as  = alpha_s;
    const float16 fth = fore_th NORM;
    const float16 v19 = 19 NORM;
    float   a = *(INPUT - 1)NORM;
    float16 b = convert_float16(vload16(0, INPUT))NORM;
    float   c = INPUT[16]NORM;
    srcIndex += srcXStride;
    float   d = *(INPUT - 1)NORM;
    float16 e = convert_float16(vload16(0, INPUT))NORM;
    float   f = INPUT[16]NORM;
    int16   fr = is_first_run;
    for (int k = 0; k < 16; ++k)
    {
        uint dstAlignedIndex = srcIndex; //ok, I hope that is correct, i.e. middle of the matrix is index
        srcIndex += srcXStride;
        float   g = *(INPUT - 1)NORM;
        float16 h = convert_float16(vload16(0, INPUT))NORM;
        float   i = INPUT[16]NORM;
        float16 Gx = (Z7 + 2 * Z8 + Z9) - (Z1 + 2 * Z2 + Z3);
        float16 Gy = (Z3 + 2 * Z6 + Z9) - (Z1 + 2 * Z4 + Z7);
        float16 an  = atan2(Gy, Gx);
        //prepairing stuff for Canny - result will be gradient with [0; pi) - we dont care if it poinst left or right there
             float16 mag = fabs(Gx) + fabs(Gy); //default opencv Canny behaviour
             vstore16(mag, 0, ( __global float*)(alignedGMod + dstAlignedIndex));
        //done here, must know full matrix prior can do non-maximum supression etc.
        an = select(an, an + 6.2831853f, isless(an, 0));
        vstore16(an, 0, ( __global float*)(grad_dir + dstIndex));
        float16 bx = select(vload16(0, ( __global float*)(BSx + dstIndex)), Gx, fr);
        float16 by = select(vload16(0, ( __global float*)(BSy + dstIndex)), Gy, fr);
        float16 D_Sx = Gx - bx;
        bx += D_Sx * as;
        vstore16(bx, 0, ( __global float*)(BSx + dstIndex));
        float16 D_Sy = Gy - by;
        by += D_Sy * as;
        vstore16(by, 0, ( __global float*)(BSy + dstIndex));
        if (is_minus1 || is_plus2)
        {
             const int16 zeros = 0;
             const int16 ones  = 1;
             const int16 twos  = 2;
             const int16 twos5 = 255;
             int16 mr           = convert_int16(vload16(0, ( __global uchar*)(mapRes + dstIndex)));
             mr -= is_minus1;
             int16 c1 = isgreater(fabs(D_Sx), fth ) && isgreater(fabs(Gx), v19 );
             int16 c2 = isgreater(fabs(D_Sy), fth ) && isgreater(fabs(Gy), v19 );
             mr += select(zeros, ones, c2 || c1) * twos;
             c1 = mr < zeros;
             c2 = select(mr, zeros, c1);//overflow protection
             c1 = c2 > twos5;
             mr = is_plus2 * select(c2, twos5, c1); //overflow protection
             vstore16(convert_uchar16(mr), 0, ( __global uchar*)(mapRes + dstIndex));
        }
        NEXT_ROW;
        dstIndex += dstXStride;
    }
}
#undef INPUT
//that is not full Canny, it uses pre-processed values from prior SobelAndMagicDetector
//expecting angle is specially prepared in [0;pi) so we lost left or right, top or bottom, but we don't care here
#define INPUT (( INP_MEM float*)(alignedGMod + srcIndex))
#define pi8  0.39269908125f
#define pi4  0.7853981625f
#define pi34 3.f * 0.7853981625f
#define pi2  1.570796325f
#define pi1  3.14159265f
__kernel void non_maximum(INP_MEM float16* restrict angles, INP_MEM float16* restrict alignedGMod, __global float16* restrict N)
{
      INIT_PADDED;
      float   a = *(INPUT - 1);
      float16 b = vload16(0, INPUT);
      float   c = INPUT[16];
      srcIndex += srcXStride;
      float   d = *(INPUT - 1);
      float16 e = vload16(0, INPUT);
      float   f = INPUT[16];
      for (int k = 0; k < 16; ++k)
      {
          uint dstPaddedIndex = srcIndex;
          float16 angle = vload16(0, ( INP_MEM float*)(angles + dstIndex));
          angle = select(angle, angle - pi1, isgreater(angle, pi1));
          srcIndex += srcXStride;
          float   g = *(INPUT - 1);
          float16 h = vload16(0, INPUT);
          float   i = INPUT[16];
//z1 z2 z3
//z4 z5 z6
//z7 z8 z9
          //now we have loaded all 9 points (3x3) and x16 of them
          //need to figure gradient line and pick 2 of 8 around current
          int16 atest = islessequal(fabs(angle - pi2), pi8); //90 not sure why, but this works better 90 = up/left
          float16 p1 = select(0, Z4, atest);
          float16 p2 = select(0, Z6, atest);
          atest =  isless(fabs(angle - pi4), pi8); //45
          p1 = select(p1, Z3, atest);
          p2 = select(p2, Z7, atest);
          atest =  islessequal(angle, pi8) || islessequal(fabs(angle - pi1), pi8); //0
          p1 = select(p1, Z2, atest);
          p2 = select(p2, Z8, atest);
          atest =  isless(fabs(angle - pi34), pi8); //135
          p1 = select(p1, Z1, atest);
          p2 = select(p2, Z9, atest);
          vstore16(select(0, Z5, isless(p2, Z5) && isless(p1, Z5)), 0, ( __global float*)(N + dstPaddedIndex));
          NEXT_ROW;
          dstIndex += dstXStride;
      }
};

#undef INPUT
//that will be 1 pass for speed
#define INPUT   (( INP_MEM float*)(paddedN + srcIndex))
__kernel void hysterisis(INP_MEM float16* restrict paddedN,  __global uchar16* restrict result)
{
     const float16 T1 = 40.f NORM;
     const float16 T2 = 3.f * T1 NORM;
     INIT_PADDED;
     float   a = *(INPUT - 1);
     float16 b = vload16(0, INPUT);
     float   c = INPUT[16];
     srcIndex += srcXStride;
     float   d = *(INPUT - 1);
     float16 e = vload16(0, INPUT);
     float   f = INPUT[16];
     int changes = 0;
//z1 z2 z3
//z4 z5 z6
//z7 z8 z9
     for (int k = 0; k < 16; ++k)
     {
         uint dstPaddedIndex = srcIndex;
         srcIndex += srcXStride;
         float   g = *(INPUT - 1);
         float16 h = vload16(0, INPUT);
         float   i = INPUT[16];
         //If the pixel gradient is between the two thresholds, then it will be accepted only if it is connected to a pixel that is above the upper threshold.
         int16 surrounding_greater =                  isgreater(Z5, T2);//If a pixel gradient is higher than the upper threshold, the pixel is accepted as an edge
         surrounding_greater = surrounding_greater || isgreater(Z1, T2);
         surrounding_greater = surrounding_greater || isgreater(Z2, T2);
         surrounding_greater = surrounding_greater || isgreater(Z3, T2);
         surrounding_greater = surrounding_greater || isgreater(Z4, T2);
         surrounding_greater = surrounding_greater || isgreater(Z6, T2);
         surrounding_greater = surrounding_greater || isgreater(Z7, T2);
         surrounding_greater = surrounding_greater || isgreater(Z8, T2);
         surrounding_greater = surrounding_greater || isgreater(Z9, T2);
         int16 PE = isgreater(Z5, T1);
         float16 nz5 = select(select(0, Z5, PE), T2 + 1,  surrounding_greater && PE);
         uchar16 hys   = myselectuc16(0, 255, isgreater(nz5, T2));
         vstore16(hys, 0, ( __global uchar*)(result + dstIndex));
         NEXT_ROW;
         dstIndex += dstXStride;
     }
};
#undef INPUT
