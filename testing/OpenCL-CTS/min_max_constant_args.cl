__kernel void sample_test(__constant int* src1, __constant int* src2, __constant int* src3, __constant int* src4,
    __constant int* src5, __constant int* src6, __constant int* src7, __constant int* src8, __constant int* src9,
    __constant int* src10, __constant int* src11, __constant int* src12, __constant int* src13, __constant int* src14,
    __constant int* src15, __constant int* src16, __constant int* src17, __constant int* src18, __constant int* src19,
    __constant int* src20, __constant int* src21, __constant int* src22, __constant int* src23, __constant int* src24,
    __constant int* src25, __constant int* src26, __constant int* src27, __constant int* src28, __constant int* src29,
    __constant int* src30, __constant int* src31, __constant int* src32, __constant int* src33, __constant int* src34,
    __constant int* src35, __constant int* src36, __constant int* src37, __constant int* src38, __constant int* src39,
    __constant int* src40, __constant int* src41, __constant int* src42, __constant int* src43, __constant int* src44,
    __constant int* src45, __constant int* src46, __constant int* src47, __constant int* src48, __constant int* src49,
    __constant int* src50, __constant int* src51, __constant int* src52, __constant int* src53, __constant int* src54,
    __constant int* src55, __constant int* src56, __constant int* src57, __constant int* src58, __constant int* src59,
    __constant int* src60, __constant int* src61, __constant int* src62, __constant int* src63,
    __global int* dst)
{
    int tid = get_global_id(0);

    dst[tid] = src1[tid];
    dst[tid] += src2[tid];
    dst[tid] += src3[tid];
    dst[tid] += src4[tid];
    dst[tid] += src5[tid];
    dst[tid] += src6[tid];
    dst[tid] += src7[tid];
    dst[tid] += src8[tid];
    dst[tid] += src9[tid];
    dst[tid] += src10[tid];
    dst[tid] += src11[tid];
    dst[tid] += src12[tid];
    dst[tid] += src13[tid];
    dst[tid] += src14[tid];
    dst[tid] += src15[tid];
    dst[tid] += src16[tid];
    dst[tid] += src17[tid];
    dst[tid] += src18[tid];
    dst[tid] += src19[tid];
    dst[tid] += src20[tid];
    dst[tid] += src21[tid];
    dst[tid] += src22[tid];
    dst[tid] += src23[tid];
    dst[tid] += src24[tid];
    dst[tid] += src25[tid];
    dst[tid] += src26[tid];
    dst[tid] += src27[tid];
    dst[tid] += src28[tid];
    dst[tid] += src29[tid];
    dst[tid] += src30[tid];
    dst[tid] += src31[tid];
    dst[tid] += src32[tid];
    dst[tid] += src33[tid];
    dst[tid] += src34[tid];
    dst[tid] += src35[tid];
    dst[tid] += src36[tid];
    dst[tid] += src37[tid];
    dst[tid] += src38[tid];
    dst[tid] += src39[tid];
    dst[tid] += src40[tid];
    dst[tid] += src41[tid];
    dst[tid] += src42[tid];
    dst[tid] += src43[tid];
    dst[tid] += src44[tid];
    dst[tid] += src45[tid];
    dst[tid] += src46[tid];
    dst[tid] += src47[tid];
    dst[tid] += src48[tid];
    dst[tid] += src49[tid];
    dst[tid] += src50[tid];
    dst[tid] += src51[tid];
    dst[tid] += src52[tid];
    dst[tid] += src53[tid];
    dst[tid] += src54[tid];
    dst[tid] += src55[tid];
    dst[tid] += src56[tid];
    dst[tid] += src57[tid];
    dst[tid] += src58[tid];
    dst[tid] += src59[tid];
    dst[tid] += src60[tid];
    dst[tid] += src61[tid];
    dst[tid] += src62[tid];
    dst[tid] += src63[tid];
}
