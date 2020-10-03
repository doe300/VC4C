__kernel void test_shuffle(const __global char16* in, __global char16* out)
{
    char16 tmp = in[0];
    char16 tmp1 = in[1];

    //(7, 6, 4, 8, 1, c, d, 1, 0, 9, e, f, 4, 3, 8, 6)
    out[0] = shuffle(tmp, (uchar16)(7, 6, 4, 8, 1, 12, 13, 1, 0, 9, 14, 15, 4, 3, 8, 6));
    //(1, 7, b, 12, 15, f, 8, 9, 0, 13, 2, 1, 11, d, 7, 8)
    out[1] = shuffle2(tmp, tmp1, (uchar16)(1, 7, 11, 18, 21, 15, 8, 9, 0, 19, 2, 1, 17, 13, 7, 8));

    char16 tmp2 = in[0];
    //(1a, 1b, 2, 10, 4, 19, 6, 17, 8, 9, 1c, 13, 1a, d, e, f)
    tmp2.s5170abc3 = tmp1.s9b7ac3a0;
    out[2] = tmp2;

    char4 tmp3 = in[0].xyzw;
    //(11, 1, 2, 10)
    tmp3.wx = tmp1.xy;
    //(11, 1, 2, 10, 11, 1, 2, 10, 11, 1, 2, 10, 11, 1, 2, 10)
    out[3] = (char16)(tmp3, tmp3, tmp3, tmp3);

    char16 tmp4 = 0;
    //(0, 0, 0, 1, 11, 0, 0, 10, 0, 11, 1, 2, 2, 10, 0, 0)
    tmp4.s43b79acd = (char8)(tmp3, tmp3);
    out[4] = tmp4;

    // (0, 0, 0, 0, 4, 4, 4, 4, 8, 8, 8, 8, c, c, c, c)
    out[5] = shuffle(tmp, (uchar16)(0, 0, 0, 0, 4, 4, 4, 4, 8, 8, 8, 8, 12, 12, 12, 12));
    // (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    out[6] = shuffle2(tmp, tmp1, (uchar16)(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0));

    // (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f)
    out[7] = shuffle(tmp, convert_uchar16(tmp));
    // (a, b, c, d, e, f, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9) - simple rotate all at once!
    out[8] = shuffle(tmp, (uchar16)(10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9));
    // (f, e, d, c, b, a, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
    out[9] = shuffle(tmp, (uchar16)(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
}

// Test a compiler bug writing arbitrary values for upper elements when upcasting a vector
__kernel void test_shuffle_upcast(const __global char8* in, const __global uchar8* mask, __global char8* out)
{
    char8 result = (char8) 0x17;
    // shuffle([0, 1, 2, 3, 4, 5, 6, 7], [4, 5, 6, 7, 0, 1, 2, 3]).s6 = [2]
    result.s4 = shuffle(in[0], mask[0]).s6;
    // [x, x, x, x, 2, x, x, x]
    out[0] = result;

    result = (char8) 0x42;
    // shuffle([0, 1, 2, 3, 4, 5, 6, 7], [4, 5, 6, 7, 0, 1, 2, 3]).s05 = [4, 1]
    result.s37 = shuffle(in[1], mask[1]).s05;
    // [x, x, x, 4, x, x, x, 1]
    out[1] = result;

    result = (char8) 0x13;
    // shuffle([0, 1, 2, 3, 4, 5, 6, 7], [4, 5, 6, 7, 0, 1, 2, 3]).s376 = [7, 3, 2]
    result.s052 = shuffle(in[2], mask[2]).s376;
    // [7, x, 2, x, x, 3, x, x]
    out[2] = result;

    result = (char8) 0xFF;
    // shuffle([0, 1, 2, 3, 4, 5, 6, 7], [4, 5, 6, 7, 0, 1, 2, 3]).2157 = [6, 5, 1, 3]
    result.s3147 = shuffle(in[3], mask[3]).s2157;
    // [x, 5, x, 6, 1, x, x, 3]
    out[3] = result;

    result = (char8) 0x71;
    // shuffle([0, 1, 2, 3, 4, 5, 6, 7], [4, 5, 6, 7, 0, 1, 2, 3]).2157 = [6, 5, 1, 3]
    result.s0123 = shuffle(in[4], mask[4]).s2157;
    // [6, 5, 1, 3, x, x, x, x]
    out[4] = result;

    result = (char8) 0x31;
    // shuffle([0, 1, 2, 3, 4, 5, 6, 7], [4, 5, 6, 7, 0, 1, 2, 3]).2157 = [3]
    result.s0 = shuffle(in[5], mask[5]).s7;
    // [3, x, x, x, x, x, x, x]
    out[5] = result;
}

// This is directly taken from OpenCL-CTS/relationals/shuffle_copy for "char1 to char3"
__kernel void sample_test(__global char* source, __global char* dest)
{
    if(get_global_id(0) != 0)
        return;
    // char src1 /*, src2*/;
    char3 tmp;
    tmp = (char) ((char) 0);
    tmp.s2 = source[0];
    vstore3(tmp, 0, dest);
    tmp = (char) ((char) 0);
    tmp.s0 = source[1];
    vstore3(tmp, 1, dest);
    tmp = (char) ((char) 0);
    tmp.s0 = source[2];
    vstore3(tmp, 2, dest);
    tmp = (char) ((char) 0);
    tmp.S2 = source[3];
    vstore3(tmp, 3, dest);
    tmp = (char) ((char) 0);
    tmp.S2 = source[4];
    vstore3(tmp, 4, dest);
    tmp = (char) ((char) 0);
    tmp.S1 = source[5];
    vstore3(tmp, 5, dest);
    tmp = (char) ((char) 0);
    tmp.s2 = source[6];
    vstore3(tmp, 6, dest);
    tmp = (char) ((char) 0);
    tmp.S2 = source[7];
    vstore3(tmp, 7, dest);
    tmp = (char) ((char) 0);
    tmp.S2 = source[8];
    vstore3(tmp, 8, dest);
    tmp = (char) ((char) 0);
    tmp.S1 = source[9];
    vstore3(tmp, 9, dest);
    tmp = (char) ((char) 0);
    tmp.s2 = source[10];
    vstore3(tmp, 10, dest);
    tmp = (char) ((char) 0);
    tmp.s1 = source[11];
    vstore3(tmp, 11, dest);
    tmp = (char) ((char) 0);
    tmp.s0 = source[12];
    vstore3(tmp, 12, dest);
    tmp = (char) ((char) 0);
    tmp.S1 = source[13];
    vstore3(tmp, 13, dest);
    tmp = (char) ((char) 0);
    tmp.S1 = source[14];
    vstore3(tmp, 14, dest);
    tmp = (char) ((char) 0);
    tmp.S1 = source[15];
    vstore3(tmp, 15, dest);
    tmp = (char) ((char) 0);
    tmp.S1 = source[16];
    vstore3(tmp, 16, dest);
    tmp = (char) ((char) 0);
    tmp.S0 = source[17];
    vstore3(tmp, 17, dest);
    tmp = (char) ((char) 0);
    tmp.S2 = source[18];
    vstore3(tmp, 18, dest);
    tmp = (char) ((char) 0);
    tmp.S2 = source[19];
    vstore3(tmp, 19, dest);
    tmp = (char) ((char) 0);
    tmp.s1 = source[20];
    vstore3(tmp, 20, dest);
    tmp = (char) ((char) 0);
    tmp.s0 = source[21];
    vstore3(tmp, 21, dest);
    tmp = (char) ((char) 0);
    tmp.S0 = source[22];
    vstore3(tmp, 22, dest);
    tmp = (char) ((char) 0);
    tmp.S2 = source[23];
    vstore3(tmp, 23, dest);
    tmp = (char) ((char) 0);
    tmp.S0 = source[24];
    vstore3(tmp, 24, dest);
    tmp = (char) ((char) 0);
    tmp.s1 = source[25];
    vstore3(tmp, 25, dest);
    tmp = (char) ((char) 0);
    tmp.S0 = source[26];
    vstore3(tmp, 26, dest);
    tmp = (char) ((char) 0);
    tmp.s1 = source[27];
    vstore3(tmp, 27, dest);
    tmp = (char) ((char) 0);
    tmp.s2 = source[28];
    vstore3(tmp, 28, dest);
    tmp = (char) ((char) 0);
    tmp.s1 = source[29];
    vstore3(tmp, 29, dest);
    tmp = (char) ((char) 0);
    tmp.S2 = source[30];
    vstore3(tmp, 30, dest);
    tmp = (char) ((char) 0);
    tmp.S0 = source[31];
    vstore3(tmp, 31, dest);
}
