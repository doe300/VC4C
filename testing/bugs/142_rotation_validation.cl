// code by @long-long-float, see https://github.com/doe300/VC4C/issues/142
#define I(x, y) ((y) * width / 4 + (x))
__kernel void stencil(int width, int height, __global uchar *in, __global uchar *out)
{
    for (int x = 0; x < width / 4; x++) {
        for (int y = 1; y < height - 1; y++) {
            uchar4 right = (uchar4)(0);
            uchar16 center = vload16(I(x, y), in);

            uchar16 r = (uchar16)(center.s456789AB, center.sCDEF, right);

            vstore16(r, I(x, y), out);

        }
    }
}