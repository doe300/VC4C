/* openclkernels.h

   Copyright (c) 2003-2017 HandBrake Team
   This file is part of the HandBrake source code
   Homepage: <http://handbrake.fr/>.
   It may be used under the terms of the GNU General Public License v2.
   For full terms see the file COPYING file or visit http://www.gnu.org/licenses/gpl-2.0.html
   
   Authors: Peng Gao <peng@multicorewareinc.com> <http://www.multicorewareinc.com/>
            Li   Cao <li@multicorewareinc.com> <http://www.multicorewareinc.com/>

 */
 
    typedef unsigned char fixed8;

kernel void vscale_all_dither_opencl (
        global unsigned char *dst,
        const global short *src,
        const global short *yfilter,
        int yfilterSize,
        const global short *cfilter,
        int cfilterSize,
        const global int *yfilterPos,
        const global int *cfilterPos,
        int dstWidth,
        int dstHeight,
        int srcWidth,
        int srcHeight,
        int dstStride,
        int dstChrStride,
        int srcStride,
        int srcChrStride)
    {
        const unsigned char hb_dither_8x8_128[8][8] = {
            {  36, 68,  60, 92,  34, 66,  58, 90, },
            { 100,  4, 124, 28,  98,  2, 122, 26, },
            {  52, 84,  44, 76,  50, 82,  42, 74, },
            { 116, 20, 108, 12, 114, 18, 106, 10, },
            {  32, 64,  56, 88,  38, 70,  62, 94, },
            {  96,  0, 120, 24, 102,  6, 126, 30, },
            {  48, 80,  40, 72,  54, 86,  46, 78, },
            { 112, 16, 104,  8, 118, 22, 110, 14, },
        };


        int w = get_global_id(0);
        int h = get_global_id(1);

        int chrWidth = get_global_size(0);
        int chrHeight = get_global_size(1);
        const unsigned char *local_up_dither;
        const unsigned char *local_down_dither;

        local_up_dither = hb_dither_8x8_128[h & 7];
        local_down_dither = hb_dither_8x8_128[(h + chrHeight) & 7];

        //yscale;
        int srcPos1 = (yfilterPos[h]) * srcStride + w;
        int srcPos2 = (yfilterPos[h]) * srcStride + w + (chrWidth);
        int srcPos3 = (yfilterPos[h + chrHeight]) * srcStride + w;
        int srcPos4 = (yfilterPos[h + chrHeight]) * srcStride + w + chrWidth;
        int src1Pos = dstStride * srcHeight + (cfilterPos[h]) * dstChrStride + (w);
        int src2Pos = dstStride * srcHeight + (dstChrStride*(srcHeight>>1)) + (cfilterPos[h]) * dstChrStride + w;

        int val1 = (local_up_dither[w & 7] << 12); //y offset is 0;
        int val2 = (local_up_dither[(w + chrWidth) & 7] << 12);
        int val3 = (local_down_dither[w &7] << 12);
        int val4 = (local_down_dither[(w + chrWidth) & 7] << 12);
        int val5 = (local_up_dither[w & 7] << 12);
        int val6 = (local_up_dither[(w + 3) & 7] << 12);   // 3 is offset of the chrome channel.

        int j;
        int filterPos1 = h * yfilterSize;
        int filterPos2 = ( h + chrHeight ) * yfilterSize;
        for(j = 0; j < yfilterSize; j++)
        {
            val1 += src[srcPos1] * yfilter[filterPos1 + j];
            srcPos1 += srcStride;
            val2 += src[srcPos2] * yfilter[filterPos1 + j];
            srcPos2 += srcStride;
            val3 += src[srcPos3] * yfilter[filterPos2 + j];
            srcPos3 += srcStride;
            val4 += src[srcPos4] * yfilter[filterPos2 + j];
            srcPos4 += srcStride;
            val5 += src[src1Pos] * cfilter[filterPos1 + j];
            val6 += src[src2Pos] * cfilter[filterPos1 + j];
            src1Pos += dstChrStride;
            src2Pos += dstChrStride;
        }
        dst[h * dstStride + w] = (((val1 >> 19)&(~0xFF)) ? ((-(val1 >> 19)) >> 31) : (val1 >> 19));
        dst[h * dstStride + w + chrWidth] = (((val2 >> 19)&(~0xFF)) ? ((-(val2 >> 19)) >> 31) : (val2 >> 19));
        dst[(h + chrHeight) * dstStride + w] = (((val3 >> 19)&(~0xFF)) ? ((-(val3 >> 19)) >> 31) : (val3 >> 19));
        dst[(h + chrHeight) * dstStride + w + chrWidth] = (((val4 >> 19)&(~0xFF)) ? ((-(val4 >> 19)) >> 31) : (val4 >> 19));

        int dst1Pos = dstStride * dstHeight + h*(dstChrStride)+(w);
        int dst2Pos = (dstChrStride * chrHeight) + dst1Pos;
        dst[dst1Pos] = (((val5 >> 19)&(~0xFF)) ? ((-(val5 >> 19)) >> 31) : (val5 >> 19));
        dst[dst2Pos] = (((val6 >> 19)&(~0xFF)) ? ((-(val6 >> 19)) >> 31) : (val6 >> 19));
    }
