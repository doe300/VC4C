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

/*******************************************************************************************************
dst:           Horizontal scale destination;
src:           YUV content in opencl buf;
yfilter:       Opencl memory of horizontal filter coefficients for luma/alpha planes;
yfilterPos:    Opencl memory of horizontal filter starting positions for each dst[i] for luma/alpha planes;
yfilterSize:   Horizontal filter size for luma/alpha pixels;
cfilter:       Opencl memory of horizontal filter coefficients for chroma planes;
cfilterPos:    Opencl memory of horizontal filter starting positions for each dst[i] for chroma planes;
cfilterSize:   Horizontal filter size for chroma pixels;
dstStride:     Width of destination luma/alpha planes;
dstChrStride:  Width of destination chroma planes;
********************************************************************************************************/

    kernel void hscale_all_opencl (
        global short *dst,
        const global unsigned char *src,
        const global short *yfilter,
        const global int *yfilterPos,
        int yfilterSize,
        const global short *cfilter,
        const global int *cfilterPos,
        int cfilterSize,
        int dstWidth,
        int dstHeight,
        int srcWidth,
        int srcHeight,
        int dstStride,
        int dstChrStride,
        int srcStride,
        int srcChrStride)
    {
        int w = get_global_id(0);
        int h = get_global_id(1);

        int chrWidth = get_global_size(0);
        int chrHeight = get_global_size(1);

        int srcPos1 = h * srcStride + yfilterPos[w];
        int srcPos2 = h * srcStride + yfilterPos[w + chrWidth];
        int srcPos3 = (h + (srcHeight >> 1)) * srcStride + yfilterPos[w];
        int srcPos4 = (h + (srcHeight >> 1)) * srcStride + yfilterPos[w + chrWidth];
        int srcc1Pos = srcStride * srcHeight + (h) * (srcChrStride) + cfilterPos[w];
        int srcc2Pos = srcc1Pos + ((srcChrStride)*(chrHeight));

        int val1 = 0;
        int val2 = 0;
        int val3 = 0;
        int val4 = 0;
        int val5 = 0;
        int val6 = 0;

        int filterPos1 = yfilterSize * w;
        int filterPos2 = yfilterSize * (w + chrWidth);
        int cfilterPos1 = cfilterSize * w;

        int j;
        for (j = 0; j < yfilterSize; j++)
        {
            val1 += src[srcPos1 + j] * yfilter[filterPos1+ j];
            val2 += src[srcPos2 + j] * yfilter[filterPos2 + j];
            val3 += src[srcPos3 + j] * yfilter[filterPos1 + j];
            val4 += src[srcPos4 + j] * yfilter[filterPos2 + j];
            val5 += src[srcc1Pos+j] * cfilter[cfilterPos1 + j];
            val6 += src[srcc2Pos+j] * cfilter[cfilterPos1 + j];
        }
        int dstPos1 = h *dstStride;
        int dstPos2 = (h + chrHeight) * dstStride;

        dst[dstPos1 + w] = ((val1 >> 7) > ((1 << 15) - 1) ? ((1 << 15) - 1) : (val1 >> 7));
        dst[dstPos1 + w + chrWidth] = ((val2 >> 7) > ((1 << 15) - 1) ? ((1 << 15) - 1) : (val2 >> 7));
        dst[dstPos2 + w] = ((val3 >> 7) > ((1 << 15) - 1) ? ((1 << 15) - 1) : (val3 >> 7));
        dst[dstPos2 + w + chrWidth] = ((val4 >> 7) > ((1 << 15) - 1) ? ((1 << 15) - 1) : (val4 >> 7));

        int dstPos3 = h * (dstChrStride) + w + dstStride * dstHeight;
        int dstPos4 = h * (dstChrStride) + w + dstStride * dstHeight + ((dstChrStride) * chrHeight);
        dst[dstPos3] = ((val5 >> 7) > ((1 << 15) - 1) ? ((1 << 15) - 1) : (val5 >> 7));
        dst[dstPos4] = ((val6 >> 7) > ((1 << 15) - 1) ? ((1 << 15) - 1) : (val6 >> 7));
    }
