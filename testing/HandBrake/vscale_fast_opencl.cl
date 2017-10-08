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

kernel void vscale_fast_opencl (
        global unsigned char *dst,
        const global short *src,
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
        const unsigned char hb_sws_pb_64[8] = {
            64, 64, 64, 64, 64, 64, 64, 64
        };

        int w = get_global_id(0);
        int h = get_global_id(1);

        int chrWidth = get_global_size(0);
        int chrHeight = get_global_size(1);

        const unsigned char *local_up_dither;
        const unsigned char *local_down_dither;

        local_up_dither = hb_sws_pb_64;
        local_down_dither = hb_sws_pb_64;


        int rightpart = w + chrWidth;
        int bh = h + chrHeight; // bottom part
        short val1 = (src[(yfilterPos[h]) * dstStride + w] + local_up_dither[(w + 0) & 7]) >> 7; //lum offset is 0;
        short val2 = (src[(yfilterPos[h]) * dstStride + rightpart] + local_up_dither[rightpart & 7]) >> 7;
        short val3 = (src[(yfilterPos[bh]) * dstStride + w] + local_down_dither[w & 7]) >> 7;
        short val4 = (src[(yfilterPos[bh]) * dstStride + rightpart] + local_down_dither[rightpart & 7]) >> 7;
        dst[h * dstStride + w] = ((val1&(~0xFF)) ? ((-val1) >> 31) : (val1));
        dst[h * dstStride + rightpart] = ((val2&(~0xFF)) ? ((-val2) >> 31) : (val2));
        dst[bh * dstStride + w] = ((val3&(~0xFF)) ? ((-val3) >> 31) : (val3));
        dst[bh * dstStride + rightpart] = ((val4&(~0xFF)) ? ((-val4) >> 31) : (val4));

        src += dstStride * srcHeight;
        dst += dstStride * dstHeight;
        val1 = (src[cfilterPos[h] * (dstChrStride) + w] + local_up_dither[ w & 7]) >> 7;
        dst[h * (dstChrStride) + w] = ((val1&(~0xFF)) ? ((-val1) >> 31) : (val1));

        src += dstChrStride * (srcHeight >> 1);
        dst += dstChrStride * chrHeight;
        val1 = (src[cfilterPos[h] * dstChrStride + w] + local_up_dither[ (w + 3) & 7] ) >> 7;
        dst[h * dstChrStride + w] = ((val1&(~0xFF)) ? ((-val1) >> 31) : (val1));

    }
