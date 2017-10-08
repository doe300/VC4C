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

    kernel void hscale_fast_opencl (
        global short *dst,
        const global unsigned char *src,
        int xInc,
        int chrXInc,
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
        int xpos1 = 0;
        int xpos2 = 0;
        int xx = xpos1 >> 16;
        int xalpha = (xpos1 & 0xFFFF) >> 9;
        dst[h * dstStride + w] = (src[h * srcStride + xx] << 7) + (src[h * srcStride + xx + 1] -src[h * srcStride + xx]) * xalpha;
        int lowpart = h + (chrHeight);
        dst[lowpart * dstStride + w] = (src[lowpart * srcStride + xx] << 7) + (src[lowpart * srcStride + xx + 1] - src[lowpart * srcStride + xx]) * xalpha;

        int inv_i = w * xInc >> 16;
        if( inv_i >= srcWidth - 1)
        {
            dst[h*dstStride + w] = src[h*srcStride + srcWidth-1]*128;
            dst[lowpart*dstStride + w] = src[lowpart*srcStride + srcWidth - 1] * 128;
        }

        int rightpart = w + (chrWidth);
        xx = xpos2 >> 16;
        xalpha = (xpos2 & 0xFFFF) >> 9;
        dst[h * dstStride + rightpart] = (src[h *srcStride + xx] << 7) + (src[h * srcStride + xx + 1] - src[h * srcStride + xx]) * xalpha;
        dst[lowpart * dstStride + rightpart] = (src[lowpart * srcStride + xx] << 7) + (src[lowpart * srcStride + xx + 1] - src[lowpart * srcStride + xx]) * xalpha;
        inv_i = rightpart * xInc >> 16;
        if( inv_i >= srcWidth - 1)
        {
            dst[h * dstStride + rightpart] = src[h * srcStride + srcWidth - 1] * 128;
            dst[lowpart * dstStride + rightpart] = src[lowpart * srcStride + srcWidth - 1] * 128;
        }

        int xpos = 0;
        xpos = chrXInc * w;
        xx = xpos >> 16;
        xalpha = (xpos & 0xFFFF) >> 9;
        src += srcStride * srcHeight;
        dst += dstStride * dstHeight;
        dst[h * (dstChrStride) + w] = (src[h * (srcChrStride) + xx] * (xalpha^127) + src[h * (srcChrStride) + xx + 1] * xalpha);
        inv_i = w * xInc >> 16;
        if( inv_i >= (srcWidth >> 1) - 1)
        {
            dst[h * (dstChrStride) + w] = src[h * (srcChrStride) + (srcWidth >> 1) -1]*128;
        }

        xpos = chrXInc * (w);
        xx = xpos >> 16;
        src += srcChrStride * srcHeight >> 1;
        dst += (dstChrStride * chrHeight);
        dst[h * (dstChrStride) + w] = (src[h * (srcChrStride) + xx] * (xalpha^127) + src[h * (srcChrStride) + xx + 1 ] * xalpha);

        if( inv_i >= (srcWidth >> 1) - 1)
        {
            //v channel:
            dst[h * (dstChrStride) + w] = src[h * (srcChrStride) + (srcWidth >> 1) -1] * 128;
        }
    }
