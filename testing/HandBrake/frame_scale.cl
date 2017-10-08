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

__kernel __attribute__((reqd_work_group_size(64, 1, 1))) void frame_scale(__global uchar *dst,
                          __global const uchar *src,
                          const float xscale,
                          const float yscale,
                          const int srcPlaneOffset0,
                          const int srcPlaneOffset1,
                          const int srcPlaneOffset2,
                          const int dstPlaneOffset0,
                          const int dstPlaneOffset1,
                          const int dstPlaneOffset2,
                          const int srcRowWords0,
                          const int srcRowWords1,
                          const int srcRowWords2,
                          const int dstRowWords0,
                          const int dstRowWords1,
                          const int dstRowWords2,
                          const int srcWidth,
                          const int srcHeight,
                          const int dstWidth,
                          const int dstHeight,
                          __global const float4* restrict xweights,
                          __global const float4* restrict yweights
                          )
{
    const int x = get_global_id(0);
    const int y = get_global_id(1);
    const int z = get_global_id(2);

    // Abort work items outside the dst image bounds.

    if ((get_group_id(0) * 64 >= (dstWidth >> ((z == 0) ? 0 : 1))) || (get_group_id(1) * 16 >= (dstHeight >> ((z == 0) ? 0 : 1))))
      return;

    const int srcPlaneOffset = (z == 0) ? srcPlaneOffset0 : ((z == 1) ? srcPlaneOffset1 : srcPlaneOffset2);
    const int dstPlaneOffset = (z == 0) ? dstPlaneOffset0 : ((z == 1) ? dstPlaneOffset1 : dstPlaneOffset2);
    const int srcRowWords = (z == 0) ? srcRowWords0: ((z == 1) ? srcRowWords1 : srcRowWords2);
    const int dstRowWords = (z == 0) ? dstRowWords0: ((z == 1) ? dstRowWords1 : dstRowWords2);

    __local uchar pixels[64 * 36];
    const int localRowPixels = 64;
    const int groupHeight = 16;     // src pixel height output by the workgroup
    const int ypad = 2;
    const int localx = get_local_id(0);

    const int globalStartRow = floor((get_group_id(1) * groupHeight) / yscale);
    const int globalRowCount = ceil(groupHeight / yscale) + 2 * ypad;

    float4 weights = xweights[x];
    int4 woffs = floor(x / xscale);
    woffs += (int4)(-1, 0, 1, 2);
    woffs = clamp(woffs, 0, (srcWidth >> ((z == 0) ? 0 : 1)) - 1);
    const int maxy = (srcHeight >> ((z == 0) ? 0 : 1)) - 1;

    // Scale x from global into LDS

    for (int i = 0; i <= globalRowCount; ++i) {
        int4 offs = srcPlaneOffset + clamp(globalStartRow - ypad + i, 0, maxy) * srcRowWords;
        offs += woffs;
        pixels[localx + i * localRowPixels] = convert_uchar(clamp(round(dot(weights,
                       (float4)(src[offs.x], src[offs.y], src[offs.z], src[offs.w]))), 0.0f, 255.0f));
    }
 
    barrier(CLK_LOCAL_MEM_FENCE);

    // Scale y from LDS into global

    if (x >= dstWidth >> ((z == 0) ? 0 : 1))
        return;

    int off = dstPlaneOffset + x + (get_group_id(1) * groupHeight) * dstRowWords;

    for (int i = 0; i < groupHeight; ++i) {
        if (y >= dstHeight >> ((z == 0) ? 0 : 1))
          break;
        int localy = floor((get_group_id(1) * groupHeight + i) / yscale);
        localy = localy - globalStartRow + ypad;
        int loff = localx + localy * localRowPixels;
        dst[off] = convert_uchar(clamp(round(dot(yweights[get_group_id(1) * groupHeight + i],
                                (float4)(pixels[loff - localRowPixels], pixels[loff], pixels[loff + localRowPixels]
                                , pixels[loff + localRowPixels * 2]))), 0.0f, 255.0f));
        off += dstRowWords;
    }
}
