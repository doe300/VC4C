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
dst:          Vertical scale destination;
src:          YUV content in opencl buf;
hf_Y:         Vertical filter coefficients for Y planes;
hf_UV:        Vertical filter coefficients for UV planes;
hi_Y:         Vertical filter index for Y planes;
hi_UV:        Vertical filter index for UV planes;
stride:       Src height;
filter_len:   Length of filter;
********************************************************************************************************/
    kernel void frame_v_scale (
        global fixed8 *src,
        global float *vf_Y,
        global float *vf_UV,
        global int *vi_Y,
        global int *vi_UV,
        global fixed8 *dst,
        int src_height,
        int filter_len
        )
    {
        int x = get_global_id( 0 );
        int y = get_global_id( 1 );
        int width = get_global_size( 0 );
        int height = get_global_size( 1 );
        float result_Y = 0, result_U = 0, result_V = 0;
        int i = 0;

        global fixed8 *src_Y = src;
        global fixed8 *src_U = src_Y + src_height * width;
        global fixed8 *src_V = src_U + (src_height >> 1) * (width >> 1);

        global fixed8 *dst_Y = dst;
        global fixed8 *dst_U = dst_Y + height * width;
        global fixed8 *dst_V = dst_U + (height >> 1) * (width >> 1);

        int xy = y * width + x;
        for( i = 0; i < filter_len; i++ )
        {
            result_Y += vf_Y[y + i * height] * src_Y[(vi_Y[y] + i) * width + x];
        }
        dst_Y[xy] = result_Y;

        if( y < (height >> 1) && x < (width >> 1) )
        {
            int xy = y * (width >> 1) + x;
            for( i = 0; i < filter_len; i++ )
            {
                result_U += vf_UV[y + i * (height >> 1)] * src_U[(vi_UV[y] + i) * (width >> 1) + x];
                result_V += vf_UV[y + i * (height >> 1)] * src_V[(vi_UV[y] + i) * (width >> 1) + x];
            }
            dst_U[xy] = result_U;
            dst_V[xy] = result_V;
        }
    }
