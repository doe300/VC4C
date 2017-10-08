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
dst:          Horizontal scale destination;
src:          YUV content in opencl buf;
hf_Y:         Horizontal filter coefficients for Y planes;
hf_UV:        Horizontal filter coefficients for UV planes;
hi_Y:         Horizontal filter index for Y planes;
hi_UV:        Horizontal filter index for UV planes;
stride:       Src width;
filter_len:   Length of filter;
********************************************************************************************************/
    kernel void frame_h_scale (
        global fixed8 *src,
        global float *hf_Y,
        global float *hf_UV,
        global int *hi_Y,
        global int *hi_UV,
        global fixed8 *dst,
        int stride, //src_width
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
        global fixed8 *src_U = src_Y + stride * height;
        global fixed8 *src_V = src_U + (stride >> 1) * (height >> 1);

        global fixed8 *dst_Y = dst;
        global fixed8 *dst_U = dst_Y + width * height;
        global fixed8 *dst_V = dst_U + (width >> 1) * (height >> 1);

        int xy = y * width + x;
        global fixed8 *rowdata_Y = src_Y + (y * stride);
        for( int i = 0; i < filter_len; i++ )
        {
            result_Y += ( hf_Y[x + i * width] * rowdata_Y[hi_Y[x] + i]);
        }
        dst_Y[xy] = result_Y;

        if( y < (height >> 1) && x < (width >> 1) )
        {
            int xy = y * (width >> 1) + x;
            global fixed8 *rowdata_U = src_U + (y * (stride >> 1));
            global fixed8 *rowdata_V = src_V + (y * (stride >> 1));
            for( i = 0; i < filter_len; i++ )
            {
                result_U += ( hf_UV[x + i * (width >> 1)] * rowdata_U[hi_UV[x] + i]);
                result_V += ( hf_UV[x + i * (width >> 1)] * rowdata_V[hi_UV[x] + i]);
            }
            dst_U[xy] = result_U;
            dst_V[xy] = result_V;
        }
    }
