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
input:    Input buffer;
output:   Output buffer;
w:        Width of frame;
h:        Height of frame;
********************************************************************************************************/
    kernel void nv12toyuv ( global char *input, global char* output, int w, int h )
    {
        int x = get_global_id( 0 );
        int y = get_global_id( 1 );
        int idx = y * (w >> 1) + x;
        vstore4((vload4( 0, input + (idx << 2))), 0, output + (idx << 2)); //Y
        char2 uv = vload2( 0, input + (idx << 1) + w * h );
        output[idx + w * h] = uv.s0;
        output[idx + w * h + ((w * h) >> 2)] = uv.s1;
    }
