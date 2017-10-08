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

 void filter_v6(
        global unsigned char *dst,
        global unsigned char *prev, 
        global unsigned char *cur, 
        global unsigned char *next,
        int x,
        int y,
        int width,
        int height,
        int parity,
        int inlinesize,
        int outlinesize,
        int inmode,
        int uvflag
    )
    {

        int flag = uvflag * (y >=height) * height;  
        int prefs = select(-(inlinesize), inlinesize,((y+1) - flag) <height);
        int mrefs = select(inlinesize, -(inlinesize),y - flag);
        int mode  = select(inmode,2,(y - flag==1) || (y - flag + 2==height));
        int score;
    
        global unsigned char *prev2 = parity ? prev : cur ;
        global unsigned char *next2 = parity ? cur  : next;
        int index = x + y * inlinesize;
        int outindex = x + y * outlinesize;
        int c = cur[index + mrefs]; 
        int d = (prev2[index] + next2[index])>>1; 
        int e = cur[index + prefs]; 
        int temporal_diff0 = abs((prev2[index]) - (next2[index])); 
        int temporal_diff1 =(abs(prev[index + mrefs] - c) + abs(prev[index + prefs] - e) )>>1; 
        int temporal_diff2 =(abs(next[index + mrefs] - c) + abs(next[index + prefs] - e) )>>1; 
        int diff = max(max(temporal_diff0>>1, temporal_diff1), temporal_diff2); 
        int spatial_pred = (c+e)>>1; 
        int spatial_score = abs(cur[index + mrefs-1] - cur[index + prefs-1]) + abs(c-e) + abs(cur[index + mrefs+1] - cur[index + prefs+1]) - 1; 
        //check -1
        score = abs(cur[index + mrefs-2] - cur[index + prefs])
            + abs(cur[index + mrefs-1] - cur[index + prefs+1])
            + abs(cur[index + mrefs] - cur[index + prefs+2]);
        if (score < spatial_score)
        {
            spatial_score= score;
            spatial_pred= (cur[index + mrefs-1] + cur[index + prefs+1])>>1;
        }
        //check -2
        score = abs(cur[index + mrefs-3] - cur[index + prefs+1])
            + abs(cur[index + mrefs-2] - cur[index + prefs+2])
            + abs(cur[index + mrefs-1] - cur[index + prefs+3]);
        if (score < spatial_score)
        {
            spatial_score= score;
            spatial_pred= (cur[index + mrefs-2] + cur[index + prefs+2])>>1;
        }
        //check 1
        score = abs(cur[index + mrefs] - cur[index + prefs-2])
            + abs(cur[index + mrefs+1] - cur[index + prefs-1])
            + abs(cur[index + mrefs+2] - cur[index + prefs]);
        if (score < spatial_score)
        {
            spatial_score= score;
            spatial_pred= (cur[index + mrefs+1] + cur[index + prefs-1])>>1;
        }
        //check 2
        score = abs(cur[index + mrefs+1] - cur[index + prefs-3])
            + abs(cur[index + mrefs+2] - cur[index + prefs-2])
            + abs(cur[index + mrefs+3] - cur[index + prefs-1]);
        if (score < spatial_score)
        {
            spatial_score= score;
            spatial_pred= (cur[index + mrefs+2] + cur[index + prefs-2])>>1;
        }
        if (mode < 2)
        { 
            int b = (prev2[index + (mrefs<<1)] + next2[index + (mrefs<<1)])>>1; 
            int f = (prev2[index + (prefs<<1)] + next2[index + (prefs<<1)])>>1; 
            int diffmax = max(max(d-e, d-c), min(b-c, f-e)); 
            int diffmin = min(min(d-e, d-c), max(b-c, f-e)); 

            diff = max(max(diff, diffmin), -diffmax); 
        } 
        if (spatial_pred > d + diff) 
        {
            spatial_pred = d + diff; 
        }
        else if (spatial_pred < d - diff) 
        {
            spatial_pred = d - diff; 
        }

        dst[outindex] = spatial_pred; 
    }

    kernel void yadif_filter(
        global unsigned char *dst,
        global unsigned char *prev,
        global unsigned char *cur,
        global unsigned char *next,
        int parity,
        int inlinesizeY,
        int inlinesizeUV,
        int outlinesizeY,
        int outlinesizeUV,
        int mode)
    {
        int x=get_global_id(0);
        int y=(get_global_id(1)<<1) + (!parity);
        int width=(get_global_size(0)<<1)/3;
        int height=get_global_size(1)<<1;
    

        global unsigned char *dst_Y=dst;
        global unsigned char *dst_U=dst_Y+height*outlinesizeY;

        global unsigned char *prev_Y=prev;
        global unsigned char *prev_U=prev_Y+height*inlinesizeY;

        global unsigned char *cur_Y=cur;
        global unsigned char *cur_U=cur_Y+height*inlinesizeY;

        global unsigned char *next_Y=next;
        global unsigned char *next_U=next_Y+height*inlinesizeY;

        if(x < width)
        {
            filter_v6(dst_Y,prev_Y,cur_Y,next_Y,x,y,width,height,parity,inlinesizeY,outlinesizeY,mode,0);
        }
        else
        {
            x = x - width;
            filter_v6(dst_U,prev_U,cur_U,next_U,x,y,width>>1,height>>1,parity,inlinesizeUV,outlinesizeUV,mode,1);
        }
    }
