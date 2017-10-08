/*
 * Copyright 1993-2010 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

#define PI 3.1415926536f

/* 
 * Paint a 2D texture with a moving red/green hatch pattern on a
 * strobing blue background.  Note that this kernel reads to and
 * writes from the texture, hence why this texture was not mapped
 * as WriteDiscard.
 */
__kernel void cl_kernel_texture_2d(
#ifdef USE_STAGING_BUFFER
				__global uint *bufOut, 
#else
				__write_only image2d_t texOut, 
#endif
				__read_only image2d_t texIn, 
				uint width, 
				uint height, 
				uint pitch, 
				float t)
{
    const int tx = get_local_id(0);		// Cuda equivalent : threadIdx.x
    const int ty = get_local_id(1);		// Cuda equivalent : threadIdx.y
    const int bw = get_local_size(0);	// Cuda equivalent : blockDim.x
    const int bh = get_local_size(1);	// Cuda equivalent : blockDim.y
    const int x = get_global_id(0);		// Cuda equivalent : blockIdx.x*bw + tx
    const int y = get_global_id(1);		// Cuda equivalent : blockIdx.y*bh + ty

    // in the case where, due to quantization into grids, we have
    // more threads than pixels, skip the threads which don't 
    // correspond to valid pixels
	if (x >= width || y >= height) return;

    //int w = get_image_width(texIn); 
    //int h = get_image_height(texIn); 
    
    float4 pixel = (float4)0;
    float4 color;
	
    int2 coord = (int2)(x, y); 
    const sampler_t smp = CLK_FILTER_NEAREST; 
	pixel = read_imagef(texIn, smp, coord); 
	// populate it
	float value_x = 0.5f + 0.5f*cos(t + 10.0f*( (2.0f*x)/width  - 1.0f ) );
	float value_y = 0.5f + 0.5f*cos(t + 10.0f*( (2.0f*y)/height - 1.0f ) );
#if 0 // To test if we can read correctly the source... I can't for now...
	color = pixel;
#else
	color.x = 0.9*pixel.x + 0.1*pow(value_x, 3.0f);
	color.y = 0.9*pixel.y + 0.1*pow(value_y, 3.0f);
	color.z = 0.5f + 0.5f*cos(t);
	color.w = 1.0f;
#endif
#ifdef USE_STAGING_BUFFER
	color *= 255.0;
	uint value = (((uchar)color.w)<<24) 
		  | (((uchar)color.z)<<16) 
		  | (((uchar)color.y)<<8) 
		  | ((uchar)color.x);
	bufOut[x + y*pitch] = value;
#else
    write_imagef(texOut, coord, color); 
#endif
}

