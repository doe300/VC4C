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

/* 
 * Paint a 3D texture with a gradient in X (blue) and Z (green), and have every
 * other Z slice have full red.
 */
__kernel void cl_kernel_texture_volume(
				__global uint *bufOut, 
				uint width, 
				uint height, 
				uint depth, 
				uint pitch, 
				uint pitchSlice )
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
    
    // walk across the Z slices of this texture.  it should be noted that 
    // this is far from optimal data access.
	float4 pixel;
    for (int z = 0; z < depth; ++z)
    {
		// get a pointer to this pixel
		pixel.x = (float)x / (float)(pitch - 1);
		pixel.y = (float)y / (float)(height - 1);
		pixel.z = (float)z / (float)(depth - 1);
		pixel.w = 1.0;

		pixel *= 255.0;
		uint res = (((uchar)pixel.w)<<24) 
			  | (((uchar)pixel.z)<<16) 
			  | (((uchar)pixel.y)<<8) 
			  | ((uchar)pixel.x);
		bufOut[x + y*pitch + z*pitchSlice] = res;
    }
}

