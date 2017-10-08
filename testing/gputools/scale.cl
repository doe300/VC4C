
#ifdef USENEAREST
#define SAMPLERFILTER CLK_FILTER_NEAREST
#else
#define SAMPLERFILTER CLK_FILTER_LINEAR
#endif





__kernel void scale(__read_only image3d_t input, __global TYPENAME* output)
{

    const sampler_t sampler = CLK_NORMALIZED_COORDS_TRUE |
      CLK_ADDRESS_CLAMP_TO_EDGE |	SAMPLERFILTER;

  uint i = get_global_id(0);
  uint j = get_global_id(1);
  uint k = get_global_id(2);
  
  uint Nx = get_global_size(0);
  uint Ny = get_global_size(1);
  uint Nz = get_global_size(2);

  TYPENAME pix = READ_IMAGE(input,sampler,(float4)(1.f*i/(Nx-1.f),
						 1.f*j/(Ny-1.f),
						 1.f*k/(Nz-1.f),0)).x;
  
  output[i+Nx*j+Nx*Ny*k] = pix;
  

}

