/*
 * Tests handling of images
 */
__kernel void test_images(write_only image1d_t out_img, read_only image2d_t in_img, read_only image3d_t img3, read_only image1d_buffer_t img1d_buffer, write_only image1d_array_t img1d_array, read_only image2d_array_t img2d_array, __global int* out)
{
	out[0] = get_image_width(in_img);
	out[1] = get_image_height(in_img);
	out[2] = get_image_width(out_img);
	out[3] = get_image_depth(img3);
	out[4] = get_image_array_size(img2d_array);
	out[5] = get_image_channel_data_type(img1d_buffer);
	out[6] = get_image_channel_order(img3);

	float4 color = read_imagef(in_img, (int2)(0,0));
	write_imagef(out_img, 0, color.x);
	
	sampler_t sampler;
	
	int4 tmp0 = read_imagei(img3, sampler, color);
	uint4 tmp1 = read_imageui(img3, tmp0);
	
	out[7] = tmp1.x;
}
