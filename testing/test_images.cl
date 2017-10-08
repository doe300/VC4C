/*
 * Tests handling of images
 */
__kernel void test_images(read_only image2d_t in_img, write_only image1d_t out_img, __global int* out)
{
	out[0] = get_image_width(in_img);
	out[1] = get_image_height(in_img);
	out[2] = get_image_width(out_img);

	float4 color = read_imagef(in_img, (int2)(0,0));
	write_imagef(out_img, 0, color.x);
}
