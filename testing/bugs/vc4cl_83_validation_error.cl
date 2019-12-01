#define IMAGE_DECLARATION(name)                                                                                        \
    __global uchar *name##_ptr, uint name##_stride_x, uint name##_step_x, uint name##_stride_y, uint name##_step_y,    \
        uint name##_offset_first_element_in_bytes

#define CONVERT_TO_IMAGE_STRUCT(name)                                                                                  \
    update_image_workitem_ptr(name##_ptr, name##_offset_first_element_in_bytes, name##_stride_x, name##_step_x,        \
        name##_stride_y, name##_step_y)

#define CONVERT_TO_IMAGE_STRUCT_NO_STEP(name)                                                                          \
    update_image_workitem_ptr(name##_ptr, name##_offset_first_element_in_bytes, name##_stride_x, 0, name##_stride_y, 0)

/** Structure to hold Image information */
typedef struct Image
{
    __global uchar* ptr;               /*< Pointer to the starting postion of the buffer*/
    int offset_first_element_in_bytes; /*< The offset of the first element in the source image*/
    int stride_x;                      /*< Stride of the image in X dimension(in bytes) */
    int stride_y;                      /*< Stride of the image in Y dimension(in bytes) */
} Image;

__kernel void test_kernel(IMAGE_DECLARATION(in), IMAGE_DECLARATION(out), __global float matrix[9])
{
    Image in = CONVERT_TO_IMAGE_STRUCT_NO_STEP(in);
    Image out = CONVERT_TO_IMAGE_STRUCT(out);

    float8 mtx = (float8)(matrix[0], matrix[1], matrix[2], matrix[3], matrix[4], matrix[5], 0.0, 0.0);

    float4 in_x_coords = (float4)(0, 1, 2, 3);

    float4 new_x = mad(in_x_coords, in_x_coords, in_x_coords);

    uchar4 newxcv = convert_uchar4_rtn(new_x);

    vstore4(newxcv, 0, out.ptr);
}