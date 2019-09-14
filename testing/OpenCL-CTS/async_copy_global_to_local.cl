#define TYPE char8
#define SCALAR char

__kernel void test_async_copy_global_to_local(const __global TYPE* src, __global TYPE* dst, __local TYPE* localBuffer,
    int copiesPerWorkgroup, int copiesPerWorkItem)
{
    int i;
    // Zero the local storage first
    for(i = 0; i < copiesPerWorkItem; i++)
        localBuffer[get_local_id(0) * copiesPerWorkItem + i] = (TYPE)(SCALAR) 0;
    // Do this to verify all kernels are done zeroing the local buffer before we try the copy
    barrier(CLK_LOCAL_MEM_FENCE);
    event_t event;
    event = async_work_group_copy((__local TYPE*) localBuffer,
        (__global const TYPE*) (src + copiesPerWorkgroup * get_group_id(0)), (size_t) copiesPerWorkgroup, (event_t) 0);
    // Wait for the copy to complete, then verify by manually copying to the dest
    wait_group_events(1, &event);
    for(i = 0; i < copiesPerWorkItem; i++)
        dst[get_global_id(0) * copiesPerWorkItem + i] = localBuffer[get_local_id(0) * copiesPerWorkItem + i];
}
