#ifndef TYPE
#define TYPE char
#endif
#define CONCAT(a, b) a##b
#define CAT(a, b) CONCAT(a, b)

__kernel void test_fn3(
    __global CAT(TYPE, 3) * srcValues, __global uint* offsets, __global TYPE* destBuffer, uint alignmentOffset)
{
    int tid = get_global_id(0);
    if((tid & 3) == 0)
    { // if \"tid\" is a multiple of 4
        vstore3(srcValues[3 * (tid >> 2)], offsets[tid], destBuffer + alignmentOffset);
    }
    else
    {
        vstore3(vload3(tid, (__global TYPE*) srcValues), offsets[tid], destBuffer + alignmentOffset);
    }
}