#ifndef SRCTYPE
#define SRCTYPE int4
#endif

#ifndef DSTTYPE
#define DSTTYPE short8
#endif

#define CONCAT(a, b) a##b
#define CAT(a, b) CONCAT(a, b)

__kernel void test_fn(__global SRCTYPE* src, __global DSTTYPE* dst)
{
    int tid = get_global_id(0);
    DSTTYPE tmp = CAT(as_, DSTTYPE)(src[tid]);
    dst[tid] = tmp;
}
