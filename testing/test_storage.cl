// This is just for file display purposes
#ifndef TYPE
#define TYPE uint
#endif

#define CONCAT(a, b) a##b
#define CAT(a, b) CONCAT(a, b)

#ifdef STORAGE
__attribute__((reqd_work_group_size(1, 1, 1)))
#endif
__kernel void
test(__global TYPE* out, const __global TYPE* in
#ifndef STORAGE
    ,
    __global TYPE* p11, __global CAT(TYPE, 16) * p12, __global TYPE* p21, __global CAT(TYPE, 16) * p22,
    __global TYPE* p31, __global CAT(TYPE, 16) * p32
#endif
)
{
    size_t gid = get_global_id(0);

#ifdef STORAGE
    STORAGE TYPE p11[16];
    STORAGE CAT(TYPE, 16) p12[1];
    STORAGE TYPE p21[16];
    STORAGE CAT(TYPE, 16) p22[1];

    STORAGE TYPE p31[16];
    STORAGE CAT(TYPE, 16) p32[1];
#endif

#ifndef STORAGE
#define STORAGE __global
#endif

    CAT(TYPE, 16) t = vload16(gid, in);

    // test vstore + assignment load
    vstore16(t, 0, p11);
    vstore16(t + (TYPE) 3, 0, (STORAGE TYPE*) p12);

    CAT(TYPE, 16) t1 = *((STORAGE CAT(TYPE, 16)*) p11);
    CAT(TYPE, 16) t2 = *p12;

    // test assignment store + vload
    *((STORAGE CAT(TYPE, 16)*) p21) = t + (TYPE) 7;
    *p22 = t + (TYPE) 11;

    CAT(TYPE, 16) t3 = vload16(0, p21);
    CAT(TYPE, 16) t4 = vload16(0, (STORAGE TYPE*) p22);

    // test vstore + vload
    vstore16(t + (TYPE) 13, 0, p31);
    vstore16(t + (TYPE) 17, 0, (STORAGE TYPE*) p32);

    CAT(TYPE, 16) t5 = vload16(0, p31);
    CAT(TYPE, 16) t6 = vload16(0, (STORAGE TYPE*) p32);

    vstore16(t1 + t2 + t3 + t4 + t5 + t6, gid, out);
}