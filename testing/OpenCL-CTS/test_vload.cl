__kernel void test_vload2( __global int *src, __global uint *offsets, __global uint *alignmentOffsets, __global int2 *results )
{
    int tid = get_global_id( 0 );
    int2 tmp = vload2( offsets[ tid ], ( (__global int *) src ) + alignmentOffsets[ tid ] );
   results[ tid ] = tmp;
}

__kernel void sample_test(__global short3 *sourceA, __global int *destValues)
{
    int  tid = get_global_id(0);
    destValues[tid] = all(vload3(tid, (__global short *)sourceA));

}

