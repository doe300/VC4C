#define PRIV_TYPE char2
#define PRIV_SIZE 128
__kernel void test_fn( __global char2 *src, __global uint *offsets, __global uint *alignmentOffsets, __global char2 *results )
{
    __private PRIV_TYPE sPrivateStorage[ PRIV_SIZE ];
    int tid = get_global_id( 0 );

    for( int i = 0; i < 128; i++ )
      sPrivateStorage[ i ] = src[ i ];

    char2 tmp = vload2( offsets[ tid ], ( (__private char *) sPrivateStorage ) + alignmentOffsets[ tid ] );
   results[ tid ] = tmp;
}

#undef PRIV_TYPE
#undef PRIV_SIZE
#define PRIV_TYPE uchar2
#define PRIV_SIZE 128
__kernel void test_fn2( __global uchar2 *src, __global uint *offsets, __global uint *alignmentOffsets, __global uchar2 *results )
{
    __private PRIV_TYPE sPrivateStorage[ PRIV_SIZE ];
    int tid = get_global_id( 0 );

    for( int i = 0; i < 128; i++ )
      sPrivateStorage[ i ] = src[ i ];

    uchar2 tmp = vload2( offsets[ tid ], ( (__private uchar *) sPrivateStorage ) + alignmentOffsets[ tid ] );
   results[ tid ] = tmp;
}

#undef PRIV_TYPE
#undef PRIV_SIZE
#define PRIV_TYPE short2
#define PRIV_SIZE 64
__kernel void test_fn3( __global short2 *src, __global uint *offsets, __global uint *alignmentOffsets, __global short2 *results )
{
    __private PRIV_TYPE sPrivateStorage[ PRIV_SIZE ];
    int tid = get_global_id( 0 );

    for( int i = 0; i < 64; i++ )
      sPrivateStorage[ i ] = src[ i ];

    short2 tmp = vload2( offsets[ tid ], ( (__private short *) sPrivateStorage ) + alignmentOffsets[ tid ] );
   results[ tid ] = tmp;
}

#undef PRIV_TYPE
#undef PRIV_SIZE
#define PRIV_TYPE ushort2
#define PRIV_SIZE 64
__kernel void test_fn4( __global ushort2 *src, __global uint *offsets, __global uint *alignmentOffsets, __global ushort2 *results )
{
    __private PRIV_TYPE sPrivateStorage[ PRIV_SIZE ];
    int tid = get_global_id( 0 );

    for( int i = 0; i < 64; i++ )
      sPrivateStorage[ i ] = src[ i ];

    ushort2 tmp = vload2( offsets[ tid ], ( (__private ushort *) sPrivateStorage ) + alignmentOffsets[ tid ] );
   results[ tid ] = tmp;
}

#undef PRIV_TYPE
#undef PRIV_SIZE
#define PRIV_TYPE int2
#define PRIV_SIZE 32
__kernel void test_fn5( __global int2 *src, __global uint *offsets, __global uint *alignmentOffsets, __global int2 *results )
{
    __private PRIV_TYPE sPrivateStorage[ PRIV_SIZE ];
    int tid = get_global_id( 0 );

    for( int i = 0; i < 32; i++ )
      sPrivateStorage[ i ] = src[ i ];

    int2 tmp = vload2( offsets[ tid ], ( (__private int *) sPrivateStorage ) + alignmentOffsets[ tid ] );
   results[ tid ] = tmp;
}

#undef PRIV_TYPE
#undef PRIV_SIZE
#define PRIV_TYPE uint2
#define PRIV_SIZE 32
__kernel void test_fn6( __global uint2 *src, __global uint *offsets, __global uint *alignmentOffsets, __global uint2 *results )
{
    __private PRIV_TYPE sPrivateStorage[ PRIV_SIZE ];
    int tid = get_global_id( 0 );

    for( int i = 0; i < 32; i++ )
      sPrivateStorage[ i ] = src[ i ];

    uint2 tmp = vload2( offsets[ tid ], ( (__private uint *) sPrivateStorage ) + alignmentOffsets[ tid ] );
   results[ tid ] = tmp;
}

#undef PRIV_TYPE
#undef PRIV_SIZE
#define PRIV_TYPE float2
#define PRIV_SIZE 32
__kernel void test_fn7( __global float2 *src, __global uint *offsets, __global uint *alignmentOffsets, __global float2 *results )
{
    __private PRIV_TYPE sPrivateStorage[ PRIV_SIZE ];
    int tid = get_global_id( 0 );

    for( int i = 0; i < 32; i++ )
      sPrivateStorage[ i ] = src[ i ];

    float2 tmp = vload2( offsets[ tid ], ( (__private float *) sPrivateStorage ) + alignmentOffsets[ tid ] );
   results[ tid ] = tmp;
}
