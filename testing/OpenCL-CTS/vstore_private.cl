
__kernel void test_fn( __global char2 *srcValues, __global uint *offsets, __global char2 *destBuffer, uint alignmentOffset )
{
    __private char2 sPrivateStorage[ 128 ];
    int tid = get_global_id( 0 );
 sPrivateStorage[tid] = (char2)(char)0;

   vstore2( srcValues[ tid ], offsets[ tid ], ( (__private char *)sPrivateStorage ) + alignmentOffset );

  uint i;
  __private char *sp = (__private char*) (sPrivateStorage + offsets[tid]) + alignmentOffset;
  __global char *dp = (__global char*) (destBuffer + offsets[tid]) + alignmentOffset;
  for( i = 0; i < sizeof( sPrivateStorage[0]) / sizeof( *sp ); i++ ) 
       dp[i] = sp[i];
}

__kernel void test_fn2( __global uchar2 *srcValues, __global uint *offsets, __global uchar2 *destBuffer, uint alignmentOffset )
{
    __private uchar2 sPrivateStorage[ 128 ];
    int tid = get_global_id( 0 );
 sPrivateStorage[tid] = (uchar2)(uchar)0;

   vstore2( srcValues[ tid ], offsets[ tid ], ( (__private uchar *)sPrivateStorage ) + alignmentOffset );

  uint i;
  __private uchar *sp = (__private uchar*) (sPrivateStorage + offsets[tid]) + alignmentOffset;
  __global uchar *dp = (__global uchar*) (destBuffer + offsets[tid]) + alignmentOffset;
  for( i = 0; i < sizeof( sPrivateStorage[0]) / sizeof( *sp ); i++ ) 
       dp[i] = sp[i];
}

__kernel void test_fn3( __global int2 *srcValues, __global uint *offsets, __global int2 *destBuffer, uint alignmentOffset )
{
    __private int2 sPrivateStorage[ 32 ];
    int tid = get_global_id( 0 );
 sPrivateStorage[tid] = (int2)(int)0;

   vstore2( srcValues[ tid ], offsets[ tid ], ( (__private int *)sPrivateStorage ) + alignmentOffset );

  uint i;
  __private int *sp = (__private int*) (sPrivateStorage + offsets[tid]) + alignmentOffset;
  __global int *dp = (__global int*) (destBuffer + offsets[tid]) + alignmentOffset;
  for( i = 0; i < sizeof( sPrivateStorage[0]) / sizeof( *sp ); i++ ) 
       dp[i] = sp[i];
}

__kernel void test_fn4( __global uint2 *srcValues, __global uint *offsets, __global uint2 *destBuffer, uint alignmentOffset )
{
    __private uint2 sPrivateStorage[ 32 ];
    int tid = get_global_id( 0 );
 sPrivateStorage[tid] = (uint2)(uint)0;

   vstore2( srcValues[ tid ], offsets[ tid ], ( (__private uint *)sPrivateStorage ) + alignmentOffset );

  uint i;
  __private uint *sp = (__private uint*) (sPrivateStorage + offsets[tid]) + alignmentOffset;
  __global uint *dp = (__global uint*) (destBuffer + offsets[tid]) + alignmentOffset;
  for( i = 0; i < sizeof( sPrivateStorage[0]) / sizeof( *sp ); i++ ) 
       dp[i] = sp[i];
}


__kernel void test_fn5( __global float2 *srcValues, __global uint *offsets, __global float2 *destBuffer, uint alignmentOffset )
{
    __private float2 sPrivateStorage[ 32 ];
    int tid = get_global_id( 0 );
 sPrivateStorage[tid] = (float2)(float)0;

   vstore2( srcValues[ tid ], offsets[ tid ], ( (__private float *)sPrivateStorage ) + alignmentOffset );

  uint i;
  __private float *sp = (__private float*) (sPrivateStorage + offsets[tid]) + alignmentOffset;
  __global float *dp = (__global float*) (destBuffer + offsets[tid]) + alignmentOffset;
  for( i = 0; i < sizeof( sPrivateStorage[0]) / sizeof( *sp ); i++ ) 
       dp[i] = sp[i];
}

