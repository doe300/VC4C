kernel void test_kernel (global ulong *output)
{
   local char  l_int8[3]; 
   local int   l_int32[3];
   local float l_float[3];
   output[0] = (ulong)l_int8;
   output[1] = (ulong)l_int32;
   output[2] = (ulong)l_float;
}
