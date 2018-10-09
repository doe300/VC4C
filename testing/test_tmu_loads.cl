
__kernel void test_tmu_loads(const __global int16* in0, const __global int16* in1, const __global int16* in2, __global int16* out)
{
  unsigned lid = get_local_id(0);
  out[lid] = in0[lid] + in1[lid] + in2[lid];
  
  int16 a = in0[16];
  int16 b = in0[17];
  int16 c = in0[18];
  int16 d = in0[19];
  int16 e = in1[16];
  int16 f = in1[17];
  int16 g = in1[18];
  int16 h = in1[19];
  int16 i = in2[16];
  int16 k = in2[17];
  int16 l = in2[18];
  int16 m = in2[19];
  out[16 + lid] = a + b + c + d + e + f + g + h + i + k + l + m;
}