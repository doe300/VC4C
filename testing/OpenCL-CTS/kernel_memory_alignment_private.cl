kernel void test(global uint *results)
{
   private char mem0[3];
   private char2 mem2[3];
   private char3 mem3[3];
   private char4 mem4[3];
   private char8 mem8[3];
   private char16 mem16[3];
   results[0] = (uint)&mem0[0];
   results[1] = (uint)&mem2[0];
   results[2] = (uint)&mem3[0];
   results[3] = (uint)&mem4[0];
   results[4] = (uint)&mem8[0];
   results[5] = (uint)&mem16[0];
}

