kernel void kernel_memory_alignment_private(global uint *results)
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

kernel void kernel_memory_alignment_global(global uint *results, global char *mem0, global char2 *mem2, global char2 *mem3, global char4 *mem4, global char8 *mem8, global char16 *mem16)
{
   results[0] = (uint)&mem0[0];
   results[1] = (uint)&mem2[0];
   results[2] = (uint)&mem3[0];
   results[3] = (uint)&mem4[0];
   results[4] = (uint)&mem8[0];
   results[5] = (uint)&mem16[0];
}

kernel void kernel_memory_alignment_local(global uint *results, local char *mem0, local char2 *mem2, local char2 *mem3, local char4 *mem4, local char8 *mem8, local char16 *mem16)
{
   results[0] = (uint)&mem0[0];
   results[1] = (uint)&mem2[0];
   results[2] = (uint)&mem3[0];
   results[3] = (uint)&mem4[0];
   results[4] = (uint)&mem8[0];
   results[5] = (uint)&mem16[0];
}

kernel void kernel_memory_alignment_local_char(global uint *results)
{
   local char mem0[3];
   local char2 mem2[3];
   local char3 mem3[3];
   local char4 mem4[3];
   local char8 mem8[3];
   local char16 mem16[3];
   results[0] = (uint)&mem0[0];
   results[1] = (uint)&mem2[0];
   results[2] = (uint)&mem3[0];
   results[3] = (uint)&mem4[0];
   results[4] = (uint)&mem8[0];
   results[5] = (uint)&mem16[0];
}


kernel void kernel_memory_alignment_constant_parameter(global uint *results, constant char *mem0, constant char2 *mem2, constant char2 *mem3, constant char4 *mem4, constant char8 *mem8, constant char16 *mem16)
{
   results[0] = (uint)&mem0[0];
   results[1] = (uint)&mem2[0];
   results[2] = (uint)&mem3[0];
   results[3] = (uint)&mem4[0];
   results[4] = (uint)&mem8[0];
   results[5] = (uint)&mem16[0];
}

  constant char mem0[3]    = {0};
  constant char2 mem2[3]   = {(char2)(0)};
  constant char3 mem3[3]   = {(char3)(0)};
  constant char4 mem4[3]   = {(char4)(0)};
  constant char8 mem8[3]   = {(char8)(0)};
  constant char16 mem16[3] = {(char16)(0)};

kernel void kernel_memory_alignment_constant(global uint *results)
{
   results[0] = (uint)&mem0;
   results[1] = (uint)&mem2;
   results[2] = (uint)&mem3;
   results[3] = (uint)&mem4;
   results[4] = (uint)&mem8;
   results[5] = (uint)&mem16;
}
