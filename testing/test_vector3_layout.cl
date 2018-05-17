__kernel void test_vector_3(__global int3* in1, __global int3* out1, __global short3* in2, __global int3* out2, __global char3* in3, __global int3* out3)
{
	size_t id = get_global_id(0);
	
	out1[id] = in1[id] + (int)id;
//	Original example:
//	out2[id] = in2[id] + (short)id;
//	out3[id] = in3[id] + (char)id;
//	This works:
//	out2[id] = vc4cl_bitcast_short(in1[id]) + (short)id;
//	out3[id] = vc4cl_bitcast_char(in1[id]) + (char)id;
//	This doesn't (out2 and out3 int3*)
	out2[id] = convert_int3(in2[id]) + (int)id;
	out3[id] = convert_int3(in3[id]) + (int)id;
//FIXME error reading with size != 32 bits
	//replicates the first element 1(3) times for short(byte)
	// ~> (32 - type-size)/8
//TODO test all other types/vectors char/short (1, 2,3, 4, 8, 16)
}
