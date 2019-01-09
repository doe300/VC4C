//See https://github.com/doe300/VC4CL/issues/27 for discussion
__kernel void hello(__global float * x){                                                                                                                                                     
	int ind = get_global_id(0);                                                                                                                                                            
	x[ind] = x[ind] * 2;                                                                                                                                                                   
}   
