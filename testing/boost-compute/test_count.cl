__kernel void serial_count_if_int_equal(uint size, __global uint* result, __global int* _buf0)
{
uint count = 0;
for(uint i = 0; i < size; i++)
{
	const int value=_buf0[i];
	if(value==2)
	{
		count++;
	}
}
*result = count;
}

__kernel void serial_count_if_vector_equal(uint size, __global uint* result, __global int4* _buf0)
{
uint count = 0;
for(uint i = 0; i < size; i++)
{
	const int4 value=_buf0[i];
	if(all(value==(int4)(0,1,2,3)))
	{
		count++;
	}
}
*result = count;
}

__kernel void serial_count_if_char_equal(uint size, __global uint* result, __global char* _buf0)
{
uint count = 0;
for(uint i = 0; i < size; i++)
{
	const char value=_buf0[i];
	if(value=='\n')
	{
	count++;
	}
}
*result = count;
}

__kernel void serial_count_if_uchar_equal(uint size, __global uint* result, __global uchar* _buf0)
{
uint count = 0;
for(uint i = 0; i < size; i++)
{
	const uchar value=_buf0[i];
	if(value==0x10)
	{
	count++;
	}
}
*result = count;
}

__kernel void serial_count_if_element_less(uint size, __global uint* result, __global int2* _buf0)
{
uint count = 0;
for(uint i = 0; i < size; i++)
{
	const int2 value=_buf0[i];
	if((value.s0)<4)
	{
		count++;
	}
}
*result = count;
}

__kernel void serial_count_if_int_equal_offset(uint size, __global uint* result, __global int* _buf0)
{
uint count = 0;
for(uint i = 0; i < size; i++){
const int value=_buf0[1+(i)];
if(value==2){
count++;
}
}
*result = count;

}

__kernel void serial_count_if_float_greater(uint size, __global uint* result, __global float* _buf0)
{
uint count = 0;
for(uint i = 0; i < size; i++){
const float value=_buf0[i];
if(value>2.00000f){
count++;
}
}
*result = count;

}

__kernel void serial_count_if_vector_equal_offset(uint size, __global uint* result, __global int4* _buf0)
{
uint count = 0;
for(uint i = 0; i < size; i++){
const int4 value=_buf0[i];
if(all(value==(int4)(1,2,3,4))){
count++;
}
}
*result = count;

}
