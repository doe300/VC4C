/*
 * Uses as many OpenCL runtime-library methods as possible
 */

float4 doSomeMore(float4 x)
{
	char little = clamp(120, 0, 100);	//100
	float deg = degrees(2.0f);			//114.6
	int m = max(4,5);			//5
	m = min(4,5);				//4
	float blend = mix(1.7f, 2.5f, 0.5f);	//2.1
	float rad = radians(deg);		//~2
	int s = sign(-5.0f);			//-1
	float4 v1;
	float4 v2;
	float4 c = cross(v1,v2);
	float d = dot(v1, v2);
	float dis = distance(v1, v2);		//=length(v1-v2)
	float l = length(v1-v2);
	float4 n = normalize(v1);		//length(n) = 1
	float fd = fast_distance(v1,v2);
	float fl = fast_length(v1-v2);
	float4 fn = fast_normalize(v1);

	return fn;
}

int doSomeStuff(int i)
{
	i = i + 1;
	float f = i;
	float a = acos(f);
	//when only this long is method inlined
	i = ceil(f);
	int fl = floor(f);

	f = cospi(f);
	fl = ceil(f);	

	float g = copysign(f, -1.0f);
	
	for(int x = 0; x < 10; ++x)
		g += x * x - fl;

	return fl * g;
}

__kernel void test_kernel(__global const float2 * x,__global float2 * y,int p)
{
	char little = clamp(120, 0, 100);	//100
	float deg = degrees(2.0f);			//114.6
	int m = max(4,5);			//5
	m = min(4,5);				//4
	float blend = mix(1.7f, 2.5f, 0.5f);	//2.1
	float rad = radians(deg);		//~2
	float mx = max(deg, rad);
	int s = sign(-5.0f);			//-1
	float4 v1;
	v1[0] = MAXFLOAT;
	v1[1] =  HUGE_VALF ;
	v1[2] = INFINITY;
	v1[3] = NAN;
	float4 v2;
	float ex = exp(mx);
	float ex10 = exp10(mx);
	float ex2 = exp2(mx);
	ex = log(ex);
	ex10 = log10(ex10);
	ex2 = log2(ex2);
	float4 c = cross(v1,v2);
	float d = dot(v1, v2);
	float dis = distance(v1, v2);		//=length(v1-v2)
	float l = length(v1-v2);
	float4 n = normalize(v1);		//length(n) = 1
	float fd = fast_distance(v1,v2);
	float fl = fast_length(v1-v2);
	float4 fn = fast_normalize(v1);

	short a = vec_step(v2);			//4
	ushort ad = abs_diff((short)500,a);		//300

	uchar2 u2;
	u2[0] = (uchar) ((*x)[0]);
	u2[1] = (uchar) ((*x)[1]);
	uchar lz = clz(u2[0]);
	uchar2 lz2 = clz(u2);
	int m24 = mad24(s, 42, 154);		//a * b (both 24-bit) + c (32-bit) -> 32-bit
	int2 i2;
	i2[0] = ((*x)[0]);
	i2[1] = ((*x)[1]);
	i2 = mul24(i2, i2);
	i2[1] = mul24(m, 42);
	fn = mad(fn, fn, fn);
	int mhi = mad_hi(s, 42, 154);		//mul_hi(a, b) + c
	int msat = mad_sat(s, 42, 154);		//a * b + c (saturated)
	m24 = mul24(s, 42);
	mhi = mul_hi(s, 42);
	int2 rot = rotate(i2, 3);
	uchar2 subs = sub_sat(u2, 42);
	ushort2 upsam = upsample(u2, u2);
	float sqr = sqrt(rad);
	float non = nan(17);
	non = rsqrt(non);

	rot = shuffle(rot, convert_uint2(rot));
	int4 sh = shuffle2(rot, i2, (uint4)(1,3,2,1));

	if(a < ad)
	{
		for(int i = 0; i < 100; ++i)
			a += i;
		doSomeMore(n);
	}
	a = max((ushort)a, ad);
	s = a == 4 ? 4 : 5;
	*y = s;

	a = a * s;
	s = a / s;
	m = m / 4;
	int k = s * 128;
	s = m % k + non;
	
	*y = doSomeStuff(s + sh.y);
}
