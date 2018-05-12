
#ifdef LLVM_BUILTINS
//test to which SPIR-V functions these built-ins are converted to
//https://clang.llvm.org/docs/LanguageExtensions.html#builtin-functions
__kernel void test_clang_builtins(const float4 f, __global int4* out)
{
	float4 g = __builtin_shufflevector(f, f, 3, 1, 2, 0);	//VectorShuffle
	int4 i = __builtin_convertvector(f, int4);	//ConvertFToS
	__builtin_add_overflow(i, i, &i);
	out[0] = i;
	out[1] = __builtin_convertvector(g, int4);	//ConvertFToS
}
#else
__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test_math(const float val, const float val2, const int val3, __global float * out, __global int* out2)
{
	//val = 0.5f
	//val2 = -0.75f
	//val3 = 13;
	int i = 0;
	out[i++] = acos(val);					//acos(0.5) = 1.0471975511965979
	out[i++] = acosh(4 * val);				//acosh(2) = 1.3169578969248166
	out[i++] = acospi(val);
	out[i++] = asin(val);					//asin(0.5) = 0.5235987755983
	out[i++] = asinh(val);					//asinh(0.5) = 0.4812118250596
	out[i++] = asinpi(val);					//asin(0.5*pi) = 1.233403117511217
	out[i++] = atan(val);					//atan(0.5) = 0.4636476090008061
	out[i++] = atan2(val, val2);
	out[i++] = atanh(val);					//atanh(0.5) = 0.5493061443340548
	out[i++] = atanpi(val);					//atan(0.5*pi) = 1.0038848218538872
	out[i++] = atan2pi(val, val2);
	out[i++] = cbrt(val);
	out[i++] = ceil(val);
	out[i++] = copysign(val, val2);
	out[i++] = cos(val);					//cos(0.5) = 0.8775825618903728
	out[i++] = cosh(val);					//cosh(0.5) = 1.1276259652063807
	out[i++] = cospi(val);					//cos(0.5*pi) = 0
	out[i++] = erfc(val);
	out[i++] = erf(val);
	out[i++] = exp(val);					//exp(0.5) = 1.6487212707001282
	out[i++] = exp2(val);					//exp2(0.5) = 1.4142135623730951
	out[i++] = exp10(val);					//exp10(0.5) = 3.1622776601683795
	out[i++] = expm1(val);
	out[i++] = fabs(val2);					//fabs(-0.75) = 0.75
	out[i++] = fdim(val, val2);
	out[i++] = floor(val);					//floor(0.5) = 0
	out[i++] = fma(val, val2, val2);		//fma(0.5, -0.75, -0.75) = -1.125
	out[i++] = fmax(val, val2);				//fmax(0.5, -0.75) = 0.5
	out[i++] = fmin(val, val2);				//fmin(0.5, -0.75) = -0.75
	out[i++] = fmod(val, val2);
	out[i++] = fract(val, &out[i++]);
	out[i++] = frexp(val, &out2[i-1]);
	out[i++] = hypot(val, val2);
	out[i++] = ilogb(val);
	out[i++] = lgamma(val);
	out[i++] = lgamma_r(val, &out2[i-1]);
	out[i++] = log(val);					//log(0.5) =  -0.6931471805599453
	out[i++] = log2(val);					//log2(0.5) = -1
	out[i++] = log10(val);					//log10(0.5) = -0.30102999566398114
	out[i++] = log1p(val);
	out[i++] = logb(val);
	out[i++] = mad(val, val2, val2);
	out[i++] = maxmag(val, val2);
	out[i++] = minmag(val, val2);
	out[i++] = modf(val, &out[i++]);
	out[i++] = nan((uint)val3);
	out[i++] = nextafter(val, val2);
	out[i++] = pow(val, val2);				//pow(0.5, -0.75) = 1.681792830507429
	out[i++] = pown(val, val3);				//pown(0.5, 13) = 0.0001220703125
	out[i++] = powr(val, val2);			//powr(0.5, -0.75) = 1.681792830507429
	out[i++] = remainder(val, val2);
	out[i++] = remquo(val, val2, &out2[i-1]);
	out[i++] = rint(val);
	out[i++] = rootn(val, val3);
	out[i++] = round(val);
	out[i++] = rsqrt(val);					//rsqrt(0.5) = 1.414213562373095
	out[i++] = sin(val);					//sin(0.5) = 0.4794255386042
	out[i++] = sincos(val, &out[i++]);
	out[i++] = sinh(val);
	out[i++] = sinpi(val);					//sin(0.5*pi) = 1
	out[i++] = sqrt(val);					//sqrt(0.5) = 0.7071067811865476
	out[i++] = tan(val);					//tan(0.5) = 0.5463024898437905
	out[i++] = tanh(val);					//tanh(0.5) = 0.46211715726000974
	out[i++] = tanpi(val);
	out[i++] = tgamma(val);
	out[i++] = trunc(val);
}

__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test_integer(const int val, const int val2, __global int* out)
{
	//val = -13
	//val2 = 42
	int i = 0;
	out[i++] = abs(val);						//abs(-13) = 13
	out[i++] = abs_diff(val, val2);				//abs_diff(-13, 42) = 55
	out[i++] = add_sat(val, val2);
	out[i++] = hadd(val, val2);
	out[i++] = rhadd(val, val2);
	out[i++] = clamp(val, 0, val2);				//clamp(-13, 0, 42) = 0
	out[i++] = clz(val);						//clz(-13) = 0
	out[i++] = mad_hi(val, val2, val2);			
	out[i++] = mad_sat(val, val2, val2);
	out[i++] = max(val, val2);					//max(-13, 42) = 42
	out[i++] = min(val, val2);					//min(-13, 42) = -13
	out[i++] = mul_hi(val, val2);
	out[i++] = rotate(val, val2);
	out[i++] = sub_sat(val, val2);
	out[i++] = upsample((short)val, (ushort)val2);
	out[i++] = mad24(val, val2, val2);			//mad24(-13, 42, 42) = -504
	out[i++] = mul24(val, val2);				//mul24(-13, 42) = -546
}

__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test_common(const float val, const float val2, __global float* out)
{
	int i = 0;
	out[i++] = clamp(val, 0.0f, val2);
	out[i++] = degrees(val);
	out[i++] = max(val, val2);
	out[i++] = min(val, val2);
	out[i++] = mix(val, val2, 0.5f);
	out[i++] = radians(val);
	out[i++] = step(val, val2);
	out[i++] = smoothstep(val, 0.0f, val2);
	out[i++] = sign(val);
}

__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test_geometric(const float4 val, const float4 val2, __global float4* out)
{
	int i = 0;
	out[i++] = cross(val, val2);
	out[i++].x = dot(val, val2);
	out[i++].x = distance(val, val2);
	out[i++].x = length(val);
	out[i++] = normalize(val);
}

__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test_relational(const float16 val, const float16 val2, const int16 val3, __global int16* out)
{
	int i = 0;
	out[i++] = isequal(val, val2);
	out[i++] = isnotequal(val, val2);
	out[i++] = isgreater(val, val2);
	out[i++] = isgreaterequal(val, val2);
	out[i++] = isless(val, val2);
	out[i++] = islessgreater(val, val2);
	out[i++] = isfinite(val);
	out[i++] = isinf(val);
	out[i++] = isnan(val);
	out[i++] = isnormal(val);
	out[i++] = isordered(val, val2);
	out[i++] = isunordered(val, val2);
	out[i++] = signbit(val);
	out[i++] = any(val3);
	out[i++] = all(val3);
	out[i++] = convert_int16(bitselect(val, val2, val));
	out[i++] = convert_int16(select(val, val2, convert_uint16(val3)));
}

__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test_vector(__global const float* in, const int offset, __global float* out)
{
	float16 tmp = vload16(offset, in);
	vstore16(tmp, offset, out);
	int i = 1;
	out[i++] = vec_step(in[0]);
	out[i++] = (shuffle(tmp, (uint2)(0))).y;
	out[i++] = (shuffle2(tmp, in[0], (uint4)(4,2,1,8))).w;
	float4 tmp1 = tmp.s9ab4;
	tmp1.s31 = tmp.s57;
	vstore4(tmp1, 20, out);
}

__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test_synchronization()
{
	barrier(CLK_GLOBAL_MEM_FENCE);
	mem_fence(CLK_GLOBAL_MEM_FENCE);
	read_mem_fence(CLK_GLOBAL_MEM_FENCE);
	write_mem_fence(CLK_GLOBAL_MEM_FENCE);
}

__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test_async(__global const float16* in, const int num_values, __local float16* out)
{
	event_t event = async_work_group_copy(out, in, num_values, 0);
	event_t event2 = async_work_group_strided_copy(out + num_values, in, num_values, num_values, 0);
	
	wait_group_events(1, &event);
	wait_group_events(1, &event2);

	prefetch(in + 2 * num_values, num_values);
}

__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test_atomic(volatile __global int* ptr, const int value, __global int* out)
{
	int i = 0;
	out[i++] = atomic_add(ptr, value);
	out[i++] = atomic_sub(ptr, value);
	out[i++] = atomic_xchg(ptr, value);
	out[i++] = atomic_inc(ptr);
	out[i++] = atomic_dec(ptr);
	out[i++] = atomic_cmpxchg(ptr, value, value);
	out[i++] = atomic_min(ptr, value);
	out[i++] = atomic_max(ptr, value);
	out[i++] = atomic_and(ptr, value);
	out[i++] = atomic_or(ptr, value);
	out[i++] = atomic_xor(ptr, value);
	out[i++] = atomic_min(ptr, value);
}

#ifdef __IMAGE_SUPPORT__
const sampler_t sampler = CLK_NORMALIZED_COORDS_TRUE | CLK_ADDRESS_NONE | CLK_FILTER_LINEAR;

__attribute__((reqd_work_group_size(1,1,1)))
__kernel void test_image(read_only image1d_t i1d, read_only image2d_t i2d, read_only image3d_t i3d, read_only image1d_array_t i1da, read_only image2d_array_t i2da, const int2 c2i, const float2 c2f, const int4 c4i, const float4 c4f, __global float4* out, __global int4* out2)
{
	int i = 0;
	int j = 0;
	out[i++] = read_imagef(i1d, sampler, c2i.x);
	out[i++] = read_imagef(i1d, sampler, c2f.x);
	out2[j++] = read_imagei(i1d, sampler, c2i.x);
	out2[j++] = read_imagei(i1d, sampler, c2f.x);

	out[i++] = read_imagef(i2d, sampler, c2i);
	out[i++] = read_imagef(i2d, sampler, c2f);
	out2[j++] = read_imagei(i2d, sampler, c2i);
	out2[j++] = read_imagei(i2d, sampler, c2f);

	out[i++] = read_imagef(i3d, sampler, c4i);
	out[i++] = read_imagef(i3d, sampler, c4f);
	out2[j++] = read_imagei(i3d, sampler, c4i);
	out2[j++] = read_imagei(i3d, sampler, c4f);

	out[i++] = read_imagef(i1da, sampler, c2i);
	out[i++] = read_imagef(i1da, sampler, c2f);
	out2[j++] = read_imagei(i1da, sampler, c2i);
	out2[j++] = read_imagei(i1da, sampler, c2f);

	out[i++] = read_imagef(i2da, sampler, c4i);
	out[i++] = read_imagef(i2da, sampler, c4f);
	out2[j++] = read_imagei(i2da, sampler, c4i);
	out2[j++] = read_imagei(i2da, sampler, c4f);

	out[i++] = read_imagef(i1d, c2i.x);
	out2[j++] = read_imagei(i1d, c2i.x);

	out[i++] = read_imagef(i2d, c2i);
	out2[j++] = read_imagei(i2d, c2i);

	out[i++] = read_imagef(i3d, c4i);
	out2[j++] = read_imagei(i3d, c4i);

	out[i++] = read_imagef(i1da, c2i);
	out2[j++] = read_imagei(i1da, c2i);

	out[i++] = read_imagef(i2da, c4i);
	out2[j++] = read_imagei(i2da, c4i);

//TODO
/*	write_imagef(i2d, c2i, c4f);
	write_imagei(i2d, c2i, c4i);
	write_imagef(i2da, c4i, c4f);
	write_imagei(i2da, c4i, c4i);
	write_imagef(i1d, c2i.x, c4f);
	write_imagei(i1d, c2i.x, c4i);
	write_imagef(i1da, c2i, c4f);
	write_imagei(i1da, c2i, c4i);
*/

	out2[j++] = get_image_width(i1d);
	out2[j++] = get_image_width(i2d);
	out2[j++] = get_image_width(i3d);

	out2[j++] = get_image_height(i2d);
	out2[j++] = get_image_height(i3d);

	out2[j++] = get_image_depth(i3d);

	out2[j++] = get_image_channel_data_type(i1d);
	out2[j++] = get_image_channel_data_type(i2d);
	out2[j++] = get_image_channel_data_type(i3d);

	out2[j++] = get_image_channel_order(i1d);
	out2[j++] = get_image_channel_order(i2d);
	out2[j++] = get_image_channel_order(i3d);

	out2[j++].x = get_image_dim(i2d).x;
	out2[j++] = get_image_dim(i3d);

	out2[j++].x = get_image_array_size(i1da);
	out2[j++].x = get_image_array_size(i2da);
}
#endif
#endif
