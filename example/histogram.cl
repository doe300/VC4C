__attribute__((reqd_work_group_size(1,1,1)))
__kernel void histogram_single_min_max(__global const TYPE* src, const uint num_elements, const uint num_buckets, __global uint* res, const TYPE minimum, const TYPE maximum)
{
	const TYPE range_per_bucket = (maximum - minimum) / num_buckets;
	//for each entry, determine bucket, increment number
	for(uint i = 0; i < num_elements; ++i)
	{
#ifdef FLOATING_TYPE
		res[convert_int(src[i] / range_per_bucket)] += 1;
#else
		res[src[i] / range_per_bucket] += 1;
#endif
	}
}

__attribute__((reqd_work_group_size(1,1,1)))
__kernel void histogram_single(__global const TYPE* src, const uint num_elements, const uint num_buckets, __global uint* res)
{
	//1. calculate minimum and maximum
	TYPE minimum = MAX_VALUE;
	TYPE maximum = MIN_VALUE;
	for(uint i = 0; i < num_elements; ++i)
	{
		minimum = min(minimum, src[i]);
		maximum = max(maximum, src[i]);
	}

	//2. for each entry, determine bucket and increment
	histogram_single_min_max(src, num_elements, num_buckets, res, minimum, maximum);
}


__kernel void histogram_parallel_min_max(__global const TYPE* src, const uint num_elements, const uint num_buckets, __global uint* res, const TYPE minimum, const TYPE maximum)
{
	const uint execution_index = get_global_id(0);
	const uint elements_per_execution = num_elements / get_global_size(0);
	const TYPE range_per_bucket = (maximum - minimum) / num_buckets;

	//for each entry, determine bucket, increment number
	for(uint i = execution_index * elements_per_execution; i < (execution_index + 1) * elements_per_execution; ++i)
	{
#ifdef FLOATING_TYPE
		atomic_inc(&res[convert_int(trunc(src[i] / range_per_bucket)]);
#else
		atomic_inc(&res[src[i] / range_per_bucket]);
#endif
	}
}

//NOTE: This kernel can only run in 1 work-group!!
__kernel void histogram_parallel(__global const TYPE* src, const uint num_elements, const uint num_buckets, __global uint* res)
{
	__local TYPE total_minimum;
	__local TYPE total_maximum;

	const uint execution_index = get_local_id(0);
	const uint elements_per_execution = num_elements / get_local_size(0);

	if(execution_index == 0)
	{
		total_minimum = MAX_VALUE;
		total_maximum = MIN_VALUE;
	}

	//1. calculate minimum and maximum (for each area)
	TYPE minimum = MAX_VALUE;
	TYPE maximum = MIN_VALUE;
	for(uint i = execution_index * elements_per_execution; i < (execution_index + 1) * elements_per_execution; ++i)
	{
		minimum = min(minimum, src[i]);
		maximum = max(maximum, src[i]);
	}

	//2. calculate total minimum and maximum
	barrier(CLK_LOCAL_MEM_FENCE);
	vc4cl_mutex_lock();
	total_minimum = min(total_minimum, minimum);
	total_maximum = max(total_maximum, maximum);
	vc4cl_mutex_unlock();

	//3. for each entry, determine bucket and increment
	histogram_parallel_min_max(src, num_elements, num_buckets, res, total_minimum, total_maximum);
}
