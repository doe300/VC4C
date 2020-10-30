
__kernel void test_prime(const int maybePrime, __global uint* out)
{
	float tmp = maybePrime;
	tmp = sqrt(tmp);
	int root = ceil(tmp);
	for(int i = 2; i <= root; i++)
	{
		if(maybePrime % i == 0)
		{
			*out = false;
			return;
		}
	}
	*out = true;
}
