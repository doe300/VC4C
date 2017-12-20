#include <cmath>
#include <iostream>
#include <utility>

std::pair<uint32_t, uint32_t> calcConstants(uint64_t divisor, uint64_t accuracy)
{
	uint32_t shift = static_cast<uint32_t>(std::log2(divisor * accuracy)) + 2;
	uint32_t factor = static_cast<uint32_t>(std::round(std::pow(2.0, shift) / static_cast<double>(divisor)));

	return std::make_pair(factor, shift);
}

static uint32_t calcDivision(uint32_t numerator, uint32_t denominator, uint32_t factor, uint32_t offset)
{
	uint32_t tmp = static_cast<uint32_t>(numerator * factor) >> offset;
	//XXX test up to 256: fails (only) for exact multiples (but not all of them)
	//This line fixes this issue
//	if(((numerator - static_cast<uint32_t>(tmp * denominator)) >= denominator))
//	if((static_cast<int32_t>(numerator) - static_cast<int32_t>(tmp * denominator)) >= denominator)
	if((static_cast<int32_t>(denominator) - (static_cast<int32_t>(numerator) - static_cast<int32_t>(tmp * denominator))) <= 0)
		return tmp + 1;
	return tmp;
}

int main(int argc, char** argv)
{
	
	for(uint32_t d = 1; d < 65536; ++d)
	{
		//accuracy is determined by experiment: <= 16000 fails with value mismatch, >= 16500 with constant overflow
		const auto constants = calcConstants(d, 16100);
		if(constants.first >= 65536 || constants.second > 31)
		{
			std::cerr << "Constants out of range for denominator " << d << ", f=" << constants.first << ", s=" << constants.second << std::endl;
			continue;
		}
		for(uint32_t n = 0; n < 65536; ++n)
		{
			if(calcDivision(n, d, constants.first, constants.second) != (n / d))
			{
				std::cerr << "Mismatch for n=" << n << ", d=" << d << " (f=" << constants.first << ", s=" << constants.second << ")" << std::endl;
				std::cerr << "Got " << calcDivision(n, d, constants.first, constants.second) << ", expected " << (n / d) << std::endl;
			}
		}
	}

	return 0;
}
