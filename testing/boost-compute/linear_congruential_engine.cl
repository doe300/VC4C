__kernel void multiplicand(__global uint *multiplicands)
{
    uint a = 1099087573;
    multiplicands[0] = a;
    for(uint i = 1; i < 1024; i++){
        multiplicands[i] = a * multiplicands[i-1];
    }
}

__kernel void fill(const uint seed,
                   __global uint *multiplicands,
                   __global uint *result,
                   const uint offset)
{
    const uint i = get_global_id(0);
    result[offset+i] = seed * multiplicands[i];
}
