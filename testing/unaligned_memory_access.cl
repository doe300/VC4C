// Test loading and storing of data "aligned" and "unaligned" to register or VPM-row bounds.

__kernel void test_private_register(__global uint8* out, const __global uint4* in, const __global uint* offsets)
{
    // NOTE: offsets is [0, 1, 2, 3]
    // NOTE: in is [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f]
    __private uint uloc[16] = {0};

    // aligned to register start
    // -> [0, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    vstore4(in[0], 0, (__private uint*) uloc);
    // unaligned with fixed vector offset
    // -> [0, 1, 2, 3, 4, 5, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0]
    vstore4(in[1], 1, (__private uint*) uloc);
    // unaligned with dynamic vector offset
    // -> [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, 0, 0, 0, 0]
    vstore4(in[2], offsets[2], (__private uint*) uloc);
    // unaligned with fixed element offset
    // -> [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, c, d, e, f, 0]
    vstore4(in[3], 0, ((__private uint*) uloc) + 11);
    // unaligned with dynamic element offset
    // -> [0, 1, c, d, e, f, 6, 7, 8, 9, a, c, d, e, f, 0]
    vstore4(in[3], 0, ((__private uint*) uloc) + offsets[2]);

    // aligned to register start
    // -> [0, 1, c, d, e, f, 6, 7]
    out[0] = vload8(0, (__private uint*) uloc);
    // unaligned with fixed vector offset
    // -> [8, 9, a, c, d, e, f, 0]
    out[1] = vload8(1, (__private uint*) uloc);
    // unaligned with dynamic vector offset
    // -> [8, 9, a, c, d, e, f, 0]
    out[2] = vload8(offsets[1], (__private uint*) uloc);
    // unaligned with fixed element offset
    // -> [f, 6, 7, 8, 9, a, c, d]
    out[3] = vload8(0, ((__private uint*) uloc) + 5);
    // unaligned with dynamic element offset
    // -> [d, e, f, 6, 7, 8, 9, a]
    out[4] = vload8(0, ((__private uint*) uloc) + offsets[3]);
}

__kernel void test_private_vpm_full_row(__global uint8* out, const __global uint8* in, const __global uint* offsets)
{
    // NOTE: offsets is [0, 1, 2, 3, 11]
    // NOTE: in is [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    // 19, 1a, 1b, 1c, 1d, 1e, 1f]
    __private uint16 uloc[2] = {0};

    // aligned to VPM row start
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[0], 0, (__private uint*) uloc);
    // unaligned with fixed vector offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[1], 1, (__private uint*) uloc);
    // unaligned with dynamic vector offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[2], offsets[2], (__private uint*) uloc);
    // unaligned with fixed element offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__private uint*) uloc) + 20);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 18, 19, 1a, 1b, 1c, 1d, 1e, 1f, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__private uint*) uloc) + offsets[4]);

    // aligned to VPM row start
    // -> [00, 01, 02, 03, 04, 05, 06, 07]
    out[0] = vload8(0, (__private uint*) uloc);
    // unaligned with fixed vector offset
    // -> [08, 09, 0a, 18, 19, 1a, 1b, 1c]
    out[1] = vload8(1, (__private uint*) uloc);
    // unaligned with dynamic vector offset
    // -> [1d, 1e, 1f, 13, 18, 19, 1a, 1b]
    out[2] = vload8(offsets[2], (__private uint*) uloc);
    // unaligned with fixed element offset (crossing row boundaries)
    // -> [0a, 18, 19, 1a, 1b, 1c, 1d, 1e]
    out[3] = vload8(0, ((__private uint*) uloc) + 10);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [18, 19, 1a, 1b, 1c, 1d, 1e, 1f]
    out[4] = vload8(0, ((__private uint*) uloc) + offsets[4]);
}

__kernel void test_private_vpm_partial_row(__global uint8* out, const __global uint8* in, const __global uint* offsets)
{
    // XXX is currently not lowered to VPM (because a) takes too many rows to fit uncompressed and b) offset of the 2
    // dynamic elements cannot be determined) -> problem with partial row does not exist!

    // NOTE: offsets is [0, 1, 2, 3, 11]
    // NOTE: in is [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    // 19, 1a, 1b, 1c, 1d, 1e, 1f]
    __private uint4 uloc[8] = {0};

    // aligned to VPM row start (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[0], 0, (__private uint*) uloc);
    // unaligned with fixed vector offset (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[1], 1, (__private uint*) uloc);
    // unaligned with dynamic vector offset (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[2], offsets[2], (__private uint*) uloc);
    // unaligned with fixed element offset (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__private uint*) uloc) + 20);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 18, 19, 1a, 1b, 1c, 1d, 1e, 1f, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__private uint*) uloc) + offsets[4]);

    // aligned to VPM row start (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07]
    out[0] = vload8(0, (__private uint*) uloc);
    // unaligned with fixed vector offset (crossing row boundaries)
    // -> [08, 09, 0a, 18, 19, 1a, 1b, 1c]
    out[1] = vload8(1, (__private uint*) uloc);
    // unaligned with dynamic vector offset (crossing row boundaries)
    // -> [1d, 1e, 1f, 13, 18, 19, 1a, 1b]
    out[2] = vload8(offsets[2], (__private uint*) uloc);
    // unaligned with fixed element offset (crossing row boundaries)
    // -> [0a, 18, 19, 1a, 1b, 1c, 1d, 1e]
    out[3] = vload8(0, ((__private uint*) uloc) + 10);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [18, 19, 1a, 1b, 1c, 1d, 1e, 1f]
    out[4] = vload8(0, ((__private uint*) uloc) + offsets[4]);
}

__kernel void test_local_vpm_full_row(__global uint8* out, const __global uint8* in, const __global uint* offsets)
{
    // NOTE: offsets is [0, 1, 2, 3, 11]
    // NOTE: in is [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    // 19, 1a, 1b, 1c, 1d, 1e, 1f]
    __local uint16 uloc[2];
    // __local memory cannot have initializer
    vstore16((uint16) 0, 0, (__local uint*) uloc);
    vstore16((uint16) 0, 1, (__local uint*) uloc);

    // aligned to VPM row start
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[0], 0, (__local uint*) uloc);
    // unaligned with fixed vector offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[1], 1, (__local uint*) uloc);
    // unaligned with dynamic vector offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[2], offsets[2], (__local uint*) uloc);
    // unaligned with fixed element offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__local uint*) uloc) + 20);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 18, 19, 1a, 1b, 1c, 1d, 1e, 1f, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__local uint*) uloc) + offsets[4]);

    // aligned to VPM row start
    // -> [00, 01, 02, 03, 04, 05, 06, 07]
    out[0] = vload8(0, (__local uint*) uloc);
    // unaligned with fixed vector offset
    // -> [08, 09, 0a, 18, 19, 1a, 1b, 1c]
    out[1] = vload8(1, (__local uint*) uloc);
    // unaligned with dynamic vector offset
    // -> [1d, 1e, 1f, 13, 18, 19, 1a, 1b]
    out[2] = vload8(offsets[2], (__local uint*) uloc);
    // unaligned with fixed element offset (crossing row boundaries)
    // -> [0a, 18, 19, 1a, 1b, 1c, 1d, 1e]
    out[3] = vload8(0, ((__local uint*) uloc) + 10);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [18, 19, 1a, 1b, 1c, 1d, 1e, 1f]
    out[4] = vload8(0, ((__local uint*) uloc) + offsets[4]);
}

__kernel void test_local_vpm_partial_row(__global uint8* out, const __global uint8* in, const __global uint* offsets)
{
    // NOTE: offsets is [0, 1, 2, 3, 11]
    // NOTE: in is [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    // 19, 1a, 1b, 1c, 1d, 1e, 1f]
    __local uint4 uloc[8];
    // __local memory cannot have initializer
    vstore16((uint16) 0, 0, (__local uint*) uloc);
    vstore16((uint16) 0, 1, (__local uint*) uloc);

    // aligned to VPM row start
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[0], 0, (__local uint*) uloc);
    // unaligned with fixed vector offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[1], 1, (__local uint*) uloc);
    // unaligned with dynamic vector offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[2], offsets[2], (__local uint*) uloc);
    // unaligned with fixed element offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__local uint*) uloc) + 20);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 18, 19, 1a, 1b, 1c, 1d, 1e, 1f, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__local uint*) uloc) + offsets[4]);

    // aligned to VPM row start
    // -> [00, 01, 02, 03, 04, 05, 06, 07]
    out[0] = vload8(0, (__local uint*) uloc);
    // unaligned with fixed vector offset
    // -> [08, 09, 0a, 18, 19, 1a, 1b, 1c]
    out[1] = vload8(1, (__local uint*) uloc);
    // unaligned with dynamic vector offset
    // -> [1d, 1e, 1f, 13, 18, 19, 1a, 1b]
    out[2] = vload8(offsets[2], (__local uint*) uloc);
    // unaligned with fixed element offset (crossing row boundaries)
    // -> [0a, 18, 19, 1a, 1b, 1c, 1d, 1e]
    out[3] = vload8(0, ((__local uint*) uloc) + 10);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [18, 19, 1a, 1b, 1c, 1d, 1e, 1f]
    out[4] = vload8(0, ((__local uint*) uloc) + offsets[4]);
}

__kernel void test_local_parameter(
    __global uint8* out, const __global uint8* in, const __global uint* offsets, __local uint16 uloc[2])
{
    // XXX is not lowered into VPM (because offset of the 2 dynamic elements cannot be determined)
    // NOTE: offsets is [0, 1, 2, 3, 11]
    // NOTE: in is [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    // 19, 1a, 1b, 1c, 1d, 1e, 1f]

    // __local memory cannot have initializer
    vstore16((uint16) 0, 0, (__local uint*) uloc);
    vstore16((uint16) 0, 1, (__local uint*) uloc);

    // aligned to VPM row start
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[0], 0, (__local uint*) uloc);
    // unaligned with fixed vector offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[1], 1, (__local uint*) uloc);
    // unaligned with dynamic vector offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[2], offsets[2], (__local uint*) uloc);
    // unaligned with fixed element offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__local uint*) uloc) + 20);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 18, 19, 1a, 1b, 1c, 1d, 1e, 1f, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__local uint*) uloc) + offsets[4]);

    // aligned to VPM row start
    // -> [00, 01, 02, 03, 04, 05, 06, 07]
    out[0] = vload8(0, (__local uint*) uloc);
    // unaligned with fixed vector offset
    // -> [08, 09, 0a, 18, 19, 1a, 1b, 1c]
    out[1] = vload8(1, (__local uint*) uloc);
    // unaligned with dynamic vector offset
    // -> [1d, 1e, 1f, 13, 18, 19, 1a, 1b]
    out[2] = vload8(offsets[2], (__local uint*) uloc);
    // unaligned with fixed element offset (crossing row boundaries)
    // -> [0a, 18, 19, 1a, 1b, 1c, 1d, 1e]
    out[3] = vload8(0, ((__local uint*) uloc) + 10);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [18, 19, 1a, 1b, 1c, 1d, 1e, 1f]
    out[4] = vload8(0, ((__local uint*) uloc) + offsets[4]);
}

__kernel void test_global(
    __global uint8* out, const __global uint8* in, const __global uint* offsets, __global uint16 gloc[2])
{
    // NOTE: offsets is [0, 1, 2, 3, 11]
    // NOTE: in is [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    // 19, 1a, 1b, 1c, 1d, 1e, 1f]

    // aligned to VPM row start
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[0], 0, (__global uint*) gloc);
    // unaligned with fixed vector offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[1], 1, (__global uint*) gloc);
    // unaligned with dynamic vector offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 14, 15, 16, 17, 00, 00, 00,
    // 00, 00, 00, 00, 00]
    vstore8(in[2], offsets[2], (__global uint*) gloc);
    // unaligned with fixed element offset
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 0b, 0c, 0d, 0e, 0f, 10, 11, 12, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__global uint*) gloc) + 20);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 0a, 18, 19, 1a, 1b, 1c, 1d, 1e, 1f, 13, 18, 19, 1a, 1b, 1c, 1d, 1e,
    // 1f, 00, 00, 00, 00]
    vstore8(in[3], 0, ((__global uint*) gloc) + offsets[4]);

    // aligned to VPM row start
    // -> [00, 01, 02, 03, 04, 05, 06, 07]
    out[0] = vload8(0, (__global uint*) gloc);
    // unaligned with fixed vector offset
    // -> [08, 09, 0a, 18, 19, 1a, 1b, 1c]
    out[1] = vload8(1, (__global uint*) gloc);
    // unaligned with dynamic vector offset
    // -> [1d, 1e, 1f, 13, 18, 19, 1a, 1b]
    out[2] = vload8(offsets[2], (__global uint*) gloc);
    // unaligned with fixed element offset (crossing row boundaries)
    // -> [0a, 18, 19, 1a, 1b, 1c, 1d, 1e]
    out[3] = vload8(0, ((__global uint*) gloc) + 10);
    // unaligned with dynamic element offset (crossing row boundaries)
    // -> [18, 19, 1a, 1b, 1c, 1d, 1e, 1f]
    out[4] = vload8(0, ((__global uint*) gloc) + offsets[4]);
}