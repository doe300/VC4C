//Taken from:
// https://github.com/elorimer/rpi-playground/blob/master/QPU/SHA-256/reference/sha256.cpp

#define NUM_QPUS        1
#define BUFFER_SIZE     NUM_QPUS * 16

__constant uint K[] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
    0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
    0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
};

inline uint CH(uint x, uint y, uint z) {
    return (x & y) ^ (~x & z);
}

inline uint Maj(uint x, uint y, uint z) {
    return (x & y) ^ (x & z) ^ (y & z);
}

inline uint RotR(uint x, uchar shift) {
    return (x >> shift) | (x << (32-shift));
}

inline uint sigma0(uint x) {
    return RotR(x, 2) ^ RotR(x, 13) ^ RotR(x, 22);
}

inline uint sigma1(uint x) {
    return RotR(x, 6) ^ RotR(x, 11) ^ RotR(x, 25);
}

inline uint smsigma0(uint x) {
    return RotR(x, 7) ^ RotR(x, 18) ^ (x >> 3);
}

inline uint smsigma1(uint x) {
    return RotR(x, 17) ^ RotR(x, 19) ^ (x >> 10);
}

#define ENDIAN(x, i)        ((x[i*4] << 24) | (x[i*4+1] << 16) | (x[i*4+2] << 8) | (x[i*4+3]))

/*
 * data is an array of BUFFER_SIZE buffers to hash
 * H is an input/output parameter
 * stride is the stride for data (TODO: handle hashes of more than one block)
 */
__kernel void execute_sha256_cpu(__global const uint *data, __global uint *H, int stride)
{
    uint W[64];
    uint a, b, c, d, e, f, g, h;

    for (int k=0; k < BUFFER_SIZE; k++)
    {
        for (int i=0; i < 16; i++)
            W[i] = data[k*stride+i];
        for (int i=16; i < 64; i++)
            W[i] = smsigma1(W[i-2]) + W[i-7] + smsigma0(W[i-15]) + W[i-16];

        a = H[k*8+0];
        b = H[k*8+1];
        c = H[k*8+2];
        d = H[k*8+3];
        e = H[k*8+4];
        f = H[k*8+5];
        g = H[k*8+6];
        h = H[k*8+7];

        for (int i=0; i < 64; i++)
        {
            uint T1 = h + sigma1(e) + CH(e,f,g) + K[i] + W[i];
            uint T2 = sigma0(a) + Maj(a,b,c);
            h = g;
            g = f;
            f = e;
            e = d + T1;
            d = c;
            c = b;
            b = a;
            a = T1 + T2;
        }

        H[k*8+0] += a;
        H[k*8+1] += b;
        H[k*8+2] += c;
        H[k*8+3] += d;
        H[k*8+4] += e;
        H[k*8+5] += f;
        H[k*8+6] += g;
        H[k*8+7] += h;
    }
}

// Below is the above original version partially optimized by me:
__attribute__((reqd_work_group_size(1,1,1)))
__kernel void execute_sha256_gpu(__global const uint *data, __global uint8 *H, int stride)
{
    uint W[64];
    uint a, b, c, d, e, f, g, h;

    for (int k=0; k < BUFFER_SIZE; k++)
    {
        for (int i=0; i < 16; i++)
            W[i] = data[k*stride+i];
        for (int i=16; i < 64; i++)
            W[i] = smsigma1(W[i-2]) + W[i-7] + smsigma0(W[i-15]) + W[i-16];

        uint8 tmp = H[k];

        a = tmp.s0;
        b = tmp.s1;
        c = tmp.s2;
        d = tmp.s3;
        e = tmp.s4;
        f = tmp.s5;
        g = tmp.s6;
        h = tmp.s7;

        for (int i=0; i < 64; i++)
        {
            uint T1 = h + sigma1(e) + CH(e,f,g) + K[i] + W[i];
            uint T2 = sigma0(a) + Maj(a,b,c);
            h = g;
            g = f;
            f = e;
            e = d + T1;
            d = c;
            c = b;
            b = a;
            a = T1 + T2;
        }

#ifdef EMULATE_CPU
        H[k] += uint8{a, b, c, d, e, f, g, h};
#else
        H[k] += (uint8)(a, b, c, d, e, f, g, h);
#endif
    }
}
