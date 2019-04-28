/*
 * Adapted from:
 * https://en.wikipedia.org/wiki/MD5#Algorithm
 */

// Note: All variables are unsigned 32 bit and wrap modulo 2^32 when calculating
__constant unsigned K[64] = {0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613,
    0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8, 0x21e1cde6,
    0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681,
    0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa, 0xd4ef3085,
    0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665, 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
    0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82,
    0xbd3af235, 0x2ad7d2bb, 0xeb86d391};
__constant unsigned s[64] = {7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5,
    9, 14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15, 21, 6, 10, 15, 21,
    6, 10, 15, 21, 6, 10, 15, 21};

// leftrotate function definition
unsigned leftrotate(unsigned x, unsigned c)
{
    return (x << c) | (x >> (32 - c));
}

__kernel void calculate_md5(__global char* msg, unsigned length, __global char* digest)
{
    unsigned origLength = length;
    __private unsigned M[16];

    // Initialize variables:
    unsigned a0 = 0x67452301; // A
    unsigned b0 = 0xefcdab89; // B
    unsigned c0 = 0x98badcfe; // C
    unsigned d0 = 0x10325476; // D

    // TODO preprocessing is on bits!!

    // Pre-processing: adding a single 1 bit
    // "append "1" bit to message"
    // Notice: the input bytes are considered as bits strings,
    //  where the first bit is the most significant bit of the byte.

    // Pre-processing: padding with zeros
    // "append "0" bit until message length in bits ≡ 448 (mod 512)"
    // "append original length in bits mod 2^64 to message"

    // Process the message in successive 512-bit chunks:
    // "for each 512-bit chunk of padded message"
    for(unsigned part = 0; part < length / sizeof(unsigned); part += 16)
    {
        // "break chunk into sixteen 32-bit words M[j], 0 ≤ j ≤ 15"
        M[0] = ((__global unsigned*) msg)[part];
        M[1] = ((__global unsigned*) msg)[part + 1];
        M[2] = ((__global unsigned*) msg)[part + 2];
        M[3] = ((__global unsigned*) msg)[part + 3];
        M[4] = ((__global unsigned*) msg)[part + 4];
        M[5] = ((__global unsigned*) msg)[part + 5];
        M[6] = ((__global unsigned*) msg)[part + 6];
        M[7] = ((__global unsigned*) msg)[part + 7];
        M[8] = ((__global unsigned*) msg)[part + 8];
        M[9] = ((__global unsigned*) msg)[part + 9];
        M[10] = ((__global unsigned*) msg)[part + 10];
        M[11] = ((__global unsigned*) msg)[part + 11];
        M[12] = ((__global unsigned*) msg)[part + 12];
        M[13] = ((__global unsigned*) msg)[part + 13];
        M[14] = ((__global unsigned*) msg)[part + 14];
        M[15] = ((__global unsigned*) msg)[part + 15];

        // Initialize hash value for this chunk:
        unsigned A = a0;
        unsigned B = b0;
        unsigned C = c0;
        unsigned D = d0;

        // Main loop:
        // XXX works so far with this loop disabled
        // TODO passes for i = 0, 16, 32 and 48 bu not for full loop
        /* for(unsigned i = 0; i < 64; ++i)
        {
            unsigned F, g;
            if(i < 16)
            {
                F = (B & C) | ((~B) & D);
                g = i;
            }
            else if(i < 32)
            {
                F = (D & B) | ((~D) & C);
                g = (5 * i + 1) % 16;
            }
            else if(i < 48)
            {
                F = B ^ C ^ D;
                g = (3 * i + 5) % 16;
            }
            else
            {
                F = C ^ (B | (~D));
                g = (7 * i) % 16;
            }
            // Be wary of the below definitions of a,b,c,d
            F = F + A + K[i] + M[g];
            A = D;
            D = C;
            C = B;
            B = B + leftrotate(F, s[i]);
        }*/

        // Add this chunk's hash to result so far:
        a0 = a0 + A;
        b0 = b0 + B;
        c0 = c0 + C;
        d0 = d0 + D;
    }

    ((__global unsigned*) digest)[0] = a0;
    ((__global unsigned*) digest)[1] = b0;
    ((__global unsigned*) digest)[2] = c0;
    ((__global unsigned*) digest)[3] = d0;
}