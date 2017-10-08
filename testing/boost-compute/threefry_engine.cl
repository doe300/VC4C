#define THREEFRY2x32_DEFAULT_ROUNDS 20
#define SKEIN_KS_PARITY_32 0x1BD11BDA

enum r123_enum_threefry32x2 {
    R_32x2_0_0=13,
    R_32x2_1_0=15,
    R_32x2_2_0=26,
    R_32x2_3_0= 6,
    R_32x2_4_0=17,
    R_32x2_5_0=29,
    R_32x2_6_0=16,
    R_32x2_7_0=24
};

static uint RotL_32(uint x, uint N)
{
    return (x << (N & 31)) | (x >> ((32-N) & 31));
}

struct r123array2x32 {
    uint v[2];
};

typedef struct r123array2x32 threefry2x32_ctr_t;
typedef struct r123array2x32 threefry2x32_key_t;

threefry2x32_ctr_t threefry2x32_R(unsigned int Nrounds, threefry2x32_ctr_t in, threefry2x32_key_t k)\
{
    threefry2x32_ctr_t X;
    uint ks[3];
    uint  i; 
    ks[2] =  SKEIN_KS_PARITY_32;
    for (i=0;i < 2; i++) {
	ks[i] = k.v[i];
        X.v[i]  = in.v[i];
        ks[2] ^= k.v[i];
    }
    X.v[0] += ks[0]; X.v[1] += ks[1];
    if(Nrounds>0){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_0_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>1){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_1_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>2){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_2_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>3){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_3_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>3){
        X.v[0] += ks[1]; X.v[1] += ks[2];
        X.v[1] += 1;
    }
    if(Nrounds>4){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_4_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>5){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_5_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>6){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_6_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>7){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_7_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>7){
        X.v[0] += ks[2]; X.v[1] += ks[0];
        X.v[1] += 2;
    }
    if(Nrounds>8){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_0_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>9){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_1_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>10){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_2_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>11){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_3_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>11){
        X.v[0] += ks[0]; X.v[1] += ks[1];
        X.v[1] += 3;
    }
    if(Nrounds>12){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_4_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>13){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_5_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>14){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_6_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>15){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_7_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>15){
        X.v[0] += ks[1]; X.v[1] += ks[2];
        X.v[1] += 4;
    }
    if(Nrounds>16){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_0_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>17){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_1_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>18){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_2_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>19){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_3_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>19){
        X.v[0] += ks[2]; X.v[1] += ks[0];
        X.v[1] += 5;
    }
    if(Nrounds>20){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_4_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>21){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_5_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>22){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_6_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>23){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_7_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>23){
        X.v[0] += ks[0]; X.v[1] += ks[1];
        X.v[1] += 6;
    }
    if(Nrounds>24){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_0_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>25){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_1_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>26){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_2_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>27){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_3_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>27){
        X.v[0] += ks[1]; X.v[1] += ks[2];
        X.v[1] += 7;
    }
    if(Nrounds>28){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_4_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>29){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_5_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>30){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_6_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>31){  X.v[0] += X.v[1]; X.v[1] = RotL_32(X.v[1],R_32x2_7_0); X.v[1] ^= X.v[0]; }
    if(Nrounds>31){
        X.v[0] += ks[2]; X.v[1] += ks[0];
        X.v[1] += 8;
    }
    return X;
}

__kernel void generate_rng(__global uint *ctr, __global uint *key, const uint offset) {
    threefry2x32_ctr_t in;
    threefry2x32_key_t k;
    const uint i = get_global_id(0);
    in.v[0] = ctr[2 * (offset + i)];
    in.v[1] = ctr[2 * (offset + i) + 1];
    k.v[0] = key[2 * (offset + i)];
    k.v[1] = key[2 * (offset + i) + 1];
    in = threefry2x32_R(20, in, k);
    ctr[2 * (offset + i)] = in.v[0];
    ctr[2 * (offset + i) + 1] = in.v[1];
}
