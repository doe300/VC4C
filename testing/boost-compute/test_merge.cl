#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)
typedef struct {
    int first;
    float second;
} _pair_int_float_t;

__kernel void serial_merge(uint size1, uint size2, __global _pair_int_float_t* _buf0, __global _pair_int_float_t* _buf1, __global _pair_int_float_t* _buf2)
{
uint i = 0;
uint j = 0;
uint k = 0;
_pair_int_float_t j_value = _buf0[0];
_pair_int_float_t k_value = _buf1[0];
while(j < size1 && k < size2){
    if((j_value.first)<(k_value.first)){
        _buf2[i++] = j_value;
        j_value = _buf0[++j];
    }
    else{
        _buf2[i++] = k_value;
        k_value = _buf1[++k];
    }
}
while(j < size1){
_buf2[i++] = _buf0[j++];
}
while(k < size2){
_buf2[i++] = _buf1[k++];
}

}
