#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)
typedef struct __attribute__((packed)) {
    int a;
    int b;
    float c;
} UDD;

inline bool compare_UDD_device(UDD lhs, UDD rhs){ return lhs.a < rhs.a; }


__kernel void serial_insertion_sort(__local UDD* data, uint n, __global UDD* _buf0)
{
for(uint i = 0; i < n; i++){
    data[i] = _buf0[i];
}
for(uint i = 1; i < n; i++){
    const UDD value = data[i];
    uint pos = i;
    while(pos > 0 && compare_UDD_device(value, data[pos-1])){
        data[pos] = data[pos-1];
        pos--;
    }
    data[pos] = value;
}
for(uint i = 0; i < n; i++){
    _buf0[i] = data[i];
}

}

