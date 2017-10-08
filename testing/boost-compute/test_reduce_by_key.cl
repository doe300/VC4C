#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)


__kernel void serial_reduce_by_key(uint count, __global uint* result_size, __global int* _buf0, __global int* _buf1, __global int* _buf2, __global int* _buf3)
{
int result = _buf0[0];
int previous_key = _buf1[0];
int value;
int key;
uint size = 1;
_buf2[0] = previous_key;
_buf3[0] = result;
for(ulong i = 1; i < count; i++) {
    value = _buf0[i];
    key = _buf1[i];
    if (((previous_key)==(key))) {
        result = ((result)+(value));
    }
     else { 
_buf2[size - 1] = previous_key;
_buf3[size - 1] = result;
        result = value;
        size++;
    } 
    previous_key = key;
}
_buf2[size - 1] = previous_key;
_buf3[size - 1] = result;
*result_size = size;
}

