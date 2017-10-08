__kernel void serial_adjacent_find(const uint size, __global uint* output, __global short* _buf0)
{
uint result = size;
for(uint i = 0; i < size - 1; i++){
    if(((_buf0[i+1])<(_buf0[i]))){
        result = i;
        break;
    }
}
*output = result;
}


__kernel void serial_insertion_sort(__local short* data, uint n, __global short* _buf0)
{
for(uint i = 0; i < n; i++){
    data[i] = _buf0[i];
}
for(uint i = 1; i < n; i++){
    const short value = data[i];
    uint pos = i;
    while(pos > 0 && ((value)<(data[pos-1]))){
        data[pos] = data[pos-1];
        pos--;
    }
    data[pos] = value;
}
for(uint i = 0; i < n; i++){
    _buf0[i] = data[i];
}
}
