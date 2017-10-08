__kernel void iota_1(__global int* _buf0, const uint count)
{
uint index = get_local_id(0) + (32 * get_group_id(0));
for(uint i = 0; i < 4; i++){
    if(index < count){
_buf0[2+(index)]=(-5+index);
       index += 8;
    }
}
}

__kernel void iota_2(__global int* _buf0, const uint count)
{
uint index = get_local_id(0) + (32 * get_group_id(0));
for(uint i = 0; i < 4; i++){
    if(index < count){
_buf0[index]=(4+index);
       index += 8;
    }
}
}
