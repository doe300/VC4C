#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)

inline float triangle_area(uint4 i, __global float4* triangle_vertices){ const float4 a = triangle_vertices[i.x]; const float4 b = triangle_vertices[i.y]; const float4 c = triangle_vertices[i.z]; return length(cross(b-a, c-a)) / 2; }


__kernel void copy(__global float* _buf0, __global uint4* _buf1, __global float4* _buf2, const uint count)
{
uint index = get_local_id(0) + (512 * get_group_id(0));
for(uint i = 0; i < 4; i++){
    if(index < count){
_buf0[index]=triangle_area(_buf1[index], _buf2);
       index += 128;
    }
}

}

