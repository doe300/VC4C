__kernel void copy(__global int* _buf0, const uint count)
{
uint index = get_local_id(0) + (32 * get_group_id(0));
for(uint i = 0; i < 4; i++){
    if(index < count){
_buf0[index]=(1+index);
       index += 8;
    }
}
}

//int_min_max
#define boost_pair_type(t1, t2) _pair_ ## t1 ## _ ## t2 ## _t
#define boost_pair_get(x, n) (n == 0 ? x.first ## x.second)
#define boost_make_pair(t1, x, t2, y) (boost_pair_type(t1, t2)) { x, y }
#define boost_tuple_get(x, n) (x.v ## n)
//manual test correct
__kernel void find_extrema_min(__global int* _buf0, __global uint* index)
{
const uint gid = get_global_id(0);
uint old_index = *index;
int old = _buf0[old_index];
int new = _buf0[gid];
bool compare_result;
#ifdef BOOST_COMPUTE_FIND_MAXIMUM
while((compare_result = ((old)<(new))) || (!(compare_result || ((new)<(old))) && gid < old_index)){
#else
while((compare_result = ((new)<(old))) || (!(compare_result || ((old)<(new))) && gid < old_index)){
#endif
  if(atomic_cmpxchg(index, old_index, gid) == old_index)
      break;
  else
    old_index = *index;
old = _buf0[old_index];
}
}


#define BOOST_COMPUTE_FIND_MAXIMUM
//manual test correct
__kernel void find_extrema_max(__global int* _buf0, __global uint* index)
{
const uint gid = get_global_id(0);
uint old_index = *index;
int old = _buf0[old_index];
int new = _buf0[gid];
bool compare_result;
#ifdef BOOST_COMPUTE_FIND_MAXIMUM
while((compare_result = ((old)<(new))) || (!(compare_result || ((new)<(old))) && gid < old_index)){
#else
while((compare_result = ((new)<(old))) || (!(compare_result || ((old)<(new))) && gid < old_index)){
#endif
  if(atomic_cmpxchg(index, old_index, gid) == old_index)
      break;
  else
    old_index = *index;
old = _buf0[old_index];
}
}
#undef BOOST_COMPUTE_FIND_MAXIMUM

__kernel void find_extrema_on_cpu_min(uint count, __global int* output, __global uint* output_idx, __global int* _buf0)
{
uint block = (uint)ceil(((float)count)/get_global_size(0));
uint index = get_global_id(0) * block;
uint end = min(count, index + block);
uint value_index = index;
int value = _buf0[index];
index++;
while(index < end){
int candidate = _buf0[index];
#ifndef BOOST_COMPUTE_FIND_MAXIMUM
bool compare = ((candidate)<(value));
#else
bool compare = ((value)<(candidate));
#endif
value = compare ? candidate : value;
value_index = compare ? index : value_index;
index++;
}
output[get_global_id(0)] = value;
output_idx[get_global_id(0)] = value_index;
}

__kernel void serial_find_extrema_min(__global int* _buf0, __global uint* index, uint size)
{
int value = _buf0[0];
uint value_index = 0;
for(uint i = 1; i < size; i++){
  int candidate=_buf0[i];
#ifndef BOOST_COMPUTE_FIND_MAXIMUM
  if(((candidate)<(value))){
#else
  if(((value)<(candidate))){
#endif
    value = candidate;
    value_index = i;
  }
}
*index = value_index;
}

#define BOOST_COMPUTE_FIND_MAXIMUM
__kernel void find_extrema_on_cpu_max(uint count, __global int* output, __global uint* output_idx, __global int* _buf0)
{
uint block = (uint)ceil(((float)count)/get_global_size(0));
uint index = get_global_id(0) * block;
uint end = min(count, index + block);
uint value_index = index;
int value = _buf0[index];
index++;
while(index < end){
int candidate = _buf0[index];
#ifndef BOOST_COMPUTE_FIND_MAXIMUM
bool compare = ((candidate)<(value));
#else
bool compare = ((value)<(candidate));
#endif
value = compare ? candidate : value;
value_index = compare ? index : value_index;
index++;
}
output[get_global_id(0)] = value;
output_idx[get_global_id(0)] = value_index;
}

//TODO this one causes result mismatch (or the previous one?!)
__kernel void serial_find_extrema_max(__global int* _buf0, __global uint* index, uint size)
{
int value = _buf0[0];
uint value_index = 0;
for(uint i = 1; i < size; i++){
  int candidate=_buf0[i];
#ifndef BOOST_COMPUTE_FIND_MAXIMUM
  if(((candidate)<(value))){
#else
  if(((value)<(candidate))){
#endif
    value = candidate;
    value_index = i;
  }
}
*index = value_index;
}
#undef BOOST_COMPUTE_FIND_MAXIMUM


//TODO this here hangs execution
__kernel void find_extrema_reduce_min(uint count, __local int* block, __local uint* block_idx, __global uint* _buf0, __global int* _buf1, __global int* _buf2)
{
const uint gid = get_global_id(0);
uint idx = gid;
int acc;
uint acc_idx;
if(gid < count) {
#ifdef BOOST_COMPUTE_USE_INPUT_IDX
acc_idx = _buf0[idx];
#else
acc_idx = idx;
#endif
acc = _buf1[idx];
idx += get_global_size(0);
}
bool compare_result;
bool equal;
while( idx < count ){
int next = _buf1[idx];
#ifdef BOOST_COMPUTE_USE_INPUT_IDX
uint next_idx = _buf0[idx];
#endif
#ifdef BOOST_COMPUTE_FIND_MAXIMUM
compare_result = ((next)<(acc));
# ifdef BOOST_COMPUTE_USE_INPUT_IDX
equal = !compare_result && !((acc)<(next));
# endif
#else
compare_result = ((acc)<(next));
# ifdef BOOST_COMPUTE_USE_INPUT_IDX
equal = !compare_result && !((next)<(acc));
# endif
#endif
acc = compare_result ? acc : next;
#ifdef BOOST_COMPUTE_USE_INPUT_IDX
acc_idx = compare_result ? acc_idx : (equal ? min(acc_idx, next_idx) : next_idx);
#else
acc_idx = compare_result ? acc_idx : idx;
#endif
idx += get_global_size(0);
}
const uint lid = get_local_id(0);
block[lid] = acc;
block_idx[lid] = acc_idx;
barrier(CLK_LOCAL_MEM_FENCE);
uint group_offset = count - (get_local_size(0) * get_group_id(0));
#pragma unroll
for(uint offset = 12 / 2; offset > 0; offset = offset / 2) {
if((lid < offset) && ((lid + offset) < group_offset)) { 
int mine = block[lid];
int other = block[lid+offset];
#ifdef BOOST_COMPUTE_FIND_MAXIMUM
compare_result = ((other)<(mine));
equal = !compare_result && !((mine)<(other));
#else
compare_result = ((mine)<(other));
equal = !compare_result && !((other)<(mine));
#endif
block[lid] = compare_result ? mine : other;
uint mine_idx = block_idx[lid];
uint other_idx = block_idx[lid+offset];
block_idx[lid] = compare_result ? mine_idx : (equal ? min(mine_idx, other_idx) : other_idx);
}
barrier(CLK_LOCAL_MEM_FENCE);
}
if(lid == 0){
_buf2[get_group_id(0)] = block[0];
_buf0[get_group_id(0)] = block_idx[0];
}
}

#define BOOST_COMPUTE_FIND_MAXIMUM
//TODO this here hangs execution (or maybe hangs because blocked by previous kernel)
__kernel void find_extrema_reduce_max(uint count, __local int* block, __local uint* block_idx, __global uint* _buf0, __global int* _buf1, __global int* _buf2)
{
const uint gid = get_global_id(0);
uint idx = gid;
int acc;
uint acc_idx;
if(gid < count) {
#ifdef BOOST_COMPUTE_USE_INPUT_IDX
acc_idx = _buf0[idx];
#else
acc_idx = idx;
#endif
acc = _buf1[idx];
idx += get_global_size(0);
}
bool compare_result;
bool equal;
while( idx < count ){
int next = _buf1[idx];
#ifdef BOOST_COMPUTE_USE_INPUT_IDX
uint next_idx = _buf0[idx];
#endif
#ifdef BOOST_COMPUTE_FIND_MAXIMUM
compare_result = ((next)<(acc));
# ifdef BOOST_COMPUTE_USE_INPUT_IDX
equal = !compare_result && !((acc)<(next));
# endif
#else
compare_result = ((acc)<(next));
# ifdef BOOST_COMPUTE_USE_INPUT_IDX
equal = !compare_result && !((next)<(acc));
# endif
#endif
acc = compare_result ? acc : next;
#ifdef BOOST_COMPUTE_USE_INPUT_IDX
acc_idx = compare_result ? acc_idx : (equal ? min(acc_idx, next_idx) : next_idx);
#else
acc_idx = compare_result ? acc_idx : idx;
#endif
idx += get_global_size(0);
}
const uint lid = get_local_id(0);
block[lid] = acc;
block_idx[lid] = acc_idx;
barrier(CLK_LOCAL_MEM_FENCE);
uint group_offset = count - (get_local_size(0) * get_group_id(0));
#pragma unroll
for(uint offset = 12 / 2; offset > 0; offset = offset / 2) {
if((lid < offset) && ((lid + offset) < group_offset)) { 
int mine = block[lid];
int other = block[lid+offset];
#ifdef BOOST_COMPUTE_FIND_MAXIMUM
compare_result = ((other)<(mine));
equal = !compare_result && !((mine)<(other));
#else
compare_result = ((mine)<(other));
equal = !compare_result && !((other)<(mine));
#endif
block[lid] = compare_result ? mine : other;
uint mine_idx = block_idx[lid];
uint other_idx = block_idx[lid+offset];
block_idx[lid] = compare_result ? mine_idx : (equal ? min(mine_idx, other_idx) : other_idx);
}
barrier(CLK_LOCAL_MEM_FENCE);
}
if(lid == 0){
_buf2[get_group_id(0)] = block[0];
_buf0[get_group_id(0)] = block_idx[0];
}
}
#undef BOOST_COMPUTE_FIND_MAXIMUM
