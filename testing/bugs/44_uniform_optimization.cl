//taken from https://github.com/doe300/VC4C/issues/44 (by @nomaddo)
__kernel void loop0 (__global float * a) {
  int id = get_local_id(0);
  float16 v = vload16 (id, a);
  vstore16(v * 2, id, a);
}

__kernel void loop1 (__global float * a){
   int id = get_local_id(0);
   a[id] = a[id] * 2;
}
