//For discussion, see https://github.com/doe300/VC4C/issues/51
__kernel void loop1 (__global float * a) {
  int id = get_local_id(0);
  a[id] = a[id] * 2;
}
