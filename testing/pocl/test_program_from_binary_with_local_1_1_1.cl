__kernel void test(__global unsigned * restrict buffer,
                   __local unsigned * restrict local_input,
                   const unsigned vec_size)
{
  unsigned i, j;
  size_t gid = get_global_id(0);
  size_t lid = get_local_id(0);
  size_t lsize = get_local_size(0);
  event_t event_read, event_write;
  /* FIXME this is UB, since the arguments for the work-items in a work-group are not exactly the same! */
  event_read = async_work_group_copy(local_input, &buffer[gid*vec_size*lsize], vec_size*lsize, 0);
  for (i=0; i<vec_size; i++)
    {
      if (i == 0)
        wait_group_events(1, &event_read);
      local_input[i*lsize+lid]++;
    }
  event_write = async_work_group_copy(&buffer[gid*vec_size*lsize], local_input, vec_size*lsize, event_write);
  wait_group_events(1, &event_write);
}   
