#ifndef TYPE
#define TYPE int
#endif
/*
__kernel void test_atomic_add(volatile __global TYPE* destMemory, __global TYPE* oldValues)
{
    int tid = get_global_id(0);

    oldValues[tid] = atom_add(&destMemory[0], tid + 3);
    atom_add(&destMemory[0], tid + 3);
    atom_add(&destMemory[0], tid + 3);
    atom_add(&destMemory[0], tid + 3);
}

__kernel void test_atomic_sub(volatile __global TYPE* destMemory, __global TYPE* oldValues)
{
    int tid = get_global_id(0);

    oldValues[tid] = atom_sub(&destMemory[0], tid + 3);
}
*/
__kernel void test_atomic_cmpxchg(volatile __global TYPE* destMemory, __global TYPE* oldValues)
{
    int tid = get_global_id(0);

    int oldValue, origValue, newValue;
    do
    {
        origValue = destMemory[0];
        newValue = origValue + tid + 2;
        oldValue = atom_cmpxchg(&destMemory[0], origValue, newValue);
    } while(oldValue != origValue);
    oldValues[tid] = oldValue;
}