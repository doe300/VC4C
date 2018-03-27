/*
 * Tests RemoveConstantLoadInLoops optimization
 */
__kernel void test_remove_constant_load_in_loops_opt(global float a[], global float b[]) {
    for (int j = 0; j < 200; j++) {
        for (int i = 0; i < 200; i++) {
            a[i + j * 200] = i * j;
        }
    }

    for (int i = 0; i < 40000; i++) {
        b[i] = i;
    }
}
