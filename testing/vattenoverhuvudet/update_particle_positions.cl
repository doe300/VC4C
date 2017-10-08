/* Struct containing a fluid particle
Total memory size = 12 + 12 + 4 + 12 + 4 = 44 bytes */
struct __attribute__ ((packed)) Particle {
    float3 position;    // 12 bytes
    float3 velocity;    // 12 bytes

    float density;      // 4 bytes

    float3 force;       // 12 bytes
    float color_field;  // 4 bytes
};

__kernel void taskParallelIntegrateVelocity(__global float3* positions,
                                            __global float3* velocities,
                                            const float dt) {
    const int id = get_global_id(0);

    positions[id].x = positions[id].x + dt * velocities[id].x;
    positions[id].y = positions[id].y + dt * velocities[id].y;
    positions[id].z = positions[id].z + dt * velocities[id].z;
}
