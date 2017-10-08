__kernel void taskParallelIntegrateVelocity(__global float3* positions,
                                            __global float3* velocities,
                                            const float dt) {
    int id = get_global_id(0);

    positions[id].x = positions[id].x + dt * velocities[id].x;
    positions[id].y = positions[id].y + dt * velocities[id].y;
    positions[id].z = positions[id].z + dt * velocities[id].z;
}


__kernel void taskParallelCalculateDensity(__global float* densities,
                                           __global float3* positions,
                                           const float mass,
                                           const float kernelSize,
                                           const int howMany) {
    int id = get_global_id(0);
    float3 r;
    float pi = 3.14;

    for(int i = 0; i < howMany; i++){
        r.x = positions[id].x - positions[i].x;
        r.y = positions[id].y - positions[i].y;
        r.z = positions[id].z - positions[i].z;

        float radius = sqrt(r.x*r.x + r.y*r.y + r.z*r.z);

        if( radius < kernelSize){
            densities[id] = densities[id] + mass*(315/(64*pi*pown(h, 9))) * pown(kernelSize * kernelSize - radius * radius, 3);
        }
    }
}

__kernel void taskParallelCalculatePressureForce(__global float3* pressureForces,
                                           __global float3* positions,
                                           __global float* densities,
                                           const float kernelSize,
                                           const int howMany,
                                           const float mass,
                                           const float restDensity,
                                           const float gasConstant) {
    int id = get_global_id(0);
    float3 r;
    float p_i;
    float p_id;
    float pi = 3.14;

    p_id = gasConstant * ( densities[id] - restDensity );

    for(int i = 0; i < howMany; i++){
        p_i = gasConstant * ( densities[i] - restDensity );

        r.x = positions[id].x - positions[i].x;
        r.y = positions[id].y - positions[i].y;
        r.z = positions[id].z - positions[i].z;

        float radius = sqrt(r.x*r.x + r.y*r.y + r.z*r.z);

        if( radius < kernelSize){
            pressureForces[id].x = pressureForces[id].x + mass * ( (p_i + p_id)/(2*density[i]) ) * (15 / (pi * pown(kernelSize, 6))) * 3 * pown(kernelSize - radius, 2) * (- r[id].x / radius);
            pressureForces[id].y = pressureForces[id].y + mass * ( (p_i + p_id)/(2*density[i]) ) * (15 / (pi * pown(kernelSize, 6))) * 3 * pown(kernelSize - radius, 2) * (- r[id].y / radius);
            pressureForces[id].z = pressureForces[id].z + mass * ( (p_i + p_id)/(2*density[i]) ) * (15 / (pi * pown(kernelSize, 6))) * 3 * pown(kernelSize - radius, 2) * (- r[id].z / radius);
        }
    }
}

__kernel void taskParallelCalculateViscosityForce(__global float3* viscosityForces,
                                           __global float3* positions,
                                           __global float3* velocities,
                                           __global float* densities,
                                           const float kernelSize,
                                           const int howMany,
                                           const float mass,
                                           const float viscosityConstant) {
    int id = get_global_id(0);
    float3 r;
    float pi = 3.14;

    for(int i = 0; i < howMany; i++){
        r.x = positions[id].x - positions[i].x;
        r.y = positions[id].y - positions[i].y;
        r.z = positions[id].z - positions[i].z;

        float radius = sqrt(r.x*r.x + r.y*r.y + r.z*r.z);

        if( radius < kernelSize ){
            viscosityForces[id].x = viscosityForces[id].x + mass * ( (velocities[i].x - velocities[id].x) / densities[i] ) * (45 / (pi * pown(kernelSize, 6))) * (kernelSize - radius);
            viscosityForces[id].y = viscosityForces[id].y + mass * ( (velocities[i].y - velocities[id].y) / densities[i] ) * (45 / (pi * pown(kernelSize, 6))) * (kernelSize - radius);
            viscosityForces[id].z = viscosityForces[id].z + mass * ( (velocities[i].z - velocities[id].z) / densities[i] ) * (45 / (pi * pown(kernelSize, 6))) * (kernelSize - radius);
        }
    }
}

__kernel void taskParallelCalculateTensionForce(__global float3* tensionForces,
                                           __global float3* positions,
                                           __global float3* velocities,
                                           __global float* densities,
                                           const float kernelSize,
                                           const int howMany,
                                           const float mass,
                                           const float nThreshold,
                                           const float sigma,
                                           const float viscosityConstant) {
    int id = get_global_id(0);
    float3 r;
    float p_i;
    float p_id;
    float pi = 3.14;
    float cs;
    float3 n;
    float laplacianCs;
    float k;
    float norm_n;

    for(int i = 0; i < howMany; i++){

        r.x = positions[id].x - positions[i].x;
        r.y = positions[id].y - positions[i].y;
        r.z = positions[id].z - positions[i].z;

        float radius = sqrt(r.x*r.x + r.y*r.y + r.z*r.z);

        if( radius < kernelSize ){
            cs = cs + mass * (1 / densities[i]) * (315/(64*pi*pown(h, 9))) * pown(kernelSize * kernelSize - radius * radius, 3);

            n.x = n + mass * (1 / densities[i]) * (-315/(64*pi*pown(kernelSize,9))) * 6 * pown(kernelSize * kernelSize - radius * radius, 2) * r.x;
            n.y = n + mass * (1 / densities[i]) * (-315/(64*pi*pown(kernelSize,9))) * 6 * pown(kernelSize * kernelSize - radius * radius, 2) * r.y;
            n.z = n + mass * (1 / densities[i]) * (-315/(64*pi*pown(kernelSize,9))) * 6 * pown(kernelSize * kernelSize - radius * radius, 2) * r.z;

            laplacianCs = laplacianCs + mass * (1 / densities[i]) * (315/(64*pi*pown(kernelSize,9))) * (24 * radius * radius * (kernelSize * kernelSize - radius * radius) - 6 * pown(kernelSize * kernelSize - radius * radius, 2));

            norm_n = sqrt(n.x*n.x + n.y*n.y + n.z*n.z);
            if(norm_n < nThreshold){
                tensionForce[id].x = 0;
                tensionForce[id].y = 0;
                tensionForce[id].z = 0;
            } else {
                k = -laplacianCs / norm_n;
                tensionForce[id].x = sigma * k * n.x;
                tensionForce[id].y = sigma * k * n.y;
                tensionForce[id].z = sigma * k * n.z;
            }

        }
    }
}

__kernel void taskParallelCalculateForce(__gloval float3* forces,
                                         __global float3* pressureForce,
                                         __global float3* viscosityForce,
                                         __global float3* tensionForces,
                                         const float3 externalForce)
                                         {
    int id = get_global_id(0);

    forces[id].x = pressureForce[id].x + viscosityForce[id].x + tensionForces[id].x + externalForce.x;
    forces[id].y = pressureForce[id].y + viscosityForce[id].y + tensionForces[id].y + externalForce.y;
    forces[id].z = pressureForce[id].z + viscosityForce[id].z + tensionForces[id].z + externalForce.z;
}
