#pragma OPENCL EXTENSION cl_khr_global_int32_base_atomics : enable

//typedef struct __attribute__ ((packed)) def_ParticleData {
typedef struct def_ParticleData {
    float density;      // 4 bytes

    float3 force;       // 12 bytes
    float color_field;  // 4 bytes
} ParticleData;

//typedef struct __attribute__ ((packed)) def_VoxelGridInfo {
typedef struct def_VoxelGridInfo {
	// How many grid cells there are in each dimension (i.e. [x=8 y=8 z=10])
	uint3 grid_dimensions;

	// How many grid cells there are in total
	uint total_grid_cells;

	// The size (x/y/z) of each cell
	float grid_cell_size;

	// The bottom-most corner of the grid, where the grid cell [0 0 0] starts
	float3 grid_origin;

	uint max_cell_particle_count;
} VoxelGridInfo;

// Map a particle index inside a voxel cell to its global buffer index
uint get_particle_buffer_index(const uint voxel_cell_index, 
							   const uint voxel_particle_index, 
					 		   const uint max_cell_particle_count,
					 		   __global const uint* restrict indices);

// Get the position of a particle based on its cell index and particle index inside the given cell
float3 get_particle_velocity(const uint voxel_cell_index, 
							 const uint voxel_particle_index, 
					 		 const uint max_cell_particle_count,
					 		 __global const uint* restrict indices, 
					 		 __global const float* restrict velocities);

__kernel void simple_voxel_grid_move(__global float *positions, // The position of each particle
									 __global float *velocities, // The velocity of each particle
								   	 __global const uint *indices, // Indices from each voxel cell to each particle. Is [max_cell_particle_count * total_grid_cells] long
								   	 __global const uint *cell_particle_count, // Particle counter for each voxel cell. Is [total_grid_cells] long
								   	 const VoxelGridInfo grid_info,
								   	 const float dt) {
	const uint voxel_cell_index = get_global_id(0);
	const uint particle_count = cell_particle_count[voxel_cell_index];

	for (uint i = 0; i < particle_count; ++i) {
		const uint particle_buffer_index = 3 * get_particle_buffer_index(voxel_cell_index, 
																	     i, 
																	     grid_info.max_cell_particle_count, 
																	     indices);

		float3 velocity = (float3)(velocities[particle_buffer_index],
								   velocities[particle_buffer_index + 1],
								   velocities[particle_buffer_index + 2]);
		float3 position = (float3)(positions[particle_buffer_index],
						    	   positions[particle_buffer_index + 1],
								   positions[particle_buffer_index + 2]);

		position = position + dt * velocity;

		// x-boundaries
		if (position.x != clamp(position.x, -1.0f, 1.0f)) {
			position.x = clamp(position.x, -1.0f, 1.0f);
			velocity.x = - velocity.x;
		}

		// y-boundaries
		if (position.y != clamp(position.y, -1.0f, 1.0f)) {
			position.y = clamp(position.y, -1.0f, 1.0f);
			velocity.y = - velocity.y;
		}

		// z-boundaries
		if (position.z != clamp(position.z, -1.0f, 1.0f)) {
			position.z = clamp(position.z, -1.0f, 1.0f);
			velocity.z = - velocity.z;
		}

		positions[particle_buffer_index] = position.x;
		positions[particle_buffer_index + 1] = position.y;
		positions[particle_buffer_index + 2] = position.z;
		
		velocities[particle_buffer_index] = velocity.x;
		velocities[particle_buffer_index + 1] = velocity.y;
		velocities[particle_buffer_index + 2] = velocity.z;
	}
}

uint get_particle_buffer_index(const uint voxel_cell_index, 
							   const uint voxel_particle_index, 
					 		   const uint max_cell_particle_count,
					 		   __global const uint* restrict indices) {
	return indices[voxel_cell_index * max_cell_particle_count + voxel_particle_index];
}

float3 get_particle_velocity(const uint voxel_cell_index, 
							 const uint voxel_particle_index, 
					 		 const uint max_cell_particle_count,
					 		 __global const uint* restrict indices, 
					 		 __global const float* restrict velocities) {
	const uint particle_position_index = 3 * get_particle_buffer_index(voxel_cell_index, 
																  voxel_particle_index, 
																  max_cell_particle_count, 
																  indices);

	return (float3)(velocities[particle_position_index], 
					velocities[particle_position_index + 1], 
					velocities[particle_position_index + 2]);
}