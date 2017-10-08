#pragma OPENCL EXTENSION cl_khr_global_int32_base_atomics : enable

//typedef struct __attribute__ ((packed)) def_ParticleData {
typedef struct /*__attribute__ ((packed))*/ def_ParticleData {
    float density;      // 4 bytes

    float3 force;       // 12 bytes
    float color_field;  // 4 bytes
} ParticleData;

//typedef struct __attribute__ ((packed)) def_VoxelGridInfo {
typedef struct /*__attribute__ ((packed))*/ def_VoxelGridInfo {
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

// Calculate the voxel cell indices (x/y/z) representing the cell that contains the supplied position
int3 calculate_voxel_cell_indices(const float3 position, const VoxelGridInfo grid_info) {
	// todo investigate if ceil, floor or round should be used
	//return convert_int3(ceil((position - grid_info.grid_origin) / grid_info.grid_cell_size));
	return clamp(convert_int3(floor((position - grid_info.grid_origin) / grid_info.grid_cell_size)), 
		(int3)(0, 0, 0), // Minimum indices
		(int3)(convert_int3(grid_info.grid_dimensions) - (int3)(1, 1, 1)) // Maximum indices
	);
}

// Calculate the 1D-mapped voxel cell index for the given 3D voxel cell indices (x/y/z)
uint calculate_voxel_cell_index(const uint3 voxel_cell_indices, const VoxelGridInfo grid_info) {
	return voxel_cell_indices.x + grid_info.grid_dimensions.x * (voxel_cell_indices.y + grid_info.grid_dimensions.y * voxel_cell_indices.z);
}

__kernel void calculate_voxel_grid(__global const float *positions, // The position of each particle
								   __global volatile uint *indices, // Indices from each voxel cell to each particle. Is [max_cell_particle_count * total_grid_cells] long
								   __global volatile uint *cell_particle_count, // Particle counter for each voxel cell. Is [total_grid_cells] long
								   const VoxelGridInfo grid_info
){ 
	const uint particle_id = get_global_id(0);
	const uint particle_positions_id = 3 * particle_id;

	const float3 position = (float3)(positions[particle_positions_id], 
									 positions[particle_positions_id + 1], 
									 positions[particle_positions_id + 2]);

	const int3 voxel_cell_indices = calculate_voxel_cell_indices(position, grid_info);
	// Make sure that the voxel indices are within the grid itself
	/*
	if (voxel_cell_indices.x != clamp(voxel_cell_indices.x, (int)(0), (int)(grid_info.grid_dimensions.x - (1))) || 
		voxel_cell_indices.y != clamp(voxel_cell_indices.y, (int)(0), (int)(grid_info.grid_dimensions.y - (1))) || 
		voxel_cell_indices.z != clamp(voxel_cell_indices.z, (int)(0), (int)(grid_info.grid_dimensions.z - (1)))) {
		return;
	}*/

	// Safe to convert voxel cell indices to unsigned
	const uint voxel_cell_index = calculate_voxel_cell_index(convert_uint3(voxel_cell_indices), grid_info);

	// This line should not be neccessary since we've already clamped above
	// const uint voxel_cell_index_clamped = clamp(voxel_cell_index, (uint)(0), (grid_info.total_grid_cells - 1));

	// Increment the counter for this voxel cell, returning the old counter. This is used to index the voxel cell indices array
	const uint old_count = atomic_inc(&(cell_particle_count[voxel_cell_index]));

	// Undo last operation if maximum particle count is reached
	// todo fix this ugly anti-pattern
	if (old_count >= grid_info.max_cell_particle_count) {
		atomic_dec(&(cell_particle_count[voxel_cell_index]));
		return;
	}

	// Store this particle's buffer array index in the voxel cell
	indices[voxel_cell_index * grid_info.max_cell_particle_count + old_count] = particle_id;
}

__kernel void reset_voxel_grid(__global volatile uint *indices, // Indices from each voxel cell to each particle. Is [max_cell_particle_count * total_grid_cells] long
							   __global volatile uint *cell_particle_count, // Particle counter for each voxel cell. Is [total_grid_cells] long
							   const VoxelGridInfo grid_info) {
	const uint voxel_cell_index = get_global_id(0);
	cell_particle_count[voxel_cell_index] = 0;

	for (uint i = 0; i < grid_info.max_cell_particle_count; ++i) {
		indices[voxel_cell_index * grid_info.max_cell_particle_count + i] = 0;
	}
}