#pragma OPENCL EXTENSION cl_khr_global_int32_base_atomics : enable

#define zero3 (float3)(0.0f, 0.0f, 0.0f);

__constant float PI = 3.1415926535f;
__constant float EPSILON = 1e-5;

__constant float DENSITY_MIN = 5000.0f;
__constant float DENSITY_MAX = 100000.0f;

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

typedef struct def_FluidInfo {
	// The mass of each fluid particle
	float mass;

	float k_gas;
	float k_viscosity;
	float rest_density;
	float sigma;
	float k_threshold;
	float k_wall_damper;
	float k_wall_friction;

	float3 gravity;
} FluidInfo;

// Calculates the euclidean length of the vector r
float euclidean_distance(const float3 r);

// Calculates the squared euclidean length of the vector r (x^2 + y^2 + z^2)
float euclidean_distance2(const float3 r);

// The SPH kernel "poly6": used for density- and "color field" calc
float W_poly6(const float3 r, const float h);

// Gradient of the SPH-kernel "poly6": used for "color field" gradient calc
float3 gradW_poly6(const float3 r, const float h);

// Laplacian of the SPH-kernel "poly6": used for "color field" laplacian calc
float laplacianW_poly6(const float3 r, const float h);

// Gradient of the SPH-kernel "spiky": used for pressure force calc
float3 gradW_spiky(const float3 r, const float h);

// Laplacian of the SPH-kernel "viscosity": used for viscosity force calc
float laplacianW_viscosity(const float3 r, const float h);

// Map a particle index inside a voxel cell to its global buffer index
uint get_particle_buffer_index(const uint voxel_cell_index, 
							   const uint voxel_particle_index, 
					 		   const uint max_cell_particle_count,
					 		   __global const uint* restrict indices);

// Get the position of a particle based on its cell index and particle index inside the given cell
float3 get_particle_position(const uint voxel_cell_index, 
							 const uint voxel_particle_index, 
					 		 const uint max_cell_particle_count,
					 		 __global const uint* restrict indices, 
					 		 __global const float* restrict positions);

float3 get_particle_velocity(const uint voxel_cell_index, 
							 const uint voxel_particle_index, 
					 		 const uint max_cell_particle_count,
					 		 __global const uint* restrict indices, 
					 		 __global const float* restrict velocities);

// Calculate the 1D-mapped voxel cell index for the given 3D voxel cell indices (x/y/z)
uint calculate_voxel_cell_index(const uint3 voxel_cell_indices, const VoxelGridInfo grid_info);

__kernel void calculate_forces(__global const float* restrict positions, // The position of each particle
							   __global const float* restrict velocities, // The position of each particle
							   __global float3* restrict forces, 		 // The force on each particle
							   __global const float* restrict densities, // The density of each particle. Is [max_cell_particle_count * total_grid_cells] long, since
																	   // it does NOT need to match up with the particle's global positions/velocities buffers 
						   	   __global const uint* restrict indices,   // Indices from each voxel cell to each particle. Is [max_cell_particle_count * total_grid_cells] long
						   	   __global const uint* restrict cell_particle_count, // Particle counter for each voxel cell. Is [total_grid_cells] long
						   	   const VoxelGridInfo grid_info,
						   	   const FluidInfo fluid_info) {
	
	const uint3 voxel_cell_indices = (uint3)(get_global_id(0), get_global_id(1), get_global_id(2));
	const uint voxel_cell_index = calculate_voxel_cell_index(voxel_cell_indices, grid_info);
	const uint particle_count = cell_particle_count[voxel_cell_index];

	// Store the cumulative forces locally (in private kernel memory) during calc
	float3 processed_particle_forces[@VOXEL_CELL_PARTICLE_COUNT@];
	
	// Store the cumulative colorfield (and its gradient and laplacian) locally during calc
	float processed_particle_colorfield[@VOXEL_CELL_PARTICLE_COUNT@];
	float3 processed_particle_colorfield_grad[@VOXEL_CELL_PARTICLE_COUNT@];
	float processed_particle_colorfield_laplacian[@VOXEL_CELL_PARTICLE_COUNT@];

	// Pre-calculate the processed particle's pressure
	float processed_particle_pressure[@VOXEL_CELL_PARTICLE_COUNT@];

	float3 processed_particle_positions[@VOXEL_CELL_PARTICLE_COUNT@];
	float3 processed_particle_velocities[@VOXEL_CELL_PARTICLE_COUNT@];

	for (uint idp = 0; idp < particle_count; ++idp) {
		// Pre-store the position of the particle being processed locally (in private memory)
		processed_particle_positions[idp] = get_particle_position(voxel_cell_index, 
																  idp, 
																  grid_info.max_cell_particle_count,
																  indices,
																  positions);
		processed_particle_velocities[idp] = get_particle_velocity(voxel_cell_index, 
																   idp, 
																   grid_info.max_cell_particle_count,
																   indices,
																   velocities);

		// Pre-calculate the pressure
		processed_particle_pressure[idp] = (densities[voxel_cell_index * grid_info.max_cell_particle_count + idp] - fluid_info.rest_density) * fluid_info.k_gas;

		// Initialize the force sum and all colorfield sums to zeroes
		processed_particle_forces[idp] = (float3)(0.0f, 0.0f, 0.0f);

		processed_particle_colorfield[idp] = 0.0f;
		processed_particle_colorfield_grad[idp] = (float3)(0.0f, 0.0f, 0.0f);
		processed_particle_colorfield_laplacian[idp] = 0.0f;
	}

	// Pre-define this before x*y*z loop
	const int3 max_cell_indices = convert_int3(grid_info.grid_dimensions) - (int3)(1, 1, 1);

	// Pre-declare memory for relative position, for speeeeeeeeeeeed
	float3 relative_position = (float3)(0.0f, 0.0f, 0.0f);

	// Loop through all voxel cells around the currently processed voxel cell
	// todo optimize these for-loops and voxel cell index generation
	for (int d_idx = -1; d_idx <= 1; ++d_idx) {
		
		// Check if the x-index lies outside the voxel grid
		const int idx = convert_int(voxel_cell_indices.x) + d_idx;
		if (idx == clamp(idx, 0, max_cell_indices.x)) {
			for (int d_idy = -1; d_idy <= 1; ++d_idy) {

				// Check if the x-index lies outside the voxel grid
				const int idy = convert_int(voxel_cell_indices.y) + d_idy;
				if (idy == clamp(idy, 0, max_cell_indices.y)) {
					for (int d_idz = -1; d_idz <= 1; ++d_idz) {

						// Check if the x-index lies outside the voxel grid
						const int idz = convert_int(voxel_cell_indices.z) + d_idz;
						if (idz == clamp(idz, 0, max_cell_indices.z)) {
							const uint current_voxel_cell_index = calculate_voxel_cell_index((uint3)(idx, idy, idz), grid_info);
							const uint current_voxel_particle_count = cell_particle_count[current_voxel_cell_index];

							// Iterate through this cell's particles
							for (uint idp = 0; idp < current_voxel_particle_count; ++idp) {
								// LOOK HERE:
								// The below line is the reason the nested loops are the way they are:
								//
								// Since the position of a particle is NOT located within the grid cell, each time we want to use the position
								// of a particle we have to calculate its global buffer index and retrieve it that way. This is a slow operation.
								// So instead of having the outer-most loop be over each particle in the current voxel we loop through the voxels
								// This way we only need to fetch the position of each particle in the neighbouring cells ONCE. :D
								const float3 position = get_particle_position(current_voxel_cell_index, 
																			  idp,
																			  grid_info.max_cell_particle_count,
																			  indices,
																			  positions);
								const float3 velocity = get_particle_velocity(current_voxel_cell_index, 
																			  idp,
																			  grid_info.max_cell_particle_count,
																			  indices,
																			  velocities);

								const float density = clamp(densities[voxel_cell_index * grid_info.max_cell_particle_count + idp], DENSITY_MIN, DENSITY_MAX);
								//const float density = 6000.0f;
								const float pressure = (density - fluid_info.rest_density) * fluid_info.k_gas;

								// Pre-calc colorfield constant used in all three colorfield calculations
								const float c_colorfield = fluid_info.mass / density;

								for (uint processed_particle_id = 0; processed_particle_id < particle_count; ++processed_particle_id) {
									/** Calculate the current particle's force contributions to the processed particle based on the 'idp' **/
									// todo investigate if any values can be pre-calculated outside this loop
									relative_position = processed_particle_positions[processed_particle_id] - position;

									/* Pressure force */
									processed_particle_forces[processed_particle_id] = processed_particle_forces[processed_particle_id] -
										fluid_info.mass * ( (pressure + processed_particle_pressure[processed_particle_id]) / (2 * density) ) * gradW_spiky(relative_position, grid_info.grid_cell_size);

									/* Viscosity force */
									processed_particle_forces[processed_particle_id] = processed_particle_forces[processed_particle_id] + 
									fluid_info.k_viscosity * fluid_info.mass * ( 1 / density ) * laplacianW_viscosity(relative_position, grid_info.grid_cell_size) * (velocity - processed_particle_velocities[processed_particle_id]);

									/* Color field contribution */
									processed_particle_colorfield[processed_particle_id] = processed_particle_colorfield[processed_particle_id] + 
										c_colorfield * W_poly6(relative_position, grid_info.grid_cell_size);

									processed_particle_colorfield_grad[processed_particle_id] = processed_particle_colorfield_grad[processed_particle_id] + 
										c_colorfield * gradW_poly6(relative_position, grid_info.grid_cell_size);
									
									processed_particle_colorfield_laplacian[processed_particle_id] = processed_particle_colorfield_laplacian[processed_particle_id] + 
										c_colorfield * laplacianW_poly6(relative_position, grid_info.grid_cell_size);						
								}
							}
						}
					}
				}
			}
		}
	}

	// Final calculation and storage of each processed particle
	for (uint idp = 0; idp < particle_count; ++idp) {
		/* See if tension force should be applied for each particle */
		const float colorfield_grad_length = euclidean_distance2(processed_particle_colorfield_grad[idp]);
		if (colorfield_grad_length >= pow(fluid_info.k_threshold, 2)) {
			processed_particle_forces[idp] = processed_particle_forces[idp] - 
				fluid_info.sigma * processed_particle_colorfield_laplacian[idp] * processed_particle_colorfield_grad[idp] / colorfield_grad_length;
		}
	    
	    processed_particle_forces[idp] = processed_particle_forces[idp];

		// The global force buffer array is simply linear with the particles in no particular order
		// To retrieve the correct index for a particle in a particular voxel cell we have to call our special function :)
		const uint particle_force_index = get_particle_buffer_index(voxel_cell_index,
																		idp,
																		grid_info.max_cell_particle_count,
																		indices);
		
		forces[particle_force_index].x = processed_particle_forces[idp].x;
		forces[particle_force_index].y = processed_particle_forces[idp].y;
		forces[particle_force_index].z = processed_particle_forces[idp].z;
	}
}

__kernel void calculate_particle_densities(__global const float* restrict positions, // The position of each particle
											     __global float* restrict out_densities,   // The density of each particle. Is [max_cell_particle_count * total_grid_cells] long, since
											 											   // it does NOT need to match up with the particle's global positions/velocities buffers 
										   	     __global const uint* restrict indices, // Indices from each voxel cell to each particle. Is [max_cell_particle_count * total_grid_cells] long
										   	     __global const uint* restrict cell_particle_count, // Particle counter for each voxel cell. Is [total_grid_cells] long
										   	     const VoxelGridInfo grid_info,
										   	     const FluidInfo fluid_info) {
	const uint3 voxel_cell_indices = (uint3)(get_global_id(0), get_global_id(1), get_global_id(2));
	const uint voxel_cell_index = calculate_voxel_cell_index(voxel_cell_indices, grid_info);
	const uint particle_count = cell_particle_count[voxel_cell_index];

	// Store the densities locally (in private kernel memory) during calculation
	float processed_particle_densities[@VOXEL_CELL_PARTICLE_COUNT@];

	// Pre-store the positions of the particles being processed locally (in private memory)
	float3 processed_particle_positions[@VOXEL_CELL_PARTICLE_COUNT@];
	for (uint idp = 0; idp < particle_count; ++idp) {
		processed_particle_positions[idp] = get_particle_position(voxel_cell_index, 
																  idp, 
																  grid_info.max_cell_particle_count,
																  indices,
																  positions);
		processed_particle_densities[idp] = 0.0f;
	}

	// Pre-define this before x*y*z loop
	const int3 max_cell_indices = convert_int3(grid_info.grid_dimensions) - (int3)(1, 1, 1);

	// Loop through all voxel cells around the currently processed voxel cell
	// todo optimize these for-loops and voxel cell index generation
	for (int d_idx = -1; d_idx <= 1; ++d_idx) {

		// Check if the x-index lies outside the voxel grid
		const int idx = convert_int(voxel_cell_indices.x) + d_idx;
		if (idx == clamp(idx, 0, max_cell_indices.x)) {
			for (int d_idy = -1; d_idy <= 1; ++d_idy) {

				// Check if the x-index lies outside the voxel grid
				const int idy = convert_int(voxel_cell_indices.y) + d_idy;
				if (idy == clamp(idy, 0, max_cell_indices.y)) {
					for (int d_idz = -1; d_idz <= 1; ++d_idz) {

						// Check if the x-index lies outside the voxel grid
						const int idz = convert_int(voxel_cell_indices.z) + d_idz;
						if (idz == clamp(idz, 0, max_cell_indices.z)) {
							const uint current_voxel_cell_index = calculate_voxel_cell_index((uint3)(idx, idy, idz), grid_info);
							const uint current_voxel_particle_count = cell_particle_count[current_voxel_cell_index];

							// Iterate through this cell's particles
							for (uint idp = 0; idp < current_voxel_particle_count; ++idp) {
								// LOOK HERE:
								// The below line is the reason the nested loops are the way they are:
								//
								// Since the position of a particle is NOT located within the grid cell, each time we want to use the position
								// of a particle we have to calculate its global buffer index and retrieve it that way. This is a slow operation.
								// So instead of having the outer-most loop be over each particle in the current voxel we loop through the voxels
								// This way we only need to fetch the position of each particle in the neighbouring cells ONCE. :D
								const float3 position = get_particle_position(current_voxel_cell_index, 
																			  idp,
																			  grid_info.max_cell_particle_count,
																			  indices,
																			  positions);

								for (uint processed_particle_id = 0; processed_particle_id < particle_count; ++processed_particle_id) {
									// Calculate and apply the processed particle's density based on the 'idp'
									processed_particle_densities[processed_particle_id] = processed_particle_densities[processed_particle_id] 
										+ fluid_info.mass * W_poly6(processed_particle_positions[processed_particle_id] - position, grid_info.grid_cell_size);
								}
							}
						}	
					}
				}
			}
		}
	}

	// Move the privately stored densities to global memory
	for (uint idp = 0; idp < particle_count; ++idp) {
		// The global density buffer array is simply linear with the particles in no particular order
		// To retrieve the correct index for a particle in a particular voxel cell we have to call our special function :)

		out_densities[voxel_cell_index * grid_info.max_cell_particle_count + idp] = processed_particle_densities[idp];

		//out_densities[voxel_cell_index * grid_info.max_cell_particle_count + idp]
		//	= clamp(processed_particle_densities[idp], DENSITY_MIN, DENSITY_MAX);
	}
}

float euclidean_distance2(const float3 r) {
	return r.x * r.x + r.y * r.y + r.z * r.z;
}

float euclidean_distance(const float3 r) {
	return sqrt(r.x * r.x + r.y * r.y + r.z * r.z);
}

float W_poly6(const float3 r, const float h) {
	const float tmp = h * h - euclidean_distance2(r);
	if (tmp < EPSILON) {
		return 0.0f;
	}

	return ( 315.0f / (64.0f * PI * pow(h,9)) ) * pow((tmp), 3);
}

float3 gradW_poly6(const float3 r, const float h) {
	const float radius2 = euclidean_distance2(r);
	if (radius2 >= h * h) {
		return zero3;
	}
	if (radius2 <= EPSILON) {
		return zero3;
	}

	const float kernel_constant = - (315 /(64 * PI * pow(h, 9))) * 6 * pow((h * h - euclidean_distance2(r)), 2);
	return (float3)(kernel_constant * r.x,
					kernel_constant * r.y,
					kernel_constant * r.z);
}

float laplacianW_poly6(const float3 r, const float h) {
	const float radius2 = euclidean_distance2(r);
	if (radius2 >= h*h) {
		return 0.0f;
	}

	// todo maybe pre-calculate h^2 - radius2 since it is used 2 times? then again, maybe not...
	return (315 / (64 * PI * pow(h, 9))) * (24 * radius2 * (pow(h, 2) - radius2) - 6 * pow((pow(h, 2) - radius2), 2));
}

float3 gradW_spiky(const float3 r, const float h) {
	const float radius2 = euclidean_distance2(r);
	if (radius2 >= h * h) {
		return zero3;
	}
	if (radius2 <= EPSILON) {
		return zero3;
	}

	const float radius = sqrt(radius2);
	const float kernel_constant = - (15 / (PI * pow(h, 6))) * 3 * pow(h - radius, 2) / radius;

	return (float3)(kernel_constant * r.x, 
				   	kernel_constant * r.y, 
				   	kernel_constant * r.z);
}

float laplacianW_viscosity(const float3 r, const float h) {
	const float tmp = h - euclidean_distance(r);
	if (tmp <= 0.0f) {
		return 0.0f;
	}

	return (45 / (PI * pow(h, 6))) * (h - euclidean_distance(r));
}

uint calculate_voxel_cell_index(const uint3 voxel_cell_indices, const VoxelGridInfo grid_info) {
	return voxel_cell_indices.x + grid_info.grid_dimensions.x * (voxel_cell_indices.y + grid_info.grid_dimensions.y * voxel_cell_indices.z);
}

uint get_particle_buffer_index(const uint voxel_cell_index, 
							   const uint voxel_particle_index, 
					 		   const uint max_cell_particle_count,
					 		   __global const uint* restrict indices) {
	return indices[voxel_cell_index * max_cell_particle_count + voxel_particle_index];
}

float3 get_particle_position(const uint voxel_cell_index, 
							 const uint voxel_particle_index, 
					 		 const uint max_cell_particle_count,
					 		 __global const uint* restrict indices, 
					 		 __global const float* restrict positions) {
	const uint particle_position_index = 3 * get_particle_buffer_index(voxel_cell_index, 
																  voxel_particle_index, 
																  max_cell_particle_count, 
																  indices);

	return (float3)(positions[particle_position_index], 
					positions[particle_position_index + 1], 
					positions[particle_position_index + 2]);
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