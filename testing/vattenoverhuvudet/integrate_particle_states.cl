#pragma OPENCL EXTENSION cl_khr_global_int32_base_atomics : enable

#define zero3 (float3)(0.0f, 0.0f, 0.0f);

__constant float EPSILON = 1e-5;
__constant float PI = 3.1415926535f;
#define MAX_VEL 5.0f
__constant float3 VELOCITY_CLAMP = (float3)(MAX_VEL, MAX_VEL, MAX_VEL);

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

float euclidean_distance2(const float3 r) {
	return r.x * r.x + r.y * r.y + r.z * r.z;
}

float euclidean_distance(const float3 r) {
	return sqrt(r.x * r.x + r.y * r.y + r.z * r.z);
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

__kernel void integrate_particle_states(__global float* restrict positions,
										__global float* restrict velocities,
										__global const float3* restrict forces,
										const VoxelGridInfo grid_info,
										const FluidInfo fluid_info,
										const float dt) {
	const uint particle_id = get_global_id(0);
	const uint particle_position_id = 3 * particle_id;

	float3 force = forces[particle_id];

	float3 position = (float3)(positions[particle_position_id],
									 positions[particle_position_id + 1],
									 positions[particle_position_id + 2]);
	float3 velocity = (float3)(velocities[particle_position_id],
									 velocities[particle_position_id + 1],
									 velocities[particle_position_id + 2]);

	// Apply forces from the walls
	float3 boundary_force = zero3;
	float diff = 0.0f;
	float hardness = 1000.0f;

	// Bottom bound
	float3 r = (float3)(0.0f, fabs(position.y - grid_info.grid_origin.y), 0.0f);
	boundary_force = boundary_force - fluid_info.mass * 10.0f * hardness * gradW_spiky(r, 5.0f * grid_info.grid_cell_size);

	float distance = sqrt(pow(position.x, 2) + pow(position.z, 2));
	// Using 1/2 of grid_origin.x as radius of cylinder
	diff = 0.5f * grid_info.grid_dimensions.x * grid_info.grid_cell_size - distance;
	r = - (float3)(position / distance) * diff;
	boundary_force = boundary_force - fluid_info.mass * hardness * gradW_spiky(r, 5.0f * grid_info.grid_cell_size);

    // Apply external forces
    force = force + boundary_force;

	// Acceleration according to Newton's law: a = F / m
	const float3 acceleration = force / fluid_info.mass + fluid_info.gravity;

	// Integrate to new state using simple Euler integration
	// Todo investigate other methods such as velocity verlet or leap-frog
	//velocity = velocity + acceleration * dt;
	velocity = clamp(velocity + acceleration * dt, -VELOCITY_CLAMP, VELOCITY_CLAMP);

	if (position.y - grid_info.grid_origin.y < grid_info.grid_cell_size || euclidean_distance2(r) < grid_info.grid_cell_size) {
		//velocity = (1 - dt) * fluid_info.k_wall_friction * velocity;
	}
	/*
	if (euclidean_distance2(r) < grid_info.grid_cell_size) {
		velocity = dt * fluid_info.k_wall_friction * velocity;
	}
*/

	position = position + 0.5f * velocity * dt;

	// Write new position and velocity
	positions[particle_position_id] = position.x;
	positions[particle_position_id + 1] = position.y;
	positions[particle_position_id + 2] = position.z;
	velocities[particle_position_id] = velocity.x;
	velocities[particle_position_id + 1] = velocity.y;
	velocities[particle_position_id + 2] = velocity.z;
}