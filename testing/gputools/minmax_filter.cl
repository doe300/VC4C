//2D

__kernel void max_2_x(__global float * input,
						__global float * output,
						const int Nh
					 ){

  int i = get_global_id(0);
  int j = get_global_id(1);

  int Nx = get_global_size(0);
  int Ny = get_global_size(1);

  float res = -INFINITY;


  int start = i-Nh/2;


  const int h_start = max(0,Nh/2-i);
  const int h_end = min(Nh,Nx-i+Nh/2);

  for (int ht = h_start; ht< h_end; ++ht)
	  res = fmax(res,input[start+ht+j*Nx]);

  output[i+j*Nx] = res;
}

__kernel void max_2_y(__global float * input,
						__global float * output,
						const int Nh
					 ){

  int i = get_global_id(0);
  int j = get_global_id(1);
  
  int Nx = get_global_size(0);
  int Ny = get_global_size(1);

  float res = -INFINITY;

  int start = j-Nh/2;

  const int h_start = max(0,Nh/2-j);
  const int h_end = min(Nh,Ny-j+Nh/2);



  for (int ht = h_start; ht< h_end; ++ht)
	res = fmax(res,input[i+(start+ht)*Nx]);

  output[i+j*Nx] = res;
}


__kernel void min_2_x(__global float * input,
						__global float * output,
						const int Nh
					 ){

  int i = get_global_id(0);
  int j = get_global_id(1);
  
  int Nx = get_global_size(0);
  int Ny = get_global_size(1);

  float res = INFINITY;

  int start = i-Nh/2;

  const int h_start = max(0,Nh/2-i);
  const int h_end = min(Nh,Nx-i+Nh/2);

  for (int ht = h_start; ht< h_end; ++ht)
	  res = fmin(res,input[start+ht+j*Nx]);

  output[i+j*Nx] = res;
}

__kernel void min_2_y(__global float * input,
						__global float * output,
						const int Nh
					 ){

  int i = get_global_id(0);
  int j = get_global_id(1);
  
  int Nx = get_global_size(0);
  int Ny = get_global_size(1);

  float res = INFINITY;

  int start = j-Nh/2;

  const int h_start = max(0,Nh/2-j);
  const int h_end = min(Nh,Ny-j+Nh/2);



  for (int ht = h_start; ht< h_end; ++ht)
	res = fmin(res,input[i+(start+ht)*Nx]);

  output[i+j*Nx] = res;
}


//3D

__kernel void max_3_x(__global float * input,
				    __global float * output,
					   const int Nh
					 ){

					 int i = get_global_id(0);
  int j = get_global_id(1);
  int k = get_global_id(2);

  int Nx = get_global_size(0);
  int Ny = get_global_size(1);
  int Nz = get_global_size(2);



  float res = -INFINITY;

  int start = i-Nh/2;

  const int h_start = max(0,Nh/2-i);
  const int h_end = min(Nh,Nx-i+Nh/2);

  for (int ht = h_start; ht< h_end; ++ht)
	  res = fmax(res,input[start+ht+j*Nx+k*Nx*Ny]);

  output[i+j*Nx+k*Nx*Ny] = res;
}

__kernel void max_3_y(__global float * input,
				    __global float * output,
					   const int Nh
					 ){

  int i = get_global_id(0);
  int j = get_global_id(1);
  int k = get_global_id(2);

  int Nx = get_global_size(0);
  int Ny = get_global_size(1);
  int Nz = get_global_size(2);



  float res = -INFINITY;

  int start = j-Nh/2;

  const int h_start = max(0,Nh/2-j);
  const int h_end = min(Nh,Ny-j+Nh/2);

  for (int ht = h_start; ht< h_end; ++ht)
	res = fmax(res,input[i+(start+ht)*Nx+k*Nx*Ny]);


  output[i+j*Nx+k*Nx*Ny] = res;
}

__kernel void max_3_z(__global float * input,
				    __global float * output,
					   const int Nh
					 ){

  int i = get_global_id(0);
  int j = get_global_id(1);
  int k = get_global_id(2);

  int Nx = get_global_size(0);
  int Ny = get_global_size(1);
  int Nz = get_global_size(2);



  float res = -INFINITY;

  int start = k-Nh/2;

  const int h_start = max(0,Nh/2-k);
  const int h_end = min(Nh,Nz-k+Nh/2);

  for (int ht = h_start; ht< h_end; ++ht)
	res = fmax(res,input[i+j*Nx+(start+ht)*Nx*Ny]);


  output[i+j*Nx+k*Nx*Ny] = res;
}


__kernel void min_3_x(__global float * input,
				    __global float * output,
					   const int Nh
					 ){

					 int i = get_global_id(0);
  int j = get_global_id(1);
  int k = get_global_id(2);

  int Nx = get_global_size(0);
  int Ny = get_global_size(1);
  int Nz = get_global_size(2);



  float res = INFINITY;

  int start = i-Nh/2;

  const int h_start = max(0,Nh/2-i);
  const int h_end = min(Nh,Nx-i+Nh/2);

  for (int ht = h_start; ht< h_end; ++ht)
	  res = fmin(res,input[start+ht+j*Nx+k*Nx*Ny]);

  output[i+j*Nx+k*Nx*Ny] = res;
}

__kernel void min_3_y(__global float * input,
				    __global float * output,
					   const int Nh
					 ){

  int i = get_global_id(0);
  int j = get_global_id(1);
  int k = get_global_id(2);

  int Nx = get_global_size(0);
  int Ny = get_global_size(1);
  int Nz = get_global_size(2);



  float res = INFINITY;

  int start = j-Nh/2;

  const int h_start = max(0,Nh/2-j);
  const int h_end = min(Nh,Ny-j+Nh/2);

  for (int ht = h_start; ht< h_end; ++ht)
	res = fmin(res,input[i+(start+ht)*Nx+k*Nx*Ny]);


  output[i+j*Nx+k*Nx*Ny] = res;
}

__kernel void min_3_z(__global float * input,
				    __global float * output,
					   const int Nh
					 ){

  int i = get_global_id(0);
  int j = get_global_id(1);
  int k = get_global_id(2);

  int Nx = get_global_size(0);
  int Ny = get_global_size(1);
  int Nz = get_global_size(2);



  float res = INFINITY;

  int start = k-Nh/2;

  const int h_start = max(0,Nh/2-k);
  const int h_end = min(Nh,Nz-k+Nh/2);

  for (int ht = h_start; ht< h_end; ++ht)
	res = fmin(res,input[i+j*Nx+(start+ht)*Nx*Ny]);


  output[i+j*Nx+k*Nx*Ny] = res;
}
