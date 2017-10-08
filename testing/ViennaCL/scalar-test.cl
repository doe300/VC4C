__kernel void as_cpu( 
  __global float * s1, 
 
  float fac2, 
  unsigned int options2, 
  __global const float * s2) 
{ 
  float alpha = fac2; 
 
  if (options2 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
      *s1 =- *s2 / alpha ; 
    } else { 
      *s1 =- *s2 * alpha ; 
    } 
  } else { 
    if (options2 & (1 << 1)) { 
      *s1 =+ *s2 / alpha ; 
    } else { 
      *s1 =+ *s2 * alpha ; 
    } 
  } 
} 
__kernel void as_gpu( 
  __global float * s1, 
 
  __global float * fac2, 
  unsigned int options2, 
  __global const float * s2) 
{ 
  float alpha = fac2[0]; 
 
  if (options2 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
      *s1 =- *s2 / alpha ; 
    } else { 
      *s1 =- *s2 * alpha ; 
    } 
  } else { 
    if (options2 & (1 << 1)) { 
      *s1 =+ *s2 / alpha ; 
    } else { 
      *s1 =+ *s2 * alpha ; 
    } 
  } 
} 
__kernel void asbs_cpu_cpu( 
  __global float * s1, 
 
  float fac2, 
  unsigned int options2, 
  __global const float * s2, 

  float fac3, 
  unsigned int options3, 
  __global const float * s3) 
{ 
  float alpha = fac2; 
 
  float beta = fac3; 
  if (options2 & (1 << 0)) { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 / alpha - *s3 / beta; 
     else 
      *s1 =- *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 * alpha - *s3 / beta; 
     else 
      *s1 =- *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 / alpha + *s3 / beta; 
     else 
      *s1 =- *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 * alpha + *s3 / beta; 
     else 
      *s1 =- *s2 * alpha + *s3 * beta; 
    } 
   } 
  } else { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 / alpha - *s3 / beta; 
     else 
      *s1 =+ *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 * alpha - *s3 / beta; 
     else 
      *s1 =+ *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 / alpha + *s3 / beta; 
     else 
      *s1 =+ *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 * alpha + *s3 / beta; 
     else 
      *s1 =+ *s2 * alpha + *s3 * beta; 
    } 
   } 
  } 
} 
__kernel void asbs_cpu_gpu( 
  __global float * s1, 
 
  float fac2, 
  unsigned int options2, 
  __global const float * s2, 

  __global float * fac3, 
  unsigned int options3, 
  __global const float * s3) 
{ 
  float alpha = fac2; 
 
  float beta = fac3[0]; 
  if (options2 & (1 << 0)) { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 / alpha - *s3 / beta; 
     else 
      *s1 =- *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 * alpha - *s3 / beta; 
     else 
      *s1 =- *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 / alpha + *s3 / beta; 
     else 
      *s1 =- *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 * alpha + *s3 / beta; 
     else 
      *s1 =- *s2 * alpha + *s3 * beta; 
    } 
   } 
  } else { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 / alpha - *s3 / beta; 
     else 
      *s1 =+ *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 * alpha - *s3 / beta; 
     else 
      *s1 =+ *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 / alpha + *s3 / beta; 
     else 
      *s1 =+ *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 * alpha + *s3 / beta; 
     else 
      *s1 =+ *s2 * alpha + *s3 * beta; 
    } 
   } 
  } 
} 
__kernel void asbs_gpu_cpu( 
  __global float * s1, 
 
  __global float * fac2, 
  unsigned int options2, 
  __global const float * s2, 

  float fac3, 
  unsigned int options3, 
  __global const float * s3) 
{ 
  float alpha = fac2[0]; 
 
  float beta = fac3; 
  if (options2 & (1 << 0)) { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 / alpha - *s3 / beta; 
     else 
      *s1 =- *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 * alpha - *s3 / beta; 
     else 
      *s1 =- *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 / alpha + *s3 / beta; 
     else 
      *s1 =- *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 * alpha + *s3 / beta; 
     else 
      *s1 =- *s2 * alpha + *s3 * beta; 
    } 
   } 
  } else { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 / alpha - *s3 / beta; 
     else 
      *s1 =+ *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 * alpha - *s3 / beta; 
     else 
      *s1 =+ *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 / alpha + *s3 / beta; 
     else 
      *s1 =+ *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 * alpha + *s3 / beta; 
     else 
      *s1 =+ *s2 * alpha + *s3 * beta; 
    } 
   } 
  } 
} 
__kernel void asbs_gpu_gpu( 
  __global float * s1, 
 
  __global float * fac2, 
  unsigned int options2, 
  __global const float * s2, 

  __global float * fac3, 
  unsigned int options3, 
  __global const float * s3) 
{ 
  float alpha = fac2[0]; 
 
  float beta = fac3[0]; 
  if (options2 & (1 << 0)) { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 / alpha - *s3 / beta; 
     else 
      *s1 =- *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 * alpha - *s3 / beta; 
     else 
      *s1 =- *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 / alpha + *s3 / beta; 
     else 
      *s1 =- *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =- *s2 * alpha + *s3 / beta; 
     else 
      *s1 =- *s2 * alpha + *s3 * beta; 
    } 
   } 
  } else { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 / alpha - *s3 / beta; 
     else 
      *s1 =+ *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 * alpha - *s3 / beta; 
     else 
      *s1 =+ *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 / alpha + *s3 / beta; 
     else 
      *s1 =+ *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 =+ *s2 * alpha + *s3 / beta; 
     else 
      *s1 =+ *s2 * alpha + *s3 * beta; 
    } 
   } 
  } 
} 
__kernel void asbs_s_cpu_cpu( 
  __global float * s1, 
 
  float fac2, 
  unsigned int options2, 
  __global const float * s2, 

  float fac3, 
  unsigned int options3, 
  __global const float * s3) 
{ 
  float alpha = fac2; 
 
  float beta = fac3; 
  if (options2 & (1 << 0)) { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 / alpha - *s3 / beta; 
     else 
      *s1 +=- *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 * alpha - *s3 / beta; 
     else 
      *s1 +=- *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 / alpha + *s3 / beta; 
     else 
      *s1 +=- *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 * alpha + *s3 / beta; 
     else 
      *s1 +=- *s2 * alpha + *s3 * beta; 
    } 
   } 
  } else { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 / alpha - *s3 / beta; 
     else 
      *s1 +=+ *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 * alpha - *s3 / beta; 
     else 
      *s1 +=+ *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 / alpha + *s3 / beta; 
     else 
      *s1 +=+ *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 * alpha + *s3 / beta; 
     else 
      *s1 +=+ *s2 * alpha + *s3 * beta; 
    } 
   } 
  } 
} 
__kernel void asbs_s_cpu_gpu( 
  __global float * s1, 
 
  float fac2, 
  unsigned int options2, 
  __global const float * s2, 

  __global float * fac3, 
  unsigned int options3, 
  __global const float * s3) 
{ 
  float alpha = fac2; 
 
  float beta = fac3[0]; 
  if (options2 & (1 << 0)) { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 / alpha - *s3 / beta; 
     else 
      *s1 +=- *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 * alpha - *s3 / beta; 
     else 
      *s1 +=- *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 / alpha + *s3 / beta; 
     else 
      *s1 +=- *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 * alpha + *s3 / beta; 
     else 
      *s1 +=- *s2 * alpha + *s3 * beta; 
    } 
   } 
  } else { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 / alpha - *s3 / beta; 
     else 
      *s1 +=+ *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 * alpha - *s3 / beta; 
     else 
      *s1 +=+ *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 / alpha + *s3 / beta; 
     else 
      *s1 +=+ *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 * alpha + *s3 / beta; 
     else 
      *s1 +=+ *s2 * alpha + *s3 * beta; 
    } 
   } 
  } 
} 
__kernel void asbs_s_gpu_cpu( 
  __global float * s1, 
 
  __global float * fac2, 
  unsigned int options2, 
  __global const float * s2, 

  float fac3, 
  unsigned int options3, 
  __global const float * s3) 
{ 
  float alpha = fac2[0]; 
 
  float beta = fac3; 
  if (options2 & (1 << 0)) { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 / alpha - *s3 / beta; 
     else 
      *s1 +=- *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 * alpha - *s3 / beta; 
     else 
      *s1 +=- *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 / alpha + *s3 / beta; 
     else 
      *s1 +=- *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 * alpha + *s3 / beta; 
     else 
      *s1 +=- *s2 * alpha + *s3 * beta; 
    } 
   } 
  } else { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 / alpha - *s3 / beta; 
     else 
      *s1 +=+ *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 * alpha - *s3 / beta; 
     else 
      *s1 +=+ *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 / alpha + *s3 / beta; 
     else 
      *s1 +=+ *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 * alpha + *s3 / beta; 
     else 
      *s1 +=+ *s2 * alpha + *s3 * beta; 
    } 
   } 
  } 
} 
__kernel void asbs_s_gpu_gpu( 
  __global float * s1, 
 
  __global float * fac2, 
  unsigned int options2, 
  __global const float * s2, 

  __global float * fac3, 
  unsigned int options3, 
  __global const float * s3) 
{ 
  float alpha = fac2[0]; 
 
  float beta = fac3[0]; 
  if (options2 & (1 << 0)) { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 / alpha - *s3 / beta; 
     else 
      *s1 +=- *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 * alpha - *s3 / beta; 
     else 
      *s1 +=- *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 / alpha + *s3 / beta; 
     else 
      *s1 +=- *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=- *s2 * alpha + *s3 / beta; 
     else 
      *s1 +=- *s2 * alpha + *s3 * beta; 
    } 
   } 
  } else { 
   if (options3 & (1 << 0)) { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 / alpha - *s3 / beta; 
     else 
      *s1 +=+ *s2 / alpha - *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 * alpha - *s3 / beta; 
     else 
      *s1 +=+ *s2 * alpha - *s3 * beta; 
    } 
   } else { 
    if (options2 & (1 << 1)) { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 / alpha + *s3 / beta; 
     else 
      *s1 +=+ *s2 / alpha + *s3 * beta; 
    } else { 
     if (options3 & (1 << 1)) 
      *s1 +=+ *s2 * alpha + *s3 / beta; 
     else 
      *s1 +=+ *s2 * alpha + *s3 * beta; 
    } 
   } 
  } 
} 
__kernel void swap( 
          __global float * s1, 
          __global float * s2) 
{ 
  float tmp = *s2; 
  *s2 = *s1; 
  *s1 = tmp; 
} 
