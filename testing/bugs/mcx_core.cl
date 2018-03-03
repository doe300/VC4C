/***************************************************************************//**
**  \mainpage Monte Carlo eXtreme - GPU accelerated Monte Carlo Photon Migration \
**      -- OpenCL edition
**  \author Qianqian Fang <q.fang at neu.edu>
**  \copyright Qianqian Fang, 2009-2018
**
**  \section sref Reference:
**  \li \c (\b Yu2018) Leiming Yu, Fanny Nina-Paravecino, David Kaeli, and Qianqian Fang,
**         "Scalable and massively parallel Monte Carlo photon transport simulations 
**         for heterogeneous computing platforms," J. Biomed. Optics, 23(1), 010504 (2018)
**
**  \section slicense License
**          GPL v3, see LICENSE.txt for details
*******************************************************************************/

#ifdef MCX_SAVE_DETECTORS
  #pragma OPENCL EXTENSION cl_khr_global_int32_base_atomics : enable
#endif

#ifdef USE_HALF
  #pragma OPENCL EXTENSION cl_khr_fp16 : enable
  #define FLOAT4VEC half4
  #define TOFLOAT4  convert_half4
#else
  #define FLOAT4VEC float4
  #define TOFLOAT4
#endif

#ifdef MCX_USE_NATIVE
  #define MCX_MATHFUN(fun)              native_##fun
  #define MCX_SINCOS(theta,osin,ocos)   {(osin)=native_sin(theta);(ocos)=native_cos(theta);} 
#else
  #define MCX_MATHFUN(fun)              fun
  #define MCX_SINCOS(theta,osin,ocos)   (ocos)=sincos((theta),&(osin))
#endif

#ifdef MCX_GPU_DEBUG
  #define GPUDEBUG(x)        printf x             // enable debugging in CPU mode
#else
  #define GPUDEBUG(x)
#endif

#define R_PI               0.318309886183791f
#define RAND_MAX           4294967295

#define ONE_PI             3.1415926535897932f     //pi
#define TWO_PI             6.28318530717959f       //2*pi

#define C0                 299792458000.f          //speed of light in mm/s
#define R_C0               3.335640951981520e-12f  //1/C0 in s/mm

#define EPS                FLT_EPSILON             //round-off limit
#define VERY_BIG           (1.f/FLT_EPSILON)       //a big number
#define JUST_ABOVE_ONE     1.0001f                 //test for boundary
#define SAME_VOXEL         -9999.f                 //scatter within a voxel
#define NO_LAUNCH          9999                    //when fail to launch, for debug
#define MAX_PROP           256                     //maximum property number

#define DET_MASK           0x80
#define MED_MASK           0x7F
#define NULL               0

typedef struct KernelParams {
  float4 ps,c0;
  float4 maxidx;
  uint4  dimlen,cp0,cp1;
  uint2  cachebox;
  float  minstep;
  float  twin0,twin1,tmax;
  float  oneoverc0;
  uint   isrowmajor,save2pt,doreflect,dorefint,savedet;
  float  Rtstep;
  float  minenergy;
  float  skipradius2;
  float  minaccumtime;
  uint   maxdetphoton;
  uint   maxmedia;
  uint   detnum;
  uint   idx1dorig;
  uint   mediaidorig;
  uint   blockphoton;
  uint   blockextra;
} MCXParam __attribute__ ((aligned (32)));


//#ifndef USE_XORSHIFT128P_RAND     // xorshift128+ is the default RNG
#ifdef USE_LL5_RAND                 //enable the legacy Logistic Lattic RNG

#define RAND_BUF_LEN       5        //register arrays
#define RAND_SEED_LEN      5        //32bit seed length (32*5=160bits)
#define INIT_LOGISTIC      100

typedef float RandType;

#define FUN(x)               (4.f*(x)*(1.f-(x)))
#define NU                   1e-7f
#define NU2                  (1.f-2.f*NU)
#define MIN_INVERSE_LIMIT    1e-7f
#define logistic_uniform(v)  (acos(1.f-2.f*(v))*R_PI)
#define R_MAX_C_RAND         (1.f/RAND_MAX)
#define LOG_MT_MAX           22.1807097779182f

#define RING_FUN(x,y,z)      (NU2*(x)+NU*((y)+(z)))

void logistic_step(__private RandType *t, __private RandType *tnew, int len_1){
    t[0]=FUN(t[0]);
    t[1]=FUN(t[1]);
    t[2]=FUN(t[2]);
    t[3]=FUN(t[3]);
    t[4]=FUN(t[4]);
    tnew[4]=RING_FUN(t[0],t[4],t[1]);   /* shuffle the results by separation of 2*/
    tnew[0]=RING_FUN(t[1],t[0],t[2]);
    tnew[1]=RING_FUN(t[2],t[1],t[3]);
    tnew[2]=RING_FUN(t[3],t[2],t[4]);
    tnew[3]=RING_FUN(t[4],t[3],t[0]);
}
// generate random number for the next zenith angle
void rand_need_more(__private RandType t[RAND_BUF_LEN]){
    RandType tnew[RAND_BUF_LEN]={0.f};
    logistic_step(t,tnew,RAND_BUF_LEN-1);
    logistic_step(tnew,t,RAND_BUF_LEN-1);
}

void logistic_init(__private RandType *t,__global uint *seed,uint idx){
     int i;
     for(i=0;i<RAND_BUF_LEN;i++)
           t[i]=(RandType)seed[idx*RAND_BUF_LEN+i]*R_MAX_C_RAND;

     for(i=0;i<INIT_LOGISTIC;i++)  /*initial randomization*/
           rand_need_more(t);
}
// transform into [0,1] random number
RandType rand_uniform01(__private RandType t[RAND_BUF_LEN]){
    rand_need_more(t);
    return logistic_uniform(t[0]);
}
void gpu_rng_init(__private RandType t[RAND_BUF_LEN],__global uint *n_seed,int idx){
    logistic_init(t,n_seed,idx);
}

#else

#pragma OPENCL EXTENSION cl_khr_fp64 : enable

#define RAND_BUF_LEN       2        //register arrays
#define RAND_SEED_LEN      4        //48 bit packed with 64bit length
#define LOG_MT_MAX         22.1807097779182f
#define IEEE754_DOUBLE_BIAS     0x3FF0000000000000ul /* Added to exponent.  */

typedef ulong  RandType;

static float xorshift128p_nextf (__private RandType t[RAND_BUF_LEN]){
   union {
        double d;
        ulong  i;
   } s1;
   const ulong s0 = t[1];
   s1.i = t[0];
   t[0] = s0;
   s1.i ^= s1.i << 23; // a
   t[1] = s1.i ^ s0 ^ (s1.i >> 18) ^ (s0 >> 5); // b, c
   s1.i = t[1] + s0;
   s1.i = (s1.i >> 12) | IEEE754_DOUBLE_BIAS;

   return (float)s1.d - 1.0f;
}

static void copystate(__private RandType t[RAND_BUF_LEN], __private RandType tnew[RAND_BUF_LEN]){
    tnew[0]=t[0];
    tnew[1]=t[1];
}

// generate random number for the next zenith angle
static void rand_need_more(__private RandType t[RAND_BUF_LEN]){
}

static float rand_uniform01(__private RandType t[RAND_BUF_LEN]){
    return xorshift128p_nextf(t);
}

static void xorshift128p_seed (__global uint *seed,RandType t[RAND_BUF_LEN])
{
    t[0] = (ulong)seed[0] << 32 | seed[1] ;
    t[1] = (ulong)seed[2] << 32 | seed[3];
}

static void gpu_rng_init(__private RandType t[RAND_BUF_LEN], __global uint *n_seed, int idx){
    xorshift128p_seed((n_seed+idx*RAND_SEED_LEN),t);
}
static void gpu_rng_reseed(__private RandType t[RAND_BUF_LEN],__global uint *cpuseed,uint idx,float reseed){
}

#endif

float rand_next_scatlen(__private RandType t[RAND_BUF_LEN]){
    return -MCX_MATHFUN(log)(rand_uniform01(t)+EPS);
}

#define rand_next_aangle(t)  rand_uniform01(t)
#define rand_next_zangle(t)  rand_uniform01(t)
#define rand_next_reflect(t) rand_uniform01(t)
#define rand_do_roulette(t)  rand_uniform01(t) 


#ifdef USE_ATOMIC
// OpenCL float atomicadd hack:
// http://suhorukov.blogspot.co.uk/2011/12/opencl-11-atomic-operations-on-floating.html
// https://devtalk.nvidia.com/default/topic/458062/atomicadd-float-float-atomicmul-float-float-/

inline void atomicadd(volatile __global float* address, const float value){
    float old = value;
    while ((old = atomic_xchg(address, atomic_xchg(address, 0.0f)+old))!=0.0f);
}
#endif

void clearpath(__local float *p, __constant MCXParam *gcfg){
      uint i;
      for(i=0;i<gcfg->maxmedia;i++)
      	   p[i]=0.f;
}

#ifdef MCX_SAVE_DETECTORS
uint finddetector(float4 *p0,__constant float4 *gdetpos,uint id){
      if((gdetpos[id].x-p0[0].x)*(gdetpos[id].x-p0[0].x)+
	   (gdetpos[id].y-p0[0].y)*(gdetpos[id].y-p0[0].y)+
	   (gdetpos[id].z-p0[0].z)*(gdetpos[id].z-p0[0].z) < gdetpos[id].w){
	        return id+1;
      }
      return 0;
}

void savedetphoton(uint detid,__global float *n_det,__global uint *detectedphoton,float nscat,
                   __local float *ppath,float4 *p0,__constant float4 *gdetpos,__constant MCXParam *gcfg){
      detid=finddetector(p0,gdetpos,detid-1);
      if(detid){
	 uint baseaddr=atomic_inc(detectedphoton);
	 if(baseaddr<gcfg->maxdetphoton){
	    uint i;
	    baseaddr*=gcfg->maxmedia+2;
	    n_det[baseaddr++]=detid;
	    n_det[baseaddr++]=nscat;
	    for(i=0;i<gcfg->maxmedia;i++){
		n_det[baseaddr+i]=ppath[i]; // save partial pathlength to the memory
	    }
	 }
      }
}
#endif

float mcx_nextafterf(float a, int dir){
      union{
          float f;
	  uint  i;
      } num;
      num.f=a+1000.f;
      num.i+=dir ^ (num.i & 0x80000000U);
      return num.f-1000.f;
}

#ifndef USE_HALF

float hitgrid(float4 *p0, float4 *v, float4 *htime, int *id){
      float dist;

      //time-of-flight to hit the wall in each direction

      htime[0]=fabs(floor(p0[0])-convert_float4(isgreater(v[0],((float4)(0.f))))-p0[0]);
      htime[0]=fabs(native_divide(htime[0]+(float4)EPS,v[0]));

      //get the direction with the smallest time-of-flight
      dist=fmin(fmin(htime[0].x,htime[0].y),htime[0].z);
      (*id)=(dist==htime[0].x?0:(dist==htime[0].y?1:2));

      htime[0]=p0[0]+(float4)(dist)*v[0];

#ifdef MCX_VECTOR_INDEX
      ((float*)htime)[*id]=mcx_nextafterf(convert_float_rte(((float*)htime)[*id]),  (((float*)v)[*id] > 0.f)-(((float*)v)[*id] < 0.f));
#else
      (*id==0) ?
          (htime[0].x=mcx_nextafterf(convert_float_rte(htime[0].x), (v[0].x > 0.f)-(v[0].x < 0.f))) :
	  ((*id==1) ? 
	  	(htime[0].y=mcx_nextafterf(convert_float_rte(htime[0].y), (v[0].y > 0.f)-(v[0].y < 0.f))) :
		(htime[0].z=mcx_nextafterf(convert_float_rte(htime[0].z), (v[0].z > 0.f)-(v[0].z < 0.f))) );
#endif
      return dist;
}

#else

half mcx_nextafter_half(const half a, short dir){
      union{
          half f;
          short i;
      } num;
      num.f=a;
      ((num.i & 0x7FFFU)==0) ? num.i =(((dir & 0x8000U) ) | 1) : ((num.i & 0x8000U) ? (num.i-=dir) : (num.i+=dir) );
      return num.f;
}

float hitgrid(float4 *p, float4 *v0, half4 *htime, int *id){
      half dist;
      half4 p0, v;

      p0=convert_half4(p[0]);
      v=convert_half4(v0[0]);

      //time-of-flight to hit the wall in each direction

      htime[0]=fabs(floor(p0)-convert_half4(isgreater(v,((half4)(0.f))))-p0);
      htime[0]=fabs((htime[0]+(half4)EPS)/v);

      //get the direction with the smallest time-of-flight
      dist=fmin(fmin(htime[0].x,htime[0].y),htime[0].z);
      (*id)=(dist==htime[0].x?0:(dist==htime[0].y?1:2));

      htime[0]=p0+(half4)(dist)*v;

#ifdef MCX_VECTOR_INDEX
      ((half*)htime)[*id]=mcx_nextafter_half(round(((half*)htime)[*id]), (((half*)&v)[*id] > 0.f)-(((half*)&v)[*id] < 0.f));
#else
      (*id==0) ?
          (htime[0].x=mcx_nextafter_half(round(htime[0].x), (v.x > 0.f)-(v.x < 0.f))) :
	  ((*id==1) ? 
	  	(htime[0].y=mcx_nextafter_half(round(htime[0].y), (v.y > 0.f)-(v.y < 0.f))) :
		(htime[0].z=mcx_nextafter_half(round(htime[0].z), (v.z > 0.f)-(v.z < 0.f))) );
#endif
      return convert_float(dist);
}

#endif

void rotatevector(float4 *v, float stheta, float ctheta, float sphi, float cphi){
      if( v[0].z>-1.f+EPS && v[0].z<1.f-EPS ) {
   	  float tmp0=1.f-v[0].z*v[0].z;
   	  float tmp1=stheta*rsqrt(tmp0);
   	  *((float4*)v)=(float4)(
   	       tmp1*(v[0].x*v[0].z*cphi - v[0].y*sphi) + v[0].x*ctheta,
   	       tmp1*(v[0].y*v[0].z*cphi + v[0].x*sphi) + v[0].y*ctheta,
   	      -tmp1*tmp0*cphi                          + v[0].z*ctheta,
   	       v[0].w
   	  );
      }else{
   	  v[0]=(float4)(stheta*cphi,stheta*sphi,(v[0].z>0.f)?ctheta:-ctheta,v[0].w);
      }
      v[0].xyz=v[0].xyz*rsqrt(v[0].x*v[0].x+v[0].y*v[0].y+v[0].z*v[0].z);
      GPUDEBUG(((__constant char*)"new dir: %10.5e %10.5e %10.5e\n",v[0].x,v[0].y,v[0].z));
}

void transmit(float4 *v, float n1, float n2,int flipdir){
      float tmp0=n1/n2;
      v[0].xyz*=tmp0;

      (flipdir==0) ?
          (v[0].x=sqrt(1.f - v[0].y*v[0].y - v[0].z*v[0].z)*((v[0].x>0.f)-(v[0].x<0.f))):
	  ((flipdir==1) ? 
	      (v[0].y=sqrt(1.f - v[0].x*v[0].x - v[0].z*v[0].z)*((v[0].y>0.f)-(v[0].y<0.f))):
	      (v[0].z=sqrt(1.f - v[0].x*v[0].x - v[0].y*v[0].y)*((v[0].z>0.f)-(v[0].z<0.f))));
}

int launchnewphoton(float4 *p,float4 *v,float4 *f,FLOAT4VEC *prop,uint *idx1d,
           uint *mediaid,float *w0,uint isdet, __local float *ppath,float *energyloss,float *energylaunched,
	   __global float *n_det,__global uint *dpnum, __constant float4 *gproperty,
	   __constant float4 *gdetpos,__constant MCXParam *gcfg,int threadid, int threadphoton, 
	   int oddphotons, __local int *blockphoton){
      
      if(p[0].w>=0.f){
          *energyloss+=p[0].w;  // sum all the remaining energy
	  p[0].w=0.f;
#ifdef GROUP_LOAD_BALANCE
          if(f[0].w<0.f || atomic_sub(blockphoton,1)<=1)
              return 1;
          if(blockphoton[0]<get_local_size(0) && get_local_id(0))
              f[0].w=-2.f;
#endif
#ifdef MCX_SAVE_DETECTORS
          // let's handle detectors here
          if(gcfg->savedet){
             if(*mediaid==0 && isdet){
	          savedetphoton(isdet>>16, n_det,dpnum,v[0].w,ppath,p,gdetpos,gcfg);
	     }
	     clearpath(ppath,gcfg);
          }
#endif
      }

#ifndef GROUP_LOAD_BALANCE
      if(f[0].w>=(threadphoton+(threadid<oddphotons)))
         return 1; // all photons complete 
#endif
      p[0]=gcfg->ps;
      v[0]=gcfg->c0;
      f[0]=(float4)(0.f,0.f,gcfg->minaccumtime,f[0].w+1);
      *idx1d=gcfg->idx1dorig;
      *mediaid=gcfg->mediaidorig;
      prop[0]=TOFLOAT4(gproperty[*mediaid & MED_MASK]); //always use mediaid to read gproperty[]
      *energylaunched+=p[0].w;
      *w0=p[0].w;
      return 0;
}

/*
   this is the core Monte Carlo simulation kernel, please see Fig. 1 in Fang2009
*/
__kernel void mcx_main_loop(const int nphoton, const int ophoton,__global const uint *media,
     __global float *field, __global float *genergy, __global uint *n_seed,
     __global float *n_det,__constant float4 *gproperty,
     __constant float4 *gdetpos, __global uint *stopsign,__global uint *detectedphoton,
     __local float *sharedmem, __constant MCXParam *gcfg){

     int idx= get_global_id(0);

     float4 p={0.f,0.f,0.f,-1.f};  //{x,y,z}: x,y,z coordinates,{w}:packet weight
     float4 v=gcfg->c0;  //{x,y,z}: ix,iy,iz unitary direction vector, {w}:total scat event
     float4 f={0.f,0.f,0.f,0.f};  //f.w can be dropped to save register
     float  energyloss=0.f;
     float  energylaunched=0.f;

     uint idx1d, idx1dold;   //idx1dold is related to reflection

     uint   mediaid=gcfg->mediaidorig,mediaidold=0,isdet=0;
     float  w0;
     float  n1;   //reflection var
     int flipdir=0;

     //for MT RNG, these will be zero-length arrays and be optimized out
     RandType t[RAND_BUF_LEN];
     FLOAT4VEC prop;    //can become float2 if no reflection

     __local float *ppath=sharedmem+get_local_id(0)*gcfg->maxmedia;
     __local int   blockphoton[1];

#ifdef GROUP_LOAD_BALANCE
     if(get_local_id(0) == 0)
	blockphoton[0] = gcfg->blockphoton + ((int)get_group_id(0) < gcfg->blockextra);
     barrier(CLK_LOCAL_MEM_FENCE);
#endif

#ifdef  MCX_SAVE_DETECTORS
     if(gcfg->savedet) clearpath(ppath,gcfg);
#endif

     gpu_rng_init(t,n_seed,idx);

     if(launchnewphoton(&p,&v,&f,&prop,&idx1d,&mediaid,&w0,0,ppath,
		      &energyloss,&energylaunched,n_det,detectedphoton,gproperty,gdetpos,gcfg,idx,nphoton,ophoton,blockphoton)){
         n_seed[idx]=NO_LAUNCH;
         return;
     }
#ifdef GROUP_LOAD_BALANCE
     while(blockphoton[0]>0 || f.w<0.f) {
#else
     while(f.w<=nphoton + (idx<ophoton)) {
#endif
          GPUDEBUG(((__constant char*)"photonid [%d] L=%f w=%e medium=%d\n",(int)f.w,f.x,p.w,mediaid));

	  if(f.x<=0.f) {  // if this photon has finished the current jump
   	       f.x=rand_next_scatlen(t);

               GPUDEBUG(((__constant char*)"scat L=%f RNG=[%e %e %e] \n",f.x,rand_next_aangle(t),rand_next_zangle(t),rand_uniform01(t)));

	       if(p.w<1.f){ //weight
                       //random arimuthal angle
                       float cphi,sphi,theta,stheta,ctheta;
                       float tmp0=TWO_PI*rand_next_aangle(t); //next arimuth angle
                       MCX_SINCOS(tmp0,sphi,cphi);
                       GPUDEBUG(((__constant char*)"scat phi=%f\n",tmp0));

                       //Henyey-Greenstein Phase Function, "Handbook of Optical 
                       //Biomedical Diagnostics",2002,Chap3,p234, also see Boas2002

                       if(prop.z>EPS){  //if prop.z is too small, the distribution of theta is bad
		           tmp0=(1.f-prop.z*prop.z)/(1.f-prop.z+2.f*prop.z*rand_next_zangle(t));
		           tmp0*=tmp0;
		           tmp0=(1.f+prop.z*prop.z-tmp0)/(2.f*prop.z);

                           // when ran=1, CUDA will give me 1.000002 for tmp0 which produces nan later
                           // detected by Ocelot,thanks to Greg Diamos,see http://bit.ly/cR2NMP
                           tmp0=max(-1.f, min(1.f, tmp0));

		           theta=acos(tmp0);
		           stheta=MCX_MATHFUN(sin)(theta);
		           ctheta=tmp0;
                       }else{
			   theta=acos(2.f*rand_next_zangle(t)-1.f);
                           MCX_SINCOS(theta,stheta,ctheta);
                       }
                       GPUDEBUG(((__constant char*)"scat theta=%f\n",theta));
                       rotatevector(&v,stheta,ctheta,sphi,cphi);
                       v.w+=1.f;
	       }
	  }

	  n1=prop.w;
	  prop=TOFLOAT4(gproperty[mediaid & MED_MASK]);

          FLOAT4VEC htime;            //reflection var
	  f.z=hitgrid(&p, &v, &htime, &flipdir);
	  float slen=f.z*prop.y;
	  slen=fmin(slen,f.x);
	  f.z=native_divide(slen,prop.y);

          GPUDEBUG(((__constant char*)"p=[%f %f %f] -> <%f %f %f>*%f -> hit=[%f %f %f] flip=%d\n",p.x,p.y,p.z,v.x,v.y,v.z,f.z,htime.x,htime.y,htime.z,flipdir));

#ifdef USE_HALF
	  p.xyz = (slen==f.x) ? p.xyz+(float3)(f.z)*v.xyz : convert_float3(htime.xyz);
#else
	  p.xyz = (slen==f.x) ? p.xyz+(float3)(f.z)*v.xyz : htime.xyz;
#endif
	  p.w*=MCX_MATHFUN(exp)(-prop.x*f.z);
	  f.x-=slen;
	  f.y+=f.z*prop.w*gcfg->oneoverc0;

          GPUDEBUG(((__constant char*)"update p=[%f %f %f] -> f.z=%f\n",p.x,p.y,p.z,f.z));

#ifdef MCX_SAVE_DETECTORS
          if(gcfg->savedet)
	      ppath[(mediaid & MED_MASK)-1]+=f.z; //(unit=grid)
#endif

          mediaidold=mediaid | isdet;
          idx1dold=idx1d;
          idx1d=((int)floor(p.z)*gcfg->dimlen.y+(int)floor(p.y)*gcfg->dimlen.x+(int)floor(p.x));
          GPUDEBUG(((__constant char*)"idx1d [%d]->[%d]\n",idx1dold,idx1d));
#ifdef MCX_SIMPLIFY_BRANCH
	  mediaid=(any(isless(p.xyz,(float3)(0.f))) || any(isgreaterequal(p.xyz,(gcfg->maxidx.xyz))));
	  mediaid=mediaid ? 0 : media[idx1d];
          isdet=mediaid & DET_MASK;
          mediaid &= MED_MASK;
#else
          if(any(isless(p.xyz,(float3)(0.f))) || any(isgreaterequal(p.xyz,(gcfg->maxidx.xyz)))){
	      mediaid=0;
	  }else{
              mediaid=media[idx1d];
              isdet=mediaid & DET_MASK;
              mediaid &= MED_MASK;
          }
#endif

          GPUDEBUG(((__constant char*)"medium [%d]->[%d]\n",mediaidold,mediaid));

	  if(idx1d!=idx1dold && idx1dold>0 && mediaidold){
             GPUDEBUG(((__constant char*)"field add to %d->%f(%d)\n",idx1dold,w0-p.w,(int)f.w));
             // if t is within the time window, which spans cfg->maxgate*cfg->tstep wide
             if(gcfg->save2pt && f.y>=gcfg->twin0 && f.y<gcfg->twin1){
                  GPUDEBUG(((__constant char*)"deposit to [%d] %e, w=%f\n",idx1dold,w0-p.w,p.w));
#ifndef USE_ATOMIC
                  field[idx1dold+(int)(floor((f.y-gcfg->twin0)*gcfg->Rtstep))*gcfg->dimlen.z]+=w0-p.w;
#else
		  atomicadd(& field[idx1dold+(int)(floor((f.y-gcfg->twin0)*gcfg->Rtstep))*gcfg->dimlen.z], w0-p.w);
                  GPUDEBUG(((__constant char*)"atomic write to [%d] %e, w=%f\n",idx1dold,weight,p.w));
#endif
	     }
	     w0=p.w;
	  }

          if((mediaid==0 && (!gcfg->doreflect || (gcfg->doreflect && n1==gproperty[mediaid & MED_MASK].w))) || f.y>gcfg->twin1){
                  GPUDEBUG(((__constant char*)"direct relaunch at idx=[%d] mediaid=[%d], ref=[%d]\n",idx1d,mediaid,gcfg->doreflect));
		  if(launchnewphoton(&p,&v,&f,&prop,&idx1d,&mediaid,&w0,(mediaidold & DET_MASK),ppath,
		      &energyloss,&energylaunched,n_det,detectedphoton,gproperty,gdetpos,gcfg,idx,nphoton,ophoton,blockphoton)){ 
                         break;
		  }
                  isdet=mediaid & DET_MASK;
                  mediaid &= MED_MASK;
                  continue;
          }
#ifdef MCX_DO_REFLECTION
          //if hit the boundary, exceed the max time window or exit the domain, rebound or launch a new one
          if(gcfg->doreflect && n1!=gproperty[mediaid & MED_MASK].w){
	          float Rtotal=1.f;
                  float cphi,sphi,stheta,ctheta;

                  *((float4*)(&prop))=gproperty[mediaid & MED_MASK]; // optical property across the interface

                  float tmp0=n1*n1;
                  float tmp1=prop.w*prop.w;
                  cphi=fabs( (flipdir==0) ? v.x : (flipdir==1 ? v.y : v.z)); // cos(si)
                  sphi=1.f-cphi*cphi;            // sin(si)^2

                  f.z=1.f-tmp0/tmp1*sphi;   //1-[n1/n2*sin(si)]^2
	          GPUDEBUG(((__constant char*)"ref total ref=%f\n",f.z));

                  if(f.z>0.f) {
                     ctheta=tmp0*cphi*cphi+tmp1*f.z;
                     stheta=2.f*n1*prop.w*cphi*sqrt(f.z);
                     Rtotal=(ctheta-stheta)/(ctheta+stheta);
       	       	     ctheta=tmp1*cphi*cphi+tmp0*f.z;
       	       	     Rtotal=(Rtotal+(ctheta-stheta)/(ctheta+stheta))*0.5f;
		     GPUDEBUG(((__constant char*)"Rtotal=%f\n",Rtotal));
                  }
	          if(Rtotal<1.f && rand_next_reflect(t)>Rtotal){ // do transmission
                        transmit(&v,n1,prop.w,flipdir);
                        if(mediaid==0){ // transmission to external boundary
                            GPUDEBUG(((__constant char*)"transmit to air, relaunch\n"));
		    	    if(launchnewphoton(&p,&v,&f,&prop,&idx1d,&mediaid,&w0,(mediaidold & DET_MASK),
			        ppath,&energyloss,&energylaunched,n_det,detectedphoton,gproperty,gdetpos,gcfg,idx,nphoton,ophoton,blockphoton)){
                                    break;
			    }
                            isdet=mediaid & DET_MASK;
                            mediaid &= MED_MASK;
			    continue;
			}
	                GPUDEBUG(((__constant char*)"do transmission\n"));
		  }else{ //do reflection
	                GPUDEBUG(((__constant char*)"do reflection\n"));
	                GPUDEBUG(((__constant char*)"ref faceid=%d p=[%f %f %f] v_old=[%f %f %f]\n",flipdir,p.x,p.y,p.z,v.x,v.y,v.z));
                	(flipdir==0) ? (v.x=-v.x) : ((flipdir==1) ? (v.y=-v.y) : (v.z=-v.z)) ;
			(flipdir==0) ?
        		    (p.x=mcx_nextafterf(convert_float_rte(p.x), (v.x > 0.f)-0.5f)) :
			    ((flipdir==1) ? 
				(p.y=mcx_nextafterf(convert_float_rte(p.y), (v.y > 0.f)-0.5f)) :
				(p.z=mcx_nextafterf(convert_float_rte(p.z), (v.z > 0.f)-0.5f)) );
	                GPUDEBUG(((__constant char*)"ref p_new=[%f %f %f] v_new=[%f %f %f]\n",p.x,p.y,p.z,v.x,v.y,v.z));
                	idx1d=idx1dold;
		 	mediaid=(media[idx1d] & MED_MASK);
			prop=TOFLOAT4(gproperty[mediaid]);
			n1=prop.w;
		  }
              }
#endif
     }
     genergy[idx<<1]=energyloss+p.w;
     genergy[(idx<<1)+1]=energylaunched;
}

