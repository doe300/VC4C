
The following list of OpenCL libraries has been taken from 
[List of libraries](http://www.iwocl.org/resources/opencl-libraries-and-toolkits/)
and 
[List of OpenCL applications](https://en.wikipedia.org/wiki/List_of_OpenCL_applications)
as well as very coarse google searches.

### Working libraries
Lists all (fully) working libraries, which have been tested in productive use.

### Partially working libraries

* [CLPeak](https://github.com/krrishnarraj/clpeak): OpenCL application to test GPU peak performance

  *NOTE: `double`, `half` and `integer` benchmarks not supported*

### Runs the tests
Lists all libraries for which all OpenCL relevant test-cases run successfully. 

### To be tested
* [cltorch](https://github.com/hughperkins/cltorch): OpenCL backend for torch NN framework
* [Caffe](https://github.com/BVLC/caffe/tree/opencl): built-in OpenCL backend for caffe NN framework
* [OpenCL-caffe](https://github.com/amd/OpenCL-caffe): OpenCL backend for caffe NN framework
* [boost compute](https://github.com/boostorg/compute): boost-library for general purpose calculations on GPU 

  *NOTE: Some kernels require `long` to be supported*
  
* [CLTune](https://github.com/CNugteren/CLTune): auto-tuning for OpenCL kernels

  *NOTE: Sample-programs allocate too much memory without checking the device memory*
* [DeepCL](https://github.com/hughperkins/DeepCL): OpenCL NN framework
* [ViennaCL](http://viennacl.sourceforge.net/): math libraries

  *NOTE: Some test-cases require fixed local sizes of more than 12 work-items*
* [clMath Libraries](https://github.com/clMathLibraries)

  *NOTE: clFFT uses too large work-group sizes*
  *NOTE: clRNG throws compilation errors: `#error "This code has only been tested on x86 and powerpc platforms."`*
  *NOTE: clBLAS fails to compile test-programs, undefined references*
* [libCL](https://github.com/jochenstier/libcl)

  *NOTE: Visual Studio only project*
* [Advanced Simulation Library](https://github.com/AvtechScientific/ASL)
* [OpenCV 3.x](https://github.com/opencv/opencv): OpenCV image manipulation

  *NOTE: some kernels require work-group size of 256 and/or double data-type*

* [vattenoverhuvudet](https://github.com/Hedlundaren/vattenoverhuvudet): Fluid Simulation

  *NOTE: Some kernels have compilation errors (in LLVM-frontend)*
  
* [spimagine](https://github.com/maweigert/spimagine): OpenCL accelerated volume rendering in python
* [gputools](https://github.com/maweigert/gputools): OpenCL accelerated volume processing in python
* [HandBrake](https://github.com/HandBrake/HandBrake): Video transcoder

  *TODO: Kernels compile, test execution*
* [bfgminer](https://github.com/luke-jr/bfgminer): Hash miner
* [rendergirl](https://github.com/henriquenj/rendergirl): OpenCL ray-tracer

  *NOTE: Visual C++ only project*
* [JohnTheRipper](https://github.com/magnumripper/JohnTheRipper): Password cracker

  *NOTE: Some kernels require `long` to be supported*
* [rodinia](https://github.com/jrmrjnck/rodinia). Benchmark suite

  *NOTE: Some kernels require `double` or `long` to be supported*
  
* [hashcat](https://github.com/hashcat/hashcat): Password recovery util

  *NOTE: Some kernels require `long` to be supported*

* [OpenCLIPP](https://github.com/CRVI/OpenCLIPP): image processing library

  *NOTE: Has build errors, unresolved references*
  
* [Paralution](http://www.paralution.com/): parallel computation library

* [VexCL](https://github.com/ddemidov/vexcl): C++ vector expression library
* [clnn](https://github.com/hughperkins/clnn): OpenCL backend for torch NN framework
* [Wolfram](https://www.wolfram.com/raspberry-pi/]: mathematical computation library with optional OpenCL support, [OpenCL library](https://reference.wolfram.com/language/OpenCLLink/guide/OpenCLLink.html)
* [Halide](https://github.com/halide/Halide): image processing DSL
* [EasyCL](https://github.com/hughperkins/EasyCL): wrapper around OpenCL for easier usage
* [Bolt](https://github.com/HSA-Libraries/Bolt): C++ template library for hetegerogeneous computing

  *NOTE: downloads and compiles all dependencies, even if packages exist*
* [paralultion](http://www.paralution.com/download/): C++ sparse iterative solver library
* [AMGCL](https://github.com/ddemidov/amgcl): C++ header-only solver for large sparse linear systems
* [CLBlast](https://github.com/CNugteren/CLBlast): Tuned OpenCL BLAS library
* [libxcam](https://github.com/01org/libxcam): image quality improvement library

  *NOTE: requires DRM library*


### Won't work
* [BulletCL (Bullet 3)](http://bulletphysics.org/wordpress/)

  *Reason: Requires 64 work-items per work-group (we only support 12)*
  *TODO: not for all kernels?*
* [cp2k](https://www.cp2k.org/): Molecular Dynamics

  *Reason: Requires the `double` data-type*
  
* [octopus](http://octopus-code.org/wiki/Main_Page): Scientific virtual experimentation

  *Reason: Requires the `double` data-type*
* [clDNN](https://github.com/01org/cldnn): Compute library for deep neuronal networks

  *Reason: "Uses several Intel extensions and requires the Intel graphics driver to run"*
  
* [MIOpen](https://github.com/ROCmSoftwarePlatform/MIOpen): AMD's Machine Intelligence Library

  *Reason: No ROCm enabled compiler for Raspbian/VideoCore IV*
 
*[Zeta](https://github.com/smatovic/Zeta): OpenCL chess engine

  *Reason: requires work-group size of 64 work-items*
  
*[WeeChess](https://github.com/EwanC/WeeChess): OpenCL chess engine

  *Reason: compilation errors in C++ code*