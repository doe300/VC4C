### Types
* 64-bit data-types (**long** and **double** via `cl_khr_fp64`) are unsupported, since the VideoCore IV GPU only provides 32-bit instructions.
* The `cl_khr_fp16` **half** floating-point type is also not supported.
* Images and any associated feature (e.g. samplers) are not supported and can't be, since even embedded-profile MUST support images with *half* data-type

### Structure
Inclusion of method from other files is not supported, unless their bodies are `#include`d into the compilation unit.
This also means, the VC4C compiler is not able to **link** functions from several source files into one executable.

### Applications
**Any application which requires a work-group with more than 12 work-items**. Among others,
BulletCL, since some parts require 64 work-items to be executed in parallel (per work-group). 
Indifferent of the configuration (12 work-groups * 1 work-item or 1 work-group * 12 work-items),
the VideoCore IV cannot execute 64 work-items in parallel!
