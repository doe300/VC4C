## VideoCore IV GPU

### Components

* 12 QPUs
* 3 SFUs
* 16 semaphores
* 1 global hardware mutex
* L2 cache, separate instruction and uniform cache
* 1 VPM
* 6 TMU


### QPU

- Quad Processing Unit, a processor core. 
- The Raspberry Pi has 12 QPUs, 3 "slices" with 4 cores each.
- Has a 16-way virtual parallelism (16-way SIMD) by executing an instruction with a 4-way data parallelism, 4 cycles in a row 
- 4-stage pipeline, thus every instruction takes 4 cycles. But since every cycle an instruction is started/finished, it executes 1 instruction per clock cycle
- Fixed clock rate of 250MHz (300MHz for newer models)
- each register has 512 bit (16-way SIMD, 4 Byte per element)
- 2 physical register banks with 32 general purpose registers each, 5 accumulators of which 2 have special functions
- Additional hardware is accessed via special instructions (only semaphores) or 64 additional registers for periphery access
- 2 asymmetric ALUs
- 1 ALU for "addition" operations: add/sub, min/max (float and int), bitwise-operators, etc.
- 1 ALU for "multiply" operations: fmul, mul24 (24-bit integer multiplication), ...
- no hardware-support for division (float or int), modulo, integer-multiplication

### SFU

- Special Functions Unit
- One per slice, so the VideoCore IV in the Raspberry Pi models has  QPUs
- Calculation of mathematical functions: 2^x, log2(x), 1/sqrt(x), 1/x
- inaccurate approximation, accuracy of 11 to 13 binary digits (relative error of 2048 to 8192 ULP)
- Calculation is initiated by writing into special periphery-registers
- Calculation takes 4 cycles, after which result is available at one of the accumulators

### Semaphores

- 16 global semaphores, all QPUs share access to
- special instruction to increment/decrement semaphore
- 4 bit counting, blocks if decremented beneath zero, incremented above 15

### Mutex

- single hardware binary mutex
- used to synchronize access to VPM

### VPM

- Vertex Pipe Memory
- component used to read/write RAM
- contains internal cache of 4KB to 12KB
- accessed RAM via DMA (no MMU between GPU and RAM)
- shared between all QPUs
- very flexible configuration for accessing the internal cache and RAM
- used for general purpose I/O
- rates up to 1GB/s (write) and 700MB/s (read) with 16-way 4 Byte vectors; 300MB/s (write) and 200MB/s (read) for single 32-bit values (between RAM and VPM)
- DMA access to RAM inserts a delay of at least 5 instructions
- accessing the VPM cache introduces no extra delay

### TMU

- Texture and Memory Unit
- can be used to read 16 separate values (4 Byte) from 16 separate memory addresses at once!
- supports image lookup (calculation of coordinates, image-format, interpolation)
- no write-access to RAM, very simple read access
- used to read images
- internal cache
- per-QPU request pipeline, no synchronization required?
- much slower than VPM (about 28MB/s)
- reading RAM takes 9 to 20 instructions, depending on whether the data is read from TMU cache, L2 or RAM
- two TMUs per slice, automatically configured for QPU0 and QPU1 to use TMU0, QPU1 and QPU2 to use TMU1