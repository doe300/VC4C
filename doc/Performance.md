
## Overall Performance

* QPU has 16-way virtual SIMD, implemented by 4-way data parallelism executed 4 cycles in a row
* QPU clock runs at 1/4 of the system clock (in the Raspberry Pi this is usually fixed to 250MHz)
* The Raspberry Pi VideoCore IV GPU has 12 QPUs
* Each QPU has 2 asymmetric ALUs (ADD and MUL)
* This results in a theoretical performance of 24GFLOPs: 2 ALUs \* 4 way data-parallelism \* 250MHz \* 12 QPUs
* The VC4C compiler supports scalar and vector-types with 2, 4, 8 or 16 elements. Since a QPU is a 16-way virtual SIMD, 16-element vectors yield the best performance.
* A branch takes 4 cycles to execute

**&rArr; Prefer to use the 16-element vector-type**

### Integer Performance

* Division and Remainder (signed or unsigned) are not natively supported and are implemented in software (very slow!) taking XXX cycles
* The VideoCore IV supports a 24-bit multiplication, but no 32-bit multiplication. The full integer multiplication is emulated via several mul24-instructions.

**&rArr; Prefer mul24 over integer-multiplication, if possible**

### Floating Performance

* Division is not natively supported, but is emulated in software taking XXX cycles
* Special functions (RECIP, RECIP_SQRT, EXP and LOG) are handled via a SFU (special function unit) and take 4 clock cycles

### Memory Performance

* All QPUs share the same VPM (vertex pipeline memory), thus heavy I/O activity greatly dampens performance on parallel tasks.
* To prevent race conditions, all read/write access to the VPM is guarded by a hardware-mutex shared between all QPUs

**&rArr; Only access the memory to get the arguments and to store the result. Use local variables for all temporary results**

### Other Performance

* The ternary operator `(x ? y : z)` only costs about 4 instructions overhead, whereas an if-else construct takes at least 8 instructions 