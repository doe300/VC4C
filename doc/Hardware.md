## VideoCore IV GPU

**TODO** general

[Source](http://www.codedivine.org/2014/03/03/broadcom-videocore-iv-architecture-overview/)
[Source](https://www.raspberrypi.org/forums/viewtopic.php?f=67&t=169576&p=1110785#p1110785)
[Source](https://petewarden.com/2014/08/07/how-to-optimize-raspberry-pi-code-using-its-gpu/)

### Components

* 12 QPUs
* SFUs
* 16 global 4-bit counting semaphores
* 1 global hardware mutex
* **TODO** Cache (instruction, data)
* Shared VPM


### QPU
Quad Processing Unit
**TODO** "core", 16-way virtual, 4-way data/-time parallelism

### SFU
**TODO** one per slice

### Semaphores

**TODO** if used, any other code using them will collide -> don't use??

### Mutex
**TODO** used to lock VPM

### VPM
**TODO** shared between all QPUs, needs synchronization, cache-size, has DMA into GPU-memory

### TMU
**TODO** read stream of data, unused