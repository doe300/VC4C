## Cross Compilation

### VC4C

Modify `CMakeLists.txt`:

* Set `CROSS_COMPILE` to `ON`
* In the `if(CROSS_COMPILE)` section, adjust the path to the [Raspberry Pi cross-compiler](https://github.com/raspberrypi/tools)

### Khronos SPIRV-LLVM
[Source](http://llvm.org/docs/HowToCrossCompileLLVM.html)

Assumptions: 

* The Khronos SPIRV-LLVM was built (for the host-architecture!) into path `/opt/SPIRV-LLVM/build`
* The SPIRV-LLVM source is in `/opt/SPIRV-LLVM/`
* The [Raspberry Pi cross-compiler](https://github.com/raspberrypi/tools) is in `/opt/rasperrypi/tools`

1. Run this command (adjust if necessary) in the directory to build the cross-compiled tools (i.e. `/tmp/SPIRV-LLVM`):
`cmake -G "Unix Makefiles" -DCMAKE_C_COMPILER=/opt/rasperrypi/tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/bin/arm-linux-gnueabihf-gcc -DCMAKE_CXX_COMPILER=/opt/rasperrypi/tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/bin/arm-linux-gnueabihf-g++ -DCMAKE_SYSTEM_NAME=Linux -DCMAKE_FIND_ROOT_PATH="/opt/rasperrypi/tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64 /opt/rasperrypi/tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/arm-linux-gnueabihf" -DCMAKE_CROSSCOMPILING=True -DLLVM_TABLEGEN=/opt/SPIRV-LLVM/build/bin/llvm-tblgen -DCLANG_TABLEGEN=/opt/SPIRV-LLVM/build/bin/clang-tblgen -DLLVM_DEFAULT_TARGET_TRIPLE=arm-linux-gnueabihf -DLLVM_TARGET_ARCH=ARM /opt/SPIRV-LLVM/`
2. Run `make clang llvm-as llvm-spirv`
3. Copy destination directory to Raspberry Pi
4. Set correct path to cross-compiled SPIRV-LLVM in VC4C `CMakeLists.txt` (setting `SPIRV_COMPILER_ROOT`)