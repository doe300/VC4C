# Status

[![CircleCI](https://circleci.com/gh/doe300/VC4C.svg?style=svg)](https://circleci.com/gh/doe300/VC4C)

# VC4C

Compiler for the [VC4CL](https://github.com/doe300/VC4CL) OpenCL-implementation.
This compiler supports OpenCL C (via LLVM or [SPIRV-LLVM](https://github.com/KhronosGroup/SPIRV-LLVM)), LLVM-IR and SPIR-V code, depending on the build configuration.

## Required software

- A C++11 capable compiler
- CMake in version >= 3.1
- A suitable OpenCL 1.2 compiler. Supported versions are the "original" LLVM/CLang (version 3.9 and up), which can be found in the Raspbian repositories, and Khronos [SPIRV-LLVM](https://github.com/KhronosGroup/SPIRV-LLVM) with the Khronos [SPIR-V compiler frontend](https://github.com/KhronosGroup/SPIR/tree/spirv-1.0) (only the tools clang and llvm-spirv need to be built, **using this one is recommended**).
- The source-code for [VC4CLStdLib](https://github.com/doe300/VC4CLStdLib) for the GPU-side standard-library

## Build

The following configuration variables can be set in CMake:

- `BUILD_TESTING` toggles building of test program
- `BUILD_DEBUG` toggles building debug or release program
- `MULTI_THREADED` toggles building with multi-threaded support
- `VERIFY_OUTPUT` toggles the usage of [vc4asm](https://github.com/maazl/vc4asm) to validate the generated machine code
- `LLVMIR_FRONTEND` toggles building of the LLVM-IR frontend, requires an installed CLang compiler (LLVM or SPIRV-LLVM)
- `VC4CL_STDLIB_HEADER_SOURCE` sets the headers for the GPU-side [VC4CLStdLib](https://github.com/doe300/VC4CLStdLib), defaults to `../VC4CLStdLib/include/VC4CLStdLib.h`
- `CROSS_COMPILE` toggles whether to cross-compile for the Raspberry Pi, requires the [Raspberry Pi cross-compiler](https://github.com/raspberrypi/tools) to be installed
- `CROSS_COMPILER_PATH` sets the root path to the Raspberry Pi cross compiler, defaults to `/opt/rasperrypi/tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64` (e.g. for the cross compiler cloned into the directory `/opt/raspberrypi/tools/`)
- `SPIRV_FRONTEND` toggles building of the SPIR-V frontend, requires SPIRV-LLVM
- `SPIRV_COMPILER_ROOT` sets the root-path to binaries of the [SPIRV-LLVM](https://github.com/KhronosGroup/SPIRV-LLVM) compiler, defaults to `/opt/SPIRV-LLVM/build/bin/`

## Known Issues

If the [VC4CLStdLib](https://github.com/doe300/VC4CLStdLib) is updated, the LLVM precompiled header (PCH) needs to be rebuilt. For this to happen, simply delete the file `include/VC4CLStdLib.h.pch` and rebuild the VC4C compiler (or just the `vc4cl-stdlib` target).

Sometimes, at least on my Raspberry Pi, if a compilation fails, it somehow removes the symbolic `/dev/stdout` to the current process' standard output, resulting in no program can write to stdout anymore!! To remedy, restart the Pi. See [here](https://github.com/doe300/VC4C/issues/3) for the status of the issue.
