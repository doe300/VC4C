#!/bin/sh
# Run from project root directly
# The arguments are passed verbatim to CMake

SCAN_BUILD=$(which scan-build)

mkdir -p build_clang_analyzer
cd build_clang_analyzer
rm -f CMakeCache.txt
make clean || true
$SCAN_BUILD cmake -DCMAKE_BUILD_TYPE=Debug $* ../
$SCAN_BUILD \
    --status-bugs \
    -enable-checker alpha.core.BoolAssignment \
    -enable-checker alpha.security.ArrayBoundV2 \
    -enable-checker nullability.NullableDereferenced \
    -enable-checker optin.performance.Padding \
    -enable-checker optin.portability.UnixAPI \
    -enable-checker security.FloatLoopCounter \
    -enable-checker security.insecureAPI.strcpy \
    make -j$(nproc)