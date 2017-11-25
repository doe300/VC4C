#!/bin/sh
# Usage: <cmd> file-regex
for filename in $1; do
    echo "SPIRV: $filename:"
    ./build/VC4C --spirv --hex -o /dev/null "$filename" > /dev/null
    echo "LLVM: $filename:"
    ./build/VC4C --llvm --hex -o /dev/null "$filename" > /dev/null
done
