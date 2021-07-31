#!/bin/bash

RET=0
DIR=$(readlink -f $(dirname $0)/..)

echo "Running clang-format in:"
clang-format --version

for file in src/*.cpp src/*/*.cpp include/*.h src/*.h src/*/*.h; do
    clang-format $file > /tmp/file
    A=$(diff $file /tmp/file)
    if [ "$?" == 1 ]; then
        echo "file: $file"
        echo "$A"
    fi
done

exit "${RET}"
