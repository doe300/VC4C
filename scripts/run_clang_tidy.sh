#!/bin/sh
# Run from build directly (where the compilation_commands.json) is located

# Skip all external headers (all headers located in lib)
HEADER_FILTER="^((?!lib).)*$"

python /usr/share/clang/run-clang-tidy.py \
    -j $(nproc) \
    -header-filter=$HEADER_FILTER \
    -checks="*, \
        -fuchsia-*, \
        -hicpp-*, \
        -abseil-string-find-startswith, \
        -misc-unused-parameters,-misc-macro-parentheses, \
        -google-readability-*,-google-runtime-references,-google-build-using-namespace,-google-runtime-int, \
        -readability-implicit-bool-conversion,-readability-braces-around-statements,-readability-else-after-return,-readability-redundant-member-init, \
        -modernize-use-auto,-modernize-pass-by-value,-modernize-make-unique,-modernize-return-braced-init-list,-modernize-use-transparent-functors, \
        -cert-err58-cpp, \
        -cppcoreguidelines-owning-memory,-cppcoreguidelines-pro-bounds-pointer-arithmetic,-cppcoreguidelines-pro-bounds-constant-array-index,
        -cppcoreguidelines-pro-type-reinterpret-cast,-cppcoreguidelines-pro-bounds-array-to-pointer-decay, \
        -llvm-header-guard" \
    $1
# see: https://clang.llvm.org/extra/clang-tidy/
# and: https://clang.llvm.org/extra/clang-tidy/checks/list.html
