#/!bin/sh
lcov --directory . --capture --output-file /tmp/my_prog.info
genhtml --output-directory /tmp/coverage --show-details  --demangle-cpp --num-spaces 2 --sort --function-coverage --branch-coverage --legend   /tmp/my_prog.info
