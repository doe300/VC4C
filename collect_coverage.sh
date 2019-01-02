#/!bin/sh
lcov --directory . --capture --output-file /tmp/my_prog.info
lcov --remove /tmp/my_prog.info '/usr/include/*' '/usr/lib/*' 'lib/*' 'testing/*' -o /tmp/my_prog_filtered.info
genhtml --output-directory /tmp/coverage --show-details  --demangle-cpp --num-spaces 2 --sort --function-coverage --branch-coverage --legend   /tmp/my_prog_filtered.info
