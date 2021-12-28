
###
# Emulator
###
add_executable(qpu_emulator emulator.cpp)
target_link_libraries(qpu_emulator VC4CC ${SYSROOT_LIBRARY_FLAGS})
target_include_directories(qpu_emulator PRIVATE "${PROJECT_SOURCE_DIR}/src")
target_include_directories(qpu_emulator PRIVATE ${variant_HEADERS})
target_compile_options(qpu_emulator PRIVATE ${VC4C_ENABLED_WARNINGS})

if(ENABLE_COVERAGE)
	target_compile_options(qpu_emulator PRIVATE -fprofile-arcs -ftest-coverage --coverage)
	target_link_libraries(qpu_emulator gcov "-fprofile-arcs -ftest-coverage")
endif(ENABLE_COVERAGE)
