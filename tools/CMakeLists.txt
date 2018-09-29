
###
# Emulator
###
add_executable(qpu_emulator emulator.cpp)
target_link_libraries(qpu_emulator VC4CC ${SYSROOT_LIBRARY_FLAGS})
target_include_directories(qpu_emulator PRIVATE "${PROJECT_SOURCE_DIR}/lib/cpplog/include")
target_include_directories(qpu_emulator PRIVATE "${PROJECT_SOURCE_DIR}/src")
target_include_directories(qpu_emulator PRIVATE "${PROJECT_SOURCE_DIR}/include")
target_compile_definitions(qpu_emulator PRIVATE CPPLOG_NAMESPACE=logging CPPLOG_CUSTOM_LOGGER=true)
target_include_directories(qpu_emulator PRIVATE "${PROJECT_SOURCE_DIR}/lib/variant/include")

if(BUILD_DEBUG)
	target_compile_definitions(qpu_emulator PRIVATE DEBUG_MODE=1)
endif(BUILD_DEBUG)
if(ENABLE_COVERAGE)
	target_compile_options(qpu_emulator PRIVATE -fprofile-arcs -ftest-coverage --coverage)
	target_link_libraries(qpu_emulator gcov "-fprofile-arcs -ftest-coverage")
endif(ENABLE_COVERAGE)