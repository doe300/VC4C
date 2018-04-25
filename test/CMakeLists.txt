add_executable(TestVC4C "")
include(sources.list)
target_link_libraries(TestVC4C VC4CC ${SYSROOT_LIBRARY_FLAGS})

if(BUILD_DEBUG)
	target_compile_definitions(TestVC4C PRIVATE DEBUG_MODE=1)
endif(BUILD_DEBUG)

target_include_directories (TestVC4C PRIVATE "${PROJECT_SOURCE_DIR}/src")
target_include_directories (TestVC4C PRIVATE "${PROJECT_SOURCE_DIR}/include")
target_compile_definitions(TestVC4C PRIVATE CPPLOG_NAMESPACE=logging CPPLOG_CUSTOM_LOGGER=true)

add_dependencies(TestVC4C cpptest-lite-project-build)
add_library(cpptest-lite SHARED IMPORTED)
ExternalProject_Get_Property(cpptest-lite-project BINARY_DIR)
set_target_properties(cpptest-lite PROPERTIES IMPORTED_LOCATION "${BINARY_DIR}/libcpptest-lite.so")
target_link_libraries(TestVC4C cpptest-lite)
target_include_directories (TestVC4C PRIVATE "${PROJECT_SOURCE_DIR}/lib/cpptest-lite/include")


add_test(NAME Regressions COMMAND ./build/test/TestVC4C --fast-regressions WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME Emulator COMMAND ./build/test/TestVC4C --test-emulator WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME Instructions COMMAND ./build/test/TestVC4C --test-instructions WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME Operators COMMAND ./build/test/TestVC4C --test-operators WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME Parser COMMAND ./build/test/TestVC4C --test-parser WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME Scanner COMMAND ./build/test/TestVC4C --test-scanner WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME Stdlib COMMAND ./build/test/TestVC4C --test-stdlib WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
