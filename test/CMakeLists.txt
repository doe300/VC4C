include_directories(../src)
include_directories(../include)

#Include the source files
file( GLOB SRCS *.cpp */*.cpp *.h */*.h)

include_directories ("${PROJECT_SOURCE_DIR}/lib/cpptest-lite/include")

add_executable(TestVC4C ${SRCS})
add_dependencies(TestVC4C cpptest-lite-project-build)
add_library(cpptest-lite SHARED IMPORTED)
ExternalProject_Get_Property(cpptest-lite-project BINARY_DIR)
set_target_properties(cpptest-lite PROPERTIES IMPORTED_LOCATION "${BINARY_DIR}/libcpptest-lite.so")
target_link_libraries(TestVC4C cpptest-lite)
target_link_libraries(TestVC4C VC4CC)
