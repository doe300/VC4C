include_directories(../src)
include_directories(../include)
include_directories(../lib/cpplog/include)

#Include the source files
file( GLOB EMULATOR_SRCS emulator.cpp)

add_executable(qpu_emulator ${EMULATOR_SRCS})
target_link_libraries(qpu_emulator VC4CC)
