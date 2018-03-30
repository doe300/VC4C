# Toolchain file for cross-compiling for the Raspberry Pi
SET(CMAKE_SYSTEM_NAME 		"Linux")

SET(CMAKE_C_COMPILER   		"${CROSS_COMPILER_PATH}/arm-linux-gnueabihf-gcc")
SET(CMAKE_CXX_COMPILER 		"${CROSS_COMPILER_PATH}/arm-linux-gnueabihf-g++")

SET(CMAKE_FIND_ROOT_PATH  	"${CROSS_COMPILER_PATH}/../../")

SET(CMAKE_AR                    "${CROSS_COMPILER_PATH}/arm-linux-gnueabihf-ar")
SET(CMAKE_CXX_COMPILER_AR       "${CROSS_COMPILER_PATH}/arm-linux-gnueabihf-gcc-ar")
SET(CMAKE_CXX_COMPILER_RANLIB   "${CROSS_COMPILER_PATH}/arm-linux-gnueabihf-ranlib")
SET(CMAKE_C_COMPILER_AR         "${CROSS_COMPILER_PATH}/arm-linux-gnueabihf-gcc-ar")
SET(CMAKE_CXX_COMPILER_RANLIB   "${CROSS_COMPILER_PATH}/arm-linux-gnueabihf-ranlib")
SET(CMAKE_LINKER                "${CROSS_COMPILER_PATH}/arm-linux-gnueabihf-ld")

set(CMAKE_CROSSCOMPILING ON)