cmake_minimum_required (VERSION 3.1)
include(${CMAKE_ROOT}/Modules/ExternalProject.cmake)
# XXX required as long as we need to retrieve the VC4C compiler output path for the install precompiled headers command
cmake_policy(SET CMP0026 OLD)

####
# General configuration
####
# Option to enable/disable test-program
option(BUILD_TESTING "Build testing program" ON)
# Option to enable/disable multi-threaded compilation
option(MULTI_THREADED "Runs the optimization steps multi-threaded" ON)
# Option whether to verify the output (using vc4asm)
option(VERIFY_OUTPUT "uses the external library vc4asm to verify the output" ON)
# Option to enable/disable cross compilation
option(CROSS_COMPILE "Cross compile for Raspbian" OFF)
# Option whether to include the SPIR-V Tools front-end
option(SPIRV_FRONTEND "Enables a second front-end for the SPIR-V intermediate language" OFF)
# Option whether to include the LLVM library front-end. This requires the LLVM development-headers to be available for the (SPIRV-)LLVM used
option(LLVMLIB_FRONTEND "Enables the front-end using the LLVM library to read LLVM modules" ON)
# Option whether to enable code coverage analysis via gcov
option(ENABLE_COVERAGE "Enables collection of code coverage via gcov" OFF)
# Option whether to enable use of the CLang library (EXPERIMENTAL)
option(CLANG_LIBRARY "Uses the libclang library for compilation, uses the clang executable otherwise" OFF)
# Option whether to enable more compile-time checks
option(ADVANCED_CHECKS "Enable advanced compile-time checks" OFF)
# Option to skipping pre-compiling the VC4CLStdLib standard-library headers at compile time. NOTE: The headers still need to be pre-compiled before execution!
option(VC4CL_STDLIB_PRECOMPILE "Enable pre-compiling of the VC4CLStdLib headers at compile time" ON)

# Path to the VC4CL standard library
# NOTE: Resolving ~ (for home directory) is currently not supported
if(NOT VC4CL_STDLIB_DIR)
	find_file(VC4CL_STDLIB_FOUND "VC4CLStdLib/include/VC4CLStdLib.h")
	if(VC4CL_STDLIB_FOUND)
		get_filename_component(VC4CL_STDLIB_DIR ${VC4CL_STDLIB_FOUND} DIRECTORY)
		message(STATUS "VC4CL standard library headers found: ${VC4CL_STDLIB_DIR}")
	elseif(EXISTS "${CMAKE_SOURCE_DIR}/../VC4CLStdLib/include/VC4CLStdLib.h")
		set(VC4CL_STDLIB_DIR "${CMAKE_SOURCE_DIR}/../VC4CLStdLib/include/")
		message(STATUS "VC4CL standard library headers found: ${VC4CL_STDLIB_DIR}")
	else()
		message(WARNING "No development version of VC4CL standard-library headers found, will not automatically generate the precompiled files!")
	endif()
endif()

if(CROSS_COMPILE)
	include(cmake/crosscompile.cmake)
	message(STATUS "Cross compiling for Raspbian with compiler: ${CMAKE_CXX_COMPILER}")
endif()

if(NOT BUILD_NUMBER)
	set(BUILD_NUMBER 9999)
endif()

project (VC4C VERSION 0.4.${BUILD_NUMBER})

# Set C++ standard to C++14 without any extensions (e.g. GNU)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# Set default build type to Debug
if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Choose the type of build." FORCE)
endif()

if("${CMAKE_BUILD_TYPE}" STREQUAL "Release")
	# Only allow coverage for debug builds
	set(ENABLE_COVERAGE OFF)
else()
	set(BUILD_DEBUG ON)
endif()

# Set optimization and warning flags for the build types
include(cmake/flags.cmake)

if(MULTI_THREADED)
	message(STATUS "Enabling multi-threaded optimizations")
	find_package(Threads)
endif()

# clang-tidy
find_program(CLANG_TIDY NAMES clang-tidy clang-tidy-5.0 clang-tidy-6.0 clang-tidy-7 clang-tidy-8 clang-tidy-9 clang-tidy-10)
if(ADVANCED_CHECKS AND CLANG_TIDY)
    message(STATUS "Enabling clang-tidy compile time checks: ${CLANG_TIDY}")
    set(CMAKE_CXX_CLANG_TIDY "${CLANG_TIDY}")
endif()

####
# Dependencies
####

if((CMAKE_MAJOR_VERSION VERSION_GREATER 3) OR ((CMAKE_MAJOR_VERSION VERSION_EQUAL 3) AND (CMAKE_MINOR_VERSION VERSION_GREATER_EQUAL 14)))
	# CMake 3.14 introduces https://cmake.org/cmake/help/latest/module/FetchContent.html which allows us to run the configuration step
	# of the downloaded dependencies at CMake configuration step and therefore, we have the proper targets available.
	message(STATUS "Using CMake 3.14+ FetchContent to include dependencies...")
	set(DEPENDENCIES_USE_FETCH_CONTENT ON)
else()
	message(STATUS "Using CMake <3.14 ExternalProject to include dependencies...")
	set(DEPENDENCIES_USE_FETCH_CONTENT OFF)
endif()

if(BUILD_OFFLINE)
	# A result of != 0 is an error, so disable updating
	set_property(DIRECTORY ${VC4C_SOURCE_DIR} PROPERTY EP_UPDATE_DISCONNECTED 1)
	message(WARNING "Building in off-line mode, some dependencies might not be up-to-date!")
else()
	set_property(DIRECTORY ${VC4C_SOURCE_DIR} PROPERTY EP_UPDATE_DISCONNECTED 0)
endif()

####
# Determine Clang version and configure front-ends
###

# Prefer Khronos OpenCL to LLVM IR (to SPIR-V) compiler, always include SPIR-V headers
include(cmake/spirv-headers.cmake)
include(cmake/spirv.cmake)

# Fall back to "standard" CLang
include(cmake/clang.cmake)

# If enabled, check whether the LLVM library front-end can be built
if(LLVMLIB_FRONTEND)
	include(cmake/libllvm.cmake)
endif()

if(NOT ((SPIRV_LLVM_SPIR_FOUND AND SPIRV_FRONTEND) OR (LLVMLIB_FRONTEND AND LLVM_LIBS_PATH)))
	message(WARNING " Neither SPIR-V nor LLVM library front-end are configured!")
endif()

if(NOT SPIRV_CLANG_FOUND AND NOT CLANG_FOUND)
	message(FATAL_ERROR "No supported OpenCL compiler found!")
endif()

if(VERIFY_OUTPUT)
	include(cmake/vc4asm.cmake)
endif()
include(cmake/cpplog.cmake)
# Variant backport for C++ < 17
include(cmake/variant.cmake)

####
# Additional configuration
####
include(cmake/sanitizers.cmake)

####
# Main files
####
#build all from ./src into ./build
add_subdirectory(src)
add_subdirectory(tools)

if (BUILD_TESTING)
	enable_testing()
	include(cmake/cpptest-lite.cmake)
    add_subdirectory(test)
endif (BUILD_TESTING)

include(cmake/packaging.cmake)
