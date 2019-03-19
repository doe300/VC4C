cmake_minimum_required (VERSION 3.1)
include(${CMAKE_ROOT}/Modules/ExternalProject.cmake)

####
# General configuration
####
# Option to enable/disable test-program
option(BUILD_TESTING "Build testing program" ON)
# Option to enable/disable debug-output
option(BUILD_DEBUG "Build with debugging symbols. Otherwise build for performance" ON)
# Option to enable/disable multi-threaded compilation
option(MULTI_THREADED "Runs the optimization steps multi-threaded" ON)
# Option whether to verify the output (using vc4asm)
option(VERIFY_OUTPUT "uses the external library vc4asm to verify the output" ON)
# Option to enable/disable cross compilation
option(CROSS_COMPILE "Cross compile for Raspbian" OFF)
# Option whether to include the SPIR-V front-end
option(SPIRV_FRONTEND "Enables a second front-end for the SPIR-V intermediate language" OFF)
# Option whether to include the LLVM library front-end. This requires the LLVM development-headers to be available for the (SPIRV-)LLVM used
option(LLVMLIB_FRONTEND "Enables the front-end using the LLVM library to read LLVM modules" ON)
# Option whether to create deb package
option(BUILD_DEB_PACKAGE "Enables creating .deb package" ON)
# Option whether to enable code coverage analysis via gcov
option(ENABLE_COVERAGE "Enables collection of code coverage via gcov" OFF)
# Option whether to enable use of the CLang library (EXPERIMENTAL)
option(CLANG_LIBRARY "Uses the libclang library for compilation, uses the clang executable otherwise" OFF)

# Only allow coverage for debug builds
if(NOT BUILD_DEBUG)
	set(ENABLE_COVERAGE OFF)
endif()

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
	if(NOT CROSS_COMPILER_PATH)
		set(CROSS_COMPILER_PATH "/usr/bin")
	endif()
	if(NOT CROSS_COMPILER_PREFIX)
		set(CROSS_COMPILER_PREFIX "arm-linux-gnueabihf-")
	endif()
	#Set Cross compiler
	include(cmake/RaspberryPi.cmake)
	# Raspbian ships CLang 3.9 in its repositories
	set(CLANG_FOUND /usr/bin/clang-3.9)
    set(OPT_FOUND /usr/bin/opt-3.9)
	set(LLVM_LINK_FOUND /usr/bin/llvm-link-3.9)
	set(ADVANCED_OPENCL_FEATURES ON)
        if (SYSROOT_CROSS)
          # set(CMAKE_SYSROOT ${SYSROOT_CROSS})
          set(CMAKE_FIND_ROOT_PATH ${SYSROOT_CROSS})
          set(SYSROOT_LIBRARY_FLAGS "-Wl,-rpath-link,${SYSROOT_CROSS}/lib/arm-linux-gnueabihf:${SYSROOT_CROSS}/usr/lib/arm-linux-gnueabihf")
        endif()

        set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)

	message(STATUS "Cross compiling for Raspbian with compiler: ${CMAKE_CXX_COMPILER}")
endif()

project (VC4C VERSION 0.4)

# Set C++ standard to C++14 without any extensions (e.g. GNU)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS_DEBUG "-g3 -rdynamic")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(BUILD_DEBUG)
	SET(CMAKE_BUILD_TYPE Debug)
else()
	SET(CMAKE_BUILD_TYPE Release)
endif()

if(MULTI_THREADED)
	message(STATUS "Enabling multi-threaded optimizations")
	find_package(Threads)
endif()

####
# Dependencies
####

if(BUILD_OFFLINE)
	# A result of != 0 is an error, so disable updating
	set_property(DIRECTORY ${VC4C_SOURCE_DIR} PROPERTY EP_UPDATE_DISCONNECTED 1)
	message(WARNING "Building in off-line mode, some dependencies might not be up-to-date!")
else()
	set_property(DIRECTORY ${VC4C_SOURCE_DIR} PROPERTY EP_UPDATE_DISCONNECTED 0)
endif()

ExternalProject_Add( cpplog-project
	PREFIX 				${CMAKE_BINARY_DIR}/cpplog
	BINARY_DIR 			${CMAKE_BINARY_DIR}/cpplog
	SOURCE_DIR 			${CMAKE_SOURCE_DIR}/lib/cpplog
	GIT_REPOSITORY 		https://github.com/doe300/cpplog.git
	UPDATE_COMMAND 		git pull -f https://github.com/doe300/cpplog.git
	CMAKE_ARGS 			-DCPPLOG_NAMESPACE=logging -DCPPLOG_CUSTOM_LOGGER=true -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
	STEP_TARGETS 		build	#If we set our dependency on this, the install step is skipped
  	EXCLUDE_FROM_ALL 	TRUE	#Skip for "make all" to skip install
  	TIMEOUT 			30		#Timeout for downloads, in seconds
  	CMAKE_ARGS
  	  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
	  -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
	  -DCMAKE_FIND_ROOT_PATH=${CMAKE_FIND_ROOT_PATH}
)

# Prefer Khronos OpenCL to LLVM IR (to SPIR-V) compiler
if(NOT SPIRV_COMPILER_ROOT)
	# Try to detect the location of the SPIRV-LLVM binaries
	find_program(LLVM_SPIRV_FOUND NAMES llvm-spirv HINTS "/opt/SPIRV-LLVM/build/bin/")
	if(LLVM_SPIRV_FOUND)
		get_filename_component(SPIRV_COMPILER_ROOT "${LLVM_SPIRV_FOUND}" DIRECTORY)
	endif()
endif()
if(SPIRV_COMPILER_ROOT)
	message(STATUS "Khronos OpenCL toolkit: ${SPIRV_COMPILER_ROOT}")
	find_file(SPIRV_CLANG_FOUND clang PATHS ${SPIRV_COMPILER_ROOT} NO_DEFAULT_PATH)
	find_file(SPIRV_LLVM_SPIR_FOUND llvm-spirv PATHS ${SPIRV_COMPILER_ROOT} NO_DEFAULT_PATH)
	if(SPIRV_CLANG_FOUND)
		message(STATUS "Khronos OpenCL compiler: ${SPIRV_CLANG_FOUND}")
	endif()
elseif(SPIRV_FRONTEND)
	message(WARNING "SPIR-V frontend configured, but no SPIR-V compiler found!")
endif()

# fall back to "standard" CLang
if(LLVMLIB_FRONTEND AND NOT CROSS_COMPILE AND NOT SPIRV_CLANG_FOUND)
	if(NOT CLANG_FOUND)
		find_program(CLANG_FOUND clang NAMES clang clang-3.9 clang-4.0 clang-5.0 clang-6.0)
	endif()
	if (NOT OPT_FOUND)
		find_program(OPT_FOUND opt NAMES opt opt-3.9 opt-4.0 opt-5.0 opt-6.0)
	endif()
	if(NOT LLVM_LINK_FOUND)
		find_program(LLVM_LINK_FOUND llvm-link NAMES llvm-link llvm-link-3.9 llvm-link-4.0 llvm-link-5.0 llvm-link-6.0)
	endif()
	if(CLANG_FOUND)
		message(STATUS "CLang compiler found: " ${CLANG_FOUND})
		# check whether CLang is installed in the version 3.9 or greater
		# https://stackoverflow.com/questions/16150888/cmake-branch-on-clang-version
		EXECUTE_PROCESS( COMMAND ${CLANG_FOUND} --version OUTPUT_VARIABLE clang_full_version_string )
		string (REGEX REPLACE ".*clang version ([0-9]+\\.[0-9]+).*" "\\1" CLANG_VERSION_STRING ${clang_full_version_string})
		if (CLANG_VERSION_STRING VERSION_LESS 3.9)
			message(SEND_ERROR "CLang version ${CLANG_VERSION_STRING} is not supported, update your CLang to version 3.9+!")
		else()
			message(STATUS "Using enhanced OpenCL features of Clang 3.9+!")
			set(ADVANCED_OPENCL_FEATURES ON)
		endif()
	else()
		message(WARNING "No CLang compiler found!")
	endif()
	if(OPT_FOUND)
		message(STATUS "Clang opt found: " ${OPT_FOUND})
	endif()
	if(LLVM_LINK_FOUND)
		message(STATUS "LLVM-link found: " ${LLVM_LINK_FOUND})
	endif()
endif()

if(CLANG_LIBRARY)
	if(NOT LIBCLANG_LIBRARIES)
		find_library(LIBCLANG_LIBRARY_FRONTEND NAMES clangFrontend libclangFrontend)
		find_library(LIBCLANG_LIBRARY_CODEGEN NAMES clangCodeGen libclangCodeGen)

		if(LIBCLANG_LIBRARY_FRONTEND AND LIBCLANG_LIBRARY_CODEGEN)
			set(LIBCLANG_LIBRARIES ${LIBCLANG_LIBRARY_FRONTEND} ${LIBCLANG_LIBRARY_CODEGEN})
		endif()
	endif()
	if(NOT LIBCLANG_INCLUDE_PATH)
		find_path(LIBCLANG_INCLUDE_PATH clang-c/Index.h)
	endif()
	if(LIBCLANG_LIBRARIES AND LIBCLANG_INCLUDE_PATH)
		message(STATUS "Using LibClang libraries: ${LIBCLANG_LIBRARIES}")
		message(STATUS "Using LibClang headers: ${LIBCLANG_INCLUDE_PATH}")
		set(VC4C_ENABLE_LIBCLANG ON)
	else()
		message(WARNING "LibClang enabled, but no libClang libraries were found")
		set(VC4C_ENABLE_LIBCLANG OFF)
	endif()
endif()

# If the complete tool collection is provided, compile the SPIR-V front-end
if(SPIRV_LLVM_SPIR_FOUND AND SPIRV_FRONTEND)
	message(STATUS "Compiling SPIR-V front-end...")
	#Add SPIR-V headers project
	ExternalProject_Add(SPIRV-Headers
		PREFIX 				${CMAKE_BINARY_DIR}/spirv-headers
		SOURCE_DIR 			${CMAKE_SOURCE_DIR}/lib/spirv-headers
		GIT_REPOSITORY 		https://github.com/KhronosGroup/SPIRV-Headers.git
		UPDATE_COMMAND 		git pull -f https://github.com/KhronosGroup/SPIRV-Headers.git
		STEP_TARGETS 		build
  		EXCLUDE_FROM_ALL	TRUE
  		TIMEOUT 			30		#Timeout for downloads, in seconds
  		CMAKE_ARGS
  		  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
  		  -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
  		  -DCMAKE_FIND_ROOT_PATH=${CMAKE_FIND_ROOT_PATH}
	)
	# skip executables and tests for the SPIR-V parser
	ExternalProject_Add(spirv-tools-project
		DEPENDS 			SPIRV-Headers-build
		PREFIX 				${CMAKE_BINARY_DIR}/spirv-tools
		SOURCE_DIR 			${CMAKE_SOURCE_DIR}/lib/spirv-tools
		GIT_REPOSITORY 		https://github.com/KhronosGroup/SPIRV-Tools.git
		UPDATE_COMMAND 		git pull -f https://github.com/KhronosGroup/SPIRV-Tools.git
		CMAKE_ARGS 			-DSPIRV_SKIP_EXECUTABLES:BOOL=ON -DSPIRV_SKIP_TESTS:BOOL=ON -DSPIRV-Headers_SOURCE_DIR:STRING=${CMAKE_CURRENT_SOURCE_DIR}/lib/spirv-headers
		STEP_TARGETS 		build
  		EXCLUDE_FROM_ALL	TRUE
  		TIMEOUT 			30		#Timeout for downloads, in seconds
  		CMAKE_ARGS
  		  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
  		  -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
  		  -DCMAKE_FIND_ROOT_PATH=${CMAKE_FIND_ROOT_PATH}
	)
	set(VC4C_ENABLE_SPIRV_FRONTEND ON)
endif()

# If enabled, check whether the LLVM library front-end can be built
if(LLVMLIB_FRONTEND)
	# We cannot use SPIRV-LLVM/clang with libLLVM from default clang. So if SPIRV-LLVM is configured, force to use its libLLVM
	if(SPIRV_CLANG_FOUND)
		set(LLVM_CONFIG_PATH ${SPIRV_COMPILER_ROOT}/llvm-config)
	endif()
	# LLVM_CONFIG_PATH can be used to configure the path for the llvm-config program.
	# If it is not set, the default clang will be queried via CMake find_package
	if(LLVM_CONFIG_PATH)
		execute_process(COMMAND ${LLVM_CONFIG_PATH} --libdir OUTPUT_VARIABLE LLVM_LIBS_PATH OUTPUT_STRIP_TRAILING_WHITESPACE)
		execute_process(COMMAND ${LLVM_CONFIG_PATH} --includedir OUTPUT_VARIABLE LLVM_INCLUDE_PATH OUTPUT_STRIP_TRAILING_WHITESPACE)
		execute_process(COMMAND ${LLVM_CONFIG_PATH} --cppflags OUTPUT_VARIABLE LLVM_LIB_FLAGS OUTPUT_STRIP_TRAILING_WHITESPACE)
		execute_process(COMMAND ${LLVM_CONFIG_PATH} --version OUTPUT_VARIABLE LLVM_LIB_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE)
		execute_process(COMMAND ${LLVM_CONFIG_PATH} --libs core irreader bitreader OUTPUT_VARIABLE LLVM_LIB_NAMES OUTPUT_STRIP_TRAILING_WHITESPACE)
		# Additional system libraries, e.g. required for SPIRV-LLVM on raspberry, not for "default" LLVM on my development machine
		execute_process(COMMAND ${LLVM_CONFIG_PATH} --system-libs OUTPUT_VARIABLE LLVM_SYSTEM_LIB_NAMES OUTPUT_STRIP_TRAILING_WHITESPACE)
		# The --shared-mode option does not exist for e.g. SPIRV-LLVM, but we can ignore it and assume static linking
		execute_process(COMMAND ${LLVM_CONFIG_PATH} --shared-mode OUTPUT_VARIABLE LLVM_SHARED_MODE OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)
		if(LLVM_SHARED_MODE STREQUAL "shared")
			# The system libraries are only required, if LLVM is linked statically (see https://reviews.llvm.org/rL291285)
			# So for shared linking, clear system libraries
			set(LLVM_SYSTEM_LIB_NAMES "")
		endif()

		#Depending on the LLVM library version used, we need to use some other functions/headers in the LLVM library front-end
		if(LLVM_LIB_VERSION)
			string(REPLACE "." ";" LLVM_VERSION_ELEMENTS ${LLVM_LIB_VERSION})
			list(GET LLVM_VERSION_ELEMENTS 0 LLVM_LIB_VERSION_MAJOR)
			list(GET LLVM_VERSION_ELEMENTS 1 LLVM_LIB_VERSION_MINOR)
			set(LLVM_LIBRARY_VERSION ${LLVM_LIB_VERSION_MAJOR}${LLVM_LIB_VERSION_MINOR})
		else()
			message(WARNING "Failed to determine LLVM library version")
		endif()
	else()
		find_package(LLVM)
		if(LLVM_FOUND)

			#Depending on the LLVM library version used, we need to use some other functions/headers in the LLVM library front-end
			set(LLVM_LIBRARY_VERSION ${LLVM_VERSION_MAJOR}${LLVM_VERSION_MINOR})

			set(LLVM_LIBS_PATH "${LLVM_LIBRARY_DIRS}")
			set(LLVM_INCLUDE_PATH "${LLVM_INCLUDE_DIRS}")
			set(LLVM_LIB_FLAGS "${LLVM_DEFINITIONS}")
			set(LLVM_LIB_VERSION "${LLVM_PACKAGE_VERSION}")
			find_library(LLVM_SHARED_LIBRARY NAMES LLVM libLLVM PATHS "${LLVM_LIBRARY_DIRS}")
			if(LLVM_SHARED_LIBRARY)
				set(LLVM_LIB_NAMES ${LLVM_SHARED_LIBRARY})
			else()
				llvm_map_components_to_libnames(LLVM_LIB_NAMES core irreader bitreader)
			endif()
			set(LLVM_SYSTEM_LIB_NAMES "")
		endif()
	endif()

	if(LLVM_LIBS_PATH AND LLVM_INCLUDE_PATH AND LLVM_LIB_FLAGS AND LLVM_LIB_NAMES)
		message(STATUS "Compiling LLVM library front-end with LLVM in version ${LLVM_LIB_VERSION} located in '${LLVM_LIBS_PATH}'")
		set(VC4C_ENABLE_LLVM_LIB_FRONTEND ON)
	else()
		message(WARNING "LLVM library front-end enabled, but LLVM library was not found!")
	endif()
endif()

if(NOT ((SPIRV_LLVM_SPIR_FOUND AND SPIRV_FRONTEND) OR (LLVMLIB_FRONTEND AND LLVM_LIBS_PATH)))
	message(WARNING " Neither SPIR-V nor LLVM library front-end are configured!")
endif()

if(VERIFY_OUTPUT)
	ExternalProject_Add(vc4asm-project
		PREFIX				${CMAKE_BINARY_DIR}/vc4asm
		SOURCE_DIR 			${CMAKE_SOURCE_DIR}/lib/vc4asm
		GIT_REPOSITORY 		https://github.com/maazl/vc4asm.git
		UPDATE_COMMAND 		git pull -f https://github.com/maazl/vc4asm.git
		STEP_TARGETS 		build
  		EXCLUDE_FROM_ALL	TRUE
  		TIMEOUT 			30		#Timeout for downloads, in seconds
  		CMAKE_ARGS
  		  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
  		  -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
  		  -DCMAKE_FIND_ROOT_PATH=${CMAKE_FIND_ROOT_PATH}
  		  -DCMAKE_CXX_FLAGS="-fPIC"
  		  -DCMAKE_C_FLAGS="-fPIC"
	)
endif()

if(SPIRV_CLANG_FOUND)
	set(PRECOMPILE_COMMAND "${SPIRV_CLANG_FOUND}")
elseif(CLANG_FOUND)
	set(PRECOMPILE_COMMAND "${CLANG_FOUND}")
else()
	message(FATAL_ERROR "No supported OpenCL compiler found!")
endif()

# Variant backport for C++ < 17
ExternalProject_Add( variant-project
	PREFIX 				${CMAKE_BINARY_DIR}/variant
	BINARY_DIR 			${CMAKE_BINARY_DIR}/variant
	SOURCE_DIR 			${CMAKE_SOURCE_DIR}/lib/variant
	GIT_REPOSITORY 		https://github.com/mpark/variant.git
	UPDATE_COMMAND 		git pull -f https://github.com/mpark/variant.git
	CMAKE_ARGS 			-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
	STEP_TARGETS 		build	#If we set our dependency on this, the install step is skipped
  	EXCLUDE_FROM_ALL 	TRUE	#Skip for "make all" to skip install
  	TIMEOUT 			30		#Timeout for downloads, in seconds
  	CMAKE_ARGS
  	  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
	  -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
	  -DCMAKE_FIND_ROOT_PATH=${CMAKE_FIND_ROOT_PATH}
)

####
# Main files
####
#build all from ./src into ./build
add_subdirectory(src)
add_subdirectory(tools)

if (BUILD_TESTING)
	enable_testing()
	ExternalProject_Add(cpptest-lite-project
		PREFIX 				${CMAKE_BINARY_DIR}/cpptest-lite
		SOURCE_DIR 			${CMAKE_SOURCE_DIR}/lib/cpptest-lite
		GIT_REPOSITORY		https://github.com/doe300/cpptest-lite.git
		UPDATE_COMMAND 		git pull -f https://github.com/doe300/cpptest-lite.git
		STEP_TARGETS 		build
  		EXCLUDE_FROM_ALL	TRUE
  		TIMEOUT 			30		#Timeout for downloads, in seconds
  		CMAKE_ARGS
  		  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
  		  -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
  		  -DCMAKE_FIND_ROOT_PATH=${CMAKE_FIND_ROOT_PATH}
	)
    add_subdirectory(test)
endif (BUILD_TESTING)

if (BUILD_DEB_PACKAGE)
	message(STATUS "build deb package...")

	# Generate postint-script for debian package, building the VC4CLStdLib PCH
	configure_file(cmake/deb-postint.in "${CMAKE_BINARY_DIR}/postinst" @ONLY NEWLINE_STYLE UNIX)
	configure_file(cmake/deb-prerem.in "${CMAKE_BINARY_DIR}/prerem" @ONLY NEWLINE_STYLE UNIX)

	# We always need the vc4cl standard-library for installing the vc4c package
	set(PACKAGE_DEPENDENCIES "vc4cl-stdlib")
	if(NOT SPIRV_CLANG_FOUND AND CLANG_FOUND)
		# If we build with "default" clang, require its package
		set(PACKAGE_DEPENDENCIES "${PACKAGE_DEPENDENCIES}, clang-3.9")
		if(LLVMLIB_FRONTEND)
			# If we also build with libLLVM front-end, require the LLVM library and its development files too (llvm-3.9-dev contains the libLLVM.so)
			set(PACKAGE_DEPENDENCIES "${PACKAGE_DEPENDENCIES}, llvm-3.9, llvm-3.9-dev")
		endif()
		if(LIBCLANG_LIBRARY_PATH)
			# If we also build with libclang, require the library
			# TODO check versions/development headers required?
			set(PACKAGE_DEPENDENCIES "${PACKAGE_DEPENDENCIES}, libclang-3.9, libclang-3.9-dev")
		endif()
	endif()

	set(CPACK_GENERATOR "DEB")
	set(CPACK_PACKAGING_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")
	set(CPACK_PACKAGE_NAME "vc4c")
	string(TIMESTAMP BUILD_TIMESTAMP "%Y-%m-%d")
	set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}-${BUILD_TIMESTAMP}")
	set(CPACK_DEBIAN_PACKAGE_DEPENDS ${PACKAGE_DEPENDENCIES})
	if (CROSS_COMPILE)
		set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "armhf")
	else()
		set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
	endif()
	set(CPACK_PACKAGE_VENDOR "doe300")
	set(CPACK_PACKAGE_CONTACT "doe300@web.de")
	set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "OpenCL C compiler for VC4CL (rasberrypi only)")
	set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://github.com/doe300/VC4C")
	set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA ${CMAKE_BINARY_DIR}/postinst ${CMAKE_BINARY_DIR}/prerem)
	set(CPACK_PACKAGE_FILE_NAME "vc4c-0.4-Linux")
	set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
	include(CPack)
endif (BUILD_DEB_PACKAGE)
