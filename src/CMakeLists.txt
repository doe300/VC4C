##
# Main targets
##
add_library( VC4CC SHARED "")
add_executable( VC4C main.cpp concepts.h)

set(VC4C_LIBRARY_NAME VC4CC)
set(VC4C_PROGRAM_NAME VC4C)

target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE VC4C_VERSION="${PROJECT_VERSION}" VC4CL_STDLIB_HEADER="${VC4CL_STDLIB_HEADER}")
target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE VC4C_VERSION="${PROJECT_VERSION}" VC4CL_STDLIB_HEADER="${VC4CL_STDLIB_HEADER}")

# append usage of C++ to compiler flags, also optimize for speed and enable all warnings
set(VC4C_ENABLED_WARNINGS -Wall -Wextra -Wold-style-cast -Wno-unused-parameter -Wno-missing-field-initializers -Wno-write-strings)
# Enable additional warnings, if available
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
	SET(VC4C_ENABLED_WARNINGS ${VC4C_ENABLED_WARNINGS} -Weverything -Wno-c++98-compat -Wno-c++98-compat-pedantic -Wno-shadow -Wno-padded -Wno-shadow-field-in-constructor -Wno-global-constructors -Wno-exit-time-destructors -Wno-missing-prototypes -Wno-gnu-anonymous-struct -Wno-nested-anon-types -Wno-documentation -Wno-unused-command-line-argument -Wno-unused-member-function -Wno-gnu-zero-variadic-macro-arguments -Wno-covered-switch-default -Wno-switch-enum -Wswitch)
elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
	SET(VC4C_ENABLED_WARNINGS ${VC4C_ENABLED_WARNINGS} -Wdouble-promotion -fdelete-null-pointer-checks -Wnull-dereference -Wuninitialized -Wsuggest-attribute=pure -Wsuggest-attribute=const -Wsuggest-attribute=format -Wsuggest-override -Wconversion -Wzero-as-null-pointer-constant)
endif()
target_compile_options(${VC4C_LIBRARY_NAME} PRIVATE ${VC4C_ENABLED_WARNINGS})
target_compile_options(${VC4C_PROGRAM_NAME} PRIVATE ${VC4C_ENABLED_WARNINGS})

if(BUILD_DEBUG)
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE DEBUG_MODE=1)
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE DEBUG_MODE=1)
endif(BUILD_DEBUG)
if(CLANG_FOUND)
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE CLANG_PATH="${CLANG_FOUND}")
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE CLANG_PATH="${CLANG_FOUND}")
endif(CLANG_FOUND)
if(SPIRV_CLANG_FOUND)
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE SPIRV_CLANG_PATH="${SPIRV_CLANG_FOUND}")
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE SPIRV_CLANG_PATH="${SPIRV_CLANG_FOUND}")
endif(SPIRV_CLANG_FOUND)
if(ADVANCED_OPENCL_FEATURES)
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE USE_CLANG_OPENCL=1)
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE USE_CLANG_OPENCL=1)
endif(ADVANCED_OPENCL_FEATURES)

# Add all sources
include(sources.list)
include(analysis/sources.list)
include(asm/sources.list)
include(intermediate/sources.list)
include(intrinsics/sources.list)
include(llvm/sources.list)
include(optimization/sources.list)
include(periphery/sources.list)
include(spirv/sources.list)
include(tools/sources.list)

# Add all dependencies
target_link_libraries(VC4C VC4CC)
#VC4CL standard-library (build only dependency)
add_dependencies(${VC4C_LIBRARY_NAME} vc4cl-stdlib)

# cpplog
add_dependencies(${VC4C_LIBRARY_NAME} cpplog-project-build)
add_library(cpplog-static STATIC IMPORTED)
ExternalProject_Get_Property(cpplog-project BINARY_DIR)
set_target_properties(cpplog-static PROPERTIES IMPORTED_LOCATION "${BINARY_DIR}/libcpplog-static.a")
target_link_libraries(${VC4C_LIBRARY_NAME} cpplog-static)
target_include_directories(${VC4C_LIBRARY_NAME} PRIVATE "${PROJECT_SOURCE_DIR}/lib/cpplog/include")
target_include_directories(${VC4C_PROGRAM_NAME} PRIVATE "${PROJECT_SOURCE_DIR}/lib/cpplog/include")
target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE CPPLOG_NAMESPACE=logging CPPLOG_CUSTOM_LOGGER=true)
target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE CPPLOG_NAMESPACE=logging CPPLOG_CUSTOM_LOGGER=true)

# threading library
if(MULTI_THREADED)
	target_link_libraries(${VC4C_LIBRARY_NAME} ${CMAKE_THREAD_LIBS_INIT})
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE MULTI_THREADED=1)
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE MULTI_THREADED=1)
	# For dlopen, dlsym
	target_link_libraries(${VC4C_LIBRARY_NAME} dl)
endif(MULTI_THREADED)

# SPIR-V Tools
if(VC4C_ENABLE_SPIRV_FRONTEND)
	add_dependencies(${VC4C_LIBRARY_NAME} spirv-tools-project-build)
	ExternalProject_Get_Property(spirv-tools-project BINARY_DIR)

	set(spirv-tools "-Wl,--whole-archive ${BINARY_DIR}/source/libSPIRV-Tools.a -Wl,--no-whole-archive")
	target_link_libraries(${VC4C_LIBRARY_NAME} "${spirv-tools}")

	set(spirv-tools-opt "-Wl,--whole-archive ${BINARY_DIR}/source/opt/libSPIRV-Tools-opt.a -Wl,--no-whole-archive")
	target_link_libraries(${VC4C_LIBRARY_NAME} "${spirv-tools-opt}")

	add_library(spirv-tools-link STATIC IMPORTED)
	set_target_properties(spirv-tools-link PROPERTIES IMPORTED_LOCATION "${BINARY_DIR}/source/link/libSPIRV-Tools-link.a")
	target_link_libraries(${VC4C_LIBRARY_NAME} spirv-tools-link)
	
	target_include_directories(${VC4C_LIBRARY_NAME} PRIVATE "${PROJECT_SOURCE_DIR}/lib/spirv-headers/include")
	target_include_directories(${VC4C_LIBRARY_NAME} PRIVATE "${PROJECT_SOURCE_DIR}/lib/spirv-tools/include")
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE SPIRV_LLVM_SPIRV_PATH="${SPIRV_LLVM_SPIR_FOUND}" SPIRV_FRONTEND=1)
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE SPIRV_LLVM_SPIRV_PATH="${SPIRV_LLVM_SPIR_FOUND}" SPIRV_FRONTEND=1)
endif(VC4C_ENABLE_SPIRV_FRONTEND)

# LLVM library
if(VC4C_ENABLE_LLVM_LIB_FRONTEND)
	#This works with libLLVM.so as well as the single static component libraries
	string(STRIP "-L ${LLVM_LIBS_PATH} ${LLVM_LIB_NAMES} ${LLVM_SYSTEM_LIB_NAMES}" llvm)
	target_link_libraries(${VC4C_LIBRARY_NAME} "${llvm}")
	
	# Adds require macros as well as include-paths
	target_include_directories(${VC4C_LIBRARY_NAME} PRIVATE "${LLVM_INCLUDE_PATH}")
	target_compile_options(${VC4C_LIBRARY_NAME} PRIVATE ${LLVM_LIB_FLAGS})
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE USE_LLVM_LIBRARY=1 LLVM_LIBRARY_VERSION=${LLVM_LIBRARY_VERSION})
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE USE_LLVM_LIBRARY=1 LLVM_LIBRARY_VERSION=${LLVM_LIBRARY_VERSION})
endif(VC4C_ENABLE_LLVM_LIB_FRONTEND)

if(VERIFY_OUTPUT)
	add_dependencies(${VC4C_LIBRARY_NAME} vc4asm-project-build)
	add_library(vc4asm STATIC IMPORTED)
	ExternalProject_Get_Property(vc4asm-project BINARY_DIR)
	set_target_properties(vc4asm PROPERTIES IMPORTED_LOCATION "${BINARY_DIR}/libvc4asm.a")
	target_link_libraries(${VC4C_LIBRARY_NAME} vc4asm)
	
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE VERIFIER_HEADER="${PROJECT_SOURCE_DIR}/lib/vc4asm/src/Validator.h")
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE VERIFIER_HEADER="${PROJECT_SOURCE_DIR}/lib/vc4asm/src/Validator.h")
endif()

# "For shared libraries VERSION and SOVERSION can be used to specify the build version and API version respectively."
set_target_properties(
	${VC4C_LIBRARY_NAME} PROPERTIES
	# This corresponds to the project/library-version
	VERSION "${PROJECT_VERSION}"
	# This corresponds to the API-version (e.g. OpenCL 1.2)
	SOVERSION "1.2"
)

find_program(
  CLANG_FORMAT_PATH
  NAMES clang-format clang-format-3.8 clang-format-3.9 clang-format-4.0
  HINTS "/usr/bin" "/usr/local/bin"
)

if(CLANG_FORMAT_PATH)
  message (STATUS "found clang-format: ${CLANG_FORMAT_PATH}")
  get_target_property(VC4CC_SRCS ${VC4C_LIBRARY_NAME} SOURCES)
  get_target_property(VC4C_SRCS ${VC4C_PROGRAM_NAME} SOURCES)
  add_custom_target(
    clang-format
    COMMAND ${CLANG_FORMAT_PATH}
    -i ${VC4CC_SRCS} ${VC4C_SRCS}
  )
else()
  message (WARNING "clang-format not found: strongly recommend to use it before commit!")
endif()

##
# Installation targets
##
# Adds the public headers to the target, so they are exported
target_include_directories(${VC4C_LIBRARY_NAME} PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>  $<INSTALL_INTERFACE:include/vc4cc>)
# Creates the install target for the library and the compiler
install(TARGETS ${VC4C_LIBRARY_NAME} EXPORT VC4CC-targets LIBRARY DESTINATION lib)
install(TARGETS ${VC4C_PROGRAM_NAME} EXPORT VC4C-targets RUNTIME DESTINATION bin)
# Creates the export target (to be used by CMake to find the INSTALLED library)
install(EXPORT VC4CC-targets DESTINATION share/vc4cc)
# Creates the install target for the headers
install(DIRECTORY "${PROJECT_SOURCE_DIR}/include/" DESTINATION include/vc4cc FILES_MATCHING PATTERN "*.h")
# Exports the target (to be used by CMake to find the SOURCE library)
export(TARGETS ${VC4C_LIBRARY_NAME} FILE vc4cc-exports.cmake)
# Adds custom uninstall command
add_custom_target(uninstall "${CMAKE_COMMAND}" -P "cmake_uninstall.cmake")
