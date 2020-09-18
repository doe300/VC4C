##
# Main targets
##
add_library( VC4CC SHARED "")
add_executable( VC4C main.cpp concepts.h)

set(VC4C_LIBRARY_NAME VC4CC)
set(VC4C_PROGRAM_NAME VC4C)

# Set lower case application since most applications are lower case on Linux
set_target_properties(${VC4C_PROGRAM_NAME} PROPERTIES OUTPUT_NAME "vc4c")

target_compile_definitions(${VC4C_LIBRARY_NAME} PUBLIC VC4C_VERSION="${PROJECT_VERSION}")
target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE VC4C_VERSION="${PROJECT_VERSION}")
if(VC4CL_STDLIB_DIR)
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE VC4CL_STDLIB_FOLDER="${VC4CL_STDLIB_DIR}")
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE VC4CL_STDLIB_FOLDER="${VC4CL_STDLIB_DIR}")
endif()

# append usage of C++ to compiler flags, also optimize for speed and enable all warnings
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
if(LLVM_LINK_FOUND)
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE LLVM_LINK_PATH="${LLVM_LINK_FOUND}")
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE LLVM_LINK_PATH="${LLVM_LINK_FOUND}")
endif()
if(SPIRV_CLANG_FOUND)
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE SPIRV_CLANG_PATH="${SPIRV_CLANG_FOUND}")
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE SPIRV_CLANG_PATH="${SPIRV_CLANG_FOUND}")
endif(SPIRV_CLANG_FOUND)
if(SPIRV_LINK_MODULES)
    target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE SPIRV_LINK_MODULES=1)
    target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE SPIRV_LINK_MODULES=1)
endif(SPIRV_LINK_MODULES)
if(ADVANCED_OPENCL_FEATURES)
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE USE_CLANG_OPENCL=1)
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE USE_CLANG_OPENCL=1)
endif(ADVANCED_OPENCL_FEATURES)
if(ENABLE_COVERAGE)
	target_compile_options(${VC4C_LIBRARY_NAME} PRIVATE -fprofile-arcs -ftest-coverage --coverage)
	target_compile_options(${VC4C_PROGRAM_NAME} PRIVATE -fprofile-arcs -ftest-coverage --coverage)
	target_link_libraries(${VC4C_LIBRARY_NAME} gcov "-fprofile-arcs -ftest-coverage")
	target_link_libraries(${VC4C_PROGRAM_NAME} gcov "-fprofile-arcs -ftest-coverage")
endif(ENABLE_COVERAGE)
if(OPT_FOUND)
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE OPT_PATH="${OPT_FOUND}")
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE OPT_PATH="${OPT_FOUND}")
endif()
if(LLVM_DIS_FOUND)
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE LLVM_DIS_PATH="${LLVM_DIS_FOUND}")
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE LLVM_DIS_PATH="${LLVM_DIS_FOUND}")
endif()

# Add all sources
include(sources.list)
include(analysis/sources.list)
include(asm/sources.list)
include(intermediate/sources.list)
include(intrinsics/sources.list)
include(llvm/sources.list)
include(normalization/sources.list)
include(optimization/sources.list)
include(periphery/sources.list)
include(precompilation/sources.list)
include(spirv/sources.list)
include(tools/sources.list)

# Add all dependencies
target_link_libraries(${VC4C_PROGRAM_NAME} ${VC4C_LIBRARY_NAME} ${SYSROOT_LIBRARY_FLAGS})

# cpplog
add_dependencies(${VC4C_LIBRARY_NAME} cpplog-dependencies)
target_link_libraries(${VC4C_LIBRARY_NAME} ${cpplog_LIBS})
target_include_directories(${VC4C_LIBRARY_NAME} PUBLIC ${cpplog_HEADERS})
target_compile_definitions(${VC4C_LIBRARY_NAME} PUBLIC ${cpplog_DEFINES})

# SPIR-V Headers
add_dependencies(${VC4C_LIBRARY_NAME} SPIRV-Dependencies)
target_include_directories(${VC4C_LIBRARY_NAME} SYSTEM PRIVATE ${SPIRV_Headers_HEADERS})

# threading library
if(MULTI_THREADED)
	target_link_libraries(${VC4C_LIBRARY_NAME} ${CMAKE_THREAD_LIBS_INIT})
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE MULTI_THREADED=1)
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE MULTI_THREADED=1)
endif(MULTI_THREADED)

# SPIR-V Tools
if(VC4C_ENABLE_SPIRV_TOOLS_FRONTEND)
	add_dependencies(${VC4C_LIBRARY_NAME} SPIRV-Dependencies)
	target_link_libraries(${VC4C_LIBRARY_NAME} ${SPIRV_Tools_LIBS})
	target_include_directories(${VC4C_LIBRARY_NAME} SYSTEM PRIVATE ${SPIRV_Tools_HEADERS})

	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE SPIRV_LLVM_SPIRV_PATH="${SPIRV_LLVM_SPIR_FOUND}" SPIRV_TOOLS_FRONTEND=1)
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE SPIRV_LLVM_SPIRV_PATH="${SPIRV_LLVM_SPIR_FOUND}" SPIRV_TOOLS_FRONTEND=1)
endif(VC4C_ENABLE_SPIRV_TOOLS_FRONTEND)

# LLVM library
if(VC4C_ENABLE_LLVM_LIB_FRONTEND)
	#This works with libLLVM.so as well as the single static component libraries
	string(STRIP "-L ${LLVM_LIBS_PATH} ${LLVM_LIB_NAMES} ${LLVM_SYSTEM_LIB_NAMES}" llvm)
	target_link_libraries(${VC4C_LIBRARY_NAME} "${llvm}")

	# adds require macros as well as include-paths
	# SYSTEM here suppresses (at least on some compilers/systems) all warnings from the LLVM header file
	target_include_directories(${VC4C_LIBRARY_NAME} SYSTEM PRIVATE "${LLVM_INCLUDE_PATH}")
	# TODO LLVM_LIB_FLAGS results in a lot of warnings about redefining _GNU_SOURCE (at least for Clang)
	target_compile_options(${VC4C_LIBRARY_NAME} PRIVATE ${LLVM_LIB_FLAGS})
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE USE_LLVM_LIBRARY=1 LLVM_LIBRARY_VERSION=${LLVM_LIBRARY_VERSION})
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE USE_LLVM_LIBRARY=1 LLVM_LIBRARY_VERSION=${LLVM_LIBRARY_VERSION})
endif(VC4C_ENABLE_LLVM_LIB_FRONTEND)

# LibClang
if(VC4C_ENABLE_LIBCLANG)
	target_link_libraries(${VC4C_LIBRARY_NAME} "${LIBCLANG_LIBRARIES}")
	# SYSTEM here suppresses (at least on some compilers/systems) all warnings from the LLVM header file
	target_include_directories(${VC4C_LIBRARY_NAME} SYSTEM PRIVATE ${LIBCLANG_INCLUDE_PATH})
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE USE_LIBCLANG=1)
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE USE_LIBCLANG=1)
endif(VC4C_ENABLE_LIBCLANG)

if(VERIFY_OUTPUT)
	add_dependencies(${VC4C_LIBRARY_NAME} vc4asm-dependencies)
	target_link_libraries(${VC4C_LIBRARY_NAME} ${vc4asm_LIBS})
	target_compile_definitions(${VC4C_LIBRARY_NAME} PRIVATE VERIFIER_HEADER="${vc4asm_HEADER}")
	target_compile_definitions(${VC4C_PROGRAM_NAME} PRIVATE VERIFIER_HEADER="${vc4asm_HEADER}")
endif()

# Download mpark/variant library as dependency
add_dependencies(${VC4C_LIBRARY_NAME} variant-dependencies)
target_include_directories(${VC4C_LIBRARY_NAME} SYSTEM PRIVATE ${variant_HEADERS})
target_include_directories(${VC4C_PROGRAM_NAME} SYSTEM PRIVATE ${variant_HEADERS})

if(VC4CL_STDLIB_PRECOMPILE AND VC4CL_STDLIB_DIR)
	# Pre-compile VC4CL standard library files if development headers available and output files do not yet exist
	add_custom_command(TARGET ${VC4C_PROGRAM_NAME} POST_BUILD
	    COMMAND if \[ ! -e ${VC4CL_STDLIB_DIR}/VC4CLStdLib.bc -o ! -e ${VC4CL_STDLIB_DIR}/VC4CLStdLib.h.pch \]; then $<TARGET_FILE:${VC4C_PROGRAM_NAME}> --quiet --precompile-stdlib -o ${VC4CL_STDLIB_DIR}/ ${VC4CL_STDLIB_DIR}/VC4CLStdLib.h && echo \"VC4CL standard library precompiled into ${VC4CL_STDLIB_DIR}\" \; fi
	)
endif(VC4CL_STDLIB_PRECOMPILE AND VC4CL_STDLIB_DIR)

# "For shared libraries VERSION and SOVERSION can be used to specify the build version and API version respectively."
set_target_properties(
	${VC4C_LIBRARY_NAME} PROPERTIES
	# This corresponds to the project/library-version
	VERSION "${PROJECT_VERSION}"
	# This corresponds to the API-version (e.g. OpenCL 1.2)
	SOVERSION "1.2"
)

include(../cmake/clang-format.cmake)

##
# Git commit in main executable
##
add_custom_target(GetGitCommit
	# If the git command fails, write dummy content (e.g. when not on a git repository)
	COMMAND git log -1 "--format=#define GIT_COMMIT \"%h\"" > ./git_commit.h || echo "#define GIT_COMMIT \"unknown\"" > ./git_commit.h
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	VERBATIM
)
target_include_directories(${VC4C_PROGRAM_NAME} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
add_dependencies(${VC4C_PROGRAM_NAME} GetGitCommit)

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
# Precompiles the headers
# TODO replace with usage of generator expression once CMake 3.14 is in Raspbian buster repository
get_property(VC4C_PROGRAM_LOCATION TARGET ${VC4C_PROGRAM_NAME} PROPERTY LOCATION)
install(CODE "execute_process(COMMAND ${VC4C_PROGRAM_LOCATION} --quiet --precompile-stdlib -o $DESTDIR/${CMAKE_INSTALL_PREFIX}/include/vc4cl-stdlib/ ${VC4CL_STDLIB_DIR}/VC4CLStdLib.h)")
# Exports the target (to be used by CMake to find the SOURCE library)
export(TARGETS ${VC4C_LIBRARY_NAME} FILE vc4cc-exports.cmake)
# Adds custom uninstall command
add_custom_target(uninstall "${CMAKE_COMMAND}" -P "cmake_uninstall.cmake")
