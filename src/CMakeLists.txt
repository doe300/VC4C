#Include headers in the project settings (as search-path for header-files)
include_directories ("${PROJECT_SOURCE_DIR}/include" "${PROJECT_SOURCE_DIR}/lib/cpplog/include" "${PROJECT_SOURCE_DIR}/lib/vc4asm/src")

#Include headers in the project structure
file( GLOB HDRS "${PROJECT_SOURCE_DIR}/include/*.h" "${PROJECT_SOURCE_DIR}/lib/cpplog/include/*.h" "${PROJECT_SOURCE_DIR}/lib/vc4asm/src/*.h")
#Include the source files
file( GLOB SRCS *.cpp */*.cpp )

## 
# Build library
##
add_library( VC4CC SHARED ${SRCS} ${HDRS} )
add_dependencies(VC4CC cpplog-project-build)
add_library(cpplog-static STATIC IMPORTED)
ExternalProject_Get_Property(cpplog-project BINARY_DIR)
set_target_properties(cpplog-static PROPERTIES IMPORTED_LOCATION "${BINARY_DIR}/libcpplog-static.a")
target_link_libraries(VC4CC cpplog-static)
# For dlopen, dlsym
target_link_libraries(VC4CC dl)
add_dependencies(VC4CC vc4cl-stdlib)

if(SPIRV_FRONTEND AND SPIRV_COMPILER_ROOT)
	add_dependencies(VC4CC spirv-tools-project-build)
	ExternalProject_Get_Property(spirv-tools-project BINARY_DIR)
	
	#This discards some symbols (spvtools::SpirvTools) while including into libVC4CC, which are required by the SPIRV-Tools linker and optimizer
	#add_library(spirv-tools STATIC IMPORTED)
	#set_target_properties(spirv-tools PROPERTIES IMPORTED_LOCATION "${BINARY_DIR}/source/libSPIRV-Tools.a")
	#target_link_libraries(VC4CC spirv-tools)
	
	set(spirv-tools "-Wl,--whole-archive ${BINARY_DIR}/source/libSPIRV-Tools.a -Wl,--no-whole-archive")
	target_link_libraries(VC4CC "${spirv-tools}")
	
	#This discards some symbols (spvtools::opt::*) while including into libVC4CC, which are somehow required by libVC4CL
	#add_library(spirv-tools-opt STATIC IMPORTED)
	#set_target_properties(spirv-tools-opt PROPERTIES IMPORTED_LOCATION "${BINARY_DIR}/source/opt/libSPIRV-Tools-opt.a")
	#target_link_libraries(VC4CC spirv-tools-opt)
	
	set(spirv-tools-opt "-Wl,--whole-archive ${BINARY_DIR}/source/opt/libSPIRV-Tools-opt.a -Wl,--no-whole-archive")
	target_link_libraries(VC4CC "${spirv-tools-opt}")
	
	add_library(spirv-tools-link STATIC IMPORTED)
	set_target_properties(spirv-tools-link PROPERTIES IMPORTED_LOCATION "${BINARY_DIR}/source/link/libSPIRV-Tools-link.a")
	target_link_libraries(VC4CC spirv-tools-link)
endif()

if(ENABLE_LLVM_LIB_FRONTEND)
	#This works with libLLVM.so as well as the single static component libraries
	set(llvm "-L ${LLVM_LIBS_PATH} ${LLVM_LIB_NAMES} ${LLVM_SYSTEM_LIB_NAMES}")
	target_link_libraries(VC4CC "${llvm}")
endif(ENABLE_LLVM_LIB_FRONTEND)

if(VERIFY_OUTPUT)
	add_dependencies(VC4CC vc4asm-project-build)
	add_library(vc4asm STATIC IMPORTED)
	ExternalProject_Get_Property(vc4asm-project BINARY_DIR)
	set_target_properties(vc4asm PROPERTIES IMPORTED_LOCATION "${BINARY_DIR}/libvc4asm.a")
	target_link_libraries(VC4CC vc4asm)
endif()

## 
# Build executable
##
add_executable( VC4C main.cpp ${HDRS} )
target_link_libraries(VC4C VC4CC)

# "For shared libraries VERSION and SOVERSION can be used to specify the build version and API version respectively."
set_target_properties(
	VC4CC PROPERTIES
	# This corresponds to the project/library-version
	VERSION "${PROJECT_VERSION}"
	# This corresponds to the API-version (e.g. OpenCL 1.2)
	SOVERSION "1.2"
)

##
# Installation targets
##
# Adds the public headers to the target, so they are exported
target_include_directories(VC4CC PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>  $<INSTALL_INTERFACE:include/vc4cc>)
# Creates the install target for the library and the compiler
install(TARGETS VC4CC EXPORT VC4CC-targets LIBRARY DESTINATION lib)
install(TARGETS VC4C EXPORT VC4C-targets RUNTIME DESTINATION bin)
# Creates the export target (to be used by CMake to find the INSTALLED library)
install(EXPORT VC4CC-targets DESTINATION share/vc4cc)
# Creates the install target for the headers
install(DIRECTORY "${PROJECT_SOURCE_DIR}/include/" DESTINATION include/vc4cc FILES_MATCHING PATTERN "*.h")
# Exports the target (to be used by CMake to find the SOURCE library)
export(TARGETS VC4CC FILE vc4cc-exports.cmake)
# Adds custom uninstall command
add_custom_target(uninstall "${CMAKE_COMMAND}" -P "cmake_uninstall.cmake")