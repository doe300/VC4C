####
# Find LLVM SPIR-V compiler
####

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

####
# Enable SPIR-V Tools front-end
####

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