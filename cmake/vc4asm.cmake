if(DEPENDENCIES_USE_FETCH_CONTENT)
	include(FetchContent)
	FetchContent_Declare(vc4asm GIT_REPOSITORY https://github.com/maazl/vc4asm.git)
	FetchContent_MakeAvailable(vc4asm)
	FetchContent_GetProperties(vc4asm BINARY_DIR VC4ASM_BINARY_DIR)
	FetchContent_GetProperties(vc4asm SOURCE_DIR VC4ASM_SOURCE_DIR)
	
	add_library(vc4asm-dependencies STATIC IMPORTED)
	# NOTE: There is no actual target for the static library, so link the file directly
	set(vc4asm_LIBS "${VC4ASM_BINARY_DIR}/libvc4asm.a")
	set(vc4asm_HEADER "${VC4ASM_SOURCE_DIR}/src/Validator.h")
else()
	ExternalProject_Add(vc4asm-project
		PREFIX				${CMAKE_BINARY_DIR}/vc4asm
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

	ExternalProject_Get_Property(vc4asm-project BINARY_DIR)
	ExternalProject_Get_Property(vc4asm-project SOURCE_DIR)
	set(vc4asm_LIBS "${BINARY_DIR}/libvc4asm.a")
	set(vc4asm_HEADER "${SOURCE_DIR}/src/Validator.h")
	# This target is used to collect the dependencies on the vc4asm build step
	add_library(vc4asm-dependencies STATIC IMPORTED)
	add_dependencies(vc4asm-dependencies vc4asm-project-build)
endif()