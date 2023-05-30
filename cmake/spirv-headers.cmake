# Always pull in the SPIR-V headers for our built-in front-end
if(DEPENDENCIES_USE_FETCH_CONTENT)
	# CMake 3.14 introduces https://cmake.org/cmake/help/latest/module/FetchContent.html which allows us to run the configuration step
	# of the downloaded dependencies at CMake configuration step and therefore, we have the proper targets available.
	include(FetchContent)
	set(SPIRV_HEADERS_SKIP_INSTALL ON CACHE INTERNAL "Disable installation of SPIR-V headers")
	set(SPIRV_HEADERS_SKIP_EXAMPLES ON CACHE INTERNAL "Disable building of SPIR-V header examples")
	FetchContent_Declare(SPIRV-Headers GIT_REPOSITORY https://github.com/KhronosGroup/SPIRV-Headers.git GIT_TAG main)
	FetchContent_MakeAvailable(SPIRV-Headers)
	FetchContent_GetProperties(SPIRV-Headers SOURCE_DIR SPIRV_HEADERS_SOURCE_DIR)

	set(SPIRV_Headers_HEADERS ${SPIRV_HEADERS_SOURCE_DIR}/include)

	# Dummy library, so we can depend on it
	add_library(SPIRV-Dependencies STATIC IMPORTED)
else()
	#Add SPIR-V headers project
	ExternalProject_Add(SPIRV-Headers-project
		PREFIX 				${CMAKE_BINARY_DIR}/spirv-headers
		GIT_REPOSITORY 		https://github.com/KhronosGroup/SPIRV-Headers.git
		GIT_TAG                 main
		UPDATE_COMMAND 		git pull -f https://github.com/KhronosGroup/SPIRV-Headers.git
		STEP_TARGETS 		build
		EXCLUDE_FROM_ALL	TRUE
		TIMEOUT 			30		#Timeout for downloads, in seconds
		CMAKE_ARGS
		  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
		  -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
		  -DCMAKE_FIND_ROOT_PATH=${CMAKE_FIND_ROOT_PATH}
		  -DSPIRV_HEADERS_SKIP_INSTALL=ON
		  -DSPIRV_HEADERS_SKIP_EXAMPLES=ON
	)
	ExternalProject_Get_Property(SPIRV-Headers-project SOURCE_DIR)
	set(SPIRV_Headers_HEADERS ${SOURCE_DIR}/include)

	# This target is used to collect the dependencies on all SPIR-V library build steps
	add_library(SPIRV-Dependencies STATIC IMPORTED)
	add_dependencies(SPIRV-Dependencies SPIRV-Headers-project-build)
endif()
