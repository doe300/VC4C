if(DEPENDENCIES_USE_FETCH_CONTENT)
	include(FetchContent)
	FetchContent_Declare(variant GIT_REPOSITORY https://github.com/mpark/variant.git)
	FetchContent_MakeAvailable(variant)
	FetchContent_GetProperties(variant SOURCE_DIR VARIANT_SOURCE_DIR)

	add_library(variant-dependencies STATIC IMPORTED)
	set(variant_HEADERS "${VARIANT_SOURCE_DIR}/include")
else()
	ExternalProject_Add( variant-project
		PREFIX 				${CMAKE_BINARY_DIR}/variant
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

	ExternalProject_Get_Property(variant-project SOURCE_DIR)
	set(variant_HEADERS ${SOURCE_DIR}/include)

	add_library(variant-dependencies STATIC IMPORTED)
	add_dependencies(variant-dependencies variant-project-build)
endif()
