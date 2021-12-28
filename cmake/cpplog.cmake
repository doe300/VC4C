if(FALSE AND DEPENDENCIES_USE_FETCH_CONTENT)
	# TODO disabled for now, since cpplog uninstall target conflicts with VC4C uninstall target
	include(FetchContent)
	FetchContent_Declare(cpplog GIT_REPOSITORY https://github.com/doe300/cpplog.git GIT_TAG v0.6)
	# CMake configuration flags for cpplog project
	set(CPPLOG_NAMESPACE logging)
	set(CPPLOG_CUSTOM_LOGGER true)
	FetchContent_MakeAvailable(cpplog)
	FetchContent_GetProperties(cpplog BINARY_DIR CPPLOG_BINARY_DIR)
	FetchContent_GetProperties(cpplog SOURCE_DIR CPPLOG_SOURCE_DIR)
	
	add_library(cpplog-dependencies STATIC IMPORTED)
	set(cpplog_LIBS cpplog-static)
	# TODO need to set defines separately?
else()
	ExternalProject_Add( cpplog-project
		PREFIX 				${CMAKE_BINARY_DIR}/cpplog
		GIT_REPOSITORY 		https://github.com/doe300/cpplog.git
		GIT_TAG				v0.6
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

	add_library(cpplog-static STATIC IMPORTED)
	ExternalProject_Get_Property(cpplog-project BINARY_DIR)
	ExternalProject_Get_Property(cpplog-project SOURCE_DIR)
	set(cpplog_LIBS "${BINARY_DIR}/libcpplog-static.a")
	set(cpplog_HEADERS $<BUILD_INTERFACE:${SOURCE_DIR}/include>)
	set(cpplog_DEFINES CPPLOG_NAMESPACE=logging CPPLOG_CUSTOM_LOGGER=true)
	
	add_library(cpplog-dependencies STATIC IMPORTED)
	add_dependencies(cpplog-dependencies cpplog-project-build)
endif()