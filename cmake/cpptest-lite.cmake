if(FALSE AND DEPENDENCIES_USE_FETCH_CONTENT)
	# TODO disabled for now, since cpptest-lite uninstall target conflicts with VC4C uninstall target
	include(FetchContent)
	FetchContent_Declare(cpptest-lite GIT_REPOSITORY https://github.com/doe300/cpptest-lite.git)
	FetchContent_MakeAvailable(cpptest-lite)
	FetchContent_GetProperties(cpptest-lite SOURCE_DIR CPPTEST_SOURCE_DIR)
	
	add_library(cpptest-lite-dependencies STATIC IMPORTED)
	set(cpptest_LIBS cpptest-lite)
	set(cpptest_HEADERS "")
else()
	ExternalProject_Add(cpptest-lite-project
		PREFIX 				${CMAKE_BINARY_DIR}/cpptest-lite
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
	
	ExternalProject_Get_Property(cpptest-lite-project BINARY_DIR)
	ExternalProject_Get_Property(cpptest-lite-project SOURCE_DIR)
	
	set(cpptest_LIBS "${BINARY_DIR}/libcpptest-lite.so")
	set (cpptest_HEADERS "${SOURCE_DIR}/include")
	
	add_library(cpptest-lite-dependencies STATIC IMPORTED)
	add_dependencies(cpptest-lite-dependencies cpptest-lite-project-build)
endif()