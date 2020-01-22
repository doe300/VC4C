####
# Find libLLVM and configuration to use
####

# We cannot use SPIRV-LLVM/clang with libLLVM from default clang. So if SPIRV-LLVM is configured, force to use its libLLVM
if(SPIRV_CLANG_FOUND AND NOT SPIRV_TRANSLATOR_ROOT)
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