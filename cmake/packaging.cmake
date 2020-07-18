# Generate postint-script for debian package, building the VC4CLStdLib PCH
configure_file(cmake/deb-postint.in "${CMAKE_BINARY_DIR}/postinst" @ONLY NEWLINE_STYLE UNIX)
configure_file(cmake/deb-prerem.in "${CMAKE_BINARY_DIR}/prerem" @ONLY NEWLINE_STYLE UNIX)

# We always need the vc4cl standard-library for installing the vc4c package
set(PACKAGE_DEPENDENCIES "vc4cl-stdlib")
if(NOT SPIRV_CLANG_FOUND AND CLANG_FOUND)
	# If we build with "default" clang, require its package
	set(PACKAGE_DEPENDENCIES "${PACKAGE_DEPENDENCIES}, clang-6.0")
	if(LLVMLIB_FRONTEND)
		# If we also build with libLLVM front-end, require the LLVM library and its development files too (llvm-6.0-dev contains the libLLVM.so)
		set(PACKAGE_DEPENDENCIES "${PACKAGE_DEPENDENCIES}, llvm-6.0, llvm-6.0-dev")
	endif()
	if(LIBCLANG_LIBRARY_PATH)
		# If we also build with libclang, require the library
		# TODO check versions/development headers required?
		set(PACKAGE_DEPENDENCIES "${PACKAGE_DEPENDENCIES}, libclang1-6.0, libclang-6.0-dev")
	endif()
endif()

set(CPACK_GENERATOR "DEB")
set(CPACK_PACKAGING_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")
set(CPACK_PACKAGE_NAME "vc4c")
string(TIMESTAMP BUILD_TIMESTAMP "%Y-%m-%d")
set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}-${BUILD_TIMESTAMP}")
set(CPACK_DEBIAN_PACKAGE_DEPENDS ${PACKAGE_DEPENDENCIES})
if (CROSS_COMPILE)
	set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "armhf")
else()
	set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
endif()
set(CPACK_PACKAGE_VENDOR "doe300")
set(CPACK_PACKAGE_CONTACT "doe300@web.de")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "OpenCL C compiler for VC4CL (raspberrypi only)")
set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://github.com/doe300/VC4C")
set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA ${CMAKE_BINARY_DIR}/postinst ${CMAKE_BINARY_DIR}/prerem)
set(CPACK_PACKAGE_FILE_NAME "vc4c-0.4-Linux")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
include(CPack)