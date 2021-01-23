# Code to embed text files in binary take from combination of:
# https://stackoverflow.com/a/47801116
# https://stackoverflow.com/a/411000
# https://stackoverflow.com/a/14873818

set(BINARY_DEPENCENCIES "")
set(STRING_DEFINITIONS "")
set(STRING_DECLARATIONS "")
function(create_header base_folder input_file)
	get_filename_component(output_folder ${input_file} DIRECTORY)
	file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/test_files/${output_folder})
	set(output_file ${CMAKE_CURRENT_BINARY_DIR}/test_files/${input_file}.o)
	string(REGEX REPLACE "[/.-]" "_" var_name ${input_file})
	add_custom_command(OUTPUT ${output_file}
		COMMAND ld -r -b binary -o ${output_file} ${input_file}
		COMMAND objcopy --rename-section .data=.rodata,alloc,load,readonly,data,contents ${output_file} ${output_file}
		MAIN_DEPENDENCY ${base_folder}/${input_file}
		WORKING_DIRECTORY ${base_folder}
		VERBATIM)
	list(APPEND BINARY_DEPENCENCIES ${output_file})
	set(BINARY_DEPENCENCIES ${BINARY_DEPENCENCIES} PARENT_SCOPE)
	set(STRING_DEFINITIONS "${STRING_DEFINITIONS}extern char _binary_${var_name}_start[];\\n")
	set(STRING_DEFINITIONS "${STRING_DEFINITIONS}extern char _binary_${var_name}_end[];\\n")
	set(STRING_DEFINITIONS "${STRING_DEFINITIONS}const std::string test_files::${var_name}_string(_binary_${var_name}_start, _binary_${var_name}_end);\\n")
	set(STRING_DEFINITIONS ${STRING_DEFINITIONS} PARENT_SCOPE)
	set(STRING_DECLARATIONS "${STRING_DECLARATIONS}extern const std::string ${var_name}_string;\\n")
	set(STRING_DECLARATIONS ${STRING_DECLARATIONS} PARENT_SCOPE)
endfunction(create_header)

create_header(${CMAKE_CURRENT_SOURCE_DIR}/../example/ fibonacci.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../example/ hello_world.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../example/ hello_world_constant.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../example/ hello_world_vector.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../example/ md5.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../example/ SHA-256.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../example/ test.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../example/ test_instructions.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../example/ test_prime.cl)

create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_async_copy.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_atomic.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_barrier.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_branches.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_conditional_address.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_constant_load.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_hashes.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ local_private_storage.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_other.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_sfu.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_shuffle.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_storage.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_struct.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ unaligned_memory_access.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_vector.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_vectorization.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_vpm_read.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_vpm_write.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ test_work_item.cl)

create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ bugs/30_local_memory.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ bugs/33_floating_point_folding.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ bugs/vc4cl_27_wrong_result.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ bugs/54_invalid_results.cl)

create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ boost-compute/initial_reduce.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ boost-compute/adjacent_find.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ boost-compute/test_functional_popcount.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ boost-compute/test_reduce.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ boost-compute/test_insertion_sort.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ boost-compute/test_merge.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ boost-compute/test_transform2.cl)

create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/pointer_cast.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/integer_add_sat.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/sub_sat.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/uchar_compare.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/async_copy_global_to_local.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/min_max_constant_args.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/sub_buffers_read_write.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/local_kernel_scope.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/barrier.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/clamp.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCL-CTS/cross_product.cl)

create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ BabelStream/OCLStream.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ clNN/SpatialUpSamplingNearest.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ deepCL/copy.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ deepCL/inv.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ deepCL/memset.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ HandsOnOpenCL/matmul.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ HandsOnOpenCL/pi_ocl.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ NVIDIA/VectorAdd.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ OpenCLIPP/Histogram.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ pocl/test_structs_as_args.cl)
create_header(${CMAKE_CURRENT_SOURCE_DIR}/../testing/ rodinia/nearestNeighbor_kernel.cl)

set(TEST_FILES_HEADER ${CMAKE_CURRENT_BINARY_DIR}/test_files.h)
add_custom_command(OUTPUT ${TEST_FILES_HEADER}
	COMMAND printf "#include <string>\\n" > ${TEST_FILES_HEADER}
	COMMAND printf "namespace test_files {\\n" >> ${TEST_FILES_HEADER}
	COMMAND printf "${STRING_DECLARATIONS}" >> ${TEST_FILES_HEADER}
	COMMAND printf "}\\n" >> ${TEST_FILES_HEADER}
	DEPENDS ${BINARY_DEPENCENCIES}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	VERBATIM)

set(TEST_FILES_SOURCE ${CMAKE_CURRENT_BINARY_DIR}/test_files/test_files.cpp)
add_custom_command(OUTPUT ${TEST_FILES_SOURCE}
	COMMAND printf "#include \"test_files.h\"\\n" > ${TEST_FILES_SOURCE}
	COMMAND printf "${STRING_DEFINITIONS}" >> ${TEST_FILES_SOURCE}
	DEPENDS ${BINARY_DEPENCENCIES} ${TEST_FILES_HEADER}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	VERBATIM)

add_library(TestData STATIC
	ArithmeticTests.cpp
	ConversionTests.cpp
	FloatTests.cpp
	IntegerTests.cpp
	MathTests.cpp
	MemoryTests.cpp
	RelationalTests.cpp
	VectorTests.cpp
	TestData.cpp
	${BINARY_DEPENCENCIES}
	${TEST_FILES_SOURCE}
)
target_include_directories(TestData PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
target_include_directories(TestData PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_options(TestData PRIVATE ${VC4C_ENABLED_WARNINGS})
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/TestData.h ${CMAKE_CURRENT_BINARY_DIR}/TestData.h COPYONLY)
