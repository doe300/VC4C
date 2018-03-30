
###
# Emulator
###
add_executable(qpu_emulator emulator.cpp)
target_link_libraries(qpu_emulator VC4CC)
target_include_directories(qpu_emulator PRIVATE "${PROJECT_SOURCE_DIR}/lib/cpplog/include")
target_include_directories(qpu_emulator PRIVATE "${PROJECT_SOURCE_DIR}/src")
target_include_directories(qpu_emulator PRIVATE "${PROJECT_SOURCE_DIR}/include")
target_compile_definitions(qpu_emulator PRIVATE CPPLOG_NAMESPACE=logging CPPLOG_CUSTOM_LOGGER=true)