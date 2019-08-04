find_program(
  CLANG_FORMAT_PATH
  NAMES clang-format clang-format-3.8 clang-format-3.9 clang-format-4.0
  HINTS "/usr/bin" "/usr/local/bin"
)

if(CLANG_FORMAT_PATH)
  message (STATUS "found clang-format: ${CLANG_FORMAT_PATH}")
  get_target_property(VC4CC_SRCS ${VC4C_LIBRARY_NAME} SOURCES)
  get_target_property(VC4C_SRCS ${VC4C_PROGRAM_NAME} SOURCES)
  add_custom_target(
    clang-format
    COMMAND ${CLANG_FORMAT_PATH}
      -i ${VC4CC_SRCS} ${VC4C_SRCS}
    WORKING_DIRECTORY
      ${CMAKE_CURRENT_SOURCE_DIR}
  )
else()
  message (WARNING "clang-format not found: strongly recommend to use it before commit!")
endif()