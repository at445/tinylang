cmake_minimum_required(VERSION 3.30.5)
set(CMAKE_CXX_STANDARD 17)
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fexceptions")
endif()
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  project(Tinylang)

  find_package(LLVM REQUIRED HINTS "${LLVM_CMAKE_PATH}")
  if(LLVM_FOUND)
    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "\$LLVM_DIR is ${LLVM_DIR}")
    message(STATUS "\$LLVM_BINARY_DIR is ${LLVM_BINARY_DIR}")
    message(STATUS "\$LLVM_INCLUDE_DIRS is ${LLVM_INCLUDE_DIRS}")
    message(STATUS "\$LLVM_LIBRARY_DIR is ${LLVM_LIBRARY_DIR}")
    message(STATUS, "\$CMAKE_SOURCE_DIR is ${CMAKE_SOURCE_DIR}/include")
    list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})

    include(AddLLVM)
    include(HandleLLVMOptions)
    
    include_directories("${LLVM_INCLUDE_DIRS}" "${CMAKE_SOURCE_DIR}/include")
    link_directories("${LLVM_LIBRARY_DIR}")

    set(TINYLANG_BUILT_STANDALONE 1)
  else()
    message(FATAL_ERROR "LLVM not found!")
  endif()
endif()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR AND NOT MSVC_IDE)
  message(FATAL_ERROR "In-source builds are not allowed. ")
endif()

message(STATUS "\$CMAKE_CURRENT_BINARY_DIR is ${CMAKE_CURRENT_BINARY_DIR}")
message(STATUS "\$CMAKE_CURRENT_SOURCE_DIR is ${CMAKE_CURRENT_SOURCE_DIR}")
set(TINYLANG_VERSION_STRING "0.2")
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/include/tinylang/Basic/Version.inc.in
  ${CMAKE_CURRENT_BINARY_DIR}/include/tinylang/Basic/Version.inc)

include(AddTinylang)

include_directories(BEFORE
  ${CMAKE_CURRENT_BINARY_DIR}/include
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  )

add_subdirectory(lib)
add_subdirectory(tools)
