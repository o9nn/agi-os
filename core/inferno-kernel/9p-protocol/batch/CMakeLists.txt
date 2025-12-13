cmake_minimum_required(VERSION 3.10)

project(9p-batch C)

# Source files
set(SOURCES
    9p-batch.c
)

# Header files
set(HEADERS
    9p-batch.h
)

# Create library
add_library(9p-batch SHARED ${SOURCES})
add_library(9p-batch-static STATIC ${SOURCES})

# Set library properties
set_target_properties(9p-batch PROPERTIES
    VERSION 1.0.0
    SOVERSION 1
    PUBLIC_HEADER "${HEADERS}"
)

set_target_properties(9p-batch-static PROPERTIES
    OUTPUT_NAME 9p-batch
    PUBLIC_HEADER "${HEADERS}"
)

# Include directories
target_include_directories(9p-batch PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include/agi-os/9p-batch>
)

target_include_directories(9p-batch-static PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include/agi-os/9p-batch>
)

# Compiler flags
target_compile_options(9p-batch PRIVATE
    -Wall -Wextra -Wpedantic
    -O3 -march=native
)

target_compile_options(9p-batch-static PRIVATE
    -Wall -Wextra -Wpedantic
    -O3 -march=native
)

# Installation
install(TARGETS 9p-batch 9p-batch-static
    EXPORT 9p-batch-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include/agi-os/9p-batch
)

# Export targets
install(EXPORT 9p-batch-targets
    FILE 9p-batch-targets.cmake
    NAMESPACE AGI-OS::
    DESTINATION lib/cmake/9p-batch
)

# Create config file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/9p-batch-config-version.cmake"
    VERSION 1.0.0
    COMPATIBILITY AnyNewerVersion
)

configure_file(9p-batch-config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/9p-batch-config.cmake"
    @ONLY
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/9p-batch-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/9p-batch-config-version.cmake"
    DESTINATION lib/cmake/9p-batch
)
