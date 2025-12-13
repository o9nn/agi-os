cmake_minimum_required(VERSION 3.10)

project(morphule C)

# Source files
set(SOURCES
    morphule.c
)

# Header files
set(HEADERS
    morphule.h
)

# Create library
add_library(morphule SHARED ${SOURCES})
add_library(morphule-static STATIC ${SOURCES})

# Set library properties
set_target_properties(morphule PROPERTIES
    VERSION 1.0.0
    SOVERSION 1
    PUBLIC_HEADER "${HEADERS}"
)

set_target_properties(morphule-static PROPERTIES
    OUTPUT_NAME morphule
    PUBLIC_HEADER "${HEADERS}"
)

# Include directories
target_include_directories(morphule PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../vortex>
    $<INSTALL_INTERFACE:include/agi-os/morphule>
)

target_include_directories(morphule-static PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../vortex>
    $<INSTALL_INTERFACE:include/agi-os/morphule>
)

# Link vortex library
target_link_libraries(morphule vortex m)
target_link_libraries(morphule-static vortex-static m)

# Compiler flags
target_compile_options(morphule PRIVATE
    -Wall -Wextra -Wpedantic
    -O3 -march=native
)

target_compile_options(morphule-static PRIVATE
    -Wall -Wextra -Wpedantic
    -O3 -march=native
)

# Installation
install(TARGETS morphule morphule-static
    EXPORT morphule-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include/agi-os/morphule
)

# Export targets
install(EXPORT morphule-targets
    FILE morphule-targets.cmake
    NAMESPACE AGI-OS::
    DESTINATION lib/cmake/morphule
)

# Create config file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/morphule-config-version.cmake"
    VERSION 1.0.0
    COMPATIBILITY AnyNewerVersion
)

configure_file(morphule-config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/morphule-config.cmake"
    @ONLY
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/morphule-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/morphule-config-version.cmake"
    DESTINATION lib/cmake/morphule
)
