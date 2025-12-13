cmake_minimum_required(VERSION 3.10)

project(deep-tree-echo-bridge C)

# Source files
set(SOURCES
    vortex_bridge.c
)

# Header files
set(HEADERS
    vortex_bridge.h
    deep_tree_9p.h
)

# Create library
add_library(deep-tree-echo-bridge SHARED ${SOURCES})
add_library(deep-tree-echo-bridge-static STATIC ${SOURCES})

# Set library properties
set_target_properties(deep-tree-echo-bridge PROPERTIES
    VERSION 1.0.0
    SOVERSION 1
    PUBLIC_HEADER "${HEADERS}"
)

set_target_properties(deep-tree-echo-bridge-static PROPERTIES
    OUTPUT_NAME deep-tree-echo-bridge
    PUBLIC_HEADER "${HEADERS}"
)

# Include directories
target_include_directories(deep-tree-echo-bridge PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../../inferno-kernel/vortex>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../../inferno-kernel/morphule>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../../inferno-kernel/egregore>
    $<INSTALL_INTERFACE:include/agi-os/deep-tree-echo-bridge>
)

target_include_directories(deep-tree-echo-bridge-static PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../../inferno-kernel/vortex>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../../inferno-kernel/morphule>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../../inferno-kernel/egregore>
    $<INSTALL_INTERFACE:include/agi-os/deep-tree-echo-bridge>
)

# Link libraries
target_link_libraries(deep-tree-echo-bridge vortex morphule egregore m)
target_link_libraries(deep-tree-echo-bridge-static vortex-static morphule-static egregore-static m)

# Compiler flags
target_compile_options(deep-tree-echo-bridge PRIVATE
    -Wall -Wextra -Wpedantic
    -O3 -march=native
)

target_compile_options(deep-tree-echo-bridge-static PRIVATE
    -Wall -Wextra -Wpedantic
    -O3 -march=native
)

# Installation
install(TARGETS deep-tree-echo-bridge deep-tree-echo-bridge-static
    EXPORT deep-tree-echo-bridge-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include/agi-os/deep-tree-echo-bridge
)

# Export targets
install(EXPORT deep-tree-echo-bridge-targets
    FILE deep-tree-echo-bridge-targets.cmake
    NAMESPACE AGI-OS::
    DESTINATION lib/cmake/deep-tree-echo-bridge
)

# Create config file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/deep-tree-echo-bridge-config-version.cmake"
    VERSION 1.0.0
    COMPATIBILITY AnyNewerVersion
)

configure_file(deep-tree-echo-bridge-config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/deep-tree-echo-bridge-config.cmake"
    @ONLY
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/deep-tree-echo-bridge-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/deep-tree-echo-bridge-config-version.cmake"
    DESTINATION lib/cmake/deep-tree-echo-bridge
)
