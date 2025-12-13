cmake_minimum_required(VERSION 3.10)

project(vortex C)

# Source files
set(SOURCES
    matula.c
    vorticity.c
)

# Header files
set(HEADERS
    matula.h
    vorticity.h
)

# Create library
add_library(vortex SHARED ${SOURCES})
add_library(vortex-static STATIC ${SOURCES})

# Set library properties
set_target_properties(vortex PROPERTIES
    VERSION 1.0.0
    SOVERSION 1
    PUBLIC_HEADER "${HEADERS}"
)

set_target_properties(vortex-static PROPERTIES
    OUTPUT_NAME vortex
    PUBLIC_HEADER "${HEADERS}"
)

# Include directories
target_include_directories(vortex PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include/agi-os/vortex>
)

target_include_directories(vortex-static PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include/agi-os/vortex>
)

# Link math library
target_link_libraries(vortex m)
target_link_libraries(vortex-static m)

# Compiler flags
target_compile_options(vortex PRIVATE
    -Wall -Wextra -Wpedantic
    -O3 -march=native
)

target_compile_options(vortex-static PRIVATE
    -Wall -Wextra -Wpedantic
    -O3 -march=native
)

# Installation
install(TARGETS vortex vortex-static
    EXPORT vortex-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include/agi-os/vortex
)

# Export targets
install(EXPORT vortex-targets
    FILE vortex-targets.cmake
    NAMESPACE AGI-OS::
    DESTINATION lib/cmake/vortex
)

# Create config file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/vortex-config-version.cmake"
    VERSION 1.0.0
    COMPATIBILITY AnyNewerVersion
)

configure_file(vortex-config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/vortex-config.cmake"
    @ONLY
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/vortex-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/vortex-config-version.cmake"
    DESTINATION lib/cmake/vortex
)

# Build tests
option(BUILD_VORTEX_TESTS "Build vortex tests" ON)
if(BUILD_VORTEX_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()
