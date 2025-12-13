cmake_minimum_required(VERSION 3.10)

project(egregore C)

# Source files
set(SOURCES
    egregore.c
)

# Header files
set(HEADERS
    egregore.h
)

# Create library
add_library(egregore SHARED ${SOURCES})
add_library(egregore-static STATIC ${SOURCES})

# Set library properties
set_target_properties(egregore PROPERTIES
    VERSION 1.0.0
    SOVERSION 1
    PUBLIC_HEADER "${HEADERS}"
)

set_target_properties(egregore-static PROPERTIES
    OUTPUT_NAME egregore
    PUBLIC_HEADER "${HEADERS}"
)

# Include directories
target_include_directories(egregore PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../vortex>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../morphule>
    $<INSTALL_INTERFACE:include/agi-os/egregore>
)

target_include_directories(egregore-static PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../vortex>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../morphule>
    $<INSTALL_INTERFACE:include/agi-os/egregore>
)

# Link libraries
target_link_libraries(egregore vortex morphule m)
target_link_libraries(egregore-static vortex-static morphule-static m)

# Compiler flags
target_compile_options(egregore PRIVATE
    -Wall -Wextra -Wpedantic
    -O3 -march=native
)

target_compile_options(egregore-static PRIVATE
    -Wall -Wextra -Wpedantic
    -O3 -march=native
)

# Installation
install(TARGETS egregore egregore-static
    EXPORT egregore-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include/agi-os/egregore
)

# Export targets
install(EXPORT egregore-targets
    FILE egregore-targets.cmake
    NAMESPACE AGI-OS::
    DESTINATION lib/cmake/egregore
)

# Create config file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/egregore-config-version.cmake"
    VERSION 1.0.0
    COMPATIBILITY AnyNewerVersion
)

configure_file(egregore-config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/egregore-config.cmake"
    @ONLY
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/egregore-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/egregore-config-version.cmake"
    DESTINATION lib/cmake/egregore
)
