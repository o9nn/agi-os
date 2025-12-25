# AGI-OS Native Code Integration
# CMake configuration for external cognitive computing components

cmake_minimum_required(VERSION 3.16)
project(agi-os-native-code VERSION 1.0.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Build options
option(BUILD_HYPERON "Build Hyperon MeTTa implementation" ON)
option(BUILD_OPENNARS "Build OpenNARS reasoning system" ON)
option(BUILD_GGML "Build GGML tensor library" ON)
option(BUILD_DAS "Build Distributed AtomSpace" ON)
option(BUILD_SOAR_PATTERNS "Build Soar cognitive patterns" OFF)
option(BUILD_INFERNO_COMPONENTS "Build Inferno OS components" OFF)

# Output directories
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Include directories for cross-component integration
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/hyperon/c/src
    ${CMAKE_CURRENT_SOURCE_DIR}/opennars/src
    ${CMAKE_CURRENT_SOURCE_DIR}/ggml-core/include
    ${CMAKE_CURRENT_SOURCE_DIR}/das/src
)

# ==============================================================================
# OpenNARS for Applications - C-based reasoning system
# ==============================================================================
if(BUILD_OPENNARS)
    message(STATUS "Building OpenNARS reasoning system")
    
    file(GLOB OPENNARS_SOURCES
        opennars/src/*.c
        opennars/src/NetworkNAR/*.c
    )
    
    file(GLOB OPENNARS_HEADERS
        opennars/src/*.h
        opennars/src/NetworkNAR/*.h
    )
    
    add_library(opennars STATIC ${OPENNARS_SOURCES})
    target_include_directories(opennars PUBLIC 
        ${CMAKE_CURRENT_SOURCE_DIR}/opennars/src
        ${CMAKE_CURRENT_SOURCE_DIR}/opennars/src/NetworkNAR
    )
    target_compile_definitions(opennars PRIVATE 
        _POSIX_C_SOURCE=200809L
    )
    
    # OpenNARS executable
    add_executable(nars-shell opennars/src/main.c)
    target_link_libraries(nars-shell opennars m)
endif()

# ==============================================================================
# GGML - Tensor library for machine learning
# ==============================================================================
if(BUILD_GGML)
    message(STATUS "Building GGML tensor library")
    
    # Add GGML as subdirectory if CMakeLists.txt exists
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/ggml-core/CMakeLists.txt)
        add_subdirectory(ggml-core)
    endif()
endif()

# ==============================================================================
# Hyperon - MeTTa programming language (Rust-based, requires cargo)
# ==============================================================================
if(BUILD_HYPERON)
    message(STATUS "Hyperon integration configured (requires Rust/Cargo)")
    
    # Custom target for building Hyperon via Cargo
    add_custom_target(hyperon-rust
        COMMAND cargo build --release
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/hyperon
        COMMENT "Building Hyperon MeTTa implementation"
    )
    
    # C API for Hyperon
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/hyperon/c/CMakeLists.txt)
        add_subdirectory(hyperon/c hyperon-c-api)
    endif()
endif()

# ==============================================================================
# DAS - Distributed AtomSpace
# ==============================================================================
if(BUILD_DAS)
    message(STATUS "DAS integration configured")
    
    # DAS is primarily Python/Rust based
    # Add integration hooks for C++ components
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/das/experiments/metta-integration/lib/CMakeLists.txt)
        add_subdirectory(das/experiments/metta-integration/lib das-metta-lib)
    endif()
endif()

# ==============================================================================
# Soar Cognitive Architecture Patterns
# ==============================================================================
if(BUILD_SOAR_PATTERNS)
    message(STATUS "Building Soar cognitive patterns")
    
    # Extract key cognitive patterns from Soar
    add_subdirectory(soar-patterns/Core/SoarKernel soar-kernel)
endif()

# ==============================================================================
# Inferno OS Components
# ==============================================================================
if(BUILD_INFERNO_COMPONENTS)
    message(STATUS "Inferno OS components configured")
    
    # Inferno uses its own build system (mk)
    # Add integration hooks for key libraries
    add_library(inferno-lib9 INTERFACE)
    target_include_directories(inferno-lib9 INTERFACE
        ${CMAKE_CURRENT_SOURCE_DIR}/inferno-components/include
    )
endif()

# ==============================================================================
# Integration Layer - Cognitive Synergy Bridge
# ==============================================================================
add_library(cognitive-synergy-bridge INTERFACE)
target_include_directories(cognitive-synergy-bridge INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# Export configuration
install(TARGETS opennars
    EXPORT agi-os-native-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

install(EXPORT agi-os-native-targets
    FILE agi-os-native-targets.cmake
    NAMESPACE agi-os::
    DESTINATION lib/cmake/agi-os-native
)

# Generate package config
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/agi-os-native-config-version.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/agi-os-native-config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/agi-os-native-config.cmake"
    INSTALL_DESTINATION lib/cmake/agi-os-native
)

message(STATUS "AGI-OS Native Code Integration configured")
message(STATUS "  - OpenNARS: ${BUILD_OPENNARS}")
message(STATUS "  - GGML: ${BUILD_GGML}")
message(STATUS "  - Hyperon: ${BUILD_HYPERON}")
message(STATUS "  - DAS: ${BUILD_DAS}")
message(STATUS "  - Soar Patterns: ${BUILD_SOAR_PATTERNS}")
message(STATUS "  - Inferno Components: ${BUILD_INFERNO_COMPONENTS}")
