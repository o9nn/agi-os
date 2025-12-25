# AGI-OS Native Code Integrations
# CMake configuration for cognitive computing integration layers

cmake_minimum_required(VERSION 3.12)
project(agi-os-integrations VERSION 1.0.0 LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Build options
option(BUILD_NARS_INTEGRATION "Build NARS integration layer" ON)
option(BUILD_GGML_INTEGRATION "Build GGML integration layer" ON)
option(BUILD_METTA_INTEGRATION "Build MeTTa integration layer" ON)

# Find optional dependencies
find_library(OPENNARS_LIB opennars PATHS ${CMAKE_BINARY_DIR}/../opennars)
find_library(GGML_LIB ggml PATHS ${CMAKE_BINARY_DIR}/../ggml-core)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/..
    ${CMAKE_CURRENT_SOURCE_DIR}/../opennars/src
    ${CMAKE_CURRENT_SOURCE_DIR}/../ggml-core/include
)

# ==============================================================================
# Cognitive Synergy Bridge Core
# ==============================================================================

add_library(cognitive-synergy-bridge STATIC
    ../cognitive_synergy_bridge.c
)

target_include_directories(cognitive-synergy-bridge PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/..
)

target_link_libraries(cognitive-synergy-bridge PUBLIC
    pthread
    m
)

# ==============================================================================
# NARS Integration
# ==============================================================================

if(BUILD_NARS_INTEGRATION)
    add_library(nars-integration STATIC
        nars_integration.c
    )
    
    target_include_directories(nars-integration PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}/../opennars/src
    )
    
    target_link_libraries(nars-integration PUBLIC
        cognitive-synergy-bridge
        pthread
    )
    
    if(OPENNARS_LIB)
        target_compile_definitions(nars-integration PRIVATE HAVE_OPENNARS)
        target_link_libraries(nars-integration PUBLIC ${OPENNARS_LIB})
    endif()
endif()

# ==============================================================================
# GGML Integration
# ==============================================================================

if(BUILD_GGML_INTEGRATION)
    add_library(ggml-integration STATIC
        ggml_integration.c
    )
    
    target_include_directories(ggml-integration PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}/../ggml-core/include
    )
    
    target_link_libraries(ggml-integration PUBLIC
        cognitive-synergy-bridge
        pthread
        m
    )
    
    if(GGML_LIB)
        target_compile_definitions(ggml-integration PRIVATE HAVE_GGML)
        target_link_libraries(ggml-integration PUBLIC ${GGML_LIB})
    endif()
endif()

# ==============================================================================
# MeTTa Integration
# ==============================================================================

if(BUILD_METTA_INTEGRATION)
    add_library(metta-integration STATIC
        metta_integration.c
    )
    
    target_include_directories(metta-integration PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
    )
    
    target_link_libraries(metta-integration PUBLIC
        cognitive-synergy-bridge
        pthread
    )
    
    # Check for Hyperon C API
    find_library(HYPERON_LIB hyperonc PATHS ${CMAKE_BINARY_DIR}/../hyperon/c)
    if(HYPERON_LIB)
        target_compile_definitions(metta-integration PRIVATE HAVE_HYPERON)
        target_link_libraries(metta-integration PUBLIC ${HYPERON_LIB})
    endif()
endif()

# ==============================================================================
# Unified Integration Library
# ==============================================================================

add_library(agi-os-integrations STATIC
    ../cognitive_synergy_bridge.c
    $<$<BOOL:${BUILD_NARS_INTEGRATION}>:nars_integration.c>
    $<$<BOOL:${BUILD_GGML_INTEGRATION}>:ggml_integration.c>
    $<$<BOOL:${BUILD_METTA_INTEGRATION}>:metta_integration.c>
)

target_include_directories(agi-os-integrations PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/..
)

target_link_libraries(agi-os-integrations PUBLIC
    pthread
    m
)

# ==============================================================================
# Installation
# ==============================================================================

install(TARGETS 
    cognitive-synergy-bridge
    agi-os-integrations
    ARCHIVE DESTINATION lib
)

if(BUILD_NARS_INTEGRATION)
    install(TARGETS nars-integration ARCHIVE DESTINATION lib)
endif()

if(BUILD_GGML_INTEGRATION)
    install(TARGETS ggml-integration ARCHIVE DESTINATION lib)
endif()

if(BUILD_METTA_INTEGRATION)
    install(TARGETS metta-integration ARCHIVE DESTINATION lib)
endif()

install(FILES
    ../cognitive_synergy_bridge.h
    nars_integration.h
    ggml_integration.h
    metta_integration.h
    DESTINATION include/agi-os
)

# ==============================================================================
# Summary
# ==============================================================================

message(STATUS "AGI-OS Integrations configuration:")
message(STATUS "  NARS Integration: ${BUILD_NARS_INTEGRATION}")
message(STATUS "  GGML Integration: ${BUILD_GGML_INTEGRATION}")
message(STATUS "  MeTTa Integration: ${BUILD_METTA_INTEGRATION}")
