# KoboldCpp-Cog: Cognitive LLM Inference Bridge for AGI-OS
# Integrates KoboldCpp GGUF inference with OpenCog AtomSpace
#
# This component bridges local LLM inference (via KoboldCpp's OpenAI-compatible
# API) with the OpenCog cognitive architecture, enabling:
#   - AtomSpace-contextualized inference requests
#   - Cognitive agent LLM tool use
#   - PLN-guided prompt construction
#   - ECAN attention-weighted context selection
#   - MCP server for cognitive LLM tools
#
# Build dependencies:
#   - ggml-tensor (Stage 1)
#   - atomspace (Stage 2)
#   - cogutil (Stage 1)
#   - Optional: koboldcpp runtime for inference
#
cmake_minimum_required(VERSION 3.16)
project(koboldcpp-cog VERSION 1.0.0 LANGUAGES CXX C)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

message(STATUS "Configuring KoboldCpp-Cog ${PROJECT_VERSION}")

# ============================================================================
# Dependencies
# ============================================================================

# Required: cogutil and atomspace
find_package(cogutil CONFIG)
find_package(AtomSpace CONFIG)

# Optional: GGML for direct tensor operations
find_library(GGML_LIBRARY NAMES ggml)
find_path(GGML_INCLUDE_DIR NAMES ggml.h)

if(GGML_LIBRARY AND GGML_INCLUDE_DIR)
    set(HAVE_GGML TRUE)
    message(STATUS "  GGML found: ${GGML_LIBRARY}")
else()
    set(HAVE_GGML FALSE)
    message(STATUS "  GGML not found (API-only mode)")
endif()

# Optional: libcurl for HTTP client
find_package(CURL)
if(CURL_FOUND)
    set(HAVE_CURL TRUE)
    message(STATUS "  libcurl found: ${CURL_LIBRARIES}")
else()
    set(HAVE_CURL FALSE)
    message(STATUS "  libcurl not found (will use built-in HTTP)")
endif()

# Optional: nlohmann_json
find_package(nlohmann_json QUIET)
if(NOT nlohmann_json_FOUND)
    message(STATUS "  nlohmann_json not found, using bundled header")
endif()

# ============================================================================
# Source files
# ============================================================================

set(KOBOLDCPP_COG_SOURCES
    src/koboldcpp_client.cpp
    src/atomspace_context.cpp
    src/cognitive_inference.cpp
    src/prompt_builder.cpp
    src/koboldcpp_cog_module.cpp
)

set(KOBOLDCPP_COG_HEADERS
    include/opencog/koboldcpp/koboldcpp_client.h
    include/opencog/koboldcpp/atomspace_context.h
    include/opencog/koboldcpp/cognitive_inference.h
    include/opencog/koboldcpp/prompt_builder.h
    include/opencog/koboldcpp/koboldcpp_cog_module.h
)

# ============================================================================
# Build targets
# ============================================================================

# Shared library
add_library(koboldcpp-cog SHARED ${KOBOLDCPP_COG_SOURCES})

target_include_directories(koboldcpp-cog PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

if(cogutil_FOUND)
    target_link_libraries(koboldcpp-cog PUBLIC cogutil::cogutil)
endif()

if(AtomSpace_FOUND)
    target_link_libraries(koboldcpp-cog PUBLIC AtomSpace::AtomSpace)
endif()

if(HAVE_GGML)
    target_link_libraries(koboldcpp-cog PRIVATE ${GGML_LIBRARY})
    target_include_directories(koboldcpp-cog PRIVATE ${GGML_INCLUDE_DIR})
    target_compile_definitions(koboldcpp-cog PRIVATE HAVE_GGML=1)
endif()

if(HAVE_CURL)
    target_link_libraries(koboldcpp-cog PRIVATE CURL::libcurl)
    target_compile_definitions(koboldcpp-cog PRIVATE HAVE_CURL=1)
endif()

# ============================================================================
# Service launcher
# ============================================================================

add_executable(koboldcpp-cog-service src/service_main.cpp)
target_link_libraries(koboldcpp-cog-service PRIVATE koboldcpp-cog)

# ============================================================================
# Installation
# ============================================================================

install(TARGETS koboldcpp-cog koboldcpp-cog-service
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

install(DIRECTORY include/opencog/
    DESTINATION include/opencog
    FILES_MATCHING PATTERN "*.h"
)

# Python module
install(FILES python/koboldcpp_cog.py
    DESTINATION lib/python3/dist-packages/opencog
)

# Scheme module
install(FILES scm/koboldcpp-cog.scm
    DESTINATION share/guile/site/3.0/opencog
)

# MCP server script
install(PROGRAMS scripts/koboldcpp-cog-mcp
    DESTINATION bin
)

# ============================================================================
# Tests
# ============================================================================

option(BUILD_KOBOLDCPP_COG_TESTS "Build KoboldCpp-Cog tests" OFF)
if(BUILD_KOBOLDCPP_COG_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# ============================================================================
# Summary
# ============================================================================

message(STATUS "KoboldCpp-Cog configuration:")
message(STATUS "  GGML direct tensor ops: ${HAVE_GGML}")
message(STATUS "  HTTP client (libcurl): ${HAVE_CURL}")
message(STATUS "  CogUtil: ${cogutil_FOUND}")
message(STATUS "  AtomSpace: ${AtomSpace_FOUND}")
