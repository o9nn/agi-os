#
# Echo Angel — Cognitive Avatar Kernel Module
#
# Composition: agi-os( circled-operators[ agi-os[ agi-os[ {{ echo-angel }} ] ] ] )
#
# Build targets:
#   echo_angel_kernel    - Core cognitive cycle + ESN + endocrine + FACS
#   echo_angel_9p        - 9P filesystem interface
#   echo_angel_expression - MetaHuman DNA expression pipeline
#   echo_angel_introspection - Autognosis introspection engine
#   echo_angel_platform  - AI Angel platform layer
#   echo_angel           - Combined static library
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)
PROJECT(echo-angel VERSION 0.1.0 LANGUAGES C)

# ============================================================
# Compiler settings
# ============================================================
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# ============================================================
# Source files by component
# ============================================================

# Cognitive Core (unreal-echo ⊗)
set(KERNEL_SOURCES
    kernel/echo_angel_kernel.c
)

# 9P Filesystem Interface
set(NINEP_SOURCES
    kernel/echo_angel_9p.c
)

# Expression Pipeline (meta-echo-dna ⊗)
set(EXPRESSION_SOURCES
    expression/metahuman_dna_bridge.c
)

# Introspection Engine (echo-introspect ⊗)
set(INTROSPECTION_SOURCES
    introspection/autognosis_engine.c
)

# Platform Layer (aiangel-platform ⊕)
set(PLATFORM_SOURCES
    platform/aiangel_platform.c
)

# ============================================================
# Include directories
# ============================================================
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# ============================================================
# Individual component libraries
# ============================================================

add_library(echo_angel_kernel STATIC ${KERNEL_SOURCES})
target_include_directories(echo_angel_kernel PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_library(echo_angel_9p STATIC ${NINEP_SOURCES})
target_link_libraries(echo_angel_9p echo_angel_kernel)

add_library(echo_angel_expression STATIC ${EXPRESSION_SOURCES})
target_link_libraries(echo_angel_expression echo_angel_kernel)

add_library(echo_angel_introspection STATIC ${INTROSPECTION_SOURCES})
target_link_libraries(echo_angel_introspection echo_angel_kernel)

add_library(echo_angel_platform STATIC ${PLATFORM_SOURCES})
target_link_libraries(echo_angel_platform echo_angel_kernel)

# ============================================================
# Combined library (all components)
# ============================================================

add_library(echo_angel STATIC
    ${KERNEL_SOURCES}
    ${NINEP_SOURCES}
    ${EXPRESSION_SOURCES}
    ${INTROSPECTION_SOURCES}
    ${PLATFORM_SOURCES}
)
target_include_directories(echo_angel PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(echo_angel m)

# ============================================================
# Install targets
# ============================================================

install(TARGETS echo_angel echo_angel_kernel echo_angel_9p
        echo_angel_expression echo_angel_introspection echo_angel_platform
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
)

install(FILES include/echo_angel.h
    DESTINATION include/echo-angel
)

# ============================================================
# Summary
# ============================================================
message(STATUS "Echo Angel Cognitive Avatar Module v${PROJECT_VERSION}")
message(STATUS "  Components:")
message(STATUS "    Kernel (Echobeats, ESN, Endocrine, FACS)")
message(STATUS "    9P Filesystem Interface")
message(STATUS "    Expression Pipeline (MetaHuman DNA)")
message(STATUS "    Introspection Engine (Autognosis)")
message(STATUS "    Platform Layer (AI Angel)")
