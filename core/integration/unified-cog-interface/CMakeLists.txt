# Unified Cognitive Interface for AGI-OS
# Provides cross-system cognitive primitives via 9P protocol

cmake_minimum_required(VERSION 3.12)
project(unified-cog-interface VERSION 1.0.0 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(cogutil QUIET)
find_package(AtomSpace QUIET)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)

if(COGUTIL_FOUND AND ATOMSPACE_FOUND)
    include_directories(
        ${COGUTIL_INCLUDE_DIRS}
        ${ATOMSPACE_INCLUDE_DIRS}
    )
endif()

# Source files
set(UNIFIED_COG_SOURCES
    src/unified_cog.c
)

set(COG9P_SERVER_SOURCES
    servers/cog9p.c
)

# Create unified cognitive interface library
add_library(unified-cog SHARED ${UNIFIED_COG_SOURCES})

# Link libraries (if OpenCog is available)
if(COGUTIL_FOUND AND ATOMSPACE_FOUND)
    target_link_libraries(unified-cog
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
    )
    target_compile_definitions(unified-cog PRIVATE HAVE_OPENCOG=1)
endif()

# Create 9P cognitive file server executable
add_executable(cog9p ${COG9P_SERVER_SOURCES})
target_link_libraries(cog9p unified-cog)

# Installation
install(TARGETS unified-cog cog9p
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

install(DIRECTORY include/
    DESTINATION include/agi-os/unified-cog
)

# Documentation
install(FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/../COGNITIVE_RECONCILIATION.md
    DESTINATION share/doc/unified-cog-interface
)

# Enable testing
enable_testing()

# Integration tests (if available)
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/tests)
    add_subdirectory(tests)
endif()
