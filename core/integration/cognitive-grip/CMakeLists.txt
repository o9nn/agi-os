# Cognitive-Grip: Unified Abstraction Layer for AGI-OS
# Provides seamless integration between CogNumach, HurdCog, and OpenCog

cmake_minimum_required(VERSION 3.12)
project(cognitive-grip VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(cogutil REQUIRED)
find_package(AtomSpace REQUIRED)
find_package(atomspace-storage REQUIRED)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

# Source files
set(SOURCES
    src/cognitive_grip.cpp
    src/machspace_bridge.cpp
    src/hurdcog_bridge.cpp
    src/cognumach_bridge.cpp
    src/cogbolt_bridge.cpp
    src/unified_config.cpp
)

# Create library
add_library(cognitive-grip SHARED ${SOURCES})

# Link libraries
target_link_libraries(cognitive-grip
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    atomspace-storage
)

# Installation
install(TARGETS cognitive-grip
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(DIRECTORY include/
    DESTINATION include/opencog/cognitive-grip
)

# Enable testing
enable_testing()
add_subdirectory(tests)
