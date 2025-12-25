cmake_minimum_required(VERSION 3.12)
project(agi-os-native-tests LANGUAGES C)

set(CMAKE_C_STANDARD 11)

# Find the cognitive synergy bridge library
find_library(COG_BRIDGE_LIB cognitive-synergy-bridge
    PATHS ${CMAKE_BINARY_DIR}/../../build/native-code/integrations
)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../../external/native-code
    ${CMAKE_CURRENT_SOURCE_DIR}/../../external/native-code/integrations
)

# Test executable
add_executable(test_cognitive_bridge test_cognitive_bridge.c)
target_link_libraries(test_cognitive_bridge pthread m)

# Add test
enable_testing()
add_test(NAME cognitive_bridge_test COMMAND test_cognitive_bridge)
