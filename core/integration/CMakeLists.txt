# AGI-OS Integration Layer
# Coordinates all integration components

# Cognitive-Grip: Original integration layer
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cognitive-grip/CMakeLists.txt")
    add_subdirectory(cognitive-grip)
    message(STATUS "  Cognitive-Grip integration layer configured")
endif()

# Unified Cognitive Interface: 9P-based unified interface
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/unified-cog-interface/CMakeLists.txt")
    add_subdirectory(unified-cog-interface)
    message(STATUS "  Unified Cognitive Interface configured")
endif()
