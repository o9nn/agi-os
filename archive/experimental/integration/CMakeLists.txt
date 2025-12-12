CMAKE_MINIMUM_REQUIRED(VERSION 3.12)
PROJECT(occ-integration VERSION 1.0.0)

MESSAGE(STATUS "Building OCC Integration Layer")

# Integration components
OPTION(BUILD_ATOMSPACE_MACHSPACE "Build AtomSpace-MachSpace bridge" ON)
OPTION(BUILD_COGNITIVE_SCHEDULER "Build cognitive scheduler" ON)
OPTION(BUILD_COGKERNEL_BRIDGE "Build CogKernel bridge" OFF)

# AtomSpace-MachSpace bridge
IF(BUILD_ATOMSPACE_MACHSPACE)
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/atomspace-machspace/CMakeLists.txt")
        MESSAGE(STATUS "Building AtomSpace-MachSpace bridge...")
        add_subdirectory(atomspace-machspace)
    ELSE()
        MESSAGE(WARNING "AtomSpace-MachSpace bridge CMakeLists.txt not found")
    ENDIF()
ENDIF()

# Cognitive scheduler
IF(BUILD_COGNITIVE_SCHEDULER)
    MESSAGE(STATUS "Building cognitive scheduler...")
    
    # Build cognitive scheduler library
    ADD_LIBRARY(cognitive-scheduler STATIC
        cognitive-scheduler/cognitive_scheduler.c
    )
    
    TARGET_INCLUDE_DIRECTORIES(cognitive-scheduler PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/cognitive-scheduler
    )
    
    # Install library and headers
    INSTALL(TARGETS cognitive-scheduler
        ARCHIVE DESTINATION lib
    )
    
    INSTALL(FILES
        cognitive-scheduler/cognitive_scheduler.h
        DESTINATION include/cognumach
    )
ENDIF()

# CogKernel bridge (to be implemented)
IF(BUILD_COGKERNEL_BRIDGE)
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cogkernel-bridge/CMakeLists.txt")
        MESSAGE(STATUS "Building CogKernel bridge...")
        add_subdirectory(cogkernel-bridge)
    ELSE()
        MESSAGE(WARNING "CogKernel bridge not yet implemented")
    ENDIF()
ENDIF()

MESSAGE(STATUS "Integration layer configuration complete")
