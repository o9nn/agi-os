# OpenNARS for Applications - CMake Build Configuration
# AGI-OS Integration

cmake_minimum_required(VERSION 3.12)
project(opennars-native VERSION 0.9.2 LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Build options
option(OPENNARS_BUILD_SHARED "Build shared library" ON)
option(OPENNARS_BUILD_STATIC "Build static library" ON)
option(OPENNARS_BUILD_SHELL "Build NARS shell executable" ON)
option(OPENNARS_BUILD_TESTS "Build tests" OFF)
option(OPENNARS_ENABLE_NETWORK "Enable network NAR support" ON)

# Compiler flags
add_compile_options(-Wall -Wextra -O3)
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options(-ffast-math)
endif()

# Source files
set(OPENNARS_CORE_SOURCES
    src/Concept.c
    src/Config.c
    src/Cycle.c
    src/Decision.c
    src/Event.c
    src/FIFO.c
    src/HashTable.c
    src/Implication.c
    src/Inference.c
    src/Memory.c
    src/NAR.c
    src/Narsese.c
    src/PriorityQueue.c
    src/RuleTable.c
    src/Shell.c
    src/Stamp.c
    src/Table.c
    src/Term.c
    src/Truth.c
    src/Variable.c
)

set(OPENNARS_HEADERS
    src/Concept.h
    src/Config.h
    src/Cycle.h
    src/Decision.h
    src/Event.h
    src/FIFO.h
    src/Globals.h
    src/HashTable.h
    src/Implication.h
    src/Inference.h
    src/Memory.h
    src/NAR.h
    src/Narsese.h
    src/PriorityQueue.h
    src/RuleTable.h
    src/Shell.h
    src/Stamp.h
    src/Table.h
    src/Term.h
    src/Truth.h
    src/Variable.h
)

# Network NAR sources (optional)
if(OPENNARS_ENABLE_NETWORK)
    list(APPEND OPENNARS_CORE_SOURCES
        src/NetworkNAR/UDPNAR.c
    )
    list(APPEND OPENNARS_HEADERS
        src/NetworkNAR/UDPNAR.h
    )
    add_compile_definitions(ENABLE_NETWORK_NAR)
endif()

# Static library
if(OPENNARS_BUILD_STATIC)
    add_library(opennars-static STATIC ${OPENNARS_CORE_SOURCES})
    target_include_directories(opennars-static PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:include/opennars>
    )
    target_link_libraries(opennars-static PUBLIC m)
    set_target_properties(opennars-static PROPERTIES
        OUTPUT_NAME opennars
        POSITION_INDEPENDENT_CODE ON
    )
endif()

# Shared library
if(OPENNARS_BUILD_SHARED)
    add_library(opennars-shared SHARED ${OPENNARS_CORE_SOURCES})
    target_include_directories(opennars-shared PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:include/opennars>
    )
    target_link_libraries(opennars-shared PUBLIC m)
    set_target_properties(opennars-shared PROPERTIES
        OUTPUT_NAME opennars
        VERSION ${PROJECT_VERSION}
        SOVERSION 0
    )
endif()

# NARS Shell executable
if(OPENNARS_BUILD_SHELL)
    add_executable(nars-shell src/main.c)
    target_link_libraries(nars-shell PRIVATE 
        $<IF:$<BOOL:${OPENNARS_BUILD_STATIC}>,opennars-static,opennars-shared>
    )
    set_target_properties(nars-shell PROPERTIES
        OUTPUT_NAME nars
    )
endif()

# Tests
if(OPENNARS_BUILD_TESTS)
    enable_testing()
    
    # Find test files
    file(GLOB TEST_NAL_FILES "${CMAKE_CURRENT_SOURCE_DIR}/tests/*.nal")
    
    foreach(TEST_FILE ${TEST_NAL_FILES})
        get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
        add_test(
            NAME "nal_${TEST_NAME}"
            COMMAND nars-shell < ${TEST_FILE}
        )
    endforeach()
endif()

# Installation
include(GNUInstallDirs)

if(OPENNARS_BUILD_STATIC)
    install(TARGETS opennars-static
        EXPORT opennars-targets
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )
endif()

if(OPENNARS_BUILD_SHARED)
    install(TARGETS opennars-shared
        EXPORT opennars-targets
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )
endif()

if(OPENNARS_BUILD_SHELL)
    install(TARGETS nars-shell
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    )
endif()

# Install headers
install(FILES ${OPENNARS_HEADERS}
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/opennars
)

# Export targets
install(EXPORT opennars-targets
    FILE opennars-targets.cmake
    NAMESPACE opennars::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/opennars
)

# Package configuration
include(CMakePackageConfigHelpers)

configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/opennars-config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/opennars-config.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/opennars
)

write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/opennars-config-version.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/opennars-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/opennars-config-version.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/opennars
)

# pkg-config file
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/opennars.pc.in"
    "${CMAKE_CURRENT_BINARY_DIR}/opennars.pc"
    @ONLY
)

install(FILES "${CMAKE_CURRENT_BINARY_DIR}/opennars.pc"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
)

message(STATUS "OpenNARS configuration:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  Static library: ${OPENNARS_BUILD_STATIC}")
message(STATUS "  Shared library: ${OPENNARS_BUILD_SHARED}")
message(STATUS "  Shell executable: ${OPENNARS_BUILD_SHELL}")
message(STATUS "  Network NAR: ${OPENNARS_ENABLE_NETWORK}")
message(STATUS "  Tests: ${OPENNARS_BUILD_TESTS}")
