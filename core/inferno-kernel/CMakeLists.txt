#
# Inferno Kernel - AGI-OS Integration
# Root CMakeLists.txt for Inferno kernel components
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)
PROJECT(inferno-kernel C)

# Set C standard
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Build options
OPTION(BUILD_DIS_VM "Build Dis Virtual Machine" ON)
OPTION(BUILD_9P_PROTOCOL "Build 9P Protocol Stack" ON)
OPTION(BUILD_LIMBO_RUNTIME "Build Limbo Language Runtime" ON)
OPTION(BUILD_NAMESPACE_CORE "Build Namespace Management Core" ON)
OPTION(BUILD_COGNITIVE_SCHEDULER "Build Cognitive Scheduler" ON)

# Global include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

MESSAGE(STATUS "=== Inferno Kernel Configuration ===")
MESSAGE(STATUS "  Dis VM: ${BUILD_DIS_VM}")
MESSAGE(STATUS "  9P Protocol: ${BUILD_9P_PROTOCOL}")
MESSAGE(STATUS "  Limbo Runtime: ${BUILD_LIMBO_RUNTIME}")
MESSAGE(STATUS "  Namespace Core: ${BUILD_NAMESPACE_CORE}")
MESSAGE(STATUS "  Cognitive Scheduler: ${BUILD_COGNITIVE_SCHEDULER}")

# Layer 0: Dis Virtual Machine
IF(BUILD_DIS_VM)
    MESSAGE(STATUS "Building Dis Virtual Machine...")
    add_subdirectory(dis-vm)
ENDIF()

# Layer 1: 9P Protocol Stack
IF(BUILD_9P_PROTOCOL)
    MESSAGE(STATUS "Building 9P Protocol Stack...")
    add_subdirectory(9p-protocol)
ENDIF()

# Layer 2: Limbo Language Runtime
IF(BUILD_LIMBO_RUNTIME)
    MESSAGE(STATUS "Building Limbo Language Runtime...")
    add_subdirectory(limbo-runtime)
ENDIF()

# Layer 3: Namespace Management Core
IF(BUILD_NAMESPACE_CORE)
    MESSAGE(STATUS "Building Namespace Management Core...")
    add_subdirectory(namespace-core)
ENDIF()

# Layer 4: Cognitive Scheduler
IF(BUILD_COGNITIVE_SCHEDULER)
    MESSAGE(STATUS "Building Cognitive Scheduler...")
    add_subdirectory(cognitive-scheduler)
ENDIF()

# Export configuration
install(EXPORT inferno-kernel-targets
    FILE InfernoKernelTargets.cmake
    NAMESPACE InfernoKernel::
    DESTINATION lib/cmake/InfernoKernel
)

# Generate config file
include(CMakePackageConfigHelpers)
configure_package_config_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/InfernoKernelConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/InfernoKernelConfig.cmake
    INSTALL_DESTINATION lib/cmake/InfernoKernel
)

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/InfernoKernelConfig.cmake
    DESTINATION lib/cmake/InfernoKernel
)

MESSAGE(STATUS "=== Inferno Kernel Configuration Complete ===")
