#
# OpenCog Collection (OCC) - Root CMakeLists.txt
# Monorepo integration for cognitive synergy
#
# This CMakeLists.txt coordinates building the core OpenCog components
# in the optimal order for integration.
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(opencog-collection)

# Set default build type
IF (CMAKE_BUILD_TYPE STREQUAL "")
	SET(CMAKE_BUILD_TYPE Release)
ENDIF (CMAKE_BUILD_TYPE STREQUAL "")

MESSAGE(STATUS "OpenCog Collection build type: ${CMAKE_BUILD_TYPE}")

# Global compilation flags
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Options for selective building

# AGI-OS Layer 1: Cognitive Microkernel
OPTION(BUILD_COGNUMACH "Build Cognumach (GNU Mach) microkernel - Layer 1" OFF)

# AGI-OS Layer 2: Cognitive Operating System
OPTION(BUILD_HURDCOG "Build HurdCog (GNU Hurd + cognitive extensions) - Layer 2" OFF)

# Core OpenCog components
OPTION(BUILD_COGUTIL "Build CogUtil library" ON)
OPTION(BUILD_ATOMSPACE "Build AtomSpace hypergraph database" ON)  
OPTION(BUILD_COGSERVER "Build CogServer networking" ON)
OPTION(BUILD_MATRIX "Build Matrix sparse vector support" ON)
OPTION(BUILD_LEARN "Build symbolic learning" ON)
OPTION(BUILD_AGENTS "Build interactive agents" ON)
OPTION(BUILD_SENSORY "Build sensory dataflow system" ON)

# New cognitive architecture components
OPTION(BUILD_COGGML "Build CogGML self-aware microkernel" ON)
OPTION(BUILD_COGSELF "Build CogSelf AGI synergy framework" ON)
OPTION(BUILD_ATOMSPACE_ACCELERATOR "Build AtomSpace accelerator inference engine" ON)
OPTION(BUILD_AGENTIC_CHATBOTS "Build agentic chatbots integration" ON)

# External integration packages
OPTION(BUILD_GNUCASH "Build Gnucash cognitive accounting integration" OFF)
OPTION(BUILD_KOBOLDCPP "Build KoboldCpp story/world modeling integration" OFF)
OPTION(BUILD_APHRODITE "Build Aphrodite Engine LLM inference integration" OFF)

# ============================================================================
# AGI-OS Build Sequence - Three Layer Architecture
# ============================================================================
# Layer 1: Cognumach (Microkernel)
# Layer 2: HurdCog (Operating System + Cognitive Extensions) 
# Layer 3: OCC (AGI Framework)
# ============================================================================

# Layer 1: Cognumach Microkernel (autotools-based)
IF(BUILD_COGNUMACH)
    MESSAGE(STATUS "Building Layer 1: Cognumach Microkernel...")
    MESSAGE(STATUS "  Note: Requires autotools (autoconf, automake, libtool)")
    add_subdirectory(cognumach)
ENDIF()

# Layer 2: HurdCog Operating System (autotools-based + cognitive extensions)
IF(BUILD_HURDCOG)
    MESSAGE(STATUS "Building Layer 2: HurdCog Operating System...")
    MESSAGE(STATUS "  Note: Requires Cognumach and autotools")
    IF(BUILD_COGNUMACH)
        add_subdirectory(hurdcog)
    ELSE()
        MESSAGE(WARNING "HurdCog requires Cognumach. Skipping HurdCog build.")
    ENDIF()
ENDIF()

# Layer 3: OCC Components (CMake-based)
MESSAGE(STATUS "Building Layer 3: OpenCog Collection (OCC)...")

# Core components built in dependency order
IF(BUILD_COGUTIL)
    MESSAGE(STATUS "Building CogUtil...")
    add_subdirectory(cogutil)
ENDIF()

# CogGML Microkernel - foundational for cognitive architecture
IF(BUILD_COGGML)
    MESSAGE(STATUS "Building CogGML Microkernel...")
    add_subdirectory(coggml)
ENDIF()

IF(BUILD_ATOMSPACE AND BUILD_COGUTIL)
    MESSAGE(STATUS "Building AtomSpace...")
    add_subdirectory(atomspace)
ENDIF()

# AtomSpace Accelerator - can build independently for cognitive architecture
IF(BUILD_ATOMSPACE_ACCELERATOR)
    MESSAGE(STATUS "Building AtomSpace Accelerator Inference Engine...")
    add_subdirectory(atomspace-accelerator)
ENDIF()

IF(BUILD_COGSERVER AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building CogServer...")
    add_subdirectory(cogserver)
ENDIF()

IF(BUILD_MATRIX AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Matrix...")
    add_subdirectory(matrix)
ENDIF()

IF(BUILD_LEARN AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Learn...")
    add_subdirectory(learn)
ENDIF()

IF(BUILD_AGENTS AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Agents...")
    add_subdirectory(agents)
ENDIF()

IF(BUILD_SENSORY AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Sensory...")
    add_subdirectory(sensory)
ENDIF()

# Agentic Chatbots - can build independently for cognitive architecture
IF(BUILD_AGENTIC_CHATBOTS)
    MESSAGE(STATUS "Building Agentic Chatbots...")
    add_subdirectory(agentic-chatbots)
ENDIF()

# CogSelf AGI Framework - coordinates all cognitive components
IF(BUILD_COGSELF AND BUILD_COGGML)
    MESSAGE(STATUS "Building CogSelf AGI Cognitive Synergy Framework...")
    add_subdirectory(cogself)
ENDIF()

# External integration packages
IF(BUILD_GNUCASH AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Gnucash cognitive accounting integration...")
    add_subdirectory(gnucash)
ENDIF()

IF(BUILD_KOBOLDCPP AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building KoboldCpp story/world modeling integration...")
    add_subdirectory(koboldcpp)
ENDIF()

IF(BUILD_APHRODITE AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Aphrodite Engine LLM inference integration...")
    add_subdirectory(aphrodite-engine)
ENDIF()

# Optional: Additional AtomSpace extensions
OPTION(BUILD_ATOMSPACE_STORAGE "Build AtomSpace storage backends" OFF)
IF(BUILD_ATOMSPACE_STORAGE AND BUILD_ATOMSPACE)
    add_subdirectory(atomspace-storage)
    add_subdirectory(atomspace-rocks)
    add_subdirectory(atomspace-cog)
ENDIF()

OPTION(BUILD_ATOMSPACE_EXTENSIONS "Build AtomSpace language extensions" OFF)
IF(BUILD_ATOMSPACE_EXTENSIONS AND BUILD_ATOMSPACE)
    add_subdirectory(atomspace-metta)
    add_subdirectory(atomspace-bridge)
ENDIF()

# Print summary
MESSAGE(STATUS "")
MESSAGE(STATUS "OpenCog Collection Configuration Summary:")
MESSAGE(STATUS "========================================")
MESSAGE(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
MESSAGE(STATUS "")
MESSAGE(STATUS "AGI-OS Stack:")
MESSAGE(STATUS "Layer 1 - Cognumach (Microkernel): ${BUILD_COGNUMACH}")
MESSAGE(STATUS "Layer 2 - HurdCog (OS + Cognitive): ${BUILD_HURDCOG}")
MESSAGE(STATUS "Layer 3 - OCC (AGI Framework): enabled")
MESSAGE(STATUS "")
MESSAGE(STATUS "Core Components:")
MESSAGE(STATUS "CogUtil: ${BUILD_COGUTIL}")
MESSAGE(STATUS "AtomSpace: ${BUILD_ATOMSPACE}")
MESSAGE(STATUS "CogServer: ${BUILD_COGSERVER}")
MESSAGE(STATUS "Matrix: ${BUILD_MATRIX}")
MESSAGE(STATUS "Learn: ${BUILD_LEARN}")
MESSAGE(STATUS "Agents: ${BUILD_AGENTS}")
MESSAGE(STATUS "Sensory: ${BUILD_SENSORY}")
MESSAGE(STATUS "")
MESSAGE(STATUS "Cognitive Architecture Components:")
MESSAGE(STATUS "CogGML Microkernel (self-aware shards): ${BUILD_COGGML}")
MESSAGE(STATUS "CogSelf AGI Framework: ${BUILD_COGSELF}")
MESSAGE(STATUS "AtomSpace Accelerator (inference engine): ${BUILD_ATOMSPACE_ACCELERATOR}")
MESSAGE(STATUS "Agentic Chatbots: ${BUILD_AGENTIC_CHATBOTS}")
MESSAGE(STATUS "")
MESSAGE(STATUS "Extended Components:")
MESSAGE(STATUS "AtomSpace Storage: ${BUILD_ATOMSPACE_STORAGE}")
MESSAGE(STATUS "AtomSpace Extensions: ${BUILD_ATOMSPACE_EXTENSIONS}")
MESSAGE(STATUS "")
MESSAGE(STATUS "External Integration Packages:")
MESSAGE(STATUS "Gnucash (cognitive accounting): ${BUILD_GNUCASH}")
MESSAGE(STATUS "KoboldCpp (story/world modeling): ${BUILD_KOBOLDCPP}")
MESSAGE(STATUS "Aphrodite Engine (LLM inference): ${BUILD_APHRODITE}")
MESSAGE(STATUS "")