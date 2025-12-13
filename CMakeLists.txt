#
# AGI-OS - Autonomous General Intelligence Operating System
# Root CMakeLists.txt
#
# This CMakeLists.txt coordinates building the complete AGI-OS stack
# with proper dependency ordering and integration of all layers:
# Layer 0: Build Tools (MIG)
# Layer 1: Cognumach (Microkernel)
# Layer 2: HurdCog (Cognitive Operating System)
# Layer 3: OpenCog Collection (AGI Framework)
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(agi-os)

# Set default build type
IF (CMAKE_BUILD_TYPE STREQUAL "")
	SET(CMAKE_BUILD_TYPE Release)
ENDIF (CMAKE_BUILD_TYPE STREQUAL "")

MESSAGE(STATUS "AGI-OS build type: ${CMAKE_BUILD_TYPE}")

# Global compilation flags
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# ============================================================================
# AGI-OS Build Configuration Options
# ============================================================================

# Layer 0: Inferno Kernel
OPTION(BUILD_INFERNO_KERNEL "Build Inferno Kernel components" ON)
OPTION(BUILD_VORTEX_MORPHULE_EGREGORE "Build Vortex-Morphule-Egregore architecture" ON)

# Layer 0: Build Tools
OPTION(BUILD_MIG "Build MIG (Mach Interface Generator) - Build Tool" OFF)

# Layer 1: Microkernel
OPTION(BUILD_COGNUMACH "Build Cognumach (GNU Mach) microkernel - Layer 1" OFF)

# Layer 2: Cognitive Operating System
OPTION(BUILD_HURDCOG "Build HurdCog (GNU Hurd + cognitive extensions) - Layer 2" OFF)

# Layer 3: Core OpenCog Components
OPTION(BUILD_COGUTIL "Build CogUtil library" ON)
OPTION(BUILD_ATOMSPACE "Build AtomSpace hypergraph database" ON)
OPTION(BUILD_ATOMSPACE_STORAGE "Build AtomSpace storage backends - REQUIRED for CogServer" ON)
OPTION(BUILD_COGSERVER "Build CogServer networking" ON)
OPTION(BUILD_LEARN "Build symbolic learning" ON)
OPTION(BUILD_AGENTS "Build interactive agents" ON)
OPTION(BUILD_ATTENTION "Build attention mechanisms" ON)
OPTION(BUILD_ASMOSES "Build ASMOSES program synthesis" ON)

# Layer 3: Cognitive Architecture Components
OPTION(BUILD_ATOMSPACE_EXTENSIONS "Build AtomSpace language extensions" ON)
OPTION(BUILD_ATOMSPACE_ACCELERATOR "Build AtomSpace accelerator inference engine" ON)
OPTION(BUILD_AGENTIC_CHATBOTS "Build agentic chatbots integration" ON)

# Layer 3.6: 9P-Enabled Cognitive Components
OPTION(BUILD_ATOMSPACE_9P "Build AtomSpace 9P file servers" ON)
OPTION(BUILD_PLN_9P "Build PLN reasoning via 9P" ON)
OPTION(BUILD_ECAN_9P "Build ECAN attention via 9P" ON)
OPTION(BUILD_DISTRIBUTED_COGNITION "Build distributed cognition" ON)

# Layer 4: AI-Powered Development Tools
OPTION(BUILD_COGBOLT "Build CogBolt AI-Powered IDE Core" ON)

# ============================================================================
# Layer 0: Inferno Kernel
# ============================================================================

IF(BUILD_INFERNO_KERNEL)
    MESSAGE(STATUS "Building Layer 0: Inferno Kernel...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/inferno-kernel/CMakeLists.txt")
        add_subdirectory(core/inferno-kernel)
        MESSAGE(STATUS "  Inferno Kernel configured")
        
        # 9P Batch Protocol Extension
        IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/inferno-kernel/9p-protocol/batch/CMakeLists.txt")
            add_subdirectory(core/inferno-kernel/9p-protocol/batch)
            MESSAGE(STATUS "  9P Batch Protocol configured")
        ENDIF()
    ELSE()
        MESSAGE(WARNING "  Inferno Kernel not found at core/inferno-kernel/")
    ENDIF()
ENDIF()

# Vortex-Morphule-Egregore Architecture
IF(BUILD_VORTEX_MORPHULE_EGREGORE)
    MESSAGE(STATUS "Building Vortex-Morphule-Egregore Architecture...")
    
    # Vortex (Matula numbers + vorticity)
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/inferno-kernel/vortex/CMakeLists.txt")
        add_subdirectory(core/inferno-kernel/vortex)
        MESSAGE(STATUS "  Vortex (Matula + A000081) configured")
    ELSE()
        MESSAGE(WARNING "  Vortex not found at core/inferno-kernel/vortex/")
    ENDIF()
    
    # Morphule (agentic functions)
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/inferno-kernel/morphule/CMakeLists.txt")
        add_subdirectory(core/inferno-kernel/morphule)
        MESSAGE(STATUS "  Morphule (Transform Quirk) configured")
    ELSE()
        MESSAGE(WARNING "  Morphule not found at core/inferno-kernel/morphule/")
    ENDIF()
    
    # Egregore (daemon constellations)
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/inferno-kernel/egregore/CMakeLists.txt")
        add_subdirectory(core/inferno-kernel/egregore)
        MESSAGE(STATUS "  Egregore (stigmergic coordination) configured")
    ELSE()
        MESSAGE(WARNING "  Egregore not found at core/inferno-kernel/egregore/")
    ENDIF()
ENDIF()

# ============================================================================
# Layer 0: Build Tools - MIG (Mach Interface Generator)
# ============================================================================

IF(BUILD_COGNUMACH OR BUILD_HURDCOG)
    MESSAGE(STATUS "Building Layer 0: MIG (Mach Interface Generator)...")
    
    # MIG is part of Cognumach, will be built as part of Cognumach build
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/microkernel/cognumach/mig/")
        MESSAGE(STATUS "  MIG found in Cognumach (autotools-based)")
    ENDIF()
ENDIF()

# ============================================================================
# Layer 1: Cognumach Microkernel (autotools-based)
# ============================================================================

IF(BUILD_COGNUMACH)
    MESSAGE(STATUS "Building Layer 1: Cognumach Microkernel...")
    MESSAGE(STATUS "  Note: Requires autotools (autoconf, automake, libtool)")
    
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/microkernel/cognumach/")
        # Cognumach uses autotools, handled via external project
        MESSAGE(STATUS "  Cognumach found at core/microkernel/cognumach/")
    ELSE()
        MESSAGE(WARNING "  Cognumach not found at core/microkernel/cognumach/")
    ENDIF()
ENDIF()

# ============================================================================
# Layer 2: HurdCog Operating System (autotools-based + cognitive extensions)
# ============================================================================

IF(BUILD_HURDCOG)
    MESSAGE(STATUS "Building Layer 2: HurdCog Operating System...")
    MESSAGE(STATUS "  Note: Requires Cognumach and autotools")
    
    IF(BUILD_COGNUMACH)
        IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/os/hurdcog/")
            MESSAGE(STATUS "  HurdCog found at core/os/hurdcog/")
        ELSE()
            MESSAGE(WARNING "  HurdCog not found at core/os/hurdcog/")
        ENDIF()
    ELSE()
        MESSAGE(WARNING "HurdCog requires Cognumach. Skipping HurdCog build.")
    ENDIF()
ENDIF()

# ============================================================================
# Layer 3: OpenCog Collection (CMake-based)
# ============================================================================

MESSAGE(STATUS "Building Layer 3: OpenCog Collection (OCC)...")

# CRITICAL: Build order must respect dependencies
# 1. CogUtil (foundation - no dependencies)
# 2. AtomSpace (depends on CogUtil)
# 3. AtomSpace Storage (depends on AtomSpace) - MUST be before CogServer
# 4. CogServer (depends on AtomSpace AND AtomSpace Storage)
# 5. All other components (depend on AtomSpace)

# ============================================================================
# Layer 3.1: Foundation Components
# ============================================================================

IF(BUILD_COGUTIL)
    MESSAGE(STATUS "Building CogUtil (foundation library)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/foundation/cogutil/")
        add_subdirectory(core/cognition/foundation/cogutil)
    ELSE()
        MESSAGE(WARNING "CogUtil not found at core/cognition/foundation/cogutil/")
    ENDIF()
ENDIF()

# ============================================================================
# Layer 3.2: AtomSpace and Storage (CRITICAL DEPENDENCY CHAIN)
# ============================================================================

IF(BUILD_ATOMSPACE AND BUILD_COGUTIL)
    MESSAGE(STATUS "Building AtomSpace (hypergraph database)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/foundation/atomspace/")
        add_subdirectory(core/cognition/foundation/atomspace)
    ELSE()
        MESSAGE(WARNING "AtomSpace not found at core/cognition/foundation/atomspace/")
    ENDIF()
ENDIF()

# CRITICAL: AtomSpace Storage MUST be built before CogServer
IF(BUILD_ATOMSPACE_STORAGE AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building AtomSpace Storage (REQUIRED for CogServer)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/foundation/atomspace-storage/")
        add_subdirectory(core/cognition/foundation/atomspace-storage)
    ELSE()
        MESSAGE(WARNING "AtomSpace Storage not found at core/cognition/foundation/atomspace-storage/")
    ENDIF()
ENDIF()

# ============================================================================
# Layer 3.3: CogServer (depends on AtomSpace AND AtomSpace Storage)
# ============================================================================

IF(BUILD_COGSERVER AND BUILD_ATOMSPACE AND BUILD_ATOMSPACE_STORAGE)
    MESSAGE(STATUS "Building CogServer (networking and server)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/foundation/cogserver/")
        add_subdirectory(core/cognition/foundation/cogserver)
    ELSE()
        MESSAGE(WARNING "CogServer not found at core/cognition/foundation/cogserver/")
    ENDIF()
ENDIF()

# ============================================================================
# Layer 3.4: Cognitive Components (depend on AtomSpace)
# ============================================================================

IF(BUILD_LEARN AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Learn (symbolic learning)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/foundation/learn/")
        add_subdirectory(core/cognition/foundation/learn)
    ELSE()
        MESSAGE(WARNING "Learn not found at core/cognition/foundation/learn/")
    ENDIF()
ENDIF()

IF(BUILD_AGENTS AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Agents (interactive agents)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/foundation/agents/")
        add_subdirectory(core/cognition/foundation/agents)
    ELSE()
        MESSAGE(WARNING "Agents not found at core/cognition/foundation/agents/")
    ENDIF()
ENDIF()

IF(BUILD_ATTENTION AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Attention (attention mechanisms)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/foundation/attention/")
        add_subdirectory(core/cognition/foundation/attention)
    ELSE()
        MESSAGE(WARNING "Attention not found at core/cognition/foundation/attention/")
    ENDIF()
ENDIF()

IF(BUILD_ASMOSES AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building ASMOSES (program synthesis)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/foundation/asmoses/")
        add_subdirectory(core/cognition/foundation/asmoses)
    ELSE()
        MESSAGE(WARNING "ASMOSES not found at core/cognition/foundation/asmoses/")
    ENDIF()
ENDIF()

# ============================================================================
# Layer 3.5: AtomSpace Extensions and Accelerators
# ============================================================================

IF(BUILD_ATOMSPACE_EXTENSIONS AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building AtomSpace Extensions...")
    
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/storage/atomspace-metta/")
        MESSAGE(STATUS "  Building AtomSpace MeTTa extension...")
        add_subdirectory(core/cognition/storage/atomspace-metta)
    ENDIF()
    
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/storage/atomspace-bridge/")
        MESSAGE(STATUS "  Building AtomSpace Bridge...")
        add_subdirectory(core/cognition/storage/atomspace-bridge)
    ENDIF()
ENDIF()

IF(BUILD_ATOMSPACE_ACCELERATOR AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building AtomSpace Accelerator (inference engine)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/storage/atomspace-accelerator/")
        add_subdirectory(core/cognition/storage/atomspace-accelerator)
    ELSE()
        MESSAGE(WARNING "AtomSpace Accelerator not found at core/cognition/storage/atomspace-accelerator/")
    ENDIF()
ENDIF()

IF(BUILD_AGENTIC_CHATBOTS AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Agentic Chatbots...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/generation/agentic-chatbots/")
        add_subdirectory(core/cognition/generation/agentic-chatbots)
    ELSE()
        MESSAGE(WARNING "Agentic Chatbots not found at core/cognition/generation/agentic-chatbots/")
    ENDIF()
ENDIF()

# ============================================================================
# Layer 3.6: 9P-Enabled Cognitive Components
# ============================================================================

IF(BUILD_ATOMSPACE_9P AND BUILD_INFERNO_KERNEL AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building AtomSpace-9P (AtomSpace as 9P file servers)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/foundation/atomspace-9p/CMakeLists.txt")
        add_subdirectory(core/cognition/foundation/atomspace-9p)
        MESSAGE(STATUS "  AtomSpace-9P configured")
    ELSE()
        MESSAGE(WARNING "  AtomSpace-9P not found at core/cognition/foundation/atomspace-9p/")
    ENDIF()
ENDIF()

IF(BUILD_PLN_9P AND BUILD_ATOMSPACE_9P)
    MESSAGE(STATUS "Building PLN-9P (PLN reasoning via 9P)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/reasoning/pln-9p/CMakeLists.txt")
        add_subdirectory(core/cognition/reasoning/pln-9p)
        MESSAGE(STATUS "  PLN-9P configured")
    ELSE()
        MESSAGE(WARNING "  PLN-9P not found at core/cognition/reasoning/pln-9p/")
    ENDIF()
ENDIF()

IF(BUILD_ECAN_9P AND BUILD_ATOMSPACE_9P)
    MESSAGE(STATUS "Building ECAN-9P (ECAN attention via 9P)...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/attention/ecan-9p/CMakeLists.txt")
        add_subdirectory(core/cognition/attention/ecan-9p)
        MESSAGE(STATUS "  ECAN-9P configured")
    ELSE()
        MESSAGE(WARNING "  ECAN-9P not found at core/cognition/attention/ecan-9p/")
    ENDIF()
ENDIF()

IF(BUILD_DISTRIBUTED_COGNITION AND BUILD_ATOMSPACE_9P)
    MESSAGE(STATUS "Building Distributed Cognition...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/cognition/distributed/CMakeLists.txt")
        add_subdirectory(core/cognition/distributed)
        MESSAGE(STATUS "  Distributed Cognition configured")
    ELSE()
        MESSAGE(WARNING "  Distributed Cognition not found at core/cognition/distributed/")
    ENDIF()
ENDIF()

# ============================================================================
# Layer 4: CogBolt AI-Powered IDE Core
# ============================================================================

IF(BUILD_COGBOLT)
    MESSAGE(STATUS "Building Layer 4: CogBolt AI-Powered IDE Core...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cogbolt/CMakeLists.txt")
        add_subdirectory(cogbolt)
        MESSAGE(STATUS "  CogBolt found and configured")
    ELSE()
        MESSAGE(WARNING "  CogBolt not found at cogbolt/")
    ENDIF()
ENDIF()

# ============================================================================
# Integration Layer
# ============================================================================

OPTION(BUILD_INTEGRATION_LAYER "Build integration bridges" ON)
IF(BUILD_INTEGRATION_LAYER)
    MESSAGE(STATUS "Building integration layer...")
    IF(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/core/integration/CMakeLists.txt")
        add_subdirectory(core/integration)
    ENDIF()
ENDIF()

# ============================================================================
# Build Summary
# ============================================================================

MESSAGE(STATUS "")
MESSAGE(STATUS "AGI-OS Configuration Summary:")
MESSAGE(STATUS "========================================")
MESSAGE(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
MESSAGE(STATUS "")
MESSAGE(STATUS "AGI-OS Stack:")
MESSAGE(STATUS "Layer 0 - Inferno Kernel: ${BUILD_INFERNO_KERNEL}")
MESSAGE(STATUS "Layer 0 - MIG (Build Tool): ${BUILD_MIG}")
MESSAGE(STATUS "Layer 1 - Cognumach (Microkernel): ${BUILD_COGNUMACH}")
MESSAGE(STATUS "Layer 2 - HurdCog (OS + Cognitive): ${BUILD_HURDCOG}")
MESSAGE(STATUS "")
MESSAGE(STATUS "Layer 3 - OpenCog Collection:")
MESSAGE(STATUS "  Foundation:")
MESSAGE(STATUS "    CogUtil: ${BUILD_COGUTIL}")
MESSAGE(STATUS "    AtomSpace: ${BUILD_ATOMSPACE}")
MESSAGE(STATUS "    AtomSpace Storage (CRITICAL): ${BUILD_ATOMSPACE_STORAGE}")
MESSAGE(STATUS "    CogServer: ${BUILD_COGSERVER}")
MESSAGE(STATUS "")
MESSAGE(STATUS "  Cognitive Components:")
MESSAGE(STATUS "    Learn: ${BUILD_LEARN}")
MESSAGE(STATUS "    Agents: ${BUILD_AGENTS}")
MESSAGE(STATUS "    Attention: ${BUILD_ATTENTION}")
MESSAGE(STATUS "    ASMOSES: ${BUILD_ASMOSES}")
MESSAGE(STATUS "")
MESSAGE(STATUS "  Extensions:")
MESSAGE(STATUS "    AtomSpace Extensions: ${BUILD_ATOMSPACE_EXTENSIONS}")
MESSAGE(STATUS "    AtomSpace Accelerator: ${BUILD_ATOMSPACE_ACCELERATOR}")
MESSAGE(STATUS "    Agentic Chatbots: ${BUILD_AGENTIC_CHATBOTS}")
MESSAGE(STATUS "")
MESSAGE(STATUS "Integration Layer: ${BUILD_INTEGRATION_LAYER}")
MESSAGE(STATUS "")
MESSAGE(STATUS "Layer 4 - AI Development Tools:")
MESSAGE(STATUS "  CogBolt IDE Core: ${BUILD_COGBOLT}")
MESSAGE(STATUS "")
MESSAGE(STATUS "CRITICAL DEPENDENCY NOTES:")
MESSAGE(STATUS "  - CogServer REQUIRES AtomSpace Storage")
MESSAGE(STATUS "  - AtomSpace Storage MUST be built before CogServer")
MESSAGE(STATUS "  - All cognitive components require AtomSpace")
MESSAGE(STATUS "  - HurdCog requires Cognumach")
MESSAGE(STATUS "========================================")
MESSAGE(STATUS "")
