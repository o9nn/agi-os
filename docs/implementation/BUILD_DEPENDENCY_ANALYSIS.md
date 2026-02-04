# AGI-OS Build Dependency Analysis

## Date: December 12, 2025

## Overview

This document provides a comprehensive analysis of build dependencies and component integration points for the AGI-OS system, including the newly integrated CogBolt component.

## Component Hierarchy

### Layer 0: Build Tools
```
MIG (Mach Interface Generator)
├── Location: core/microkernel/cognumach/mig/
├── Symlink: core/os/hurdcog/mig -> ../../microkernel/cognumach/mig
├── Build System: Autotools (autoconf, automake, libtool)
├── Dependencies: None (build tool)
└── Required By: Cognumach, HurdCog
```

### Layer 1: Microkernel
```
Cognumach (GNU Mach + Cognitive Extensions)
├── Location: core/microkernel/cognumach/
├── Build System: Autotools
├── Dependencies: MIG
├── Required By: HurdCog, Cognitive-Grip
└── Components:
    ├── Kernel core (kern/)
    ├── IPC system (ipc/)
    ├── VM subsystem (vm/)
    ├── Device drivers (device/)
    └── Architecture support (i386/, x86_64/, aarch64/)
```

### Layer 2: Operating System
```
HurdCog (GNU Hurd + Cognitive Extensions)
├── Location: core/os/hurdcog/
├── Build System: Autotools
├── Dependencies: Cognumach, MIG
├── Required By: Cognitive-Grip
└── Components:
    ├── Translators (trans/)
    ├── Servers (pfinet/, pflocal/, proc/)
    ├── Libraries (libdiskfs/, libnetfs/, libports/)
    └── Utilities (utils/, sutils/)
```

### Layer 3: Cognitive Framework (OpenCog)

#### Foundation Layer
```
CogUtil
├── Location: core/cognition/foundation/cogutil/
├── Build System: CMake
├── Dependencies: None (base library)
└── Required By: All OpenCog components

AtomSpace
├── Location: core/cognition/foundation/atomspace/
├── Build System: CMake
├── Dependencies: CogUtil
└── Required By: All cognitive components

AtomSpace Storage
├── Location: core/cognition/storage/atomspace-storage/
├── Build System: CMake
├── Dependencies: CogUtil, AtomSpace
├── Required By: CogServer, all storage backends
└── Status: CRITICAL - Must be built before CogServer
```

#### Storage Backends
```
Storage Backends
├── atomspace-rocks (RocksDB backend)
├── atomspace-pgres (PostgreSQL backend)
├── atomspace-cog (CogServer backend)
└── atomspace-machspace (Mach/Hurd integration)
    ├── Dependencies: AtomSpace, AtomSpace-Storage, Cognumach
    └── Purpose: Bridge between AtomSpace and Mach/Hurd
```

#### Network Layer
```
CogServer
├── Location: core/cognition/network/cogserver/
├── Build System: CMake
├── Dependencies: CogUtil, AtomSpace, AtomSpace-Storage
└── Required By: Network-based cognitive services
```

#### Reasoning Layer
```
PLN (Probabilistic Logic Networks)
├── Location: core/cognition/reasoning/pln/
├── Dependencies: CogUtil, AtomSpace, URE
└── Required By: Advanced reasoning applications

URE (Unified Rule Engine)
├── Location: core/cognition/reasoning/ure/
├── Dependencies: CogUtil, AtomSpace
└── Required By: PLN, reasoning applications

Unify
├── Location: core/cognition/reasoning/unify/
├── Dependencies: CogUtil, AtomSpace
└── Required By: Pattern matching, reasoning

SpaceTime
├── Location: core/cognition/reasoning/spacetime/
├── Dependencies: CogUtil, AtomSpace
└── Required By: Temporal reasoning
```

#### Attention Layer
```
ECAN (Economic Attention Networks)
├── Location: core/cognition/attention/ecan/
├── Dependencies: CogUtil, AtomSpace
└── Required By: Attention allocation, resource management
```

#### Learning Layer
```
Learn
├── Location: core/cognition/learning/learn/
├── Dependencies: CogUtil, AtomSpace
└── Purpose: Symbolic learning algorithms

Miner
├── Location: core/cognition/learning/miner/
├── Dependencies: CogUtil, AtomSpace, URE
└── Purpose: Pattern mining

MOSES
├── Location: core/cognition/learning/moses/
├── Dependencies: CogUtil, AtomSpace
└── Purpose: Meta-optimizing semantic evolutionary search

ASMOSES
├── Location: core/cognition/foundation/asmoses/
├── Dependencies: CogUtil, AtomSpace, MOSES
└── Purpose: AtomSpace-based MOSES
```

#### Generation Layer
```
Generate
├── Location: core/cognition/generation/generate/
├── Dependencies: CogUtil, AtomSpace, Link-Grammar
└── Purpose: Natural language generation
```

#### Language Layer
```
Link-Grammar
├── Location: core/cognition/language/link-grammar/
├── Dependencies: None (standalone library)
└── Required By: lg-atomese, relex, generate

lg-atomese
├── Location: core/cognition/language/lg-atomese/
├── Dependencies: CogUtil, AtomSpace, Link-Grammar
└── Purpose: Link Grammar to AtomSpace bridge

Relex
├── Location: core/cognition/language/relex/
├── Dependencies: Link-Grammar, lg-atomese
└── Purpose: Relationship extraction
```

#### Perception Layer
```
Vision
├── Location: core/cognition/perception/vision/
├── Dependencies: CogUtil, AtomSpace
└── Purpose: Visual perception and processing
```

#### Specialized Layer
```
AGI-Bio
├── Location: core/cognition/specialized/agi-bio/
├── Dependencies: CogUtil, AtomSpace, PLN, URE
└── Purpose: Bioinformatics and computational biology
```

### Layer 4: Integration & Extensions

#### Cognitive-Grip (Integration Layer)
```
Cognitive-Grip
├── Location: core/integration/cognitive-grip/
├── Build System: CMake
├── Dependencies: CogUtil, AtomSpace, AtomSpace-Storage
├── Purpose: Unified abstraction across all layers
└── Components:
    ├── cognitive_grip.cpp (main integration)
    ├── machspace_bridge.cpp (Mach integration)
    ├── hurdcog_bridge.cpp (Hurd integration)
    ├── cognumach_bridge.cpp (Microkernel integration)
    └── unified_config.cpp (configuration management)
```

#### CogBolt (NEW - AI-Powered IDE Core)
```
CogBolt
├── Location: cogbolt/ (root level)
├── Build System: CMake (C++20)
├── Status: NEEDS INTEGRATION
├── External Dependencies:
│   ├── CURL (HTTP client) - Optional
│   ├── jsoncpp (JSON parsing) - Optional
│   ├── OpenGL (Graphics) - Optional
│   ├── GLFW (Window management) - Optional
│   ├── ImGui (GUI framework) - Optional
│   ├── ZLIB (Compression) - Optional
│   ├── GGML (AI inference) - Optional
│   └── llama.cpp (LLM support) - Optional
├── Components:
│   ├── Core Engine (AI model inference, GGML integration)
│   ├── Editor (Multi-cursor, syntax highlighting, code folding)
│   ├── Network (WebSocket, HTTP server)
│   ├── AI Integration (RWKV, tokenizer, chat system)
│   └── Utilities (Logging, error handling)
└── Integration Points:
    ├── AtomSpace: Code representation in hypergraph
    ├── CogServer: Network services for collaborative editing
    ├── PLN/URE: Code reasoning and optimization
    ├── Learn/Miner: Pattern mining from codebases
    └── Cognitive-Grip: Unified access to cognitive services
```

## Build Order (Revised with CogBolt)

### Phase 0: Build Tools
1. **MIG** (Mach Interface Generator)
   - Built as part of Cognumach
   - Required before Cognumach and HurdCog

### Phase 1: Microkernel & OS
2. **Cognumach** (Microkernel)
   - Autotools-based build
   - Includes MIG build
3. **HurdCog** (Operating System)
   - Autotools-based build
   - Requires Cognumach and MIG

### Phase 2: Foundation
4. **CogUtil** (Base library)
5. **AtomSpace** (Hypergraph database)
6. **AtomSpace-Storage** (Storage abstraction) ⭐ CRITICAL

### Phase 3: Storage Backends
7. **AtomSpace-Rocks** (RocksDB backend)
8. **AtomSpace-Pgres** (PostgreSQL backend)
9. **AtomSpace-Cog** (CogServer backend)
10. **AtomSpace-Machspace** (Mach/Hurd bridge)

### Phase 4: Network Services
11. **CogServer** (Network services)

### Phase 5: Reasoning
12. **URE** (Unified Rule Engine)
13. **Unify** (Pattern matching)
14. **SpaceTime** (Temporal reasoning)
15. **PLN** (Probabilistic Logic Networks)

### Phase 6: Attention
16. **ECAN** (Economic Attention Networks)

### Phase 7: Learning
17. **Learn** (Symbolic learning)
18. **Miner** (Pattern mining)
19. **MOSES** (Evolutionary search)
20. **ASMOSES** (AtomSpace MOSES)

### Phase 8: Language
21. **Link-Grammar** (Grammar library)
22. **lg-atomese** (Grammar bridge)
23. **Relex** (Relationship extraction)

### Phase 9: Generation
24. **Generate** (Natural language generation)

### Phase 10: Perception
25. **Vision** (Visual perception)

### Phase 11: Specialized
26. **AGI-Bio** (Bioinformatics)

### Phase 12: Integration
27. **Cognitive-Grip** (Unified integration layer)

### Phase 13: Extensions (NEW)
28. **CogBolt** (AI-Powered IDE Core)
   - Can be built independently or as part of AGI-OS
   - Optional dependencies allow flexible deployment

## MIG Dependency Resolution

### Current Status
- **Primary Location**: `core/microkernel/cognumach/mig/`
- **Symlink in HurdCog**: `core/os/hurdcog/mig` → `../../microkernel/cognumach/mig`
- **Status**: ✅ FIXED (symlink corrected)

### Build Strategy
1. MIG is built as part of Cognumach build process
2. HurdCog references MIG via symlink
3. Both components share the same MIG installation
4. No duplication, single source of truth

## CogBolt Integration Strategy

### Option 1: Standalone Build (Current)
```bash
cd cogbolt
mkdir build && cd build
cmake ..
make -j$(nproc)
```

### Option 2: Integrated Build (Recommended)
```cmake
# Add to root CMakeLists.txt
OPTION(BUILD_COGBOLT "Build CogBolt AI-Powered IDE Core" ON)

IF(BUILD_COGBOLT)
    MESSAGE(STATUS "Building CogBolt...")
    add_subdirectory(cogbolt)
ENDIF()
```

### Option 3: Cognitive Integration (Future)
```cmake
# Enhanced integration with OpenCog
IF(BUILD_COGBOLT AND BUILD_ATOMSPACE)
    # Enable AtomSpace integration
    target_compile_definitions(bolt_lib PRIVATE HAVE_ATOMSPACE=1)
    target_link_libraries(bolt_lib PUBLIC atomspace cogutil)
ENDIF()
```

## Cross-Layer Dependencies

### Cognumach → AtomSpace (via Machspace)
```
Cognumach Kernel State
    ↓
AtomSpace-Machspace Bridge
    ↓
AtomSpace Hypergraph
    ↓
Cognitive Services (PLN, ECAN, etc.)
```

### HurdCog → AtomSpace (via Translators)
```
HurdCog Translators
    ↓
HurdCog-AtomSpace Bridge
    ↓
AtomSpace Hypergraph
    ↓
Semantic Filesystem Queries
```

### CogBolt → AtomSpace (Proposed)
```
CogBolt Code Editor
    ↓
Code AST/Semantic Model
    ↓
AtomSpace Code Representation
    ↓
PLN/URE Code Reasoning
    ↓
AI-Assisted Code Completion
```

## Debian Package Dependencies

### Core Packages
```
cogutil (base)
├── No dependencies

atomspace
├── Depends: cogutil

atomspace-storage
├── Depends: cogutil, atomspace

cogserver
├── Depends: cogutil, atomspace, atomspace-storage
```

### Storage Backend Packages
```
atomspace-rocks
├── Depends: atomspace, atomspace-storage, librocksdb-dev

atomspace-pgres
├── Depends: atomspace, atomspace-storage, libpq-dev

atomspace-machspace
├── Depends: atomspace, atomspace-storage, cognumach-dev
```

### Reasoning Packages
```
ure
├── Depends: atomspace

pln
├── Depends: atomspace, ure

unify
├── Depends: atomspace

spacetime
├── Depends: atomspace
```

### Integration Packages
```
cognitive-grip
├── Depends: cogutil, atomspace, atomspace-storage
├── Recommends: cognumach, hurdcog

cogbolt (NEW - TO BE CREATED)
├── Depends: libstdc++6 (>= 10)
├── Recommends: libcurl4, libjsoncpp25, libglfw3, libimgui-dev
├── Suggests: cognitive-grip, atomspace, cogserver
```

## Build System Integration Points

### CMake Configuration
```cmake
# Root CMakeLists.txt options
OPTION(BUILD_COGNUMACH "Build Cognumach microkernel" OFF)
OPTION(BUILD_HURDCOG "Build HurdCog OS" OFF)
OPTION(BUILD_COGUTIL "Build CogUtil" ON)
OPTION(BUILD_ATOMSPACE "Build AtomSpace" ON)
OPTION(BUILD_ATOMSPACE_STORAGE "Build AtomSpace Storage" ON)
OPTION(BUILD_COGSERVER "Build CogServer" ON)
OPTION(BUILD_COGBOLT "Build CogBolt IDE" ON)  # NEW
OPTION(BUILD_COGNITIVE_GRIP "Build Cognitive-Grip" ON)
```

### Build Script Integration
```bash
# build-agi-os.sh additions
BUILD_COGBOLT=1  # Enable by default

if [ $BUILD_COGBOLT -eq 1 ]; then
    log_info "Building CogBolt AI-Powered IDE..."
    mkdir -p "$BUILD_DIR/cogbolt-build"
    cd "$BUILD_DIR/cogbolt-build"
    cmake "$ROOT_DIR/cogbolt" \
        -DCMAKE_BUILD_TYPE="$CMAKE_BUILD_TYPE" \
        -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX"
    make -j$PARALLEL_JOBS
    make install
    log_success "CogBolt built and installed"
fi
```

## Integration Recommendations

### High Priority
1. ✅ Fix MIG symlink (COMPLETED)
2. 🔴 Add CogBolt to root CMakeLists.txt
3. 🔴 Update build-agi-os.sh with CogBolt support
4. 🔴 Create debian package for CogBolt
5. 🔴 Document CogBolt integration in README

### Medium Priority
1. 🟡 Enhance Cognitive-Grip with CogBolt integration
2. 🟡 Create AtomSpace bridge for CogBolt code representation
3. 🟡 Integrate CogBolt with CogServer for collaborative editing
4. 🟡 Add PLN/URE reasoning for code analysis

### Low Priority
1. 🟢 Create CogBolt examples using OpenCog services
2. 🟢 Implement ECAN-based attention for code navigation
3. 🟢 Add pattern mining for codebase analysis
4. 🟢 Create unified documentation for all layers

## Conclusion

The AGI-OS build system is well-structured with clear dependency hierarchies. The addition of CogBolt requires:

1. **Build System Integration**: Add CogBolt to CMake and build scripts
2. **Debian Packaging**: Create cogbolt package with proper dependencies
3. **Cognitive Integration**: Bridge CogBolt with OpenCog services via Cognitive-Grip
4. **Documentation**: Update all relevant documentation

The MIG symlink issue has been resolved, ensuring proper build dependencies between Cognumach and HurdCog. The next phase will implement these integrations to create a unified, cognitively-enhanced development environment.
