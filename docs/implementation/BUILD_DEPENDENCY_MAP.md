# AGI-OS Build Dependency Map

## Date: 2025-12-14

## Layer Architecture

### Layer 0: Inferno Kernel (Foundation)
**Location**: `core/inferno-kernel/`
**Build System**: CMake + Limbo
**Dependencies**: None (kernel layer)
**Components**:
- Inferno OS core
- 9P protocol implementation
- Vortex (Matula numbers + vorticity)
- Morphule (agentic functions)
- Egregore (daemon constellations)
- Styx protocol
- Dis virtual machine

### Layer 0.5: Build Tools
**Location**: `core/microkernel/cognumach/mig/`
**Build System**: Autotools
**Dependencies**: None
**Components**:
- MIG (Mach Interface Generator)
- MIDL parser
- C code generator
- RPC stub generator

**ISSUE**: MIG duplicated in:
- `core/microkernel/cognumach/mig/` (FULL - 40+ files)
- `core/os/hurdcog/external/hurd-repos/mig/` (STUB - 1 README)

**RESOLUTION**: Keep cognumach/mig as primary, create symlink from hurdcog

### Layer 1: CoGNUMach Microkernel
**Location**: `core/microkernel/cognumach/`
**Build System**: Autotools
**Dependencies**: 
- MIG (Layer 0.5)
**Components**:
- GNU Mach kernel
- Cognitive scheduling extensions
- IPC mechanisms
- Memory management
- Thread management

### Layer 2: CoGNUHurd Operating System
**Location**: `core/os/hurdcog/`
**Build System**: Autotools
**Dependencies**:
- CoGNUMach (Layer 1)
- MIG (Layer 0.5)
**Components**:
- GNU Hurd translators
- Cognitive translators
- Filesystem servers
- Network servers
- Device drivers
- AtomSpace-Hurd bridge

### Layer 2.5: CogPlan9 Operating System
**Location**: `core/os/cogplan9/`
**Build System**: Plan9 mk
**Dependencies**:
- Inferno kernel (Layer 0)
**Components**:
- Plan9 cognitive extensions
- 9P cognitive file servers
- libatomspace (Plan9 native)
- libpln (Plan9 native)
- Cognitive syscalls
- MachSpace (distributed hypergraph)
- Cognitive Fusion Reactor
- **cogcities-kernel**: Cognitive cities distributed architecture
  - Location: `core/os/cogplan9/cogcities-kernel/`
  - Namespace-based cognitive domains
  - 9P neural transport channels
  - Cognitive swarms and process groups
  - Urban cognitive ecology modeling

### Layer 3: OpenCog Collection (OCC)

#### Layer 3.1: Foundation
**Build Order**: 1
**Dependencies**: None

1. **cogutil**
   - Location: `core/cognition/foundation/cogutil/`
   - Dependencies: None
   - Provides: Utility functions, logging, configuration

#### Layer 3.2: Hypergraph Database
**Build Order**: 2
**Dependencies**: cogutil

2. **atomspace**
   - Location: `core/cognition/foundation/atomspace/`
   - Dependencies: cogutil
   - Provides: Hypergraph database, atom types, pattern matching

#### Layer 3.3: Storage Layer (CRITICAL)
**Build Order**: 3
**Dependencies**: atomspace

3. **atomspace-storage**
   - Location: `core/cognition/foundation/atomspace-storage/`
   - Dependencies: atomspace
   - Provides: Persistence API, file storage, CSV storage
   - **CRITICAL**: Must be built BEFORE cogserver

#### Layer 3.4: Storage Backends
**Build Order**: 4
**Dependencies**: atomspace-storage

4. **atomspace-rocks**
   - Location: `core/cognition/storage/backends/rocks/`
   - Dependencies: atomspace-storage, RocksDB

5. **atomspace-pgres**
   - Location: `core/cognition/storage/backends/postgres/`
   - Dependencies: atomspace-storage, PostgreSQL

6. **atomspace-cog**
   - Location: `core/cognition/storage/atomspace-cog/`
   - Dependencies: atomspace-storage, cogserver

#### Layer 3.5: Network Services
**Build Order**: 5
**Dependencies**: atomspace, atomspace-storage

7. **cogserver**
   - Location: `core/cognition/foundation/cogserver/`
   - Dependencies: atomspace, atomspace-storage
   - Provides: Network shell, module loading, REPL

#### Layer 3.6: Reasoning
**Build Order**: 6
**Dependencies**: atomspace

8. **unify**
   - Location: `core/cognition/reasoning/unify/`
   - Dependencies: atomspace

9. **ure** (Unified Rule Engine)
   - Location: `core/cognition/reasoning/ure/`
   - Dependencies: atomspace, unify

10. **pln** (Probabilistic Logic Networks)
    - Location: `core/cognition/reasoning/pln/`
    - Dependencies: atomspace, ure

11. **spacetime**
    - Location: `core/cognition/reasoning/spacetime/`
    - Dependencies: atomspace

#### Layer 3.7: Attention
**Build Order**: 7
**Dependencies**: atomspace

12. **attention** (ECAN)
    - Location: `core/cognition/attention/ecan/`
    - Dependencies: atomspace, cogserver

#### Layer 3.8: Learning
**Build Order**: 8
**Dependencies**: atomspace

13. **learn**
    - Location: `core/cognition/learning/learn/`
    - Dependencies: atomspace, cogserver

14. **miner**
    - Location: `core/cognition/learning/miner/`
    - Dependencies: atomspace, ure

15. **asmoses**
    - Location: `core/cognition/foundation/asmoses/`
    - Dependencies: atomspace, cogutil

#### Layer 3.9: Language
**Build Order**: 9
**Dependencies**: atomspace

16. **link-grammar**
    - Location: `core/cognition/language/link-grammar/`
    - Dependencies: None (external)

17. **lg-atomese**
    - Location: `core/cognition/language/lg-atomese/`
    - Dependencies: atomspace, link-grammar

#### Layer 3.10: Agents
**Build Order**: 10
**Dependencies**: atomspace

18. **agents**
    - Location: `core/cognition/foundation/agents/`
    - Dependencies: atomspace

#### Layer 3.11: Distributed Cognition
**Build Order**: 11
**Dependencies**: atomspace, atomspace-storage

19. **das** (Distributed AtomSpace)
    - Location: `core/cognition/distributed/das/`
    - Dependencies: atomspace, atomspace-storage, Redis, MongoDB
    - Provides: Distributed hypergraph, query engine, attention broker

#### Layer 3.12: LLM Integration
**Build Order**: 12
**Dependencies**: atomspace (optional)

20. **node-llama-cog**
    - Location: `core/cognition/llm/node-llama-cog/`
    - Dependencies: Node.js 18+, llama.cpp
    - Provides: Local LLM execution, TypeScript bindings, model loading

21. **aphroditecho**
    - Location: `core/cognition/llm/aphroditecho/`
    - Dependencies: Python 3.9+, CUDA 12+, torch, vLLM
    - Provides: High-performance inference, PagedAttention, continuous batching

#### Layer 3.13: Avatar and Deep Tree Echo
**Build Order**: 13
**Dependencies**: atomspace

22. **d81p9p9**
    - Location: `core/avatar/deep-tree-echo/d81p9p9/`
    - Dependencies: Guile Scheme
    - Provides: A000081 implementation, Matula numbers, tree enumeration

23. **deltecho**
    - Location: `core/avatar/deep-tree-echo/deltecho/`
    - Dependencies: d81p9p9, DeltaChat, Dovecot
    - Provides: Deep Tree Echo orchestrator, Delta Echo desk, communication core

### Layer 4: Integration Layer
**Location**: `core/integration/cognitive-grip/`
**Build System**: CMake
**Dependencies**: All of Layer 3
**Components**:
- Unified API abstraction
- Cross-layer communication
- Cognitive synergy coordination

## Build Dependency Graph (Correct Order)

```
Layer 0: Inferno Kernel
  └─> Layer 0.5: MIG
       └─> Layer 1: CoGNUMach
            └─> Layer 2: CoGNUHurd
                 └─> Layer 3: OpenCog Collection
                      ├─> cogutil (no deps)
                      ├─> atomspace (→ cogutil)
                      ├─> atomspace-storage (→ atomspace) ⭐ CRITICAL
                      ├─> atomspace-rocks (→ atomspace-storage)
                      ├─> atomspace-pgres (→ atomspace-storage)
                      ├─> cogserver (→ atomspace, atomspace-storage)
                      ├─> atomspace-cog (→ atomspace-storage, cogserver)
                      ├─> unify (→ atomspace)
                      ├─> ure (→ atomspace, unify)
                      ├─> pln (→ atomspace, ure)
                      ├─> spacetime (→ atomspace)
                      ├─> attention (→ atomspace, cogserver)
                      ├─> learn (→ atomspace, cogserver)
                      ├─> miner (→ atomspace, ure)
                      ├─> asmoses (→ atomspace, cogutil)
                      ├─> link-grammar (no deps)
                      ├─> lg-atomese (→ atomspace, link-grammar)
                      └─> agents (→ atomspace)
                      └─> Layer 4: Cognitive-Grip (→ all Layer 3)
```

## MIG Consolidation Plan

### Current State
- **Primary**: `core/microkernel/cognumach/mig/` (40+ files, complete implementation)
- **Duplicate**: `core/os/hurdcog/external/hurd-repos/mig/` (1 README file)

### Resolution
1. Keep `core/microkernel/cognumach/mig/` as the authoritative source
2. Remove `core/os/hurdcog/external/hurd-repos/mig/`
3. Create symlink: `core/os/hurdcog/external/hurd-repos/mig -> ../../../../microkernel/cognumach/mig`
4. Update CoGNUHurd build scripts to reference cognumach MIG

## Debian Packaging Dependencies

All packages must be built in dependency order:

1. `inferno-kernel` (Layer 0)
2. `cognumach-mig` (Layer 0.5)
3. `cognumach` (Layer 1)
4. `hurdcog` (Layer 2)
5. `cogutil` (Layer 3.1)
6. `atomspace` (Layer 3.2)
7. `atomspace-storage` (Layer 3.3) ⭐
8. `atomspace-rocks`, `atomspace-pgres` (Layer 3.4)
9. `cogserver` (Layer 3.5)
10. `pln`, `ure`, `unify`, `spacetime` (Layer 3.6)
11. `attention` (Layer 3.7)
12. `learn`, `miner`, `asmoses` (Layer 3.8)
13. `link-grammar`, `lg-atomese` (Layer 3.9)
14. `agents` (Layer 3.10)
15. `cognitive-grip` (Layer 4)

## Integration Points

### Inferno ↔ CoGNUMach
- 9P protocol for IPC
- Styx protocol compatibility
- Shared namespace concepts

### CoGNUMach ↔ CoGNUHurd
- MIG-generated interfaces
- Mach IPC primitives
- Kernel services

### CoGNUHurd ↔ OpenCog
- AtomSpace-Hurd bridge
- Cognitive translators
- Semantic filesystem

### OpenCog ↔ Cognitive-Grip
- Unified API
- Cross-component communication
- Cognitive synergy

## Build System Integration

### Inferno Kernel
- Primary: CMake (for C components)
- Secondary: Limbo compiler (for Limbo modules)
- Output: Kernel image, libraries, tools

### CoGNUMach/CoGNUHurd
- Primary: Autotools (configure, make)
- Dependencies: MIG (autotools)
- Output: Kernel binaries, headers

### OpenCog Collection
- Primary: CMake
- All components use CMake
- Output: Libraries, executables, Python/Guile bindings

### Integration Strategy
1. Root CMakeLists.txt coordinates all layers
2. Autotools components built via ExternalProject_Add
3. Proper dependency ordering enforced
4. Parallel builds where possible

## Next Steps

1. ✅ Integrate infernos → core/inferno-kernel/
2. 🔄 Consolidate MIG to single location
3. 🔄 Update build scripts for correct dependency order
4. 🔄 Complete debian packaging for all components
5. 🔄 Implement cognitive-grip integration layer
6. 🔄 Test full build
7. 🔄 Commit and push changes
