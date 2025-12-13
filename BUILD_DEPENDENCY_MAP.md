# AGI-OS Build Dependency Map

## Overview

This document maps all build dependencies across CogNumach, HurdCog, and OpenCog subsystems, identifying shared components and optimal build order.

## Dependency Layers

### Layer 0: Build Tools

```
mig (Mach Interface Generator)
├── Location: core/microkernel/cognumach/mig/
├── Used by: cognumach, hurdcog
├── Type: Code generator for Mach IPC
└── Build: autotools-based
```

**Shared Dependency Resolution**:
- Primary location: `core/microkernel/cognumach/mig/`
- Symlink in hurdcog: `core/os/hurdcog/mig -> ../../microkernel/cognumach/mig`
- Status: ✅ Correctly configured

### Layer 1: Microkernel (CogNumach)

```
cognumach (GNU Mach + Cognitive Extensions)
├── Location: core/microkernel/cognumach/
├── Dependencies: mig
├── Provides: Mach IPC, cognitive scheduling primitives
├── Build System: autotools (configure, make)
└── Build Order: 1
```

**Key Components**:
- `mig/` - Mach Interface Generator
- `include/` - Mach kernel headers
- `kern/` - Kernel core
- `ipc/` - Inter-process communication
- `vm/` - Virtual memory management

### Layer 2: Operating System (HurdCog)

```
hurdcog (GNU Hurd + Cognitive Extensions)
├── Location: core/os/hurdcog/
├── Dependencies: cognumach, mig
├── Provides: Cognitive translators, semantic filesystem
├── Build System: autotools (configure, make)
└── Build Order: 2
```

**Key Components**:
- `mig` (symlink) - Shared with cognumach
- `libhurd-slab/` - Memory allocator
- `libhurdbugaddr/` - Debug utilities
- `trans/` - Translators (filesystem servers)
- `boot/` - Boot system
- `exec/` - Process execution

**Shared Dependencies with CogNumach**:
1. `mig` - Interface generator (symlinked)
2. Mach headers - From cognumach
3. IPC definitions - From cognumach

### Layer 3: OpenCog Foundation

#### 3.1 CogUtil (Foundation Library)

```
cogutil
├── Location: core/cognition/foundation/cogutil/
├── Dependencies: None (foundation)
├── Provides: Utilities, logging, config, backtrace
├── Build System: CMake
├── Build Order: 3
└── Required by: ALL OpenCog components
```

**Key Exports**:
- `libcogutil.so` - Core utilities library
- Headers: `opencog/util/*.h`
- CMake: `CogUtilConfig.cmake`

#### 3.2 AtomSpace (Hypergraph Database)

```
atomspace
├── Location: core/cognition/foundation/atomspace/
├── Dependencies: cogutil
├── Provides: Hypergraph, atoms, truth values, pattern matching
├── Build System: CMake
├── Build Order: 4
└── Required by: ALL cognitive components
```

**Key Exports**:
- `libatomspace.so` - Core hypergraph
- `libatombase.so` - Base atom types
- `libatomcore.so` - Core operations
- Headers: `opencog/atomspace/*.h`
- CMake: `AtomSpaceConfig.cmake`

**Language Bindings**:
- Python: `opencog/cython/`
- Guile: `opencog/guile/`
- Haskell: `opencog/haskell/`

#### 3.3 AtomSpace Storage

```
atomspace-storage
├── Location: core/cognition/foundation/atomspace-storage/
├── Dependencies: cogutil, atomspace
├── Provides: Persistence, storage backends
├── Build System: CMake
├── Build Order: 5
└── Required by: cogserver, distributed systems
```

**Storage Backends** (in `core/cognition/storage/`):
- `atomspace-rocks/` - RocksDB backend
- `atomspace-pgres/` - PostgreSQL backend
- `atomspace-cog/` - CogServer backend
- `atomspace-machspace/` - Mach IPC backend (for HurdCog integration)

### Layer 4: OpenCog Network

```
cogserver
├── Location: core/cognition/foundation/cogserver/
├── Dependencies: cogutil, atomspace, atomspace-storage
├── Provides: Network API, shell, modules
├── Build System: CMake
├── Build Order: 6
└── Required by: Distributed cognition
```

**Key Exports**:
- `libcogserver.so` - Network server
- Shell interface
- Module loading system

### Layer 5: OpenCog Reasoning

#### 5.1 PLN (Probabilistic Logic Networks)

```
pln
├── Location: core/cognition/reasoning/pln/
├── Dependencies: cogutil, atomspace
├── Provides: Deduction, induction, abduction
├── Build System: CMake
└── Build Order: 7
```

#### 5.2 URE (Unified Rule Engine)

```
ure
├── Location: core/cognition/reasoning/ure/
├── Dependencies: cogutil, atomspace
├── Provides: Forward/backward chaining
├── Build System: CMake
└── Build Order: 7 (parallel with PLN)
```

#### 5.3 Unify

```
unify
├── Location: core/cognition/reasoning/unify/
├── Dependencies: cogutil, atomspace
├── Provides: Unification, pattern matching
├── Build System: CMake
└── Build Order: 7 (parallel with PLN, URE)
```

#### 5.4 SpaceTime

```
spacetime
├── Location: core/cognition/reasoning/spacetime/
├── Dependencies: cogutil, atomspace
├── Provides: Spatial/temporal reasoning
├── Build System: CMake
└── Build Order: 7 (parallel with others)
```

### Layer 6: OpenCog Attention

```
ecan (Economic Attention Networks)
├── Location: core/cognition/attention/ecan/
├── Dependencies: cogutil, atomspace
├── Provides: STI, LTI, attention allocation
├── Build System: CMake
└── Build Order: 8
```

**Key Components**:
- `attentionbank/` - Attention value management
- `avalue/` - Attention value types
- `bank/` - Attention allocation algorithms

### Layer 7: OpenCog Learning

#### 7.1 Learn

```
learn
├── Location: core/cognition/foundation/learn/
├── Dependencies: cogutil, atomspace
├── Provides: Symbolic learning
├── Build System: CMake
└── Build Order: 9
```

#### 7.2 Miner

```
miner
├── Location: core/cognition/learning/miner/
├── Dependencies: cogutil, atomspace, ure
├── Provides: Pattern mining
├── Build System: CMake
└── Build Order: 9
```

#### 7.3 MOSES

```
moses
├── Location: core/cognition/learning/moses/
├── Dependencies: cogutil, atomspace
├── Provides: Program evolution
├── Build System: CMake
└── Build Order: 9
```

#### 7.4 AS-MOSES

```
asmoses
├── Location: core/cognition/foundation/asmoses/
├── Dependencies: cogutil, atomspace, moses
├── Provides: AtomSpace-based MOSES
├── Build System: CMake
└── Build Order: 10 (after moses)
```

### Layer 8: OpenCog Generation

```
generate
├── Location: core/cognition/generation/generate/
├── Dependencies: cogutil, atomspace
├── Provides: Content generation
├── Build System: CMake
└── Build Order: 11
```

### Layer 9: OpenCog Language

#### 9.1 Link Grammar

```
link-grammar
├── Location: core/cognition/language/link-grammar/
├── Dependencies: None (external)
├── Provides: Syntactic parsing
├── Build System: CMake
└── Build Order: 12
```

#### 9.2 LG-AtomESE

```
lg-atomese
├── Location: core/cognition/language/lg-atomese/
├── Dependencies: cogutil, atomspace, link-grammar
├── Provides: Link Grammar → AtomSpace
├── Build System: CMake
└── Build Order: 13
```

#### 9.3 RelEx

```
relex
├── Location: core/cognition/language/relex/
├── Dependencies: cogutil, atomspace, link-grammar
├── Provides: Semantic relation extraction
├── Build System: CMake/Java
└── Build Order: 13
```

### Layer 10: OpenCog Perception

```
vision
├── Location: core/cognition/perception/vision/
├── Dependencies: cogutil, atomspace
├── Provides: Visual processing
├── Build System: CMake
└── Build Order: 14
```

### Layer 11: OpenCog Specialized

```
agi-bio
├── Location: core/cognition/specialized/agi-bio/
├── Dependencies: cogutil, atomspace, pln
├── Provides: Bioinformatics reasoning
├── Build System: CMake
└── Build Order: 15
```

### Layer 12: Additional Components

#### 12.1 CogBolt

```
cogbolt
├── Location: cogbolt/
├── Dependencies: C++ standard library, optional OpenCog
├── Provides: AI-powered IDE core
├── Build System: CMake
└── Build Order: 16 (independent)
```

#### 12.2 Consciousness Layer

```
consciousness
├── Location: consciousness/
├── Dependencies: cogutil, atomspace, attention
├── Provides: Consciousness mechanisms
├── Build System: CMake
└── Build Order: 17
```

#### 12.3 Personification Layer

```
personification
├── Location: personification/
├── Dependencies: cogutil, atomspace, consciousness
├── Provides: Personality and identity
├── Build System: CMake
└── Build Order: 18
```

## Optimal Build Order

### Sequential Build Order

```
1. mig                      (Layer 0: Build Tools)
2. cognumach                (Layer 1: Microkernel)
3. hurdcog                  (Layer 2: Operating System)
4. cogutil                  (Layer 3: Foundation)
5. atomspace                (Layer 3: Foundation)
6. atomspace-storage        (Layer 3: Foundation)
7. cogserver                (Layer 4: Network)
8. pln, ure, unify, spacetime  (Layer 5: Reasoning - parallel)
9. ecan                     (Layer 6: Attention)
10. learn, miner, moses     (Layer 7: Learning - parallel)
11. asmoses                 (Layer 7: Learning)
12. generate                (Layer 8: Generation)
13. link-grammar            (Layer 9: Language)
14. lg-atomese, relex       (Layer 9: Language - parallel)
15. vision                  (Layer 10: Perception)
16. agi-bio                 (Layer 11: Specialized)
17. cogbolt                 (Layer 12: IDE - independent)
18. consciousness           (Layer 12: Consciousness)
19. personification         (Layer 12: Personification)
```

### Parallel Build Opportunities

**Group 1: Reasoning Components** (can build in parallel)
- pln
- ure
- unify
- spacetime

**Group 2: Learning Components** (can build in parallel after moses)
- learn
- miner

**Group 3: Language Components** (can build in parallel after link-grammar)
- lg-atomese
- relex

**Group 4: Independent Components** (can build anytime)
- cogbolt (no OpenCog dependencies)

## Critical Dependencies

### Must Build First (Blockers)

1. **cogutil** - Required by ALL OpenCog components
2. **atomspace** - Required by ALL cognitive components
3. **atomspace-storage** - Required by cogserver and distributed systems
4. **mig** - Required by cognumach and hurdcog

### Circular Dependencies

**None identified** - Clean dependency graph

### Optional Dependencies

1. **Haskell bindings** - Optional for atomspace
2. **Python bindings** - Optional but recommended
3. **Guile bindings** - Optional but recommended for Scheme interface

## Shared Build Artifacts

### Libraries

| Library | Location | Used By |
|---------|----------|---------|
| `libcogutil.so` | cogutil | ALL OpenCog components |
| `libatomspace.so` | atomspace | ALL cognitive components |
| `libatombase.so` | atomspace | ALL cognitive components |
| `libatomcore.so` | atomspace | ALL cognitive components |
| `libattentionbank.so` | ecan | Attention-aware components |
| `libcogserver.so` | cogserver | Network clients |

### Headers

| Header Path | Provided By | Used By |
|-------------|-------------|---------|
| `opencog/util/*.h` | cogutil | ALL |
| `opencog/atomspace/*.h` | atomspace | ALL cognitive |
| `opencog/attentionbank/*.h` | ecan | Attention components |
| `mach/*.h` | cognumach | hurdcog |

### CMake Config Files

| Config | Location | Purpose |
|--------|----------|---------|
| `CogUtilConfig.cmake` | cogutil | Find cogutil |
| `AtomSpaceConfig.cmake` | atomspace | Find atomspace |
| `AttentionConfig.cmake` | ecan | Find attention |
| `CogServerConfig.cmake` | cogserver | Find cogserver |

## Build System Integration

### CMake Dependency Chain

```cmake
# Root CMakeLists.txt
add_subdirectory(core/cognition/foundation/cogutil)         # 1
add_subdirectory(core/cognition/foundation/atomspace)       # 2 (needs cogutil)
add_subdirectory(core/cognition/foundation/atomspace-storage) # 3 (needs atomspace)
add_subdirectory(core/cognition/foundation/cogserver)       # 4 (needs storage)
add_subdirectory(core/cognition/reasoning/pln)              # 5 (needs atomspace)
add_subdirectory(core/cognition/reasoning/ure)              # 5 (needs atomspace)
add_subdirectory(core/cognition/reasoning/unify)            # 5 (needs atomspace)
add_subdirectory(core/cognition/reasoning/spacetime)        # 5 (needs atomspace)
add_subdirectory(core/cognition/attention/ecan)             # 6 (needs atomspace)
add_subdirectory(core/cognition/foundation/learn)           # 7 (needs atomspace)
add_subdirectory(core/cognition/learning/miner)             # 7 (needs ure)
add_subdirectory(core/cognition/learning/moses)             # 7 (needs atomspace)
add_subdirectory(core/cognition/foundation/asmoses)         # 8 (needs moses)
add_subdirectory(core/cognition/generation/generate)        # 9 (needs atomspace)
add_subdirectory(core/cognition/language/link-grammar)      # 10 (independent)
add_subdirectory(core/cognition/language/lg-atomese)        # 11 (needs link-grammar)
add_subdirectory(core/cognition/language/relex)             # 11 (needs link-grammar)
add_subdirectory(core/cognition/perception/vision)          # 12 (needs atomspace)
add_subdirectory(core/cognition/specialized/agi-bio)        # 13 (needs pln)
add_subdirectory(cogbolt)                                   # 14 (independent)
add_subdirectory(consciousness)                             # 15 (needs attention)
add_subdirectory(personification)                           # 16 (needs consciousness)
```

### Autotools Dependency Chain

```bash
# CogNumach (autotools)
cd core/microkernel/cognumach
autoreconf -fi
./configure --prefix=/usr/local
make -j$(nproc)
make install

# HurdCog (autotools, needs cognumach)
cd core/os/hurdcog
autoreconf -fi
./configure --prefix=/usr/local --with-mach=/usr/local
make -j$(nproc)
make install
```

## Inferno Kernel Integration Points

### New Dependencies for Inferno Integration

```
inferno-kernel
├── Dependencies: None (new foundation)
├── Provides: Dis VM, 9P protocol, Limbo runtime
├── Build System: Custom (Inferno build system)
└── Build Order: 0 (before or parallel with cognumach)

atomspace-9p
├── Dependencies: inferno-kernel, atomspace
├── Provides: AtomSpace as 9P file servers
├── Build System: CMake + Limbo
└── Build Order: After atomspace

pln-9p
├── Dependencies: atomspace-9p, pln
├── Provides: PLN reasoning via 9P
├── Build System: CMake + Limbo
└── Build Order: After pln

ecan-9p
├── Dependencies: atomspace-9p, ecan
├── Provides: ECAN attention via 9P
├── Build System: CMake + Limbo
└── Build Order: After ecan

distributed-cognition
├── Dependencies: atomspace-9p, pln-9p, ecan-9p
├── Provides: Distributed reasoning
├── Build System: CMake + Limbo
└── Build Order: After all 9P components
```

## Summary

### Total Components: 30+

- **Microkernel**: 1 (cognumach)
- **Operating System**: 1 (hurdcog)
- **Foundation**: 6 (cogutil, atomspace, atomspace-storage, cogserver, learn, asmoses)
- **Reasoning**: 4 (pln, ure, unify, spacetime)
- **Attention**: 1 (ecan)
- **Learning**: 2 (miner, moses)
- **Generation**: 1 (generate)
- **Language**: 3 (link-grammar, lg-atomese, relex)
- **Perception**: 1 (vision)
- **Specialized**: 1 (agi-bio)
- **Additional**: 3 (cogbolt, consciousness, personification)
- **Storage Backends**: 4+ (rocks, pgres, cog, machspace)

### Build Time Estimate

- **Sequential**: ~4-6 hours (depending on hardware)
- **Parallel (8 cores)**: ~1-2 hours
- **With Inferno integration**: +2-4 hours

### Critical Path

```
cogutil → atomspace → atomspace-storage → cogserver
                   ↓
                  pln → agi-bio
                   ↓
                 ecan → consciousness → personification
```

---

**Document Version**: 1.0  
**Date**: December 13, 2025  
**Status**: Complete dependency analysis
