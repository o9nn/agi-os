# AGI-OS Structure Analysis

## Current Repository Organization

### Layer 0: CogNumach (Microkernel) - `/cognumach/`
**Total Subdirectories**: 44

**Core Components**:
- `kern/` - Kernel core functionality
- `ipc/` - Inter-process communication
- `vm/` - Virtual memory management
- `device/` - Device drivers
- `i386/`, `x86_64/`, `aarch64/` - Architecture-specific code
- `include/` - Public headers (mach, mach_debug, device, sys)
- `mig/` - Mach Interface Generator
- `util/` - Utilities
- `tests/` - Test suite
- `docs/`, `doc/` - Documentation

**Structure Pattern**: Traditional microkernel organization
- Architecture separation (i386, x86_64, aarch64)
- Subsystem directories (kern, ipc, vm, device)
- Public API in include/
- Build tools (mig)

### Layer 1: HurdCog (Cognitive OS) - `/hurdcog/`
**Total Subdirectories**: 80+

**Core Hurd Servers**:
- `auth/` - Authentication server
- `proc/` - Process server
- `exec/` - Execution server
- `ext2fs/`, `fatfs/`, `isofs/` - Filesystem translators
- `pfinet/` - Network stack
- `console/`, `console-client/` - Console management

**Cognitive Components**:
- `cogkernel/` - Cognitive kernel integration (24 subdirs)
  - `agents/` - Cognitive agents
  - `atomspace/` - AtomSpace integration
  - `attention/` - Attention mechanisms
  - `reasoning/` - Reasoning systems
  - `meta-cognition/` - Meta-cognitive capabilities
  - `mach-integration/` - Mach integration layer
  - `cognitive-interface/` - Cognitive interfaces
  - `embodiment/` - Embodiment systems
  - `visualization/` - Visualization tools
  - `security/` - Security mechanisms
  - `tensors/` - Tensor operations
  - `ggml/` - GGML integration

**Libraries** (lib* pattern):
- `libdiskfs/`, `libnetfs/`, `libtrivfs/` - Filesystem libraries
- `libports/`, `libihash/`, `libpipe/` - Core libraries
- `libfshelp/`, `libiohelp/`, `libirqhelp/` - Helper libraries
- `libhurd-slab/`, `libmachdev/` - Hurd-specific libraries

**External Dependencies**:
- `external/` - External repositories
  - `gnu-repos/` - GNU repositories
  - `hurd-repos/` - Hurd repositories
  - `unicorn-forest-repos/` - Unicorn Forest repositories

**Build System**:
- `guix-build-system/` - Guix build stages
- `hurd-ecosystem/` - Hurd ecosystem integration

**Structure Pattern**: Hurd server architecture + cognitive extensions
- Server-based organization (auth, proc, exec, etc.)
- Translator pattern (filesystem, network)
- Cognitive kernel as integrated subsystem
- Library-based architecture

### Layer 2: OpenCog Components (Root Level)
**Total Components**: 40+ directories

**Core Framework**:
- `cogutil/` - C++ utilities library
- `atomspace/` - Hypergraph knowledge representation
- `cogserver/` - Network server

**Storage Backends**:
- `atomspace-storage/` - Storage API (CRITICAL)
- `atomspace-cog/` - CogServer storage
- `atomspace-rocks/` - RocksDB storage
- `atomspace-pgres/` - PostgreSQL storage
- `atomspace-ipfs/`, `atomspace-dht/` - Distributed storage

**Reasoning & Learning**:
- `pln/` - Probabilistic Logic Networks
- `ure/` - Unified Rule Engine
- `attention/` - Economic Attention Networks
- `miner/` - Pattern mining
- `unify/` - Unification framework
- `learn/` - Learning systems
- `generate/` - Generation systems

**NLP & Language**:
- `relex/` - Relation extraction
- `lg-atomese/` - Link Grammar integration
- `link-grammar/` - Link Grammar parser

**Specialized Systems**:
- `moses/`, `asmoses/` - Evolutionary learning
- `vision/` - Computer vision
- `spacetime/` - Spatiotemporal reasoning
- `agi-bio/` - Bioinformatics

**Structure Pattern**: Modular component architecture
- Each component is self-contained
- Flat hierarchy at root level
- Shared dependency on cogutil and atomspace
- Independent build systems per component

### Layer 3: Integration Layer - `/cognitive-grip/`
**Status**: Newly created

**Structure**:
- `include/opencog/cognitive-grip/` - Headers
- `src/` - Implementation
- `tests/` - Test suite

**Purpose**: Unified abstraction across all layers

### Packaging Infrastructure - `/opencog-debian/`
**Total Packages**: 35+

**Structure**: One directory per package
- Each package has `debian/` subdirectory
- Update scripts per package
- Build orchestration scripts at root

## Structure Analysis

### Current Issues

1. **Flat Root Organization**
   - 120+ directories at root level
   - No clear layer separation
   - Difficult to navigate
   - Unclear dependencies

2. **Inconsistent Naming**
   - Some use hyphens (atomspace-storage)
   - Some use underscores
   - Some are single words (atomspace)

3. **Mixed Concerns**
   - Microkernel, OS, and cognitive components at same level
   - Build infrastructure mixed with source
   - Documentation scattered

4. **Duplicate Functionality**
   - hurdcog/cogkernel has atomspace, attention, reasoning
   - Root has atomspace, attention, pln, ure
   - Unclear which is authoritative

5. **External Dependencies**
   - hurdcog/external contains submodules
   - No clear integration strategy

### Strengths

1. **CogNumach Organization**
   - Clear microkernel structure
   - Well-separated architecture code
   - Standard kernel organization

2. **HurdCog Cognitive Integration**
   - cogkernel/ provides clear cognitive layer
   - Server architecture is well-defined
   - Library organization follows Hurd patterns

3. **OpenCog Modularity**
   - Each component is independent
   - Clear dependency chain
   - Well-documented APIs

4. **Debian Packaging**
   - Complete packaging for all components
   - Dependency management
   - Build orchestration

## Optimal Structure Principles

### 1. Layer-Based Organization
Organize by architectural layer, not by technology

### 2. Unified Namespace
Use consistent naming across all components

### 3. Clear Dependencies
Make dependency relationships explicit in structure

### 4. Separation of Concerns
- Source code
- Build infrastructure
- Documentation
- Tests
- Examples

### 5. Integration Points
Explicit directories for layer integration

### 6. Cognitive Synergy
Structure should reflect cognitive integration, not just technical separation

## Proposed Structure Categories

### A. Core Layers
- Microkernel (CogNumach)
- Operating System (HurdCog)
- Cognitive Framework (OpenCog)
- Integration Layer (Cognitive-Grip)

### B. Cross-Cutting Concerns
- Build system
- Packaging
- Documentation
- Tests
- Examples
- Tools

### C. Shared Resources
- Libraries
- Headers
- Configuration
- Data

### D. External Dependencies
- Submodules
- Third-party integrations

## Next Steps

1. Design optimal unified structure
2. Create migration plan
3. Reorganize components
4. Update build system
5. Update documentation
6. Validate and test
