# AGI-OS Integration Architecture

This document describes the complete integration of OpenCog Collection (OCC), Cognumach, and HurdCog into a unified Autonomous General Intelligence Operating System.

## System Architecture

AGI-OS is organized as a four-layer cognitive architecture:

### Layer 0: Build Tools

**MIG (Mach Interface Generator)** is the foundational build tool used by both Cognumach and HurdCog. It generates interface definitions for inter-process communication in the microkernel architecture.

### Layer 1: Cognitive Microkernel

**Cognumach** is a fork of GNU Mach enhanced with cognitive capabilities. It provides:
- Low-level memory management with cognitive awareness
- Inter-process communication (IPC) with cognitive routing
- Hardware abstraction with cognitive optimization
- 64-bit port support for modern systems

The microkernel is built using GNU autotools and integrated into the CMake build system via custom targets.

### Layer 2: Cognitive Operating System

**HurdCog** is GNU Hurd extended with cognitive capabilities. It provides:
- File system abstraction with cognitive caching
- Process management with cognitive scheduling
- Device drivers with cognitive optimization
- Network stack with cognitive routing
- Cognitive kernel extensions for self-aware system management

HurdCog includes cognitive extensions written in Python and Scheme, enabling self-introspection and adaptive behavior.

### Layer 3: AGI Framework

**OpenCog Collection (OCC)** provides the cognitive architecture:

#### Foundation Components
- **CogUtil**: Foundational utilities and data structures
- **AtomSpace**: Hypergraph database for knowledge representation
- **AtomSpace Storage**: Pluggable storage backends (RocksDB, PostgreSQL)
- **CogServer**: Networking and server infrastructure

#### Cognitive Components
- **Learn**: Symbolic learning and pattern mining
- **Agents**: Interactive agent framework
- **Attention**: Attention allocation and control
- **ASMOSES**: Program synthesis and optimization

#### Extensions
- **AtomSpace Extensions**: MeTTa language support, bridges to other systems
- **AtomSpace Accelerator**: GPU-accelerated inference
- **Agentic Chatbots**: Conversational AI integration

## Build Dependency Order

The system enforces strict build ordering to ensure all dependencies are satisfied:

```
Layer 0: MIG
    ↓
Layer 1: Cognumach
    ↓
Layer 2: HurdCog
    ↓
Layer 3: OpenCog Collection
    ├─ CogUtil (no dependencies)
    ├─ AtomSpace (depends on CogUtil)
    ├─ AtomSpace Storage (depends on AtomSpace)
    ├─ CogServer (depends on AtomSpace + AtomSpace Storage)
    ├─ Learn (depends on AtomSpace)
    ├─ Agents (depends on AtomSpace)
    ├─ Attention (depends on AtomSpace)
    └─ Extensions (depend on AtomSpace)
```

## Directory Structure

```
agi-os/
├── core/
│   ├── cognition/
│   │   ├── foundation/
│   │   │   ├── cogutil/
│   │   │   ├── atomspace/
│   │   │   ├── atomspace-storage/
│   │   │   ├── cogserver/
│   │   │   ├── learn/
│   │   │   ├── agents/
│   │   │   └── attention/
│   │   ├── storage/
│   │   │   └── atomspace-*/
│   │   └── generation/
│   │       └── agentic-chatbots/
│   ├── microkernel/
│   │   ├── cognumach/
│   │   │   ├── mig/
│   │   │   ├── kern/
│   │   │   └── ...
│   │   └── ...
│   ├── os/
│   │   ├── hurdcog/
│   │   │   ├── cogkernel/
│   │   │   ├── hurd/
│   │   │   └── ...
│   │   └── ...
│   └── integration/
│       └── cognitive-grip/
├── CMakeLists.txt
├── build-agi-os.sh
├── DEBIAN_PACKAGING.md
└── AGI-OS-INTEGRATION.md
```

## Critical Integration Points

### 1. AtomSpace Storage as CogServer Dependency

**Problem**: CogServer requires AtomSpace Storage for s-expression parsing, but the original build order didn't ensure this dependency was satisfied.

**Solution**: 
- Updated `CMakeLists.txt` to build AtomSpace Storage before CogServer
- Added explicit dependency in CogServer's `debian/control` file
- Updated GitHub Actions workflow to enforce correct build order

### 2. MIG in Both Cognumach and HurdCog

**Problem**: MIG (Mach Interface Generator) is present in both Cognumach and HurdCog directories, creating potential conflicts.

**Solution**:
- Centralized MIG as Layer 0 build tool
- Both Cognumach and HurdCog reference the same MIG instance
- MIG is built once and shared by both components

### 3. Cognitive Extensions Integration

**Problem**: HurdCog's cognitive extensions (Python/Scheme) need to integrate with the C-based OS kernel.

**Solution**:
- Created `cogkernel/` directory in HurdCog for cognitive modules
- Implemented Python/Scheme bindings to kernel interfaces
- Integrated cognitive modules into the build process
- Enabled cognitive kernel extensions via CMake options

## Building AGI-OS

### Quick Start (OCC Only)

```bash
./build-agi-os.sh --occ-only
```

This builds the OpenCog Collection with all cognitive components.

### Full Stack Build

```bash
./build-agi-os.sh --all
```

This builds all layers: Cognumach, HurdCog, and OpenCog Collection.

### Microkernel Only

```bash
./build-agi-os.sh --cognumach
```

### Microkernel + OS

```bash
./build-agi-os.sh --hurdcog
```

### Custom Installation Prefix

```bash
./build-agi-os.sh --all --prefix /opt/agi-os --jobs 4
```

## Debian Packaging

All components support Debian packaging with complete dependency specifications:

```bash
cd core/cognition/foundation/cogutil
debuild -us -uc
```

See `DEBIAN_PACKAGING.md` for detailed packaging instructions.

## GitHub Actions Workflow

The `.github/workflows/agi-os-build.yml` workflow:

1. **Builds components in dependency order**: Each job depends on its prerequisites
2. **Caches build artifacts**: Reduces build time for repeated builds
3. **Installs progressively**: Each stage installs its dependencies before building
4. **Generates Debian packages**: Creates distributable packages
5. **Runs integration tests**: Verifies the complete system

## Integration Testing

The integration test stage verifies:

- All core components are present
- CMake configuration succeeds
- Build system is correctly structured
- Dependency declarations are accurate

## Future Enhancements

### Immediate (Phase 1)
- Complete Debian packaging for all components
- Finalize Cognumach build system
- Complete HurdCog cognitive kernel integration

### Short-term (Phase 2)
- Add Python bindings for all components
- Implement Guile/Scheme integration
- Create Docker containers for easy deployment

### Medium-term (Phase 3)
- Implement distributed AtomSpace (MachSpace)
- Add cognitive fusion reactor
- Integrate with external LLMs (Mistral, Llama)

### Long-term (Phase 4)
- Full AGI-OS kernel with cognitive scheduling
- Self-aware system management
- Autonomous repair and optimization

## Performance Considerations

### Build Optimization
- Parallel builds: Use `--jobs N` to control parallelism
- Incremental builds: CMake caches reduce rebuild time
- Ccache integration: Compiler cache speeds up repeated builds

### Runtime Optimization
- AtomSpace Storage backends: Choose RocksDB for speed, PostgreSQL for persistence
- Attention mechanisms: Allocate resources based on cognitive load
- Distributed execution: Use MachSpace for multi-machine reasoning

## Security Considerations

### Build Security
- Signed Debian packages: Use GPG keys for production
- Dependency verification: Check package signatures
- Source verification: Verify git commits and tags

### Runtime Security
- Capability-based security: Leverage Mach/Hurd security model
- Cognitive monitoring: Track system behavior for anomalies
- Sandboxing: Isolate cognitive components

## Troubleshooting

### Build Failures

Check the build log for specific errors:

```bash
./build-agi-os.sh --occ-only 2>&1 | tee build.log
```

Common issues:
- Missing dependencies: Install via `apt-get build-dep`
- CMake errors: Ensure CMake >= 3.12
- Compiler errors: Use GCC 11+ for C++17 support

### Runtime Issues

Verify installation:

```bash
pkg-config --modversion cogutil
pkg-config --modversion atomspace
cogserver --version
```

Check library paths:

```bash
ldconfig -p | grep opencog
```

## References

- [OpenCog Documentation](https://wiki.opencog.org/)
- [GNU Mach Documentation](https://www.gnu.org/software/hurd/gnumach.html)
- [GNU Hurd Documentation](https://www.gnu.org/software/hurd/)
- [CMake Documentation](https://cmake.org/documentation/)
- [Debian Policy Manual](https://www.debian.org/doc/debian-policy/)
