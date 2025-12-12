# AGI-OS: Autonomous General Intelligence Operating System

## Introduction

**AGI-OS** is a revolutionary three-layer operating system architecture that integrates cognitive computing capabilities from the microkernel level through the operating system to the application framework. It combines:

- **CogNumach**: Enhanced GNU Mach microkernel with cognitive features
- **HurdCog**: OpenCog-powered GNU Hurd cognitive operating system
- **OpenCog**: Comprehensive cognitive computing framework
- **Cognitive-Grip**: Unified abstraction layer for seamless integration

## Architecture

### Layer 0: Microkernel (CogNumach)
Provides foundational process and memory management with cognitive-aware primitives:
- Message-passing IPC optimized for cognitive agents
- Virtual memory management for large knowledge bases
- Symmetric multiprocessing (SMP) support
- Enhanced device driver infrastructure

### Layer 1: Operating System (HurdCog)
Cognitive-aware system services including:
- Process server with attention allocation
- Authentication with trust models
- Semantic filesystem indexing
- Adaptive network routing
- Cognitive translators for all system services

### Layer 2: Cognitive Framework (OpenCog)
Complete knowledge representation and reasoning:
- AtomSpace: Hypergraph knowledge base
- Probabilistic Logic Networks (PLN)
- Unified Rule Engine (URE)
- Economic Attention Networks (ECAN)
- Pattern mining and learning systems

### Layer 3: Integration (Cognitive-Grip)
Unified abstraction and coordination:
- Central AtomSpace access
- Microkernel interface abstraction
- OS service integration
- Component registration and invocation
- Storage backend coordination

## Quick Start

### Prerequisites
- Ubuntu 22.04 or later
- 4GB RAM minimum (8GB recommended)
- 20GB disk space
- Build tools: build-essential, cmake, git

### Installation

```bash
# Clone the repository
git clone https://github.com/o9nn/agi-os.git
cd agi-os

# Install dependencies
sudo apt-get update
sudo apt-get install -y build-essential cmake git libboost-all-dev

# Build all components
cd opencog-debian
bash build-all-unified.sh
```

### Verification

```bash
# Check installation
dpkg -l | grep opencog
dpkg -l | grep atomspace
dpkg -l | grep cognitive-grip
```

## Build Order

The build system enforces a strict dependency order:

1. **cognumach** - Microkernel foundation
2. **cogutil** - C++ utilities library
3. **atomspace** - Knowledge representation
4. **atomspace-storage** - CRITICAL: I/O foundation (must be before cogserver)
5. **cogserver** - Network server
6. **ure** - Unified Rule Engine
7. **hurdcog** - Cognitive OS
8. **cognitive-grip** - Integration layer
9. **attention** - Economic Attention Networks
10. **pln** - Probabilistic Logic Networks
11. And remaining cognitive components

## Key Features

### Unified Knowledge Representation
All system state is represented as Atoms in the unified AtomSpace, enabling:
- Consistent reasoning across all layers
- Unified attention allocation
- Seamless knowledge sharing

### Cognitive Synergy
Components work together for emergent capabilities:
- Microkernel provides efficient primitives
- OS provides system-level services
- OpenCog provides reasoning and learning
- Integration layer provides unified coordination

### Production Ready
- Complete Debian packaging for all components
- Comprehensive testing infrastructure
- Monitoring and debugging support
- Scalable storage backends

## Component Integration

### MachSpace Bridge
Connects Mach IPC with AtomSpace operations:
- Maps Mach ports to AtomSpace nodes
- Converts IPC messages to Atoms
- Enables cognitive processing of system messages

### HurdCog-AtomSpace Bridge
Integrates HurdCog translators with AtomSpace:
- Maps translator operations to Atoms
- Stores filesystem operations in AtomSpace
- Enables semantic search across system state

### CogNumach Scheduler Integration
Connects process scheduling with OpenCog attention:
- Maps process importance to attention values
- Uses ECAN for process prioritization
- Implements attention-driven scheduling

## Documentation

- **INTEGRATION_ANALYSIS.md**: Detailed analysis of integration opportunities
- **COMPONENT_INTEGRATION.md**: Component integration strategy
- **opencog-debian/BUILD_ORDER_ENHANCED.md**: Complete build order documentation
- **opencog-debian/AGI_OS_INTEGRATION.md**: AGI-OS integration guide

## Development

### Adding New Components

1. Create component directory: `mkdir my-component`
2. Add CMakeLists.txt with proper dependencies
3. Create Debian packaging in `opencog-debian/my-component/`
4. Update build scripts to include new component
5. Document integration points

### Testing

```bash
# Run unit tests
cd opencog-debian
bash validate-packaging.sh

# Run integration tests
bash test-integration.sh
```

## Performance Considerations

- **Memory**: Optimized for efficient knowledge representation
- **CPU**: Leverages SMP for parallel cognitive processing
- **Storage**: Multiple backend options (RocksDB, PostgreSQL, CogServer)
- **Network**: Distributed cognition support via CogServer

## Troubleshooting

### Build Failures
1. Check dependencies: `apt-get build-dep opencog-atomspace`
2. Review build logs: `opencog-debian/build.log`
3. Verify atomspace-storage is built before cogserver

### Runtime Issues
1. Check component status: `cognitive-grip-status`
2. Review system logs: `journalctl -u hurdcog`
3. Verify storage backend connectivity

## Contributing

We welcome contributions! Please:
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request
5. Ensure all tests pass

## License

AGI-OS is released under the AGPL v3 license. See LICENSE file for details.

## Support

- **Documentation**: https://opencog.org/
- **Community**: https://groups.google.com/forum/#!forum/opencog
- **Issues**: https://github.com/o9nn/agi-os/issues

## Acknowledgments

AGI-OS builds on the work of:
- OpenCog Foundation
- GNU Hurd Project
- GNU Mach Project
- Open Source Community

## Future Roadmap

- Distributed AGI-OS (multi-machine federation)
- Quantum computing backend support
- Neuromorphic hardware integration
- Advanced meta-learning capabilities
- Human-AI collaboration interfaces

---

**Status**: Production Ready (v1.0.0)  
**Last Updated**: December 2025  
**Maintainer**: OpenCog Development Team
