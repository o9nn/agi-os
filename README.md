# AGI-OS: Autonomous General Intelligence Operating System

**A unified cognitive operating system integrating CogNumach microkernel, HurdCog cognitive OS, and OpenCog cognitive framework into a seamless whole.**

🔗 **Repository**: https://github.com/o9nn/agi-os

## Overview

AGI-OS is a revolutionary operating system that integrates cognitive capabilities at every layer, from the microkernel to the application level. It combines three major systems into a unified cognitive architecture.

## Repository Structure (NEW)

The repository has been **redesigned** from 120+ top-level directories down to **7 clean directories** that clearly separate concerns:

```
agi-os/
├── core/                    # Core AGI-OS layers (unified whole)
├── infrastructure/          # Build, packaging, and tooling
├── shared/                  # Shared resources across layers
├── external/                # External dependencies
├── documentation/           # Comprehensive documentation
├── examples/                # Example applications
└── archive/                 # Historical and experimental code
```

### Core Directory (`core/`)

All three layers organized as a unified whole:

- **`core/microkernel/`** - Layer 0: CogNumach (Enhanced GNU Mach)
- **`core/os/`** - Layer 1: HurdCog (Cognitive GNU Hurd)
- **`core/cognition/`** - Layer 2: OpenCog (Cognitive Framework)
  - `foundation/` - cogutil, atomspace
  - `storage/` - atomspace-storage, backends
  - `reasoning/` - pln, ure, unify, spacetime
  - `attention/` - ecan
  - `learning/` - learn, miner, moses, asmoses
  - `generation/` - generate
  - `language/` - relex, lg-atomese, link-grammar
  - `perception/` - vision
  - `specialized/` - agi-bio
  - `network/` - cogserver
- **`core/integration/`** - Layer 3: Cognitive-Grip (Integration Layer)

## Quick Start

```bash
# Clone the repository
git clone https://github.com/o9nn/agi-os.git
cd agi-os

# Run the unified build script
./build-agi-os.sh
```

## Build Order

The unified build script enforces correct dependency order:

```
Layer 0: cognumach (microkernel)
Layer 1: cogutil, atomspace (foundation)
Layer 2: atomspace-storage ⭐ CRITICAL
Layer 3: cogserver (network services)
Layer 4: pln, ure, unify, spacetime (reasoning)
Layer 5: attention/ecan (attention)
Layer 6: learn, miner, moses, asmoses (learning)
Layer 7: generate (generation)
Layer 8: link-grammar, lg-atomese, relex (language)
Layer 9: vision (perception)
Layer 10: agi-bio (specialized)
Layer 11: cognitive-grip (integration)
```

## Key Features

### Cognitive Integration at Every Layer
- **Microkernel**: Cognitive scheduling with attention allocation
- **Operating System**: Cognitive translators and semantic filesystem
- **Cognitive Framework**: Complete reasoning, learning, and perception
- **Integration**: Seamless synergy across all layers

### Unified Knowledge Representation
- All system state stored in AtomSpace hypergraph
- Semantic queries across system and cognitive knowledge
- Unified attention mechanism for resource allocation

### Production Ready
- Complete Debian packaging (35+ packages)
- Unified build system
- Comprehensive documentation
- Clear dependency management

## Architecture

### Three-Layer Design

1. **CogNumach (Microkernel)** - Enhanced GNU Mach with cognitive scheduling
2. **HurdCog (Operating System)** - OpenCog-powered GNU Hurd
3. **OpenCog (Cognitive Framework)** - PLN, ECAN, pattern mining, NLP
4. **Cognitive-Grip (Integration)** - Unified abstraction across all layers

## Documentation

Comprehensive documentation in `documentation/`:

- **architecture/** - Design documents and analysis
- **guides/** - User and developer guides
- **api/** - API documentation
- **tutorials/** - Step-by-step tutorials
- **reports/** - Build validation and implementation reports

Key documents:
- `documentation/architecture/OPTIMAL_STRUCTURE_DESIGN.md` - New structure design
- `documentation/architecture/STRUCTURE_ANALYSIS.md` - Structure analysis
- `documentation/reports/BUILD_VALIDATION_REPORT.md` - Build validation
- `documentation/reports/AGI_OS_IMPLEMENTATION_SUMMARY.md` - Implementation summary

## Development

### Build Options

```bash
# Custom build directory
BUILD_DIR=/path/to/build ./build-agi-os.sh

# Custom install prefix
INSTALL_PREFIX=/opt/agi-os ./build-agi-os.sh

# Debug build
BUILD_TYPE=Debug ./build-agi-os.sh

# Parallel build
PARALLEL_JOBS=8 ./build-agi-os.sh
```

### Directory Organization Principles

1. **Layer-Based Organization** - Organized by architectural layer
2. **Unified Namespace** - Consistent naming conventions
3. **Clear Dependencies** - Explicit dependency relationships
4. **Separation of Concerns** - Source, build, docs, tests separated
5. **Cognitive Synergy** - Structure reflects cognitive integration

## Status

**Current Status**: Active Development

- ✅ Repository structure redesigned and optimized (120+ → 7 directories)
- ✅ All components reorganized into unified structure
- ✅ Build system updated for new structure
- ✅ Debian packaging infrastructure complete
- ✅ Comprehensive documentation created
- 🚧 Integration layer implementation in progress
- 🚧 Testing and validation ongoing

## Contributing

See `CONTRIBUTING.md` for contribution guidelines.

## License

See `LICENSE` file for licensing information.

## Links

- **Repository**: https://github.com/o9nn/agi-os
- **Documentation**: `documentation/`
- **Issues**: https://github.com/o9nn/agi-os/issues

## Acknowledgments

AGI-OS builds upon:
- GNU Mach microkernel
- GNU Hurd operating system
- OpenCog cognitive framework
- Numerous open-source projects and contributors

---

**AGI-OS**: Where cognition meets infrastructure, creating a truly intelligent operating system.

**Last Updated**: December 12, 2025  
**Maintainer**: OpenCog Development Team
