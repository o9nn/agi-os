# CLAUDE.md - AI Assistant Guide for OpenCog Collection (OCC)

This document provides comprehensive guidance for AI assistants working with the OpenCog Collection monorepo.

## Project Overview

The **OpenCog Collection (OCC)** is a monorepo integrating multiple OpenCog components and external tools into a unified cognitive architecture for Artificial General Intelligence (AGI) research. The project emphasizes **cognitive synergy** - the emergent intelligence arising from the interaction of diverse AI components.

### Core Philosophy

- **Cognitive Synergy by Design**: Components are designed to interact and collaborate across paradigms (symbolic reasoning, machine learning, evolutionary algorithms)
- **Reproducible Development**: Uses GNU Guix for declarative, isolated development environments
- **AGI-OS Architecture**: Three-layer cognitive architecture (Cognumach microkernel, HurdCog, OCC)

## Repository Structure

```
occ/
├── Core OpenCog Components (C++ with CMake)
│   ├── cogutil/           # Base utilities and configuration (build first)
│   ├── atomspace/         # Hypergraph database and knowledge representation
│   ├── cogserver/         # Networking and IPC layer
│   ├── matrix/            # Sparse matrix/graph processing for ML
│   ├── learn/             # Symbolic learning algorithms
│   ├── agents/            # Interactive cognitive agents framework
│   ├── sensory/           # Dataflow system for external interaction
│   ├── attention/         # Attention allocation mechanisms
│   ├── unify/             # Unification algorithms
│   ├── ure/               # Unified Rule Engine
│   ├── miner/             # Pattern mining
│   ├── pln/               # Probabilistic Logic Networks
│   └── asmoses/           # Meta-Optimizing Semantic Evolutionary Search
│
├── Storage Backends
│   ├── atomspace-storage/ # Base storage API
│   ├── atomspace-rocks/   # RocksDB backend
│   ├── atomspace-cog/     # Network storage (CogServer-based)
│   └── atomspace-pgres/   # PostgreSQL backend
│
├── Cognitive Architecture Components
│   ├── coggml/            # CogGML self-aware microkernel
│   ├── cogself/           # CogSelf AGI synergy framework
│   ├── atomspace-accelerator/ # High-performance inference engine
│   ├── agentic-chatbots/  # Conversational AI integration
│   └── synergy/           # Cognitive synergy modules (Scheme)
│
├── AGI-OS Layers
│   ├── cognumach/         # Layer 1: Cognitive microkernel
│   ├── hurdcog/           # Layer 2: Cognitive OS services
│   └── metamodel/         # System metamodel and ontology
│
├── Build System Files
│   ├── CMakeLists.txt     # Root CMake configuration
│   ├── Makefile           # AGI-OS unified build interface
│   ├── guix.scm           # GNU Guix package definition
│   ├── Cargo.toml         # Rust hyperon bindings
│   └── synergy.sh         # Unified synergy check script
│
├── CI/CD
│   └── .github/workflows/
│       ├── occ-build.yml  # Complete stack build
│       ├── debian-packages.yml
│       └── cogci.yml
│
└── Documentation
    ├── docs/              # Architecture and guides
    ├── README.md          # Project overview
    └── CONTRIBUTING.md    # Contribution guidelines
```

## Build System

### Primary Build Methods

1. **GNU Guix (Recommended for Reproducibility)**
   ```bash
   guix build -f guix.scm
   ```

2. **CMake Direct Build**
   ```bash
   mkdir build && cd build
   cmake .. -DCMAKE_BUILD_TYPE=Release
   make -j$(nproc)
   ```

3. **Makefile Interface (AGI-OS Stack)**
   ```bash
   make all          # Build complete stack
   make guix-build   # Build via Guix
   make direct-build # Build directly
   make test         # Run tests
   ```

### Build Dependency Order

Components must be built in this order due to dependencies:

1. `cogutil` - Foundation utilities
2. `atomspace` - Hypergraph database (requires cogutil)
3. `atomspace-storage` - Storage API (requires atomspace)
4. `cogserver` - Networking (requires atomspace)
5. `unify` - Unification (requires atomspace)
6. `ure` - Rule Engine (requires unify)
7. Higher-level components (miner, pln, attention, etc.)

### CMake Build Options

Key options in `CMakeLists.txt`:

```cmake
OPTION(BUILD_COGUTIL "Build CogUtil library" ON)
OPTION(BUILD_ATOMSPACE "Build AtomSpace hypergraph database" ON)
OPTION(BUILD_COGSERVER "Build CogServer networking" ON)
OPTION(BUILD_COGGML "Build CogGML self-aware microkernel" ON)
OPTION(BUILD_COGSELF "Build CogSelf AGI synergy framework" ON)
OPTION(BUILD_ATOMSPACE_ACCELERATOR "Build AtomSpace accelerator" ON)
OPTION(BUILD_AGENTIC_CHATBOTS "Build agentic chatbots integration" ON)
```

## Development Workflow

### System Dependencies (Ubuntu/Debian)

```bash
sudo apt-get install -y \
  build-essential cmake \
  libboost-all-dev \
  guile-3.0-dev \
  python3-dev cython3 \
  liboctomap-dev liboctomap-tools \
  cxxtest valgrind doxygen
```

### Using the Devcontainer

The recommended development environment uses VS Code devcontainers:

```bash
git clone --recurse-submodules https://github.com/Kaw-Aii/occ.git
# Open in VS Code with Remote Containers extension
```

### Running Synergy Checks

```bash
./synergy.sh  # Unified build and interoperability test
```

## Code Conventions

### C++ Standards

- C++17 standard required
- Position-independent code enabled
- Use CxxTest for unit tests
- Follow OpenCog coding style

### Python

- Python 3 with Cython bindings
- Use `requirements.txt` for dependencies
- NumPy, Pandas, scikit-learn for ML demos

### Scheme/Guile

- Guile 3.0 for AtomSpace scripting
- Scheme modules in `synergy/` directory
- `.scm` files for cognitive reasoning

### Rust (Hyperon Bindings)

- Minimal Rust used for `hyperon` bindings
- `cdylib` crate type for C interop
- Located in `src/lib.rs`

## Testing

### Running Tests

```bash
# Per-component tests
cd cogutil/build && make check
cd atomspace/build && make check

# Integration tests
make test-integration
./test-integration.sh
```

### CI/CD Pipeline

The GitHub Actions workflow (`occ-build.yml`) runs:
1. Build each component in dependency order
2. Run unit tests (allowed to fail with `continue-on-error`)
3. Generate build report

## Key Files for AI Assistants

When working on this codebase, pay attention to:

| File | Purpose |
|------|---------|
| `CMakeLists.txt` | Root build configuration |
| `guix.scm` | Guix package definition |
| `Makefile` | AGI-OS unified build |
| `synergy.sh` | Integration verification |
| `.github/workflows/occ-build.yml` | CI pipeline |
| `docs/architecture.md` | Architecture overview |
| `CONTRIBUTING.md` | Contribution guidelines |

## Common Tasks

### Adding a New Component

1. Create directory with `CMakeLists.txt`
2. Add `add_subdirectory()` in root `CMakeLists.txt`
3. Update `guix.scm` if needed
4. Add to CI workflow build order

### Modifying Build Configuration

- Edit `CMakeLists.txt` for CMake options
- Edit `guix.scm` for Guix package changes
- Update `Makefile` for AGI-OS integration

### Debugging Build Issues

1. Check dependency order in CI logs
2. Verify system dependencies are installed
3. Review `build/CMakeCache.txt` for configuration
4. Check component-specific `CMakeLists.txt`

## Architecture Notes

### Three-Layer AGI-OS Stack

1. **Cognumach (Layer 1)**: Cognitive microkernel
2. **HurdCog (Layer 2)**: OS services with cognitive extensions
3. **OCC (Layer 3)**: Full OpenCog cognitive architecture

### AtomSpace

The AtomSpace is the central knowledge representation system:
- Hypergraph database for symbolic AI
- Atoms = Nodes + Links with TruthValues
- Supports multiple storage backends
- Accessible via C++, Python, Scheme, and REST APIs

### Cognitive Synergy Modules

Located in `synergy/`:
- `core/` - Core synergy primitives
- `bridges/` - Inter-component bridges
- `identity/` - System identity management
- `membranes/` - Cognitive membrane patterns

## Important Conventions

1. **Never commit build artifacts** - Check `.gitignore`
2. **Test before committing** - Run `./synergy.sh`
3. **Follow semantic versioning** for releases
4. **Update documentation** when changing interfaces
5. **Use reproducible builds** via Guix when possible

## External Integrations (Optional)

These are disabled by default:
- `gnucash/` - Cognitive accounting
- `koboldcpp/` - Story/world modeling
- `aphrodite-engine/` - LLM inference

Enable with CMake flags: `-DBUILD_GNUCASH=ON`, etc.

## Resources

- [OpenCog Wiki](https://wiki.opencog.org/)
- [AtomSpace Documentation](https://wiki.opencog.org/w/AtomSpace)
- [GNU Guix Manual](https://guix.gnu.org/manual/)
- [Project Discussions](https://github.com/opencog/occ/discussions)
