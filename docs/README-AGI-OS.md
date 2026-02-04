# AGI-OS: Autonomous General Intelligence Operating System

AGI-OS is a unified, production-ready operating system that integrates OpenCog Collection (OCC), Cognumach (GNU Mach), and HurdCog (GNU Hurd) into a coherent autonomous general intelligence platform.

## Key Features

### Integrated Architecture
- **Layer 1**: Cognumach microkernel with cognitive awareness
- **Layer 2**: HurdCog operating system with cognitive extensions
- **Layer 3**: OpenCog Collection AGI framework with complete cognitive capabilities
- **Layer 0**: Unified build tools (MIG) for all layers

### Production-Ready Packaging
- Complete Debian packaging infrastructure for all components
- Correct build dependency ordering enforced at multiple levels
- Automated CI/CD via GitHub Actions
- Signed package support for secure distribution

### Cognitive Architecture
- **AtomSpace**: Hypergraph database for knowledge representation
- **CogServer**: Networking and distributed reasoning
- **Learning**: Symbolic learning and pattern mining
- **Agents**: Interactive autonomous agents
- **Attention**: Cognitive resource allocation
- **Program Synthesis**: ASMOSES for automated optimization

### System Integration
- Microkernel-based architecture for security and modularity
- Cognitive kernel extensions for self-aware system management
- Distributed execution via MachSpace
- Adaptive scheduling based on cognitive load

## Quick Start

### Prerequisites

```bash
sudo apt-get update
sudo apt-get install -y \
  cmake \
  build-essential \
  libboost-dev \
  libboost-filesystem-dev \
  libboost-system-dev \
  libboost-thread-dev \
  guile-3.0-dev \
  python3-dev \
  autotools-dev \
  autoconf \
  automake \
  libtool
```

### Building AGI-OS

#### OpenCog Collection Only (Recommended for Most Users)

```bash
git clone https://github.com/o9nn/agi-os.git
cd agi-os
./build-agi-os.sh --occ-only
```

#### Full Stack (Microkernel + OS + AGI Framework)

```bash
./build-agi-os.sh --all --prefix /opt/agi-os
```

#### Specific Layers

```bash
# Microkernel only
./build-agi-os.sh --cognumach

# Microkernel + OS
./build-agi-os.sh --hurdcog

# With custom settings
./build-agi-os.sh --all --jobs 8 --prefix /usr/local
```

### Installation

After building, set up your environment:

```bash
export PATH=/usr/local/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
```

Or install Debian packages:

```bash
cd core/cognition/foundation/cogutil
debuild -us -uc
sudo dpkg -i ../libcogutil_*.deb ../libcogutil-dev_*.deb
```

## Documentation

- **[AGI-OS Integration Architecture](AGI-OS-INTEGRATION.md)**: Detailed system design and integration points
- **[Debian Packaging Infrastructure](DEBIAN_PACKAGING.md)**: Complete packaging documentation
- **[OpenCog Implementation Roadmap](opencog_implementation_roadmap.csv)**: Development phases and milestones

## Project Structure

```
agi-os/
├── core/
│   ├── cognition/          # Cognitive architecture
│   │   ├── foundation/     # Core OpenCog components
│   │   ├── storage/        # Storage backends
│   │   ├── generation/     # Generative components
│   │   └── ...
│   ├── microkernel/        # Cognumach (Layer 1)
│   ├── os/                 # HurdCog (Layer 2)
│   └── integration/        # Integration components
├── CMakeLists.txt          # Root build configuration
├── build-agi-os.sh         # Comprehensive build script
├── .github/workflows/      # CI/CD pipelines
└── documentation/          # Project documentation
```

## Build System

AGI-OS uses a sophisticated build system that ensures correct dependency ordering:

### CMake Integration
- Root `CMakeLists.txt` orchestrates all components
- Conditional building based on layer selection
- Automatic dependency resolution

### Autotools Integration
- Cognumach and HurdCog use GNU autotools
- CMake wrapper provides seamless integration
- Custom targets for autotools-based builds

### GitHub Actions
- Automated builds on push and pull request
- Parallel job execution with dependency tracking
- Debian package generation
- Integration testing

## Critical Dependencies

The system enforces the following build order:

1. **CogUtil** - Foundation library (no dependencies)
2. **AtomSpace** - Hypergraph database (depends on CogUtil)
3. **AtomSpace Storage** - Storage backends (depends on AtomSpace)
4. **CogServer** - Networking (depends on AtomSpace + AtomSpace Storage)
5. **Cognitive Components** - Learn, Agents, Attention (depend on AtomSpace)

This ordering is enforced in:
- `CMakeLists.txt` - Conditional build checks
- `debian/control` files - Package dependencies
- GitHub Actions workflow - Job dependencies

## Performance

### Build Performance
- Parallel compilation: Use `--jobs N` for faster builds
- Incremental builds: Only rebuild changed components
- Ccache support: Automatic compiler caching

### Runtime Performance
- AtomSpace Storage: RocksDB for speed, PostgreSQL for persistence
- GPU Acceleration: AtomSpace Accelerator for inference
- Distributed Execution: MachSpace for multi-machine reasoning

## Security

- **Capability-Based Security**: Leverages Mach/Hurd security model
- **Signed Packages**: GPG-signed Debian packages for distribution
- **Cognitive Monitoring**: System behavior tracking and anomaly detection
- **Sandboxing**: Isolated cognitive component execution

## Contributing

We welcome contributions to AGI-OS. Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Ensure builds pass: `./build-agi-os.sh --occ-only`
5. Submit a pull request

## License

AGI-OS integrates multiple open-source projects with different licenses:
- **OpenCog**: AGPL-3.0
- **GNU Mach**: GPL-2.0
- **GNU Hurd**: GPL-2.0

See individual component directories for specific license information.

## Support

- **Documentation**: See [AGI-OS-INTEGRATION.md](AGI-OS-INTEGRATION.md)
- **Build Issues**: Check [DEBIAN_PACKAGING.md](DEBIAN_PACKAGING.md) troubleshooting section
- **OpenCog Wiki**: https://wiki.opencog.org/
- **GNU Hurd**: https://www.gnu.org/software/hurd/

## Roadmap

### Phase 1: Foundation (Current)
- ✅ Complete Debian packaging infrastructure
- ✅ Correct build dependency ordering
- ✅ GitHub Actions CI/CD
- ✅ Documentation

### Phase 2: Enhancement
- [ ] Python bindings for all components
- [ ] Guile/Scheme integration
- [ ] Docker containers
- [ ] Performance optimization

### Phase 3: Integration
- [ ] Distributed AtomSpace (MachSpace)
- [ ] Cognitive fusion reactor
- [ ] External LLM integration
- [ ] Self-aware system management

### Phase 4: Autonomy
- [ ] Full AGI-OS kernel
- [ ] Autonomous repair
- [ ] Self-optimization
- [ ] Meta-learning

## Acknowledgments

AGI-OS builds upon the excellent work of:
- **OpenCog Foundation**: For the cognitive architecture framework
- **GNU Project**: For Mach, Hurd, and essential tools
- **Open Source Community**: For countless libraries and tools

## Contact

For questions or suggestions, please open an issue on GitHub or contact the maintainers.

---

**AGI-OS: Towards Autonomous General Intelligence**
