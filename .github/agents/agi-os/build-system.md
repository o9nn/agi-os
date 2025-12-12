# AGI-OS Build System - Extended Definition

## Overview

The AGI-OS build system enforces correct dependency ordering across all three layers (microkernel, OS, cognition) and provides unified build scripts, Debian packaging, and comprehensive testing.

## Location

`infrastructure/build/`

## Build Scripts

### Primary Build Script

**Location**: `infrastructure/build/scripts/build-agi-os-unified.sh`  
**Symlink**: `build-agi-os.sh` (at repository root)

**Features**:
- Enforces correct build order
- Colored output for readability
- Comprehensive logging
- Build statistics
- Error handling and recovery

**Usage**:
```bash
# Basic build
./build-agi-os.sh

# Custom build directory
BUILD_DIR=/path/to/build ./build-agi-os.sh

# Custom install prefix
INSTALL_PREFIX=/opt/agi-os ./build-agi-os.sh

# Debug build
BUILD_TYPE=Debug ./build-agi-os.sh

# Parallel build with specific job count
PARALLEL_JOBS=8 ./build-agi-os.sh

# All options combined
BUILD_DIR=/tmp/build \
INSTALL_PREFIX=/opt/agi-os \
BUILD_TYPE=Debug \
PARALLEL_JOBS=8 \
./build-agi-os.sh
```

## Build Order

The build system enforces this strict dependency order:

```
Layer 0: Microkernel
  1. cognumach (core/microkernel/)

Layer 1: Foundation
  2. cogutil (core/cognition/foundation/cogutil/)
  3. atomspace (core/cognition/foundation/atomspace/)

Layer 2: Storage ⭐ CRITICAL
  4. atomspace-storage (core/cognition/storage/atomspace-storage/)
  5. atomspace-cog (core/cognition/storage/backends/cog/)
  6. atomspace-rocks (core/cognition/storage/backends/rocks/)
  7. atomspace-pgres (core/cognition/storage/backends/postgres/)

Layer 3: Network Services
  8. cogserver (core/cognition/network/cogserver/)
  9. ure (core/cognition/reasoning/ure/)

Layer 4: Reasoning
  10. pln (core/cognition/reasoning/pln/)
  11. unify (core/cognition/reasoning/unify/)
  12. spacetime (core/cognition/reasoning/spacetime/)

Layer 5: Attention
  13. attention/ecan (core/cognition/attention/ecan/)

Layer 6: Learning
  14. learn (core/cognition/learning/learn/)
  15. miner (core/cognition/learning/miner/)
  16. moses (core/cognition/learning/moses/)
  17. asmoses (core/cognition/learning/asmoses/)

Layer 7: Generation
  18. generate (core/cognition/generation/generate/)

Layer 8: Language
  19. link-grammar (core/cognition/language/link-grammar/)
  20. lg-atomese (core/cognition/language/lg-atomese/)
  21. relex (core/cognition/language/relex/)

Layer 9: Perception
  22. vision (core/cognition/perception/vision/)

Layer 10: Specialized
  23. agi-bio (core/cognition/specialized/agi-bio/)

Layer 11: Integration
  24. cognitive-grip (core/integration/cognitive-grip/)
```

## Critical Build Constraint

**⚠️ CRITICAL**: `atomspace-storage` (Layer 2, #4) MUST be built before `cogserver` (Layer 3, #8).

This is enforced in all build scripts. Violating this order will cause build failures.

## Build Script Structure

```bash
#!/bin/bash
# build-agi-os-unified.sh

# Configuration
AGI_OS_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
BUILD_DIR="${BUILD_DIR:-$AGI_OS_ROOT/build}"
INSTALL_PREFIX="${INSTALL_PREFIX:-/usr/local}"
PARALLEL_JOBS="${PARALLEL_JOBS:-$(nproc)}"
BUILD_TYPE="${BUILD_TYPE:-Release}"

# Logging
LOG_DIR="$BUILD_DIR/logs"
mkdir -p "$LOG_DIR"

# Build function
build_component() {
    local name=$1
    local path=$2
    local layer=$3
    
    log INFO "Building $name (Layer $layer)"
    
    local component_build_dir="$BUILD_DIR/$name"
    mkdir -p "$component_build_dir"
    cd "$component_build_dir"
    
    # CMake configuration
    cmake "$AGI_OS_ROOT/$path" \
        -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
        -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX
    
    # Build
    cmake --build . -j$PARALLEL_JOBS
    
    # Install
    sudo cmake --install .
}

# Build all components in order
build_component "cognumach" "core/microkernel" "0"
build_component "cogutil" "core/cognition/foundation/cogutil" "1"
build_component "atomspace" "core/cognition/foundation/atomspace" "1"
build_component "atomspace-storage" "core/cognition/storage/atomspace-storage" "2"
# ... etc
```

## Debian Packaging

### Location

`infrastructure/packaging/debian/`

### Package Structure

Each component has its own Debian package:

```
infrastructure/packaging/debian/
├── cogutil/
│   └── debian/
│       ├── control
│       ├── rules
│       ├── changelog
│       └── compat
├── atomspace/
│   └── debian/
│       ├── control
│       ├── rules
│       ├── changelog
│       └── compat
├── atomspace-storage/
│   └── debian/
│       ├── control (depends on atomspace)
│       ├── rules
│       ├── changelog
│       └── compat
├── cogserver/
│   └── debian/
│       ├── control (depends on atomspace-storage) ⭐
│       ├── rules
│       ├── changelog
│       └── compat
└── ... (35+ packages total)
```

### Building Debian Packages

```bash
# Build single package
cd infrastructure/packaging/debian/cogutil
dpkg-buildpackage -us -uc -b

# Build all packages
cd infrastructure/packaging/debian
./build-all-packages.sh

# Install package
sudo dpkg -i ../opencog-cogutil_*.deb
```

### Package Dependencies

Example `debian/control` for cogserver:

```
Source: opencog-cogserver
Section: science
Priority: optional
Maintainer: OpenCog Developers <opencog@googlegroups.com>
Build-Depends: debhelper (>= 10),
               cmake (>= 3.10),
               libboost-all-dev,
               opencog-atomspace-dev,
               opencog-atomspace-storage-dev
Standards-Version: 4.1.3

Package: opencog-cogserver
Architecture: any
Depends: ${shlibs:Depends},
         ${misc:Depends},
         opencog-atomspace,
         opencog-atomspace-storage
Description: OpenCog CogServer - Network server for distributed cognition
```

## CMake Configuration

### Standard CMake Options

```bash
cmake .. \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=/usr/local \
  -DCMAKE_PREFIX_PATH=/usr/local \
  -DBUILD_SHARED_LIBS=ON \
  -DBUILD_TESTING=ON
```

### Component-Specific Options

#### CogNumach
```bash
cmake .. \
  -DENABLE_COGNITIVE_SCHEDULER=ON \
  -DENABLE_ATOMSPACE_IPC=ON \
  -DENABLE_COGNITIVE_VM=ON
```

#### AtomSpace
```bash
cmake .. \
  -DENABLE_PYTHON_BINDINGS=ON \
  -DENABLE_GUILE_BINDINGS=ON \
  -DBUILD_TESTS=ON
```

#### CogServer
```bash
cmake .. \
  -DENABLE_MODULES=ON \
  -DENABLE_SECURITY=ON \
  -DMAX_CLIENTS=100
```

## Build Logs

Build logs are stored in `$BUILD_DIR/logs/`:

```
build/logs/
├── build-20251212-001234.log  # Main build log
├── cognumach-20251212-001235.log
├── cogutil-20251212-001240.log
├── atomspace-20251212-001245.log
├── atomspace-storage-20251212-001250.log
├── cogserver-20251212-001255.log
└── ... (one log per component)
```

## Error Handling

The build script handles errors gracefully:

1. **CMake Configuration Failure**: Logs error and continues to next component
2. **Build Failure**: Logs error and continues to next component
3. **Installation Failure**: Logs error and continues to next component
4. **Summary**: Reports all failures at the end

Example error handling:

```bash
if ! cmake "$AGI_OS_ROOT/$path" >> "$component_log" 2>&1; then
    log ERROR "CMake configuration failed for $name"
    FAILED_COMPONENTS+=("$name")
    return 1
fi
```

## Build Statistics

At the end of the build, statistics are displayed:

```
==========================================
Build Summary
==========================================
Total components: 24
Successfully built: 22
Failed: 2
Build time: 1h 23m 45s

Failed components:
  - vision
  - agi-bio

Build logs available in: /home/ubuntu/agi-os/build/logs
```

## Testing

### Unit Tests

Each component has its own unit tests:

```bash
cd core/cognition/foundation/cogutil/build
make test
```

### Integration Tests

Integration tests verify component interactions:

```bash
cd infrastructure/testing/integration
./run-tests.sh
```

### Build Tests

Test the build system itself:

```bash
cd infrastructure/testing/build
./test-build-order.sh
./test-dependencies.sh
```

## Continuous Integration

GitHub Actions workflows for automated building:

```yaml
# .github/workflows/build.yml
name: Build AGI-OS

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install dependencies
        run: sudo apt-get install -y build-essential cmake libboost-all-dev
      - name: Build
        run: ./build-agi-os.sh
      - name: Test
        run: cd infrastructure/testing && ./run-tests.sh
```

## Troubleshooting

### Common Build Issues

1. **Missing Dependencies**
   ```bash
   sudo apt-get build-dep opencog-atomspace
   ```

2. **Wrong Build Order**
   - Always use `build-agi-os.sh` to ensure correct order
   - Never build cogserver before atomspace-storage

3. **CMake Cache Issues**
   ```bash
   rm -rf build/
   ./build-agi-os.sh
   ```

4. **Permission Issues**
   ```bash
   sudo chown -R $USER build/
   ```

### Debug Build

For debugging build issues:

```bash
BUILD_TYPE=Debug \
CMAKE_VERBOSE_MAKEFILE=ON \
./build-agi-os.sh
```

## Advanced Usage

### Partial Build

Build only specific layers:

```bash
# Build only foundation layer
./build-agi-os.sh --layers foundation

# Build up to storage layer
./build-agi-os.sh --up-to storage

# Build from reasoning layer onwards
./build-agi-os.sh --from reasoning
```

### Parallel Build

Maximize build speed:

```bash
# Use all CPU cores
PARALLEL_JOBS=$(nproc) ./build-agi-os.sh

# Use specific number of cores
PARALLEL_JOBS=8 ./build-agi-os.sh
```

### Clean Build

Remove all build artifacts:

```bash
rm -rf build/
./build-agi-os.sh
```

## References

- CMake Documentation: https://cmake.org/documentation/
- Debian Packaging: https://www.debian.org/doc/manuals/maint-guide/
- Build System: `infrastructure/build/`
- Packaging: `infrastructure/packaging/debian/`
