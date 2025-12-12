# AGI-OS Debian Packaging Infrastructure

This document describes the complete Debian packaging infrastructure for AGI-OS, ensuring production-ready package distribution for all core components.

## Package Structure

The AGI-OS project provides Debian packages for all major components in the correct build dependency order:

### Layer 1: Foundation Packages

**libcogutil** and **libcogutil-dev** provide the foundational utility library with no external dependencies beyond standard development tools.

**libatomspace** and **libatomspace-dev** provide the hypergraph database that depends on CogUtil and standard system libraries.

### Layer 2: Storage and Server Packages

**libatomspace-storage** and **libatomspace-storage-dev** provide pluggable storage backends for AtomSpace, including RocksDB and PostgreSQL support. This package is critical for CogServer functionality.

**cogserver**, **libcogserver**, and **libcogserver-dev** provide the networking and server infrastructure. CogServer explicitly requires AtomSpace Storage as a build dependency, ensuring the correct build order.

### Layer 3: Cognitive Component Packages

**libopencog-learn** and **libopencog-learn-dev** provide symbolic learning capabilities.

**libopencog-agents** and **libopencog-agents-dev** provide interactive agent capabilities.

**libopencog-attention** and **libopencog-attention-dev** provide attention mechanisms.

## Critical Dependency Chain

The build system enforces the following dependency order:

1. CogUtil (no dependencies)
2. AtomSpace (depends on CogUtil)
3. AtomSpace Storage (depends on AtomSpace)
4. CogServer (depends on AtomSpace AND AtomSpace Storage)
5. Cognitive components (Learn, Agents, Attention - depend on AtomSpace)

This ordering is enforced in three places:

- **CMakeLists.txt**: The root CMakeLists.txt includes conditional checks ensuring AtomSpace Storage is built before CogServer
- **Debian control files**: Each package lists its dependencies explicitly
- **GitHub Actions workflow**: The CI/CD pipeline builds components in the correct order with explicit job dependencies

## Building Debian Packages

### Prerequisites

Install the required build tools:

```bash
sudo apt-get install -y debhelper cmake build-essential
```

### Building Individual Packages

To build a specific package:

```bash
cd core/cognition/foundation/cogutil
debuild -us -uc
```

The `-us` flag skips signing, and `-uc` skips changelog signing. For production builds, remove these flags to sign packages.

### Building All Packages

Use the provided build script:

```bash
./build-agi-os.sh --occ-only
```

This builds all OpenCog Collection packages in the correct dependency order.

## Package Installation

Install packages in dependency order:

```bash
sudo dpkg -i libcogutil_*.deb libcogutil-dev_*.deb
sudo dpkg -i libatomspace_*.deb libatomspace-dev_*.deb
sudo dpkg -i libatomspace-storage_*.deb libatomspace-storage-dev_*.deb
sudo dpkg -i cogserver_*.deb libcogserver_*.deb libcogserver-dev_*.deb
```

Or use apt to handle dependencies automatically:

```bash
sudo apt-get install ./libcogutil_*.deb ./libatomspace_*.deb ./libatomspace-storage_*.deb ./cogserver_*.deb
```

## Debian Control File Format

Each component includes a `debian/control` file specifying:

- **Source**: The source package name
- **Section**: Package category (libs, net, etc.)
- **Priority**: Installation priority
- **Maintainer**: Package maintainer contact
- **Build-Depends**: Packages required for building
- **Standards-Version**: Debian policy version
- **Homepage**: Project homepage
- **Vcs-Git** and **Vcs-Browser**: Version control information

Each source package typically produces multiple binary packages:
- Runtime libraries (e.g., libcogutil)
- Development headers (e.g., libcogutil-dev)
- Executables (e.g., cogserver)

## Debian Rules File

The `debian/rules` file controls the build process. AGI-OS uses CMake for building:

```makefile
%:
	dh $@ -Scmake

override_dh_auto_configure:
	dh_auto_configure -- \
		-DCMAKE_BUILD_TYPE=Release \
		-DCMAKE_INSTALL_LIBDIR=lib/$(DEB_HOST_MULTIARCH) \
		-DCMAKE_INSTALL_INCLUDEDIR=include
```

This ensures consistent build configuration across all packages.

## Version Management

Each component maintains its own version in the `debian/changelog` file. The format follows Debian conventions:

```
package-name (version-revision) distribution; urgency=level

  * Change description

 -- Maintainer Name <email@example.com>  Day, DD Mon YYYY HH:MM:SS +ZZZZ
```

## Continuous Integration

The GitHub Actions workflow in `.github/workflows/agi-os-build.yml` automatically:

1. Builds each component in dependency order
2. Caches build artifacts for efficiency
3. Installs components progressively
4. Generates Debian packages
5. Runs integration tests

## Production Deployment

For production deployments:

1. Build signed packages: `debuild -sa` (with GPG key configured)
2. Create a Debian repository using `reprepro` or similar
3. Distribute packages via APT repository
4. Use `apt-get install` for end-user installation

## Troubleshooting

### Missing Dependencies

If a package fails to build due to missing dependencies, check the `Build-Depends` field in the control file and install required packages:

```bash
sudo apt-get build-dep ./core/cognition/foundation/cogutil
```

### Build Failures

Check the build log for specific errors. Common issues include:

- Missing development headers: Install `-dev` packages for dependencies
- CMake configuration errors: Ensure CMake version >= 3.12
- Compiler errors: Use GCC 11+ for C++17 support

### Installation Issues

If installation fails due to dependency conflicts, use:

```bash
sudo apt-get install -f
```

This attempts to fix broken dependencies.

## Future Enhancements

The Debian packaging infrastructure is designed to be extensible:

- Additional storage backends can be added to atomspace-storage
- New cognitive components can be packaged following the same pattern
- Cognumach and HurdCog packages can be added when their build systems are finalized
- Python bindings can be packaged separately as python3-opencog-*

## References

- [Debian Policy Manual](https://www.debian.org/doc/debian-policy/)
- [Debian New Maintainers' Guide](https://www.debian.org/doc/manuals/maint-guide/)
- [CMake Debian Package Integration](https://cmake.org/cmake/help/latest/cpack_gen/deb.html)
