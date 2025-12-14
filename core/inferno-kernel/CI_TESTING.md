# Inferno OS CI/CD and Testing

This document describes the continuous integration and testing infrastructure for the Inferno OS project.

## Overview

The project now includes comprehensive CI/CD automation and testing capabilities covering:

- **Inferno OS build system** (mk-based build with proper environment setup)
- **Python cognitive components** (distributed networking, cognitive grammar)
- **Integration testing** (end-to-end validation)
- **Multi-platform support** (Linux/386 with extensibility for other platforms)

## GitHub Actions CI/CD

### Main Workflow: `.github/workflows/ci.yml`

The CI workflow runs automatically on:
- Push to `main` or `develop` branches
- Pull requests to `main` or `develop` branches
- Manual trigger via workflow_dispatch

#### Workflow Jobs:

1. **build-inferno**: Builds the Inferno OS system
   - Sets up build environment (gcc, libraries)
   - Configures mkconfig for CI environment
   - Builds mk command using makemk.sh
   - Builds system libraries and tools
   - Verifies git state remains clean
   - Uploads build artifacts

2. **test-python**: Tests Python components
   - Runs comprehensive Python test suite (72 tests)
   - Tests cognitive grammar framework
   - Tests distributed network components
   - Tests cognitive network tools

3. **test-limbo**: Tests Limbo/Inferno components
   - Downloads build artifacts from build-inferno job
   - Sets up Inferno environment
   - Runs Inferno-specific tests

4. **integration-test**: End-to-end integration tests
   - Combines all components
   - Validates system coherence
   - Generates test reports

## Test Scripts

### `scripts/test-all.sh`

Comprehensive test runner for all components:

```bash
# Run all tests
./scripts/test-all.sh

# Run only Python tests
./scripts/test-all.sh python

# Run only build tests
./scripts/test-all.sh build
```

**Test Phases:**
1. Environment validation
2. Python component tests
3. Build system tests
4. Core build tests
5. Component-specific tests
6. Integration tests
7. System coherence tests

### `scripts/build.sh`

Inferno OS build script following the exact sequence from issue requirements:

```bash
./scripts/build.sh
```

**Build Steps:**
1. Git repository update (skipped in CI)
2. Directory structure setup
3. mkconfig configuration
4. mk command build (makemk.sh)
5. Binary restoration
6. PATH setup
7. System build (mk install)
8. Git state verification
9. Build result verification

## Test Coverage

### Python Components (72 tests)
- **Cognitive Grammar Framework**: Message creation, validation, serialization
- **Distributed Network**: Agent registration, discovery, topology management
- **Cognitive Network**: Agent coordination, capability querying, resource negotiation

### Inferno OS Components
- **Build System**: mk tool, libraries, core utilities
- **Test Utilities**: itest, ftest, testsets
- **System Integration**: EMU functionality, limbo compiler

### Integration Tests
- **Cross-component**: Python ↔ Inferno interaction
- **Git State**: Clean repository verification
- **Artifact Validation**: Binary creation and functionality

## Build Requirements

### System Dependencies
```bash
# Ubuntu/Debian
sudo apt-get install gcc libc6-dev libx11-dev libxext-dev build-essential

# Python
python3 (for cognitive components)
```

### Build Configuration
The build system automatically configures:
- `ROOT`: Project root directory
- `SYSHOST`: Linux (for CI environment)
- `OBJTYPE`: 386 (primary target architecture)

### Directory Structure
```
/Linux/386/
├── bin/        # Built binaries (mk, emu, limbo, etc.)
├── lib/        # Built libraries (lib9.a, libbio.a, etc.)
└── include/    # Header files
```

## Usage Examples

### Local Development
```bash
# Set up and build everything
./scripts/build.sh

# Run comprehensive tests
./scripts/test-all.sh

# Run only Python tests during development
./scripts/test-all.sh python
```

### CI/CD Integration
The GitHub Actions workflow automatically:
1. Builds on every push/PR
2. Runs all test suites
3. Validates git state
4. Uploads artifacts
5. Reports results

### Build Verification
```bash
# Check build artifacts
ls -la Linux/386/bin/

# Test mk command
export PATH="$PWD/Linux/386/bin:$PATH"
mk --help

# Test emu (if built)
Linux/386/bin/emu -c1 echo "test"
```

## Troubleshooting

### Common Build Issues

1. **Missing include files**: Build script automatically sets up directory structure and copies necessary include files

2. **Permission errors**: Ensure scripts are executable:
   ```bash
   chmod +x scripts/*.sh
   ```

3. **Git state not clean**: The build process should leave git repository in clean state. If not, check for unexpected file modifications.

### Test Failures

1. **Python tests**: Check Python dependencies and module imports
2. **Build tests**: Verify system dependencies are installed
3. **Integration tests**: Ensure all components built successfully

## Contributing

When adding new features:

1. **Add tests**: Include tests in appropriate test suites
2. **Update CI**: Modify CI workflow if new dependencies needed
3. **Document**: Update this README for new test procedures
4. **Verify**: Run full test suite before submitting PR

## Status Badges

The CI status can be monitored via GitHub Actions tab. Build and test status is reported for all branches and pull requests.

---

*This testing infrastructure ensures reliable builds and comprehensive validation for all Inferno OS features and components.*