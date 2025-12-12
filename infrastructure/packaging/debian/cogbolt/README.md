# CogBolt Debian Package

## Overview

This directory contains the Debian packaging files for CogBolt, the AI-Powered IDE Core component of AGI-OS.

## Package Information

**Package Name**: cogbolt  
**Version**: 1.0.0  
**Architecture**: any (C++ compiled binary)  
**Section**: devel  
**Priority**: optional

## Packages Provided

### cogbolt
The main package containing the CogBolt runtime libraries and executables.

**Provides**:
- CogBolt core libraries
- Command-line tools
- Runtime dependencies

**Dependencies**:
- libstdc++6 (>= 10)
- Standard C++ runtime

**Recommends**:
- libcurl4 (HTTP client support)
- libjsoncpp25 (JSON parsing)
- libglfw3 (GUI support)
- libimgui-dev (GUI framework)
- cognitive-grip (AGI-OS integration)
- atomspace (OpenCog integration)

**Suggests**:
- cogserver (Network services)
- opencog-pln (Reasoning capabilities)
- opencog-ure (Rule engine)

### cogbolt-dev
Development files for building applications with CogBolt.

**Provides**:
- Header files
- Static libraries
- CMake configuration files
- pkg-config files

### cogbolt-doc
Documentation package.

**Provides**:
- API documentation
- User guides
- Integration tutorials
- Example code

## Building the Package

### Prerequisites

```bash
sudo apt-get install debhelper cmake g++ libstdc++-10-dev
sudo apt-get install libcurl4-openssl-dev libjsoncpp-dev libglfw3-dev
sudo apt-get install libgl1-mesa-dev libimgui-dev zlib1g-dev
```

### Build Commands

```bash
# From the cogbolt source directory
cd /path/to/agi-os/cogbolt

# Build the package
dpkg-buildpackage -us -uc -b

# Or use the unified build script
cd /path/to/agi-os/infrastructure/packaging/debian
./build-all-packages.sh cogbolt
```

### Build Output

The build process creates the following packages:
- `cogbolt_1.0.0-1_amd64.deb` - Main package
- `cogbolt-dev_1.0.0-1_amd64.deb` - Development files
- `cogbolt-doc_1.0.0-1_all.deb` - Documentation

## Installation

### From Built Packages

```bash
# Install main package
sudo dpkg -i cogbolt_1.0.0-1_amd64.deb

# Install dependencies if needed
sudo apt-get install -f

# Install development files (optional)
sudo dpkg -i cogbolt-dev_1.0.0-1_amd64.deb

# Install documentation (optional)
sudo dpkg -i cogbolt-doc_1.0.0-1_all.deb
```

### From Repository (Future)

```bash
sudo apt-get update
sudo apt-get install cogbolt
```

## Integration with AGI-OS

CogBolt integrates with AGI-OS through the Cognitive-Grip layer, providing:

**AtomSpace Integration**: Code representations stored in hypergraph database for cognitive processing and semantic queries.

**PLN Reasoning**: Advanced code analysis using Probabilistic Logic Networks for bug detection, complexity analysis, and optimization suggestions.

**Pattern Mining**: Automatic discovery of code patterns and best practices from large codebases using the OpenCog Miner component.

**ECAN Attention**: Attention-based code navigation and resource allocation using Economic Attention Networks.

**CogServer Integration**: Real-time collaborative editing and network-based cognitive services through the OpenCog server infrastructure.

## Configuration

### System-wide Configuration

```bash
# Configuration file location
/etc/cogbolt/cogbolt.conf

# User configuration
~/.config/cogbolt/config.json
```

### Environment Variables

```bash
# Enable AtomSpace integration
export COGBOLT_ATOMSPACE_ENABLED=1

# Set AtomSpace server
export COGBOLT_ATOMSPACE_SERVER=localhost:17001

# Enable cognitive features
export COGBOLT_COGNITIVE_FEATURES=pln,ecan,miner
```

## Usage Examples

### Standalone Mode

```bash
# Launch CogBolt IDE
cogbolt

# Open specific file
cogbolt /path/to/file.cpp

# With AI completion enabled
cogbolt --ai-completion /path/to/file.cpp
```

### Integrated with AGI-OS

```cpp
#include <cognitive_grip_enhanced.h>
#include <cogbolt/editor.h>

using namespace agi_os::cognitive_grip;

int main() {
    // Initialize Cognitive-Grip
    auto& grip = getCognitiveGrip();
    grip.initialize({});

    // Register code for cognitive processing
    auto code_handle = grip.registerCodeRepresentation(
        "example.cpp",
        read_file("example.cpp")
    );

    // Get AI-powered completions
    auto completions = grip.getCodeCompletions(
        current_context,
        cursor_position
    );

    // Analyze code using PLN
    auto analysis = grip.analyzeCode(
        code_handle,
        "complexity"
    );

    return 0;
}
```

## Testing

### Unit Tests

```bash
# Run CogBolt unit tests
cogbolt-test

# Run integration tests
cogbolt-test --integration
```

### Integration Testing

```bash
# Test with AtomSpace
cogbolt-test --atomspace

# Test with Cognitive-Grip
cogbolt-test --cognitive-grip

# Full integration test
cogbolt-test --full-integration
```

## Troubleshooting

### Common Issues

**Issue**: Package fails to install due to missing dependencies  
**Solution**: Run `sudo apt-get install -f` to install missing dependencies

**Issue**: CogBolt fails to connect to AtomSpace  
**Solution**: Ensure CogServer is running and COGBOLT_ATOMSPACE_SERVER is set correctly

**Issue**: AI features not working  
**Solution**: Check that GGML models are installed in `/usr/share/cogbolt/models/`

### Debug Mode

```bash
# Enable debug logging
export COGBOLT_DEBUG=1
cogbolt --verbose
```

## Contributing

See the main AGI-OS repository for contribution guidelines:
https://github.com/o9nn/agi-os

## License

CogBolt is licensed under the GNU General Public License v3.0 or later.
See `/usr/share/doc/cogbolt/copyright` for full license text.

## Support

For issues and questions:
- GitHub Issues: https://github.com/o9nn/agi-os/issues
- Documentation: https://agi-os.org/docs/cogbolt
- Mailing List: dev@agi-os.org

## Changelog

See `/usr/share/doc/cogbolt/changelog.Debian.gz` for detailed changelog.

---

**Last Updated**: December 12, 2025  
**Package Maintainer**: AGI-OS Development Team
