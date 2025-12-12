# HurdCog Operating System - Extended Definition

## Overview

HurdCog is an OpenCog-powered GNU Hurd operating system that integrates cognitive capabilities at the OS level. It features cognitive translators, AtomSpace-backed system state, and attention-based resource allocation.

## Location

`core/os/`

## Architecture

### Directory Structure

```
core/os/
├── servers/        # Core Hurd servers
│   ├── auth/      # Authentication server
│   ├── proc/      # Process server
│   ├── exec/      # Execution server
│   ├── init/      # Init server
│   └── boot/      # Boot server
├── translators/    # Hurd translators
│   ├── ext2fs/    # Ext2 filesystem
│   ├── tmpfs/     # Temporary filesystem
│   ├── nfs/       # Network filesystem
│   ├── pfinet/    # TCP/IP stack
│   └── cognitive/ # Cognitive translators
├── libraries/      # OS libraries
│   ├── libdiskfs/ # Disk filesystem library
│   ├── libnetfs/  # Network filesystem library
│   ├── libports/  # Port management library
│   └── libcog/    # Cognitive library
├── cognitive/      # Cognitive extensions
│   ├── cogkernel/ # Cognitive kernel
│   ├── agents/    # Cognitive agents
│   └── mach-integration/ # Mach integration
└── mig -> ../microkernel/mig  # Symlink to MIG
```

## Cognitive Features

### 1. Cognitive Translators

**Location**: `core/os/translators/cognitive/`

Translators that expose AtomSpace through filesystem interface:

- **AtomFS**: Filesystem view of AtomSpace
- **CogNet**: Network translator with cognitive routing
- **SemanticFS**: Semantic search over filesystem
- **AttentionFS**: Attention-based file prioritization

**Key Translators**:
- `atomfs` - Mount AtomSpace as filesystem
- `cognet` - Cognitive network routing
- `semanticfs` - Semantic file search
- `attentionfs` - Attention-based file access

### 2. Cognitive Kernel

**Location**: `core/os/cognitive/cogkernel/`

Kernel-level cognitive integration:

- **AtomSpace Integration**: Direct AtomSpace access from kernel
- **Attention Management**: System-wide attention allocation
- **Reasoning Engine**: Kernel-level reasoning capabilities
- **Pattern Mining**: Automatic pattern discovery in system behavior

**Key Components**:
- `atomspace/` - AtomSpace integration
- `attention/` - Attention mechanisms
- `reasoning/` - Reasoning systems
- `learning/` - Learning from system behavior

### 3. Cognitive Agents

**Location**: `core/os/cognitive/agents/`

Autonomous agents for system management:

- **Resource Manager**: Cognitive resource allocation
- **Security Agent**: Cognitive security monitoring
- **Performance Agent**: Performance optimization
- **Adaptation Agent**: System self-adaptation

**Key Agents**:
- `resource_manager` - Allocate resources based on attention
- `security_agent` - Monitor and respond to security threats
- `performance_agent` - Optimize system performance
- `adaptation_agent` - Adapt system to changing conditions

## Build Configuration

### CMake Options

```cmake
option(ENABLE_COGNITIVE_TRANSLATORS "Enable cognitive translators" ON)
option(ENABLE_COGKERNEL "Enable cognitive kernel" ON)
option(ENABLE_COGNITIVE_AGENTS "Enable cognitive agents" ON)
option(BUILD_ATOMFS "Build AtomFS translator" ON)
option(BUILD_SEMANTICFS "Build SemanticFS translator" ON)
```

### Build Commands

```bash
cd core/os
mkdir build && cd build
cmake .. \
  -DENABLE_COGNITIVE_TRANSLATORS=ON \
  -DENABLE_COGKERNEL=ON \
  -DENABLE_COGNITIVE_AGENTS=ON
make -j$(nproc)
sudo make install
```

## Dependencies

- **GNU Hurd**: Base operating system
- **CogNumach**: Cognitive microkernel (from `core/microkernel/`)
- **AtomSpace**: Knowledge representation (from `core/cognition/foundation/atomspace`)
- **CogServer**: Network services (from `core/cognition/network/cogserver`)

## Integration Points

### With CogNumach (Layer 0)

- **Mach IPC**: Communication with microkernel
- **Port Rights**: Receive port rights for system services
- **Memory Sharing**: Shared memory for efficiency

### With OpenCog (Layer 2)

- **AtomSpace Access**: Store system state in AtomSpace
- **Reasoning**: Use PLN for system decisions
- **Learning**: Learn from system behavior patterns
- **Attention**: Use ECAN for resource prioritization

### With Cognitive-Grip (Layer 3)

- **HurdCog Bridge**: Unified interface to OS services
- **Configuration**: Central configuration management
- **Monitoring**: System monitoring and introspection

## Cognitive Translators

### AtomFS

Mount AtomSpace as a filesystem:

```bash
# Mount AtomSpace
settrans -c /atomspace /hurd/atomfs

# Browse AtomSpace
cd /atomspace
ls nodes/
ls links/

# Query AtomSpace
cat /atomspace/query/concept/AI
```

### SemanticFS

Semantic search over filesystem:

```bash
# Mount semantic filesystem
settrans -c /semantic /hurd/semanticfs /home

# Search by concept
cat /semantic/search/concept/documentation

# Search by similarity
cat /semantic/similar/README.md
```

### AttentionFS

Attention-based file access:

```bash
# Mount attention filesystem
settrans -c /attention /hurd/attentionfs /home

# View high-attention files
ls /attention/high/

# View low-attention files
ls /attention/low/

# Set file attention
echo "100" > /attention/set/important_file.txt
```

## Cognitive Kernel Features

### AtomSpace Integration

```c
// Access AtomSpace from kernel
#include <hurd/cogkernel/atomspace.h>

Handle h = atomspace_add_node(CONCEPT_NODE, "SystemState");
atomspace_set_tv(h, 0.9, 0.8);
```

### Attention Management

```c
// Allocate attention to process
#include <hurd/cogkernel/attention.h>

pid_t pid = getpid();
attention_allocate(pid, 100.0); // Allocate 100 STI
```

### Reasoning

```c
// Perform reasoning in kernel
#include <hurd/cogkernel/reasoning.h>

Handle conclusion = pln_infer(premises, rule);
```

## Cognitive Agents

### Resource Manager

Allocates resources based on attention values:

```bash
# Start resource manager
/hurd/agents/resource_manager &

# View resource allocation
cat /proc/cognitive/resources

# Set resource policy
echo "attention_based" > /proc/cognitive/resource_policy
```

### Security Agent

Monitors system security using cognitive patterns:

```bash
# Start security agent
/hurd/agents/security_agent &

# View security status
cat /proc/cognitive/security

# View detected threats
cat /proc/cognitive/threats
```

## Configuration

### Cognitive Kernel Configuration

```bash
# /etc/hurd/cogkernel.conf
atomspace_backend=rocksdb
atomspace_path=/var/lib/atomspace
attention_update_interval=100
enable_reasoning=true
enable_learning=true
```

### Translator Configuration

```bash
# /etc/hurd/translators.conf
atomfs_mount=/atomspace
semanticfs_mount=/semantic
attentionfs_mount=/attention
```

## Debugging

### Cognitive Kernel Debugging

```bash
# Enable cogkernel debugging
echo "1" > /proc/sys/hurd/cogkernel_debug

# View cogkernel stats
cat /proc/cognitive/stats

# View AtomSpace stats
cat /proc/cognitive/atomspace
```

### Translator Debugging

```bash
# Debug AtomFS
/hurd/atomfs --debug /atomspace

# Debug SemanticFS
/hurd/semanticfs --debug /semantic /home
```

## Testing

### Unit Tests

```bash
cd core/os/tests
make test
```

### Integration Tests

```bash
cd infrastructure/testing/os
./test-cognitive-translators.sh
./test-cogkernel.sh
./test-cognitive-agents.sh
```

## Performance

- **Translator Overhead**: ~5-10% overhead for cognitive translators
- **Cogkernel Overhead**: ~3-5% overhead for cognitive kernel features
- **Agent Overhead**: ~2-3% overhead per active agent
- **AtomSpace Access**: ~100μs for typical operations

## Known Issues

1. **AtomFS Performance**: Can be slow for large AtomSpaces; use caching
2. **SemanticFS Latency**: Semantic search can be slow; consider indexing
3. **Agent Resource Usage**: Agents can consume significant resources; monitor usage

## Future Enhancements

- **Distributed HurdCog**: Extend across multiple machines
- **GPU Acceleration**: Accelerate cognitive operations with GPU
- **Quantum Translators**: Quantum computing translators
- **Neural Translators**: Neural network-based translators

## References

- GNU Hurd Documentation: https://www.gnu.org/software/hurd/
- Hurd Translators: https://www.gnu.org/software/hurd/hurd/translator.html
- AtomSpace: `core/cognition/foundation/atomspace/`
- Cognitive-Grip: `core/integration/cognitive-grip/`
