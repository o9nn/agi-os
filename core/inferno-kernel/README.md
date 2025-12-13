# Inferno Kernel - AGI-OS Integration

## Overview

This directory contains the Inferno kernel components for AGI-OS, implementing cognitive processing as a fundamental kernel service using the 9P file protocol and Dis virtual machine.

## Architecture

### Layer 0: Dis Virtual Machine

The Dis VM is a bytecode interpreter for the Limbo programming language. It provides:

- **Bytecode Execution**: Interprets Dis bytecode
- **Memory Management**: Heap and stack management
- **Garbage Collection**: Automatic memory reclamation
- **Module Loading**: Dynamic module loading
- **Type Safety**: Strong type system enforcement

**Components**:
- `dis-vm/vm-core.c` - Core VM implementation
- `dis-vm/bytecode-loader.c` - Bytecode loading and validation
- `dis-vm/instruction-dispatch.c` - Instruction dispatcher
- `dis-vm/memory-manager.c` - Memory management
- `dis-vm/garbage-collector.c` - Garbage collector
- `dis-vm/module-loader.c` - Module loader
- `dis-vm/type-system.c` - Type system

### Layer 1: 9P Protocol Stack

The 9P protocol provides distributed file access for cognitive operations. It enables:

- **Network Transparency**: Access remote resources as local files
- **Namespace Operations**: Cognitive operations as file operations
- **Distributed Access**: Multiple clients accessing shared cognitive state
- **Standard Interface**: Unix-like file operations

**Components**:
- `9p-protocol/9p-server.c` - 9P server implementation
- `9p-protocol/9p-client.c` - 9P client implementation
- `9p-protocol/9p-messages.c` - Message handling
- `9p-protocol/9p-marshal.c` - Message marshaling
- `9p-protocol/9p-unmarshal.c` - Message unmarshaling
- `9p-protocol/styx-protocol.c` - Styx protocol (9P variant)
- `9p-protocol/file-operations.c` - File operations
- `9p-protocol/directory-operations.c` - Directory operations

### Layer 2: Limbo Language Runtime

Limbo is a concurrent programming language designed for Inferno OS. The runtime provides:

- **Concurrent Execution**: Lightweight threads
- **Channel Communication**: CSP-style channels
- **Module System**: Dynamic module loading
- **Type Safety**: Compile-time and runtime type checking

**Status**: Stub implementation (to be completed)

### Layer 3: Namespace Management Core

The namespace core manages the cognitive namespace, providing:

- **Namespace Hierarchy**: Tree-structured namespace
- **Mount Points**: Dynamic mounting of cognitive resources
- **Path Resolution**: Efficient path lookup
- **Access Control**: Permission management

**Status**: Stub implementation (to be completed)

### Layer 4: Cognitive Scheduler

The cognitive scheduler implements attention-based process scheduling:

- **Attention-Based Scheduling**: Processes scheduled by attention values
- **Priority Management**: Dynamic priority adjustment
- **Resource Allocation**: CPU and memory allocation based on importance
- **Cognitive Load Balancing**: Distribute cognitive load across resources

**Status**: Stub implementation (to be completed)

## Building

### Prerequisites

- CMake 3.12 or later
- GCC or Clang with C11 support
- pthread library

### Build Instructions

```bash
# From AGI-OS root directory
mkdir -p build
cd build
cmake .. -DBUILD_INFERNO_KERNEL=ON
make -j$(nproc)
sudo make install
```

### Build Options

- `BUILD_DIS_VM` - Build Dis Virtual Machine (default: ON)
- `BUILD_9P_PROTOCOL` - Build 9P Protocol Stack (default: ON)
- `BUILD_LIMBO_RUNTIME` - Build Limbo Language Runtime (default: ON)
- `BUILD_NAMESPACE_CORE` - Build Namespace Management Core (default: ON)
- `BUILD_COGNITIVE_SCHEDULER` - Build Cognitive Scheduler (default: ON)

## Usage

### Initializing the Dis VM

```c
#include <inferno-kernel/dis-vm.h>

int main() {
    // Initialize VM
    if (dis_vm_init() != 0) {
        fprintf(stderr, "Failed to initialize Dis VM\n");
        return 1;
    }
    
    // Execute bytecode
    uint8_t bytecode[] = { /* ... */ };
    dis_vm_execute(bytecode, sizeof(bytecode));
    
    // Shutdown VM
    dis_vm_shutdown();
    
    return 0;
}
```

### Using 9P Protocol

```c
#include <inferno-kernel/9p-protocol.h>

// Start 9P server
int server_fd = start_9p_server(9999);

// Client connects and performs operations
// Operations are mapped to cognitive functions
```

## Integration with OpenCog

### AtomSpace as 9P File Servers

Each atom in the AtomSpace hypergraph is exposed as a 9P file server:

```
/mnt/atomspace/
  ├── nodes/
  │   ├── ConceptNode/
  │   │   ├── cat/
  │   │   │   ├── ctl              # Control file
  │   │   │   ├── tv               # Truth value
  │   │   │   ├── attention        # Attention value
  │   │   │   └── links            # Incoming/outgoing links
  │   │   └── mammal/
  │   └── PredicateNode/
  ├── links/
  │   ├── InheritanceLink/
  │   │   ├── cat-mammal/
  │   │   │   ├── ctl
  │   │   │   ├── tv
  │   │   │   ├── outgoing
  │   │   │   └── incoming
  │   └── EvaluationLink/
  └── active/                      # Currently active atoms
```

### Cognitive Operations as File Operations

| Cognitive Operation | 9P Operation | Example |
|---------------------|--------------|---------|
| Create atom | `mkdir` | `mkdir /mnt/atomspace/nodes/ConceptNode/cat` |
| Read truth value | `read` | `cat /mnt/atomspace/nodes/ConceptNode/cat/tv` |
| Update attention | `write` | `echo "100" > /mnt/atomspace/nodes/ConceptNode/cat/attention` |
| Pattern match | `glob` | `ls /mnt/atomspace/links/InheritanceLink/*-mammal` |
| Deduction | `walk` | Walk A→B, B→C, create A→C |

### PLN Reasoning as Namespace Operations

PLN reasoning operations are implemented as namespace operations:

- **Deduction**: Path composition (A→B, B→C ⇒ A→C)
- **Induction**: Reverse mounting (A→B ⇒ B→A)
- **Abduction**: Link creation (B→C, A→C ⇒ A→B)
- **Revision**: File write (update truth values)

### Attention as Mount Points

ECAN attention mechanism is implemented as dynamic namespace mounting:

```
/mnt/attention/
  ├── active/                      # STI > threshold (mounted, "hot")
  │   ├── node7 -> /mnt/atomspace/nodes/ConceptNode/cat
  │   ├── node23 -> /mnt/atomspace/nodes/PredicateNode/eats
  │   └── node1841 -> /mnt/atomspace/links/InheritanceLink/cat-mammal
  ├── dormant/                     # STI < threshold (unmounted, "cold")
  │   └── (namespace exists but not mounted = ZERO cost)
  └── ctl                          # Attention control interface
```

**Key Insight**: Unmounted nodes cost ZERO bandwidth. The namespace can be infinite, but only "hot" paths consume resources.

## Performance Characteristics

### 9P Message Cost

```
9P message cost:     ~Tmsg (serialize + RTT + deserialize)
Path walk cost:      O(depth) × Tmsg
Fan-out per node:    ~64 before fd/socket pressure
Tree depth:          log₆₄(N)

TOTAL NODES:         64^depth

depth=1:    64 nodes       1 hop      ← sweet spot for latency
depth=2:    4,096 nodes    2 hops     ← sweet spot for throughput  
depth=3:    262,144 nodes  3 hops     ← diminishing returns begin
depth=4:    16.7M nodes    4 hops     ← coordination dominates
```

### Sparse Activation Advantage

**Traditional parallel**: N nodes → O(N) coordination cost  
**9P namespace parallel**: N nodes → O(active paths) cost

The magic is: **unmounted nodes cost ZERO.**

## Development Status

### Completed

- ✅ Dis VM core structure
- ✅ Bytecode loader
- ✅ 9P protocol structure
- ✅ CMake build system
- ✅ Header files
- ✅ Integration with AGI-OS build

### In Progress

- 🚧 Dis VM instruction dispatch
- 🚧 Memory management
- 🚧 Garbage collection
- 🚧 9P message handling
- 🚧 Limbo runtime

### Planned

- 📋 Complete Dis VM implementation
- 📋 Complete 9P protocol implementation
- 📋 Limbo language runtime
- 📋 Namespace management
- 📋 Cognitive scheduler
- 📋 AtomSpace-9P integration
- 📋 PLN-9P integration
- 📋 ECAN-9P integration
- 📋 Distributed cognition

## Testing

### Unit Tests

```bash
# Run unit tests
cd build
ctest
```

### Integration Tests

```bash
# Test Dis VM
./test-dis-vm

# Test 9P protocol
./test-9p-protocol

# Test AtomSpace-9P integration
./test-atomspace-9p
```

## Documentation

- [Inferno Kernel Integration Plan](../../INFERNO_KERNEL_INTEGRATION_PLAN.md)
- [Build Dependency Map](../../BUILD_DEPENDENCY_MAP.md)
- [Errors Fixed](../../ERRORS_FIXED.md)

## References

- [Inferno OS](http://www.vitanuova.com/inferno/)
- [9P Protocol](https://en.wikipedia.org/wiki/9P_(protocol))
- [Dis Virtual Machine](http://www.vitanuova.com/inferno/papers/dis.html)
- [Limbo Language](http://www.vitanuova.com/inferno/papers/limbo.html)
- [OpenCog](https://opencog.org/)
- [AtomSpace](https://wiki.opencog.org/w/AtomSpace)

## License

See LICENSE file in AGI-OS root directory.

## Contributing

See CONTRIBUTING.md in AGI-OS root directory.

---

**Version**: 1.0.0  
**Date**: December 13, 2025  
**Status**: Foundation implementation complete, full implementation in progress
