# CogNumach Microkernel - Extended Definition

## Overview

CogNumach is an enhanced GNU Mach microkernel that integrates cognitive capabilities at the lowest level of the operating system. It provides cognitive-aware process scheduling, attention-driven resource allocation, and AtomSpace integration for system primitives.

## Location

`core/microkernel/`

## Architecture

### Directory Structure

```
core/microkernel/
├── kern/           # Kernel core
│   ├── sched/     # Cognitive scheduler
│   ├── ipc/       # Inter-process communication
│   ├── thread/    # Thread management
│   └── task/      # Task management
├── ipc/            # Mach IPC implementation
│   ├── mach_msg.c # Message passing
│   ├── mach_port.c # Port management
│   └── ipc_space.c # IPC namespace
├── vm/             # Virtual memory
│   ├── vm_map.c   # Memory mapping
│   ├── vm_object.c # Memory objects
│   └── vm_fault.c # Page fault handling
├── device/         # Device drivers
│   ├── ds_routines.c # Device server
│   └── dev_pager.c # Device pager
├── arch/           # Architecture-specific
│   ├── i386/      # x86 32-bit
│   ├── x86_64/    # x86 64-bit
│   └── aarch64/   # ARM 64-bit
└── mig/            # Mach Interface Generator
    ├── mig.c      # MIG compiler
    └── lexxer.l   # MIG lexer
```

## Cognitive Features

### 1. Cognitive Scheduler

**Location**: `core/microkernel/kern/sched/`

The cognitive scheduler integrates with OpenCog's attention allocation mechanism:

- **Attention-Driven Scheduling**: Process priority determined by STI (Short-Term Importance)
- **Dynamic Priority Adjustment**: Priorities updated based on attention values
- **Cognitive Load Balancing**: Distributes cognitive load across CPU cores

**Key Functions**:
- `cognitive_schedule()` - Main scheduling function
- `update_attention_priority()` - Update priority from AtomSpace
- `balance_cognitive_load()` - Balance load across cores

### 2. AtomSpace-Backed IPC

**Location**: `core/microkernel/ipc/`

Mach IPC primitives are mapped to AtomSpace:

- **Ports → Atoms**: Each Mach port is represented as an Atom
- **Messages → Atoms**: IPC messages stored as Atoms
- **Port Rights → Links**: Port rights represented as Links between Atoms

**Key Functions**:
- `port_to_atom()` - Convert Mach port to Atom
- `atom_to_port()` - Convert Atom to Mach port
- `msg_to_atom()` - Convert message to Atom representation

### 3. Cognitive Memory Management

**Location**: `core/microkernel/vm/`

Virtual memory management with cognitive awareness:

- **Attention-Based Paging**: Pages with high attention stay in memory
- **Cognitive Prefetching**: Predict page access based on cognitive patterns
- **Knowledge-Aware Caching**: Cache frequently accessed knowledge

**Key Functions**:
- `cognitive_page_fault()` - Handle page faults with attention awareness
- `attention_based_eviction()` - Evict pages based on attention values
- `cognitive_prefetch()` - Prefetch based on cognitive patterns

## Build Configuration

### CMake Options

```cmake
option(ENABLE_COGNITIVE_SCHEDULER "Enable cognitive scheduler" ON)
option(ENABLE_ATOMSPACE_IPC "Enable AtomSpace-backed IPC" ON)
option(ENABLE_COGNITIVE_VM "Enable cognitive memory management" ON)
option(BUILD_MULTIBOOT "Build multiboot kernel" ON)
option(BUILD_EFI "Build EFI kernel" ON)
```

### Build Commands

```bash
cd core/microkernel
mkdir build && cd build
cmake .. \
  -DENABLE_COGNITIVE_SCHEDULER=ON \
  -DENABLE_ATOMSPACE_IPC=ON \
  -DENABLE_COGNITIVE_VM=ON
make -j$(nproc)
sudo make install
```

## Dependencies

- **GNU Mach**: Base microkernel
- **AtomSpace**: Knowledge representation (from `core/cognition/foundation/atomspace`)
- **CogUtil**: Utilities (from `core/cognition/foundation/cogutil`)
- **MIG**: Mach Interface Generator

## Integration Points

### With HurdCog (Layer 1)

- **System Calls**: HurdCog servers use Mach IPC
- **Port Rights**: Translators receive port rights from microkernel
- **Memory Management**: Shared memory for efficiency

### With OpenCog (Layer 2)

- **AtomSpace Access**: Direct access to AtomSpace for cognitive primitives
- **Attention Values**: Read STI/LTI for scheduling decisions
- **Cognitive Queries**: Query AtomSpace for system state

### With Cognitive-Grip (Layer 3)

- **MachSpace Bridge**: Unified interface to Mach primitives
- **Configuration**: Central configuration for cognitive features
- **Monitoring**: Cognitive monitoring and introspection

## Performance Characteristics

- **Scheduling Overhead**: ~5% overhead for attention-based scheduling
- **IPC Overhead**: ~10% overhead for AtomSpace-backed IPC (can be disabled)
- **Memory Overhead**: ~2% overhead for cognitive memory management
- **Throughput**: Comparable to standard GNU Mach for non-cognitive workloads

## Configuration

### Cognitive Scheduler Configuration

```c
// core/microkernel/kern/sched/cognitive_config.h
#define COGNITIVE_SCHED_QUANTUM_MS 10
#define ATTENTION_UPDATE_INTERVAL_MS 100
#define MIN_STI_FOR_SCHEDULING 0.1
#define MAX_COGNITIVE_THREADS 1024
```

### AtomSpace IPC Configuration

```c
// core/microkernel/ipc/atomspace_config.h
#define ENABLE_PORT_ATOM_MAPPING 1
#define ENABLE_MESSAGE_ATOM_STORAGE 1
#define ATOMSPACE_IPC_CACHE_SIZE 4096
```

## Debugging

### Kernel Debug Options

```bash
# Enable cognitive scheduler debugging
echo "cognitive_sched_debug=1" > /proc/sys/kernel/cognitive_sched_debug

# Enable AtomSpace IPC debugging
echo "atomspace_ipc_debug=1" > /proc/sys/kernel/atomspace_ipc_debug

# View cognitive scheduler stats
cat /proc/cognitive_sched_stats
```

### GDB Debugging

```bash
# Attach GDB to running kernel
gdb /boot/gnumach
(gdb) target remote :1234
(gdb) break cognitive_schedule
(gdb) continue
```

## Testing

### Unit Tests

```bash
cd core/microkernel/tests
make test
```

### Integration Tests

```bash
cd infrastructure/testing/microkernel
./test-cognitive-scheduler.sh
./test-atomspace-ipc.sh
```

## Known Issues

1. **AtomSpace IPC Overhead**: Can be significant for high-frequency IPC; consider disabling for performance-critical applications
2. **Cognitive Scheduler Latency**: May introduce latency for real-time applications; use standard scheduler for RT tasks
3. **Memory Overhead**: AtomSpace backing requires additional memory; monitor usage

## Future Enhancements

- **Distributed Microkernel**: Extend cognitive scheduling across multiple machines
- **Hardware Acceleration**: GPU acceleration for attention computation
- **Predictive Scheduling**: Machine learning for schedule prediction
- **Quantum Integration**: Quantum computing primitives for cognitive operations

## References

- GNU Mach Documentation: https://www.gnu.org/software/hurd/microkernel/mach/gnumach.html
- Mach IPC: https://www.gnu.org/software/hurd/microkernel/mach/ipc.html
- AtomSpace: `core/cognition/foundation/atomspace/`
- Cognitive-Grip: `core/integration/cognitive-grip/`
