# Unified Cognitive Interface for AGI-OS

## Overview

The Unified Cognitive Interface provides a consistent, cross-system API for all cognitive operations in AGI-OS. It reconciles the different paradigms (Inferno, CogPlan9, CoGNUHurd, CoGNUMach, OpenCog, DAS) into a single coherent interface exposed via the 9P protocol.

## Architecture

The unified interface operates on the principle that **all cognitive operations are file operations**. Every cognitive service is exposed as a file in the `/cog` namespace, accessible via the 9P protocol.

### Design Principles

1. **Protocol Unification**: 9P as the universal cognitive interface
2. **Best-of-Breed**: Use the best implementation from each system
3. **Transparency**: Network-transparent cognitive operations
4. **Simplicity**: File-centric operations are intuitive and composable
5. **Compatibility**: Works across all AGI-OS paradigms

### Component Selection

| Function | Primary Implementation | Distributed Layer | Embedded Client |
|----------|----------------------|-------------------|-----------------|
| AtomSpace | OpenCog (C++) | DAS (Python) | CogPlan9 libatomspace (C) |
| Reasoning | OpenCog PLN | DAS inference agent | CogPlan9 libpln |
| Protocol | Inferno 9P/Styx | - | - |
| Scheduling | CoGNUMach cognitive scheduler | - | CogPlan9 Fusion Reactor |
| Memory | CoGNUMach cognitive VM | - | CogPlan9 CogVM |

## File System Structure

All cognitive operations are accessible via the `/cog` file system:

```
/cog/
├── atoms/
│   ├── create          # Create new atoms
│   ├── read/HANDLE     # Read atom by handle
│   ├── update/HANDLE/  # Update atom properties
│   │   ├── tv          # Update truth value
│   │   └── av          # Update attention value
│   ├── delete          # Delete atoms
│   └── list            # List all atoms
│
├── query/
│   ├── pattern         # Pattern matching queries
│   ├── traversal       # Graph traversal
│   └── distributed     # Distributed queries (via DAS)
│
├── reason/
│   ├── pln             # PLN inference
│   ├── ure             # Unified Rule Engine
│   ├── forward         # Forward chaining
│   └── backward        # Backward chaining
│
├── attention/
│   ├── sti/            # Short-term importance
│   │   └── top         # Atoms with highest STI
│   ├── lti/            # Long-term importance
│   ├── allocate        # Allocate attention
│   └── spread          # Spread attention
│
├── learn/
│   ├── mine            # Pattern mining
│   ├── evolve          # Evolution agent
│   └── create_links    # Link creation agent
│
└── distributed/
    ├── sync            # Synchronize distributed atomspace
    ├── query           # Distributed query
    └── status          # Cluster status
```

## Usage Examples

### Creating Atoms

```bash
# Create a concept node
echo "ConceptNode dog 0.8 0.9" > /cog/atoms/create

# Create an inheritance link
echo "InheritanceLink dog animal 0.9 0.95" > /cog/atoms/create
```

### Pattern Matching

```bash
# Query for all animals
echo "(InheritanceLink \$X (ConceptNode \"animal\"))" > /cog/query/pattern
cat /cog/query/pattern
```

### PLN Inference

```bash
# Run PLN inference on atom 12345 with max 100 steps
echo "12345 100" > /cog/reason/pln
cat /cog/reason/pln
```

### Attention Allocation

```bash
# Allocate 50 STI to atom 12345
echo "12345 50" > /cog/attention/allocate

# Get atoms with highest attention
cat /cog/attention/sti/top
```

### Distributed Operations

```bash
# Synchronize distributed atomspace
echo "sync" > /cog/distributed/sync

# Check cluster status
cat /cog/distributed/status
```

## C API

The unified interface provides a C API for programmatic access:

```c
#include <agi-os/unified-cog/unified_cog.h>

// Initialize
cog_init(NULL);
cog_connect_9p("/n/cog");

// Create atom
CogTruthValue tv = { 0.8, 0.9 };
CogHandle dog = cog_atom_create(COG_CONCEPT_NODE, "dog", NULL, 0, tv);

// Pattern matching
CogHandle results[100];
size_t count = cog_query_pattern(
    "(InheritanceLink $X (ConceptNode \"animal\"))",
    results, 100
);

// PLN inference
CogHandle inferences[100];
count = cog_reason_pln(target, 100, inferences, 100);

// Attention allocation
cog_attention_allocate(dog, 50);

// Cleanup
cog_shutdown();
```

## Integration with Existing Systems

### Inferno Kernel

The InFernOKern provides the foundational 9P protocol implementation. All cognitive services are built on top of Inferno's Styx messaging layer.

### CogPlan9

CogPlan9's cognitive file servers provide the reference implementation for exposing cognitive services via 9P. The `libatomspace` and `libpln` libraries serve as lightweight clients.

### CoGNUHurd

CoGNUHurd integrates via translators that bridge between Hurd's filesystem and the 9P cognitive services. The AtomSpace bridge connects Hurd processes to the unified interface.

### CoGNUMach

CoGNUMach provides microkernel-level cognitive scheduling and memory management. The cognitive scheduler uses attention values from ECAN to prioritize tasks.

### OpenCog

OpenCog's AtomSpace is the authoritative hypergraph database. All atom operations ultimately interact with OpenCog's C++ implementation.

### DAS (Distributed AtomSpace)

DAS provides distributed query capabilities and scales the AtomSpace across multiple nodes. It wraps OpenCog's AtomSpace for distributed access.

## Building

```bash
mkdir build && cd build
cmake ..
make
sudo make install
```

## Running the 9P Server

```bash
# Start the cognitive 9P file server
cog9p

# Mount the cognitive file system
mount -t 9p /cog

# Now all cognitive operations are available as file operations
```

## Testing

```bash
# Run integration tests
./tests/test_unified_cog.sh
```

## Architecture Benefits

1. **Unified Interface**: Single API for all cognitive operations
2. **Network Transparency**: 9P enables distributed cognitive operations
3. **Language Agnostic**: Any language with 9P support can access cognitive services
4. **Composability**: File operations can be composed with standard Unix tools
5. **Simplicity**: Intuitive file-centric interface
6. **Flexibility**: Multiple implementations coexist harmoniously
7. **Scalability**: DAS provides distributed capabilities

## Future Work

1. Complete 9P protocol implementation for all cognitive operations
2. Implement full OpenCog AtomSpace backend
3. Add DAS distributed query optimization
4. Integrate CogPlan9 MachSpace for distributed hypergraph
5. Implement attention-driven scheduling across all layers
6. Add comprehensive integration tests
7. Performance optimization and benchmarking

## References

- [COGNITIVE_RECONCILIATION.md](../COGNITIVE_RECONCILIATION.md) - Detailed reconciliation strategy
- [OpenCog AtomSpace](https://github.com/opencog/atomspace)
- [DAS](https://github.com/singnet/das)
- [Inferno 9P Protocol](http://doc.cat-v.org/inferno/4th_edition/man/5/0intro)
- [Plan 9 from Bell Labs](https://9p.io/plan9/)
