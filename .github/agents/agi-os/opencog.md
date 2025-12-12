# OpenCog Cognitive Framework - Extended Definition

## Overview

OpenCog is a comprehensive cognitive framework providing reasoning, learning, perception, and language capabilities. In AGI-OS, it is organized by cognitive function rather than by component.

## Location

`core/cognition/`

## Functional Organization

```
core/cognition/
├── foundation/      # Core knowledge representation
├── storage/         # Knowledge persistence
├── reasoning/       # Logical reasoning systems
├── attention/       # Attention allocation
├── learning/        # Pattern mining and evolution
├── generation/      # Content generation
├── language/        # Natural language processing
├── perception/      # Sensory perception
├── specialized/     # Domain-specific systems
├── network/         # Network services
├── embodiment/      # Physical embodiment
└── meta-cognition/  # Self-modification
```

## Foundation Layer

### CogUtil (`foundation/cogutil/`)

C++ utilities library providing:
- **Logging**: Comprehensive logging framework
- **Configuration**: Configuration management
- **Utilities**: Common utilities (random, timing, etc.)
- **Testing**: Testing framework

**Key Classes**:
- `Logger` - Logging system
- `Config` - Configuration management
- `RandGen` - Random number generation
- `Timer` - Timing utilities

### AtomSpace (`foundation/atomspace/`)

Weighted, labeled hypergraph for knowledge representation:

**Core Concepts**:
- **Atoms**: Nodes and Links
- **Truth Values**: Probabilistic strength and confidence
- **Attention Values**: STI, LTI, VLTI
- **Type System**: Hierarchical type system

**Key Classes**:
- `AtomSpace` - Main knowledge base
- `Handle` - Reference to Atom
- `Node` - Concept or entity
- `Link` - Relationship between Atoms
- `TruthValue` - Probabilistic truth
- `AttentionValue` - Importance values

**Example**:
```cpp
#include <opencog/atomspace/AtomSpace.h>

AtomSpace as;
Handle concept = as.add_node(CONCEPT_NODE, "AI");
Handle property = as.add_node(CONCEPT_NODE, "intelligent");
Handle inheritance = as.add_link(INHERITANCE_LINK, concept, property);
inheritance->setTruthValue(SimpleTruthValue::createTV(0.9, 0.8));
```

## Storage Layer

### AtomSpace Storage (`storage/atomspace-storage/`)

**⭐ CRITICAL**: Must be built before cogserver!

Persistence layer for AtomSpace:
- **Abstract Interface**: Backend-agnostic storage API
- **Caching**: In-memory caching for performance
- **Transactions**: ACID transactions for consistency
- **Versioning**: Version control for knowledge

**Key Classes**:
- `StorageNode` - Storage backend interface
- `BackingStore` - Abstract storage backend
- `AtomTable` - In-memory atom table with persistence

### Storage Backends (`storage/backends/`)

#### CogServer Backend (`backends/cog/`)
Network-based storage using CogServer protocol.

#### RocksDB Backend (`backends/rocks/`)
High-performance embedded database.

#### PostgreSQL Backend (`backends/postgres/`)
SQL database for structured queries.

## Reasoning Layer

### Unified Rule Engine (`reasoning/ure/`)

Generic rule-based reasoning engine:
- **Forward Chaining**: Data-driven reasoning
- **Backward Chaining**: Goal-driven reasoning
- **Rule Selection**: Attention-based rule selection
- **Inference Control**: Flexible inference control

**Example**:
```scheme
(use-modules (opencog ure))

(define bc (BackwardChainer (Concept "my-goal")))
(cog-bc bc)
```

### Probabilistic Logic Networks (`reasoning/pln/`)

Probabilistic reasoning system:
- **Deduction**: Logical deduction with uncertainty
- **Induction**: Learning from examples
- **Abduction**: Hypothesis generation
- **Revision**: Belief revision

**Key Rules**:
- Deduction Rule
- Induction Rule
- Abduction Rule
- Revision Rule

### Unification (`reasoning/unify/`)

Pattern matching and unification:
- **Pattern Matching**: Find patterns in AtomSpace
- **Unification**: Unify patterns with atoms
- **Variable Binding**: Bind variables to atoms

### SpaceTime (`reasoning/spacetime/`)

Spatial and temporal reasoning:
- **Spatial Maps**: 3D spatial representations
- **Temporal Sequences**: Time series and sequences
- **Spatiotemporal Patterns**: Combined space-time patterns

## Attention Layer

### Economic Attention Networks (`attention/ecan/`)

Attention allocation mechanism:

**Attention Values**:
- **STI** (Short-Term Importance): Immediate relevance
- **LTI** (Long-Term Importance): Historical significance
- **VLTI** (Very Long-Term Importance): Permanent importance

**Mechanisms**:
- **Spreading**: Attention spreads through links
- **Rent**: Atoms pay rent based on STI
- **Forgetting**: Low-attention atoms forgotten
- **Hebbian Learning**: Strengthen frequently co-activated links

**Example**:
```cpp
#include <opencog/attention/AttentionBank.h>

AttentionBank& ab = as.get_attention_bank();
ab.set_sti(h, 100.0);  // Set STI
ab.stimulate(h, 50.0); // Increase STI
```

## Learning Layer

### Pattern Miner (`learning/miner/`)

Discovers patterns in data:
- **Frequent Patterns**: Find frequently occurring patterns
- **Surprising Patterns**: Find unexpected patterns
- **Compositional Patterns**: Hierarchical pattern composition

### MOSES (`learning/moses/`)

Meta-Optimizing Semantic Evolutionary Search:
- **Program Learning**: Learn programs from examples
- **Feature Selection**: Select relevant features
- **Model Building**: Build predictive models

### AS-MOSES (`learning/asmoses/`)

AtomSpace-based MOSES for symbolic learning.

### Learn (`learning/learn/`)

General learning framework:
- **Supervised Learning**: Learn from labeled data
- **Unsupervised Learning**: Discover structure in data
- **Reinforcement Learning**: Learn from rewards

## Generation Layer

### Generate (`generation/generate/`)

Content generation system:
- **Text Generation**: Generate natural language
- **Code Generation**: Generate program code
- **Music Generation**: Generate musical compositions

## Language Layer

### Link Grammar (`language/link-grammar/`)

Syntactic parser for natural language:
- **Grammar Rules**: Link grammar formalism
- **Parsing**: Parse sentences into link structures
- **Multiple Languages**: Support for many languages

### LG-Atomese (`language/lg-atomese/`)

Convert Link Grammar output to Atomese:
- **Syntax Trees**: Represent syntax in AtomSpace
- **Semantic Extraction**: Extract semantic relationships

### RelEx (`language/relex/`)

Semantic relationship extractor:
- **Dependency Parsing**: Extract dependencies
- **Frame Extraction**: Extract semantic frames
- **Coreference Resolution**: Resolve pronouns

## Perception Layer

### Vision (`perception/vision/`)

Visual perception system:
- **Object Recognition**: Recognize objects in images
- **Scene Understanding**: Understand visual scenes
- **Visual Attention**: Attention-based visual processing

## Specialized Layer

### AGI-Bio (`specialized/agi-bio/`)

Bioinformatics and computational biology:
- **Gene Networks**: Model genetic regulatory networks
- **Protein Folding**: Predict protein structures
- **Drug Discovery**: Discover drug candidates

## Network Layer

### CogServer (`network/cogserver/`)

Network server for distributed cognition:
- **Network Protocol**: TCP/IP protocol for AtomSpace access
- **Multi-Client**: Support multiple concurrent clients
- **Security**: Authentication and authorization
- **Modules**: Extensible module system

**Example**:
```bash
# Start CogServer
cogserver -c /etc/opencog/cogserver.conf

# Connect with telnet
telnet localhost 17001
```

## Build Order

**Critical**: Components must be built in this order:

```
1. cogutil (foundation)
2. atomspace (foundation)
3. atomspace-storage (storage) ⭐ CRITICAL
4. storage backends (optional)
5. cogserver (network)
6. ure (reasoning)
7. pln, unify, spacetime (reasoning)
8. attention/ecan (attention)
9. learn, miner, moses, asmoses (learning)
10. generate (generation)
11. link-grammar, lg-atomese, relex (language)
12. vision (perception)
13. agi-bio (specialized)
```

## Configuration

### AtomSpace Configuration

```scheme
; ~/.opencog/atomspace.conf
(use-modules (opencog))

(define atomspace-config
  '((max-atoms . 1000000)
    (attention-enabled . #t)
    (storage-backend . "rocksdb")
    (storage-path . "/var/lib/atomspace")))
```

### CogServer Configuration

```scheme
; /etc/opencog/cogserver.conf
(use-modules (opencog cogserver))

(define cogserver-config
  '((port . 17001)
    (max-clients . 100)
    (auth-enabled . #t)
    (modules . ("libattention.so" "libpln.so"))))
```

## API Examples

### Creating Atoms

```cpp
#include <opencog/atomspace/AtomSpace.h>

AtomSpace as;

// Create nodes
Handle ai = as.add_node(CONCEPT_NODE, "AI");
Handle intelligent = as.add_node(CONCEPT_NODE, "intelligent");

// Create link
Handle inh = as.add_link(INHERITANCE_LINK, ai, intelligent);

// Set truth value
inh->setTruthValue(SimpleTruthValue::createTV(0.9, 0.8));
```

### Pattern Matching

```scheme
(use-modules (opencog query))

(define pattern
  (Bind
    (Variable "$X")
    (Inheritance (Variable "$X") (Concept "intelligent"))
    (Variable "$X")))

(cog-execute! pattern)
```

### Reasoning

```scheme
(use-modules (opencog ure))

(define bc (BackwardChainer (Concept "goal")))
(cog-bc bc)
```

## Performance

- **AtomSpace**: Millions of atoms, sub-millisecond access
- **Pattern Matching**: Optimized with indexing
- **Reasoning**: Parallel inference with attention control
- **Storage**: Depends on backend (RocksDB fastest)

## References

- OpenCog Documentation: https://wiki.opencog.org/
- AtomSpace: https://wiki.opencog.org/w/AtomSpace
- PLN: https://wiki.opencog.org/w/PLN
- ECAN: https://wiki.opencog.org/w/ECAN
