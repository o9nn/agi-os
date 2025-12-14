# Membrane Computing with Rooted Shell Namespaces

## Overview

Membrane computing (P-Systems) provides a computational model inspired by biological cells that fundamentally changes how we think about complexity. This document explains how the Plan 9 Cognitive Cities Kernel implements membrane computing concepts and how they relate to rooted shell namespaces.

**For a detailed analysis of how membrane computing collapses the P vs NP distinction through maximal parallelism, see [Membrane Complexity Theory](membrane-complexity-theory.md).**

**For the mathematical foundations of rooted tree enumeration and generation, see [Rooted Trees: Enumeration and Generation](rooted-trees-enumeration.md).**

## Triple Representation

The rooted shell namespace system provides a **triple representation** for each shell:

1. **Shell as Namespace**: Container for nested structures (filesystem view)
2. **Shell as File**: Addressable entity with metadata (file view)
3. **Shell as Membrane**: P-System compartment for computation (membrane computing view)

## Membrane Computing Foundation

### P-Systems

A **P-System** (membrane system) is a computational model inspired by biological cells:

```
Cell Structure:        P-System:              Shell Namespace:
┌─────────────────┐   ┌─────────────────┐   ┌─────────────────┐
│ Cell Membrane   │ → │ Membrane        │ → │ Shell/Namespace │
│ ┌─────────┐     │   │ ┌─────────┐     │   │ ┌─────────┐     │
│ │ Organelle│     │   │ │ Submembr│     │   │ │ Subshell│     │
│ │ ┌─────┐ │     │   │ │ ┌─────┐ │     │   │ │ ┌─────┐ │     │
│ │ │Inner│ │     │   │ │ │Inner│ │     │   │ │ │Inner│ │     │
│ │ └─────┘ │     │   │ │ └─────┘ │     │   │ │ └─────┘ │     │
│ └─────────┘     │   │ └─────────┘     │   │ └─────────┘     │
│   Objects       │   │   Multisets     │   │   Files/Data    │
└─────────────────┘   └─────────────────┘   └─────────────────┘
```

### A000081 and P-Systems

The OEIS A000081 sequence enumerates:
- **Rooted trees**: Hierarchical structures
- **Free hyper-multisets**: Nested collections
- **P-System configurations**: Membrane topologies

Each parentheses notation represents:
```
(())     → Simple membrane with submembrane
(()())   → Membrane with two submembranes (siblings)
((()))   → Three nested membranes (deep nesting)
```

## Membrane Architecture in Plan 9

### Membrane as Shell

Every shell in the rooted namespace system is a membrane:

```c
struct RootedShell {
    // ... existing fields ...
    
    // Membrane computing properties
    char *membrane_id;              // Unique membrane identifier
    Multiset *objects;              // Objects (multiset) in membrane
    Rule **evolution_rules;         // Evolution rules for objects
    int rule_count;                 // Number of rules
    
    // P-System topology
    RootedShell *parent_membrane;   // Containing membrane
    RootedShell **child_membranes;  // Nested membranes
    int child_membrane_count;       // Number of submembranes
    
    // Membrane properties
    char *permeability;             // Permeability rules
    int dissolution_flag;           // Membrane dissolution state
    int division_flag;              // Membrane division state
};
```

### Multiset Operations

Objects in a membrane form a multiset:

```
Multiset in membrane: {a³, b², c}
Representation: a, a, a, b, b, c

Filesystem analogy:
/membrane/
  ├── object-a-1
  ├── object-a-2
  ├── object-a-3
  ├── object-b-1
  ├── object-b-2
  └── object-c-1
```

### Evolution Rules

Membranes evolve according to rules:

```
Rule format: u → v
where u, v are multisets

Example rules:
  a → bc     (object a transforms to b and c)
  ab → c     (objects a and b react to produce c)
  a →ₒ b     (send b to outer membrane)
  a →ᵢ b     (send b to inner membrane)
```

## Filesystem as Membrane Computer

### Membrane Hierarchy

The rooted shell namespace directly provides membrane structure:

```
/cognitive-cities/domains/transportation/
├── membrane0/                    # Outermost membrane
│   ├── membrane0.shell          # Membrane as file
│   ├── objects/                 # Multiset of objects
│   │   ├── vehicle-1
│   │   ├── vehicle-2
│   │   └── signal-1
│   ├── rules/                   # Evolution rules
│   │   ├── traffic-flow.rule
│   │   └── signal-timing.rule
│   └── membrane1/               # Nested membrane
│       ├── membrane1.shell      # Submembrane as file
│       ├── objects/
│       │   ├── route-1
│       │   └── route-2
│       └── rules/
│           └── route-optimization.rule
```

### Object Files

Objects in membranes are represented as files:

```bash
# Create object in membrane
echo "vehicle,type=car,speed=60" > /membrane0/objects/vehicle-123

# Read object state
cat /membrane0/objects/vehicle-123

# Object multiplicity (multiset)
ls /membrane0/objects/vehicle-* | wc -l  # Count vehicles
```

### Rule Files

Evolution rules are also files:

```bash
# Define evolution rule
cat > /membrane0/rules/traffic-flow.rule << EOF
# Rule: vehicle + signal → moving-vehicle
when: vehicle present AND signal=green
action: transform vehicle to moving-vehicle
target: send to outer-membrane
EOF

# Apply rules
echo "evolve" > /membrane0/ctl
```

### Membrane Operations

Standard P-System operations map to filesystem operations:

| P-System Operation | Filesystem Operation |
|-------------------|---------------------|
| Create membrane | `mkdir /membrane/submembrane` |
| Dissolve membrane | `rmdir /membrane/submembrane` |
| Add object | `touch /membrane/objects/obj` |
| Remove object | `rm /membrane/objects/obj` |
| Move object in | `mv obj /membrane/submembrane/objects/` |
| Move object out | `mv /membrane/objects/obj ../objects/` |
| Apply rules | `echo "evolve" > /membrane/ctl` |
| Query multiset | `ls /membrane/objects/` |

## P-System Configurations via A000081

The A000081 enumeration provides all possible membrane topologies:

### Size 1: Single Membrane
```
Configuration: ()
Membrane structure: ┌───┐
                    │ m │
                    └───┘
Path: /m0
P-System: Single compartment
```

### Size 2: Nested Membranes
```
Configuration: (())
Membrane structure: ┌─────┐
                    │ m0  │
                    │ ┌─┐ │
                    │ │m1││
                    │ └─┘ │
                    └─────┘
Path: /m0/m1
P-System: Two-level hierarchy
```

### Size 3: Branching Structures
```
Configuration 1: ((()))
Membrane structure: ┌───────┐
                    │  m0   │
                    │ ┌───┐ │
                    │ │m1 │ │
                    │ │┌─┐│ │
                    │ ││m2││ │
                    │ │└─┘│ │
                    │ └───┘ │
                    └───────┘
Path: /m0/m1/m2
P-System: Linear three-level

Configuration 2: (()())
Membrane structure: ┌───────┐
                    │  m0   │
                    │┌─┐ ┌─┐│
                    ││m1│ │m2││
                    │└─┘ └─┘│
                    └───────┘
Paths: /m0/m1, /m0/m2
P-System: Sibling membranes
```

## Cognitive Cities as Membrane Computer

### Domain-Specific Membranes

Each cognitive domain is a membrane computer:

#### Transportation Domain
```
Membranes: Intersection zones
Objects: Vehicles, signals, pedestrians
Rules:
  - vehicle + green_signal → moving_vehicle
  - moving_vehicle + red_signal → stopped_vehicle
  - congestion → split_traffic (membrane division)
```

#### Energy Domain
```
Membranes: Grid sections
Objects: Power units, loads, generation sources
Rules:
  - solar_unit + sunlight → power_unit
  - power_unit + load → consumed_power
  - overload → shed_load (membrane dissolution)
```

#### Governance Domain
```
Membranes: Policy layers
Objects: Proposals, votes, decisions
Rules:
  - proposal + votes → approved_policy
  - approved_policy → implementation (send to outer membrane)
  - expired_policy → archive (membrane dissolution)
```

#### Environment Domain
```
Membranes: Monitoring zones
Objects: Sensors, readings, alerts
Rules:
  - sensor + reading → data_point
  - data_point >threshold → alert
  - alert → action (send to governance membrane)
```

## Membrane Computing Operations

### Creating a Membrane System

```bash
# Create membrane topology (3-membrane system)
cogctl rooted-create transportation '(()())'

# This creates membrane structure:
# m0 (outermost)
#   ├── m1 (first submembrane)
#   └── m2 (second submembrane)
```

### Adding Objects

```bash
# Add objects to membrane
echo "create vehicle car-123" > /transportation/m0/objects/ctl
echo "create signal light-1" > /transportation/m0/objects/ctl
echo "create vehicle car-456" > /transportation/m0/objects/ctl

# Result: multiset {vehicle², signal}
```

### Defining Evolution Rules

```bash
# Create evolution rule file
cat > /transportation/m0/rules/flow.rule << EOF
rule: vehicle + green_signal → moving_vehicle
target: outer
priority: 1
EOF

# Add rule to membrane
echo "add-rule flow.rule" > /transportation/m0/ctl
```

### Evolving the System

```bash
# Execute one evolution step
echo "evolve" > /transportation/m0/ctl

# Continuous evolution
echo "evolve-continuous" > /transportation/m0/ctl

# Stop evolution
echo "stop" > /transportation/m0/ctl
```

### Querying Membrane State

```bash
# List all objects in membrane
ls /transportation/m0/objects/

# Count object multiplicities
ls /transportation/m0/objects/ | sort | uniq -c

# View membrane configuration
cat /transportation/m0.shell

# View multiset representation
cat /transportation/m0/multiset
```

## Membrane Communication

### Inter-Membrane Communication

Objects move between membranes following rules:

```
Parent membrane m0:
  ├── Child membrane m1: {a, b}
  └── Child membrane m2: {c}

Rule in m1: a →ₒ x
Effect: Send x to m0 (outer membrane)

Rule in m0: b →ᵢ y
Effect: Send y to m1 or m2 (inner membranes)
```

Filesystem operations:

```bash
# Send object from m1 to outer membrane m0
mv /transportation/m0/m1/objects/vehicle-123 \
   /transportation/m0/objects/

# Send object from m0 to inner membrane m1
mv /transportation/m0/objects/signal-1 \
   /transportation/m0/m1/objects/
```

### Cross-Domain Membrane Communication

Membranes in different domains can communicate via neural channels:

```
Transportation membrane ←neural channel→ Energy membrane
     {vehicle³}                              {power_unit²}

Rule: vehicle + power_unit → charged_vehicle
Crosses domain boundary via neural transport
```

## Membrane Computing Examples

### Example 1: Traffic Flow Optimization

```bash
# Create membrane structure
cogctl rooted-create transportation '((())())'

# Membrane topology:
# m0 (city)
#   ├── m1 (district)
#   │   └── m2 (intersection)
#   └── m3 (highway)

# Add vehicles to intersection
for i in {1..10}; do
  echo "vehicle car-$i" > /transportation/m0/m1/m2/objects/ctl
done

# Add traffic signal
echo "signal light-1 state=red" > /transportation/m0/m1/m2/objects/ctl

# Define evolution rule
cat > /transportation/m0/m1/m2/rules/flow.rule << EOF
# When signal turns green, move vehicles to district level
vehicle + signal[state=green] → moving_vehicle
target: outer
EOF

# Change signal state
echo "update signal light-1 state=green" > /transportation/m0/m1/m2/objects/ctl

# Evolve system
echo "evolve" > /transportation/m0/m1/m2/ctl

# Vehicles now move to district level (m1)
ls /transportation/m0/m1/objects/  # Shows moving vehicles
```

### Example 2: Energy Grid Management

```bash
# Create grid membrane structure
cogctl rooted-create energy '(()())'

# m0 (grid)
#   ├── m1 (renewable sources)
#   └── m2 (consumption)

# Add generation
echo "solar_unit su-1 capacity=100" > /energy/m0/m1/objects/ctl
echo "solar_unit su-2 capacity=100" > /energy/m0/m1/objects/ctl

# Add loads
echo "load l-1 demand=50" > /energy/m0/m2/objects/ctl
echo "load l-2 demand=80" > /energy/m0/m2/objects/ctl

# Rule: Generate power
cat > /energy/m0/m1/rules/generate.rule << EOF
solar_unit + sunlight → power_unit
target: outer
EOF

# Rule: Consume power
cat > /energy/m0/m2/rules/consume.rule << EOF
load + power_unit → satisfied_load
priority: 1
EOF

# Add sunlight (environmental input)
echo "sunlight intensity=high" > /energy/m0/m1/objects/ctl

# Evolve
echo "evolve" > /energy/m0/m1/ctl
echo "evolve" > /energy/m0/ctl
echo "evolve" > /energy/m0/m2/ctl

# Check results
ls /energy/m0/objects/       # Power units from generation
ls /energy/m0/m2/objects/    # Satisfied loads
```

## Mathematical Properties

### Membrane Configuration Count

Follows A000081 sequence:

| Membranes (n) | Configurations | Computational Capacity |
|---------------|----------------|----------------------|
| 1 | 1 | Single compartment |
| 2 | 1 | Linear hierarchy |
| 3 | 2 | Branching begins |
| 4 | 4 | Multiple topologies |
| 5 | 9 | Rich structure space |
| 10 | 719 | Massive parallelism |

### Computational Universality

P-Systems with membrane creation/dissolution are **Turing-complete**:
- Can simulate any Turing machine
- Can solve NP-complete problems in polynomial time (with exponential workspace)
- Natural model for parallel, distributed computation

### Membrane Dynamics

Key membrane operations:
- **Division**: `a [₀]₁ → [₀]₁ [₀]₁` (one membrane becomes two)
- **Dissolution**: `δ [₀]₁ → δ` (membrane disappears, contents released)
- **Creation**: `a → [₀]₁` (new submembrane created)

Filesystem equivalents:
- Division: `cp -r /membrane /membrane-copy`
- Dissolution: `mv /membrane/objects/* ../objects/; rmdir /membrane`
- Creation: `mkdir /membrane/new-submembrane`

## Integration with Cognitive Cities

### Membrane-Based Cognitive Architecture

The cognitive cities architecture now has membrane computing semantics:

```
Cognitive City = Membrane Computer
  where:
    - Domains = Top-level membranes
    - Services = Objects in membranes
    - Neural channels = Inter-membrane communication
    - Swarms = Parallel evolution rules
    - Emergence = Multiset transformations
    - Adaptation = Membrane restructuring
```

### Emergent Computation

Emergent patterns in cognitive cities are multiset transformations:

```
Transportation membrane: {congested_route³, alternative²}
Rule: congested_route → split_traffic
Evolution: Membrane divides, distributing load
Result: {route¹, route¹, route¹, alternative²} across multiple membranes
```

### Swarm Coordination as Membrane Evolution

Cognitive swarms are evolution rules:

```
Swarm agents = Evolution rules
Swarm coordination = Parallel rule application
Swarm coherence = Membrane stability
Emergent behavior = Multiset convergence
```

## Future Directions

### 1. Membrane Division and Dissolution

Implement dynamic membrane topology:

```c
int divide_membrane(RootedShell *membrane);
int dissolve_membrane(RootedShell *membrane);
int create_submembrane(RootedShell *parent);
```

### 2. Multiset Operations

Implement efficient multiset management:

```c
typedef struct Multiset Multiset;
void multiset_add(Multiset *ms, char *object, int count);
int multiset_count(Multiset *ms, char *object);
void multiset_apply_rule(Multiset *ms, Rule *rule);
```

### 3. Evolution Engine

Implement rule-based evolution:

```c
typedef struct Rule Rule;
void load_evolution_rules(RootedShell *membrane, char *rule_file);
void evolve_membrane_step(RootedShell *membrane);
void evolve_membrane_continuous(RootedShell *membrane);
```

### 4. Parallel Evolution

Leverage Plan 9 process groups for parallel evolution:

```c
void parallel_evolve(RootedShell **membranes, int count);
// Each membrane evolves in parallel process
```

### 5. Membrane Visualization

Visualize membrane topology and multiset dynamics:

```bash
cogctl membrane-visualize transportation
# Shows:
# - Membrane structure (tree)
# - Object distribution (multisets)
# - Rule activity (evolution)
# - Communication patterns (object movement)
```

## Philosophical Implications

### Triple Representation Unity

The shell/membrane system demonstrates:

1. **Structural**: Namespace hierarchy (filesystem view)
2. **Informational**: File entities (data view)
3. **Computational**: Membrane dynamics (process view)

These three views are **isomorphic** - the same underlying structure supports all three interpretations.

### Natural Computing

Membrane computing is:
- **Bio-inspired**: Models cellular processes
- **Distributed**: Inherently parallel
- **Hierarchical**: Natural nesting structure
- **Rule-based**: Evolution through rewriting

This aligns perfectly with cognitive cities:
- **Distributed intelligence**: No central control
- **Hierarchical organization**: Domains and services
- **Emergent behavior**: From simple rules
- **Self-organization**: Through evolution

### Computational Substrate

The rooted shell namespace provides a **universal computational substrate**:
- Any P-System can be implemented
- Any membrane topology can be enumerated (via A000081)
- Natural mapping to filesystem operations
- Integration with Plan 9 namespace model

## Parallelism and Complexity

### Maximal Parallel Semantics

A fundamental property of membrane computing is **maximal parallelism**: all applicable rules fire simultaneously, not sequentially.

```c
// NOT sequential application:
for (each rule in rules) {
    apply_rule_to_all_objects();  // O(n) time for n rules
}

// BUT maximal parallel application:
apply_all_rules_simultaneously();  // O(1) time step
```

This has profound implications for computational complexity. See [Membrane Complexity Theory](membrane-complexity-theory.md) for a detailed analysis.

### Multiplicity as Weight, Not Time

In membrane systems, the **multiplicity** of objects (how many copies exist) affects **space**, not **time**:

```
Multiset: {a, a, a, b, b}

Sequential interpretation:
  - 3 copies of object a → 3 time steps
  - 2 copies of object b → 2 time steps
  - Total: 5 time steps

Membrane interpretation:
  - 3 copies of object a → 3 concurrent processes
  - 2 copies of object b → 2 concurrent processes
  - Total: 1 time step (all execute in parallel)
```

### Connection to Matula Numbers

Matula numbers encode tree structure via prime factorization, where **exponents represent multiplicity**:

```
Tree: (()()())
Matula: 8 = 2³
Interpretation: Root with THREE children of type ()

Sequential cost: 3 operations
Parallel cost: 1 operation (3 concurrent membranes)
```

The exponent encodes **how many** (spatial weight), not **how long** (temporal duration).

### Complexity Collapse

In maximally parallel membrane systems:

| Model | Time | Space |
|-------|------|-------|
| Sequential (Classical) | O(2^n) | O(n) |
| Parallel (Membrane) | O(n) | O(2^n) |

The **exponential cost moves from TIME to SPACE**.

For NP-complete problems like SAT:
- Classical: Exponential time to search all assignments
- Membrane: Polynomial time (create membranes in parallel, check in parallel, collect results in parallel)
- Cost: Exponential space (exponential number of membranes)

**This is why P and NP collapse in the membrane model** - both become polynomial time, though NP requires exponential space.

For a rigorous treatment, see [Membrane Complexity Theory](membrane-complexity-theory.md).

## References

### Membrane Computing

- Păun, Gheorghe (2002). "Membrane Computing: An Introduction"
- Rozenberg, Grzegorz; Salomaa, Arto (1997). "Handbook of Formal Languages"
- The P-Systems Website: http://ppage.psystems.eu/

### Mathematical Foundations

- OEIS A000081: Number of rooted trees with n nodes
- OEIS A000311: Schröder's fourth problem (related to P-Systems)
- Cayley's theorem on labeled trees

### Cognitive Computing

- This implementation: Rooted shell namespaces
- Neural transport channels: Inter-membrane communication
- Cognitive swarms: Parallel evolution rules

## License

MIT License - see [LICENSE](../../LICENSE) for details.

---

**Membrane Computing with Rooted Shell Namespaces**: Where Plan 9 filesystems meet biological computing, creating a natural computation model for cognitive cities that is simultaneously a namespace hierarchy, a file system, and a distributed membrane computer.
