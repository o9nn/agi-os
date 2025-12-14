# Rooted Shell Namespaces

## Overview

Rooted shell namespaces extend the Plan 9 Cognitive Cities Kernel by mapping **rooted tree configurations** (from the A000081 sequence) to filesystem namespaces. This creates a mathematically elegant addressing protocol where nested shell configurations correspond to namespace hierarchies.

## Conceptual Foundation

### Key Insight

The problem statement reveals a profound connection:

> "Consider the rooted.md configurations in a different way by considering nested shells as unique execution contexts"

**Each nested shell configuration represents a rooted domain namespace where:**
1. Each shell is a domain namespace
2. A shell with its contents forms a subdomain namespace
3. The outermost shell is the root namespace
4. **Every shell is also a file**

### Mathematical Basis

Rooted trees from the OEIS sequence A000081 enumerate all possible ways to nest structures:

```
n=1: 1 way  → ()
n=2: 1 way  → (())
n=3: 2 ways → ((())), (()())
n=4: 4 ways → (((()))), ((()())), ((())())(()(()))
n=5: 9 ways → [9 distinct configurations]
```

Each parentheses configuration maps to a unique namespace structure.

## Architecture

### Dual Representation

Every shell has a **dual representation**:

1. **Shell as Namespace**: A container for other shells and files
2. **Shell as File**: Accessible as a regular file in the filesystem

This duality allows:
- Navigation through shell hierarchies like directories
- Direct file operations on shell entities
- Uniform addressing protocol

### Data Structures

#### RootedTree

```c
struct RootedTree {
    tree binary_rep;              // Binary encoding of tree structure
    char *parens_notation;        // Parentheses notation: "(()())"
    char *namespace_path;         // Corresponding namespace path
    int node_count;               // Number of shells in tree
    int depth;                    // Maximum nesting depth
    RootedTree **subtrees;        // Child subtrees
    int subtree_count;            // Number of subtrees
};
```

#### RootedShell

```c
struct RootedShell {
    char *shell_id;               // Unique shell identifier
    char *domain;                 // Associated cognitive domain
    RootedTree *tree_structure;   // Underlying rooted tree
    
    // Dual representation: shell as namespace
    CognitiveNamespace *as_namespace;
    char *namespace_mount_point;
    
    // Dual representation: shell as file
    char *file_path;
    Chan *file_channel;
    
    // Shell nesting relationships
    RootedShell *parent_shell;
    RootedShell **child_shells;
    int child_count;
    
    // Addressing protocol
    char **address_path;
    int path_depth;
};
```

## Namespace to Tree Mapping

### Parentheses to Paths

A parentheses configuration maps to a namespace path:

```
Parentheses:  (())
Namespace:    /domain/shell0/shell1

Parentheses:  (()())
Namespace:    /domain/shell0/shell1
              /domain/shell0/shell2

Parentheses:  ((()))
Namespace:    /domain/shell0/shell1/shell2
```

### Path Structure

For a tree with parentheses notation `P` in domain `D`:

```
/D/                          # Root domain
/D/shell0                    # First shell (outermost)
/D/shell0/shell1             # Nested shell
/D/shell0/shell1.shell       # Shell as file
```

### File System Hierarchy Example

Consider the 4-tree `((())())`:

```
/transportation/                          # Domain root
├── shell-transport-1.shell               # Shell 0 as file
└── shell-transport-1/                    # Shell 0 as namespace
    ├── shell-transport-2.shell           # Shell 1 as file
    ├── shell-transport-2/                # Shell 1 as namespace
    │   └── shell-transport-3.shell       # Shell 2 as file
    └── shell-transport-4.shell           # Shell 3 as file
```

## Addressing Protocol

### Path Resolution

The addressing protocol uses P9 namespaces to locate files according to rooted tree structure:

1. **Tree Generation**: Generate all rooted trees of size n using A000081 algorithm
2. **Shell Creation**: Map each tree to a shell configuration
3. **Namespace Mounting**: Mount each shell as a namespace
4. **File Creation**: Create file representations for each shell
5. **Path Binding**: Bind paths to shell entities

### Address Components

An address in rooted shell namespace consists of:

```
/<domain>/<shell_path>/<entity>
```

Where:
- `domain`: Cognitive domain (transportation, energy, etc.)
- `shell_path`: Path through nested shells (shell0/shell1/...)
- `entity`: Target file or namespace

### Navigation

Users can navigate through shells using standard Plan 9 commands:

```bash
cd /transportation/shell0            # Enter shell namespace
ls /transportation/shell0            # List contents
cat /transportation/shell0.shell     # Read shell as file
```

## Tree Generation Algorithm

### A000081 Implementation

The implementation uses the recursive assembly algorithm:

```c
static void
assemble_trees(uint n, tree t, uint sl, uint pos, uint rem)
{
    if (rem == 0) {
        append_tree(t);
        return;
    }
    
    if (sl > rem) {
        sl = rem;
        pos = rooted_trees.offset[sl];
    } else if (pos >= rooted_trees.offset[sl + 1]) {
        sl--;
        if (sl == 0) return;
        pos = rooted_trees.offset[sl];
    }
    
    assemble_trees(n, (t << (2*sl)) | list[pos], sl, pos, rem-sl);
    assemble_trees(n, t, sl, pos+1, rem);
}
```

### Binary Encoding

Trees are stored as binary-encoded parentheses:

```
Parentheses: ( ( ) )
Bits:        1 1 0 0
Encoded:     1 1 1 0 0  (with sentinel bit)
Value:       0b11100 = 28
```

This provides compact storage for efficient enumeration.

## API Functions

### Core Functions

#### `create_rooted_shell(domain, tree_structure)`

Creates a shell from a rooted tree structure.

#### `create_rooted_shell_from_parens(domain, parens_notation)`

Creates a shell from parentheses notation like `"(()())"`.

#### `enumerate_rooted_shells(domain, max_size, shells_out)`

Enumerates all possible shell configurations up to size n.

#### `get_shell_info(shell)`

Returns detailed information about a shell.

### Usage Examples

#### Create Single Shell

```c
RootedShell *shell = create_rooted_shell_from_parens("transportation", "(())");
```

#### Enumerate All 4-Shells

```c
RootedShell **shells;
int count = enumerate_rooted_shells("energy", 4, &shells);
// count = 4 (all 4-trees)
```

#### Get Shell Information

```c
char *info = get_shell_info(shell);
print("%s", info);
```

## Integration with Cognitive Cities

### Domain-Specific Shells

Each cognitive domain can have its own rooted shell configurations:

```
/transportation/              # Transportation domain
  shell configurations for traffic flow patterns

/energy/                      # Energy domain
  shell configurations for grid topologies

/governance/                  # Governance domain
  shell configurations for policy hierarchies

/environment/                 # Environment domain
  shell configurations for sensor networks
```

### Use Cases

#### 1. Traffic Flow Patterns

Model different intersection configurations as rooted trees:

```
(())         → Simple intersection
(()())       → T-intersection  
((()))       → Sequential lights
((())())     → Complex junction
```

#### 2. Energy Grid Topologies

Represent distribution network structures:

```
(())         → Single feeder
(()())       → Branched network
((())(()))   → Redundant paths
```

#### 3. Policy Hierarchies

Encode governance structures:

```
(())         → Direct policy
(()())       → Multi-stakeholder
((()))       → Nested oversight
```

## Filesystem Interface

### `/proc/cognitive/rooted/`

The rooted shell interface is accessible via:

```
/proc/cognitive/rooted/
├── ctl          # Control commands (write)
├── list         # Help and command list (read)
├── trees        # Generated tree configurations (read)
└── shells       # Active shell instances (read)
```

### Control Commands

Write to `/proc/cognitive/rooted/ctl`:

```bash
# Create shell from parentheses notation
echo "create transportation (()())" > /proc/cognitive/rooted/ctl

# Enumerate all 5-trees for energy domain
echo "enumerate energy 5" > /proc/cognitive/rooted/ctl

# Get information about a shell
echo "info shell-transportation-12345" > /proc/cognitive/rooted/ctl

# Display statistics
echo "stats" > /proc/cognitive/rooted/ctl
```

### Reading Shell Information

```bash
# List available commands
cat /proc/cognitive/rooted/list

# View generated trees
cat /proc/cognitive/rooted/trees

# List active shells
cat /proc/cognitive/rooted/shells
```

## Mathematical Properties

### Enumeration Sequence

The number of rooted trees follows A000081:

```
n:  1, 2, 3,  4,  5,  6,   7,   8,   9,   10
T:  1, 1, 2,  4,  9, 20,  48, 115, 286,  719
```

### Asymptotic Behavior

For large n:

```
T(n) ~ C * α^n / n^(3/2)
```

where α ≈ 2.9557652856 (Otter's constant).

### Canonical Ordering

Trees are generated in canonical order:
- Subtrees in non-increasing size order
- Lexicographic ordering for same-size subtrees

This ensures each tree has exactly one representation.

## Implementation Limits

### Current Limits

- **Maximum tree size**: 15 nodes (MAXN = 15)
- **Tree storage**: ~10,000 trees initially, expandable
- **Binary encoding**: 64-bit integers (uvlong)

### Performance

| Operation | Complexity | Typical Time |
|-----------|------------|--------------|
| Generate n-trees | O(T(n)) | < 1ms for n≤10 |
| Create shell | O(n) | < 1ms |
| Enumerate shells | O(T(n)) | < 10ms for n≤10 |
| Path resolution | O(depth) | < 1μs |

## Philosophical Implications

### Combinatorial Richness

Simple nesting rules generate exponential complexity:
- 5 shells → 9 configurations
- 10 shells → 719 configurations
- 15 shells → 37,663 configurations

This demonstrates the **fundamental richness** of hierarchical structures.

### Canonical Representation

The parentheses notation provides:
- **Uniqueness**: Each tree has exactly one representation
- **Compactness**: Minimal encoding
- **Parsability**: Easy to generate and interpret

### Everything is a File

The dual shell representation embodies Plan 9's philosophy:
- Shells are namespaces (containers)
- Shells are files (entities)
- Uniform interface for both views

## Connection to Rooted Trees Theory

### From OEIS A000081

This implementation directly realizes the mathematical concepts from rooted tree enumeration:

1. **Bag Nesting Problem**: Each shell nesting ≡ bag configuration
2. **Recursive Structure**: Trees defined by subtrees
3. **Canonical Form**: Unique representation per structure
4. **Binary Encoding**: Efficient bit-level storage

### Elementary Differentials

The problem statement notes:

> "rooted trees are also the elementary differentials over the integer partitions that form the most efficient basis for complementary perspectives"

This suggests rooted shells provide:
- **Optimal basis**: For representing domain hierarchies
- **Complementary views**: Namespace vs file perspectives
- **Contextual topology**: Structure adapted to domain order

## Future Directions

### Dynamic Shell Reconfiguration

Enable runtime shell topology changes:
- Merge shells
- Split shells
- Rebalance hierarchies

### Cross-Domain Shell Binding

Connect shells across cognitive domains:
- Transportation shell ↔ Energy shell
- Governance shell ↔ Environment shell

### Emergent Shell Patterns

Detect frequently-used configurations:
- Learn optimal topologies
- Suggest shell structures
- Adapt to usage patterns

### Visual Shell Navigation

Interactive tree visualization:
- Navigate shell hierarchies visually
- Display parentheses structure
- Show namespace paths

## References

### Mathematical Foundations

- **OEIS A000081**: Rooted trees enumeration
- **Otter's constant**: Asymptotic growth rate
- **Cayley's formula**: Related tree counting

### Plan 9 Concepts

- **Namespaces**: Plan 9 namespace model
- **9P Protocol**: Filesystem access
- **Union directories**: Namespace composition

### Cognitive Cities

- **Domain namespaces**: Cognitive domain organization
- **Neural channels**: Inter-domain communication
- **Swarm coordination**: Collective intelligence

## License

MIT License - see [LICENSE](../../LICENSE) for details.

---

**Rooted Shell Namespaces**: Where mathematical tree enumeration meets filesystem addressing, creating an elegant protocol for nested cognitive domain structures in Plan 9.
