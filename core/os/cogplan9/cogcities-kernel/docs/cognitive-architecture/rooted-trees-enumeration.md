# Rooted Trees: Enumeration and Generation

## Overview

Rooted trees are fundamental combinatorial structures that represent hierarchical relationships with a designated root node. This implementation provides efficient algorithms for **enumerating all distinct rooted trees** with a given number of nodes, a problem that corresponds to the celebrated OEIS sequence [A000081](https://oeis.org/A000081).

## Concept

### The Bag Nesting Problem

Imagine you have `n` identical plastic bags that you want to nest inside each other. How many distinct ways can you arrange them? This seemingly simple problem has profound mathematical depth:

- For **1 bag**: 1 way → `()`
- For **2 bags**: 1 way → `(())`
- For **3 bags**: 2 ways → `((()))`, `(()())`
- For **4 bags**: 4 ways → `(((())))`, `((()()))`, `((())())`, `(()(()))`
- For **5 bags**: 9 ways → The full enumeration

Each bag nesting configuration represents a **rooted tree** where:
- Each bag is a **node**
- A bag with its contents forms a **subtree**
- The outermost bag is the **root**

### Mathematical Foundation

The number of rooted trees with `n` nodes follows the **OEIS A000081** sequence:
```
n:     1,  2,  3,  4,   5,   6,   7,    8,    9,    10,   11,    12, ...
T(n):  1,  1,  2,  4,   9,  20,  48,  115,  286,   719, 1842,  4766, ...
```

This sequence counts **unlabeled rooted trees**, meaning trees are considered identical if they have the same structure, regardless of how we label the nodes.

## Representation

### Parentheses Notation

Trees are represented using nested parentheses, where:
- `(` represents entering a bag/subtree
- `)` represents exiting a bag/subtree
- Siblings (subtrees at the same level) are concatenated

Examples:
```
()          # Single node (1-tree)
(())        # Two nodes, one child (2-tree)
((()))      # Three nodes in a chain (3-tree)
(()())      # Three nodes, two siblings (3-tree)
(((())))    # Four nodes in a chain (4-tree)
((()()))    # Four nodes, nested structure (4-tree)
((())())    # Four nodes, mixed structure (4-tree)
(()(()))    # Four nodes, two branches (4-tree)
```

### Binary Encoding

For efficient storage and manipulation, trees are encoded as integers where:
- Each bit pair represents a parenthesis
- Bit 1 = `(`, Bit 0 = `)`
- Trees are stored with a leading 1 bit as sentinel

Example for `(())`:
```
String:  ( ( ) )
Bits:    1 1 0 0
Encoded: 1 1 1 0 0  (with leading 1)
         = 0b11100 = 28 decimal
```

## Connection to Membrane Computing

### Rooted Trees as Membrane Configurations

In the Plan 9 Cognitive Cities Kernel, rooted trees directly correspond to **P-System membrane configurations**:

```
Tree Notation:     Membrane Structure:          Matula Number:
()                 Single membrane              1
(())               Membrane with 1 child        2
(()())             Membrane with 2 children     4 (= 2²)
((()))             Nested 3-deep                3 (= p(2))
```

### Multiplicity and Concurrency

The key insight connecting rooted trees to membrane computing complexity:

**In membrane systems, multiplicity = concurrent execution, not sequential time**

Example:
```
Tree: (()()())
Matula: 8 (= 2³)
Interpretation: Three identical submembranes

Sequential model:   3 operations × time_per_op
Parallel model:     1 operation (all execute simultaneously)
```

This is where the **P vs NP collapse** occurs in membrane computing.

## Algorithm

### Core Approach: Recursive Assembly

The algorithm builds trees of size `n` by assembling smaller subtrees:

1. **Base Case**: A 1-tree is just `()`
2. **Recursive Case**: An n-tree consists of:
   - An outer pair of parentheses
   - Inside: a combination of smaller trees that sum to `n-1` nodes

### Key Operations

#### 1. Tree Assembly

```
assemble(n, t, sl, pos, rem):
  n:   target tree size
  t:   partially assembled tree so far
  sl:  size of subtree being considered
  pos: position in list of sl-sized trees
  rem: remaining nodes to add
```

The algorithm:
1. If `rem == 0`: We've assembled a complete tree → save it
2. Otherwise: Try adding subtrees of size `sl` or smaller
3. Recursively continue with remaining nodes

#### 2. Tree Generation

```
mktrees(n):
  1. Generate all smaller trees first (mktrees(n-1))
  2. Assemble n-trees from (n-1)-trees using all valid combinations
  3. Store offset positions for efficient lookup
```

### Example: Building 4-trees

To build all 4-trees:
1. We need 3 nodes inside the outer parentheses
2. Possible partitions of 3:
   - `3` → one 3-tree
   - `2+1` → one 2-tree + one 1-tree
   - `1+1+1` → three 1-trees

The algorithm handles **canonical ordering** to avoid duplicates by ensuring subtrees appear in non-increasing size order.

## Matula Numbers and Prime Factorization

### Encoding Multiplicity

Matula numbers encode tree structure via prime factorization:

```
Tree        Structure               Matula  Factorization
()          Single node             1       1
(())        One child of type ()    2       p(1) = 2
(()())      Two children of type () 4       2² = 2×2
((()))      One child of type (())  3       p(2) = 3
(()()())    Three children          8       2³ = 2×2×2
```

**Key insight**: The exponent in factorization = multiplicity = number of parallel copies

### Multiplicity as Weight, Not Time

In classical computation:
```c
// Sequential: O(n) time for n copies
for (int i = 0; i < n; i++) {
    execute_operation();
}
```

In membrane computing:
```c
// Parallel: O(1) time for n copies
// All n operations execute simultaneously
// Cost is space (weight), not time
```

## Complexity Collapse in Membrane Systems

### Why P and NP Collapse

**Classical Model (Turing Machine):**
- Time = number of sequential steps
- Branching = exponential time growth
- NP problems require exponential search

**Membrane Model (P-System):**
- Time = number of parallel synchronization rounds
- Branching = increase in membrane count (space)
- Multiplicity encoded as weights, not time steps

### Example: SAT Solving

Classical approach:
```
n variables → 2^n assignments to check
Sequential: O(2^n) time
```

Membrane approach:
```
n variables → Create 2^n membranes in parallel
Each membrane checks one assignment
Time: O(n) parallel steps (not 2^n sequential steps)
Cost: Exponential space (2^n membranes), not time
```

### The Core Distinction

```
Classical:     time = f(branching width)
Membrane:      time = f(dependency depth)
               space = f(branching width)
```

In Matula encoding:
```
()          depth=1, width=1    → Matula=1
(())        depth=2, width=1    → Matula=2
(()()())    depth=2, width=3    → Matula=8
```

The width (number of children) appears as an **exponent** (multiplicity), which in membrane computing translates to **parallel copies**, not sequential time.

## Implementation in Plan 9 Kernel

### Representing Parallel Execution

```c
// Traditional tree node
struct TreeNode {
    TreeNode **children;
    int child_count;  // Sequential interpretation: loop this many times
};

// Membrane computing interpretation
struct MembraneNode {
    TreeNode **children;
    int child_count;  // Parallel interpretation: this many concurrent processes
    uvlong matula;    // Encodes multiplicity in factorization
};
```

### Extracting Multiplicity from Matula Number

```c
// Get multiplicity of submembrane type
void extract_multiplicities(uvlong matula, int *types, int *counts) {
    int exponents[MAX_PRIMES];
    factorize(matula, exponents, MAX_PRIMES);
    
    int idx = 0;
    for (int i = 0; i < MAX_PRIMES && exponents[i] > 0; i++) {
        // Prime index = submembrane type
        // Exponent = multiplicity = number of concurrent copies
        types[idx] = i + 1;        // Matula number of child type
        counts[idx] = exponents[i]; // How many in parallel
        idx++;
    }
}
```

### Parallel Rule Application

```c
// In a P-System, rules apply maximally in parallel
void apply_membrane_rules(Membrane *m) {
    // Not: for each object, apply rule sequentially
    // But: apply ALL applicable rules SIMULTANEOUSLY
    
    Multiset *new_objects = empty_multiset();
    
    // Collect ALL rule applications
    for (Object *obj in m->objects) {
        for (Rule *r in m->rules) {
            if (rule_applicable(r, obj)) {
                add_to_multiset(new_objects, r->products);
            }
        }
    }
    
    // Update state in ONE parallel step
    m->objects = new_objects;
    m->step_count++;  // Only ONE time unit elapsed
}
```

## Performance Characteristics

### Time Complexity

- **Generation**: O(T(n)) where T(n) is the number of trees
- **Assembly**: Exponential in n, but tractable for small n
- **For n=5**: ~9 trees generated
- **For n=10**: ~719 trees generated
- **For n=15**: ~37,663 trees generated

### Space Complexity

- **Storage**: O(T(n)) for all trees
- **Binary encoding**: 2n bits per tree
- **Matula encoding**: log₂(matula) bits per tree
- **Offset array**: O(n) for lookup table

### Practical Limits

Language-specific limits due to integer size and recursion:

| Model | Max n | Limiting Factor |
|-------|-------|-----------------|
| Sequential | ~20 | Stack/time |
| Parallel (theoretical) | ~30 | Space only |
| Membrane (P-System) | Depends on parallel hardware | Physical membranes |

## Mathematical Properties

### Cayley's Formula Connection

Cayley proved that the number of labeled trees on n vertices is n^(n-2). Our unlabeled rooted trees follow A000081, which is related but distinct.

### Recurrence Relation

The sequence satisfies:
```
a(n) = (1/n) * Σ(k=1 to n-1) [ (Σ(d|k) d*a(d)) * a(n-k) ]
```

### Generating Function

The ordinary generating function T(x) satisfies:
```
T(x) = x * exp(Σ(k≥1) T(x^k)/k)
```

### Asymptotic Behavior

For large n:
```
T(n) ~ C * α^n / n^(3/2)
```
where α ≈ 2.9557652856 (Otter's constant) and C ≈ 0.4399240125.

## Applications

### 1. Chemical Isomers

Rooted trees enumerate **structural isomers** of alkanes (CnH2n+2):
- Each carbon atom is a node
- Bonds determine tree structure
- Root represents a designated carbon

### 2. Membrane Computer Configuration Space

In membrane computing:
- Each tree = a valid membrane topology
- Matula number = configuration ID
- Multiplicity in factorization = concurrent submembranes

### 3. Parallel Algorithm Design

Rooted trees help analyze:
- Dependency structures
- Parallel width vs depth trade-offs
- Load balancing strategies

### 4. Hypergraph Filesystems

In the Plan 9 Cognitive Cities model:
- Nodes = filesystem directories
- Edges = communication channels
- Weights = Matula exponents = concurrent processes

## Philosophical Implications

### Combinatorial Explosion

Even simple rules (nest bags) lead to rapid growth:
- **Linear input**: n bags
- **Exponential output**: ~2.955^n trees

This demonstrates the **fundamental richness** of combinatorial structures.

### Parallel vs Sequential Complexity

The membrane computing model shows that:
- Complexity depends on the computational model
- P vs NP is model-dependent
- Parallel resources can collapse search trees

**Key insight**: What is "hard" in a sequential model may be "easy" in a parallel model, and vice versa.

### The Cost of Concurrency

Membrane computing doesn't make NP problems "easy" - it trades:
- **Sequential time** for **parallel space**
- **Step count** for **membrane count**
- **Algorithmic complexity** for **hardware complexity**

## Membrane Complexity Class Collapse Theorem

### Statement

**Theorem**: In a maximally parallel membrane computing system with exponential membrane creation:

```
For any NP-complete problem P:
  Sequential Time Complexity: O(2^n)
  Membrane Space Complexity: O(2^n)
  Membrane Time Complexity: O(poly(n))
```

### Proof Sketch

1. **NP problem structure**: Verifier runs in polynomial time
2. **Membrane encoding**: Create one membrane per possible solution
3. **Parallel verification**: All membranes verify simultaneously
4. **Result collection**: Accept if any membrane accepts
5. **Time analysis**: 
   - Membrane creation: O(n) parallel doubling steps
   - Verification: O(poly(n)) per membrane, in parallel
   - Collection: O(1) global synchronization
   - Total: O(poly(n))

### Interpretation

This doesn't mean P=NP in the classical sense. It means:

> **In the membrane model, the distinction between P and NP collapses
> because branching complexity is absorbed into spatial parallelism.**

The "hardness" of NP problems manifests as:
- **Classical**: Exponential time
- **Membrane**: Exponential space (membrane count)

### Practical Implications

Real-world membrane computers (e.g., DNA computing, quantum systems):
- Are limited by physical resources
- Cannot create unlimited membranes
- Face practical constraints on parallelism

Thus, the theoretical collapse doesn't translate to practical solvability of NP-hard problems.

## Future Directions

### Parallel Generation

Generate tree ranges in parallel:
- Partition by subtree combinations
- Independent assembly of ranges
- Merge results

### Membrane Computing Hardware

Design physical systems that exploit:
- Biological membranes (P-Systems)
- Quantum superposition (quantum computing)
- DNA molecules (DNA computing)
- Chemical reactions (reaction networks)

### Hypergraph Kernel Integration

Implement in Plan 9 kernel:
- Native support for parallel execution
- Membrane-aware scheduling
- Matula-indexed process trees

## References

### Academic Papers

- Păun, Gh. (2000). "Computing with Membranes"
- Cayley, A. (1857). "On the Theory of the Analytical Forms called Trees"
- Otter, R. (1948). "The Number of Trees"
- Matula, D. (1968). "A natural rooted tree enumeration by prime factorization"

### Online Resources

- OEIS A000081: https://oeis.org/A000081
- Membrane Computing: http://ppage.psystems.eu/
- Rosetta Code: http://rosettacode.org/wiki/List_rooted_trees

### Related Sequences

- A000108: Catalan numbers (binary trees)
- A000055: Free trees (unrooted)
- A000669: Labeled rooted trees
- A001190: Wedderburn-Etherington numbers

## Conclusion

Rooted trees provide a fundamental bridge between:
- **Combinatorial structures** (trees, partitions, factorizations)
- **Computational models** (sequential, parallel, membrane)
- **Complexity theory** (P, NP, space-time trade-offs)

In the Plan 9 Cognitive Cities Kernel:
- Rooted trees = namespace configurations
- Matula numbers = integer identifiers
- Membrane computing = parallel execution model
- Complexity collapse = theoretical foundation

**The enumeration of rooted trees is not just a mathematical curiosity - it's the foundation for understanding how parallel computational models fundamentally change the nature of algorithmic complexity.**

---

*Implementation notes: See tools/demos/rooted-shell-demo.c and tools/demos/matula-demo.c for working examples of tree enumeration and Matula encoding in the Plan 9 kernel.*
