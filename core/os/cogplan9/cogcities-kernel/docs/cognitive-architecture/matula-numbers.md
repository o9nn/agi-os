# Matula Numbers for Rooted Trees

## Overview

Matula numbers provide a **bijection** between rooted trees and natural numbers using prime factorization. This elegant encoding scheme allows every rooted tree to be uniquely represented as a positive integer, enabling efficient storage, indexing, and comparison of tree structures.

## The Encoding Scheme

### Basic Principle

The Matula number encoding works recursively:

1. **Base case**: The single-node tree `()` has Matula number **1**
2. **Recursive case**: A tree with children c₁, c₂, ..., cₙ has Matula number:
   ```
   M(tree) = p(M(c₁)) × p(M(c₂)) × ... × p(M(cₙ))
   ```
   where `p(k)` is the k-th prime number and `M(t)` is the Matula number of tree t

### Examples

| Tree Notation | Structure | Matula Calculation | Matula Number | Prime Factorization |
|--------------|-----------|-------------------|---------------|---------------------|
| `()` | Single node | 1 | 1 | 1 |
| `(())` | One child `()` | p(1) | 2 | 2 |
| `(()())` | Two children: `()`, `()` | p(1) × p(1) | 4 | 2² |
| `((()))` | One child `(())` | p(2) | 3 | 3 |
| `((()()))` | One child `(()())` | p(4) | 7 | 7 |
| `((())())` | Two children: `(())`, `()` | p(2) × p(1) | 6 | 2 × 3 |
| `(()()())` | Three children: `()`, `()`, `()` | p(1) × p(1) × p(1) | 8 | 2³ |
| `(((())))` | One child `((()))` | p(3) | 5 | 5 |

## Mathematical Properties

### Bijection

The Matula encoding establishes a one-to-one correspondence:
- Every rooted tree maps to exactly one positive integer
- Every positive integer maps to exactly one rooted tree
- No two different trees share the same Matula number
- No Matula number corresponds to more than one tree

### Prime Factorization Interpretation

The prime factorization directly reveals the tree structure:
- **Prime number p**: Tree with a single child whose Matula number is the index of p
- **Power p^k**: Tree with k identical children, each having Matula number (index of p)
- **Product p₁ × p₂ × ... × pₙ**: Tree with n children having different Matula numbers

### Examples of Factorization

```
Matula 12 = 2² × 3
          = p(1) × p(1) × p(2)
          → Tree with three children: (), (), (())
          → Parentheses: (()()(()))

Matula 18 = 2 × 3²
          = p(1) × p(2) × p(2)
          → Tree with three children: (), (()), (())
          → Parentheses: (()((()))(())))

Matula 11 = 11 (prime)
          = p(5)
          → Tree with one child having Matula 5
          → Child 5 = p(3), so child is ((()))
          → Result: ((((())))
```

## Connection to OEIS A000081

The Matula encoding respects the A000081 sequence enumeration:

| n | Count | Matula Range | Description |
|---|-------|--------------|-------------|
| 1 | 1 | 1 | Single node |
| 2 | 1 | 2 | One way to arrange 2 nodes |
| 3 | 2 | 3-4 | Two ways to arrange 3 nodes |
| 4 | 4 | 5-8 | Four ways to arrange 4 nodes |
| 5 | 9 | 9-20 | Nine ways to arrange 5 nodes |
| 6 | 20 | 21-48 | Twenty ways to arrange 6 nodes |

Not every integer corresponds to an n-node tree - the Matula numbers are scattered throughout the integers, with gaps corresponding to composite numbers that don't represent valid tree structures for that node count.

## Implementation

### Data Structure

The `RootedTree` structure includes the Matula number:

```c
struct RootedTree {
    tree binary_rep;              // Binary encoding
    char *parens_notation;        // Parentheses: "(()())"
    char *namespace_path;         // Namespace path
    int node_count;               // Number of nodes
    int depth;                    // Nesting depth
    RootedTree **subtrees;        // Child subtrees
    int subtree_count;            // Number of children
    uvlong matula_number;         // Matula encoding
};
```

### Key Functions

#### Encoding: Tree → Matula Number

```c
static uvlong parens_to_matula(char *parens);
```

Converts a parentheses representation to its Matula number.

**Algorithm:**
1. Parse the parentheses to extract direct children
2. Recursively compute Matula number for each child
3. For each child with Matula number m, multiply result by p(m)
4. Return the product

**Example:**
```c
parens_to_matula("((())())") 
  → children: ["(())"], ["()"]
  → child_1: parens_to_matula("(())") = 2
  → child_2: parens_to_matula("()") = 1
  → result: p(2) × p(1) = 3 × 2 = 6
```

#### Decoding: Matula Number → Tree

```c
static char* matula_to_parens(uvlong matula);
```

Converts a Matula number back to parentheses notation.

**Algorithm:**
1. Factor the Matula number into prime powers
2. For each prime factor p with exponent e:
   - Find the index i such that p = primes[i]
   - Add e children, each with Matula number (i+1)
3. Recursively convert children to parentheses
4. Wrap in outer parentheses

**Example:**
```c
matula_to_parens(6)
  → factorization: 2 × 3
  → factor 2 (index 0): add child with Matula 1 → "()"
  → factor 3 (index 1): add child with Matula 2 → "(())"
  → result: "(()((())))"
```

### Prime Number Table

A precomputed table of the first 100 primes supports trees up to very large sizes:

```c
static uvlong primes[] = {
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, ...
};
```

Helper functions:
- `nth_prime(n)`: Returns the n-th prime (1-indexed)
- `prime_index(p)`: Returns n such that p is the n-th prime
- `factorize(n, exponents, max)`: Computes prime factorization

## Applications in Cognitive Cities

### 1. Unique Addressing

Each tree configuration has a unique integer identifier:

```
Transportation intersection with Matula 12:
  /transportation/matula-12/
  Configuration: (()()(()))
  Quickly identifies the structure without parsing
```

### 2. Efficient Storage

Store tree configurations as integers instead of full structures:

```c
// Instead of storing full tree
struct Config {
    char *tree_notation;  // "((()())())"
    // ... more fields
};

// Store just the Matula number
struct Config {
    uvlong matula;        // 12
    // ... more fields
};
```

### 3. Quick Lookup

Use Matula numbers as database keys or hash table indices:

```c
// Fast lookup by Matula number
RootedTree* lookup_tree(uvlong matula) {
    return tree_cache[matula];
}
```

### 4. Pattern Matching

Compare tree structures by comparing integers:

```c
if (config1->matula == config2->matula) {
    // Structures are identical
}

if (is_prime(config->matula)) {
    // Configuration has a single child
}
```

### 5. Structure Classification

Use factorization to classify configurations:

```c
int exponents[100];
factorize(matula, exponents, 100);

int num_children = 0;
for (int i = 0; i < 100; i++)
    num_children += exponents[i];

if (num_children == 1) {
    // Linear chain structure
} else if (exponents[0] == num_children) {
    // All children are leaves (star topology)
}
```

## Usage Examples

### Creating Trees with Matula Numbers

```bash
# Create a tree and see its Matula number
cogctl rooted-create transportation '(()())'
# Output: Created shell with Matula number 4

# Create directly from Matula number
cogctl rooted-from-matula energy 12
# Output: Created shell with configuration (()()(()))
```

### Querying by Matula Number

```bash
# Find all shells with a specific Matula number
cogctl rooted-find-matula 6
# Output:
# transportation/shell-1: ((())())
# energy/shell-7: ((())(()))

# List shells by Matula range
cogctl rooted-list-range 5 10
```

### Analyzing Structure

```bash
# Show Matula number for a shell
cogctl rooted-info shell-transportation-123
# Output:
# Shell ID: shell-transportation-123
# Matula Number: 12
# Factorization: 2² × 3
# Children: 3 (two type-1, one type-2)
# Structure: (()()(()))
```

## Visualization

### Matula Number Space

```
Matula:  1    2    3    4    5    6    7    8    9   10   11   12
Tree:   ()  (()) (()()()(((())((())(()()()(((())...
Nodes:   1    2    3    2    4    3    4    3    4    4    5    4
Prime:  no  yes  yes   no  yes   no  yes   no   no   no  yes   no
```

### Tree → Number Examples

```
      ()           (())         (()())        ((()))
      ↓             ↓             ↓             ↓
      1         p(1)=2        2²=4          p(2)=3

   ((()()))     ((())())    (()()())      (((())))
      ↓            ↓            ↓             ↓
   p(4)=7       2×3=6        2³=8         p(3)=5
```

## Demo Program

The `matula-demo` program demonstrates the encoding:

```bash
cd tools/demos
./matula-demo
```

Output includes:
- Forward encoding: parentheses → Matula number
- Reverse decoding: Matula number → parentheses
- Prime factorization display
- Verification of roundtrip conversion
- Examples for all small Matula numbers

## References

1. **OEIS A000081**: Number of unlabeled rooted trees with n nodes
2. **David Matula (1968)**: "A natural rooted tree enumeration by prime factorization"
3. **Ivan Gutman & Yeong-Nan Yeh (1993)**: "Deducing properties of trees from their Matula numbers"
4. **Plan 9 Cognitive Cities**: Integration with namespace hierarchy

## Mathematical Notes

### Inversion Formula

Given a Matula number M, the tree can be reconstructed:
1. Factor M = p₁^e₁ × p₂^e₂ × ... × pₖ^eₖ
2. For each prime pᵢ:
   - Let nᵢ be the index such that pᵢ is the nᵢ-th prime
   - Add eᵢ children, each with Matula number nᵢ
3. Recursively reconstruct each child

### Bounds

For a tree with n nodes:
- Minimum Matula: Achieved by deep linear chain
- Maximum Matula: Achieved by star topology (all leaves)

Examples:
```
n=4 nodes:
Min: (((())))  = p(p(p(1))) = p(p(2)) = p(3) = 5
Max: (()()())  = 2³ = 8

n=5 nodes:
Min: (((((()))))) = p(5) = 11
Max: (()()()())  = 2⁴ = 16
```

### Density

The density of n-node trees in the Matula space decreases as n grows:
- For n=3: 2 trees in range [3,4] = 50% density
- For n=4: 4 trees in range [5,8] = 50% density
- For n=5: 9 trees in range [9,20] ≈ 75% density
- For n=6: 20 trees in range [21,48] ≈ 71% density

## Conclusion

Matula numbers provide an elegant and practical encoding scheme for rooted trees. By leveraging prime factorization, they enable:
- Compact integer representation
- Efficient storage and lookup
- Direct structure analysis
- Seamless integration with databases
- Natural ordering and comparison

This encoding is particularly valuable in the Plan 9 Cognitive Cities context, where tree configurations represent physical and logical network topologies that need to be efficiently stored, retrieved, and analyzed.
