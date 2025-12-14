# Matula Numbers Implementation Summary

## Overview

This implementation adds **Matula number encoding** to the Plan 9 Cognitive Cities Kernel's rooted tree system. Matula numbers provide a bijection between rooted trees and natural numbers using prime factorization, enabling efficient storage, indexing, and comparison of tree structures.

## What Was Implemented

### 1. Core Encoding/Decoding Functions (port/cognitive.c)

#### Prime Number Support
- Added table of first 100 primes for encoding/decoding
- `nth_prime(n)`: Returns the n-th prime number (1-indexed)
- `prime_index(p)`: Finds the index of a prime number
- `factorize(n, exponents, max)`: Computes prime factorization

#### Tree Parsing
- `parse_children_from_parens(parens, children_out)`: Extracts direct children from parentheses notation
- Handles nested structures correctly
- Allocates and returns array of child strings

#### Encoding: Tree → Number
- `parens_to_matula(parens)`: Converts parentheses notation to Matula number
- **Algorithm:**
  1. Base case: `()` maps to 1
  2. Parse direct children from parentheses
  3. Recursively compute Matula number for each child
  4. Multiply p(M(child_i)) for each child
  5. Return product

#### Decoding: Number → Tree
- `matula_to_parens(matula)`: Converts Matula number to parentheses notation
- **Algorithm:**
  1. Base case: 1 maps to `()`
  2. Factorize the Matula number
  3. For each prime p^e in factorization:
     - Add e children with Matula number (index of p)
  4. Recursively convert children to parentheses
  5. Wrap in outer parentheses

#### Integration
- `compute_matula_number(rt)`: Computes and stores Matula number for a RootedTree
- Updated `create_rooted_tree()` to automatically compute Matula numbers
- Added `matula_number` field to RootedTree structure

### 2. Enhanced Shell Information

#### Updated get_shell_info()
- Now displays Matula number for each shell
- Format: `Matula Number: %llud`
- Provides quick integer identifier for shells

#### New Helper Function
- `list_trees_with_matula(max_size)`: Lists all trees up to size n with their Matula numbers
- Formatted table showing: Size, Tree, Matula, Factorization
- Useful for debugging and exploration

### 3. Filesystem Interface Updates (port/devcognitive.c)

#### Enhanced /proc/cognitive/rooted/ Interface

**rooted/list**: Updated to mention Matula numbers
- Added description of Matula encoding
- Examples: `() = 1, (()) = 2, (()()) = 4`
- Reference to matula command

**rooted/trees**: Enhanced with Matula information
- Explains the encoding scheme
- Shows examples of common patterns
- Points to documentation

**rooted/shells**: Updated with Matula indexing info
- Describes shell's triple representation
- Mentions Matula number for indexing
- Instructions for viewing Matula numbers

### 4. Documentation

#### Created docs/cognitive-architecture/matula-numbers.md
Comprehensive 300+ line documentation covering:
- Mathematical foundation and principles
- Encoding/decoding algorithms
- Examples and visualizations
- OEIS A000081 correspondence
- Applications in Cognitive Cities
- Usage examples
- Mathematical properties and bounds

#### Updated README.md
- Added Matula numbers to triple representation example
- Updated mathematical foundation section
- Added Matula encoding examples
- Added matula-demo to Quick Start
- Listed matula-demo in Available Tools
- Added matula-demo.c to project structure
- Added Matula Numbers documentation link

### 5. Demonstration Programs

#### matula-demo.c
Interactive demonstration showing:
- Forward encoding: tree → Matula number
- Reverse decoding: Matula number → tree
- Prime factorization display
- Roundtrip verification
- Examples for Matula numbers 1-12
- Properties and applications
- Visual formatting with Unicode box drawing

**Output includes:**
- Tree structure with description
- Matula number value
- Prime factorization
- Reconstructed tree with verification
- Mapping table for A000081
- Properties explanation
- Applications in Cognitive Cities

#### matula-test.c
Comprehensive test suite with 48 assertions:
- **Basic encoding tests**: Single node, one child, two children, etc.
- **Prime encoding tests**: Verification that primes encode correctly
- **Composite encoding tests**: Products and powers
- **Decoding tests**: Number → tree conversion
- **Roundtrip tests**: tree → number → tree consistency
- **Factorization tests**: Prime factorization accuracy
- **Edge cases**: Empty input, NULL, deep nesting
- **Known sequence tests**: Verification against OEIS A000081

**All 48 assertions pass successfully!**

### 6. Build System Updates

Updated tools/demos/mkfile:
- Added matula-demo to build targets
- Added matula-test to build targets
- Both programs compile and run successfully

## Key Features

### 1. Bijection Property
- Every tree maps to exactly one positive integer
- Every positive integer maps to exactly one tree
- No ambiguity in either direction

### 2. Efficient Storage
Store trees as 64-bit integers instead of strings:
```c
// Before
struct Config {
    char *tree_notation;  // "((()())())" - variable length
};

// After
struct Config {
    uvlong matula;        // 12 - fixed 8 bytes
};
```

### 3. Fast Lookup
Use Matula numbers as database keys or array indices:
```c
RootedTree* tree_cache[MAX_MATULA];
tree = tree_cache[matula];  // O(1) lookup
```

### 4. Structure Analysis
Prime factorization reveals tree structure:
- Prime p: Single child with Matula (index of p)
- Power p^k: k identical children
- Product: Multiple different children

### 5. Pattern Matching
Compare structures by comparing integers:
```c
if (config1->matula == config2->matula) {
    // Identical structures
}

if (is_prime(config->matula)) {
    // Linear chain structure
}
```

## Examples

### Encoding Examples
```
()          → 1   (base case)
(())        → 2   (p(1) = 2)
(()())      → 4   (2² = two () children)
((()))      → 3   (p(2) = 3)
((()()))    → 7   (p(4) = 7)
(()(()))    → 6   (2 × 3 = () and (()) children)
(()()())    → 8   (2³ = three () children)
```

### Decoding Examples
```
1  → ()
2  → (())
3  → ((()))
4  → (()())
5  → (((())))
6  → (()(()))
7  → ((()()))
8  → (()()())
9  → ((())(()))
10 → (()((())))
```

## Mathematical Properties

### Density
The density of n-node trees in Matula space:
- n=3: 2 trees in [3,4] = 50%
- n=4: 4 trees in [5,8] = 50%
- n=5: 9 trees in [9,20] ≈ 75%
- n=6: 20 trees in [21,48] ≈ 71%

### Bounds for n-node trees
- **Minimum**: Deep linear chain = p(p(...p(1)...))
- **Maximum**: Star topology = 2^(n-1)

Example for n=5:
- Min: (((((()))))) = p(p(p(p(1)))) = p(p(p(2))) = p(p(3)) = p(5) = 11
- Max: (()()()()) = 2^4 = 16

## Applications in Cognitive Cities

### 1. Transportation Domain
Model intersection configurations with integer IDs:
```
T-intersection: (()())     = Matula 4
Complex junction: ((())()) = Matula 6
Sequential lights: ((()))  = Matula 3
```

### 2. Energy Distribution
Grid topologies with numeric identifiers:
```
Single feeder: (())           = Matula 2
Branched: (()())              = Matula 4
Redundant paths: ((())(()))   = Matula 9
```

### 3. Database Indexing
```sql
CREATE INDEX idx_matula ON configurations(matula_number);
SELECT * FROM configurations WHERE matula_number = 12;
```

### 4. Configuration Comparison
```c
// Instead of string comparison
if (strcmp(tree1->parens, tree2->parens) == 0)

// Use integer comparison
if (tree1->matula == tree2->matula)
```

## Testing Results

Comprehensive test suite validates:
✓ Basic encoding (5 tests)
✓ Prime number encoding (4 tests)
✓ Composite number encoding (4 tests)
✓ Decoding (4 tests)
✓ Roundtrip conversion (12 tests)
✓ Prime factorization (7 tests)
✓ Edge cases (4 tests)
✓ OEIS A000081 correspondence (8 tests)

**Total: 48 assertions, all passing**

## Files Modified/Created

### Modified Files
1. `port/cognitive.c` - Added ~250 lines of Matula encoding/decoding
2. `port/devcognitive.c` - Enhanced filesystem interface with Matula info
3. `README.md` - Updated with Matula numbers information
4. `tools/demos/mkfile` - Added new demo programs

### Created Files
1. `docs/cognitive-architecture/matula-numbers.md` - Comprehensive documentation (300+ lines)
2. `tools/demos/matula-demo.c` - Interactive demonstration (300+ lines)
3. `tools/demos/matula-test.c` - Test suite (400+ lines)
4. `tools/demos/matula-demo` - Compiled executable
5. `tools/demos/matula-test` - Compiled test executable

### Total Implementation
- ~1,200 lines of code
- ~300 lines of documentation
- 48 automated tests
- 2 demonstration programs

## Usage

### Running the Demo
```bash
cd tools/demos
./matula-demo
```

Shows encoding examples, decoding examples, properties, and applications.

### Running Tests
```bash
cd tools/demos
./matula-test
```

Validates implementation with comprehensive test suite.

### Using in Code
```c
// Create a rooted tree
RootedTree *tree = create_rooted_tree(binary_rep, 4);

// Matula number is automatically computed
printf("Matula: %llu\n", tree->matula_number);

// Convert Matula to tree
char *parens = matula_to_parens(12);
printf("Tree: %s\n", parens);  // Output: (()()(()))
```

### Via Filesystem
```bash
# View shell info (includes Matula number)
cat /proc/cognitive/rooted/shells

# Read documentation
cat /proc/cognitive/rooted/trees
```

## Conclusion

This implementation successfully adds Matula number encoding to the Plan 9 Cognitive Cities Kernel, providing:

1. **Mathematical rigor**: Based on well-studied bijection
2. **Practical utility**: Efficient storage and lookup
3. **Complete implementation**: Encoding, decoding, and testing
4. **Comprehensive documentation**: Theory and applications
5. **Working demonstrations**: Interactive and testable

The Matula encoding enriches the rooted tree system with a powerful integer-based representation that enables new possibilities for indexing, comparing, and analyzing tree structures in the cognitive cities architecture.

## References

1. **OEIS A000081**: Unlabeled rooted trees with n nodes
2. **David Matula (1968)**: "A natural rooted tree enumeration by prime factorization"
3. **Ivan Gutman & Yeong-Nan Yeh (1993)**: "Deducing properties of trees from their Matula numbers"
4. **Problem Statement**: Original requirements for Matula number implementation

---

*Implementation completed with all tests passing and comprehensive documentation.*
