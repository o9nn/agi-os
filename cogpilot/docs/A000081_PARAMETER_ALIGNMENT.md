# A000081 Parameter Alignment

## Fundamental Principle

**NO ARBITRARY VALUES**: All parameters in the Deep Tree Echo system MUST be derived from the OEIS A000081 sequence or related mathematical structures consistent with rooted tree topology.

## OEIS A000081: The Foundation Sequence

The sequence A000081 counts the number of unlabeled rooted trees with n nodes:

```
n:  1   2   3   4   5    6    7     8      9      10     11     12
a:  1   1   2   4   9   20   48   115    286    719   1842   4766
```

This sequence serves as the **ontogenetic generator** for the entire Deep Tree Echo architecture, providing:

- **Structural alphabet**: The fundamental units of computation
- **Complexity measure**: Quantification of tree diversity at each order
- **Enumeration basis**: Count of elementary differentials in B-series
- **Growth pattern**: Natural expansion rates for self-organization

## The 1-1 Correspondence Principle (CRITICAL)

For any given order n, the system maintains a **1-1 relationship** between three expressions of A000081:

1. **B-series roots** (rooted trees) = A000081[n]
2. **P-system membranes** (at order n) = A000081[n]
3. **J-surface differentials** (elementary differentials) = A000081[n]

These three counts MUST be equal for mathematical consistency. This is known as the **Matula number correspondence**.

### Reference Table

| Order | Reservoir Size* | Roots | Membranes | Differentials |
|-------|-----------------|-------|-----------|---------------|
| 1     | 1               | 1     | 1         | 1             |
| 2     | 2               | 1     | 1         | 1             |
| 3     | 4               | 2     | 2         | 2             |
| 4     | 8               | 4     | 4         | 4             |
| 5     | 17              | 9     | 9         | 9             |
| 6     | 37              | 20    | 20        | 20            |
| 7     | 85              | 48    | 48        | 48            |
| 8     | 200             | 115   | 115       | 115           |
| 9     | 486             | 286   | 286       | 286           |
| 10    | 1205            | 719   | 719       | 719           |

*Reservoir Size = cumulative sum of A000081[1:n]

### Validation

Use `validate_component_correspondence()` to ensure the 1-1 relationship holds:

```julia
using DeepTreeEcho.A000081Parameters

# Validate order 5 components
is_valid, msg = validate_component_correspondence(
    5,  # order
    9,  # num B-series roots
    9,  # num P-system membranes  
    9   # num J-surface differentials
)
# Returns: (true, "✓ 1-1 correspondence validated...")
```

## Parameter Derivation Rules

### 1. Reservoir Size

**Derivation**: Cumulative sum of tree counts up to a base order.

```julia
reservoir_size = sum(A000081[1:base_order])
```

**Rationale**: The reservoir must accommodate all distinct tree structures up to the system's complexity horizon. Each tree represents a unique structural pattern that can be encoded in the reservoir.

**Valid Values**:
- base_order=1: reservoir_size = 1
- base_order=2: reservoir_size = 2 (1+1)
- base_order=3: reservoir_size = 4 (1+1+2)
- base_order=4: reservoir_size = 8 (1+1+2+4)
- base_order=5: reservoir_size = 17 (1+1+2+4+9)
- base_order=6: reservoir_size = 37 (1+1+2+4+9+20)

**Why NOT arbitrary values like 50 or 100?**
These don't correspond to any natural tree count or cumulative structure in A000081.

### 2. Maximum Tree Order

**Derivation**: Base order plus a small offset for exploration.

```julia
max_tree_order = base_order + offset  # offset typically 2-4
```

**Rationale**: The system needs to explore slightly beyond its base complexity to discover new patterns. The offset should be small to maintain computational tractability.

**Valid Configurations**:
- base_order=4, offset=3 → max_tree_order=7
- base_order=5, offset=3 → max_tree_order=8
- base_order=6, offset=3 → max_tree_order=9

**Why NOT arbitrary values like 15?**
Unnecessarily large orders create computational burden without corresponding to the system's structural capacity.

### 3. Number of Membranes

**Derivation**: Direct value from A000081 at a specific membrane order.

```julia
num_membranes = A000081[membrane_order]
```

**Rationale**: Membranes represent hierarchical compartments. Their count should align with natural tree diversity at a chosen structural level.

**Valid Values**:
- membrane_order=1: num_membranes = 1 (single membrane)
- membrane_order=2: num_membranes = 1 (still single)
- membrane_order=3: num_membranes = 2 (bifurcation)
- membrane_order=4: num_membranes = 4 (quadfurcation)
- membrane_order=5: num_membranes = 9 (higher complexity)

**Why NOT arbitrary value 3?**
3 doesn't appear in A000081[1:10], so it lacks structural justification from rooted tree topology.

### 4. Growth Rate

**Derivation**: Ratio of consecutive A000081 terms.

```julia
growth_rate = A000081[base_order + 1] / A000081[base_order]
```

**Rationale**: The growth rate should reflect how tree complexity naturally increases. This ratio captures the intrinsic expansion factor of rooted trees.

**Valid Values**:
- base_order=3: growth_rate = 4/2 = 2.0
- base_order=4: growth_rate = 9/4 = 2.25
- base_order=5: growth_rate = 20/9 ≈ 2.22
- base_order=6: growth_rate = 48/20 = 2.4

**Why NOT arbitrary value 0.1?**
This is orders of magnitude smaller than natural tree growth rates and lacks mathematical justification.

### 5. Mutation Rate

**Derivation**: Inverse of tree count at base order.

```julia
mutation_rate = 1.0 / A000081[base_order]
```

**Rationale**: Higher complexity (more available tree structures) should lead to lower mutation rates to maintain stability. The mutation rate is inversely proportional to structural diversity.

**Valid Values**:
- base_order=3: mutation_rate = 1/2 = 0.5
- base_order=4: mutation_rate = 1/4 = 0.25
- base_order=5: mutation_rate = 1/9 ≈ 0.111
- base_order=6: mutation_rate = 1/20 = 0.05

**Why NOT arbitrary value 0.05 for base_order=4?**
At order 4, there are only 4 trees, so mutations should be more frequent (0.25) to explore the space effectively.

## Recommended Configurations

### Small System (Exploration & Testing)
```julia
params = get_parameter_set(4, membrane_order=3)
# reservoir_size = 8
# max_tree_order = 7
# num_membranes = 2
# growth_rate = 2.25
# mutation_rate = 0.25
```

**Use case**: Quick experiments, unit tests, initial prototyping

### Medium System (Standard Applications)
```julia
params = get_parameter_set(5, membrane_order=4)
# reservoir_size = 17
# max_tree_order = 8
# num_membranes = 4
# growth_rate ≈ 2.22
# mutation_rate ≈ 0.111
```

**Use case**: Default configuration, production systems, research

### Large System (Heavy Computation)
```julia
params = get_parameter_set(6, membrane_order=4)
# reservoir_size = 37
# max_tree_order = 9
# num_membranes = 4
# growth_rate = 2.4
# mutation_rate = 0.05
```

**Use case**: Large-scale simulations, complex pattern learning

## Usage Examples

### Correct: A000081-Derived Parameters

```julia
using DeepTreeEcho

# Option 1: Auto-derive (BEST)
system = DeepTreeEchoSystem(base_order=5)

# Option 2: Explicit parameter set
params = get_parameter_set(5, membrane_order=4)
system = DeepTreeEchoSystem(
    reservoir_size = params.reservoir_size,
    max_tree_order = params.max_tree_order,
    num_membranes = params.num_membranes,
    growth_rate = params.growth_rate,
    mutation_rate = params.mutation_rate
)

# Option 3: Explain parameters
explain_parameters(params)
```

### Incorrect: Arbitrary Parameters (Legacy/Deprecated)

```julia
# ⚠️ WARNING: These values are NOT aligned with A000081
system = DeepTreeEchoSystem(
    reservoir_size = 100,    # No tree count gives 100
    max_tree_order = 8,      # Disconnected from reservoir_size
    num_membranes = 3,       # Not in A000081[1:6]
    growth_rate = 0.1,       # Too small, not a tree ratio
    mutation_rate = 0.05     # Arbitrary, not 1/tree_count
)
```

The system will show warnings and suggest corrections.

## Parameter Validation

To check if parameters align with A000081:

```julia
using DeepTreeEcho

is_valid, message = validate_parameters(
    reservoir_size=17,
    max_tree_order=8,
    num_membranes=4,
    growth_rate=20/9,
    mutation_rate=1/9
)

if is_valid
    println("✓ Parameters are A000081-aligned")
else
    println("⚠ Issues found:")
    println(message)
end
```

## Mathematical Justification

### Why This Matters

1. **Structural Consistency**: Parameters derived from A000081 maintain mathematical coherence with the rooted tree foundation of the system.

2. **Predictable Behavior**: When parameters align with tree topology, system behavior follows natural growth and evolution patterns.

3. **Reproducibility**: A000081-derived parameters provide a canonical reference, ensuring different implementations produce comparable results.

4. **Theoretical Guarantees**: Convergence, stability, and universality properties depend on proper parameter scaling relative to tree structure.

### The Unification Equation

The complete system evolution equation:

```
∂ψ/∂t = J(ψ) · ∇H_A000081(ψ) + R_echo(ψ, t) + M_membrane(ψ)
```

Each component depends on A000081 structure:
- **J(ψ)**: Structure matrix from tree topology
- **H_A000081(ψ)**: Hamiltonian encoding tree complexity
- **R_echo(ψ, t)**: Reservoir with tree-derived connectivity
- **M_membrane(ψ)**: Membrane evolution with tree-aligned counts

## Migration Guide

### From Arbitrary to A000081-Aligned Parameters

**Old Code** (Arbitrary):
```julia
system = DeepTreeEchoSystem(
    reservoir_size = 50,
    max_tree_order = 8,
    num_membranes = 3,
    growth_rate = 0.1,
    mutation_rate = 0.05
)
```

**New Code** (A000081-Aligned):
```julia
# Find closest A000081 configuration
# reservoir_size=50 → closest is 37 (base_order=6)
# num_membranes=3 → closest is 4 (membrane_order=4)
# Recalculate all parameters consistently

params = get_parameter_set(6, membrane_order=4)
system = DeepTreeEchoSystem(
    reservoir_size = params.reservoir_size,   # 37
    max_tree_order = params.max_tree_order,   # 9
    num_membranes = params.num_membranes,     # 4
    growth_rate = params.growth_rate,         # 2.4
    mutation_rate = params.mutation_rate      # 0.05
)

# Or simply:
system = DeepTreeEchoSystem(base_order=6)
```

## Future Extensions

### Continuous Tree Spaces

For future work extending to continuous tree manifolds, parameters would be derived from continuous analogs:

```julia
reservoir_size = floor(Int, ∫[1 to n] ρ_A000081(x) dx)
```

where ρ_A000081(x) is a smooth interpolation of the A000081 sequence.

### Higher-Order Sequences

Advanced configurations might use related sequences:
- **A000055**: Number of unlabeled trees (unrooted)
- **A000169**: Number of labeled rooted trees (Cayley's formula)
- **A001190**: Number of planar rooted trees

## References

1. **Cayley, A. (1857)**: "On the Theory of the Analytical Forms called Trees"
2. **Pólya, G. (1937)**: "Kombinatorische Anzahlbestimmungen für Gruppen, Graphen und chemische Verbindungen"
3. **OEIS A000081**: https://oeis.org/A000081
4. **Butcher, J.C. (2016)**: "Numerical Methods for Ordinary Differential Equations" (Chapter on B-series and rooted trees)

## Summary

**THE RULE**: Never select arbitrary values. Always derive from A000081 or justify the relationship to rooted tree structure.

**THE TOOL**: Use `get_parameter_set()` for automatic derivation.

**THE CHECK**: Use `validate_parameters()` to verify alignment.

**THE GOAL**: Maintain mathematical consistency with the rooted tree topology that underlies the entire Deep Tree Echo architecture.

---

*"In the beginning was the Tree, and the Tree was with A000081, and the Tree was A000081."*
