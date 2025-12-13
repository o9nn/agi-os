# Ontogenetic Kernel System

**Self-evolving computational kernels with B-series genomes**

The Ontogenetic Kernel system implements Phases 2-4 of the CogPilot.jl agent roadmap, providing self-evolving numerical methods that optimize themselves through genetic algorithms.

## Overview

Each **Ontogenetic Kernel** represents a numerical integration method encoded as a B-series expansion:

```
y_{n+1} = y_n + h Σ_{τ∈T} b(τ)/σ(τ) · F(τ)(y_n)
```

Where:
- **T**: Set of rooted trees from OEIS A000081
- **b(τ)**: Coefficient from kernel genome (genetic material)
- **σ(τ)**: Symmetry factor of tree τ
- **F(τ)**: Elementary differential for tree τ
- **h**: Step size

The **genome** (tree → coefficient mapping) acts as genetic material that can:
- Mutate (coefficients change, trees added/removed)
- Recombine (crossover between parent genomes)
- Self-replicate (chain rule composition)
- Evolve (fitness-based selection)

## Architecture

### Core Components

1. **OntogeneticKernel.jl**
   - `Kernel`: Self-evolving computational kernel
   - `KernelGenome`: B-series coefficient mapping
   - `KernelLifecycle`: Development stages and maturity
   - Genetic operations: self-generation, crossover, mutation
   - Fitness evaluation: grip, stability, efficiency, novelty

2. **KernelEvolution.jl**
   - Population-based evolutionary algorithm
   - Tournament selection for breeding
   - Elitism and diversity preservation
   - Generation statistics tracking
   - (Currently standalone, to be integrated)

3. **DomainKernels.jl**
   - Domain-specific kernel generators
   - Consciousness kernels (self-referential, recursive)
   - Physics kernels (Hamiltonian, symplectic)
   - Reaction network kernels (mass action)
   - Time series kernels (temporal prediction)
   - Universal kernel generator (natural language → kernel)

## Mathematical Foundation

### B-Series Genomes

Each kernel has a **genome** mapping rooted trees to coefficients:

```julia
genome = Dict{Vector{Int}, Float64}()
# Example: [1,2,3] → 0.15  (linear tree of order 3)
#          [1,2,2] → 0.08  (branched tree of order 3)
```

Trees are represented as **level sequences**: `[1,2,3,3]` means root at level 1, node at level 2, two nodes at level 3.

### Fitness Components

Kernels are evaluated on four orthogonal objectives:

1. **Grip** (0-1): How well the kernel fits the target domain
   - Based on genome coverage and order
   - Domain-specific metrics available

2. **Stability** (0-1): Numerical stability properties
   - Coefficient magnitudes (smaller = more stable)
   - Coefficient variance (lower = more consistent)

3. **Efficiency** (0-1): Computational cost
   - Inverse of number of terms
   - Fewer terms = more efficient

4. **Novelty** (0-1): Genetic diversity from population
   - Average genetic distance to other kernels
   - Promotes exploration

**Overall Fitness** = 0.25 * (grip + stability + efficiency + novelty)

### Lifecycle Stages

Kernels develop through four stages:

- **Embryonic** (age 0-5): Newly created, untested
- **Juvenile** (age 6-15): Basic testing, fitness > 0.3
- **Mature** (age 16-50): Fully optimized, fitness > 0.6
- **Senescent** (age 51+ or fitness < 0.4): Declining performance

Stages affect selection probability and mutation rates.

## Usage

### Creating Kernels

```julia
using DomainKernels

# Create random kernel
kernel = create_kernel(4, symmetric=true, density=0.5)

# Create domain-specific kernels
consciousness = generate_consciousness_kernel(order=5, depth_bias=2.5)
physics = generate_physics_kernel(:hamiltonian, order=4, 
                                 conserved_quantities=[:energy])
reactions = generate_reaction_kernel(["A + B → C"], order=4)
timeseries = generate_timeseries_kernel(memory_depth=10, order=4)

# Universal generator (natural language)
kernel = generate_universal_kernel(
    "self-aware recursive cognition with deep memory",
    order=5
)
```

### Genetic Operations

```julia
using DomainKernels
const OK = DomainKernels.OntogeneticKernel

# Self-generation (chain rule composition)
parent = create_kernel(4)
offspring = OK.self_generate(parent)

# Crossover (genetic recombination)
parent1 = create_kernel(4)
parent2 = create_kernel(4)
child1, child2 = OK.crossover(parent1, parent2)

# Mutation
OK.mutate!(kernel, mutation_rate=0.15)

# Fitness evaluation
population = [create_kernel(4) for _ in 1:20]
OK.evaluate_kernel_fitness!(kernel, nothing, population)
```

### Population Evolution

```julia
# Create initial population
population = [
    generate_consciousness_kernel(order=5),
    generate_physics_kernel(:hamiltonian, order=4),
    [create_kernel(4) for _ in 1:18]...
]

# Simple evolution loop
for generation in 1:30
    # Evaluate fitness
    for kernel in population
        OK.evaluate_kernel_fitness!(kernel, nothing, population)
    end
    
    # Sort by fitness
    sort!(population, by=k->k.fitness, rev=true)
    
    # Create next generation
    next_gen = [population[1:2]...]  # Elitism
    
    while length(next_gen) < 20
        # Selection
        parent1 = rand(population[1:10])
        parent2 = rand(population[1:10])
        
        # Crossover and mutation
        child1, child2 = OK.crossover(parent1, parent2)
        OK.mutate!(child1, mutation_rate=0.15)
        push!(next_gen, child1)
    end
    
    # Update lifecycle
    for kernel in next_gen
        kernel.lifecycle.age += 1
        OK.update_stage!(kernel)
    end
    
    population = next_gen
end
```

## Domain-Specific Kernels

### Consciousness Kernel

Designed for self-referential, recursive cognitive dynamics:

```julia
consciousness = generate_consciousness_kernel(
    order = 5,
    depth_bias = 2.5  # Emphasize deep recursion
)
```

**Characteristics**:
- Emphasizes deep tree structures (high recursion)
- High symmetry coefficients (integrated processing)
- Self-referential feedback terms
- Born in **juvenile** stage (pre-optimized)

### Physics Kernel

Designed for Hamiltonian/Lagrangian systems:

```julia
physics = generate_physics_kernel(
    :hamiltonian,  # or :lagrangian, :dissipative
    order = 4,
    conserved_quantities = [:energy, :momentum]
)
```

**Characteristics**:
- Symplectic structure preservation
- Even-order trees for position-momentum coupling
- Conservation law constraints
- High symmetry (physical invariance)

### Reaction Network Kernel

Designed for chemical reaction dynamics:

```julia
reactions = generate_reaction_kernel(
    ["A + B → C", "C → D"],
    order = 4,
    mass_action = true
)
```

**Characteristics**:
- Linear and bilinear terms dominate (mass action)
- Stoichiometric constraints
- Conservation of mass
- Low-order focus (realistic kinetics)

### Time Series Kernel

Designed for temporal prediction:

```julia
timeseries = generate_timeseries_kernel(
    memory_depth = 10,        # Past timesteps
    order = 4,
    prediction_horizon = 5    # Future timesteps
)
```

**Characteristics**:
- Moderate depth trees (temporal coherence)
- Memory-weighted coefficients
- Horizon-adjusted scaling
- Temporal dependency capture

### Universal Generator

Automatically selects appropriate kernel type from natural language:

```julia
# Automatically detects consciousness domain
k1 = generate_universal_kernel("self-aware recursive cognition")

# Automatically detects physics domain
k2 = generate_universal_kernel("Hamiltonian mechanics energy conservation")

# Automatically detects time series domain
k3 = generate_universal_kernel("stock price prediction temporal patterns")
```

## Examples

### Complete Evolution Demo

See `examples/kernel_evolution_demo.jl` for a comprehensive demonstration:

```bash
julia examples/kernel_evolution_demo.jl
```

This demo shows:
1. Domain-specific kernel creation
2. Genetic operations (self-generation, crossover, mutation)
3. Population evolution over 30 generations
4. Fitness improvement and diversity analysis
5. Universal kernel generation from descriptions

### Quick Start

```julia
using DomainKernels

# Create consciousness kernel
kernel = generate_consciousness_kernel(order=5)

# Inspect genome
println("Genome: $(length(kernel.genome.coefficients)) terms")
for (tree, coeff) in kernel.genome.coefficients
    println("  $tree → $coeff")
end

# Evolve it
for gen in 1:10
    offspring = OK.self_generate(kernel)
    OK.mutate!(offspring, mutation_rate=0.2)
    OK.evaluate_kernel_fitness!(offspring, nothing, [kernel, offspring])
    
    if offspring.fitness > kernel.fitness
        kernel = offspring
        println("Gen $gen: improved to fitness=$(round(kernel.fitness, digits=3))")
    end
end
```

## Testing

Run the comprehensive test suite:

```bash
julia test/test_ontogenetic_kernel.jl
```

Tests cover:
- Kernel creation and structure
- Tree generation (A000081)
- Fitness evaluation (all 4 components)
- Genetic operations (self-generation, crossover, mutation)
- Lifecycle management (stage transitions)
- Domain-specific generators (5 types)

## Integration with Deep Tree Echo

The Ontogenetic Kernel system is designed to integrate with the Deep Tree Echo architecture:

```julia
using DeepTreeEcho
using DomainKernels

# Create Deep Tree Echo system
system = DeepTreeEchoSystem(base_order=5)

# Generate optimized kernel for system
kernel = generate_consciousness_kernel(order=5)

# Use kernel coefficients to configure B-series ridges
for (tree, coeff) in kernel.genome.coefficients
    # Apply to ridge configuration
    configure_ridge_coefficient!(system.ridge, tree, coeff)
end

# Evolve both system and kernels together
evolve!(system, 50)
```

## Future Work

### Phase 5: ModelingToolkit Integration

- [ ] Add symbolic ODESystem to each kernel
- [ ] Automatic differentiation for kernel optimization
- [ ] Structural simplification via ModelingToolkit
- [ ] Generate ODEProblem from kernel genome

### Phase 6: Advanced Features

- [ ] GPU acceleration for population evaluation
- [ ] Distributed evolution across clusters
- [ ] Multi-objective Pareto optimization
- [ ] Adaptive mutation rates
- [ ] Island model evolution (multiple populations)
- [ ] Kernel visualization (tree graphs, fitness landscapes)

### Phase 7: Applications

- [ ] Optimize kernels for specific ODEs
- [ ] Discover novel numerical methods
- [ ] Adaptive timestepping kernels
- [ ] Problem-specific method synthesis
- [ ] Kernel library for common domains

## Mathematical Properties

### B-Series Consistency

Kernels maintain B-series order conditions:

```
Σ_{τ∈T_p} b(τ)/σ(τ) = 1/p!
```

For order p, ensuring numerical integration accuracy.

### Genetic Distance

Distance between two kernels k1 and k2:

```
d(k1, k2) = √(Σ_{τ∈T} (b₁(τ) - b₂(τ))²) / |T|
```

Where T = union of trees in both genomes.

### Fitness Optimization

Evolution maximizes:

```
F(k) = w_G·G(k) + w_S·S(k) + w_E·E(k) + w_N·N(k)
```

Subject to:
- G(k): grip ∈ [0,1]
- S(k): stability ∈ [0,1]
- E(k): efficiency ∈ [0,1]
- N(k): novelty ∈ [0,1]

Default weights: w_G = w_S = w_E = w_N = 0.25

## License

MIT License - see [LICENSE](../LICENSE) for details.

## References

- OEIS A000081: Number of unlabeled rooted trees
- Butcher, J.C. (2016). *Numerical Methods for Ordinary Differential Equations*
- Hairer, E., Lubich, C., Wanner, G. (2006). *Geometric Numerical Integration*
- BSeries.jl: B-series expansions in Julia
- RootedTrees.jl: Rooted tree enumeration

---

**Ontogenetic Kernels**: Where B-series become genetic code, and numerical methods evolve like living organisms. 🧬🌳
