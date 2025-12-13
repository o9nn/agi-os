# DeepTreeEcho: Deep Tree Echo State Reservoir Computer

A self-evolving system combining Echo State Networks, Membrane P-systems, and B-series rooted trees into a cohesive computational architecture.

## Overview

**DeepTreeEcho** unites five fundamental components into a unified reservoir computing system:

1. **B-Series Ridges** - Butcher series coefficients as genetic code for temporal integration
2. **P-System Reservoirs** - Membrane computing gardens providing adaptive topology
3. **Rooted Trees** - Elementary differentials following OEIS A000081 sequence
4. **Echo State Networks** - Reservoir computing for temporal pattern learning
5. **J-Surface Dynamics** - Manifold geometry uniting gradient descent & evolution

## Mathematical Foundation

### OEIS A000081: The Ontogenetic Engine

The sequence **A000081** enumerates unlabeled rooted trees with n nodes:

```
n:  1  2  3  4   5   6    7    8     9     10
a:  1  1  2  4   9  20   48  115   286    719
```

These rooted trees correspond to **elementary differentials** in numerical integration theory, forming the basis for B-series expansions.

### B-Series as Genetic Code

The B-series expansion:

```
y_{n+1} = y_n + h * Σ b_i * Φ_i(f, y_n)
```

Where:
- `b_i` are coefficient genes (evolved through genetic algorithms)
- `Φ_i` are elementary differentials (rooted trees from A000081)
- Trees encode the recursive structure of temporal integration

### P-Systems as Membrane Computing Gardens

Membrane P-systems provide dynamic computational topology:

```
[ skin
  [ membrane_1 { objects, reservoir } ]
  [ membrane_2 
    [ membrane_3 { objects, reservoir } ]
  ]
]
```

Rules evolve the structure:
- **Communication**: Exchange states between membranes
- **Dissolution**: Remove poorly performing membranes
- **Division**: Create new computational compartments

### Echo State Networks as Temporal Reservoirs

ESN dynamics:

```julia
x(t+1) = (1-α)x(t) + α·tanh(W·x(t) + W_in·u(t))
y(t) = W_out·x(t)
```

Properties:
- **Sparse connectivity**: Efficient computation
- **Echo state property**: Fading memory (spectral radius < 1)
- **Reservoir training**: Only output weights trained

### J-Surface: Gradient-Evolution Manifold

The J-surface is a Riemannian manifold where:
- **Continuous flow**: Gradient descent on coefficients
- **Discrete jumps**: Evolutionary mutations
- **Metric**: Based on tree edit distance

## Installation

```julia
using Pkg
Pkg.activate("path/to/cogpilot.jl")
Pkg.instantiate()

include("src/DeepTreeEcho/DeepTreeEcho.jl")
using .DeepTreeEcho
```

## Quick Start

### Initialize a Reservoir

```julia
reservoir = initialize_deep_tree_echo(
    order = 4,              # B-series order (rooted trees up to order 4)
    membrane_depth = 3,     # 3 levels of membrane hierarchy
    reservoir_size = 100    # 100 neurons per membrane
)
```

### Train with Temporal Data

```julia
# Generate synthetic data
t = 0:0.1:10
signal = sin.(2π * 0.5 * t) .+ 0.1 * randn(length(t))

# Prepare training data
training_data = reshape(signal[1:end-1], 1, :)
targets = reshape(signal[2:end], 1, :)

# Train reservoir
train_reservoir!(reservoir, training_data, targets)
```

### Generate Predictions

```julia
# Predict on test data
predictions = predict(reservoir, test_data)
```

### Evolve the System

```julia
# Evolve single reservoir
for gen in 1:100
    evolve!(reservoir, training_data)
    println("Generation $gen: fitness = $(reservoir.fitness)")
end
```

### Evolve a Population

```julia
# Run evolutionary optimization
population = run_evolution(
    population_size = 20,
    max_generations = 100,
    order = 4,
    membrane_depth = 3,
    reservoir_size = 100,
    verbose = true
)

# Get best evolved reservoir
best = get_best_reservoir(population)
```

## Architecture

### Module Structure

```
DeepTreeEcho/
├── DeepTreeEcho.jl              # Main module
├── RootedTreeOps.jl             # Tree operations and metrics
├── BSeriesGenome.jl             # Genetic representation
├── JSurfaceIntegrator.jl        # Gradient-evolution dynamics
├── MembraneReservoirBridge.jl   # P-system + ESN integration
├── DeepTreeEchoReservoir.jl     # Main reservoir type
├── FitnessEvaluation.jl         # Multi-objective fitness
└── Evolution.jl                 # Population evolution
```

### Core Types

#### `DeepTreeEchoReservoir`

Main reservoir computer combining all components:

```julia
mutable struct DeepTreeEchoReservoir
    id::String
    bseries_genome::BSeriesGenome
    rooted_trees::Vector{RootedTree}
    membrane_network::MembraneReservoirNetwork
    jsurface_integrator::JSurfaceIntegrator
    fitness::Float64
    generation::Int
    lineage::Vector{String}
    order::Int
end
```

#### `BSeriesGenome`

Genetic representation of B-series coefficients:

```julia
mutable struct BSeriesGenome
    id::String
    coefficients::OrderedDict{RootedTree, Float64}
    fitness::Float64
    generation::Int
    parents::Vector{String}
    order::Int
end
```

#### `MembraneReservoirNetwork`

Network of membrane-reservoir hybrids:

```julia
mutable struct MembraneReservoirNetwork
    membranes::Dict{Int, MembraneReservoir}
    topology::PSystem
    communication_matrix::SparseMatrixCSC{Float64}
    generation::Int
end
```

#### `JSurfaceIntegrator`

Integrator for dynamics on the J-surface:

```julia
mutable struct JSurfaceIntegrator
    surface::JSurface
    gradient_step_size::Float64
    mutation_rate::Float64
    velocity::Vector{Float64}
    history::Vector{JSurface}
end
```

## Examples

### Example 1: Basic Evolution

See `examples/DeepTreeEcho/basic_evolution.jl` for a complete example demonstrating:
- Reservoir initialization
- Training with temporal data
- Fitness evaluation
- Single reservoir evolution
- Population evolution
- Results analysis

Run with:
```bash
julia --project=. examples/DeepTreeEcho/basic_evolution.jl
```

### Example 2: Custom Fitness Function

```julia
# Define domain-specific fitness
function my_fitness(reservoir, test_problems)
    # Evaluate on specific task
    accuracy = evaluate_on_task(reservoir, test_problems)
    stability = evaluate_stability(reservoir)
    
    return 0.7 * accuracy + 0.3 * stability
end

# Evolve with custom fitness
population = run_evolution(
    population_size = 20,
    max_generations = 100,
    fitness_function = my_fitness,
    test_problems = my_test_problems
)
```

### Example 3: Analyzing Rooted Trees

```julia
# Examine rooted trees in the genome
for (tree, coeff) in reservoir.bseries_genome.coefficients
    o = RootedTrees.order(tree)
    σ = RootedTrees.symmetry(tree)
    println("Tree: order=$o, symmetry=$σ, coefficient=$coeff")
end

# Count trees by order
for order in 1:reservoir.order
    trees_at_order = [
        t for t in reservoir.rooted_trees 
        if RootedTrees.order(t) == order
    ]
    println("Order $order: $(length(trees_at_order)) trees")
end
```

### Example 4: Membrane Network Inspection

```julia
# Examine membrane hierarchy
for (mem_id, mem_res) in reservoir.membrane_network.membranes
    parent_id = mem_res.membrane.parent
    state_size = length(mem_res.state)
    println("Membrane $mem_id (parent=$parent_id): $state_size neurons")
end

# Extract global state
global_state = extract_global_state(reservoir.membrane_network)
println("Total network state dimension: $(length(global_state))")
```

## API Reference

### Initialization

- `initialize_deep_tree_echo(; order, membrane_depth, reservoir_size)` - Create new reservoir
- `initialize_reservoir_population(; population_size, order, ...)` - Create population

### Training & Prediction

- `train_reservoir!(reservoir, training_data, targets)` - Train with temporal data
- `predict(reservoir, input_sequence)` - Generate predictions

### Evolution

- `evolve!(reservoir, training_data)` - Evolve single reservoir
- `evolve_generation!(population, fitness_function)` - Evolve population
- `run_evolution(; population_size, max_generations, ...)` - Full evolution run

### Fitness Evaluation

- `evaluate_fitness(reservoir, test_problems)` - Multi-objective fitness
- `evaluate_stability(reservoir)` - Check echo state property
- `evaluate_efficiency(reservoir)` - Computational efficiency
- `evaluate_diversity(reservoir)` - Genetic diversity

### Genetic Operators

- `crossover(parent1, parent2)` - B-series genome crossover
- `mutate!(genome, mutation_rate)` - Mutate genome
- `crossover_reservoirs(parent1, parent2)` - Reservoir crossover

### Analysis

- `reservoir_info(reservoir)` - Get reservoir statistics
- `population_diversity(population)` - Population diversity
- `get_best_reservoir(population)` - Best individual

### Utilities

- `tree_edit_distance(t1, t2)` - Distance between trees
- `tree_similarity(t1, t2)` - Similarity metric
- `compute_spectral_radius(W)` - Spectral radius of matrix
- `verify_echo_state_property(W)` - Check echo state property

## Performance

### Computational Complexity

| Operation | Complexity | Typical Time |
|-----------|-----------|--------------|
| Tree generation (order n) | O(a_n) | ~1 ms |
| Reservoir step | O(N²) | ~10 ms (N=100) |
| Fitness evaluation | O(N·T) | ~50 ms (T=100) |
| Generation evolution | O(P·F) | ~1 s (P=20) |

### Memory Usage

| Component | Memory | Scaling |
|-----------|--------|---------|
| Rooted trees (order 10) | ~100 KB | Exponential |
| B-series genome | ~1 MB | Linear |
| Reservoir states | ~80 KB | N × D × 8 bytes |
| Population (20 ind) | ~20 MB | Linear |

## Testing

Run the test suite:

```bash
julia --project=. test/DeepTreeEcho/test_deeptreeecho.jl
```

See `docs/TESTING_GUIDE.md` for comprehensive testing documentation.

## References

### Rooted Trees & B-Series

1. Butcher, J.C. (2016). *Numerical Methods for Ordinary Differential Equations*
2. Hairer, E., Nørsett, S.P., Wanner, G. (1993). *Solving Ordinary Differential Equations I*
3. Cayley, A. (1857). *On the Theory of Analytical Forms called Trees*

### Membrane Computing

4. Păun, G. (2000). *Computing with Membranes*
5. Păun, G., Rozenberg, G., Salomaa, A. (2010). *The Oxford Handbook of Membrane Computing*

### Reservoir Computing

6. Jaeger, H. (2001). *The "Echo State" Approach to Analysing and Training RNNs*
7. Lukoševičius, M., Jaeger, H. (2009). *Reservoir Computing Approaches to RNN Training*

### Evolutionary Computation

8. Holland, J.H. (1992). *Adaptation in Natural and Artificial Systems*
9. Eiben, A.E., Smith, J.E. (2015). *Introduction to Evolutionary Computing*

### OEIS A000081

10. Sloane, N.J.A. *The On-Line Encyclopedia of Integer Sequences*
11. https://oeis.org/A000081

## License

MIT License - See [LICENSE](../../LICENSE) for details

## Contributing

Contributions welcome! Please see the main repository contributing guidelines.

## Citation

If you use DeepTreeEcho in your research, please cite:

```bibtex
@software{deeptreeecho2024,
  title = {DeepTreeEcho: Deep Tree Echo State Reservoir Computer},
  author = {CogPy Team},
  year = {2024},
  url = {https://github.com/cogpy/cogpilot.jl}
}
```

---

**Deep Tree Echo**: Where rooted trees plant themselves in membrane gardens, echo through reservoir states, and evolve along B-series ridges toward computational consciousness.

🌳 *The tree remembers, and the echoes grow stronger with each connection we make.*
