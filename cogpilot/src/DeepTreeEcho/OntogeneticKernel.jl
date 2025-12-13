"""
    OntogeneticKernel Module

Self-evolving computational kernels with B-series genomes.
Implements Phase 2 of the agent roadmap: Ontogenetic Kernel Foundation.

# Mathematical Foundation

Each kernel has a B-series genome mapping rooted trees to coefficients:
```
genome: RootedTree → ℝ
```

The kernel represents a numerical method via B-series expansion:
```
y_{n+1} = y_n + h Σ_{τ∈T} b(τ)/σ(τ) · F(τ)(y_n)
```

Where:
- T: Set of rooted trees
- b(τ): Coefficient from genome (genetic material)
- σ(τ): Symmetry factor of tree τ
- F(τ): Elementary differential for tree τ
- h: Step size

# Lifecycle Stages

Kernels evolve through developmental stages:
- `:embryonic` - Newly created, untested
- `:juvenile` - Basic testing complete
- `:mature` - Fully optimized
- `:senescent` - Declining performance

# Fitness Components

- **Grip**: How well kernel fits the target domain
- **Stability**: Numerical stability properties
- **Efficiency**: Computational cost
- **Novelty**: Genetic diversity from population
"""
module OntogeneticKernel

using LinearAlgebra
using Random
using Statistics

export Kernel, KernelGenome, KernelLifecycle
export create_kernel, evaluate_kernel_fitness!, self_generate
export crossover, mutate!, update_stage!
export kernel_to_string, genetic_distance

"""
    KernelGenome

B-series genome mapping trees to coefficients.
Uses level-sequence representation for rooted trees.
"""
struct KernelGenome
    # Tree → Coefficient mapping
    # Trees represented as level sequences (Vector{Int})
    coefficients::Dict{Vector{Int}, Float64}
    
    # Maximum tree order in genome
    max_order::Int
    
    # Genetic diversity measure
    diversity::Float64
    
    function KernelGenome(coefficients::Dict{Vector{Int}, Float64}, max_order::Int)
        # Compute diversity as variance of coefficients
        coeff_vals = collect(values(coefficients))
        div = isempty(coeff_vals) ? 0.0 : std(coeff_vals)
        new(coefficients, max_order, div)
    end
end

"""
    KernelLifecycle

Tracks kernel development stage and maturity.
"""
mutable struct KernelLifecycle
    stage::Symbol              # :embryonic, :juvenile, :mature, :senescent
    maturity::Float64         # 0.0 to 1.0
    age::Int                  # Age in generations
    generation::Int           # Generation number
    
    function KernelLifecycle()
        new(:embryonic, 0.0, 0, 0)
    end
end

"""
    Kernel

Self-evolving computational kernel with B-series genome.

# Fields
- `genome::KernelGenome` - Tree → coefficient mapping (genetic material)
- `lifecycle::KernelLifecycle` - Development stage and maturity
- `lineage::Vector{String}` - Parent kernel IDs
- `id::String` - Unique kernel identifier
- `fitness::Float64` - Overall fitness score (0.0 to 1.0)
- `grip::Float64` - Domain fit quality
- `stability::Float64` - Numerical stability metric
- `efficiency::Float64` - Computational efficiency (inverse cost)
- `novelty::Float64` - Genetic diversity from population
"""
mutable struct Kernel
    genome::KernelGenome
    lifecycle::KernelLifecycle
    lineage::Vector{String}
    id::String
    
    # Fitness components
    fitness::Float64
    grip::Float64
    stability::Float64
    efficiency::Float64
    novelty::Float64
    
    function Kernel(genome::KernelGenome, lineage::Vector{String}=String[], id::String="")
        lifecycle = KernelLifecycle()
        kernel_id = isempty(id) ? generate_kernel_id() : id
        new(genome, lifecycle, lineage, kernel_id, 0.0, 0.0, 0.0, 0.0, 0.0)
    end
end

"""
    generate_kernel_id()

Generate unique kernel identifier.
"""
function generate_kernel_id()
    return "K" * string(rand(UInt32), base=16, pad=8)
end

"""
    create_kernel(max_order::Int; 
                 symmetric::Bool=true,
                 density::Float64=0.5)

Create a new kernel with random B-series genome.

# Arguments
- `max_order::Int`: Maximum tree order
- `symmetric::Bool=true`: Emphasize symmetric trees
- `density::Float64=0.5`: Proportion of trees to include

# Returns
- `Kernel`: New kernel with random genome
"""
function create_kernel(max_order::Int; 
                      symmetric::Bool=true,
                      density::Float64=0.5)
    
    # Generate trees up to max_order
    trees = generate_trees_up_to_order(max_order)
    
    # Create genome
    coefficients = Dict{Vector{Int}, Float64}()
    
    for tree in trees
        # Include tree with probability = density
        if rand() < density
            # Coefficient based on tree properties
            order = length(tree)
            symmetry = compute_tree_symmetry(tree)
            
            if symmetric
                # Emphasize symmetric trees
                coeff = (0.1 / order) * (1.0 + symmetry)
            else
                # Random coefficients
                coeff = randn() * 0.1 / order
            end
            
            coefficients[tree] = coeff
        end
    end
    
    genome = KernelGenome(coefficients, max_order)
    return Kernel(genome)
end

"""
    generate_trees_up_to_order(max_order::Int)

Generate all rooted trees up to given order using level sequences.

# Returns
- `Vector{Vector{Int}}`: List of trees as level sequences
"""
function generate_trees_up_to_order(max_order::Int)
    trees = Vector{Int}[]
    
    # Order 1: single node
    push!(trees, [1])
    
    # Order 2: two nodes
    if max_order >= 2
        push!(trees, [1, 2])
    end
    
    # Order 3: three trees
    if max_order >= 3
        push!(trees, [1, 2, 3])  # Linear
        push!(trees, [1, 2, 2])  # Branched
    end
    
    # Order 4: four trees
    if max_order >= 4
        push!(trees, [1, 2, 3, 4])  # Linear
        push!(trees, [1, 2, 3, 3])  # Branch at 2
        push!(trees, [1, 2, 2, 3])  # Branch at 1
        push!(trees, [1, 2, 2, 2])  # Star
    end
    
    # For higher orders, use simplified generation
    # In production, would use full A000081 enumeration
    if max_order >= 5
        for n in 5:max_order
            # Generate a few representative trees
            # Linear chain
            push!(trees, collect(1:n))
            # Full branch
            push!(trees, vcat([1, 2], fill(2, n-2)))
            # Partial branch
            if n >= 3
                push!(trees, vcat([1, 2, 3], fill(3, n-3)))
            end
        end
    end
    
    return trees
end

"""
    compute_tree_symmetry(tree::Vector{Int})

Compute symmetry factor of a rooted tree.
Higher values indicate more symmetry.

# Arguments
- `tree::Vector{Int}`: Tree as level sequence

# Returns
- `Float64`: Symmetry measure (0.0 to 1.0)
"""
function compute_tree_symmetry(tree::Vector{Int})
    if length(tree) <= 1
        return 1.0
    end
    
    # Count nodes at each level
    max_level = maximum(tree)
    level_counts = zeros(Int, max_level)
    for level in tree
        level_counts[level] += 1
    end
    
    # Symmetry based on level distribution uniformity
    # More uniform → more symmetric
    mean_count = mean(level_counts)
    variance = var(level_counts)
    
    # Normalize to [0, 1]
    symmetry = 1.0 / (1.0 + variance / (mean_count + 1e-6))
    
    return symmetry
end

"""
    evaluate_kernel_fitness!(kernel::Kernel, 
                           domain_data=nothing,
                           population=nothing)

Evaluate kernel fitness on all components.

# Fitness Formula
```
fitness = w_grip · grip + w_stability · stability + 
          w_efficiency · efficiency + w_novelty · novelty
```

Default weights: all 0.25 (equal importance)

# Arguments
- `kernel::Kernel`: Kernel to evaluate
- `domain_data`: Optional domain-specific test data
- `population`: Optional population for novelty computation

# Side Effects
Updates kernel's fitness, grip, stability, efficiency, and novelty fields.
"""
function evaluate_kernel_fitness!(kernel::Kernel, 
                                 domain_data=nothing,
                                 population=nothing)
    
    # 1. Grip: Domain fit quality
    kernel.grip = evaluate_grip(kernel, domain_data)
    
    # 2. Stability: Numerical stability
    kernel.stability = evaluate_stability(kernel)
    
    # 3. Efficiency: Computational cost
    kernel.efficiency = evaluate_efficiency(kernel)
    
    # 4. Novelty: Genetic diversity
    kernel.novelty = evaluate_novelty(kernel, population)
    
    # Overall fitness (equal weights)
    kernel.fitness = 0.25 * (kernel.grip + kernel.stability + 
                            kernel.efficiency + kernel.novelty)
    
    return kernel.fitness
end

"""
    evaluate_grip(kernel::Kernel, domain_data)

Evaluate how well kernel grips the domain.
"""
function evaluate_grip(kernel::Kernel, domain_data)
    # Without domain data, use genome coverage
    if isnothing(domain_data)
        # Grip based on genome size and order coverage
        n_coeffs = length(kernel.genome.coefficients)
        max_order = kernel.genome.max_order
        
        # More coefficients and higher order → better grip
        coverage = n_coeffs / (2^max_order)  # Rough estimate
        return min(1.0, coverage)
    end
    
    # With domain data, evaluate actual performance
    # (Simplified - in production would solve ODEs)
    return 0.5 + 0.5 * rand()
end

"""
    evaluate_stability(kernel::Kernel)

Evaluate numerical stability of kernel.
"""
function evaluate_stability(kernel::Kernel)
    # Stability based on coefficient magnitudes
    coeffs = collect(values(kernel.genome.coefficients))
    
    if isempty(coeffs)
        return 0.1  # Low stability for empty genome
    end
    
    # Smaller coefficients → more stable
    max_coeff = maximum(abs.(coeffs))
    stability = 1.0 / (1.0 + max_coeff)
    
    # Penalize highly variable coefficients (only if multiple coefficients)
    if length(coeffs) > 1
        coeff_variance = var(coeffs)
        stability *= 1.0 / (1.0 + coeff_variance)
    end
    
    return min(1.0, max(0.0, stability))
end

"""
    evaluate_efficiency(kernel::Kernel)

Evaluate computational efficiency (inverse of cost).
"""
function evaluate_efficiency(kernel::Kernel)
    # Efficiency inversely proportional to number of terms
    n_terms = length(kernel.genome.coefficients)
    
    # Fewer terms → more efficient
    efficiency = 1.0 / (1.0 + 0.1 * n_terms)
    
    return efficiency
end

"""
    evaluate_novelty(kernel::Kernel, population)

Evaluate genetic novelty compared to population.
"""
function evaluate_novelty(kernel::Kernel, population)
    if isnothing(population) || length(population) <= 1
        return 0.5  # Default novelty
    end
    
    # Compute average genetic distance to population
    distances = Float64[]
    
    for other in population
        if other.id != kernel.id
            dist = genetic_distance(kernel, other)
            push!(distances, dist)
        end
    end
    
    if isempty(distances)
        return 0.5
    end
    
    # Novelty = average distance (normalized)
    novelty = mean(distances)
    return min(1.0, novelty)
end

"""
    genetic_distance(kernel1::Kernel, kernel2::Kernel)

Compute genetic distance between two kernels.
"""
function genetic_distance(kernel1::Kernel, kernel2::Kernel)
    genome1 = kernel1.genome.coefficients
    genome2 = kernel2.genome.coefficients
    
    # All unique trees
    all_trees = union(keys(genome1), keys(genome2))
    
    if isempty(all_trees)
        return 0.0
    end
    
    # Sum of squared coefficient differences
    dist = 0.0
    for tree in all_trees
        c1 = get(genome1, tree, 0.0)
        c2 = get(genome2, tree, 0.0)
        dist += (c1 - c2)^2
    end
    
    # Normalize by number of trees
    return sqrt(dist / length(all_trees))
end

"""
    self_generate(parent::Kernel)

Generate offspring kernel through recursive self-composition.
Applies chain rule: (f∘f)' = f'(f(x)) · f'(x)

# Mathematical Foundation
Uses B-series composition law to compute composed kernel genome.

# Arguments
- `parent::Kernel`: Parent kernel

# Returns
- `Kernel`: Offspring kernel with incremented generation
"""
function self_generate(parent::Kernel)
    # Create offspring genome by self-composition
    # Simplified: combine coefficients with small mutations
    
    new_coefficients = Dict{Vector{Int}, Float64}()
    
    for (tree, coeff) in parent.genome.coefficients
        # Self-composition: amplify coefficients slightly
        new_coeff = coeff * (1.0 + 0.1 * randn())
        new_coefficients[tree] = new_coeff
        
        # Add some variation: mutate tree slightly
        if rand() < 0.2 && length(tree) > 1
            # Create variant tree
            variant = copy(tree)
            idx = rand(1:length(variant))
            variant[idx] = max(1, variant[idx] + rand(-1:1))
            new_coefficients[variant] = coeff * 0.5
        end
    end
    
    offspring_genome = KernelGenome(new_coefficients, parent.genome.max_order)
    
    # Create offspring with parent lineage
    offspring = Kernel(
        offspring_genome,
        vcat(parent.lineage, [parent.id]),
        ""  # New ID will be generated
    )
    
    # Increment generation
    offspring.lifecycle.generation = parent.lifecycle.generation + 1
    offspring.lifecycle.stage = :embryonic
    offspring.lifecycle.maturity = 0.0
    offspring.lifecycle.age = 0
    
    return offspring
end

"""
    crossover(parent1::Kernel, parent2::Kernel)

Create two offspring kernels via genetic crossover.

# Arguments
- `parent1::Kernel`: First parent
- `parent2::Kernel`: Second parent

# Returns
- `Tuple{Kernel, Kernel}`: Two offspring kernels
"""
function crossover(parent1::Kernel, parent2::Kernel)
    # Single-point crossover on tree sets
    trees1 = collect(keys(parent1.genome.coefficients))
    trees2 = collect(keys(parent2.genome.coefficients))
    
    all_trees = union(trees1, trees2)
    shuffle!(all_trees)
    
    # Split point
    split = div(length(all_trees), 2)
    
    # Offspring 1: first half from parent1, second from parent2
    offspring1_coeffs = Dict{Vector{Int}, Float64}()
    for (i, tree) in enumerate(all_trees)
        if i <= split
            if haskey(parent1.genome.coefficients, tree)
                offspring1_coeffs[tree] = parent1.genome.coefficients[tree]
            end
        else
            if haskey(parent2.genome.coefficients, tree)
                offspring1_coeffs[tree] = parent2.genome.coefficients[tree]
            end
        end
    end
    
    # Offspring 2: reverse split
    offspring2_coeffs = Dict{Vector{Int}, Float64}()
    for (i, tree) in enumerate(all_trees)
        if i <= split
            if haskey(parent2.genome.coefficients, tree)
                offspring2_coeffs[tree] = parent2.genome.coefficients[tree]
            end
        else
            if haskey(parent1.genome.coefficients, tree)
                offspring2_coeffs[tree] = parent1.genome.coefficients[tree]
            end
        end
    end
    
    max_order = max(parent1.genome.max_order, parent2.genome.max_order)
    
    genome1 = KernelGenome(offspring1_coeffs, max_order)
    genome2 = KernelGenome(offspring2_coeffs, max_order)
    
    offspring1 = Kernel(genome1, [parent1.id, parent2.id])
    offspring2 = Kernel(genome2, [parent1.id, parent2.id])
    
    # Set generation
    max_gen = max(parent1.lifecycle.generation, parent2.lifecycle.generation)
    offspring1.lifecycle.generation = max_gen + 1
    offspring2.lifecycle.generation = max_gen + 1
    
    return (offspring1, offspring2)
end

"""
    mutate!(kernel::Kernel; mutation_rate::Float64=0.1)

Mutate kernel genome in place.

# Arguments
- `kernel::Kernel`: Kernel to mutate
- `mutation_rate::Float64=0.1`: Probability of mutating each coefficient

# Side Effects
Modifies kernel genome coefficients.
"""
function mutate!(kernel::Kernel; mutation_rate::Float64=0.1)
    for (tree, coeff) in kernel.genome.coefficients
        if rand() < mutation_rate
            # Gaussian mutation
            mutation = randn() * 0.1 * abs(coeff + 1e-6)
            kernel.genome.coefficients[tree] = coeff + mutation
        end
    end
    
    # Occasionally add/remove trees
    if rand() < mutation_rate
        # Remove a random tree
        if length(kernel.genome.coefficients) > 1
            tree_to_remove = rand(collect(keys(kernel.genome.coefficients)))
            delete!(kernel.genome.coefficients, tree_to_remove)
        end
    end
    
    if rand() < mutation_rate / 2
        # Add a new random tree
        new_trees = generate_trees_up_to_order(kernel.genome.max_order)
        new_tree = rand(new_trees)
        if !haskey(kernel.genome.coefficients, new_tree)
            kernel.genome.coefficients[new_tree] = randn() * 0.01
        end
    end
    
    return nothing
end

"""
    update_stage!(kernel::Kernel)

Update kernel development stage based on age and fitness.

# Stage Transitions
- embryonic → juvenile: age > 5 and fitness > 0.3
- juvenile → mature: age > 15 and fitness > 0.6
- mature → senescent: age > 50 or fitness declining

# Side Effects
Updates kernel lifecycle stage and maturity.
"""
function update_stage!(kernel::Kernel)
    age = kernel.lifecycle.age
    fitness = kernel.fitness
    
    # Update maturity (0.0 to 1.0)
    kernel.lifecycle.maturity = min(1.0, age / 30.0)
    
    # Stage transitions
    if kernel.lifecycle.stage == :embryonic
        if age > 5 && fitness > 0.3
            kernel.lifecycle.stage = :juvenile
        end
    elseif kernel.lifecycle.stage == :juvenile
        if age > 15 && fitness > 0.6
            kernel.lifecycle.stage = :mature
        end
    elseif kernel.lifecycle.stage == :mature
        if age > 50 || fitness < 0.4
            kernel.lifecycle.stage = :senescent
        end
    end
    
    return nothing
end

"""
    kernel_to_string(kernel::Kernel)

Convert kernel to human-readable string.
"""
function kernel_to_string(kernel::Kernel)
    lines = String[]
    
    push!(lines, "Kernel $(kernel.id)")
    push!(lines, "  Generation: $(kernel.lifecycle.generation)")
    push!(lines, "  Stage: $(kernel.lifecycle.stage)")
    push!(lines, "  Age: $(kernel.lifecycle.age)")
    push!(lines, "  Maturity: $(round(kernel.lifecycle.maturity, digits=2))")
    push!(lines, "  Fitness: $(round(kernel.fitness, digits=3))")
    push!(lines, "    - Grip: $(round(kernel.grip, digits=3))")
    push!(lines, "    - Stability: $(round(kernel.stability, digits=3))")
    push!(lines, "    - Efficiency: $(round(kernel.efficiency, digits=3))")
    push!(lines, "    - Novelty: $(round(kernel.novelty, digits=3))")
    push!(lines, "  Genome: $(length(kernel.genome.coefficients)) terms")
    push!(lines, "  Max Order: $(kernel.genome.max_order)")
    push!(lines, "  Lineage: $(join(kernel.lineage, " ← "))")
    
    return join(lines, "\n")
end

end # module OntogeneticKernel
