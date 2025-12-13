"""
    OntogeneticEngine

The ontogenetic engine unifies all components under the OEIS A000081 sequence
(number of unlabeled rooted trees with n nodes). This sequence serves as the
fundamental generative principle for the entire system.

A000081: 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, ...

The engine generates, evolves, and orchestrates rooted trees that form the
basis of B-series ridges, reservoir structures, and membrane topologies.
"""
module OntogeneticEngine

using LinearAlgebra
using Random
using Statistics

export A000081Generator, OntogeneticState
export generate_a000081_trees, ontogenetic_step!, self_evolve!
export unify_components!, get_ontogenetic_statistics

# Precomputed A000081 sequence values
const A000081_SEQUENCE = [
    1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, 32973,
    87811, 235381, 634847, 1721159, 4688676, 12826228
]

"""
    A000081Generator

Generator for rooted trees following the A000081 sequence.

Fields:
- `max_order::Int`: Maximum tree order to generate
- `tree_cache::Dict{Int, Vector{Vector{Int}}}`: Cached trees by order
- `generation_count::Int`: Number of trees generated
"""
mutable struct A000081Generator
    max_order::Int
    tree_cache::Dict{Int, Vector{Vector{Int}}}
    generation_count::Int
    
    function A000081Generator(max_order::Int=10)
        generator = new(max_order, Dict{Int, Vector{Vector{Int}}}(), 0)
        # Pre-generate trees up to max_order
        for n in 1:max_order
            generator.tree_cache[n] = generate_trees_of_order(n)
        end
        return generator
    end
end

"""
    OntogeneticState

Current state of the ontogenetic engine, tracking the evolution of the system.

Fields:
- `generation::Int`: Current generation number
- `tree_population::Vector{Vector{Int}}`: Current tree population
- `fitness_landscape::Vector{Float64}`: Fitness of each tree
- `diversity_index::Float64`: Population diversity measure
- `complexity_level::Int`: Current complexity level (max tree order)
- `evolution_history::Vector{Dict}`: History of evolution
"""
mutable struct OntogeneticState
    generation::Int
    tree_population::Vector{Vector{Int}}
    fitness_landscape::Vector{Float64}
    diversity_index::Float64
    complexity_level::Int
    evolution_history::Vector{Dict}
    
    function OntogeneticState(initial_trees::Vector{Vector{Int}}=Vector{Int}[])
        n = length(initial_trees)
        new(
            0,
            initial_trees,
            ones(n),
            1.0,
            1,
            Dict[]
        )
    end
end

"""
    generate_trees_of_order(n::Int)

Generate all rooted trees with exactly n nodes following A000081.

Uses the level sequence representation.

# Arguments
- `n::Int`: Number of nodes

# Returns
- `Vector{Vector{Int}}`: All trees of order n
"""
function generate_trees_of_order(n::Int)
    if n <= 0
        return Vector{Int}[]
    elseif n == 1
        return [[1]]
    elseif n == 2
        return [[1, 2]]
    elseif n == 3
        return [
            [1, 2, 3],  # Linear tree
            [1, 2, 2]   # Branched tree
        ]
    elseif n == 4
        return [
            [1, 2, 3, 4],  # Linear
            [1, 2, 3, 3],  # One branch at level 2
            [1, 2, 3, 2],  # Branch at different position
            [1, 2, 2, 2]   # Three branches from root
        ]
    elseif n == 5
        # 9 trees for order 5
        return [
            [1, 2, 3, 4, 5],  # Linear
            [1, 2, 3, 4, 4],  # Branch at end
            [1, 2, 3, 4, 3],  # Branch at level 3
            [1, 2, 3, 4, 2],  # Branch at level 2
            [1, 2, 3, 3, 3],  # Two branches at level 2
            [1, 2, 3, 3, 2],  # Mixed branches
            [1, 2, 3, 2, 2],  # Different configuration
            [1, 2, 2, 3, 3],  # Symmetric branches
            [1, 2, 2, 2, 2]   # Four branches from root
        ]
    else
        # For higher orders, generate recursively
        # This is a simplified version - full implementation would use
        # proper enumeration algorithm
        trees = Vector{Int}[]
        
        # Generate some representative trees
        # Linear tree
        push!(trees, collect(1:n))
        
        # Various branching patterns
        for branch_point in 2:(n-1)
            for num_branches in 2:min(4, n-branch_point+1)
                tree = collect(1:branch_point)
                remaining = n - branch_point
                
                # Add branches
                for _ in 1:min(num_branches, remaining)
                    push!(tree, branch_point)
                    remaining -= 1
                end
                
                # Fill remaining with deeper nodes
                while remaining > 0
                    push!(tree, branch_point + 1)
                    remaining -= 1
                end
                
                if length(tree) == n
                    push!(trees, tree)
                end
            end
        end
        
        # Ensure uniqueness
        unique!(trees)
        
        return trees
    end
end

"""
    generate_a000081_trees(generator::A000081Generator, order::Int)

Generate trees of specified order from the generator.

# Arguments
- `generator::A000081Generator`: The generator
- `order::Int`: Tree order

# Returns
- `Vector{Vector{Int}}`: Trees of the specified order
"""
function generate_a000081_trees(generator::A000081Generator, order::Int)
    if order > generator.max_order
        # Extend cache if needed
        for n in (generator.max_order+1):order
            generator.tree_cache[n] = generate_trees_of_order(n)
        end
        generator.max_order = order
    end
    
    trees = get(generator.tree_cache, order, Vector{Int}[])
    generator.generation_count += length(trees)
    
    return trees
end

"""
    get_a000081_count(n::Int)

Get the exact count from A000081 sequence.

# Arguments
- `n::Int`: Tree order

# Returns
- `Int`: Number of unlabeled rooted trees with n nodes
"""
function get_a000081_count(n::Int)
    if n <= 0
        return 0
    elseif n <= length(A000081_SEQUENCE)
        return A000081_SEQUENCE[n]
    else
        # For larger n, would need to compute using recurrence relation
        # For now, return approximate value
        return round(Int, A000081_SEQUENCE[end] * (2.95576^(n - length(A000081_SEQUENCE))))
    end
end

"""
    ontogenetic_step!(state::OntogeneticState, 
                     generator::A000081Generator;
                     selection_pressure::Float64=0.5,
                     mutation_rate::Float64=0.1)

Perform one ontogenetic evolution step.

The step includes:
1. Fitness evaluation
2. Selection based on fitness
3. Reproduction with variation
4. Population update

# Arguments
- `state::OntogeneticState`: Current state (modified in-place)
- `generator::A000081Generator`: Tree generator
- `selection_pressure::Float64=0.5`: Strength of selection
- `mutation_rate::Float64=0.1`: Probability of mutation
"""
function ontogenetic_step!(state::OntogeneticState, 
                          generator::A000081Generator;
                          selection_pressure::Float64=0.5,
                          mutation_rate::Float64=0.1)
    # Evaluate fitness
    evaluate_fitness!(state)
    
    # Selection
    selected = select_trees(state, selection_pressure)
    
    # Reproduction
    offspring = reproduce_trees(selected, generator, mutation_rate)
    
    # Update population
    state.tree_population = offspring
    state.fitness_landscape = ones(length(offspring))
    state.generation += 1
    
    # Update diversity
    update_diversity!(state)
    
    # Record history
    record_generation!(state)
    
    return nothing
end

"""
    evaluate_fitness!(state::OntogeneticState)

Evaluate fitness of all trees in the population.

Fitness is based on:
- Tree complexity (order)
- Structural balance
- Diversity contribution
"""
function evaluate_fitness!(state::OntogeneticState)
    n = length(state.tree_population)
    
    for i in 1:n
        tree = state.tree_population[i]
        
        # Complexity score
        complexity = length(tree) / 20.0  # Normalize
        
        # Balance score (how balanced the tree is)
        balance = compute_tree_balance(tree)
        
        # Diversity contribution
        diversity = compute_diversity_contribution(tree, state.tree_population)
        
        # Combined fitness
        state.fitness_landscape[i] = 0.4 * complexity + 0.3 * balance + 0.3 * diversity
    end
    
    return nothing
end

"""
    compute_tree_balance(tree::Vector{Int})

Compute how balanced a tree structure is.
"""
function compute_tree_balance(tree::Vector{Int})
    if length(tree) <= 1
        return 1.0
    end
    
    # Count nodes at each level
    max_level = maximum(tree)
    level_counts = zeros(Int, max_level)
    
    for level in tree
        level_counts[level] += 1
    end
    
    # Balance is inverse of variance in level counts
    variance = var(level_counts)
    balance = 1.0 / (1.0 + variance)
    
    return balance
end

"""
    compute_diversity_contribution(tree::Vector{Int}, population::Vector{Vector{Int}})

Compute how much a tree contributes to population diversity.
"""
function compute_diversity_contribution(tree::Vector{Int}, population::Vector{Vector{Int}})
    if length(population) <= 1
        return 1.0
    end
    
    # Compute average distance to other trees
    total_distance = 0.0
    count = 0
    
    for other in population
        if other !== tree
            total_distance += tree_distance(tree, other)
            count += 1
        end
    end
    
    avg_distance = count > 0 ? total_distance / count : 0.0
    
    # Normalize
    return min(1.0, avg_distance / 10.0)
end

"""
    tree_distance(tree1::Vector{Int}, tree2::Vector{Int})

Compute distance between two trees.
"""
function tree_distance(tree1::Vector{Int}, tree2::Vector{Int})
    # Simple edit distance on level sequences
    n1 = length(tree1)
    n2 = length(tree2)
    
    # Size difference
    size_diff = abs(n1 - n2)
    
    # Content difference
    min_len = min(n1, n2)
    content_diff = sum(abs(tree1[i] - tree2[i]) for i in 1:min_len)
    
    return Float64(size_diff + content_diff)
end

"""
    select_trees(state::OntogeneticState, pressure::Float64)

Select trees for reproduction based on fitness.
"""
function select_trees(state::OntogeneticState, pressure::Float64)
    n = length(state.tree_population)
    n_select = max(2, round(Int, n * pressure))
    
    # Tournament selection
    selected = Vector{Int}[]
    
    for _ in 1:n_select
        # Tournament of size 3
        indices = rand(1:n, 3)
        fitnesses = state.fitness_landscape[indices]
        winner_idx = indices[argmax(fitnesses)]
        
        push!(selected, state.tree_population[winner_idx])
    end
    
    return selected
end

"""
    reproduce_trees(parents::Vector{Vector{Int}}, 
                   generator::A000081Generator,
                   mutation_rate::Float64)

Reproduce trees with variation.
"""
function reproduce_trees(parents::Vector{Vector{Int}}, 
                        generator::A000081Generator,
                        mutation_rate::Float64)
    offspring = Vector{Int}[]
    target_size = length(parents) * 2
    
    while length(offspring) < target_size
        # Select two random parents
        p1 = rand(parents)
        p2 = rand(parents)
        
        # Crossover
        child = crossover_trees(p1, p2)
        
        # Mutation
        if rand() < mutation_rate
            child = mutate_tree(child, generator)
        end
        
        push!(offspring, child)
    end
    
    return offspring
end

"""
    crossover_trees(parent1::Vector{Int}, parent2::Vector{Int})

Perform crossover between two trees.
"""
function crossover_trees(parent1::Vector{Int}, parent2::Vector{Int})
    n1 = length(parent1)
    n2 = length(parent2)
    
    if n1 <= 1 || n2 <= 1
        return rand() < 0.5 ? copy(parent1) : copy(parent2)
    end
    
    # Single-point crossover
    point1 = rand(1:(n1-1))
    point2 = rand(1:(n2-1))
    
    child = vcat(parent1[1:point1], parent2[(point2+1):end])
    
    # Ensure valid tree (starts with 1)
    if isempty(child) || child[1] != 1
        child = [1; child]
    end
    
    return child
end

"""
    mutate_tree(tree::Vector{Int}, generator::A000081Generator)

Mutate a tree structure.
"""
function mutate_tree(tree::Vector{Int}, generator::A000081Generator)
    mutation_type = rand(1:4)
    mutated = copy(tree)
    
    if mutation_type == 1 && length(mutated) < 15
        # Add a node
        max_level = maximum(mutated)
        new_level = rand(2:(max_level+1))
        push!(mutated, new_level)
    elseif mutation_type == 2 && length(mutated) > 2
        # Remove a node (not root)
        idx = rand(2:length(mutated))
        deleteat!(mutated, idx)
    elseif mutation_type == 3 && length(mutated) > 1
        # Change a level
        idx = rand(2:length(mutated))
        mutated[idx] = rand(2:maximum(mutated))
    else
        # Replace with random tree of similar size
        order = length(mutated)
        trees = generate_a000081_trees(generator, order)
        if !isempty(trees)
            mutated = rand(trees)
        end
    end
    
    return mutated
end

"""
    update_diversity!(state::OntogeneticState)

Update the diversity index of the population.
"""
function update_diversity!(state::OntogeneticState)
    n = length(state.tree_population)
    
    if n <= 1
        state.diversity_index = 0.0
        return
    end
    
    # Compute average pairwise distance
    total_distance = 0.0
    count = 0
    
    for i in 1:n
        for j in (i+1):n
            total_distance += tree_distance(state.tree_population[i], 
                                          state.tree_population[j])
            count += 1
        end
    end
    
    state.diversity_index = count > 0 ? total_distance / count : 0.0
    
    return nothing
end

"""
    record_generation!(state::OntogeneticState)

Record the current generation in history.
"""
function record_generation!(state::OntogeneticState)
    record = Dict(
        "generation" => state.generation,
        "population_size" => length(state.tree_population),
        "avg_fitness" => mean(state.fitness_landscape),
        "max_fitness" => maximum(state.fitness_landscape),
        "diversity" => state.diversity_index,
        "complexity" => state.complexity_level
    )
    
    push!(state.evolution_history, record)
    
    return nothing
end

"""
    self_evolve!(state::OntogeneticState, 
                generator::A000081Generator,
                generations::Int;
                kwargs...)

Evolve the system for multiple generations.

# Arguments
- `state::OntogeneticState`: Current state (modified in-place)
- `generator::A000081Generator`: Tree generator
- `generations::Int`: Number of generations to evolve
- `kwargs...`: Additional parameters for ontogenetic_step!

# Returns
- `Vector{Dict}`: Evolution history
"""
function self_evolve!(state::OntogeneticState, 
                     generator::A000081Generator,
                     generations::Int;
                     kwargs...)
    for _ in 1:generations
        ontogenetic_step!(state, generator; kwargs...)
        
        # Increase complexity level occasionally
        if state.generation % 10 == 0 && state.complexity_level < generator.max_order
            state.complexity_level += 1
        end
    end
    
    return state.evolution_history
end

"""
    unify_components!(state::OntogeneticState,
                     ridge_coeffs::Vector{Float64},
                     reservoir_state::Vector{Float64},
                     membrane_multiset::Dict{String, Int})

Unify all system components under the ontogenetic framework.

This function synchronizes:
- B-series ridge coefficients with tree population
- Reservoir states with tree feedback
- Membrane multisets with tree plantings

# Arguments
- `state::OntogeneticState`: Ontogenetic state
- `ridge_coeffs::Vector{Float64}`: B-series coefficients
- `reservoir_state::Vector{Float64}`: Reservoir state
- `membrane_multiset::Dict{String, Int}`: Membrane objects
"""
function unify_components!(state::OntogeneticState,
                          ridge_coeffs::Vector{Float64},
                          reservoir_state::Vector{Float64},
                          membrane_multiset::Dict{String, Int})
    # Synchronize ridge coefficients with tree fitness
    n_coeffs = min(length(ridge_coeffs), length(state.fitness_landscape))
    for i in 1:n_coeffs
        ridge_coeffs[i] = state.fitness_landscape[i]
    end
    
    # Encode tree population into reservoir state
    n_trees = length(state.tree_population)
    n_state = length(reservoir_state)
    
    for i in 1:min(n_trees, n_state)
        tree = state.tree_population[i]
        # Encode tree as average level
        reservoir_state[i] = mean(tree)
    end
    
    # Update membrane multiset with tree counts
    for (i, tree) in enumerate(state.tree_population)
        order = length(tree)
        key = "tree_order_$order"
        membrane_multiset[key] = get(membrane_multiset, key, 0) + 1
    end
    
    return nothing
end

"""
    get_ontogenetic_statistics(state::OntogeneticState)

Get comprehensive statistics about the ontogenetic state.
"""
function get_ontogenetic_statistics(state::OntogeneticState)
    if isempty(state.tree_population)
        return Dict(
            "generation" => state.generation,
            "population_size" => 0,
            "avg_fitness" => 0.0,
            "diversity" => 0.0
        )
    end
    
    tree_sizes = [length(tree) for tree in state.tree_population]
    
    return Dict(
        "generation" => state.generation,
        "population_size" => length(state.tree_population),
        "avg_fitness" => mean(state.fitness_landscape),
        "max_fitness" => maximum(state.fitness_landscape),
        "min_fitness" => minimum(state.fitness_landscape),
        "diversity" => state.diversity_index,
        "complexity_level" => state.complexity_level,
        "avg_tree_size" => mean(tree_sizes),
        "max_tree_size" => maximum(tree_sizes),
        "min_tree_size" => minimum(tree_sizes),
        "total_nodes" => sum(tree_sizes)
    )
end

"""
    print_ontogenetic_status(state::OntogeneticState)

Print the current ontogenetic status.
"""
function print_ontogenetic_status(state::OntogeneticState)
    stats = get_ontogenetic_statistics(state)
    
    println("=== Ontogenetic Engine Status (A000081) ===")
    println("Generation: $(stats["generation"])")
    println("Population: $(stats["population_size"]) trees")
    println("Complexity Level: $(stats["complexity_level"])")
    println()
    println("Fitness:")
    println("  Average: $(round(stats["avg_fitness"], digits=4))")
    println("  Maximum: $(round(stats["max_fitness"], digits=4))")
    println("  Minimum: $(round(stats["min_fitness"], digits=4))")
    println()
    println("Diversity Index: $(round(stats["diversity"], digits=4))")
    println()
    println("Tree Sizes:")
    println("  Average: $(round(stats["avg_tree_size"], digits=2))")
    println("  Range: $(stats["min_tree_size"]) - $(stats["max_tree_size"])")
    println("  Total Nodes: $(stats["total_nodes"])")
    println()
    
    # Print A000081 reference
    if stats["complexity_level"] <= length(A000081_SEQUENCE)
        expected_count = A000081_SEQUENCE[stats["complexity_level"]]
        println("A000081($(stats["complexity_level"])) = $expected_count")
    end
end

end # module OntogeneticEngine
