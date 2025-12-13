"""
    MembraneGardenCore

Membrane computing gardens where rooted trees from A000081 are planted, grown,
and cross-pollinated, with feedback loops connecting to reservoirs and J-surfaces.

# The Garden Metaphor

The membrane garden is where:
1. **Trees are Planted**: Rooted trees from A000081 are planted in membrane "soil"
2. **Trees Grow**: Trees evolve through mutation and selection
3. **Cross-Pollination**: Trees exchange genetic material across membranes
4. **Feedback Harvest**: Tree fitness feeds back to reservoir and J-surface
5. **Pruning**: Low-fitness trees are removed
6. **Seeding**: New trees are generated from successful lineages

# Mathematical Foundation

Each tree τ ∈ Garden has:
- **Structure**: Level sequence representation
- **Fitness**: f(τ) based on reservoir performance
- **Age**: Number of generations survived
- **Lineage**: Parent trees and mutations
- **Membrane**: Which membrane it's planted in

The garden evolves through:
    
    τ(t+1) = select(mutate(crossover(τ(t))), fitness)

With feedback:
    
    fitness(τ) = performance(reservoir(τ)) + energy(jsurface(τ))

# Feedback Loop Architecture

```
    Trees → Reservoir Connectivity → Performance
      ↑                                    ↓
      ←─────── Fitness Feedback ──────────┘
      ↓
    Mutation & Selection
      ↓
    New Trees → Plant in Membranes
```

This creates a self-organizing system where structure and dynamics co-evolve.
"""
module MembraneGardenCore

using LinearAlgebra
using Random
using Statistics

export MembraneGarden, PlantedTree
export create_garden, plant_tree!, grow_garden!
export cross_pollinate_trees!, prune_garden!, harvest_garden_feedback!
export compute_tree_fitness, get_garden_statistics

"""
    PlantedTree

A rooted tree planted in the membrane garden.

# Fields
- `tree::Vector{Int}`: Level sequence representation
- `membrane_id::Int`: Which membrane it's planted in
- `fitness::Float64`: Current fitness score
- `age::Int`: Generations survived
- `parent_trees::Vector{Vector{Int}}`: Parent lineage
- `mutations::Int`: Number of mutations applied
- `energy_contribution::Float64`: Contribution to J-surface energy
- `reservoir_performance::Float64`: Reservoir performance metric
"""
mutable struct PlantedTree
    tree::Vector{Int}
    membrane_id::Int
    fitness::Float64
    age::Int
    parent_trees::Vector{Vector{Int}}
    mutations::Int
    energy_contribution::Float64
    reservoir_performance::Float64
    
    function PlantedTree(tree::Vector{Int}, membrane_id::Int)
        new(tree, membrane_id, 0.0, 0, Vector{Int}[], 0, 0.0, 0.0)
    end
end

"""
    GrowthDynamics

Parameters controlling garden growth and evolution.
"""
struct GrowthDynamics
    growth_rate::Float64
    mutation_rate::Float64
    crossover_rate::Float64
    selection_pressure::Float64
    pruning_threshold::Float64
    
    function GrowthDynamics(;
        growth_rate::Float64=0.1,
        mutation_rate::Float64=0.05,
        crossover_rate::Float64=0.3,
        selection_pressure::Float64=0.5,
        pruning_threshold::Float64=0.1
    )
        new(growth_rate, mutation_rate, crossover_rate, 
            selection_pressure, pruning_threshold)
    end
end

"""
    MembraneGarden

The membrane computing garden where trees grow and evolve.

# Fields
- `planted_trees::Vector{PlantedTree}`: All planted trees
- `membrane_trees::Dict{Int, Vector{Int}}`: Tree indices by membrane
- `dynamics::GrowthDynamics`: Growth parameters
- `generation::Int`: Current generation
- `fitness_history::Vector{Float64}`: Average fitness over time
- `diversity_history::Vector{Float64}`: Diversity over time
- `feedback_signals::Dict{String, Vector{Float64}}`: Feedback from reservoirs
"""
mutable struct MembraneGarden
    planted_trees::Vector{PlantedTree}
    membrane_trees::Dict{Int, Vector{Int}}
    dynamics::GrowthDynamics
    generation::Int
    fitness_history::Vector{Float64}
    diversity_history::Vector{Float64}
    feedback_signals::Dict{String, Vector{Float64}}
    
    function MembraneGarden(dynamics::GrowthDynamics=GrowthDynamics())
        new(PlantedTree[], Dict{Int, Vector{Int}}(), dynamics, 0, 
            Float64[], Float64[], Dict{String, Vector{Float64}}())
    end
end

"""
    create_garden(; kwargs...)

Create a new membrane garden.
"""
function create_garden(;
    growth_rate::Float64=0.1,
    mutation_rate::Float64=0.05,
    crossover_rate::Float64=0.3,
    selection_pressure::Float64=0.5,
    pruning_threshold::Float64=0.1
)
    dynamics = GrowthDynamics(
        growth_rate=growth_rate,
        mutation_rate=mutation_rate,
        crossover_rate=crossover_rate,
        selection_pressure=selection_pressure,
        pruning_threshold=pruning_threshold
    )
    return MembraneGarden(dynamics)
end

"""
    plant_tree!(garden::MembraneGarden, tree::Vector{Int}, membrane_id::Int)

Plant a tree in the garden.
"""
function plant_tree!(garden::MembraneGarden, tree::Vector{Int}, membrane_id::Int)
    planted = PlantedTree(tree, membrane_id)
    push!(garden.planted_trees, planted)
    
    # Update membrane index
    if !haskey(garden.membrane_trees, membrane_id)
        garden.membrane_trees[membrane_id] = Int[]
    end
    push!(garden.membrane_trees[membrane_id], length(garden.planted_trees))
    
    return length(garden.planted_trees)
end

"""
    compute_tree_fitness(tree::PlantedTree, 
                        reservoir_feedback::Dict{String, Float64},
                        jsurface_feedback::Dict{String, Float64})

Compute fitness of a tree based on reservoir and J-surface feedback.
"""
function compute_tree_fitness(
    tree::PlantedTree,
    reservoir_feedback::Dict{String, Float64},
    jsurface_feedback::Dict{String, Float64}
)
    # Base fitness from tree structure
    order = length(tree.tree)
    structure_fitness = 1.0 / (1.0 + order)  # Prefer smaller trees
    
    # Reservoir performance contribution
    reservoir_activity = get(reservoir_feedback, "activity", 0.0)
    reservoir_stability = get(reservoir_feedback, "stability", 0.0)
    reservoir_fitness = 0.5 * (reservoir_activity + reservoir_stability)
    
    # J-surface energy contribution
    energy = get(jsurface_feedback, "energy", 0.0)
    gradient_norm = get(jsurface_feedback, "gradient_norm", 0.0)
    jsurface_fitness = 1.0 / (1.0 + abs(energy) + gradient_norm)
    
    # Age bonus (survival of the fittest)
    age_bonus = min(tree.age * 0.01, 0.2)
    
    # Combined fitness
    fitness = 0.3 * structure_fitness + 
              0.4 * reservoir_fitness + 
              0.2 * jsurface_fitness + 
              0.1 * age_bonus
    
    # Update tree metrics
    tree.reservoir_performance = reservoir_fitness
    tree.energy_contribution = jsurface_fitness
    tree.fitness = fitness
    
    return fitness
end

"""
    mutate_tree(tree::Vector{Int}, mutation_rate::Float64)

Mutate a tree structure.
"""
function mutate_tree(tree::Vector{Int}, mutation_rate::Float64)
    if rand() > mutation_rate
        return copy(tree)
    end
    
    mutated = copy(tree)
    
    # Choose mutation type
    mutation_type = rand(1:4)
    
    if mutation_type == 1 && length(mutated) > 1
        # Remove a node
        idx = rand(2:length(mutated))
        deleteat!(mutated, idx)
        
    elseif mutation_type == 2
        # Add a node
        if length(mutated) < 20  # Max size limit
            parent_level = rand(mutated)
            push!(mutated, parent_level + 1)
        end
        
    elseif mutation_type == 3 && length(mutated) > 1
        # Change a level
        idx = rand(2:length(mutated))
        max_level = maximum(mutated[1:idx-1]) + 1
        mutated[idx] = rand(2:max_level)
        
    else
        # Swap two nodes
        if length(mutated) >= 2
            idx1, idx2 = rand(2:length(mutated), 2)
            mutated[idx1], mutated[idx2] = mutated[idx2], mutated[idx1]
        end
    end
    
    # Ensure valid tree (starts with 1)
    if !isempty(mutated) && mutated[1] != 1
        mutated[1] = 1
    end
    
    return mutated
end

"""
    crossover_trees(tree1::Vector{Int}, tree2::Vector{Int})

Perform crossover between two trees.
"""
function crossover_trees(tree1::Vector{Int}, tree2::Vector{Int})
    if isempty(tree1)
        return copy(tree2)
    elseif isempty(tree2)
        return copy(tree1)
    end
    
    # Single-point crossover
    len1 = length(tree1)
    len2 = length(tree2)
    
    point1 = rand(1:len1)
    point2 = rand(1:len2)
    
    # Create offspring
    offspring = vcat(tree1[1:point1], tree2[(point2+1):end])
    
    # Ensure valid
    if !isempty(offspring) && offspring[1] != 1
        offspring[1] = 1
    end
    
    return offspring
end

"""
    grow_garden!(garden::MembraneGarden, num_generations::Int,
                reservoir_feedback::Dict{String, Float64},
                jsurface_feedback::Dict{String, Float64})

Grow the garden for multiple generations with feedback.
"""
function grow_garden!(
    garden::MembraneGarden,
    num_generations::Int,
    reservoir_feedback::Dict{String, Float64}=Dict{String, Float64}(),
    jsurface_feedback::Dict{String, Float64}=Dict{String, Float64}()
)
    for gen in 1:num_generations
        # Update fitness for all trees
        for tree in garden.planted_trees
            compute_tree_fitness(tree, reservoir_feedback, jsurface_feedback)
            tree.age += 1
        end
        
        # Selection: keep top performers
        if !isempty(garden.planted_trees)
            fitnesses = [t.fitness for t in garden.planted_trees]
            threshold = quantile(fitnesses, garden.dynamics.selection_pressure)
            
            selected_indices = findall(f -> f >= threshold, fitnesses)
            selected_trees = garden.planted_trees[selected_indices]
            
            # Mutation
            new_trees = PlantedTree[]
            for tree in selected_trees
                if rand() < garden.dynamics.mutation_rate
                    mutated = mutate_tree(tree.tree, 1.0)
                    new_tree = PlantedTree(mutated, tree.membrane_id)
                    new_tree.parent_trees = [tree.tree]
                    new_tree.mutations = tree.mutations + 1
                    push!(new_trees, new_tree)
                end
            end
            
            # Crossover
            if length(selected_trees) >= 2 && rand() < garden.dynamics.crossover_rate
                parent1 = rand(selected_trees)
                parent2 = rand(selected_trees)
                offspring_tree = crossover_trees(parent1.tree, parent2.tree)
                offspring = PlantedTree(offspring_tree, parent1.membrane_id)
                offspring.parent_trees = [parent1.tree, parent2.tree]
                push!(new_trees, offspring)
            end
            
            # Add new trees to garden
            for new_tree in new_trees
                push!(garden.planted_trees, new_tree)
                membrane_id = new_tree.membrane_id
                if !haskey(garden.membrane_trees, membrane_id)
                    garden.membrane_trees[membrane_id] = Int[]
                end
                push!(garden.membrane_trees[membrane_id], length(garden.planted_trees))
            end
        end
        
        garden.generation += 1
        
        # Record statistics
        if !isempty(garden.planted_trees)
            avg_fitness = mean([t.fitness for t in garden.planted_trees])
            push!(garden.fitness_history, avg_fitness)
            
            diversity = compute_diversity(garden.planted_trees)
            push!(garden.diversity_history, diversity)
        end
    end
    
    return nothing
end

"""
    compute_diversity(trees::Vector{PlantedTree})

Compute diversity of tree population.
"""
function compute_diversity(trees::Vector{PlantedTree})
    if length(trees) <= 1
        return 0.0
    end
    
    # Diversity based on tree structure differences
    total_distance = 0.0
    count = 0
    
    for i in 1:length(trees)
        for j in (i+1):length(trees)
            # Simple distance: difference in length + level differences
            tree1 = trees[i].tree
            tree2 = trees[j].tree
            
            len_diff = abs(length(tree1) - length(tree2))
            
            min_len = min(length(tree1), length(tree2))
            level_diff = sum(abs(tree1[k] - tree2[k]) for k in 1:min_len)
            
            distance = len_diff + level_diff
            total_distance += distance
            count += 1
        end
    end
    
    return count > 0 ? total_distance / count : 0.0
end

"""
    cross_pollinate_trees!(garden::MembraneGarden, 
                          membrane1::Int, membrane2::Int, num_crosses::Int)

Cross-pollinate trees between two membranes.
"""
function cross_pollinate_trees!(
    garden::MembraneGarden,
    membrane1::Int,
    membrane2::Int,
    num_crosses::Int
)
    if !haskey(garden.membrane_trees, membrane1) || 
       !haskey(garden.membrane_trees, membrane2)
        return 0
    end
    
    indices1 = garden.membrane_trees[membrane1]
    indices2 = garden.membrane_trees[membrane2]
    
    if isempty(indices1) || isempty(indices2)
        return 0
    end
    
    crosses_made = 0
    
    for _ in 1:num_crosses
        # Select random trees from each membrane
        idx1 = rand(indices1)
        idx2 = rand(indices2)
        
        tree1 = garden.planted_trees[idx1]
        tree2 = garden.planted_trees[idx2]
        
        # Create hybrid
        hybrid_tree = crossover_trees(tree1.tree, tree2.tree)
        
        # Plant in both membranes
        for membrane_id in [membrane1, membrane2]
            hybrid = PlantedTree(hybrid_tree, membrane_id)
            hybrid.parent_trees = [tree1.tree, tree2.tree]
            push!(garden.planted_trees, hybrid)
            push!(garden.membrane_trees[membrane_id], length(garden.planted_trees))
        end
        
        crosses_made += 1
    end
    
    return crosses_made
end

"""
    prune_garden!(garden::MembraneGarden)

Remove low-fitness trees from the garden.
"""
function prune_garden!(garden::MembraneGarden)
    if isempty(garden.planted_trees)
        return 0
    end
    
    threshold = garden.dynamics.pruning_threshold
    
    # Find trees to keep
    keep_indices = Int[]
    for (i, tree) in enumerate(garden.planted_trees)
        if tree.fitness >= threshold
            push!(keep_indices, i)
        end
    end
    
    pruned_count = length(garden.planted_trees) - length(keep_indices)
    
    # Keep only selected trees
    garden.planted_trees = garden.planted_trees[keep_indices]
    
    # Rebuild membrane index
    garden.membrane_trees = Dict{Int, Vector{Int}}()
    for (i, tree) in enumerate(garden.planted_trees)
        membrane_id = tree.membrane_id
        if !haskey(garden.membrane_trees, membrane_id)
            garden.membrane_trees[membrane_id] = Int[]
        end
        push!(garden.membrane_trees[membrane_id], i)
    end
    
    return pruned_count
end

"""
    harvest_garden_feedback!(garden::MembraneGarden)

Harvest feedback signals from the garden.
"""
function harvest_garden_feedback!(garden::MembraneGarden)
    if isempty(garden.planted_trees)
        return Dict{String, Float64}()
    end
    
    # Aggregate feedback
    avg_fitness = mean([t.fitness for t in garden.planted_trees])
    max_fitness = maximum([t.fitness for t in garden.planted_trees])
    avg_age = mean([t.age for t in garden.planted_trees])
    diversity = compute_diversity(garden.planted_trees)
    
    # Reservoir feedback
    avg_reservoir = mean([t.reservoir_performance for t in garden.planted_trees])
    
    # J-surface feedback
    avg_energy = mean([t.energy_contribution for t in garden.planted_trees])
    
    feedback = Dict{String, Float64}(
        "avg_fitness" => avg_fitness,
        "max_fitness" => max_fitness,
        "avg_age" => avg_age,
        "diversity" => diversity,
        "avg_reservoir_performance" => avg_reservoir,
        "avg_energy_contribution" => avg_energy,
        "population_size" => Float64(length(garden.planted_trees))
    )
    
    # Store in feedback signals
    for (key, value) in feedback
        if !haskey(garden.feedback_signals, key)
            garden.feedback_signals[key] = Float64[]
        end
        push!(garden.feedback_signals[key], value)
    end
    
    return feedback
end

"""
    get_garden_statistics(garden::MembraneGarden)

Get comprehensive garden statistics.
"""
function get_garden_statistics(garden::MembraneGarden)
    if isempty(garden.planted_trees)
        return Dict{String, Any}(
            "population_size" => 0,
            "generation" => garden.generation
        )
    end
    
    fitnesses = [t.fitness for t in garden.planted_trees]
    ages = [t.age for t in garden.planted_trees]
    orders = [length(t.tree) for t in garden.planted_trees]
    
    return Dict{String, Any}(
        "population_size" => length(garden.planted_trees),
        "generation" => garden.generation,
        "avg_fitness" => mean(fitnesses),
        "max_fitness" => maximum(fitnesses),
        "min_fitness" => minimum(fitnesses),
        "avg_age" => mean(ages),
        "max_age" => maximum(ages),
        "avg_order" => mean(orders),
        "diversity" => compute_diversity(garden.planted_trees),
        "num_membranes" => length(garden.membrane_trees),
        "fitness_trend" => length(garden.fitness_history) >= 2 ? 
                          garden.fitness_history[end] - garden.fitness_history[end-1] : 0.0
    )
end

end  # module MembraneGardenCore
