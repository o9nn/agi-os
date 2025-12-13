"""
    MembraneGarden

Membrane computing gardens where rooted trees are planted, grown, and evolved
within membrane contexts. Trees provide feedback from their leaf nodes back
to roots, creating recursive learning dynamics.
"""
module MembraneGarden

using LinearAlgebra
using Random

export Garden, TreePlanting, GrowthDynamics
export create_garden, plant_tree!, grow_trees!, harvest_feedback!
export cross_pollinate!, prune_garden!, get_tree_population

"""
    TreePlanting

A rooted tree planted in a membrane garden.

Fields:
- `tree_id::Int`: Unique tree identifier
- `level_sequence::Vector{Int}`: Tree structure as level sequence
- `membrane_id::Int`: Membrane where tree is planted
- `age::Int`: Age of the tree (growth cycles)
- `fitness::Float64`: Fitness/health of the tree
- `feedback_state::Vector{Float64}`: Feedback from leaves to root
"""
mutable struct TreePlanting
    tree_id::Int
    level_sequence::Vector{Int}
    membrane_id::Int
    age::Int
    fitness::Float64
    feedback_state::Vector{Float64}
    
    function TreePlanting(id::Int, levels::Vector{Int}, membrane_id::Int)
        n = length(levels)
        new(id, levels, membrane_id, 0, 1.0, zeros(n))
    end
end

"""
    GrowthDynamics

Dynamics governing tree growth in the garden.

Fields:
- `growth_rate::Float64`: Rate of tree growth
- `mutation_rate::Float64`: Probability of mutation during growth
- `feedback_strength::Float64`: Strength of leaf-to-root feedback
- `pruning_threshold::Float64`: Fitness threshold for pruning
"""
struct GrowthDynamics
    growth_rate::Float64
    mutation_rate::Float64
    feedback_strength::Float64
    pruning_threshold::Float64
    
    function GrowthDynamics(;
        growth_rate::Float64=0.1,
        mutation_rate::Float64=0.05,
        feedback_strength::Float64=0.8,
        pruning_threshold::Float64=0.3)
        new(growth_rate, mutation_rate, feedback_strength, pruning_threshold)
    end
end

"""
    Garden

A membrane computing garden containing populations of rooted trees.

Fields:
- `trees::Dict{Int, TreePlanting}`: All trees indexed by id
- `membrane_trees::Dict{Int, Vector{Int}}`: Trees in each membrane
- `dynamics::GrowthDynamics`: Growth dynamics
- `generation::Int`: Current generation number
- `next_tree_id::Int`: Counter for tree ids
"""
mutable struct Garden
    trees::Dict{Int, TreePlanting}
    membrane_trees::Dict{Int, Vector{Int}}
    dynamics::GrowthDynamics
    generation::Int
    next_tree_id::Int
    
    function Garden(dynamics::GrowthDynamics=GrowthDynamics())
        new(
            Dict{Int, TreePlanting}(),
            Dict{Int, Vector{Int}}(),
            dynamics,
            0,
            1
        )
    end
end

"""
    create_garden(;dynamics::GrowthDynamics=GrowthDynamics())

Create a new membrane garden.

# Arguments
- `dynamics::GrowthDynamics`: Growth dynamics parameters

# Returns
- `Garden`: Initialized garden
"""
function create_garden(;dynamics::GrowthDynamics=GrowthDynamics())
    return Garden(dynamics)
end

"""
    plant_tree!(garden::Garden, 
               level_sequence::Vector{Int}, 
               membrane_id::Int)

Plant a rooted tree in a specific membrane.

# Arguments
- `garden::Garden`: The garden
- `level_sequence::Vector{Int}`: Tree structure
- `membrane_id::Int`: Target membrane

# Returns
- `Int`: Tree id of planted tree
"""
function plant_tree!(garden::Garden, 
                    level_sequence::Vector{Int}, 
                    membrane_id::Int)
    tree_id = garden.next_tree_id
    garden.next_tree_id += 1
    
    # Create tree planting
    tree = TreePlanting(tree_id, level_sequence, membrane_id)
    garden.trees[tree_id] = tree
    
    # Add to membrane
    if !haskey(garden.membrane_trees, membrane_id)
        garden.membrane_trees[membrane_id] = Int[]
    end
    push!(garden.membrane_trees[membrane_id], tree_id)
    
    return tree_id
end

"""
    grow_trees!(garden::Garden, cycles::Int=1)

Grow all trees in the garden for specified cycles.

Growth involves:
1. Age increment
2. Feedback propagation from leaves to root
3. Fitness update based on feedback
4. Possible mutation

# Arguments
- `garden::Garden`: The garden
- `cycles::Int=1`: Number of growth cycles
"""
function grow_trees!(garden::Garden, cycles::Int=1)
    for _ in 1:cycles
        for tree in values(garden.trees)
            # Increment age
            tree.age += 1
            
            # Propagate feedback from leaves to root
            propagate_feedback!(tree, garden.dynamics)
            
            # Update fitness based on feedback quality
            update_fitness!(tree, garden.dynamics)
            
            # Possible mutation
            if rand() < garden.dynamics.mutation_rate
                mutate_tree!(tree)
            end
        end
        
        garden.generation += 1
    end
    
    return nothing
end

"""
    propagate_feedback!(tree::TreePlanting, dynamics::GrowthDynamics)

Propagate feedback from leaf nodes to root through the tree structure.

The feedback flows backward through the tree, accumulating information
from leaves to root.
"""
function propagate_feedback!(tree::TreePlanting, dynamics::GrowthDynamics)
    n = length(tree.level_sequence)
    feedback = zeros(n)
    
    # Initialize leaf feedback (highest level nodes)
    max_level = maximum(tree.level_sequence)
    for i in 1:n
        if tree.level_sequence[i] == max_level
            # Leaf nodes generate initial feedback
            feedback[i] = randn() * dynamics.feedback_strength
        end
    end
    
    # Propagate backward through levels
    for level in (max_level-1):-1:1
        for i in 1:n
            if tree.level_sequence[i] == level
                # Accumulate feedback from children at next level
                for j in (i+1):n
                    if tree.level_sequence[j] == level + 1
                        feedback[i] += feedback[j] * dynamics.feedback_strength
                    end
                end
            end
        end
    end
    
    tree.feedback_state = feedback
    return nothing
end

"""
    update_fitness!(tree::TreePlanting, dynamics::GrowthDynamics)

Update tree fitness based on feedback quality.

Fitness measures how well the tree processes information from leaves to root.
"""
function update_fitness!(tree::TreePlanting, dynamics::GrowthDynamics)
    # Fitness based on root feedback (first node)
    root_feedback = abs(tree.feedback_state[1])
    
    # Normalize by tree size
    normalized_feedback = root_feedback / length(tree.level_sequence)
    
    # Update fitness with exponential moving average
    alpha = dynamics.growth_rate
    tree.fitness = (1 - alpha) * tree.fitness + alpha * normalized_feedback
    
    # Clamp fitness to [0, 1]
    tree.fitness = clamp(tree.fitness, 0.0, 1.0)
    
    return nothing
end

"""
    mutate_tree!(tree::TreePlanting)

Mutate a tree's structure slightly.

Mutations can:
1. Add a new node
2. Remove a leaf node
3. Change a node's level (within constraints)
"""
function mutate_tree!(tree::TreePlanting)
    mutation_type = rand(1:3)
    
    if mutation_type == 1 && length(tree.level_sequence) < 20
        # Add a node
        add_node!(tree)
    elseif mutation_type == 2 && length(tree.level_sequence) > 1
        # Remove a leaf
        remove_leaf!(tree)
    else
        # Modify a level
        modify_level!(tree)
    end
    
    return nothing
end

"""
    add_node!(tree::TreePlanting)

Add a new node to the tree.
"""
function add_node!(tree::TreePlanting)
    # Add at a random level
    max_level = maximum(tree.level_sequence)
    new_level = rand(2:(max_level+1))
    push!(tree.level_sequence, new_level)
    push!(tree.feedback_state, 0.0)
    return nothing
end

"""
    remove_leaf!(tree::TreePlanting)

Remove a leaf node from the tree.
"""
function remove_leaf!(tree::TreePlanting)
    if length(tree.level_sequence) <= 1
        return nothing
    end
    
    # Find leaf nodes (maximum level)
    max_level = maximum(tree.level_sequence)
    leaf_indices = findall(x -> x == max_level, tree.level_sequence)
    
    if !isempty(leaf_indices)
        # Remove a random leaf
        idx = rand(leaf_indices)
        deleteat!(tree.level_sequence, idx)
        deleteat!(tree.feedback_state, idx)
    end
    
    return nothing
end

"""
    modify_level!(tree::TreePlanting)

Modify the level of a random node.
"""
function modify_level!(tree::TreePlanting)
    if length(tree.level_sequence) <= 1
        return nothing
    end
    
    # Don't modify root
    idx = rand(2:length(tree.level_sequence))
    current_level = tree.level_sequence[idx]
    
    # Change by ±1
    new_level = current_level + rand([-1, 1])
    new_level = max(2, new_level)  # Keep at least level 2
    
    tree.level_sequence[idx] = new_level
    
    return nothing
end

"""
    cross_pollinate!(garden::Garden, 
                    membrane_id1::Int, 
                    membrane_id2::Int,
                    count::Int=1)

Cross-pollinate trees between two membranes.

Creates hybrid offspring by combining trees from different membranes.

# Arguments
- `garden::Garden`: The garden
- `membrane_id1::Int`: First membrane
- `membrane_id2::Int`: Second membrane
- `count::Int=1`: Number of offspring to create
"""
function cross_pollinate!(garden::Garden, 
                         membrane_id1::Int, 
                         membrane_id2::Int,
                         count::Int=1)
    # Get trees from each membrane
    trees1 = get(garden.membrane_trees, membrane_id1, Int[])
    trees2 = get(garden.membrane_trees, membrane_id2, Int[])
    
    if isempty(trees1) || isempty(trees2)
        return Int[]
    end
    
    offspring_ids = Int[]
    
    for _ in 1:count
        # Select random parents
        parent1_id = rand(trees1)
        parent2_id = rand(trees2)
        
        parent1 = garden.trees[parent1_id]
        parent2 = garden.trees[parent2_id]
        
        # Create offspring through crossover
        offspring_levels = crossover_trees(parent1.level_sequence, 
                                          parent2.level_sequence)
        
        # Plant in random parent membrane
        target_membrane = rand([membrane_id1, membrane_id2])
        offspring_id = plant_tree!(garden, offspring_levels, target_membrane)
        
        push!(offspring_ids, offspring_id)
    end
    
    return offspring_ids
end

"""
    crossover_trees(levels1::Vector{Int}, levels2::Vector{Int})

Perform crossover between two tree level sequences.
"""
function crossover_trees(levels1::Vector{Int}, levels2::Vector{Int})
    # Single-point crossover
    n1 = length(levels1)
    n2 = length(levels2)
    
    if n1 <= 1 || n2 <= 1
        return rand() < 0.5 ? copy(levels1) : copy(levels2)
    end
    
    point1 = rand(1:(n1-1))
    point2 = rand(1:(n2-1))
    
    offspring = vcat(levels1[1:point1], levels2[(point2+1):end])
    
    # Ensure valid tree structure (starts with 1)
    if isempty(offspring) || offspring[1] != 1
        offspring = [1; offspring]
    end
    
    return offspring
end

"""
    prune_garden!(garden::Garden)

Prune low-fitness trees from the garden.

Removes trees below the pruning threshold.

# Arguments
- `garden::Garden`: The garden

# Returns
- `Int`: Number of trees pruned
"""
function prune_garden!(garden::Garden)
    threshold = garden.dynamics.pruning_threshold
    pruned_count = 0
    
    # Find trees to prune
    to_prune = Int[]
    for (tree_id, tree) in garden.trees
        if tree.fitness < threshold
            push!(to_prune, tree_id)
        end
    end
    
    # Remove pruned trees
    for tree_id in to_prune
        tree = garden.trees[tree_id]
        
        # Remove from membrane
        membrane_id = tree.membrane_id
        if haskey(garden.membrane_trees, membrane_id)
            filter!(id -> id != tree_id, garden.membrane_trees[membrane_id])
        end
        
        # Remove from garden
        delete!(garden.trees, tree_id)
        pruned_count += 1
    end
    
    return pruned_count
end

"""
    harvest_feedback!(garden::Garden, membrane_id::Int)

Harvest feedback from all trees in a membrane.

Collects and aggregates feedback states for use in reservoir adaptation.

# Arguments
- `garden::Garden`: The garden
- `membrane_id::Int`: Target membrane

# Returns
- `Vector{Float64}`: Aggregated feedback vector
"""
function harvest_feedback!(garden::Garden, membrane_id::Int)
    tree_ids = get(garden.membrane_trees, membrane_id, Int[])
    
    if isempty(tree_ids)
        return Float64[]
    end
    
    # Collect all feedback states
    all_feedback = Vector{Float64}[]
    for tree_id in tree_ids
        tree = garden.trees[tree_id]
        push!(all_feedback, tree.feedback_state)
    end
    
    # Aggregate by averaging
    max_length = maximum(length(f) for f in all_feedback)
    aggregated = zeros(max_length)
    
    for feedback in all_feedback
        for (i, val) in enumerate(feedback)
            aggregated[i] += val
        end
    end
    
    aggregated ./= length(all_feedback)
    
    return aggregated
end

"""
    get_tree_population(garden::Garden, membrane_id::Union{Int, Nothing}=nothing)

Get tree population statistics.

# Arguments
- `garden::Garden`: The garden
- `membrane_id::Union{Int, Nothing}`: Specific membrane or all if nothing

# Returns
- `Dict`: Population statistics
"""
function get_tree_population(garden::Garden, membrane_id::Union{Int, Nothing}=nothing)
    trees = if isnothing(membrane_id)
        values(garden.trees)
    else
        tree_ids = get(garden.membrane_trees, membrane_id, Int[])
        [garden.trees[id] for id in tree_ids]
    end
    
    if isempty(trees)
        return Dict(
            "count" => 0,
            "avg_fitness" => 0.0,
            "avg_age" => 0.0,
            "avg_size" => 0.0
        )
    end
    
    return Dict(
        "count" => length(trees),
        "avg_fitness" => mean(t.fitness for t in trees),
        "avg_age" => mean(t.age for t in trees),
        "avg_size" => mean(length(t.level_sequence) for t in trees),
        "max_fitness" => maximum(t.fitness for t in trees),
        "min_fitness" => minimum(t.fitness for t in trees)
    )
end

"""
    print_garden_status(garden::Garden)

Print the current status of the garden.
"""
function print_garden_status(garden::Garden)
    println("=== Membrane Garden Status ===")
    println("Generation: $(garden.generation)")
    println("Total trees: $(length(garden.trees))")
    println()
    
    for (membrane_id, tree_ids) in sort(collect(garden.membrane_trees))
        println("Membrane $membrane_id:")
        stats = get_tree_population(garden, membrane_id)
        println("  Trees: $(stats["count"])")
        println("  Avg fitness: $(round(stats["avg_fitness"], digits=3))")
        println("  Avg age: $(round(stats["avg_age"], digits=1))")
        println("  Avg size: $(round(stats["avg_size"], digits=1))")
        println()
    end
end

end # module MembraneGarden
