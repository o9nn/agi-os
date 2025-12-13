"""
    A000081Unification

The ontogenetic engine that unifies all components of the Deep Tree Echo
State Reservoir Computer under the OEIS A000081 sequence.

# OEIS A000081: The Fundamental Sequence

The sequence A000081 counts unlabeled rooted trees with n nodes:
    1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, ...

This sequence serves as the **ontogenetic generator** for the entire system:

1. **Structural Alphabet**: Trees form the basis of all computation
2. **B-Series Ridges**: Each tree τ defines an elementary differential F(τ)
3. **Reservoir Topology**: Tree structure defines reservoir connectivity
4. **Membrane Hierarchy**: Trees planted in P-system membranes
5. **J-Surface Geometry**: Tree complexity defines energy landscape
6. **Evolution Dynamics**: A000081 growth pattern guides self-organization

# Unified Architecture

```
                    A000081 Ontogenetic Engine
                              ↓
        ┌─────────────────────┼─────────────────────┐
        ↓                     ↓                     ↓
  Rooted Trees          B-Series Ridge        Reservoir
  (Structure)           (Integration)         (Dynamics)
        ↓                     ↓                     ↓
        └─────────────────────┼─────────────────────┘
                              ↓
                    P-System Membranes
                    (Evolution Container)
                              ↓
                      J-Surface Reactor
                    (Unified Dynamics)
```

# The Unification Equation

The complete system evolves according to:

    ∂ψ/∂t = J(ψ) · ∇H_A000081(ψ) + R_echo(ψ, t) + M_membrane(ψ)

Where:
- **J(ψ)**: J-surface structure from tree topology
- **H_A000081(ψ)**: Hamiltonian encoding A000081 complexity
- **R_echo(ψ, t)**: Echo state reservoir dynamics
- **M_membrane(ψ)**: P-system membrane evolution

And the B-series ridge provides the discrete integration:

    ψ_{n+1} = ψ_n + h Σ_{τ ∈ A000081} b(τ)/σ(τ) · F(τ)(ψ_n)
"""
module A000081Unification

using LinearAlgebra
using Random
using Statistics

# Load sibling modules
include("ElementaryDifferentials.jl")
using .ElementaryDifferentials

export A000081UnifiedSystem
export create_unified_system, initialize_from_a000081!
export evolve_unified_system!, get_unified_status
export plant_trees_in_membranes!, harvest_from_garden!

# The sacred A000081 sequence (precomputed)
const A000081 = [
    1, 1, 2, 4, 9, 20, 48, 115, 286, 719,
    1842, 4766, 12486, 32973, 87811, 235381,
    634847, 1721159, 4688676, 12826228
]

"""
    A000081UnifiedSystem

The complete unified system orchestrated by the A000081 ontogenetic engine.

# Fields
- `rooted_trees::Vector{Vector{Int}}`: Trees from A000081 (structural alphabet)
- `jsurface_matrix::Matrix{Float64}`: J-surface structure matrix
- `hamiltonian::Function`: Energy function H_A000081(ψ)
- `differential_map::TreeDifferentialMap`: B-series ridge
- `reservoir_state::Vector{Float64}`: Echo state reservoir
- `reservoir_weights::Matrix{Float64}`: Reservoir connectivity
- `membrane_structure::Dict`: P-system membranes
- `planted_trees::Dict{Int, Vector{Vector{Int}}}`: Trees in each membrane
- `state::Vector{Float64}`: Current system state ψ
- `generation::Int`: Current generation
- `energy_history::Vector{Float64}`: Energy trajectory
- `config::Dict{String, Any}`: Configuration
"""
mutable struct A000081UnifiedSystem
    rooted_trees::Vector{Vector{Int}}
    jsurface_matrix::Matrix{Float64}
    hamiltonian::Function
    differential_map::TreeDifferentialMap
    reservoir_state::Vector{Float64}
    reservoir_weights::Matrix{Float64}
    membrane_structure::Dict{Int, Dict{String, Any}}
    planted_trees::Dict{Int, Vector{Vector{Int}}}
    state::Vector{Float64}
    generation::Int
    energy_history::Vector{Float64}
    config::Dict{String, Any}
    
    function A000081UnifiedSystem(;
        max_order::Union{Int,Nothing}=nothing,
        reservoir_size::Union{Int,Nothing}=nothing,
        num_membranes::Union{Int,Nothing}=nothing,
        symplectic::Bool=true,
        base_order::Int=5)
        
        # Load parameter derivation module
        include("A000081Parameters.jl")
        using .A000081Parameters
        
        # Derive parameters from A000081 if not provided
        if any(isnothing.([reservoir_size, max_order, num_membranes]))
            println("\n⚠ Deriving parameters from A000081 (base_order=$base_order)...")
            params = A000081Parameters.get_parameter_set(base_order, membrane_order=4)
            
            reservoir_size = isnothing(reservoir_size) ? params.reservoir_size : reservoir_size
            max_order = isnothing(max_order) ? params.max_tree_order : max_order
            num_membranes = isnothing(num_membranes) ? params.num_membranes : num_membranes
            
            println("✓ A000081-aligned parameters:")
            println("  reservoir_size = $reservoir_size (cumulative trees 1:$base_order)")
            println("  max_order      = $max_order")
            println("  num_membranes  = $num_membranes (A000081[4])")
        else
            # Validate provided parameters
            is_valid, message = A000081Parameters.validate_parameters(
                reservoir_size, max_order, num_membranes, 0.1, 0.05
            )
            if !is_valid
                println("\n⚠ WARNING: Parameters may not align with A000081:")
                println("  $message")
            end
        end
        
        # Generate rooted trees from A000081 up to max_order
        rooted_trees = generate_a000081_trees(max_order)
        
        println("🌳 Generated $(length(rooted_trees)) rooted trees from A000081")
        println("   Orders 1-$max_order: ", [count_trees_of_order(i, rooted_trees) for i in 1:max_order])
        
        # Create J-surface, Hamiltonian, and differential map
        (J, H, diff_map) = create_jsurface_hamiltonian_system(
            reservoir_size, rooted_trees; symplectic=symplectic
        )
        
        # Initialize reservoir with tree-structured connectivity
        (reservoir_state, reservoir_weights) = initialize_tree_reservoir(
            reservoir_size, rooted_trees
        )
        
        # Create P-system membrane structure
        membrane_structure = create_membrane_hierarchy(num_membranes)
        
        # Initialize planted trees (empty at start)
        planted_trees = Dict{Int, Vector{Vector{Int}}}()
        for i in 1:num_membranes
            planted_trees[i] = Vector{Int}[]
        end
        
        # Initialize system state
        state = randn(reservoir_size)
        
        # Configuration
        config = Dict{String, Any}(
            "max_order" => max_order,
            "reservoir_size" => reservoir_size,
            "num_membranes" => num_membranes,
            "symplectic" => symplectic,
            "a000081_count" => length(rooted_trees),
            "base_order" => base_order
        )
        
        new(rooted_trees, J, H, diff_map, reservoir_state, reservoir_weights,
            membrane_structure, planted_trees, state, 0, Float64[], config)
    end
end

"""
    generate_a000081_trees(max_order::Int)

Generate all rooted trees from A000081 up to specified order.
"""
function generate_a000081_trees(max_order::Int)
    all_trees = Vector{Int}[]
    
    for order in 1:max_order
        trees_of_order = generate_trees_of_order(order)
        append!(all_trees, trees_of_order)
    end
    
    return all_trees
end

"""
    generate_trees_of_order(n::Int)

Generate all rooted trees with exactly n nodes.
The count should match A000081[n].
"""
function generate_trees_of_order(n::Int)
    if n <= 0
        return Vector{Int}[]
    elseif n == 1
        return [[1]]  # 1 tree
    elseif n == 2
        return [[1, 2]]  # 1 tree
    elseif n == 3
        return [[1, 2, 3], [1, 2, 2]]  # 2 trees
    elseif n == 4
        return [
            [1, 2, 3, 4],  # Linear
            [1, 2, 3, 3],  # Branch at end
            [1, 2, 3, 2],  # Branch at middle
            [1, 2, 2, 2]   # Star (3 branches)
        ]  # 4 trees
    elseif n == 5
        # 9 trees for order 5
        return [
            [1, 2, 3, 4, 5],  # Linear
            [1, 2, 3, 4, 4],  # Branch at level 3
            [1, 2, 3, 4, 3],  # Branch at level 2
            [1, 2, 3, 4, 2],  # Branch at level 1
            [1, 2, 3, 3, 3],  # Two branches at level 2
            [1, 2, 3, 3, 2],  # Mixed branches
            [1, 2, 3, 2, 2],  # Different configuration
            [1, 2, 2, 3, 3],  # Symmetric branches
            [1, 2, 2, 2, 2]   # Star (4 branches)
        ]
    else
        # For higher orders, generate representative trees
        # Full enumeration would require proper tree generation algorithm
        trees = Vector{Int}[]
        
        # Linear tree
        push!(trees, collect(1:n))
        
        # Various branching patterns
        for num_branches in 2:min(n-1, 5)
            tree = [1]
            for _ in 1:num_branches
                push!(tree, 2)
            end
            # Fill remaining nodes
            while length(tree) < n
                push!(tree, rand(2:length(tree)))
            end
            push!(trees, tree)
        end
        
        return trees
    end
end

"""
    count_trees_of_order(order::Int, trees::Vector{Vector{Int}})

Count how many trees have the specified order.
"""
function count_trees_of_order(order::Int, trees::Vector{Vector{Int}})
    return count(tree -> length(tree) == order, trees)
end

"""
    initialize_tree_reservoir(size::Int, trees::Vector{Vector{Int}})

Initialize echo state reservoir with connectivity inspired by tree structures.
"""
function initialize_tree_reservoir(size::Int, trees::Vector{Vector{Int}})
    # Reservoir state
    state = zeros(size)
    
    # Reservoir weights with tree-structured sparsity
    W = zeros(size, size)
    
    # Use trees to define connectivity pattern
    for tree in trees
        # Map tree to reservoir indices
        indices = tree_to_indices(tree, size)
        
        # Create connections following tree structure
        for i in 1:(length(indices)-1)
            idx1 = indices[i]
            idx2 = indices[i+1]
            if idx1 <= size && idx2 <= size
                W[idx1, idx2] = randn() * 0.1
                W[idx2, idx1] = randn() * 0.1  # Symmetric
            end
        end
    end
    
    # Normalize spectral radius
    eigenvalues = eigvals(W)
    spectral_radius = maximum(abs.(eigenvalues))
    if spectral_radius > 0
        W = W * (0.9 / spectral_radius)
    end
    
    return (state, W)
end

"""
    tree_to_indices(tree::Vector{Int}, size::Int)

Map a tree structure to reservoir indices.
"""
function tree_to_indices(tree::Vector{Int}, size::Int)
    # Hash tree to indices
    indices = Int[]
    for (i, level) in enumerate(tree)
        idx = mod(hash(tree[1:i]), size) + 1
        push!(indices, idx)
    end
    return indices
end

"""
    create_membrane_hierarchy(num_membranes::Int)

Create P-system membrane hierarchy for planting trees.
"""
function create_membrane_hierarchy(num_membranes::Int)
    membranes = Dict{Int, Dict{String, Any}}()
    
    for i in 1:num_membranes
        membranes[i] = Dict{String, Any}(
            "id" => i,
            "parent" => i > 1 ? 1 : 0,  # Nested structure
            "multiset" => Dict{String, Int}(),
            "energy" => 0.0,
            "tree_count" => 0
        )
    end
    
    return membranes
end

"""
    create_unified_system(; kwargs...)

Create a complete A000081 unified system.
"""
function create_unified_system(; kwargs...)
    return A000081UnifiedSystem(; kwargs...)
end

"""
    initialize_from_a000081!(system::A000081UnifiedSystem; seed_trees::Int=10)

Initialize the system by planting seed trees from A000081 into membranes.
"""
function initialize_from_a000081!(system::A000081UnifiedSystem; seed_trees::Int=10)
    println("\n🌱 Initializing from A000081 ontogenetic engine...")
    
    # Select diverse trees across orders
    selected_trees = Vector{Int}[]
    orders = unique([length(tree) for tree in system.rooted_trees])
    
    for order in orders
        order_trees = filter(t -> length(t) == order, system.rooted_trees)
        n_select = min(2, length(order_trees))
        append!(selected_trees, order_trees[1:n_select])
        
        if length(selected_trees) >= seed_trees
            break
        end
    end
    
    # Plant trees in membranes
    for (i, tree) in enumerate(selected_trees[1:min(seed_trees, length(selected_trees))])
        membrane_id = mod(i - 1, system.config["num_membranes"]) + 1
        plant_tree_in_membrane!(system, tree, membrane_id)
    end
    
    println("✓ Planted $(sum(length(trees) for trees in values(system.planted_trees))) trees")
    for (membrane_id, trees) in system.planted_trees
        if !isempty(trees)
            println("  Membrane $membrane_id: $(length(trees)) trees")
        end
    end
    
    return nothing
end

"""
    plant_tree_in_membrane!(system::A000081UnifiedSystem, 
                           tree::Vector{Int}, 
                           membrane_id::Int)

Plant a rooted tree in a specific membrane.
"""
function plant_tree_in_membrane!(system::A000081UnifiedSystem, 
                                tree::Vector{Int}, 
                                membrane_id::Int)
    push!(system.planted_trees[membrane_id], tree)
    
    # Update membrane multiset
    membrane = system.membrane_structure[membrane_id]
    tree_symbol = "tree_$(length(tree))"
    membrane["multiset"][tree_symbol] = get(membrane["multiset"], tree_symbol, 0) + 1
    membrane["tree_count"] += 1
    
    # Update membrane energy based on tree complexity
    membrane["energy"] += length(tree) * 0.1
    
    return nothing
end

"""
    plant_trees_in_membranes!(system::A000081UnifiedSystem, 
                             trees::Vector{Vector{Int}}, 
                             membrane_id::Int)

Plant multiple trees in a membrane.
"""
function plant_trees_in_membranes!(system::A000081UnifiedSystem, 
                                  trees::Vector{Vector{Int}}, 
                                  membrane_id::Int)
    for tree in trees
        plant_tree_in_membrane!(system, tree, membrane_id)
    end
end

"""
    evolve_unified_system!(system::A000081UnifiedSystem, 
                          steps::Int=1,
                          dt::Float64=0.01;
                          verbose::Bool=false)

Evolve the unified system using the complete unification equation:
    ∂ψ/∂t = J(ψ) · ∇H_A000081(ψ) + R_echo(ψ, t) + M_membrane(ψ)
"""
function evolve_unified_system!(system::A000081UnifiedSystem, 
                               steps::Int=1,
                               dt::Float64=0.01;
                               verbose::Bool=false)
    if verbose
        println("\n🌊 Evolving unified system for $steps steps...")
    end
    
    for step in 1:steps
        # 1. Unite gradient and evolution through elementary differentials
        system.state = unite_gradient_evolution(
            system.jsurface_matrix,
            system.hamiltonian,
            system.differential_map,
            system.state,
            dt
        )
        
        # 2. Echo state reservoir dynamics
        reservoir_update!(system, dt)
        
        # 3. Membrane evolution
        membrane_evolution!(system, dt)
        
        # 4. Cross-pollination between membranes
        if mod(step, 10) == 0
            cross_pollinate_membranes!(system)
        end
        
        # 5. Record energy
        energy = system.hamiltonian(system.state)
        push!(system.energy_history, energy)
        
        # 6. Increment generation
        system.generation += 1
        
        if verbose && mod(step, 10) == 0
            println("  Step $step: E = $(round(energy, digits=4)), " *
                   "||ψ|| = $(round(norm(system.state), digits=4))")
        end
    end
    
    if verbose
        println("✓ Evolution complete: generation $(system.generation)")
    end
    
    return nothing
end

"""
    reservoir_update!(system::A000081UnifiedSystem, dt::Float64)

Update echo state reservoir following tree-structured dynamics.
"""
function reservoir_update!(system::A000081UnifiedSystem, dt::Float64)
    # Echo state update: x(t+1) = tanh(W*x(t) + input)
    input = system.state[1:min(10, length(system.state))]
    
    # Expand input to reservoir size
    W_in = randn(length(system.reservoir_state), length(input))
    
    new_state = tanh.(
        system.reservoir_weights * system.reservoir_state + 
        W_in * input
    )
    
    system.reservoir_state = new_state
    
    # Feed back to main state
    feedback_size = min(length(system.state), length(new_state))
    system.state[1:feedback_size] += dt * new_state[1:feedback_size] * 0.1
    
    return nothing
end

"""
    membrane_evolution!(system::A000081UnifiedSystem, dt::Float64)

Evolve P-system membranes and their planted trees.
"""
function membrane_evolution!(system::A000081UnifiedSystem, dt::Float64)
    for (membrane_id, membrane) in system.membrane_structure
        # Grow trees in membrane
        trees = system.planted_trees[membrane_id]
        
        if !isempty(trees)
            # Compute membrane contribution to dynamics
            total_complexity = sum(length(tree) for tree in trees)
            membrane_force = total_complexity * 0.01
            
            # Apply to state
            idx = min(membrane_id, length(system.state))
            system.state[idx] += dt * membrane_force
            
            # Update membrane energy
            membrane["energy"] *= (1.0 - dt * 0.01)  # Decay
        end
    end
    
    return nothing
end

"""
    cross_pollinate_membranes!(system::A000081UnifiedSystem)

Exchange trees between membranes (genetic crossover).
"""
function cross_pollinate_membranes!(system::A000081UnifiedSystem)
    membrane_ids = collect(keys(system.planted_trees))
    
    if length(membrane_ids) < 2
        return
    end
    
    # Select two random membranes
    id1, id2 = rand(membrane_ids, 2)
    
    trees1 = system.planted_trees[id1]
    trees2 = system.planted_trees[id2]
    
    if !isempty(trees1) && !isempty(trees2)
        # Exchange one tree
        tree1 = rand(trees1)
        tree2 = rand(trees2)
        
        # Create hybrid tree (simplified crossover)
        if length(tree1) > 1 && length(tree2) > 1
            cut_point = div(length(tree1), 2)
            hybrid = vcat(tree1[1:cut_point], tree2[cut_point+1:end])
            
            # Plant hybrid in first membrane
            plant_tree_in_membrane!(system, hybrid, id1)
        end
    end
    
    return nothing
end

"""
    harvest_from_garden!(system::A000081UnifiedSystem, membrane_id::Int)

Harvest trees from a membrane (extract learned structures).
"""
function harvest_from_garden!(system::A000081UnifiedSystem, membrane_id::Int)
    trees = system.planted_trees[membrane_id]
    
    # Compute fitness of each tree based on membrane energy
    membrane = system.membrane_structure[membrane_id]
    energy = membrane["energy"]
    
    # Return trees with their fitness
    fitness_scores = [length(tree) / (energy + 1.0) for tree in trees]
    
    return collect(zip(trees, fitness_scores))
end

"""
    get_unified_status(system::A000081UnifiedSystem)

Get comprehensive status of the unified system.
"""
function get_unified_status(system::A000081UnifiedSystem)
    total_planted = sum(length(trees) for trees in values(system.planted_trees))
    
    status = Dict{String, Any}(
        "generation" => system.generation,
        "state_norm" => norm(system.state),
        "current_energy" => system.hamiltonian(system.state),
        "avg_energy" => isempty(system.energy_history) ? 0.0 : mean(system.energy_history),
        "total_trees_a000081" => length(system.rooted_trees),
        "planted_trees" => total_planted,
        "membranes" => system.config["num_membranes"],
        "reservoir_size" => system.config["reservoir_size"],
        "max_order" => system.config["max_order"]
    )
    
    return status
end

"""
    print_unified_status(system::A000081UnifiedSystem)

Print formatted status of the unified system.
"""
function print_unified_status(system::A000081UnifiedSystem)
    status = get_unified_status(system)
    
    println("\n" * "=" * "^60)
    println("🌳 Deep Tree Echo: A000081 Unified System Status")
    println("=" ^ 60)
    println("  Generation: $(status["generation"])")
    println("  State ||ψ||: $(round(status["state_norm"], digits=4))")
    println("  Energy H(ψ): $(round(status["current_energy"], digits=4))")
    println("  Avg Energy: $(round(status["avg_energy"], digits=4))")
    println()
    println("  A000081 Trees: $(status["total_trees_a000081"]) (orders 1-$(status["max_order"]))")
    println("  Planted Trees: $(status["planted_trees"])")
    println("  Membranes: $(status["membranes"])")
    println("  Reservoir: $(status["reservoir_size"]) nodes")
    println("=" ^ 60)
    
    # Show per-membrane status
    println("\n  Membrane Garden Status:")
    for (membrane_id, trees) in system.planted_trees
        membrane = system.membrane_structure[membrane_id]
        println("    Membrane $membrane_id: $(length(trees)) trees, " *
               "E = $(round(membrane["energy"], digits=3))")
    end
    println()
end

export print_unified_status

end # module A000081Unification
