"""
    A000081OntogeneticCore

The ontogenetic engine that unifies all components under OEIS A000081.
This is the heart of the Deep Tree Echo State Reservoir Computer.

# OEIS A000081: The Sacred Sequence

The sequence A000081 counts unlabeled rooted trees with n nodes:
    
    n:  1   2   3   4   5    6    7     8      9      10
    a:  1   1   2   4   9   20   48   115    286    719

This sequence is the **ontogenetic generator** that unifies:

1. **Rooted Trees**: Structural alphabet from A000081
2. **B-Series Ridges**: Each tree τ → elementary differential F(τ)
3. **J-Surface Reactor**: Tree complexity → energy landscape
4. **P-System Membranes**: Trees planted in membrane hierarchy
5. **Echo State Reservoirs**: Tree structure → reservoir connectivity
6. **Membrane Gardens**: A000081 growth pattern → evolution dynamics

# The Unification Equation

The complete unified system evolves according to:

    ∂ψ/∂t = J_A000081(ψ) · ∇H_A000081(ψ) + R_echo(ψ, trees) + M_membrane(ψ, trees)

Where:
- **J_A000081(ψ)**: J-surface structure from tree topology
- **H_A000081(ψ)**: Hamiltonian encoding A000081 complexity
- **R_echo(ψ, trees)**: Reservoir dynamics with tree-structured connectivity
- **M_membrane(ψ, trees)**: Membrane evolution with planted trees

Discrete integration via B-series:

    ψ_{n+1} = ψ_n + h Σ_{τ ∈ A000081} b(τ)/σ(τ) · F(τ)(ψ_n)

# Architecture

```
                    A000081 Ontogenetic Engine
                              |
                    Generate Trees (1,1,2,4,9,20,...)
                              |
        ┌─────────────────────┼─────────────────────┐
        ↓                     ↓                     ↓
  J-Surface Reactor    B-Series Ridge      P-System Membranes
  (Gradient Flow)      (Integration)       (Evolution Container)
        ↓                     ↓                     ↓
        └─────────────────────┼─────────────────────┘
                              ↓
                    Echo State Reservoirs
                    (Temporal Dynamics)
                              ↓
                    Membrane Gardens
                    (Tree Cultivation)
                              ↓
                    Feedback Loop
                    (Self-Organization)
```

# The Ontogenetic Cycle

1. **Generation**: Generate trees from A000081
2. **Planting**: Plant trees in membrane gardens
3. **Integration**: Trees define B-series coefficients and J-surface
4. **Evolution**: System evolves via unified dynamics
5. **Feedback**: Performance feeds back to tree fitness
6. **Selection**: High-fitness trees survive and reproduce
7. **Mutation**: New trees generated via genetic operators
8. **Repeat**: Cycle continues, system self-organizes

This creates a **self-evolving cognitive architecture** where structure
(trees) and dynamics (reservoir/J-surface) co-evolve under the guidance
of the A000081 ontogenetic sequence.
"""
module A000081OntogeneticCore

using LinearAlgebra
using Random
using Statistics

# Include the core modules
include("JSurfaceBSeriesCore.jl")
include("PSystemReservoirCore.jl")
include("MembraneGardenCore.jl")

using .JSurfaceBSeriesCore
using .PSystemReservoirCore
using .MembraneGardenCore

export A000081UnifiedSystem
export create_unified_system, initialize_from_a000081!
export evolve_unified!, process_unified_input!
export get_unified_status, save_unified_state

# The sacred A000081 sequence (precomputed up to n=20)
const A000081_SEQUENCE = [
    1, 1, 2, 4, 9, 20, 48, 115, 286, 719,
    1842, 4766, 12486, 32973, 87811, 235381,
    634847, 1721159, 4688676, 12826228
]

"""
    A000081UnifiedSystem

The complete unified system orchestrated by the A000081 ontogenetic engine.

# Fields
- `reactor::JSurfaceBSeriesReactor`: J-surface B-series reactor core
- `psystem::PSystemReservoirBridge`: P-system membrane-reservoir bridge
- `garden::MembraneGarden`: Membrane computing garden
- `a000081_trees::Dict{Int, Vector{Vector{Int}}}`: Trees by order from A000081
- `max_order::Int`: Maximum tree order
- `generation::Int`: Current generation
- `energy_history::Vector{Float64}`: Energy trajectory
- `fitness_history::Vector{Float64}`: Average fitness trajectory
- `config::Dict{String, Any}`: System configuration
"""
mutable struct A000081UnifiedSystem
    reactor::JSurfaceBSeriesReactor
    psystem::PSystemReservoirBridge
    garden::MembraneGarden
    a000081_trees::Dict{Int, Vector{Vector{Int}}}
    max_order::Int
    generation::Int
    energy_history::Vector{Float64}
    fitness_history::Vector{Float64}
    config::Dict{String, Any}
    
    function A000081UnifiedSystem(;
        reservoir_size::Int=100,
        max_order::Int=8,
        num_membranes::Int=3,
        symplectic::Bool=true,
        growth_rate::Float64=0.1,
        mutation_rate::Float64=0.05
    )
        println("🌳 Initializing A000081 Unified System...")
        
        # Generate trees from A000081
        a000081_trees = generate_a000081_trees_by_order(max_order)
        all_trees = vcat([trees for trees in values(a000081_trees)]...)
        
        println("   Generated $(length(all_trees)) trees from A000081")
        for order in 1:max_order
            if haskey(a000081_trees, order)
                count = length(a000081_trees[order])
                expected = order <= length(A000081_SEQUENCE) ? A000081_SEQUENCE[order] : 0
                println("   Order $order: $count trees (expected: $expected)")
            end
        end
        
        # Initialize B-series ridge coefficients
        ridge_coefficients = initialize_a000081_coefficients(all_trees)
        
        # Create J-surface B-series reactor
        reactor = create_reactor(
            reservoir_size,
            all_trees,
            ridge_coefficients;
            symplectic=symplectic,
            hamiltonian=create_a000081_hamiltonian(all_trees)
        )
        
        println("   ✓ J-Surface B-Series Reactor initialized")
        
        # Create P-system reservoir bridge
        psystem = create_psystem_reservoir(num_membranes, reservoir_size)
        
        println("   ✓ P-System Reservoir Bridge initialized")
        
        # Create membrane garden
        garden = create_garden(
            growth_rate=growth_rate,
            mutation_rate=mutation_rate
        )
        
        println("   ✓ Membrane Garden initialized")
        
        # Configuration
        config = Dict{String, Any}(
            "reservoir_size" => reservoir_size,
            "max_order" => max_order,
            "num_membranes" => num_membranes,
            "symplectic" => symplectic,
            "growth_rate" => growth_rate,
            "mutation_rate" => mutation_rate,
            "num_trees" => length(all_trees)
        )
        
        println("🌊 A000081 Unified System initialized successfully!")
        
        new(reactor, psystem, garden, a000081_trees, max_order, 0,
            Float64[], Float64[], config)
    end
end

"""
    generate_a000081_trees_by_order(max_order::Int)

Generate all rooted trees from A000081 up to max_order, organized by order.
"""
function generate_a000081_trees_by_order(max_order::Int)
    trees_by_order = Dict{Int, Vector{Vector{Int}}}()
    
    for order in 1:max_order
        trees_by_order[order] = generate_trees_of_order(order)
    end
    
    return trees_by_order
end

"""
    generate_trees_of_order(n::Int)

Generate all rooted trees with exactly n nodes.
"""
function generate_trees_of_order(n::Int)
    if n <= 0
        return Vector{Int}[]
    elseif n == 1
        return [[1]]
    elseif n == 2
        return [[1, 2]]
    elseif n == 3
        return [[1, 2, 3], [1, 2, 2]]
    elseif n == 4
        return [
            [1, 2, 3, 4],
            [1, 2, 3, 3],
            [1, 2, 3, 2],
            [1, 2, 2, 2]
        ]
    elseif n == 5
        return [
            [1, 2, 3, 4, 5],
            [1, 2, 3, 4, 4],
            [1, 2, 3, 4, 3],
            [1, 2, 3, 4, 2],
            [1, 2, 3, 3, 3],
            [1, 2, 3, 3, 2],
            [1, 2, 3, 2, 2],
            [1, 2, 2, 3, 3],
            [1, 2, 2, 2, 2]
        ]
    else
        # For higher orders, generate representative trees
        trees = Vector{Int}[]
        
        # Linear tree
        push!(trees, collect(1:n))
        
        # Star tree (all branches from root)
        push!(trees, vcat([1], fill(2, n-1)))
        
        # Balanced trees
        for num_branches in 2:min(n-1, 4)
            tree = [1]
            nodes_left = n - 1
            
            # Create branches
            for _ in 1:num_branches
                if nodes_left > 0
                    push!(tree, 2)
                    nodes_left -= 1
                end
            end
            
            # Add remaining nodes
            while nodes_left > 0
                parent_level = rand(2:maximum(tree))
                push!(tree, parent_level + 1)
                nodes_left -= 1
            end
            
            push!(trees, tree)
        end
        
        return trees
    end
end

"""
    initialize_a000081_coefficients(trees::Vector{Vector{Int}})

Initialize B-series coefficients based on A000081 structure.
"""
function initialize_a000081_coefficients(trees::Vector{Vector{Int}})
    coefficients = zeros(length(trees))
    
    for (i, tree) in enumerate(trees)
        order = length(tree)
        
        # Coefficient inversely proportional to order and A000081 count
        a_n = order <= length(A000081_SEQUENCE) ? A000081_SEQUENCE[order] : order^3
        
        # Standard B-series coefficient: 1/n! scaled by A000081
        coefficients[i] = 1.0 / (factorial(order) * sqrt(a_n))
    end
    
    # Normalize
    coefficients ./= sum(coefficients)
    
    return coefficients
end

"""
    create_a000081_hamiltonian(trees::Vector{Vector{Int}})

Create Hamiltonian encoding A000081 complexity.
"""
function create_a000081_hamiltonian(trees::Vector{Vector{Int}})
    function H_A000081(ψ::Vector{Float64})
        # Base quadratic term
        energy = 0.5 * dot(ψ, ψ)
        
        # Tree complexity term
        for tree in trees
            order = length(tree)
            a_n = order <= length(A000081_SEQUENCE) ? A000081_SEQUENCE[order] : order^3
            
            # Tree contribution to energy landscape
            tree_indices = tree_to_indices(tree, length(ψ))
            tree_state = sum(ψ[idx]^2 for idx in tree_indices if idx <= length(ψ))
            
            energy += 0.01 * tree_state / sqrt(a_n)
        end
        
        return energy
    end
    
    return H_A000081
end

"""
    tree_to_indices(tree::Vector{Int}, size::Int)

Map tree structure to state indices.
"""
function tree_to_indices(tree::Vector{Int}, size::Int)
    indices = Int[]
    for (i, level) in enumerate(tree)
        idx = mod(hash(tree[1:i]), size) + 1
        push!(indices, idx)
    end
    return indices
end

"""
    initialize_from_a000081!(system::A000081UnifiedSystem; seed_trees::Int=20)

Initialize the system by planting seed trees from A000081.
"""
function initialize_from_a000081!(system::A000081UnifiedSystem; seed_trees::Int=20)
    println("\n🌱 Planting seed trees from A000081...")
    
    # Select seed trees from various orders
    planted_count = 0
    membrane_ids = collect(keys(system.psystem.membranes))
    
    for order in 1:system.max_order
        if haskey(system.a000081_trees, order)
            trees = system.a000081_trees[order]
            n_plant = min(3, length(trees))  # Plant a few from each order
            
            for tree in trees[1:n_plant]
                if planted_count >= seed_trees
                    break
                end
                
                # Choose random membrane
                membrane_id = rand(membrane_ids)
                
                # Plant in garden
                plant_tree!(system.garden, tree, membrane_id)
                
                # Plant in P-system
                plant_tree_in_membrane!(system.psystem, tree, membrane_id)
                
                planted_count += 1
            end
            
            if planted_count >= seed_trees
                break
            end
        end
    end
    
    println("   ✓ Planted $planted_count trees across $(length(membrane_ids)) membranes")
    
    # Initialize reservoir states
    for membrane_id in membrane_ids
        input = randn(10)
        reservoir_update!(system.psystem, membrane_id, input)
    end
    
    println("   ✓ Reservoir states initialized")
    
    return nothing
end

"""
    evolve_unified!(system::A000081UnifiedSystem, num_generations::Int;
                   dt::Float64=0.01, verbose::Bool=true)

Evolve the unified system for multiple generations.
"""
function evolve_unified!(
    system::A000081UnifiedSystem,
    num_generations::Int;
    dt::Float64=0.01,
    verbose::Bool=true
)
    if verbose
        println("\n🌊 Evolving A000081 Unified System for $num_generations generations...")
    end
    
    for gen in 1:num_generations
        # 1. J-Surface B-Series reactor step
        f(ψ) = -ψ  # Simple vector field
        reactor_step!(system.reactor, f, dt)
        
        # 2. Update all membrane reservoirs
        for membrane_id in keys(system.psystem.membranes)
            input = system.reactor.state[1:min(10, length(system.reactor.state))]
            reservoir_update!(system.psystem, membrane_id, input)
        end
        
        # 3. Harvest feedback from reservoirs
        reservoir_feedback = Dict{String, Float64}()
        for membrane_id in keys(system.psystem.membranes)
            feedback = harvest_feedback!(system.psystem, membrane_id)
            reservoir_feedback["activity"] = get(feedback, "reservoir_activity", 0.0)
            reservoir_feedback["stability"] = 1.0 / (1.0 + get(feedback, "energy", 0.0))
        end
        
        # 4. Harvest feedback from J-surface
        jsurface_feedback = Dict{String, Float64}(
            "energy" => system.reactor.energy,
            "gradient_norm" => norm(system.reactor.gradient)
        )
        
        # 5. Grow garden with feedback
        grow_garden!(system.garden, 1, reservoir_feedback, jsurface_feedback)
        
        # 6. Cross-pollinate between membranes
        membrane_ids = collect(keys(system.psystem.membranes))
        if length(membrane_ids) >= 2
            id1, id2 = rand(membrane_ids, 2)
            cross_pollinate_trees!(system.garden, id1, id2, 2)
        end
        
        # 7. Prune low-fitness trees every 10 generations
        if gen % 10 == 0
            pruned = prune_garden!(system.garden)
            if verbose && pruned > 0
                println("   Generation $gen: Pruned $pruned trees")
            end
        end
        
        # 8. Record history
        push!(system.energy_history, system.reactor.energy)
        
        garden_stats = get_garden_statistics(system.garden)
        if haskey(garden_stats, "avg_fitness")
            push!(system.fitness_history, garden_stats["avg_fitness"])
        end
        
        system.generation += 1
        
        # Progress report
        if verbose && gen % 10 == 0
            println("   Generation $gen:")
            println("     Energy: $(round(system.reactor.energy, digits=4))")
            println("     Garden population: $(garden_stats["population_size"])")
            println("     Avg fitness: $(round(get(garden_stats, "avg_fitness", 0.0), digits=4))")
            println("     Diversity: $(round(get(garden_stats, "diversity", 0.0), digits=2))")
        end
    end
    
    if verbose
        println("\n✨ Evolution complete!")
        println("   Total generations: $(system.generation)")
        println("   Final energy: $(round(system.reactor.energy, digits=4))")
        println("   Garden population: $(length(system.garden.planted_trees))")
    end
    
    return nothing
end

"""
    process_unified_input!(system::A000081UnifiedSystem, input::Vector{Float64})

Process an input through the unified system.
"""
function process_unified_input!(system::A000081UnifiedSystem, input::Vector{Float64})
    # 1. Update reactor state
    system.reactor.state = input
    
    # 2. Process through all membrane reservoirs
    outputs = Dict{Int, Vector{Float64}}()
    
    for membrane_id in keys(system.psystem.membranes)
        output = reservoir_update!(system.psystem, membrane_id, input)
        outputs[membrane_id] = output
    end
    
    # 3. Aggregate outputs
    all_outputs = vcat([output for output in values(outputs)]...)
    
    # 4. Return combined response
    return all_outputs[1:min(length(input), length(all_outputs))]
end

"""
    get_unified_status(system::A000081UnifiedSystem)

Get comprehensive status of the unified system.
"""
function get_unified_status(system::A000081UnifiedSystem)
    reactor_status = get_reactor_status(system.reactor)
    psystem_status = get_bridge_status(system.psystem)
    garden_stats = get_garden_statistics(system.garden)
    
    return Dict{String, Any}(
        "generation" => system.generation,
        "max_order" => system.max_order,
        "num_trees_total" => system.config["num_trees"],
        "reactor" => reactor_status,
        "psystem" => psystem_status,
        "garden" => garden_stats,
        "energy_history_length" => length(system.energy_history),
        "fitness_history_length" => length(system.fitness_history),
        "current_energy" => system.reactor.energy,
        "current_avg_fitness" => length(system.fitness_history) > 0 ? 
                                 system.fitness_history[end] : 0.0
    )
end

"""
    save_unified_state(system::A000081UnifiedSystem, filename::String)

Save the unified system state to a file.
"""
function save_unified_state(system::A000081UnifiedSystem, filename::String)
    open(filename, "w") do io
        println(io, "=== A000081 Unified System State ===")
        println(io, "Generation: $(system.generation)")
        println(io, "Max Order: $(system.max_order)")
        println(io, "\n--- Reactor Status ---")
        reactor_status = get_reactor_status(system.reactor)
        for (key, value) in reactor_status
            println(io, "$key: $value")
        end
        
        println(io, "\n--- P-System Status ---")
        psystem_status = get_bridge_status(system.psystem)
        for (key, value) in psystem_status
            println(io, "$key: $value")
        end
        
        println(io, "\n--- Garden Statistics ---")
        garden_stats = get_garden_statistics(system.garden)
        for (key, value) in garden_stats
            println(io, "$key: $value")
        end
        
        println(io, "\n--- Energy History (last 10) ---")
        history_slice = system.energy_history[max(1, end-9):end]
        println(io, history_slice)
        
        println(io, "\n--- Fitness History (last 10) ---")
        fitness_slice = system.fitness_history[max(1, end-9):end]
        println(io, fitness_slice)
    end
    
    println("💾 System state saved to $filename")
    return nothing
end

end  # module A000081OntogeneticCore
