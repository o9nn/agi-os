"""
    DeepTreeEcho

The Deep Tree Echo State Reservoir Computer - a unified cognitive architecture
integrating Echo State Networks, B-Series ridges, P-System membrane computing,
and rooted tree gardens, all orchestrated by the ontogenetic engine following
the OEIS A000081 sequence.

# Architecture

The system operates through five integrated layers:

1. **Rooted Tree Foundation** (A000081): Fundamental structures
2. **B-Series Computational Ridges**: Numerical integration methods
3. **Reservoir Echo States**: Temporal pattern learning
4. **Membrane Computing Gardens**: Evolutionary containers
5. **J-Surface Reactor Core**: Unified gradient-evolution dynamics

# Main Components

- `DeepTreeEchoSystem`: Main system orchestrator
- `JSurfaceReactor`: Gradient flow and evolution
- `BSeriesRidge`: Computational ridges
- `PSystemReservoir`: Membrane computing
- `MembraneGarden`: Tree cultivation
- `OntogeneticEngine`: A000081-based generation
- `TaskflowIntegration`: Parallel task graph execution
- `A000081Parameters`: Parameter derivation from OEIS A000081

# Parameter Philosophy: A000081 Alignment

**CRITICAL**: All system parameters MUST be derived from the OEIS A000081 sequence
to ensure mathematical consistency with the rooted tree topology.

A000081: {1, 1, 2, 4, 9, 20, 48, 115, 286, 719, ...}

## Recommended Usage (A000081-Derived Parameters)

```julia
using DeepTreeEcho

# Option 1: Let the system derive all parameters (RECOMMENDED)
system = DeepTreeEchoSystem(base_order=5)

# Option 2: Use explicit parameter set derivation
params = get_parameter_set(5, membrane_order=4)
system = DeepTreeEchoSystem(
    reservoir_size = params.reservoir_size,   # 17 (cumulative trees)
    max_tree_order = params.max_tree_order,   # 8
    num_membranes = params.num_membranes,     # 4 (A000081[4])
    growth_rate = params.growth_rate,         # ≈2.22 (20/9)
    mutation_rate = params.mutation_rate      # ≈0.11 (1/9)
)

# Explain parameter derivation
explain_parameters(params)
```

## Legacy Usage (Manual Parameters - NOT RECOMMENDED)

```julia
# This will show warnings if parameters don't align with A000081
system = DeepTreeEchoSystem(
    reservoir_size = 100,  # ⚠ Arbitrary value
    max_tree_order = 8,
    num_membranes = 3      # ⚠ Not in A000081[1:6]
)
```

# Taskflow Integration

```julia
using DeepTreeEcho
using DeepTreeEcho.TaskflowIntegration

# Create hybrid system with parallel execution
system = DeepTreeEchoSystem(base_order=5)
tf_system = TaskflowOntogeneticSystem(system, num_threads=8)

# Evolve with parallel task graphs
evolve_with_taskflow!(tf_system, 30, verbose=true)
```
"""
module DeepTreeEcho

using LinearAlgebra
using Random
using Statistics

# Include submodules
include("A000081Parameters.jl")
include("JSurfaceReactor.jl")
include("BSeriesRidge.jl")
include("PSystemReservoir.jl")
include("MembraneGarden.jl")
include("OntogeneticEngine.jl")
include("TaskflowIntegration.jl")
include("PackageIntegration.jl")
include("Visualization.jl")

using .A000081Parameters
using .JSurfaceReactor
using .BSeriesRidge
using .PSystemReservoir
using .MembraneGarden
using .OntogeneticEngine
using .TaskflowIntegration
using .PackageIntegration
using .Visualization

export DeepTreeEchoSystem
export initialize!, evolve!, process_input!, get_system_status
export plant_trees!, harvest_feedback!, adapt_topology!
export TaskflowOntogeneticSystem, evolve_with_taskflow!
export get_parameter_set, explain_parameters, validate_parameters, A000081ParameterSet

"""
    DeepTreeEchoSystem

Main system integrating all components of the Deep Tree Echo architecture.

# Fields
- `jsurface::JSurface`: J-surface reactor core
- `jsurface_state::JSurfaceState`: Current J-surface state
- `ridge::Ridge`: B-series computational ridge
- `reservoir::MembraneReservoir`: P-system membrane reservoir
- `garden::Garden`: Membrane computing garden
- `generator::A000081Generator`: Tree generator
- `ontogenetic_state::OntogeneticState`: Ontogenetic evolution state
- `config::Dict`: System configuration
- `step_count::Int`: Total evolution steps
"""
mutable struct DeepTreeEchoSystem
    jsurface::JSurface
    jsurface_state::JSurfaceState
    ridge::Ridge
    reservoir::MembraneReservoir
    garden::Garden
    generator::A000081Generator
    ontogenetic_state::OntogeneticState
    config::Dict
    step_count::Int
    
    function DeepTreeEchoSystem(;
        reservoir_size::Union{Int,Nothing}=nothing,
        max_tree_order::Union{Int,Nothing}=nothing,
        num_membranes::Union{Int,Nothing}=nothing,
        symplectic::Bool=true,
        growth_rate::Union{Float64,Nothing}=nothing,
        mutation_rate::Union{Float64,Nothing}=nothing,
        base_order::Int=5)
        
        # Track if parameters are A000081-aligned
        is_a000081_aligned = false
        
        # Derive parameters from A000081 if not provided
        if any(isnothing.([reservoir_size, max_tree_order, num_membranes, growth_rate, mutation_rate]))
            println("\n⚠ Some parameters not provided - deriving from A000081 (base_order=$base_order)...")
            params = get_parameter_set(base_order, membrane_order=3)
            
            reservoir_size = isnothing(reservoir_size) ? params.reservoir_size : reservoir_size
            max_tree_order = isnothing(max_tree_order) ? params.max_tree_order : max_tree_order
            num_membranes = isnothing(num_membranes) ? params.num_membranes : num_membranes
            growth_rate = isnothing(growth_rate) ? params.growth_rate : growth_rate
            mutation_rate = isnothing(mutation_rate) ? params.mutation_rate : mutation_rate
            
            println("✓ Derived A000081-aligned parameters:")
            println("  reservoir_size  = $reservoir_size (cumulative trees up to order $base_order)")
            println("  max_tree_order  = $max_tree_order")
            println("  num_membranes   = $num_membranes (A000081[3])")
            println("  growth_rate     = $(round(growth_rate, digits=4))")
            println("  mutation_rate   = $(round(mutation_rate, digits=4))")
            
            is_a000081_aligned = true
        else
            # Validate provided parameters
            is_valid, message = validate_parameters(reservoir_size, max_tree_order, num_membranes,
                                                   growth_rate, mutation_rate)
            if !is_valid
                println("\n⚠ WARNING: Parameters may not align with A000081 topology:")
                for line in split(message, "\n")
                    println("  $line")
                end
                println("  Consider using get_parameter_set() for mathematically justified parameters.")
                println()
            end
            is_a000081_aligned = is_valid
        end
        
        # Create J-surface
        jsurface = create_jsurface(reservoir_size; symplectic=symplectic)
        # Use A000081-derived value for population size
        population_size = A000081Parameters.derive_num_membranes(4)  # 4 → 4 populations
        jsurface_state = JSurfaceState(reservoir_size, population_size)
        
        # Create B-series ridge
        ridge = create_ridge(max_tree_order)
        
        # Create membrane reservoir with structure based on num_membranes
        membrane_structure = if num_membranes == 1
            "[]'1"
        elseif num_membranes == 2
            "[[]'2]'1"
        elseif num_membranes == 4
            "[[]'2 []'3 []'4]'1"
        else
            "[[]'2 []'3]'1"  # Default for other values
        end
        
        reservoir = create_membrane_reservoir(
            membrane_structure,
            alphabet=["a", "b", "c", "d", "e"]
        )
        
        # Create garden
        dynamics = GrowthDynamics(
            growth_rate=growth_rate,
            mutation_rate=mutation_rate
        )
        garden = create_garden(dynamics=dynamics)
        
        # Create ontogenetic engine
        generator = A000081Generator(max_tree_order)
        ontogenetic_state = OntogeneticState()
        
        # Configuration
        config = Dict(
            "reservoir_size" => reservoir_size,
            "max_tree_order" => max_tree_order,
            "num_membranes" => num_membranes,
            "symplectic" => symplectic,
            "growth_rate" => growth_rate,
            "mutation_rate" => mutation_rate,
            "base_order" => base_order,
            "a000081_aligned" => is_a000081_aligned
        )
        
        new(jsurface, jsurface_state, ridge, reservoir, garden,
            generator, ontogenetic_state, config, 0)
    end
end

"""
    initialize!(system::DeepTreeEchoSystem; seed_trees::Int=10)

Initialize the system with seed trees from A000081.

# Arguments
- `system::DeepTreeEchoSystem`: The system to initialize
- `seed_trees::Int=10`: Number of initial trees
"""
function initialize!(system::DeepTreeEchoSystem; seed_trees::Int=10)
    println("Initializing Deep Tree Echo System...")
    
    # Generate initial tree population from A000081
    initial_trees = Vector{Int}[]
    for order in 1:min(5, system.config["max_tree_order"])
        trees = generate_a000081_trees(system.generator, order)
        # Take a few from each order
        n_take = min(2, length(trees))
        append!(initial_trees, trees[1:n_take])
    end
    
    # Ensure we have enough trees
    while length(initial_trees) < seed_trees
        order = rand(1:system.config["max_tree_order"])
        trees = generate_a000081_trees(system.generator, order)
        if !isempty(trees)
            push!(initial_trees, rand(trees))
        end
    end
    
    # Initialize ontogenetic state
    system.ontogenetic_state = OntogeneticState(initial_trees[1:seed_trees])
    
    # Plant trees in membranes
    membrane_ids = collect(keys(system.reservoir.membranes))
    for tree in initial_trees[1:seed_trees]
        membrane_id = rand(membrane_ids)
        plant_tree!(system.garden, tree, membrane_id)
    end
    
    # Initialize reservoir states
    for (membrane_id, membrane) in system.reservoir.membranes
        # Encode initial state
        state = randn(system.config["reservoir_size"])
        membrane.reservoir_state = state
        
        # Create initial multiset
        multiset = encode_state_as_multiset(state, system.reservoir.alphabet)
        membrane.multiset = multiset
    end
    
    println("✓ Initialized with $(seed_trees) trees across $(length(membrane_ids)) membranes")
    println("✓ A000081 generator ready up to order $(system.config["max_tree_order"])")
    
    return nothing
end

"""
    evolve!(system::DeepTreeEchoSystem, 
           generations::Int;
           dt::Float64=0.01,
           verbose::Bool=true)

Evolve the entire system for multiple generations.

# Arguments
- `system::DeepTreeEchoSystem`: The system
- `generations::Int`: Number of generations
- `dt::Float64=0.01`: Time step for J-surface integration
- `verbose::Bool=true`: Print progress
"""
function evolve!(system::DeepTreeEchoSystem, 
                generations::Int;
                dt::Float64=0.01,
                verbose::Bool=true)
    
    if verbose
        println("\nEvolving Deep Tree Echo System for $generations generations...")
    end
    
    for gen in 1:generations
        # 1. J-Surface gradient flow
        symplectic_integrate!(system.jsurface, system.jsurface_state, dt)
        
        # 2. Ontogenetic evolution step
        ontogenetic_step!(
            system.ontogenetic_state,
            system.generator,
            selection_pressure=0.5,
            mutation_rate=system.config["mutation_rate"]
        )
        
        # 3. Grow trees in garden
        grow_trees!(system.garden, 1)
        
        # 4. Evolve membrane reservoir
        evolve_membrane!(system.reservoir, 1)
        
        # 5. Cross-pollinate between membranes
        membrane_ids = collect(keys(system.reservoir.membranes))
        if length(membrane_ids) >= 2
            id1, id2 = rand(membrane_ids, 2)
            cross_pollinate!(system.garden, id1, id2, 2)
        end
        
        # 6. Unify components
        unify_components!(
            system.ontogenetic_state,
            system.ridge.coefficients,
            system.jsurface_state.position,
            system.reservoir.membranes[1].multiset.objects
        )
        
        # 7. Prune low-fitness trees
        if gen % 10 == 0
            pruned = prune_garden!(system.garden)
            if verbose && pruned > 0
                println("  Generation $gen: Pruned $pruned trees")
            end
        end
        
        system.step_count += 1
        
        # Print progress
        if verbose && gen % 10 == 0
            stats = get_ontogenetic_statistics(system.ontogenetic_state)
            println("  Generation $gen: fitness=$(round(stats["avg_fitness"], digits=3)), diversity=$(round(stats["diversity"], digits=2))")
        end
    end
    
    if verbose
        println("✓ Evolution complete: $(system.step_count) total steps")
    end
    
    return nothing
end

"""
    process_input!(system::DeepTreeEchoSystem, 
                  input::Vector{Float64})

Process an input through the system.

# Arguments
- `system::DeepTreeEchoSystem`: The system
- `input::Vector{Float64}`: Input vector

# Returns
- `Vector{Float64}`: System response
"""
function process_input!(system::DeepTreeEchoSystem, 
                       input::Vector{Float64})
    # 1. Encode input as membrane multiset
    root_membrane = system.reservoir.membranes[system.reservoir.root_id]
    input_multiset = encode_state_as_multiset(input, system.reservoir.alphabet)
    
    # Add to root membrane
    for (obj, count) in input_multiset.objects
        root_membrane.multiset.objects[obj] = 
            get(root_membrane.multiset.objects, obj, 0) + count
    end
    
    # 2. Evolve membrane system
    evolve_membrane!(system.reservoir, 3)
    
    # 3. Harvest feedback from garden
    feedback = harvest_feedback!(system.garden, system.reservoir.root_id)
    
    # 4. Update J-surface state
    if !isempty(feedback)
        n = min(length(feedback), length(system.jsurface_state.position))
        system.jsurface_state.position[1:n] = feedback[1:n]
    end
    
    # 5. Gradient flow
    gradient_flow!(system.jsurface, system.jsurface_state, 0.01)
    
    # 6. Decode response
    response = decode_multiset_to_state(
        root_membrane.multiset,
        system.reservoir.alphabet,
        length(input)
    )
    
    return response
end

"""
    plant_trees!(system::DeepTreeEchoSystem, 
                trees::Vector{Vector{Int}},
                membrane_id::Int)

Plant multiple trees in a specific membrane.

# Arguments
- `system::DeepTreeEchoSystem`: The system
- `trees::Vector{Vector{Int}}`: Trees to plant
- `membrane_id::Int`: Target membrane
"""
function plant_trees!(system::DeepTreeEchoSystem, 
                     trees::Vector{Vector{Int}},
                     membrane_id::Int)
    for tree in trees
        plant_tree!(system.garden, tree, membrane_id)
    end
    return nothing
end

"""
    harvest_feedback!(system::DeepTreeEchoSystem)

Harvest feedback from all membranes.

# Returns
- `Dict{Int, Vector{Float64}}`: Feedback from each membrane
"""
function harvest_feedback!(system::DeepTreeEchoSystem)
    feedback = Dict{Int, Vector{Float64}}()
    
    for membrane_id in keys(system.reservoir.membranes)
        fb = MembraneGarden.harvest_feedback!(system.garden, membrane_id)
        if !isempty(fb)
            feedback[membrane_id] = fb
        end
    end
    
    return feedback
end

"""
    adapt_topology!(system::DeepTreeEchoSystem; 
                   add_membrane::Bool=false,
                   dissolve_membrane::Union{Int,Nothing}=nothing)

Adapt the membrane topology.

# Arguments
- `system::DeepTreeEchoSystem`: The system
- `add_membrane::Bool=false`: Add a new membrane
- `dissolve_membrane::Union{Int,Nothing}=nothing`: Membrane to dissolve
"""
function adapt_topology!(system::DeepTreeEchoSystem; 
                        add_membrane::Bool=false,
                        dissolve_membrane::Union{Int,Nothing}=nothing)
    if add_membrane
        # Add new child membrane to root
        new_id = maximum(keys(system.reservoir.membranes)) + 1
        add_child_membrane!(
            system.reservoir,
            system.reservoir.root_id,
            new_id,
            new_id
        )
        println("Added membrane $new_id")
    end
    
    if !isnothing(dissolve_membrane) && dissolve_membrane != system.reservoir.root_id
        dissolve_membrane!(system.reservoir, dissolve_membrane)
        println("Dissolved membrane $dissolve_membrane")
    end
    
    return nothing
end

"""
    get_system_status(system::DeepTreeEchoSystem)

Get comprehensive system status.

# Returns
- `Dict`: System status information
"""
function get_system_status(system::DeepTreeEchoSystem)
    onto_stats = get_ontogenetic_statistics(system.ontogenetic_state)
    
    garden_stats = Dict{Int, Dict}()
    for membrane_id in keys(system.reservoir.membranes)
        garden_stats[membrane_id] = get_tree_population(system.garden, membrane_id)
    end
    
    return Dict(
        "step_count" => system.step_count,
        "ontogenetic" => onto_stats,
        "garden" => garden_stats,
        "num_membranes" => length(system.reservoir.membranes),
        "ridge_order" => system.ridge.order,
        "jsurface_energy" => system.jsurface.hamiltonian(system.jsurface_state.position),
        "config" => system.config
    )
end

"""
    print_system_status(system::DeepTreeEchoSystem)

Print detailed system status.
"""
function print_system_status(system::DeepTreeEchoSystem)
    println("\n" * "="^60)
    println("DEEP TREE ECHO STATE RESERVOIR COMPUTER")
    println("="^60)
    println()
    
    # Ontogenetic status
    print_ontogenetic_status(system.ontogenetic_state)
    println()
    
    # Garden status
    print_garden_status(system.garden)
    println()
    
    # Membrane reservoir status
    println("=== Membrane Reservoir Status ===")
    print_membrane_structure(system.reservoir)
    println()
    
    # J-Surface status
    println("=== J-Surface Reactor Status ===")
    println("Energy: $(round(system.jsurface.hamiltonian(system.jsurface_state.position), digits=6))")
    println("Velocity norm: $(round(norm(system.jsurface_state.velocity), digits=6))")
    println("Generation: $(system.jsurface_state.generation)")
    println()
    
    # B-Series Ridge status
    println("=== B-Series Ridge Status ===")
    println("Order: $(system.ridge.order)")
    println("Dimension: $(system.ridge.dimension)")
    println("Coefficient norm: $(round(norm(system.ridge.coefficients), digits=6))")
    println()
    
    println("Total system steps: $(system.step_count)")
    println("="^60)
end

"""
    save_system_state(system::DeepTreeEchoSystem, filename::String)

Save the current system state to a file.

# Arguments
- `system::DeepTreeEchoSystem`: The system
- `filename::String`: Output filename
"""
function save_system_state(system::DeepTreeEchoSystem, filename::String)
    status = get_system_status(system)
    
    # Convert to JSON-like format (simplified)
    open(filename, "w") do io
        println(io, "# Deep Tree Echo System State")
        println(io, "# Generated: $(now())")
        println(io)
        
        for (key, value) in status
            println(io, "$key: $value")
        end
    end
    
    println("System state saved to $filename")
    return nothing
end

end # module DeepTreeEcho
