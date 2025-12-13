"""
Bridge between P-systems (membrane computing) and Echo State Networks.

This module implements the integration of membrane computing gardens with
reservoir computing, where each membrane contains a reservoir subnet and
communication rules transfer reservoir states.
"""

"""
    MembraneReservoir

A membrane containing an embedded echo state network reservoir.

# Fields
- `membrane::Membrane`: P-system membrane
- `reservoir::ESN`: Echo state network
- `input_channels::Vector{Symbol}`: Input channel names
- `output_channels::Vector{Symbol}`: Output channel names
- `state::Vector{Float64}`: Current reservoir state
"""
mutable struct MembraneReservoir
    membrane::Membrane
    reservoir::ESN
    input_channels::Vector{Symbol}
    output_channels::Vector{Symbol}
    state::Vector{Float64}
    
    function MembraneReservoir(membrane::Membrane,
                              reservoir_size::Int,
                              input_dim::Int,
                              output_dim::Int)
        # Create ESN reservoir
        reservoir = ESN(
            reservoir_size,
            input_dim,
            output_dim,
            reservoir_driver = RNN(),
            nla_type = NLADefault(),
            states_type = StandardStates()
        )
        
        input_channels = Symbol[]
        output_channels = Symbol[]
        state = zeros(Float64, reservoir_size)
        
        new(membrane, reservoir, input_channels, output_channels, state)
    end
end

"""
    MembraneReservoirNetwork

Network of membrane-reservoir hybrids forming a hierarchical computational structure.

# Fields
- `membranes::Dict{Int, MembraneReservoir}`: Membrane-reservoir pairs indexed by membrane ID
- `topology::PSystem`: P-system defining membrane structure and rules
- `communication_matrix::SparseMatrixCSC{Float64}`: Inter-membrane communication weights
- `generation::Int`: Current generation number
"""
mutable struct MembraneReservoirNetwork
    membranes::Dict{Int, MembraneReservoir}
    topology::PSystem
    communication_matrix::SparseMatrixCSC{Float64}
    generation::Int
    
    function MembraneReservoirNetwork(membranes::Dict{Int, MembraneReservoir},
                                     topology::PSystem)
        n = length(membranes)
        comm_matrix = spzeros(Float64, n, n)
        new(membranes, topology, comm_matrix, 0)
    end
end

"""
    create_membrane_network(psystem::PSystem,
                           reservoir_sizes::Dict{Int, Int},
                           input_dim::Int = 1,
                           output_dim::Int = 1) -> MembraneReservoirNetwork

Create a membrane-reservoir network from a P-system.

# Arguments
- `psystem::PSystem`: P-system defining membrane structure
- `reservoir_sizes::Dict{Int, Int}`: Reservoir size for each membrane
- `input_dim::Int`: Input dimension for reservoirs
- `output_dim::Int`: Output dimension for reservoirs

# Returns
- `MembraneReservoirNetwork`: Integrated network

# Examples
```julia
psystem = create_hierarchical_psystem(3, 2)
sizes = Dict(1 => 100, 2 => 50, 3 => 50)
network = create_membrane_network(psystem, sizes)
```
"""
function create_membrane_network(psystem::PSystem,
                                reservoir_sizes::Dict{Int, Int},
                                input_dim::Int = 1,
                                output_dim::Int = 1)
    membranes = Dict{Int, MembraneReservoir}()
    
    # Create membrane-reservoir for each membrane in the P-system
    for membrane in psystem.membranes
        mem_id = membrane.label
        res_size = get(reservoir_sizes, mem_id, 100)  # Default size 100
        
        mem_res = MembraneReservoir(membrane, res_size, input_dim, output_dim)
        membranes[mem_id] = mem_res
    end
    
    network = MembraneReservoirNetwork(membranes, psystem)
    
    # Initialize communication matrix based on membrane hierarchy
    initialize_communication_matrix!(network)
    
    return network
end

"""
    initialize_communication_matrix!(network::MembraneReservoirNetwork)

Initialize the communication matrix based on membrane parent-child relationships.

# Arguments
- `network::MembraneReservoirNetwork`: Network to initialize

# Examples
```julia
initialize_communication_matrix!(network)
```
"""
function initialize_communication_matrix!(network::MembraneReservoirNetwork)
    membrane_ids = sort(collect(keys(network.membranes)))
    n = length(membrane_ids)
    
    # Create mapping from membrane ID to matrix index
    id_to_idx = Dict(id => i for (i, id) in enumerate(membrane_ids))
    
    # Initialize communication weights based on parent-child relationships
    for (id, mem_res) in network.membranes
        membrane = mem_res.membrane
        parent_id = membrane.parent
        
        if parent_id !== nothing && haskey(id_to_idx, parent_id)
            i = id_to_idx[id]
            j = id_to_idx[parent_id]
            
            # Bidirectional communication with parent
            network.communication_matrix[i, j] = 0.5
            network.communication_matrix[j, i] = 0.5
        end
    end
    
    return network
end

"""
    step!(network::MembraneReservoirNetwork,
          inputs::Dict{Int, Vector{Float64}})

Execute one computational step in the membrane-reservoir network.

# Arguments
- `network::MembraneReservoirNetwork`: Network to step
- `inputs::Dict{Int, Vector{Float64}}`: Input vectors for each membrane

# Examples
```julia
inputs = Dict(1 => [0.5], 2 => [0.3])
step!(network, inputs)
```
"""
function step!(network::MembraneReservoirNetwork,
              inputs::Dict{Int, Vector{Float64}})
    # Step 1: Update reservoir states
    for (mem_id, mem_res) in network.membranes
        input = get(inputs, mem_id, zeros(Float64, 1))
        
        # Add communication from other membranes
        membrane_ids = sort(collect(keys(network.membranes)))
        id_to_idx = Dict(id => i for (i, id) in enumerate(membrane_ids))
        
        if haskey(id_to_idx, mem_id)
            idx = id_to_idx[mem_id]
            
            # Sum weighted inputs from connected membranes
            for (other_id, other_mem_res) in network.membranes
                if other_id != mem_id && haskey(id_to_idx, other_id)
                    other_idx = id_to_idx[other_id]
                    weight = network.communication_matrix[idx, other_idx]
                    
                    if weight > 0
                        # Add weighted state from other membrane
                        input = input .+ weight * mean(other_mem_res.state)
                    end
                end
            end
        end
        
        # Update reservoir state (simplified - actual ESN update is more complex)
        # This is a placeholder for the full ESN dynamics
        W = mem_res.reservoir.reservoir
        α = 0.3  # Leaking rate
        
        # x(t+1) = (1-α)x(t) + α·tanh(W·x(t) + W_in·u(t))
        if size(W, 1) == length(mem_res.state)
            new_state = (1 - α) * mem_res.state + α * tanh.(W * mem_res.state .+ input[1])
            mem_res.state = new_state
        end
    end
    
    # Step 2: Apply P-system rules (simplified)
    # In full implementation, would execute membrane computing rules
    
    return network
end

"""
    adapt_topology!(network::MembraneReservoirNetwork,
                   performance_metrics::Dict{Int, Float64})

Adapt the membrane topology based on performance metrics.

Poorly performing membranes may be dissolved, and well-performing ones
may divide to create new computational compartments.

# Arguments
- `network::MembraneReservoirNetwork`: Network to adapt
- `performance_metrics::Dict{Int, Float64}`: Performance score for each membrane

# Examples
```julia
metrics = Dict(1 => 0.9, 2 => 0.3, 3 => 0.8)
adapt_topology!(network, metrics)
```
"""
function adapt_topology!(network::MembraneReservoirNetwork,
                        performance_metrics::Dict{Int, Float64})
    # Threshold for membrane operations
    dissolution_threshold = 0.2
    division_threshold = 0.8
    
    membranes_to_remove = Int[]
    membranes_to_add = Dict{Int, MembraneReservoir}()
    
    for (mem_id, performance) in performance_metrics
        if !haskey(network.membranes, mem_id)
            continue
        end
        
        # Dissolve poorly performing membranes
        if performance < dissolution_threshold
            push!(membranes_to_remove, mem_id)
        end
        
        # Divide high-performing membranes
        if performance > division_threshold && rand() < 0.1  # 10% chance
            # Create offspring membrane
            parent_mem_res = network.membranes[mem_id]
            new_id = maximum(keys(network.membranes)) + 1
            
            # Create new membrane as child
            new_membrane = Membrane(
                new_id,
                parent_mem_res.membrane.label,
                mem_id  # Parent ID
            )
            
            # Create new reservoir with same size
            res_size = length(parent_mem_res.state)
            new_mem_res = MembraneReservoir(
                new_membrane,
                res_size,
                1,  # input_dim
                1   # output_dim
            )
            
            # Initialize with perturbed copy of parent state
            new_mem_res.state = parent_mem_res.state .+ 0.1 * randn(res_size)
            
            membranes_to_add[new_id] = new_mem_res
        end
    end
    
    # Remove dissolved membranes
    for mem_id in membranes_to_remove
        delete!(network.membranes, mem_id)
    end
    
    # Add new membranes
    for (mem_id, mem_res) in membranes_to_add
        network.membranes[mem_id] = mem_res
    end
    
    # Reinitialize communication matrix
    if !isempty(membranes_to_remove) || !isempty(membranes_to_add)
        initialize_communication_matrix!(network)
    end
    
    network.generation += 1
    
    return network
end

"""
    extract_global_state(network::MembraneReservoirNetwork) -> Vector{Float64}

Extract a global state vector from all membrane reservoirs.

# Arguments
- `network::MembraneReservoirNetwork`: Network to extract from

# Returns
- `Vector{Float64}`: Concatenated state vector

# Examples
```julia
global_state = extract_global_state(network)
```
"""
function extract_global_state(network::MembraneReservoirNetwork)
    states = Float64[]
    
    # Sort by membrane ID for consistency
    membrane_ids = sort(collect(keys(network.membranes)))
    
    for mem_id in membrane_ids
        append!(states, network.membranes[mem_id].state)
    end
    
    return states
end

"""
    membrane_network_size(network::MembraneReservoirNetwork) -> Int

Get the total number of reservoir neurons in the network.

# Arguments
- `network::MembraneReservoirNetwork`: Network to measure

# Returns
- `Int`: Total number of neurons

# Examples
```julia
size = membrane_network_size(network)
```
"""
function membrane_network_size(network::MembraneReservoirNetwork)
    total = 0
    for (_, mem_res) in network.membranes
        total += length(mem_res.state)
    end
    return total
end
