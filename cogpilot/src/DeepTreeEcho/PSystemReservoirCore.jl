"""
    PSystemReservoirCore

Enhanced P-System membrane computing integrated with echo state reservoirs.
This module implements the deep connection between:

1. **P-System Membranes**: Hierarchical containment structures
2. **Echo State Reservoirs**: Temporal pattern learning
3. **Multiset Evolution**: Membrane computing rules
4. **Tree Planting**: Rooted trees from A000081 planted in membranes
5. **Feedback Loops**: Membrane → Reservoir → Garden → Membrane cycles

# Mathematical Foundation

Each membrane m contains:
- **Multiset**: M_m = {(a, n_a), (b, n_b), ...}
- **Reservoir State**: r_m ∈ ℝ^N
- **Planted Trees**: T_m = {τ_1, τ_2, ...} ⊂ A000081
- **Evolution Rules**: R_m = {(lhs, rhs, target)}

The membrane evolves according to:

    M_m(t+1) = M_m(t) + Σ_r apply_rule(r, M_m(t))
    r_m(t+1) = tanh(W_m · r_m(t) + U_m · encode(M_m(t)))

Where trees in T_m influence the reservoir connectivity W_m.

# Feedback Mechanism

The feedback loop operates as:

    Trees → Reservoir Connectivity → Reservoir State → Multiset Encoding
    → Evolution Rules → New Trees → Plant in Membranes → ...

This creates a self-organizing system where the structure (trees) and
dynamics (reservoir) co-evolve through membrane computing.
"""
module PSystemReservoirCore

using LinearAlgebra
using Random

export PSystemReservoirBridge
export create_psystem_reservoir, plant_tree_in_membrane!
export evolve_membrane!, reservoir_update!, harvest_feedback!
export cross_membrane_pollination!, membrane_status, get_bridge_status

"""
    Multiset

Represents a multiset as a dictionary of objects to counts.
"""
mutable struct Multiset
    objects::Dict{String, Int}
    
    Multiset() = new(Dict{String, Int}())
    Multiset(pairs::Pair...) = new(Dict{String, Int}(pairs...))
end

"""
    EvolutionRule

P-system evolution rule: lhs → rhs (target membrane)
"""
struct EvolutionRule
    membrane_id::Int
    lhs::Multiset
    rhs::Multiset
    target_membrane::Int  # 0 = same membrane, >0 = send to target
    
    function EvolutionRule(membrane_id::Int, lhs::Multiset, rhs::Multiset, 
                          target::Int=0)
        new(membrane_id, lhs, rhs, target)
    end
end

"""
    Membrane

A single membrane in the P-system hierarchy.
"""
mutable struct Membrane
    id::Int
    parent_id::Int
    children_ids::Vector{Int}
    multiset::Multiset
    reservoir_state::Vector{Float64}
    reservoir_weights::Matrix{Float64}
    planted_trees::Vector{Vector{Int}}
    evolution_rules::Vector{EvolutionRule}
    energy::Float64
    
    function Membrane(id::Int, reservoir_size::Int; parent_id::Int=0)
        multiset = Multiset()
        reservoir_state = zeros(reservoir_size)
        reservoir_weights = randn(reservoir_size, reservoir_size) * 0.1
        planted_trees = Vector{Int}[]
        evolution_rules = EvolutionRule[]
        energy = 0.0
        
        new(id, parent_id, Int[], multiset, reservoir_state, 
            reservoir_weights, planted_trees, evolution_rules, energy)
    end
end

"""
    PSystemReservoirBridge

The bridge between P-system membranes and echo state reservoirs.

# Fields
- `membranes::Dict{Int, Membrane}`: All membranes by ID
- `root_id::Int`: Root membrane ID
- `reservoir_size::Int`: Size of each reservoir
- `alphabet::Vector{String}`: Multiset alphabet
- `input_weights::Matrix{Float64}`: Input encoding weights
- `generation::Int`: Current generation
- `feedback_history::Vector{Dict}`: History of feedback signals
"""
mutable struct PSystemReservoirBridge
    membranes::Dict{Int, Membrane}
    root_id::Int
    reservoir_size::Int
    alphabet::Vector{String}
    input_weights::Matrix{Float64}
    generation::Int
    feedback_history::Vector{Dict{String, Any}}
    
    function PSystemReservoirBridge(
        num_membranes::Int,
        reservoir_size::Int;
        alphabet::Vector{String}=["a", "b", "c", "d", "e"]
    )
        membranes = Dict{Int, Membrane}()
        
        # Create root membrane
        root = Membrane(1, reservoir_size)
        membranes[1] = root
        
        # Create child membranes
        for i in 2:num_membranes
            membrane = Membrane(i, reservoir_size, parent_id=1)
            membranes[i] = membrane
            push!(root.children_ids, i)
        end
        
        # Input encoding weights
        input_weights = randn(reservoir_size, length(alphabet)) * 0.1
        
        new(membranes, 1, reservoir_size, alphabet, input_weights, 0, [])
    end
end

"""
    create_psystem_reservoir(num_membranes::Int, reservoir_size::Int; kwargs...)

Create a P-system reservoir bridge.
"""
function create_psystem_reservoir(
    num_membranes::Int,
    reservoir_size::Int;
    alphabet::Vector{String}=["a", "b", "c", "d", "e"]
)
    return PSystemReservoirBridge(num_membranes, reservoir_size; alphabet=alphabet)
end

"""
    plant_tree_in_membrane!(bridge::PSystemReservoirBridge, 
                           tree::Vector{Int}, membrane_id::Int)

Plant a rooted tree in a membrane, affecting its reservoir connectivity.
"""
function plant_tree_in_membrane!(
    bridge::PSystemReservoirBridge,
    tree::Vector{Int},
    membrane_id::Int
)
    if !haskey(bridge.membranes, membrane_id)
        error("Membrane $membrane_id does not exist")
    end
    
    membrane = bridge.membranes[membrane_id]
    
    # Add tree to planted trees
    push!(membrane.planted_trees, tree)
    
    # Update reservoir connectivity based on tree structure
    update_reservoir_from_tree!(membrane, tree)
    
    # Add multiset objects based on tree
    tree_encoding = encode_tree_to_multiset(tree, bridge.alphabet)
    for (obj, count) in tree_encoding.objects
        membrane.multiset.objects[obj] = get(membrane.multiset.objects, obj, 0) + count
    end
    
    return nothing
end

"""
    update_reservoir_from_tree!(membrane::Membrane, tree::Vector{Int})

Update reservoir weights based on tree structure.
"""
function update_reservoir_from_tree!(membrane::Membrane, tree::Vector{Int})
    n = length(membrane.reservoir_state)
    
    # Map tree nodes to reservoir indices
    for i in 1:(length(tree)-1)
        # Create connection between consecutive levels
        idx1 = mod(hash(tree[1:i]), n) + 1
        idx2 = mod(hash(tree[1:i+1]), n) + 1
        
        # Strengthen connection
        weight = 0.1 / sqrt(length(tree))
        membrane.reservoir_weights[idx1, idx2] += weight
        membrane.reservoir_weights[idx2, idx1] += weight  # Symmetric
    end
    
    # Normalize spectral radius
    eigenvalues = eigvals(membrane.reservoir_weights)
    spectral_radius = maximum(abs.(eigenvalues))
    if spectral_radius > 1.0
        membrane.reservoir_weights *= (0.95 / spectral_radius)
    end
    
    return nothing
end

"""
    encode_tree_to_multiset(tree::Vector{Int}, alphabet::Vector{String})

Encode a tree as a multiset.
"""
function encode_tree_to_multiset(tree::Vector{Int}, alphabet::Vector{String})
    multiset = Multiset()
    
    for level in tree
        # Map level to alphabet symbol
        idx = mod(level - 1, length(alphabet)) + 1
        symbol = alphabet[idx]
        multiset.objects[symbol] = get(multiset.objects, symbol, 0) + 1
    end
    
    return multiset
end

"""
    reservoir_update!(bridge::PSystemReservoirBridge, membrane_id::Int, 
                     input::Vector{Float64})

Update reservoir state in a membrane.
"""
function reservoir_update!(
    bridge::PSystemReservoirBridge,
    membrane_id::Int,
    input::Vector{Float64}
)
    membrane = bridge.membranes[membrane_id]
    
    # Encode multiset as input
    multiset_input = encode_multiset_to_vector(membrane.multiset, bridge.alphabet)
    
    # Combine external input and multiset
    combined_input = vcat(input, multiset_input)
    if length(combined_input) < bridge.reservoir_size
        combined_input = vcat(combined_input, 
                             zeros(bridge.reservoir_size - length(combined_input)))
    else
        combined_input = combined_input[1:bridge.reservoir_size]
    end
    
    # Reservoir update: r(t+1) = tanh(W·r(t) + U·input)
    # Ensure input weights have correct dimensions
    input_contrib = zeros(bridge.reservoir_size)
    for i in 1:min(length(combined_input), size(bridge.input_weights, 2))
        input_contrib += bridge.input_weights[:, i] * combined_input[i]
    end
    
    new_state = tanh.(
        membrane.reservoir_weights * membrane.reservoir_state +
        input_contrib
    )
    
    membrane.reservoir_state = new_state
    
    # Update energy
    membrane.energy = 0.5 * dot(new_state, new_state)
    
    return new_state
end

"""
    encode_multiset_to_vector(multiset::Multiset, alphabet::Vector{String})

Encode multiset as a vector.
"""
function encode_multiset_to_vector(multiset::Multiset, alphabet::Vector{String})
    vec = zeros(length(alphabet))
    
    for (i, symbol) in enumerate(alphabet)
        vec[i] = get(multiset.objects, symbol, 0)
    end
    
    return vec
end

"""
    evolve_membrane!(bridge::PSystemReservoirBridge, membrane_id::Int)

Apply evolution rules to a membrane.
"""
function evolve_membrane!(bridge::PSystemReservoirBridge, membrane_id::Int)
    membrane = bridge.membranes[membrane_id]
    
    # Apply each evolution rule
    for rule in membrane.evolution_rules
        if can_apply_rule(rule, membrane.multiset)
            apply_rule!(rule, membrane, bridge.membranes)
        end
    end
    
    return nothing
end

"""
    can_apply_rule(rule::EvolutionRule, multiset::Multiset)

Check if a rule can be applied to a multiset.
"""
function can_apply_rule(rule::EvolutionRule, multiset::Multiset)
    for (obj, count) in rule.lhs.objects
        if get(multiset.objects, obj, 0) < count
            return false
        end
    end
    return true
end

"""
    apply_rule!(rule::EvolutionRule, membrane::Membrane, 
               all_membranes::Dict{Int, Membrane})

Apply an evolution rule.
"""
function apply_rule!(
    rule::EvolutionRule,
    membrane::Membrane,
    all_membranes::Dict{Int, Membrane}
)
    # Remove lhs from current membrane
    for (obj, count) in rule.lhs.objects
        membrane.multiset.objects[obj] -= count
        if membrane.multiset.objects[obj] <= 0
            delete!(membrane.multiset.objects, obj)
        end
    end
    
    # Add rhs to target membrane
    target_id = rule.target_membrane == 0 ? membrane.id : rule.target_membrane
    if haskey(all_membranes, target_id)
        target = all_membranes[target_id]
        for (obj, count) in rule.rhs.objects
            target.multiset.objects[obj] = get(target.multiset.objects, obj, 0) + count
        end
    end
    
    return nothing
end

"""
    harvest_feedback!(bridge::PSystemReservoirBridge, membrane_id::Int)

Harvest feedback from a membrane's reservoir state.
"""
function harvest_feedback!(bridge::PSystemReservoirBridge, membrane_id::Int)
    membrane = bridge.membranes[membrane_id]
    
    feedback = Dict{String, Any}(
        "membrane_id" => membrane_id,
        "generation" => bridge.generation,
        "reservoir_state" => copy(membrane.reservoir_state),
        "energy" => membrane.energy,
        "num_trees" => length(membrane.planted_trees),
        "multiset_size" => sum(values(membrane.multiset.objects)),
        "reservoir_activity" => norm(membrane.reservoir_state)
    )
    
    push!(bridge.feedback_history, feedback)
    
    return feedback
end

"""
    cross_membrane_pollination!(bridge::PSystemReservoirBridge, 
                                source_id::Int, target_id::Int, num_trees::Int)

Cross-pollinate trees between membranes.
"""
function cross_membrane_pollination!(
    bridge::PSystemReservoirBridge,
    source_id::Int,
    target_id::Int,
    num_trees::Int
)
    if !haskey(bridge.membranes, source_id) || !haskey(bridge.membranes, target_id)
        return nothing
    end
    
    source = bridge.membranes[source_id]
    target = bridge.membranes[target_id]
    
    if isempty(source.planted_trees)
        return nothing
    end
    
    # Select random trees from source
    n_transfer = min(num_trees, length(source.planted_trees))
    transferred_trees = rand(source.planted_trees, n_transfer)
    
    # Plant in target
    for tree in transferred_trees
        # Create hybrid tree (simple crossover)
        if !isempty(target.planted_trees)
            target_tree = rand(target.planted_trees)
            hybrid = create_hybrid_tree(tree, target_tree)
            push!(target.planted_trees, hybrid)
            update_reservoir_from_tree!(target, hybrid)
        else
            push!(target.planted_trees, tree)
            update_reservoir_from_tree!(target, tree)
        end
    end
    
    return n_transfer
end

"""
    create_hybrid_tree(tree1::Vector{Int}, tree2::Vector{Int})

Create a hybrid tree from two parent trees.
"""
function create_hybrid_tree(tree1::Vector{Int}, tree2::Vector{Int})
    # Simple crossover: take prefix from tree1, suffix from tree2
    len1 = length(tree1)
    len2 = length(tree2)
    
    if len1 == 0
        return copy(tree2)
    elseif len2 == 0
        return copy(tree1)
    end
    
    # Crossover point
    point = rand(1:min(len1, len2))
    
    hybrid = vcat(tree1[1:point], tree2[(point+1):end])
    
    # Ensure valid level sequence (non-decreasing at start)
    if !isempty(hybrid) && hybrid[1] != 1
        hybrid[1] = 1
    end
    
    return hybrid
end

"""
    membrane_status(bridge::PSystemReservoirBridge, membrane_id::Int)

Get status of a membrane.
"""
function membrane_status(bridge::PSystemReservoirBridge, membrane_id::Int)
    if !haskey(bridge.membranes, membrane_id)
        return nothing
    end
    
    membrane = bridge.membranes[membrane_id]
    
    return Dict(
        "id" => membrane.id,
        "parent_id" => membrane.parent_id,
        "num_children" => length(membrane.children_ids),
        "num_trees" => length(membrane.planted_trees),
        "multiset_size" => sum(values(membrane.multiset.objects)),
        "energy" => membrane.energy,
        "reservoir_activity" => norm(membrane.reservoir_state),
        "num_rules" => length(membrane.evolution_rules)
    )
end

"""
    get_bridge_status(bridge::PSystemReservoirBridge)

Get overall bridge status.
"""
function get_bridge_status(bridge::PSystemReservoirBridge)
    total_trees = sum(length(m.planted_trees) for m in values(bridge.membranes))
    total_energy = sum(m.energy for m in values(bridge.membranes))
    
    return Dict(
        "num_membranes" => length(bridge.membranes),
        "reservoir_size" => bridge.reservoir_size,
        "generation" => bridge.generation,
        "total_trees" => total_trees,
        "total_energy" => total_energy,
        "feedback_history_length" => length(bridge.feedback_history)
    )
end

end  # module PSystemReservoirCore
