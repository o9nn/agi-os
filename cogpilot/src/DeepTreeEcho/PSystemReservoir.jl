"""
    PSystemReservoir

P-System membrane computing reservoirs that provide hierarchical containment
and evolution of computational states.

Membranes act as boundaries for echo state reservoirs, with multisets
representing reservoir states and evolution rules implementing dynamics.
"""
module PSystemReservoir

using LinearAlgebra
using Random

export MembraneReservoir, MembraneState, EvolutionRule
export create_membrane_reservoir, evolve_membrane!, apply_rules!
export encode_state_as_multiset, decode_multiset_to_state
export membrane_communication!, dissolve_membrane!

"""
    Multiset

A multiset is a collection of objects with multiplicities.
"""
struct Multiset
    objects::Dict{String, Int}
    
    Multiset() = new(Dict{String, Int}())
    Multiset(pairs::Pair...) = new(Dict{String, Int}(pairs...))
    Multiset(dict::Dict{String, Int}) = new(dict)
end

"""
    EvolutionRule

An evolution rule for membrane computing.

Format: [lhs]'label → [rhs]'label
"""
struct EvolutionRule
    membrane_label::Int
    lhs::Multiset
    rhs::Multiset
    target::Union{Int, Symbol, Nothing}  # :out, :in_X, or membrane id
    dissolve::Bool
    priority::Int
    
    function EvolutionRule(label::Int, lhs::Multiset, rhs::Multiset;
                          target::Union{Int, Symbol, Nothing}=nothing,
                          dissolve::Bool=false,
                          priority::Int=0)
        new(label, lhs, rhs, target, dissolve, priority)
    end
end

"""
    MembraneState

State of a single membrane including its multiset and children.

Fields:
- `id::Int`: Unique membrane identifier
- `label::Int`: Membrane type/label
- `parent::Union{Int, Nothing}`: Parent membrane id
- `children::Vector{Int}`: Child membrane ids
- `multiset::Multiset`: Current multiset of objects
- `reservoir_state::Union{Vector{Float64}, Nothing}`: Associated reservoir state
"""
mutable struct MembraneState
    id::Int
    label::Int
    parent::Union{Int, Nothing}
    children::Vector{Int}
    multiset::Multiset
    reservoir_state::Union{Vector{Float64}, Nothing}
    
    function MembraneState(id::Int, label::Int, parent::Union{Int, Nothing}=nothing)
        new(id, label, parent, Int[], Multiset(), nothing)
    end
end

"""
    MembraneReservoir

A hierarchical membrane computing reservoir.

Fields:
- `membranes::Dict{Int, MembraneState}`: All membranes indexed by id
- `rules::Vector{EvolutionRule}`: Evolution rules
- `root_id::Int`: Root membrane id
- `alphabet::Vector{String}`: Object alphabet
- `step_count::Int`: Number of evolution steps taken
"""
mutable struct MembraneReservoir
    membranes::Dict{Int, MembraneState}
    rules::Vector{EvolutionRule}
    root_id::Int
    alphabet::Vector{String}
    step_count::Int
    
    function MembraneReservoir(root_id::Int=1)
        membranes = Dict{Int, MembraneState}()
        membranes[root_id] = MembraneState(root_id, 1, nothing)
        
        new(membranes, EvolutionRule[], root_id, String[], 0)
    end
end

"""
    create_membrane_reservoir(structure::String; alphabet::Vector{String}=String[])

Create a membrane reservoir from a structure specification.

# Arguments
- `structure::String`: Membrane structure in P-Lingua notation (e.g., "[[]'2]'1")
- `alphabet::Vector{String}`: Object alphabet

# Returns
- `MembraneReservoir`: Constructed membrane reservoir

# Example
```julia
reservoir = create_membrane_reservoir("[[]'2 []'3]'1", alphabet=["a", "b", "c"])
```
"""
function create_membrane_reservoir(structure::String="[]'1"; 
                                  alphabet::Vector{String}=String[])
    # Simplified parser - full version would parse P-Lingua syntax
    reservoir = MembraneReservoir(1)
    reservoir.alphabet = alphabet
    
    # For now, create a simple 3-level hierarchy
    # Root membrane 1
    # ├── Child membrane 2
    # └── Child membrane 3
    
    if occursin("2", structure)
        add_child_membrane!(reservoir, 1, 2, 2)
    end
    
    if occursin("3", structure)
        add_child_membrane!(reservoir, 1, 3, 3)
    end
    
    return reservoir
end

"""
    add_child_membrane!(reservoir::MembraneReservoir, 
                       parent_id::Int, 
                       child_id::Int, 
                       label::Int)

Add a child membrane to a parent.
"""
function add_child_membrane!(reservoir::MembraneReservoir, 
                            parent_id::Int, 
                            child_id::Int, 
                            label::Int)
    parent = reservoir.membranes[parent_id]
    child = MembraneState(child_id, label, parent_id)
    
    push!(parent.children, child_id)
    reservoir.membranes[child_id] = child
    
    return nothing
end

"""
    add_evolution_rule!(reservoir::MembraneReservoir, rule::EvolutionRule)

Add an evolution rule to the reservoir.
"""
function add_evolution_rule!(reservoir::MembraneReservoir, rule::EvolutionRule)
    push!(reservoir.rules, rule)
    return nothing
end

"""
    encode_state_as_multiset(state::Vector{Float64}, alphabet::Vector{String})

Encode a reservoir state vector as a multiset.

Maps continuous values to discrete object counts.

# Arguments
- `state::Vector{Float64}`: Reservoir state vector
- `alphabet::Vector{String}`: Object alphabet

# Returns
- `Multiset`: Encoded multiset
"""
function encode_state_as_multiset(state::Vector{Float64}, alphabet::Vector{String})
    objects = Dict{String, Int}()
    
    # Quantize state values to object counts
    for (i, val) in enumerate(state)
        if i <= length(alphabet)
            # Map value to count: positive values -> object count
            count = max(0, round(Int, abs(val) * 10))
            if count > 0
                objects[alphabet[i]] = count
            end
        end
    end
    
    return Multiset(objects)
end

"""
    decode_multiset_to_state(multiset::Multiset, 
                            alphabet::Vector{String}, 
                            dim::Int)

Decode a multiset back to a reservoir state vector.

# Arguments
- `multiset::Multiset`: Multiset to decode
- `alphabet::Vector{String}`: Object alphabet
- `dim::Int`: Dimension of output state

# Returns
- `Vector{Float64}`: Decoded state vector
"""
function decode_multiset_to_state(multiset::Multiset, 
                                 alphabet::Vector{String}, 
                                 dim::Int)
    state = zeros(dim)
    
    for (i, obj) in enumerate(alphabet)
        if i <= dim && haskey(multiset.objects, obj)
            # Map count back to value
            state[i] = Float64(multiset.objects[obj]) / 10.0
        end
    end
    
    return state
end

"""
    apply_rules!(reservoir::MembraneReservoir, membrane_id::Int)

Apply evolution rules to a membrane with maximal parallelism.

# Arguments
- `reservoir::MembraneReservoir`: The membrane reservoir
- `membrane_id::Int`: Target membrane id
"""
function apply_rules!(reservoir::MembraneReservoir, membrane_id::Int)
    membrane = reservoir.membranes[membrane_id]
    
    # Find applicable rules for this membrane
    applicable = filter(r -> r.membrane_label == membrane.label, reservoir.rules)
    
    # Sort by priority
    sort!(applicable, by=r -> r.priority, rev=true)
    
    # Apply rules with maximal parallelism
    for rule in applicable
        # Check if lhs is contained in multiset
        if can_apply_rule(membrane.multiset, rule.lhs)
            # Apply rule
            apply_rule!(membrane, rule, reservoir)
        end
    end
    
    return nothing
end

"""
    can_apply_rule(multiset::Multiset, lhs::Multiset)

Check if a rule's lhs is contained in the multiset.
"""
function can_apply_rule(multiset::Multiset, lhs::Multiset)
    for (obj, count) in lhs.objects
        if !haskey(multiset.objects, obj) || multiset.objects[obj] < count
            return false
        end
    end
    return true
end

"""
    apply_rule!(membrane::MembraneState, 
               rule::EvolutionRule, 
               reservoir::MembraneReservoir)

Apply a single evolution rule to a membrane.
"""
function apply_rule!(membrane::MembraneState, 
                    rule::EvolutionRule, 
                    reservoir::MembraneReservoir)
    # Remove lhs objects
    for (obj, count) in rule.lhs.objects
        membrane.multiset.objects[obj] -= count
        if membrane.multiset.objects[obj] == 0
            delete!(membrane.multiset.objects, obj)
        end
    end
    
    # Add rhs objects based on target
    if isnothing(rule.target)
        # Stay in same membrane
        for (obj, count) in rule.rhs.objects
            membrane.multiset.objects[obj] = get(membrane.multiset.objects, obj, 0) + count
        end
    elseif rule.target == :out
        # Send to parent
        if !isnothing(membrane.parent)
            parent = reservoir.membranes[membrane.parent]
            for (obj, count) in rule.rhs.objects
                parent.multiset.objects[obj] = get(parent.multiset.objects, obj, 0) + count
            end
        end
    elseif rule.target isa Int
        # Send to specific child
        if rule.target in membrane.children
            child = reservoir.membranes[rule.target]
            for (obj, count) in rule.rhs.objects
                child.multiset.objects[obj] = get(child.multiset.objects, obj, 0) + count
            end
        end
    end
    
    # Handle dissolution
    if rule.dissolve
        dissolve_membrane!(reservoir, membrane.id)
    end
    
    return nothing
end

"""
    evolve_membrane!(reservoir::MembraneReservoir, steps::Int=1)

Evolve the entire membrane system for a number of steps.

# Arguments
- `reservoir::MembraneReservoir`: The membrane reservoir
- `steps::Int=1`: Number of evolution steps
"""
function evolve_membrane!(reservoir::MembraneReservoir, steps::Int=1)
    for _ in 1:steps
        # Apply rules to all membranes
        membrane_ids = collect(keys(reservoir.membranes))
        
        for id in membrane_ids
            if haskey(reservoir.membranes, id)  # Check if not dissolved
                apply_rules!(reservoir, id)
            end
        end
        
        reservoir.step_count += 1
    end
    
    return nothing
end

"""
    membrane_communication!(reservoir::MembraneReservoir, 
                          source_id::Int, 
                          target_id::Int, 
                          objects::Multiset)

Explicitly communicate objects between membranes.

# Arguments
- `reservoir::MembraneReservoir`: The membrane reservoir
- `source_id::Int`: Source membrane id
- `target_id::Int`: Target membrane id
- `objects::Multiset`: Objects to transfer
"""
function membrane_communication!(reservoir::MembraneReservoir, 
                                source_id::Int, 
                                target_id::Int, 
                                objects::Multiset)
    source = reservoir.membranes[source_id]
    target = reservoir.membranes[target_id]
    
    # Remove from source
    for (obj, count) in objects.objects
        if haskey(source.multiset.objects, obj)
            source.multiset.objects[obj] -= count
            if source.multiset.objects[obj] <= 0
                delete!(source.multiset.objects, obj)
            end
        end
    end
    
    # Add to target
    for (obj, count) in objects.objects
        target.multiset.objects[obj] = get(target.multiset.objects, obj, 0) + count
    end
    
    return nothing
end

"""
    dissolve_membrane!(reservoir::MembraneReservoir, membrane_id::Int)

Dissolve a membrane, sending its contents to parent.

# Arguments
- `reservoir::MembraneReservoir`: The membrane reservoir
- `membrane_id::Int`: Membrane to dissolve
"""
function dissolve_membrane!(reservoir::MembraneReservoir, membrane_id::Int)
    if membrane_id == reservoir.root_id
        # Cannot dissolve root
        return nothing
    end
    
    membrane = reservoir.membranes[membrane_id]
    
    # Send contents to parent
    if !isnothing(membrane.parent)
        parent = reservoir.membranes[membrane.parent]
        
        # Transfer multiset
        for (obj, count) in membrane.multiset.objects
            parent.multiset.objects[obj] = get(parent.multiset.objects, obj, 0) + count
        end
        
        # Remove from parent's children
        filter!(id -> id != membrane_id, parent.children)
    end
    
    # Remove membrane
    delete!(reservoir.membranes, membrane_id)
    
    return nothing
end

"""
    get_total_objects(reservoir::MembraneReservoir)

Count total objects across all membranes.
"""
function get_total_objects(reservoir::MembraneReservoir)
    total = 0
    for membrane in values(reservoir.membranes)
        total += sum(values(membrane.multiset.objects))
    end
    return total
end

"""
    print_membrane_structure(reservoir::MembraneReservoir, membrane_id::Int=reservoir.root_id, indent::Int=0)

Print the hierarchical membrane structure.
"""
function print_membrane_structure(reservoir::MembraneReservoir, 
                                 membrane_id::Int=reservoir.root_id, 
                                 indent::Int=0)
    membrane = reservoir.membranes[membrane_id]
    prefix = "  " ^ indent
    
    println("$(prefix)Membrane $(membrane_id) (label $(membrane.label)):")
    println("$(prefix)  Objects: $(membrane.multiset.objects)")
    
    for child_id in membrane.children
        print_membrane_structure(reservoir, child_id, indent + 1)
    end
end

end # module PSystemReservoir
