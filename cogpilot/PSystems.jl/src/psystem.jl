"""
    PSystem

Complete definition of a P-System.

# Fields
- `membranes::Vector{Membrane}`: All membranes in the system
- `alphabet::Vector{String}`: Set of object symbols
- `initial_multisets::Dict{Int, Multiset}`: Initial configuration (membrane_id => multiset)
- `rules::Vector{Rule}`: Evolution rules
- `membrane_dict::Dict{Int, Membrane}`: Quick lookup for membranes by ID

# Examples

```julia
system = PSystem(
    membranes = [Membrane(1, 1, nothing)],
    alphabet = ["a", "b"],
    initial_multisets = Dict(1 => Multiset("a" => 2)),
    rules = [Rule(1, Multiset("a" => 1), Multiset("b" => 2))]
)
```
"""
struct PSystem
    membranes::Vector{Membrane}
    alphabet::Vector{String}
    initial_multisets::Dict{Int,Multiset}
    rules::Vector{Rule}
    membrane_dict::Dict{Int,Membrane}
    
    function PSystem(;
        membranes::Vector{Membrane},
        alphabet::Vector{String},
        initial_multisets::Dict{Int,Multiset},
        rules::Vector{Rule}
    )
        # Build membrane dictionary for quick lookup
        membrane_dict = Dict(m.id => m for m in membranes)
        
        # Build parent-child relationships
        for membrane in membranes
            if membrane.parent !== nothing
                if haskey(membrane_dict, membrane.parent)
                    add_child!(membrane_dict[membrane.parent], membrane.id)
                end
            end
        end
        
        system = new(membranes, alphabet, initial_multisets, rules, membrane_dict)
        
        # Validate the system
        validate(system)
        
        return system
    end
end

"""
    get_membrane(system::PSystem, id::Int)

Get a membrane by its ID.
"""
function get_membrane(system::PSystem, id::Int)
    return get(system.membrane_dict, id, nothing)
end

"""
    get_rules_for_membrane(system::PSystem, label::Int)

Get all rules applicable to membranes with a given label.
"""
function get_rules_for_membrane(system::PSystem, label::Int)
    return filter(r -> r.membrane_label == label, system.rules)
end

"""
    get_skin_membrane(system::PSystem)

Get the skin membrane (outermost membrane).
"""
function get_skin_membrane(system::PSystem)
    for membrane in system.membranes
        if is_skin(membrane)
            return membrane
        end
    end
    return nothing
end

"""
    get_children(system::PSystem, membrane_id::Int)

Get all child membranes of a given membrane.
"""
function get_children(system::PSystem, membrane_id::Int)
    membrane = get_membrane(system, membrane_id)
    if membrane === nothing
        return Membrane[]
    end
    return [get_membrane(system, child_id) for child_id in membrane.children]
end

"""
    show(io::IO, system::PSystem)

Display a P-System summary.
"""
function Base.show(io::IO, system::PSystem)
    println(io, "PSystem:")
    println(io, "  Membranes: $(length(system.membranes))")
    println(io, "  Alphabet: $(join(system.alphabet, ", "))")
    println(io, "  Rules: $(length(system.rules))")
    println(io, "  Initial configuration:")
    for (membrane_id, multiset) in sort(collect(system.initial_multisets))
        println(io, "    Membrane $membrane_id: $multiset")
    end
end

"""
    validate(system::PSystem)

Validate a P-System for consistency.
Returns `true` if valid, or throws an error with details.
"""
function validate(system::PSystem)
    # Check that all initial multisets refer to existing membranes
    for membrane_id in keys(system.initial_multisets)
        if !haskey(system.membrane_dict, membrane_id)
            throw(ArgumentError("Initial multiset refers to non-existent membrane $membrane_id"))
        end
    end
    
    # Check that all rules refer to valid membrane labels
    valid_labels = Set(m.label for m in system.membranes)
    for rule in system.rules
        if !(rule.membrane_label in valid_labels)
            throw(ArgumentError("Rule refers to non-existent membrane label $(rule.membrane_label)"))
        end
    end
    
    # Check that parent-child relationships are consistent
    for membrane in system.membranes
        if membrane.parent !== nothing
            if !haskey(system.membrane_dict, membrane.parent)
                throw(ArgumentError("Membrane $(membrane.id) has non-existent parent $(membrane.parent)"))
            end
        end
    end
    
    return true
end
