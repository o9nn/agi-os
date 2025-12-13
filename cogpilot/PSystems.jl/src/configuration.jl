"""
    Configuration

Represents the current state of a P-System during computation.

# Fields
- `multisets::Dict{Int, Multiset}`: Current multiset in each membrane
- `active_membranes::Set{Int}`: IDs of currently active (non-dissolved) membranes
- `step::Int`: Current computation step

# Examples

```julia
config = Configuration(system)
```
"""
mutable struct Configuration
    multisets::Dict{Int,Multiset}
    active_membranes::Set{Int}
    step::Int
    
    function Configuration(system::PSystem)
        multisets = Dict{Int,Multiset}()
        
        # Initialize multisets from system
        for membrane in system.membranes
            if haskey(system.initial_multisets, membrane.id)
                multisets[membrane.id] = copy(system.initial_multisets[membrane.id])
            else
                multisets[membrane.id] = Multiset()
            end
        end
        
        # All membranes start active
        active_membranes = Set(m.id for m in system.membranes)
        
        new(multisets, active_membranes, 0)
    end
    
    function Configuration(multisets::Dict{Int,Multiset}, active_membranes::Set{Int}, step::Int)
        new(multisets, active_membranes, step)
    end
end

"""
    copy(config::Configuration)

Create a deep copy of a configuration.
"""
function Base.copy(config::Configuration)
    multisets_copy = Dict(id => copy(ms) for (id, ms) in config.multisets)
    active_copy = copy(config.active_membranes)
    return Configuration(multisets_copy, active_copy, config.step)
end

"""
    is_active(config::Configuration, membrane_id::Int)

Check if a membrane is currently active (not dissolved).
"""
function is_active(config::Configuration, membrane_id::Int)
    return membrane_id in config.active_membranes
end

"""
    dissolve!(config::Configuration, membrane_id::Int)

Mark a membrane as dissolved.
"""
function dissolve!(config::Configuration, membrane_id::Int)
    delete!(config.active_membranes, membrane_id)
end

"""
    get_multiset(config::Configuration, membrane_id::Int)

Get the multiset for a given membrane.
"""
function get_multiset(config::Configuration, membrane_id::Int)
    return get(config.multisets, membrane_id, Multiset())
end

"""
    set_multiset!(config::Configuration, membrane_id::Int, multiset::Multiset)

Set the multiset for a given membrane.
"""
function set_multiset!(config::Configuration, membrane_id::Int, multiset::Multiset)
    config.multisets[membrane_id] = multiset
end

"""
    is_halted(config::Configuration, system::PSystem)

Check if the computation has halted (no applicable rules).
"""
function is_halted(config::Configuration, system::PSystem)
    # Check each active membrane
    for membrane in system.membranes
        if !is_active(config, membrane.id)
            continue
        end
        
        multiset = get_multiset(config, membrane.id)
        rules = get_rules_for_membrane(system, membrane.label)
        
        # If any rule is applicable, not halted
        for rule in rules
            if is_applicable(rule, multiset)
                return false
            end
        end
    end
    
    return true
end

"""
    show(io::IO, config::Configuration)

Display a configuration.
"""
function Base.show(io::IO, config::Configuration)
    println(io, "Configuration (Step $(config.step)):")
    
    # Sort by membrane ID for consistent display
    for membrane_id in sort(collect(keys(config.multisets)))
        if is_active(config, membrane_id)
            multiset = config.multisets[membrane_id]
            status = "active"
        else
            multiset = config.multisets[membrane_id]
            status = "dissolved"
        end
        
        println(io, "  Membrane $membrane_id ($status): $multiset")
    end
end
