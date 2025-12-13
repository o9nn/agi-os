"""
    Membrane

Represents a membrane in a P-System.

# Fields
- `id::Int`: Unique identifier for the membrane
- `label::Int`: Label/type of the membrane
- `parent::Union{Int, Nothing}`: ID of parent membrane (nothing for skin membrane)
- `children::Vector{Int}`: IDs of child membranes

# Examples

```julia
# Skin membrane (no parent)
skin = Membrane(1, 1, nothing)

# Nested membrane
inner = Membrane(2, 2, 1)  # Inside membrane 1
```
"""
mutable struct Membrane
    id::Int
    label::Int
    parent::Union{Int,Nothing}
    children::Vector{Int}
    
    function Membrane(id::Int, label::Int, parent::Union{Int,Nothing}=nothing)
        new(id, label, parent, Int[])
    end
end

"""
    add_child!(membrane::Membrane, child_id::Int)

Add a child membrane to this membrane.
"""
function add_child!(membrane::Membrane, child_id::Int)
    if !(child_id in membrane.children)
        push!(membrane.children, child_id)
    end
end

"""
    remove_child!(membrane::Membrane, child_id::Int)

Remove a child membrane from this membrane.
"""
function remove_child!(membrane::Membrane, child_id::Int)
    filter!(id -> id != child_id, membrane.children)
end

"""
    is_skin(membrane::Membrane)

Check if this is a skin membrane (no parent).
"""
is_skin(membrane::Membrane) = membrane.parent === nothing

"""
    is_elementary(membrane::Membrane)

Check if this is an elementary membrane (no children).
"""
is_elementary(membrane::Membrane) = isempty(membrane.children)

"""
    show(io::IO, m::Membrane)

Display a membrane.
"""
function Base.show(io::IO, m::Membrane)
    parent_str = m.parent === nothing ? "none" : string(m.parent)
    children_str = isempty(m.children) ? "none" : join(m.children, ", ")
    print(io, "Membrane(id=$(m.id), label=$(m.label), parent=$parent_str, children=[$children_str])")
end
