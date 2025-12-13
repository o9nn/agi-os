"""
    Multiset

A multiset (bag) is a collection of objects where each object can appear multiple times.
Objects are represented as strings, and their multiplicity as integers.

# Fields
- `objects::Dict{String, Int}`: Mapping from object names to their multiplicities

# Examples

```julia
# Create multisets
m1 = Multiset(Dict("a" => 2, "b" => 1))
m2 = Multiset("a" => 2, "b" => 1)
m3 = multiset"a{2}, b"  # Using string macro

# Operations
m1 + m2  # Union
m1 - m2  # Difference
m1 ⊆ m2  # Inclusion
```
"""
struct Multiset
    objects::Dict{String, Int}
    
    function Multiset(objects::Dict{String, Int})
        # Remove objects with zero or negative multiplicity
        filtered = Dict(k => v for (k, v) in objects if v > 0)
        new(filtered)
    end
end

# Constructors
Multiset() = Multiset(Dict{String, Int}())
Multiset(pairs::Pair{String, Int}...) = Multiset(Dict(pairs...))

"""
    +(m1::Multiset, m2::Multiset)

Union of two multisets (multiplicities are added).
"""
function Base.:+(m1::Multiset, m2::Multiset)
    result = copy(m1.objects)
    for (obj, count) in m2.objects
        result[obj] = get(result, obj, 0) + count
    end
    Multiset(result)
end

"""
    -(m1::Multiset, m2::Multiset)

Difference of two multisets (multiplicities are subtracted).
Returns nothing if m2 is not a subset of m1.
"""
function Base.:-(m1::Multiset, m2::Multiset)
    result = copy(m1.objects)
    for (obj, count) in m2.objects
        current = get(result, obj, 0)
        if current < count
            return nothing  # Cannot subtract more than we have
        end
        new_count = current - count
        if new_count > 0
            result[obj] = new_count
        else
            delete!(result, obj)
        end
    end
    Multiset(result)
end

"""
    *(n::Int, m::Multiset)
    *(m::Multiset, n::Int)

Scalar multiplication of a multiset.
"""
function Base.:*(n::Int, m::Multiset)
    if n <= 0
        return Multiset()
    end
    Multiset(Dict(obj => count * n for (obj, count) in m.objects))
end

Base.:*(m::Multiset, n::Int) = n * m

"""
    issubset(m1::Multiset, m2::Multiset)

Check if m1 is a subset of m2 (all objects in m1 appear in m2 with at least the same multiplicity).
"""
function Base.issubset(m1::Multiset, m2::Multiset)
    for (obj, count) in m1.objects
        if get(m2.objects, obj, 0) < count
            return false
        end
    end
    return true
end

# Alias for subset operator
const ⊆ = issubset

"""
    isempty(m::Multiset)

Check if the multiset is empty.
"""
Base.isempty(m::Multiset) = isempty(m.objects)

"""
    length(m::Multiset)

Total number of objects in the multiset (counting multiplicities).
"""
Base.length(m::Multiset) = sum(values(m.objects))

"""
    get(m::Multiset, obj::String, default=0)

Get the multiplicity of an object in the multiset.
"""
Base.get(m::Multiset, obj::String, default=0) = get(m.objects, obj, default)

"""
    copy(m::Multiset)

Create a copy of the multiset.
"""
Base.copy(m::Multiset) = Multiset(copy(m.objects))

"""
    ==(m1::Multiset, m2::Multiset)

Check if two multisets are equal.
"""
function Base.:(==)(m1::Multiset, m2::Multiset)
    return m1.objects == m2.objects
end

"""
    hash(m::Multiset, h::UInt)

Compute hash of multiset for use in dictionaries.
"""
Base.hash(m::Multiset, h::UInt) = hash(m.objects, h)

"""
    show(io::IO, m::Multiset)

Display a multiset in a readable format.
"""
function Base.show(io::IO, m::Multiset)
    if isempty(m)
        print(io, "∅")
        return
    end
    
    items = String[]
    for (obj, count) in sort(collect(m.objects))
        if count == 1
            push!(items, obj)
        else
            push!(items, "$(obj){$(count)}")
        end
    end
    print(io, join(items, ", "))
end

"""
    @multiset_str(s)

String macro for creating multisets from P-Lingua notation.

# Example
```julia
m = multiset"a{2}, b, c{3}"
```
"""
macro multiset_str(s)
    # Simple parser for multiset notation
    objects = Dict{String, Int}()
    
    for part in split(s, ',')
        part = strip(part)
        if isempty(part)
            continue
        end
        
        # Check for multiplicity notation: obj{n}
        m = match(r"^(\w+)\{(\d+)\}$", part)
        if m !== nothing
            obj, count = m.captures
            objects[obj] = parse(Int, count)
        else
            # Single object
            objects[part] = 1
        end
    end
    
    return :(Multiset($objects))
end
