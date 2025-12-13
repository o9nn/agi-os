"""
    Target

Represents the target location for objects produced by a rule.

- `:here`: Objects stay in the same membrane
- `:out`: Objects move to parent membrane
- `(:in, membrane_id)`: Objects move into specified child membrane
"""
const Target = Union{Symbol,Tuple{Symbol,Int}}

"""
    Rule

Represents an evolution rule in a P-System.

# Fields
- `membrane_label::Int`: Label of the membrane where the rule applies
- `lhs::Multiset`: Left-hand side (consumed objects)
- `rhs::Multiset`: Right-hand side (produced objects)
- `target::Target`: Where produced objects go (:here, :out, or (:in, id))
- `dissolve::Bool`: Whether the membrane dissolves after applying this rule
- `priority::Int`: Priority level (higher = more priority)

# Examples

```julia
# Simple rewriting: a -> b (in membrane 1)
r1 = Rule(1, Multiset("a" => 1), Multiset("b" => 1))

# Send to parent: a -> (b, out)
r2 = Rule(1, Multiset("a" => 1), Multiset("b" => 1), :out)

# Send into child: a -> (b, in_2)
r3 = Rule(1, Multiset("a" => 1), Multiset("b" => 1), (:in, 2))

# Membrane dissolution: a -> b with membrane dissolving
r4 = Rule(1, Multiset("a" => 1), Multiset("b" => 1), :here, true)

# With priority
r5 = Rule(1, Multiset("a" => 1), Multiset("b" => 1), :here, false, 2)
```
"""
struct Rule
    membrane_label::Int
    lhs::Multiset
    rhs::Multiset
    target::Target
    dissolve::Bool
    priority::Int
    
    function Rule(
        membrane_label::Int,
        lhs::Multiset,
        rhs::Multiset,
        target::Target = :here,
        dissolve::Bool = false,
        priority::Int = 0
    )
        new(membrane_label, lhs, rhs, target, dissolve, priority)
    end
end

"""
    is_applicable(rule::Rule, multiset::Multiset)

Check if a rule can be applied to a given multiset.
"""
function is_applicable(rule::Rule, multiset::Multiset)
    return rule.lhs ⊆ multiset
end

"""
    apply_rule(rule::Rule, multiset::Multiset)

Apply a rule to a multiset, returning the resulting multiset and target.
Returns `nothing` if the rule cannot be applied.
"""
function apply_rule(rule::Rule, multiset::Multiset)
    # Check if rule is applicable
    result = multiset - rule.lhs
    if result === nothing
        return nothing
    end
    
    # Add right-hand side
    return result + rule.rhs
end

"""
    show(io::IO, r::Rule)

Display a rule in a readable format.
"""
function Base.show(io::IO, r::Rule)
    print(io, "[$(r.lhs)]'$(r.membrane_label) → ")
    
    if r.target == :here
        print(io, "[$(r.rhs)]'$(r.membrane_label)")
    elseif r.target == :out
        print(io, "($(r.rhs), out)")
    elseif r.target isa Tuple && r.target[1] == :in
        print(io, "($(r.rhs), in_$(r.target[2]))")
    end
    
    if r.dissolve
        print(io, " [dissolve]")
    end
    
    if r.priority > 0
        print(io, " (priority: $(r.priority))")
    end
end

"""
    ==(r1::Rule, r2::Rule)

Check if two rules are equal.
"""
function Base.:(==)(r1::Rule, r2::Rule)
    return (r1.membrane_label == r2.membrane_label &&
            r1.lhs == r2.lhs &&
            r1.rhs == r2.rhs &&
            r1.target == r2.target &&
            r1.dissolve == r2.dissolve &&
            r1.priority == r2.priority)
end

"""
    hash(r::Rule, h::UInt)

Compute hash of a rule.
"""
function Base.hash(r::Rule, h::UInt)
    hash((r.membrane_label, r.lhs, r.rhs, r.target, r.dissolve, r.priority), h)
end
