"""
P-Lingua Abstract Syntax Tree (AST) nodes.
"""

abstract type ASTNode end

"""
    PLinguaModel

Represents a complete P-Lingua model.
"""
struct PLinguaModel <: ASTNode
    model_type::String  # e.g., "transition", "probabilistic"
    definitions::Vector{ASTNode}
end

"""
    MembraneStructure

Represents membrane structure definition (@mu).
"""
struct MembraneStructure <: ASTNode
    structure::Vector{Any}  # Nested structure representation
end

"""
    MultisetDefinition

Represents multiset definition (@ms).
"""
struct MultisetDefinition <: ASTNode
    membrane_id::Int
    objects::Dict{String,Int}
end

"""
    RuleDefinition

Represents an evolution rule.
"""
struct RuleDefinition <: ASTNode
    membrane_label::Int
    lhs::Dict{String,Int}
    rhs::Dict{String,Int}
    target::Target
    dissolve::Bool
    priority::Int
end

"""
    PriorityRelation

Represents a priority relation between rules.
"""
struct PriorityRelation <: ASTNode
    higher_priority::RuleDefinition
    lower_priority::RuleDefinition
end
