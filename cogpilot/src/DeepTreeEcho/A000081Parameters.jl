"""
    A000081Parameters

Module for deriving all system parameters from the OEIS A000081 sequence.

OEIS A000081: Number of unlabeled rooted trees with n nodes
    {1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, ...}

This module ensures that NO parameter is arbitrarily chosen. Instead, all
parameters are derived from the topology and structure of the A000081 sequence,
ensuring mathematical consistency throughout the system.

# Core Principles

1. **Reservoir Size**: Must be derived from cumulative tree counts at a specific order
2. **Max Tree Order**: Determines the complexity horizon and must be chosen deliberately
3. **Number of Membranes**: Must correspond to a tree count at a specific order
4. **Growth Rate**: Derived from ratios between consecutive A000081 terms
5. **Mutation Rate**: Derived from diversity within tree populations

# 1-1 Correspondence Principle (Critical)

For a given order n, the system maintains **1-1 relationships** between:
- **B-series roots** (rooted trees) = A000081[n]
- **P-system membranes** (at order n) = A000081[n]
- **J-surface differentials** (elementary differentials) = A000081[n]

These three expressions of A000081 (Matula numbers) must be equal for consistency.

# Parameter Derivation Rules

For a given **base order** n (typically 3-7):

| Order | Reservoir Size | Roots/Mems/Diffs |
|-------|----------------|------------------|
| 1     | 1              | 1                |
| 2     | 2              | 1                |
| 3     | 4              | 2                |
| 4     | 8              | 4                |
| 5     | 17             | 9                |
| 6     | 37             | 20               |
| 7     | 85             | 48               |
| 8     | 200            | 115              |
| 9     | 486            | 286              |
| 10    | 1205           | 719              |

Where:
- Column 2: `reservoir_size = sum(A000081[1:n])` (cumulative trees up to order n)
- Column 3: `num_trees = A000081[n]` (number of roots/membranes/differentials at order n)

Derived parameters:
- `num_membranes = A000081[k]` where k is chosen based on structural needs
- `growth_rate = A000081[n+1] / A000081[n]` (growth ratio)
- `mutation_rate = 1.0 / A000081[n]` (inversely proportional to complexity)

All parameters MUST be justified by their relationship to the A000081 topology.

Note: Related sequences A000108 (Catalan), A000055 (unrooted trees), and 
A000041 (partitions) may also have correspondence in advanced configurations.
"""
module A000081Parameters

export A000081_SEQUENCE
export derive_reservoir_size, derive_num_membranes, derive_growth_rate, derive_mutation_rate
export get_parameter_set, validate_parameters, explain_parameters
export A000081ParameterSet
export validate_component_correspondence

# The fundamental OEIS A000081 sequence
const A000081_SEQUENCE = [
    1, 1, 2, 4, 9, 20, 48, 115, 286, 719,
    1842, 4766, 12486, 32973, 87811, 235381,
    634847, 1721159, 4688676, 12826228
]

"""
    A000081ParameterSet

A complete, validated set of parameters derived from A000081.

# Fields
- `base_order::Int`: The base order used for derivation (typically 3-7)
- `max_tree_order::Int`: Maximum tree order (typically base_order + 2 to base_order + 5)
- `reservoir_size::Int`: Derived from cumulative A000081 counts
- `num_membranes::Int`: Derived from A000081[membrane_order]
- `growth_rate::Float64`: Derived from A000081 growth ratio
- `mutation_rate::Float64`: Derived inversely from complexity
- `membrane_order::Int`: The order whose count determines num_membranes
- `derivation_explanation::Dict{String, String}`: Explanation of each parameter
"""
struct A000081ParameterSet
    base_order::Int
    max_tree_order::Int
    reservoir_size::Int
    num_membranes::Int
    growth_rate::Float64
    mutation_rate::Float64
    membrane_order::Int
    derivation_explanation::Dict{String, String}
    
    function A000081ParameterSet(base_order::Int, membrane_order::Int, max_order_offset::Int=3)
        # Validate inputs
        if base_order < 1 || base_order > length(A000081_SEQUENCE) - 1
            error("base_order must be between 1 and $(length(A000081_SEQUENCE) - 1)")
        end
        if membrane_order < 1 || membrane_order > length(A000081_SEQUENCE)
            error("membrane_order must be between 1 and $(length(A000081_SEQUENCE))")
        end
        
        # Derive parameters
        max_tree_order = base_order + max_order_offset
        reservoir_size = derive_reservoir_size(base_order)
        num_membranes = derive_num_membranes(membrane_order)
        growth_rate = derive_growth_rate(base_order)
        mutation_rate = derive_mutation_rate(base_order)
        
        # Create explanations
        explanations = Dict{String, String}(
            "base_order" => "Foundation order for parameter derivation: $base_order",
            "max_tree_order" => "Base order ($base_order) + offset ($max_order_offset) = $max_tree_order",
            "reservoir_size" => "Cumulative A000081[1:$base_order] = $reservoir_size nodes",
            "num_membranes" => "A000081[$membrane_order] = $num_membranes membranes",
            "growth_rate" => "A000081[$(base_order+1)] / A000081[$base_order] ≈ $(round(growth_rate, digits=4))",
            "mutation_rate" => "1 / A000081[$base_order] ≈ $(round(mutation_rate, digits=4))",
            "membrane_order" => "Order $membrane_order selected for membrane count derivation"
        )
        
        new(base_order, max_tree_order, reservoir_size, num_membranes,
            growth_rate, mutation_rate, membrane_order, explanations)
    end
end

"""
    derive_reservoir_size(base_order::Int)

Derive reservoir size from cumulative tree counts up to base_order.

The reservoir size represents the total structural capacity, which should
correspond to the total number of distinct trees available up to a given order.

# Arguments
- `base_order::Int`: Base order for calculation (typically 3-7)

# Returns
- `Int`: Cumulative sum of A000081[1:base_order]

# Example
```julia
# Order 5: 1 + 1 + 2 + 4 + 9 = 17 trees total
reservoir_size = derive_reservoir_size(5)  # Returns 17
```
"""
function derive_reservoir_size(base_order::Int)
    if base_order < 1 || base_order > length(A000081_SEQUENCE)
        error("base_order must be between 1 and $(length(A000081_SEQUENCE))")
    end
    
    return sum(A000081_SEQUENCE[1:base_order])
end

"""
    derive_num_membranes(membrane_order::Int)

Derive number of membranes from A000081 at specific order.

The number of membranes represents hierarchical compartments, which should
align with the tree count at a specific structural order.

# Arguments
- `membrane_order::Int`: Order whose tree count determines membrane count (typically 2-4)

# Returns
- `Int`: A000081[membrane_order]

# Example
```julia
# Order 3 has 2 trees, so 2 membranes
num_membranes = derive_num_membranes(3)  # Returns 2

# Order 4 has 4 trees, so 4 membranes
num_membranes = derive_num_membranes(4)  # Returns 4
```
"""
function derive_num_membranes(membrane_order::Int)
    if membrane_order < 1 || membrane_order > length(A000081_SEQUENCE)
        error("membrane_order must be between 1 and $(length(A000081_SEQUENCE))")
    end
    
    return A000081_SEQUENCE[membrane_order]
end

"""
    derive_growth_rate(base_order::Int)

Derive growth rate from the ratio of consecutive A000081 terms.

The growth rate reflects how quickly tree complexity increases, which
should guide the evolutionary growth dynamics of the system.

# Arguments
- `base_order::Int`: Base order for ratio calculation

# Returns
- `Float64`: A000081[base_order + 1] / A000081[base_order]

# Example
```julia
# Growth from order 5 to 6: 20/9 ≈ 2.222
growth_rate = derive_growth_rate(5)
```
"""
function derive_growth_rate(base_order::Int)
    if base_order < 1 || base_order > length(A000081_SEQUENCE) - 1
        error("base_order must be between 1 and $(length(A000081_SEQUENCE) - 1)")
    end
    
    numerator = Float64(A000081_SEQUENCE[base_order + 1])
    denominator = Float64(A000081_SEQUENCE[base_order])
    
    return numerator / denominator
end

"""
    derive_mutation_rate(base_order::Int)

Derive mutation rate inversely from tree count at base order.

Higher complexity (more trees) should lead to lower mutation rates to
maintain stability. The mutation rate is inversely proportional to
the number of available structural configurations.

# Arguments
- `base_order::Int`: Base order for calculation

# Returns
- `Float64`: 1.0 / A000081[base_order]

# Example
```julia
# Order 5 has 9 trees, so mutation rate ≈ 0.111
mutation_rate = derive_mutation_rate(5)
```
"""
function derive_mutation_rate(base_order::Int)
    if base_order < 1 || base_order > length(A000081_SEQUENCE)
        error("base_order must be between 1 and $(length(A000081_SEQUENCE))")
    end
    
    return 1.0 / Float64(A000081_SEQUENCE[base_order])
end

"""
    get_parameter_set(base_order::Int; membrane_order::Int=3, max_order_offset::Int=3)

Get a complete, validated parameter set derived from A000081.

This is the PRIMARY function to use when creating a DeepTreeEcho system.
It ensures all parameters are mathematically consistent with the A000081 topology.

# Arguments
- `base_order::Int`: Base order for parameter derivation (recommended: 4-6)
- `membrane_order::Int`: Order for membrane count (default: 3 → 2 membranes)
- `max_order_offset::Int`: Offset from base_order for max_tree_order (default: 3)

# Returns
- `A000081ParameterSet`: Complete parameter set with derivation explanations

# Recommended Configurations

**Small System (base_order=4)**
- reservoir_size = 8 (1+1+2+4)
- num_membranes = 2 (A000081[3])
- max_tree_order = 7

**Medium System (base_order=5)**
- reservoir_size = 17 (1+1+2+4+9)
- num_membranes = 2 or 4 (A000081[3] or A000081[4])
- max_tree_order = 8

**Large System (base_order=6)**
- reservoir_size = 37 (1+1+2+4+9+20)
- num_membranes = 4 (A000081[4])
- max_tree_order = 9

# Example
```julia
# Create medium system with mathematically justified parameters
params = get_parameter_set(5, membrane_order=4)
system = DeepTreeEchoSystem(
    reservoir_size = params.reservoir_size,
    max_tree_order = params.max_tree_order,
    num_membranes = params.num_membranes,
    growth_rate = params.growth_rate,
    mutation_rate = params.mutation_rate
)
```
"""
function get_parameter_set(base_order::Int; membrane_order::Int=3, max_order_offset::Int=3)
    return A000081ParameterSet(base_order, membrane_order, max_order_offset)
end

"""
    validate_parameters(reservoir_size::Int, max_tree_order::Int, num_membranes::Int, 
                       growth_rate::Float64, mutation_rate::Float64)

Validate that parameters align with A000081 topology.

Checks if the provided parameters could be derived from some valid A000081-based
configuration. Returns validation result and suggestions.

# Returns
- `Tuple{Bool, String}`: (is_valid, message)
"""
function validate_parameters(reservoir_size::Int, max_tree_order::Int, num_membranes::Int, 
                            growth_rate::Float64, mutation_rate::Float64)
    warnings = String[]
    
    # Check if reservoir_size is a cumulative A000081 sum
    valid_reservoir_sizes = [sum(A000081_SEQUENCE[1:i]) for i in 1:min(10, length(A000081_SEQUENCE))]
    if !(reservoir_size in valid_reservoir_sizes)
        push!(warnings, "reservoir_size=$reservoir_size is not a cumulative A000081 sum. Valid: $valid_reservoir_sizes")
    end
    
    # Check if num_membranes appears in A000081
    if !(num_membranes in A000081_SEQUENCE[1:min(10, length(A000081_SEQUENCE))])
        valid_membrane_counts = A000081_SEQUENCE[1:min(6, length(A000081_SEQUENCE))]
        push!(warnings, "num_membranes=$num_membranes not in A000081[1:6]. Valid: $valid_membrane_counts")
    end
    
    # Check if growth_rate matches an A000081 ratio
    valid_ratios = [A000081_SEQUENCE[i+1]/A000081_SEQUENCE[i] for i in 1:min(8, length(A000081_SEQUENCE)-1)]
    closest_ratio = valid_ratios[argmin(abs.(valid_ratios .- growth_rate))]
    if abs(growth_rate - closest_ratio) > 0.1
        push!(warnings, "growth_rate=$growth_rate not close to A000081 ratios. Closest: $(round(closest_ratio, digits=3))")
    end
    
    # Check if mutation_rate is inverse of A000081
    valid_mutation_rates = [1.0/A000081_SEQUENCE[i] for i in 1:min(10, length(A000081_SEQUENCE))]
    closest_mutation = valid_mutation_rates[argmin(abs.(valid_mutation_rates .- mutation_rate))]
    if abs(mutation_rate - closest_mutation) > 0.01
        push!(warnings, "mutation_rate=$mutation_rate not inverse of A000081. Closest: $(round(closest_mutation, digits=4))")
    end
    
    is_valid = isempty(warnings)
    message = is_valid ? "All parameters align with A000081 topology ✓" : join(warnings, "\n")
    
    return (is_valid, message)
end

"""
    explain_parameters(params::A000081ParameterSet)

Print a detailed explanation of how each parameter was derived from A000081.
"""
function explain_parameters(params::A000081ParameterSet)
    println("\n" * "="^70)
    println("A000081 PARAMETER DERIVATION")
    println("="^70)
    println("\nOEIS A000081 Sequence: Number of unlabeled rooted trees with n nodes")
    println("  {1, 1, 2, 4, 9, 20, 48, 115, 286, 719, ...}")
    println()
    
    for (param, explanation) in sort(collect(params.derivation_explanation))
        println("• $param:")
        println("  $explanation")
    end
    
    println("\n" * "="^70)
    println("PARAMETER VALUES")
    println("="^70)
    println("  reservoir_size  = $(params.reservoir_size)")
    println("  max_tree_order  = $(params.max_tree_order)")
    println("  num_membranes   = $(params.num_membranes)")
    println("  growth_rate     = $(round(params.growth_rate, digits=4))")
    println("  mutation_rate   = $(round(params.mutation_rate, digits=4))")
    println("="^70)
    println()
end

"""
    explain_parameters(reservoir_size::Int, max_tree_order::Int, num_membranes::Int,
                      growth_rate::Float64, mutation_rate::Float64)

Validate and explain arbitrary parameters, suggesting A000081-aligned alternatives.
"""
function explain_parameters(reservoir_size::Int, max_tree_order::Int, num_membranes::Int,
                          growth_rate::Float64, mutation_rate::Float64)
    println("\n" * "="^70)
    println("PARAMETER VALIDATION AGAINST A000081")
    println("="^70)
    println("\nCurrent Parameters:")
    println("  reservoir_size  = $reservoir_size")
    println("  max_tree_order  = $max_tree_order")
    println("  num_membranes   = $num_membranes")
    println("  growth_rate     = $(round(growth_rate, digits=4))")
    println("  mutation_rate   = $(round(mutation_rate, digits=4))")
    println()
    
    is_valid, message = validate_parameters(reservoir_size, max_tree_order, num_membranes,
                                           growth_rate, mutation_rate)
    
    if is_valid
        println("✓ All parameters align with A000081 topology!")
    else
        println("⚠ Parameters do NOT fully align with A000081:")
        println()
        for line in split(message, "\n")
            println("  $line")
        end
        println()
        println("Recommended: Use get_parameter_set() for A000081-derived parameters")
        println()
        println("Example configurations:")
        for base_order in 4:6
            params = get_parameter_set(base_order)
            println("  base_order=$base_order → reservoir=$(params.reservoir_size), membranes=$(params.num_membranes)")
        end
    end
    
    println("="^70)
    println()
end

"""
    validate_component_correspondence(order::Int, 
                                     num_bseries_roots::Int, 
                                     num_psystem_membranes::Int, 
                                     num_jsurface_diffs::Int)

Validate the 1-1 correspondence principle for a given order.

For order n, the following must all equal A000081[n]:
- Number of B-series roots (rooted trees)
- Number of P-system membranes (at order n)
- Number of J-surface differentials (elementary differentials)

This ensures the three expressions of A000081 (Matula numbers) are consistent.

# Arguments
- `order::Int`: The order being validated
- `num_bseries_roots::Int`: Number of B-series rooted trees at this order
- `num_psystem_membranes::Int`: Number of P-system membranes at this order
- `num_jsurface_diffs::Int`: Number of J-surface elementary differentials at this order

# Returns
- `Tuple{Bool, String}`: (is_valid, message)

# Example
```julia
# For order 5, all three should equal 9
is_valid, msg = validate_component_correspondence(5, 9, 9, 9)
# Returns: (true, "✓ 1-1 correspondence validated...")

# Invalid case
is_valid, msg = validate_component_correspondence(5, 9, 8, 9)
# Returns: (false, "✗ P-system membranes (8) ≠ A000081[5] (9)")
```
"""
function validate_component_correspondence(order::Int, 
                                          num_bseries_roots::Int, 
                                          num_psystem_membranes::Int, 
                                          num_jsurface_diffs::Int)
    if order < 1 || order > length(A000081_SEQUENCE)
        return (false, "Order $order out of range [1, $(length(A000081_SEQUENCE))]")
    end
    
    expected = A000081_SEQUENCE[order]
    errors = String[]
    
    if num_bseries_roots != expected
        push!(errors, "B-series roots ($num_bseries_roots) ≠ A000081[$order] ($expected)")
    end
    
    if num_psystem_membranes != expected
        push!(errors, "P-system membranes ($num_psystem_membranes) ≠ A000081[$order] ($expected)")
    end
    
    if num_jsurface_diffs != expected
        push!(errors, "J-surface differentials ($num_jsurface_diffs) ≠ A000081[$order] ($expected)")
    end
    
    if isempty(errors)
        msg = "✓ 1-1 correspondence validated for order $order: " *
              "roots=$num_bseries_roots, membranes=$num_psystem_membranes, " *
              "diffs=$num_jsurface_diffs = A000081[$order]"
        return (true, msg)
    else
        msg = "✗ 1-1 correspondence FAILED for order $order:\n  " * join(errors, "\n  ")
        return (false, msg)
    end
end

end # module A000081Parameters
