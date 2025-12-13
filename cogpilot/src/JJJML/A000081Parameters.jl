"""
A000081 Parameter Alignment

Derive system parameters from OEIS A000081 sequence (number of rooted trees).
"""

"""
    derive_jjjml_parameters(base_order::Int)

Derive JJJML parameters from A000081 sequence.

# Arguments
- `base_order::Int`: Base order (index into A000081 sequence)

# Returns
Named tuple with derived parameters:
- `reservoir_size`: Sum of A000081[1:base_order]
- `num_reservoirs`: A000081[base_order]
- `learning_rate`: 1.0 / A000081[base_order]
- `decay_rate`: A000081[base_order] / A000081[base_order+1]
- `num_layers`: base_order
- `hidden_dim`: 2^base_order * A000081[base_order]
- `batch_size`: A000081[base_order]
- `num_epochs`: A000081[base_order+1]
"""
function derive_jjjml_parameters(base_order::Int)
    # Validate base order
    if base_order < 1 || base_order >= length(A000081_SEQUENCE)
        error("base_order must be between 1 and $(length(A000081_SEQUENCE)-1)")
    end
    
    # Get A000081 values
    a_n = A000081_SEQUENCE[base_order + 1]  # +1 because sequence starts at n=0
    a_n_plus_1 = A000081_SEQUENCE[base_order + 2]
    
    # Derive parameters
    return (
        # Reservoir parameters
        reservoir_size = sum(A000081_SEQUENCE[2:(base_order+1)]),  # Skip first 0
        num_reservoirs = a_n,
        
        # Learning rates
        learning_rate = 1.0 / a_n,
        decay_rate = a_n / a_n_plus_1,
        
        # Architecture parameters
        num_layers = base_order,
        hidden_dim = 2^base_order * a_n,
        
        # Training parameters
        batch_size = a_n,
        num_epochs = a_n_plus_1,
        
        # Growth rate
        growth_rate = a_n_plus_1 / a_n,
        
        # Mutation rate
        mutation_rate = 1.0 / a_n
    )
end

"""
    get_a000081_value(n::Int)

Get the n-th value from the A000081 sequence.
"""
function get_a000081_value(n::Int)
    if n < 0 || n >= length(A000081_SEQUENCE)
        error("Index $n out of bounds for A000081 sequence")
    end
    return A000081_SEQUENCE[n + 1]  # +1 for 0-indexing
end

"""
    cumulative_a000081(n::Int)

Get cumulative sum of A000081 up to order n.
"""
function cumulative_a000081(n::Int)
    if n < 0 || n >= length(A000081_SEQUENCE)
        error("Index $n out of bounds for A000081 sequence")
    end
    return sum(A000081_SEQUENCE[1:(n+1)])
end

"""
    print_a000081_parameters(base_order::Int)

Print derived parameters for inspection.
"""
function print_a000081_parameters(base_order::Int)
    params = derive_jjjml_parameters(base_order)
    
    println("A000081-Derived Parameters (base_order=$base_order):")
    println("=" ^ 50)
    println("Reservoir:")
    println("  reservoir_size:  $(params.reservoir_size)")
    println("  num_reservoirs:  $(params.num_reservoirs)")
    println()
    println("Learning:")
    println("  learning_rate:   $(params.learning_rate)")
    println("  decay_rate:      $(params.decay_rate)")
    println("  mutation_rate:   $(params.mutation_rate)")
    println()
    println("Architecture:")
    println("  num_layers:      $(params.num_layers)")
    println("  hidden_dim:      $(params.hidden_dim)")
    println()
    println("Training:")
    println("  batch_size:      $(params.batch_size)")
    println("  num_epochs:      $(params.num_epochs)")
    println()
    println("Growth:")
    println("  growth_rate:     $(params.growth_rate)")
    println("=" ^ 50)
    
    return params
end
