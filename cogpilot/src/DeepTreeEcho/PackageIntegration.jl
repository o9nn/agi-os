"""
    PackageIntegration

Integration module connecting Deep Tree Echo with existing Julia packages:
- RootedTrees.jl: Proper rooted tree implementation
- BSeries.jl: Complete B-series functionality
- ReservoirComputing.jl: Advanced ESN features

This module replaces the simplified implementations with full-featured packages.
"""
module PackageIntegration

# Add package paths to LOAD_PATH
push!(LOAD_PATH, joinpath(@__DIR__, "..", "..", "RootedTrees.jl", "src"))
push!(LOAD_PATH, joinpath(@__DIR__, "..", "..", "BSeries.jl", "src"))
push!(LOAD_PATH, joinpath(@__DIR__, "..", "..", "ReservoirComputing.jl", "src"))

using LinearAlgebra

# Try to load packages, fallback to our implementations if not available
const ROOTED_TREES_AVAILABLE = try
    # Load RootedTrees.jl
    include(joinpath(@__DIR__, "..", "..", "RootedTrees.jl", "src", "RootedTrees.jl"))
    using .RootedTrees
    true
catch e
    @warn "RootedTrees.jl not available: $e"
    false
end

const BSERIES_AVAILABLE = try
    # Load BSeries.jl
    include(joinpath(@__DIR__, "..", "..", "BSeries.jl", "src", "BSeries.jl"))
    using .BSeries
    true
catch e
    @warn "BSeries.jl not available: $e"
    false
end

const RESERVOIR_COMPUTING_AVAILABLE = try
    # Load ReservoirComputing.jl
    include(joinpath(@__DIR__, "..", "..", "ReservoirComputing.jl", "src", "ReservoirComputing.jl"))
    using .ReservoirComputing
    true
catch e
    @warn "ReservoirComputing.jl not available: $e"
    false
end

export convert_to_rooted_tree, convert_from_rooted_tree
export create_bseries_from_tree, evaluate_bseries
export create_esn_reservoir, train_esn!
export integration_status
export generate_trees_up_to_order, count_trees_of_order

"""
    integration_status()

Report which packages are successfully integrated.
"""
function integration_status()
    status = Dict{String, Bool}(
        "RootedTrees.jl" => ROOTED_TREES_AVAILABLE,
        "BSeries.jl" => BSERIES_AVAILABLE,
        "ReservoirComputing.jl" => RESERVOIR_COMPUTING_AVAILABLE
    )
    
    println("Package Integration Status:")
    for (pkg, available) in status
        symbol = available ? "✓" : "✗"
        println("  $symbol $pkg")
    end
    
    return status
end

# RootedTrees.jl Integration

if ROOTED_TREES_AVAILABLE
    """
        convert_to_rooted_tree(level_sequence::Vector{Int})
    
    Convert level sequence to RootedTrees.jl RootedTree object.
    """
    function convert_to_rooted_tree(level_sequence::Vector{Int})
        # RootedTrees.jl uses a different representation
        # Convert our level sequence to their format
        
        if isempty(level_sequence)
            return RootedTree(Int[])
        end
        
        # Build level sequence in RootedTrees.jl format
        # Their format: level sequence where each node stores its level
        return RootedTree(level_sequence)
    end
    
    """
        convert_from_rooted_tree(tree::RootedTree)
    
    Convert RootedTrees.jl RootedTree to our level sequence format.
    """
    function convert_from_rooted_tree(tree::RootedTree)
        return copy(tree.level_sequence)
    end
    
    """
        get_tree_order(tree::RootedTree)
    
    Get the order (number of nodes) of a rooted tree.
    """
    function get_tree_order(tree::RootedTree)
        return order(tree)
    end
    
    """
        get_tree_symmetry(tree::RootedTree)
    
    Get the symmetry factor σ(τ) of a rooted tree.
    """
    function get_tree_symmetry(tree::RootedTree)
        return symmetry(tree)
    end
    
    """
        butcher_product(tree1::RootedTree, tree2::RootedTree)
    
    Compute Butcher product of two rooted trees.
    """
    function butcher_product(tree1::RootedTree, tree2::RootedTree)
        return RootedTrees.butcher_product(tree1, tree2)
    end
    
else
    # Fallback implementations
    function convert_to_rooted_tree(level_sequence::Vector{Int})
        @warn "RootedTrees.jl not available, returning level sequence"
        return level_sequence
    end
    
    function convert_from_rooted_tree(tree)
        return tree
    end
    
    function get_tree_order(tree)
        return length(tree)
    end
    
    function get_tree_symmetry(tree)
        # Simplified symmetry calculation
        return 1
    end
    
    function butcher_product(tree1, tree2)
        # Simplified: just concatenate
        return vcat(tree1, tree2)
    end
end

# BSeries.jl Integration

if BSERIES_AVAILABLE
    """
        create_bseries_from_tree(tree::RootedTree, coefficient::Float64)
    
    Create a B-series term from a rooted tree.
    """
    function create_bseries_from_tree(tree::RootedTree, coefficient::Float64)
        # Create a B-series with single term
        return BSeries(Dict(tree => coefficient))
    end
    
    """
        evaluate_bseries(bseries::BSeries, f, y0, h)
    
    Evaluate B-series at a point.
    """
    function evaluate_bseries(bseries::BSeries, f, y0, h)
        # Evaluate the B-series
        return BSeries.evaluate(bseries, f, y0, h)
    end
    
    """
        get_order_conditions(order::Int)
    
    Get order conditions for B-series up to given order.
    """
    function get_order_conditions(order::Int)
        return BSeries.order_conditions(order)
    end
    
    """
        compose_bseries(bs1::BSeries, bs2::BSeries)
    
    Compose two B-series.
    """
    function compose_bseries(bs1::BSeries, bs2::BSeries)
        return BSeries.compose(bs1, bs2)
    end
    
else
    # Fallback implementations
    function create_bseries_from_tree(tree, coefficient::Float64)
        @warn "BSeries.jl not available, returning simple dict"
        return Dict(tree => coefficient)
    end
    
    function evaluate_bseries(bseries, f, y0, h)
        # Simplified evaluation
        return y0 + h * sum(values(bseries))
    end
    
    function get_order_conditions(order::Int)
        return []
    end
    
    function compose_bseries(bs1, bs2)
        return merge(+, bs1, bs2)
    end
end

# ReservoirComputing.jl Integration

if RESERVOIR_COMPUTING_AVAILABLE
    """
        create_esn_reservoir(input_size::Int, 
                            reservoir_size::Int,
                            output_size::Int;
                            spectral_radius::Float64=0.99,
                            sparsity::Float64=0.1)
    
    Create an Echo State Network reservoir using ReservoirComputing.jl.
    """
    function create_esn_reservoir(input_size::Int, 
                                 reservoir_size::Int,
                                 output_size::Int;
                                 spectral_radius::Float64=0.99,
                                 sparsity::Float64=0.1)
        # Create ESN
        esn = ESN(
            input_size,
            reservoir_size,
            output_size,
            spectral_radius=spectral_radius,
            sparsity=sparsity
        )
        
        return esn
    end
    
    """
        train_esn!(esn, input_data, target_data)
    
    Train an ESN reservoir.
    """
    function train_esn!(esn, input_data, target_data)
        return ReservoirComputing.train!(esn, input_data, target_data)
    end
    
    """
        predict_esn(esn, input_data)
    
    Generate predictions from trained ESN.
    """
    function predict_esn(esn, input_data)
        return ReservoirComputing.predict(esn, input_data)
    end
    
else
    # Fallback implementations
    function create_esn_reservoir(input_size::Int, 
                                 reservoir_size::Int,
                                 output_size::Int;
                                 spectral_radius::Float64=0.99,
                                 sparsity::Float64=0.1)
        @warn "ReservoirComputing.jl not available, creating simple reservoir"
        
        # Simple reservoir structure
        W_in = randn(reservoir_size, input_size) * 0.1
        W = randn(reservoir_size, reservoir_size)
        
        # Scale to spectral radius
        λ_max = maximum(abs.(eigvals(W)))
        W = W * (spectral_radius / λ_max)
        
        # Apply sparsity
        mask = rand(reservoir_size, reservoir_size) .< sparsity
        W = W .* mask
        
        W_out = randn(output_size, reservoir_size) * 0.1
        
        return Dict(
            "W_in" => W_in,
            "W" => W,
            "W_out" => W_out,
            "state" => zeros(reservoir_size)
        )
    end
    
    function train_esn!(esn, input_data, target_data)
        # Simple least squares training
        states = []
        state = esn["state"]
        
        for input in eachcol(input_data)
            state = tanh.(esn["W"] * state + esn["W_in"] * input)
            push!(states, copy(state))
        end
        
        state_matrix = hcat(states...)
        esn["W_out"] = target_data * pinv(state_matrix)
        
        return esn
    end
    
    function predict_esn(esn, input_data)
        outputs = []
        state = esn["state"]
        
        for input in eachcol(input_data)
            state = tanh.(esn["W"] * state + esn["W_in"] * input)
            output = esn["W_out"] * state
            push!(outputs, output)
        end
        
        return hcat(outputs...)
    end
end

# Utility functions

"""
    generate_trees_up_to_order(max_order::Int)

Generate all rooted trees up to given order.
"""
function generate_trees_up_to_order(max_order::Int)
    if ROOTED_TREES_AVAILABLE
        trees = RootedTree[]
        for order in 1:max_order
            append!(trees, RootedTrees.RootedTreeIterator(order))
        end
        return trees
    else
        @warn "RootedTrees.jl not available, generating simple trees"
        # Generate simple level sequences
        trees = Vector{Int}[]
        for order in 1:max_order
            push!(trees, ones(Int, order))
        end
        return trees
    end
end

"""
    count_trees_of_order(order::Int)

Count number of rooted trees of given order (A000081).
"""
function count_trees_of_order(order::Int)
    if ROOTED_TREES_AVAILABLE
        return length(collect(RootedTrees.RootedTreeIterator(order)))
    else
        # A000081 sequence (hardcoded)
        a000081 = [1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, 32973]
        return order <= length(a000081) ? a000081[order] : -1
    end
end

"""
    print_integration_info()

Print detailed integration information.
"""
function print_integration_info()
    println("\n" * "="^60)
    println("DEEP TREE ECHO PACKAGE INTEGRATION")
    println("="^60)
    
    status = integration_status()
    
    println("\nCapabilities:")
    
    if status["RootedTrees.jl"]
        println("  ✓ Full rooted tree implementation")
        println("  ✓ Butcher product operations")
        println("  ✓ Symmetry factor calculations")
        println("  ✓ Tree iteration by order")
    else
        println("  ○ Using simplified tree implementation")
    end
    
    if status["BSeries.jl"]
        println("  ✓ Complete B-series functionality")
        println("  ✓ Order conditions")
        println("  ✓ B-series composition")
        println("  ✓ Numerical integration methods")
    else
        println("  ○ Using simplified B-series")
    end
    
    if status["ReservoirComputing.jl"]
        println("  ✓ Advanced ESN implementations")
        println("  ✓ Multiple reservoir types")
        println("  ✓ Training algorithms")
        println("  ✓ Prediction methods")
    else
        println("  ○ Using simplified reservoir")
    end
    
    println("\nRecommendation:")
    if all(values(status))
        println("  All packages integrated successfully!")
    else
        missing = [k for (k, v) in status if !v]
        println("  Consider installing: $(join(missing, ", "))")
    end
    
    println("="^60)
end

end # module PackageIntegration
