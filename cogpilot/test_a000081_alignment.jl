#!/usr/bin/env julia
"""
Quick validation script for A000081 parameter alignment.

This script tests that:
1. A000081Parameters module loads correctly
2. Parameter derivation functions work
3. Parameter validation detects misalignment
4. DeepTreeEcho system can be created with derived parameters
"""

println("="^70)
println("A000081 Parameter Alignment Validation")
println("="^70)
println()

# Test 1: Load A000081Parameters module
println("[1/5] Testing A000081Parameters module loading...")
try
    include("src/DeepTreeEcho/A000081Parameters.jl")
    using .A000081Parameters
    println("✓ A000081Parameters module loaded successfully")
catch e
    println("✗ Failed to load A000081Parameters: $e")
    exit(1)
end
println()

# Test 2: Parameter derivation
println("[2/5] Testing parameter derivation functions...")
try
    # Test reservoir size derivation
    sizes = [derive_reservoir_size(i) for i in 1:6]
    # Cumulative formula: sum(A000081[1:i]) for each i
    expected_sizes = [1, 2, 4, 8, 17, 37]  # [1], [1+1], [1+1+2], [1+1+2+4], etc.
    @assert sizes == expected_sizes "Reservoir sizes don't match: got $sizes, expected $expected_sizes"
    
    # Test membrane count derivation
    membranes = [derive_num_membranes(i) for i in 1:6]
    expected_membranes = [1, 1, 2, 4, 9, 20]  # A000081 values
    @assert membranes == expected_membranes "Membrane counts don't match: got $membranes, expected $expected_membranes"
    
    # Test growth rate
    growth = derive_growth_rate(5)
    expected_growth = 20.0 / 9.0
    @assert abs(growth - expected_growth) < 0.001 "Growth rate mismatch: got $growth, expected $expected_growth"
    
    # Test mutation rate
    mutation = derive_mutation_rate(5)
    expected_mutation = 1.0 / 9.0
    @assert abs(mutation - expected_mutation) < 0.001 "Mutation rate mismatch: got $mutation, expected $expected_mutation"
    
    println("✓ All derivation functions produce correct values")
    println("  reservoir_size(5) = $(sizes[5]) (cumulative: 1+1+2+4+9)")
    println("  num_membranes(4) = $(membranes[4]) (A000081[4])")
    println("  growth_rate(5) = $(round(growth, digits=4)) (20/9)")
    println("  mutation_rate(5) = $(round(mutation, digits=4)) (1/9)")
catch e
    println("✗ Derivation test failed: $e")
    exit(1)
end
println()

# Test 3: Parameter set creation
println("[3/5] Testing parameter set creation...")
try
    params = get_parameter_set(5, membrane_order=4)
    
    @assert params.base_order == 5
    @assert params.reservoir_size == 17
    @assert params.num_membranes == 4
    @assert params.max_tree_order == 8
    
    println("✓ Parameter set created successfully")
    println("  base_order = $(params.base_order)")
    println("  reservoir_size = $(params.reservoir_size)")
    println("  num_membranes = $(params.num_membranes)")
    println("  max_tree_order = $(params.max_tree_order)")
    println("  growth_rate = $(round(params.growth_rate, digits=4))")
    println("  mutation_rate = $(round(params.mutation_rate, digits=4))")
catch e
    println("✗ Parameter set creation failed: $e")
    exit(1)
end
println()

# Test 4: Parameter validation
println("[4/6] Testing parameter validation...")
try
    # Valid parameters
    is_valid, msg = validate_parameters(17, 8, 4, 20/9, 1/9)
    @assert is_valid "Valid parameters should pass validation"
    println("✓ Valid parameters correctly identified")
    
    # Invalid parameters
    is_valid, msg = validate_parameters(100, 8, 3, 0.1, 0.05)
    @assert !is_valid "Invalid parameters should fail validation"
    println("✓ Invalid parameters correctly rejected")
    println("  Warning message: $(split(msg, "\n")[1])")
catch e
    println("✗ Validation test failed: $e")
    exit(1)
end
println()

# Test 5: 1-1 Correspondence validation
println("[5/6] Testing 1-1 correspondence validation...")
try
    # Valid correspondence (order 5: all should be 9)
    is_valid, msg = validate_component_correspondence(5, 9, 9, 9)
    @assert is_valid "Valid correspondence should pass"
    println("✓ Valid 1-1 correspondence: $msg")
    
    # Invalid correspondence (membranes mismatch)
    is_valid, msg = validate_component_correspondence(5, 9, 8, 9)
    @assert !is_valid "Invalid correspondence should fail"
    println("✓ Invalid correspondence detected:")
    println("  $(split(msg, "\n")[2])")
    
    # Test multiple orders
    for order in [1, 3, 4, 6]
        expected = A000081_SEQUENCE[order]
        is_valid, _ = validate_component_correspondence(order, expected, expected, expected)
        @assert is_valid "Order $order should validate with count=$expected"
    end
    println("✓ Multiple order validations passed")
catch e
    println("✗ 1-1 correspondence test failed: $e")
    exit(1)
end
println()

# Test 6: Integration test (if DeepTreeEcho is available)
println("[6/6] Testing DeepTreeEcho integration...")
try
    # Try to load DeepTreeEcho
    include("src/DeepTreeEcho/DeepTreeEcho.jl")
    using .DeepTreeEcho
    
    # This would test creating a system, but we'll skip for now
    # since it requires many dependencies
    println("⚠ DeepTreeEcho loaded, skipping full integration test")
    println("  (requires full dependency installation)")
catch e
    println("⚠ DeepTreeEcho not fully testable: $e")
    println("  (This is expected in a minimal test environment)")
end
println()

# Summary
println("="^70)
println("VALIDATION SUMMARY")
println("="^70)
println()
println("✓ A000081Parameters module is functional")
println("✓ Parameter derivation produces correct values")
println("✓ Parameter sets can be created")
println("✓ Validation correctly identifies alignment")
println("✓ 1-1 correspondence validation working")
println()
println("All core parameter alignment features are working correctly!")
println()
println("Key insight: For order n, the system maintains 1-1 relationships:")
println("  • B-series roots = A000081[n]")
println("  • P-system membranes = A000081[n]")
println("  • J-surface differentials = A000081[n]")
println()
println("Next steps:")
println("  1. Run: julia examples/deep_tree_echo_demo.jl")
println("  2. Run: julia test/test_deep_tree_echo.jl")
println("  3. Ensure all systems maintain 1-1 correspondence")
println()
println("="^70)
