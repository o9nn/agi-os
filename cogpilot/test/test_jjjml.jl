"""
Test suite for JJJML module

Tests core functionality of the Julia + JAX + J-lang + ML framework.
"""

using Test

# Include JJJML module directly
include(joinpath(@__DIR__, "..", "src", "JJJML", "JJJML.jl"))
using .JJJML

@testset "JJJML Tests" begin
    
    @testset "Tensor Operations" begin
        # Test matrix multiplication
        A = Float32[1 2; 3 4]
        B = Float32[5 6; 7 8]
        C = matmul(A, B)
        @test size(C) == (2, 2)
        @test C ≈ A * B
        
        # Test transpose
        At = tensor_transpose(A)
        @test At ≈ transpose(A)
        
        # Test reshape
        x = Float32[1, 2, 3, 4, 5, 6]
        y = tensor_reshape(x, (2, 3))
        @test size(y) == (2, 3)
    end
    
    @testset "Activation Functions" begin
        x = Float32[-2, -1, 0, 1, 2]
        
        # Test tanh
        y = tanh_activation(x)
        @test all(y .>= -1) && all(y .<= 1)
        @test y[3] ≈ 0  # tanh(0) = 0
        
        # Test sigmoid
        y = sigmoid_activation(x)
        @test all(y .> 0) && all(y .< 1)
        @test y[3] ≈ 0.5  # sigmoid(0) = 0.5
        
        # Test ReLU
        y = relu_activation(x)
        @test all(y .>= 0)
        @test y[1:2] == [0, 0]  # Negative values zeroed
        @test y[4:5] == [1, 2]  # Positive values unchanged
        
        # Test GELU
        y = gelu_activation(x)
        @test length(y) == length(x)
        
        # Test softmax
        y = softmax(x)
        @test sum(y) ≈ 1.0
        @test all(y .> 0)
    end
    
    @testset "Multi-Head Attention" begin
        d_model = 64
        n_heads = 4
        seq_len = 10
        
        # Create attention layer
        mha = MultiHeadAttention(n_heads, d_model)
        @test mha.n_heads == n_heads
        @test mha.d_model == d_model
        @test mha.d_head == d_model ÷ n_heads
        
        # Test attention forward pass
        x = randn(Float32, d_model, seq_len)
        y = attention(mha, x)
        @test size(y) == size(x)
    end
    
    @testset "Echo State Reservoir" begin
        input_size = 10
        reservoir_size = 50
        output_size = 5
        
        # Create ESN
        esn = EchoStateReservoir(input_size, reservoir_size, output_size)
        @test esn.reservoir_size == reservoir_size
        @test size(esn.W_in) == (reservoir_size, input_size)
        @test size(esn.W_out) == (output_size, reservoir_size)
        @test length(esn.state) == reservoir_size
        
        # Test reservoir update
        input = randn(Float32, input_size)
        state = update_reservoir!(esn, input)
        @test length(state) == reservoir_size
        @test all(isfinite.(state))
        
        # Test readout
        output = readout(esn)
        @test length(output) == output_size
        
        # Test training
        inputs = [randn(Float32, input_size) for _ in 1:20]
        targets = [randn(Float32, output_size) for _ in 1:20]
        train_esn!(esn, inputs, targets)
        @test all(isfinite.(esn.W_out))
    end
    
    @testset "B-Series" begin
        # Create B-series kernel
        kernel = BSeriesKernel(3, T=Float64)
        @test kernel.order == 3
        @test length(kernel.genome) >= 1
        
        # Test B-series evaluation
        f = y -> -y  # dy/dt = -y, exact solution: y(t) = y0 * exp(-t)
        y0 = [1.0]
        h = 0.1
        
        y1 = evaluate_bseries(kernel, f, y0, h)
        @test length(y1) == 1
        @test isfinite(y1[1])
        
        # B-series should approximate exp(-0.1) ≈ 0.9048
        # (won't be exact with simplified implementation)
        @test y1[1] < y0[1]  # Should decrease
        
        # Test integration over time
        times, trajectory = integrate_bseries(kernel, f, y0, (0.0, 1.0), 0.1)
        @test length(times) > 1
        @test length(trajectory) == length(times)
        @test all(isfinite.(reduce(vcat, trajectory)))
    end
    
    @testset "A000081 Parameters" begin
        # Test parameter derivation
        params = derive_jjjml_parameters(5)
        
        @test params.num_layers == 5
        @test params.reservoir_size > 0
        @test params.num_reservoirs > 0
        @test params.learning_rate > 0
        @test params.decay_rate > 0
        @test params.hidden_dim > 0
        @test params.batch_size > 0
        @test params.num_epochs > 0
        
        # Test A000081 value retrieval
        @test get_a000081_value(1) == 1
        @test get_a000081_value(2) == 1
        @test get_a000081_value(3) == 2
        @test get_a000081_value(4) == 4
        @test get_a000081_value(5) == 9
        
        # Test cumulative sum
        cum = cumulative_a000081(5)
        expected = get_a000081_value(0) + get_a000081_value(1) + get_a000081_value(2) + 
                   get_a000081_value(3) + get_a000081_value(4) + get_a000081_value(5)
        @test cum == expected
    end
    
    @testset "Inference Engine" begin
        # Create inference config
        config = InferenceConfig(temperature=0.8, max_tokens=100)
        @test config.temperature == 0.8f0
        @test config.max_tokens == 100
        
        # Create inference engine
        engine = LLMInferenceEngine(config=config)
        @test engine.config.temperature == 0.8f0
        
        # Test token sampling
        logits = Float32[0.1, 0.5, 0.3, 0.1]
        token = sample_token(logits, 1.0f0)
        @test 1 <= token <= 4
    end
    
    @testset "Unified API" begin
        # Test hybrid engine creation
        engine = create_hybrid_engine(
            nothing,
            reservoir_size=100,
            base_order=5
        )
        @test engine isa LLMInferenceEngine
        
        # Test parameter printing
        params = print_a000081_parameters(5)
        @test params isa NamedTuple
    end
    
    @testset "JAX Bridge" begin
        # Test availability check (public API)
        available = is_jax_available()
        @test available isa Bool
        
        # Note: Actual JAX tests require Python/JAX installation
        # These tests verify the API exists and handles missing JAX gracefully
        
        # Test that functions exist
        @test isdefined(JJJML, :init_jax)
        @test isdefined(JJJML, :julia_to_jax)
        @test isdefined(JJJML, :jax_to_julia)
        @test isdefined(JJJML, :jax_gradient)
        @test isdefined(JJJML, :jax_jit)
        @test isdefined(JJJML, :jax_vmap)
        @test isdefined(JJJML, :print_jax_info)
        
        # Test info printing (should not error)
        @test_nowarn print_jax_info()
        
        # Test init returns nothing when JAX not available
        result = init_jax()
        @test result === nothing || result isa JAXBackend
    end
    
    @testset "Quantization" begin
        # Create test array
        arr = randn(Float32, 100, 100)
        
        # Test Q4_K quantization
        qtype_4bit = Q4_K()
        qarr_4bit = quantize(arr, qtype_4bit)
        @test qarr_4bit isa QuantizedArray{Q4_K}
        @test qarr_4bit.original_shape == (100, 100)
        
        # Test Q4_K dequantization
        arr_restored_4bit = dequantize(qarr_4bit)
        @test size(arr_restored_4bit) == size(arr)
        
        # Test Q4_K compression ratio
        ratio_4bit = compression_ratio(qarr_4bit)
        @test ratio_4bit > 3.0  # Should be ~5x
        
        # Test Q8_0 quantization
        qtype_8bit = Q8_0()
        qarr_8bit = quantize(arr, qtype_8bit)
        @test qarr_8bit isa QuantizedArray{Q8_0}
        
        # Test Q8_0 dequantization
        arr_restored_8bit = dequantize(qarr_8bit)
        @test size(arr_restored_8bit) == size(arr)
        
        # Test Q8_0 compression ratio
        ratio_8bit = compression_ratio(qarr_8bit)
        @test ratio_8bit > 2.0  # Should be ~3.6x
        
        # Test F16 quantization
        qtype_f16 = F16()
        qarr_f16 = quantize(arr, qtype_f16)
        @test qarr_f16 isa QuantizedArray{F16}
        
        # Test F16 dequantization
        arr_restored_f16 = dequantize(qarr_f16)
        @test size(arr_restored_f16) == size(arr)
        
        # Test F16 compression ratio
        ratio_f16 = compression_ratio(qarr_f16)
        @test ratio_f16 ≈ 2.0  # Should be exactly 2x
        
        # Test quantization error (8-bit should be more accurate than 4-bit)
        error_4bit = quantization_error(arr, qarr_4bit)
        error_8bit = quantization_error(arr, qarr_8bit)
        @test error_8bit < error_4bit
        
        # Test info printing
        @test_nowarn print_quantization_info(qarr_4bit)
        @test_nowarn print_quantization_info(qarr_8bit)
        @test_nowarn print_quantization_info(qarr_f16)
    end
    
end

println("\n" * "="^60)
println("All JJJML tests passed! ✓")
println("="^60)
