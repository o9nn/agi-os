"""
JJJML Example: Basic Demonstration

This example demonstrates the core functionality of the JJJML framework,
which unifies Julia, JAX, and J-lang paradigms for machine learning.
"""

# Include JJJML module directly
include(joinpath(@__DIR__, "..", "src", "JJJML", "JJJML.jl"))
using .JJJML
using LinearAlgebra
using Statistics
using Random

Random.seed!(42)

println("\n" * "="^70)
println("JJJML (Julia + JAX + J-lang + ML) - Basic Example")
println("="^70)
println()

# Run the built-in demo
jjjml_demo(base_order=5)

println("\n" * "="^70)
println("Additional Examples")
println("="^70)
println()

# Example 1: Time Series Prediction with Echo State Network
println("Example 1: Time Series Prediction with ESN")
println("-"^70)

# Generate synthetic time series (sine wave with noise)
t = 0:0.1:10
signal = sin.(2π * 0.5 .* t) .+ 0.1 .* randn(length(t))

# Create ESN with A000081-derived parameters
params = derive_jjjml_parameters(5)
esn = EchoStateReservoir(1, params.reservoir_size, 1)

# Prepare training data
window_size = 10
inputs = [Float32[signal[i]] for i in 1:(length(signal)-window_size)]
targets = [Float32[signal[i+window_size]] for i in 1:(length(signal)-window_size)]

# Train ESN
println("Training ESN with reservoir size $(params.reservoir_size)...")
train_esn!(esn, inputs[1:50], targets[1:50])

# Test prediction
esn.state = zeros(Float32, esn.reservoir_size)
predictions = Float32[]
for i in 51:min(70, length(inputs))
    update_reservoir!(esn, inputs[i])
    pred = readout(esn)
    push!(predictions, pred[1])
end

mse = mean((predictions .- [t[1] for t in targets[51:min(70, length(targets))]]).^2)
println("Prediction MSE: $mse")
println()

# Example 2: B-Series Integration of ODE
println("Example 2: B-Series Integration")
println("-"^70)

# Simple harmonic oscillator: d²x/dt² = -x
# Converted to first-order system: [x, v]' = [v, -x]'
function harmonic_oscillator(y)
    x, v = y
    return Float64[v, -x]
end

# Initial condition
y0 = [1.0, 0.0]  # Start at x=1, v=0

# Integrate using B-series
kernel = BSeriesKernel(3, T=Float64)
times, trajectory = integrate_bseries(kernel, harmonic_oscillator, y0, (0.0, 2π), 0.1)

# Check energy conservation (should be approximately constant)
energies = [0.5 * (y[2]^2 + y[1]^2) for y in trajectory]
energy_variation = maximum(energies) - minimum(energies)
println("Energy conservation (lower is better): $energy_variation")
println("Initial energy: $(energies[1])")
println("Final energy: $(energies[end])")
println()

# Example 3: Transformer Attention Mechanism
println("Example 3: Multi-Head Attention")
println("-"^70)

# Create attention layer
d_model = 128
n_heads = 8
mha = MultiHeadAttention(n_heads, d_model)

# Create sample sequence (e.g., embedded tokens)
seq_len = 20
x = randn(Float32, d_model, seq_len)

# Apply attention
y = attention(mha, x)

println("Input shape: $(size(x))")
println("Output shape: $(size(y))")
println("Number of heads: $n_heads")
println("Dimension per head: $(mha.d_head)")
println()

# Example 4: A000081-Guided Configuration
println("Example 4: A000081-Guided System Configuration")
println("-"^70)

for order in 3:7
    params = derive_jjjml_parameters(order)
    println("Order $order:")
    println("  Reservoir size: $(params.reservoir_size)")
    println("  Hidden dim: $(params.hidden_dim)")
    println("  Learning rate: $(round(params.learning_rate, digits=4))")
    println("  Growth rate: $(round(params.growth_rate, digits=4))")
end
println()

# Example 5: Hybrid Engine Creation
println("Example 5: Creating Hybrid Inference Engine")
println("-"^70)

engine = create_hybrid_engine(
    nothing,  # No model loaded yet
    reservoir_size = 100,
    use_jax_autodiff = false,
    use_j_preprocessing = false,
    base_order = 5
)

println("Engine created with A000081-aligned configuration")
println("Temperature: $(engine.config.temperature)")
println("Max tokens: $(engine.config.max_tokens)")
println()

println("="^70)
println("JJJML Example Complete!")
println("="^70)
println()
println("Next steps:")
println("  1. Load a model with: load_model(\"path/to/model.gguf\")")
println("  2. Generate text with: generate(model, \"prompt\")")
println("  3. Explore JAX integration for autodiff")
println("  4. Explore J-lang integration for array operations")
println("="^70)
