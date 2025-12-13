# JJJML (Julia + JAX + J-lang + ML)

A unified MLOps tensor framework that synthesizes three powerful programming paradigms into a cohesive system for machine learning inference and training:

- **Julia** - Scientific computing with SciML ecosystem
- **JAX** - Automatic differentiation and XLA compilation
- **J-lang** - Array-oriented tacit programming

This framework provides functionality similar to **ggml**, **llama.cpp**, and **rwkv.cpp** while incorporating **deep-tree-echo-state-reservoir computing** insights and **A000081** mathematical foundations.

## Features

### Layer 1: Tensor Primitive Abstraction
- Type-stable tensor operations (MatMul, Transpose, Reshape)
- Zero-copy operations where possible
- GPU-compatible data structures

### Layer 2: ML Model Components
- Multi-head attention mechanisms
- Transformer layer components
- Activation functions (tanh, sigmoid, ReLU, GELU)
- Layer normalization

### Layer 3: Reservoir Computing Integration
- Echo State Networks with A000081 alignment
- Sparse reservoir weight matrices
- Ridge regression training
- Temporal dynamics processing

### Layer 4: B-Series Integration
- **Classic numerical methods**: Euler, Midpoint, Heun, RK4
- **Adaptive integration**: Dormand-Prince (MATLAB ode45, SciPy default)
- **Production-quality ODE solvers** with error control
- Rooted tree algebra operations
- Elementary differential computation

### Layer 5: Model Inference Engine
- LLM inference capabilities
- KV cache for efficient generation
- Token sampling with temperature
- Placeholder for model loading (GGUF, safetensors, HDF5)

### NEW: JAX Integration
- **Automatic differentiation** via `jax_gradient`
- **XLA compilation** via `jax_jit` for hardware acceleration
- **Batch processing** via `jax_vmap`
- Zero-copy tensor transfer (Julia ↔ JAX)
- Optional dependency (graceful fallback)

### NEW: Quantization Support
- **Q4_K**: 4-bit K-means quantization (~5.3x compression)
- **Q8_0**: 8-bit quantization (~3.6x compression)
- **F16**: 16-bit float (2x compression)
- Model size reduction with minimal accuracy loss
- Compatible with ggml quantization schemes

### A000081 Parameter Alignment
All system parameters are derived from the OEIS A000081 sequence (number of rooted trees with n nodes), ensuring mathematical consistency:

```julia
params = derive_jjjml_parameters(5)
# Returns:
# - reservoir_size: Cumulative tree count
# - learning_rate: 1 / num_trees
# - growth_rate: Natural ratio from sequence
# - hidden_dim: 2^order * num_trees
# - and more...
```

## Installation

Currently, JJJML is part of the cogpilot.jl repository. To use it:

```julia
# Include the module
include("src/JJJML/JJJML.jl")
using .JJJML
```

## Quick Start

```julia
using JJJML
using LinearAlgebra
using Random

# Run the built-in demo
jjjml_demo(base_order=5)

# Or create components manually:

# 1. Derive A000081-aligned parameters
params = derive_jjjml_parameters(5)

# 2. Create an Echo State Network
esn = EchoStateReservoir(10, params.reservoir_size, 5)

# 3. Create multi-head attention
mha = MultiHeadAttention(8, 64)
x = randn(Float32, 64, 10)
y = attention(mha, x)

# 4. Use production ODE solvers
method = RK4()
f = y -> -y  # dy/dt = -y
y0 = [1.0]
times, solution = integrate(method, f, y0, (0.0, 1.0), 0.1)

# 5. Quantize model weights
weights = randn(Float32, 1000, 1000)
quantized = quantize(weights, Q4_K())
println("Compression: $(compression_ratio(quantized))x")

# 6. Use JAX autodiff (if available)
init_jax()
if is_jax_available()
    loss(x) = sum(x .^ 2)
    grad_fn = jax_gradient(loss)
    gradient = grad_fn(randn(Float32, 10))
end
```

## Examples

See the examples directory for comprehensive demonstrations:
- `jjjml_basic_demo.jl` - Core functionality (tensor ops, ESN, attention)
- `jjjml_advanced_demo.jl` - JAX integration and quantization
- `jjjml_bseries_demo.jl` - Production ODE solvers and method comparison
- B-series ODE integration
- Transformer attention
- A000081-guided configuration
- Hybrid engine creation

## Architecture

```
                    OEIS A000081 Sequence
                    (1, 1, 2, 4, 9, 20, 48, ...)
                            ↓
        ┌───────────────────┼───────────────────┐
        ↓                   ↓                   ↓
   Tensor Ops          Attention            Reservoir
   (Layer 1)           (Layer 2)            (Layer 3)
        ↓                   ↓                   ↓
        └───────────────────┼───────────────────┘
                            ↓
                     B-Series + Trees
                       (Layer 4)
                            ↓
                    Inference Engine
                       (Layer 5)
                            ↓
                   Unified API (Julia/JAX/J)
```

## Testing

Run the test suite:

```bash
julia test/test_jjjml.jl
```

Expected output: All 54 tests passing ✓

## API Reference

### Tensor Operations
- `matmul(A, B)` - Matrix multiplication
- `tensor_transpose(A)` - Matrix transpose
- `tensor_reshape(A, dims)` - Array reshape

### Activations
- `tanh_activation(x)` - Hyperbolic tangent
- `sigmoid_activation(x)` - Sigmoid function
- `relu_activation(x)` - Rectified Linear Unit
- `gelu_activation(x)` - Gaussian Error Linear Unit
- `softmax(x; dims=1)` - Softmax normalization
- `layer_norm(x)` - Layer normalization

### Attention
- `MultiHeadAttention(n_heads, d_model)` - Create attention layer
- `attention(mha, x)` - Apply multi-head attention
- `scaled_dot_product_attention(Q, K, V)` - Core attention mechanism

### Reservoir Computing
- `EchoStateReservoir(input_size, reservoir_size, output_size)` - Create ESN
- `update_reservoir!(esn, input)` - Update reservoir state
- `readout(esn)` - Get reservoir output
- `train_esn!(esn, inputs, targets)` - Train output weights

### B-Series
- `BSeriesKernel(order)` - Create B-series kernel
- `evaluate_bseries(kernel, f, y0, h)` - Single B-series step
- `integrate_bseries(kernel, f, y0, t_span, dt)` - Integrate ODE

### Parameters
- `derive_jjjml_parameters(base_order)` - Derive all parameters from A000081
- `get_a000081_value(n)` - Get n-th A000081 value
- `cumulative_a000081(n)` - Cumulative sum up to order n
- `print_a000081_parameters(base_order)` - Display parameters

### Unified API
- `load_model(path; backend=:julia)` - Load model (placeholder)
- `generate(model, prompt; kwargs...)` - Generate text (placeholder)
- `create_hybrid_engine(model; kwargs...)` - Create hybrid engine
- `jjjml_demo(; base_order=5)` - Run demonstration

## Mathematical Foundations

### A000081 Sequence
The number of rooted trees with n labeled nodes:
```
a(0)=0, a(1)=1, a(2)=1, a(3)=2, a(4)=4, a(5)=9, a(6)=20, ...
```

All parameters are derived from this sequence to ensure:
- Natural growth rates
- Optimal reservoir sizes
- Consistent learning rates
- Hierarchical structure alignment

### Echo State Networks
Update equation:
```
x(t+1) = (1-α)·x(t) + α·tanh(W_in·u(t) + W_res·x(t))
```

Where:
- α: leak rate (derived from A000081)
- W_in: input weights
- W_res: reservoir weights (sparse, spectral radius < 1)
- x(t): reservoir state

### B-Series
Integration formula:
```
y₁ = y₀ + Σ (h^p / σ(τ)) · b(τ) · F(τ)(y₀)
```

Where:
- h: step size
- τ: rooted tree of order p
- σ(τ): symmetry factor
- b(τ): B-series coefficient
- F(τ): elementary differential

## Future Work

### Phase 1: JAX Integration ✅ COMPLETE
- [x] Python bindings via PythonCall/PyCall
- [x] Zero-copy tensor transfer via DLPack
- [x] Automatic differentiation with jax.grad
- [x] XLA compilation for acceleration
- [x] Batch processing with vmap

### Phase 2: J-lang Integration
- [ ] J engine embedding
- [ ] Tacit array operations
- [ ] Rank polymorphism
- [ ] Array-oriented preprocessing

### Phase 3: Model Loading
- [ ] GGUF format support (llama.cpp style)
- [ ] Safetensors format (Hugging Face)
- [ ] HDF5 format
- [ ] JLD2 format (Julia native)

### Phase 4: Advanced Features
- [x] Quantization (Q4_K, Q8_0, F16) ✅ COMPLETE
- [x] Production ODE solvers (RK4, Dormand-Prince) ✅ COMPLETE
- [ ] GPU acceleration (CUDA, Metal, ROCm)
- [ ] Distributed computing
- [ ] Real-time visualization

### Phase 5: Applications
- [ ] LLM inference examples
- [ ] Time series forecasting
- [ ] Symbolic regression
- [ ] Physics-informed learning

## Contributing

This framework is part of the cogpilot.jl ecosystem. Contributions welcome!

Key areas:
1. JAX integration via PythonCall
2. J-lang integration via J engine
3. Model format loaders
4. GPU kernels
5. Documentation and examples

## License

Part of cogpilot.jl - see main repository for license information.

## Citation

If you use JJJML in your research, please cite:

```bibtex
@software{jjjml2025,
  title = {JJJML: Julia + JAX + J-lang + ML Unified Framework},
  author = {cogpilot.jl contributors},
  year = {2025},
  url = {https://github.com/cogpy/cogpilot.jl}
}
```

## Acknowledgments

JJJML builds upon:
- **RootedTrees.jl** - Rooted tree operations
- **BSeries.jl** - B-series methods
- **ReservoirComputing.jl** - Echo state networks
- **ModelingToolkit.jl** - Symbolic modeling
- **JAX** - Automatic differentiation
- **J-lang** - Array programming

---

**JJJML**: Where Julia's science, JAX's gradients, and J's arrays unite for cognitive machine learning. 🔬⚡📊
