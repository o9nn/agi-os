"""
JJJML (Julia + JAX + J-lang + ML)

A unified MLOps tensor framework that synthesizes three powerful programming paradigms:
- Julia: Scientific computing with SciML ecosystem
- JAX: Automatic differentiation and XLA compilation
- J-lang: Array-oriented tacit programming

Provides functionality similar to ggml, llama.cpp, and rwkv.cpp while incorporating
deep-tree-echo-state-reservoir computing and A000081 mathematical foundations.
"""
module JJJML

using LinearAlgebra
using Statistics
using Random

# Export core types and functions
export TensorOp, MatMul, Transpose, Reshape
export execute, matmul, tensor_transpose, tensor_reshape
export tanh_activation, sigmoid_activation, relu_activation, gelu_activation, softmax, layer_norm
export MultiHeadAttention, attention, scaled_dot_product_attention
export EchoStateReservoir, update_reservoir!, readout, train_esn!
export RootedTree, tree_order, symmetry, BSeriesKernel, evaluate_bseries, integrate_bseries
export compute_elementary_differential
export KVCache, TransformerLayer, TransformerModel, InferenceConfig, LLMInferenceEngine
export generate_token, sample_token
export derive_jjjml_parameters, get_a000081_value, cumulative_a000081, print_a000081_parameters
export load_model, generate, create_hybrid_engine, jjjml_demo

# A000081 sequence: number of rooted trees with n nodes
const A000081_SEQUENCE = [0, 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, 32973]

# Include submodules
include("TensorOps.jl")
include("Activations.jl")
include("Attention.jl")
include("ReservoirComputing.jl")
include("BSeries.jl")
include("BSeriesMethods.jl")
include("InferenceEngine.jl")
include("A000081Parameters.jl")
include("JAXBridge.jl")
include("Quantization.jl")
include("UnifiedAPI.jl")

end # module JJJML
