"""
Activation Functions

Common neural network activation functions.
"""

"""
    tanh_activation(x::AbstractArray)

Hyperbolic tangent activation function.
"""
function tanh_activation(x::AbstractArray)
    return tanh.(x)
end

"""
    sigmoid_activation(x::AbstractArray)

Sigmoid activation function: σ(x) = 1 / (1 + exp(-x))
"""
function sigmoid_activation(x::AbstractArray)
    return @. 1 / (1 + exp(-x))
end

"""
    relu_activation(x::AbstractArray)

Rectified Linear Unit: ReLU(x) = max(0, x)
"""
function relu_activation(x::AbstractArray)
    return max.(0, x)
end

"""
    gelu_activation(x::AbstractArray)

Gaussian Error Linear Unit (approximate):
GELU(x) ≈ 0.5 * x * (1 + tanh(√(2/π) * (x + 0.044715 * x³)))
"""
function gelu_activation(x::AbstractArray)
    return @. 0.5 * x * (1 + tanh(sqrt(2/π) * (x + 0.044715 * x^3)))
end

"""
    softmax(x::AbstractArray; dims=1)

Softmax activation function along specified dimension.
"""
function softmax(x::AbstractArray; dims=1)
    exp_x = exp.(x .- maximum(x; dims=dims))
    return exp_x ./ sum(exp_x; dims=dims)
end

"""
    layer_norm(x::AbstractArray; eps=1e-5)

Layer normalization.
"""
function layer_norm(x::AbstractArray; eps=1e-5)
    μ = mean(x)
    σ² = var(x)
    return @. (x - μ) / sqrt(σ² + eps)
end
