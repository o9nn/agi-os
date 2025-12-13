"""
Layer 3: Reservoir Computing Integration

Echo State Networks with A000081 alignment.
"""

using SparseArrays

"""
    EchoStateReservoir{T}

Echo State Network reservoir with A000081-aligned parameters.

# Fields
- `W_in::Matrix{T}`: Input weights
- `W_res::SparseMatrixCSC{T}`: Reservoir weights (sparse)
- `W_out::Matrix{T}`: Output weights
- `state::Vector{T}`: Current reservoir state
- `spectral_radius::T`: Spectral radius of reservoir
- `reservoir_size::Int`: A000081-derived reservoir size
- `input_scaling::T`: Input scaling factor
- `leak_rate::T`: Leak rate for update equation
"""
mutable struct EchoStateReservoir{T}
    W_in::Matrix{T}
    W_res::SparseMatrixCSC{T}
    W_out::Matrix{T}
    state::Vector{T}
    spectral_radius::T
    reservoir_size::Int
    input_scaling::T
    leak_rate::T
end

"""
    EchoStateReservoir(input_size::Int, reservoir_size::Int, output_size::Int; 
                       spectral_radius=0.9, input_scaling=1.0, leak_rate=0.3, 
                       sparsity=0.1, T=Float32)

Create an Echo State Network reservoir.
"""
function EchoStateReservoir(input_size::Int, reservoir_size::Int, output_size::Int;
                           spectral_radius=0.9, input_scaling=1.0, leak_rate=0.3,
                           sparsity=0.1, T=Float32)
    # Initialize input weights
    W_in = randn(T, reservoir_size, input_size) .* T(input_scaling)
    
    # Initialize sparse reservoir weights
    W_res = sprandn(T, reservoir_size, reservoir_size, sparsity)
    
    # Scale to desired spectral radius
    eigenvalues = eigvals(Matrix(W_res))
    current_radius = maximum(abs.(eigenvalues))
    if current_radius > 0
        W_res = W_res .* (T(spectral_radius) / current_radius)
    end
    
    # Initialize output weights (to be trained)
    W_out = zeros(T, output_size, reservoir_size)
    
    # Initialize state
    state = zeros(T, reservoir_size)
    
    return EchoStateReservoir{T}(
        W_in, W_res, W_out, state,
        T(spectral_radius), reservoir_size, T(input_scaling), T(leak_rate)
    )
end

"""
    update_reservoir!(esn::EchoStateReservoir, input::Vector)

Update reservoir state using echo state equation:
state(t+1) = (1-α)·state(t) + α·tanh(W_in·input + W_res·state(t))

where α is the leak rate.
"""
function update_reservoir!(esn::EchoStateReservoir, input::Vector)
    pre_activation = esn.W_in * input + esn.W_res * esn.state
    esn.state = (1 - esn.leak_rate) .* esn.state .+ 
                esn.leak_rate .* tanh.(pre_activation)
    return esn.state
end

"""
    readout(esn::EchoStateReservoir)

Compute readout from current reservoir state.
"""
function readout(esn::EchoStateReservoir)
    return esn.W_out * esn.state
end

"""
    train_esn!(esn::EchoStateReservoir, inputs::Vector{<:Vector}, 
               targets::Vector{<:Vector})

Train ESN output weights using ridge regression.

Uses a numerically stable approach that avoids explicit matrix inversion
for large reservoirs. Ridge parameter provides regularization.

# Arguments
- `esn::EchoStateReservoir{T}`: Echo state network to train
- `inputs::Vector{<:Vector}`: Input sequences
- `targets::Vector{<:Vector}`: Target sequences
- `ridge_param=1e-6`: Ridge regression parameter (λ)

# Implementation
Solves: W_out = Y·S^T·(S·S^T + λI)^(-1)
Using: W_out = (Y·S^T) / (S·S^T + λI) with Julia's backslash operator
which uses QR or Cholesky decomposition for numerical stability.
"""
function train_esn!(esn::EchoStateReservoir{T}, 
                   inputs::Vector{<:Vector}, 
                   targets::Vector{<:Vector};
                   ridge_param=1e-6) where T
    n_samples = length(inputs)
    
    # Collect reservoir states
    states = Matrix{T}(undef, esn.reservoir_size, n_samples)
    for (i, input) in enumerate(inputs)
        update_reservoir!(esn, input)
        states[:, i] = esn.state
    end
    
    # Stack targets
    target_matrix = hcat(targets...)
    
    # Ridge regression using numerically stable solver
    # (S·S^T + λI) is positive definite, so Cholesky or QR will be used
    ridge_matrix = states * states' + T(ridge_param) * I(esn.reservoir_size)
    esn.W_out = target_matrix * states' / ridge_matrix
    
    return esn
end
