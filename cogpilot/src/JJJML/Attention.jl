"""
Layer 2: ML Model Components

Transformer attention mechanisms and related components.
"""

"""
    MultiHeadAttention{T}

Multi-head attention layer for transformers.

# Fields
- `n_heads::Int`: Number of attention heads
- `d_model::Int`: Model dimension
- `d_head::Int`: Dimension per head
- `W_q::Matrix{T}`: Query projection weights
- `W_k::Matrix{T}`: Key projection weights
- `W_v::Matrix{T}`: Value projection weights
- `W_o::Matrix{T}`: Output projection weights
"""
struct MultiHeadAttention{T}
    n_heads::Int
    d_model::Int
    d_head::Int
    W_q::Matrix{T}
    W_k::Matrix{T}
    W_v::Matrix{T}
    W_o::Matrix{T}
end

"""
    MultiHeadAttention(n_heads::Int, d_model::Int; T=Float32)

Create a multi-head attention layer.
"""
function MultiHeadAttention(n_heads::Int, d_model::Int; T=Float32)
    d_head = d_model ÷ n_heads
    W_q = randn(T, d_model, d_model) * T(0.02)
    W_k = randn(T, d_model, d_model) * T(0.02)
    W_v = randn(T, d_model, d_model) * T(0.02)
    W_o = randn(T, d_model, d_model) * T(0.02)
    return MultiHeadAttention{T}(n_heads, d_model, d_head, W_q, W_k, W_v, W_o)
end

"""
    scaled_dot_product_attention(Q, K, V; mask=nothing)

Scaled dot-product attention: Attention(Q,K,V) = softmax(QK^T/√d_k)V

# Arguments
- `Q`: Query matrix
- `K`: Key matrix
- `V`: Value matrix
- `mask`: Optional attention mask
"""
function scaled_dot_product_attention(Q::AbstractMatrix, K::AbstractMatrix, V::AbstractMatrix; 
                                      mask=nothing)
    d_k = size(K, 1)
    scores = (Q' * K) / sqrt(d_k)
    
    if mask !== nothing
        scores = scores .+ mask
    end
    
    attn_weights = softmax(scores; dims=2)
    return V * attn_weights, attn_weights
end

"""
    attention(mha::MultiHeadAttention, x::AbstractMatrix)

Apply multi-head attention to input.

# Arguments
- `mha::MultiHeadAttention`: Multi-head attention layer
- `x::AbstractMatrix`: Input matrix (d_model × seq_len)

# Returns
- Output matrix after attention and output projection
"""
function attention(mha::MultiHeadAttention, x::AbstractMatrix)
    # Query, Key, Value projections
    Q = mha.W_q * x
    K = mha.W_k * x
    V = mha.W_v * x
    
    # Scaled dot-product attention
    attn_out, _ = scaled_dot_product_attention(Q, K, V)
    
    # Output projection
    return mha.W_o * attn_out
end
