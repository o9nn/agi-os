"""
Layer 1: Tensor Primitive Abstraction

Core tensor operations with type-stable implementations.
"""

# Abstract type for tensor operations
abstract type TensorOp end

"""
    MatMul <: TensorOp

Matrix multiplication operation.
"""
struct MatMul <: TensorOp
    A::AbstractArray
    B::AbstractArray
end

"""
    Transpose <: TensorOp

Matrix transpose operation.
"""
struct Transpose <: TensorOp
    A::AbstractArray
end

"""
    Reshape <: TensorOp

Array reshape operation.
"""
struct Reshape <: TensorOp
    A::AbstractArray
    dims::Tuple
end

"""
    execute(op::MatMul)

Execute matrix multiplication: A * B
"""
function execute(op::MatMul)
    return op.A * op.B
end

"""
    execute(op::Transpose)

Execute matrix transpose.
"""
function execute(op::Transpose)
    return transpose(op.A)
end

"""
    execute(op::Reshape)

Execute array reshape.
"""
function execute(op::Reshape)
    return reshape(op.A, op.dims)
end

"""
    matmul(A::AbstractArray, B::AbstractArray)

Matrix multiplication with type stability.
"""
function matmul(A::AbstractArray, B::AbstractArray)
    return A * B
end

"""
    tensor_transpose(A::AbstractArray)

Transpose operation.
"""
function tensor_transpose(A::AbstractArray)
    return transpose(A)
end

"""
    tensor_reshape(A::AbstractArray, dims::Tuple)

Reshape operation.
"""
function tensor_reshape(A::AbstractArray, dims::Tuple)
    return reshape(A, dims)
end
