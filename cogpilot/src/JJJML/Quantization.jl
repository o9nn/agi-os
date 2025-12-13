"""
Quantization

Model weight quantization for efficient inference.
Provides quantization methods similar to ggml/llama.cpp:
- Q4_K: 4-bit K-means quantization
- Q8_0: 8-bit quantization  
- F16: 16-bit floating point

# Features
- Reduced memory footprint
- Faster inference on CPU
- Compatible with ggml quantization schemes

# Example
```julia
# Quantize weights to 4-bit
weights_f32 = randn(Float32, 1000, 1000)
quantized = quantize(weights_f32, Q4_K())

# Dequantize for computation
weights_restored = dequantize(quantized)

# Check size reduction
original_size = sizeof(weights_f32)
quantized_size = sizeof(quantized)
compression = original_size / quantized_size
```
"""

using LinearAlgebra
using Statistics

#
# Quantization Types
#

"""
Abstract base type for quantization schemes.
"""
abstract type QuantizationType end

"""
    Q4_K <: QuantizationType

4-bit quantization using K-means clustering.
Each value is represented with 4 bits (16 levels).
Includes scale and offset per block for better accuracy.

Block size: 32 values
Storage per block:
- 16 bytes (4 bits × 32 values)
- 4 bytes (Float32 scale)
- 4 bytes (Float32 offset)
Total: 24 bytes per 32 values (vs 128 bytes for Float32)
Compression ratio: ~5.3x
"""
struct Q4_K <: QuantizationType
    block_size::Int
    
    Q4_K(; block_size=32) = new(block_size)
end

"""
    Q8_0 <: QuantizationType

8-bit quantization with scale factor per block.
Each value is represented with 8 bits (256 levels).

Block size: 32 values
Storage per block:
- 32 bytes (8 bits × 32 values)
- 4 bytes (Float32 scale)
Total: 36 bytes per 32 values (vs 128 bytes for Float32)
Compression ratio: ~3.6x
"""
struct Q8_0 <: QuantizationType
    block_size::Int
    
    Q8_0(; block_size=32) = new(block_size)
end

"""
    F16 <: QuantizationType

16-bit floating point (half precision).
Simple conversion to Float16.

Compression ratio: 2x
"""
struct F16 <: QuantizationType end

#
# Quantized Data Structures
#

"""
    QuantizedArray{T<:QuantizationType}

Container for quantized array data.
"""
struct QuantizedArray{T<:QuantizationType}
    qtype::T
    quantized_data::Vector{UInt8}
    scales::Vector{Float32}
    offsets::Union{Vector{Float32}, Nothing}
    original_shape::Tuple{Vararg{Int}}
    num_blocks::Int
end

#
# Quantization Functions
#

"""
    quantize(arr::AbstractArray{Float32}, qtype::Q4_K)

Quantize array to 4-bit K-means representation.

# Algorithm
1. Divide array into blocks of size `block_size`
2. For each block:
   - Compute min and max values
   - Map to 4-bit range [0, 15]
   - Store scale and offset

# Arguments
- `arr::AbstractArray{Float32}`: Input array
- `qtype::Q4_K`: Quantization configuration

# Returns
- `QuantizedArray{Q4_K}`: Quantized representation
"""
function quantize(arr::AbstractArray{Float32}, qtype::Q4_K)
    # Flatten array for processing
    flat = vec(arr)
    n = length(flat)
    block_size = qtype.block_size
    
    # Calculate number of blocks
    num_blocks = cld(n, block_size)  # Ceiling division
    
    # Pad to block boundary
    padded_size = num_blocks * block_size
    if n < padded_size
        flat = vcat(flat, zeros(Float32, padded_size - n))
    end
    
    # Allocate storage
    # 4 bits per value = 0.5 bytes per value
    bytes_per_block = div(block_size, 2)  # 4 bits = 0.5 bytes
    quantized_data = zeros(UInt8, num_blocks * bytes_per_block)
    scales = zeros(Float32, num_blocks)
    offsets = zeros(Float32, num_blocks)
    
    # Quantize each block
    for block_idx in 1:num_blocks
        # Extract block
        start_idx = (block_idx - 1) * block_size + 1
        end_idx = block_idx * block_size
        block = flat[start_idx:end_idx]
        
        # Compute scale and offset
        min_val = minimum(block)
        max_val = maximum(block)
        
        # Avoid division by zero
        range_val = max_val - min_val
        if range_val < 1e-10
            scale = 1.0f0
            offset = min_val
        else
            scale = range_val / 15.0f0  # 15 = 2^4 - 1
            offset = min_val
        end
        
        scales[block_idx] = scale
        offsets[block_idx] = offset
        
        # Quantize values to 4-bit
        for i in 1:block_size
            value = block[i]
            # Map to [0, 15]
            quantized = clamp(round(Int, (value - offset) / scale), 0, 15)
            
            # Pack two 4-bit values into one byte
            byte_idx = (block_idx - 1) * bytes_per_block + div(i - 1, 2) + 1
            if isodd(i)
                # Store in lower 4 bits
                quantized_data[byte_idx] |= UInt8(quantized)
            else
                # Store in upper 4 bits
                quantized_data[byte_idx] |= UInt8(quantized << 4)
            end
        end
    end
    
    return QuantizedArray{Q4_K}(
        qtype,
        quantized_data,
        scales,
        offsets,
        size(arr),
        num_blocks
    )
end

"""
    quantize(arr::AbstractArray{Float32}, qtype::Q8_0)

Quantize array to 8-bit representation.

# Algorithm
1. Divide array into blocks
2. For each block:
   - Compute scale factor
   - Map to 8-bit signed integer [-128, 127]

# Arguments
- `arr::AbstractArray{Float32}`: Input array
- `qtype::Q8_0`: Quantization configuration

# Returns
- `QuantizedArray{Q8_0}`: Quantized representation
"""
function quantize(arr::AbstractArray{Float32}, qtype::Q8_0)
    # Flatten array
    flat = vec(arr)
    n = length(flat)
    block_size = qtype.block_size
    
    # Calculate blocks
    num_blocks = cld(n, block_size)
    padded_size = num_blocks * block_size
    
    if n < padded_size
        flat = vcat(flat, zeros(Float32, padded_size - n))
    end
    
    # Allocate storage
    quantized_data = zeros(UInt8, padded_size)
    scales = zeros(Float32, num_blocks)
    
    # Quantize each block
    for block_idx in 1:num_blocks
        start_idx = (block_idx - 1) * block_size + 1
        end_idx = block_idx * block_size
        block = flat[start_idx:end_idx]
        
        # Compute scale (use max absolute value)
        max_abs = maximum(abs, block)
        scale = max_abs / 127.0f0  # 127 = 2^7 - 1
        
        if scale < 1e-10
            scale = 1.0f0
        end
        
        scales[block_idx] = scale
        
        # Quantize to signed 8-bit
        for i in 1:block_size
            value = block[i]
            quantized = clamp(round(Int, value / scale), -128, 127)
            # Convert to UInt8 (reinterpret bits)
            quantized_data[start_idx + i - 1] = reinterpret(UInt8, Int8(quantized))
        end
    end
    
    return QuantizedArray{Q8_0}(
        qtype,
        quantized_data,
        scales,
        nothing,  # No offsets for Q8_0
        size(arr),
        num_blocks
    )
end

"""
    quantize(arr::AbstractArray{Float32}, qtype::F16)

Convert array to 16-bit floating point.

# Arguments
- `arr::AbstractArray{Float32}`: Input array
- `qtype::F16`: Quantization type

# Returns
- `QuantizedArray{F16}`: Quantized representation
"""
function quantize(arr::AbstractArray{Float32}, qtype::F16)
    # Convert to Float16
    f16_arr = Float16.(arr)
    
    # Store as bytes
    quantized_data = reinterpret(UInt8, vec(f16_arr))
    
    return QuantizedArray{F16}(
        qtype,
        quantized_data,
        Float32[],  # No scales
        nothing,    # No offsets
        size(arr),
        0  # No blocks
    )
end

#
# Dequantization Functions
#

"""
    dequantize(qarr::QuantizedArray{Q4_K})

Dequantize 4-bit K-means representation back to Float32.
"""
function dequantize(qarr::QuantizedArray{Q4_K})
    block_size = qarr.qtype.block_size
    num_blocks = qarr.num_blocks
    
    # Allocate output
    total_size = num_blocks * block_size
    result = zeros(Float32, total_size)
    
    bytes_per_block = div(block_size, 2)
    
    # Dequantize each block
    for block_idx in 1:num_blocks
        scale = qarr.scales[block_idx]
        offset = qarr.offsets[block_idx]
        
        for i in 1:block_size
            # Unpack 4-bit value
            byte_idx = (block_idx - 1) * bytes_per_block + div(i - 1, 2) + 1
            byte = qarr.quantized_data[byte_idx]
            
            if isodd(i)
                # Lower 4 bits
                quantized = byte & 0x0F
            else
                # Upper 4 bits
                quantized = (byte >> 4) & 0x0F
            end
            
            # Dequantize
            result[(block_idx - 1) * block_size + i] = Float32(quantized) * scale + offset
        end
    end
    
    # Reshape to original shape
    total_elements = prod(qarr.original_shape)
    result = result[1:total_elements]
    return reshape(result, qarr.original_shape)
end

"""
    dequantize(qarr::QuantizedArray{Q8_0})

Dequantize 8-bit representation back to Float32.
"""
function dequantize(qarr::QuantizedArray{Q8_0})
    block_size = qarr.qtype.block_size
    num_blocks = qarr.num_blocks
    
    # Allocate output
    total_size = num_blocks * block_size
    result = zeros(Float32, total_size)
    
    # Dequantize each block
    for block_idx in 1:num_blocks
        scale = qarr.scales[block_idx]
        start_idx = (block_idx - 1) * block_size + 1
        end_idx = block_idx * block_size
        
        for i in start_idx:end_idx
            # Unpack signed 8-bit
            quantized = reinterpret(Int8, qarr.quantized_data[i])
            result[i] = Float32(quantized) * scale
        end
    end
    
    # Reshape to original shape
    total_elements = prod(qarr.original_shape)
    result = result[1:total_elements]
    return reshape(result, qarr.original_shape)
end

"""
    dequantize(qarr::QuantizedArray{F16})

Convert 16-bit float back to 32-bit float.
"""
function dequantize(qarr::QuantizedArray{F16})
    # Reinterpret bytes as Float16
    f16_vec = reinterpret(Float16, qarr.quantized_data)
    
    # Convert to Float32
    f32_vec = Float32.(f16_vec)
    
    # Reshape
    return reshape(f32_vec, qarr.original_shape)
end

#
# Utility Functions
#

"""
    compression_ratio(qarr::QuantizedArray)

Calculate compression ratio compared to original Float32 array.

# Returns
- Compression ratio (original_size / quantized_size)
"""
function compression_ratio(qarr::QuantizedArray)
    original_bytes = prod(qarr.original_shape) * sizeof(Float32)
    quantized_bytes = sizeof(qarr.quantized_data) + 
                     sizeof(qarr.scales) +
                     (qarr.offsets !== nothing ? sizeof(qarr.offsets) : 0)
    
    return original_bytes / quantized_bytes
end

"""
    quantization_error(original::AbstractArray{Float32}, qarr::QuantizedArray)

Compute mean absolute error between original and quantized/dequantized array.

# Returns
- Mean absolute error
"""
function quantization_error(original::AbstractArray{Float32}, qarr::QuantizedArray)
    dequantized = dequantize(qarr)
    return mean(abs, original .- dequantized)
end

"""
    print_quantization_info(qarr::QuantizedArray)

Print information about quantized array.
"""
function print_quantization_info(qarr::QuantizedArray{T}) where T
    println("=" ^ 60)
    println("Quantization Info")
    println("=" ^ 60)
    println("Type: $T")
    println("Original shape: $(qarr.original_shape)")
    println("Num elements: $(prod(qarr.original_shape))")
    println("Num blocks: $(qarr.num_blocks)")
    
    original_bytes = prod(qarr.original_shape) * sizeof(Float32)
    quantized_bytes = sizeof(qarr.quantized_data) + 
                     sizeof(qarr.scales) +
                     (qarr.offsets !== nothing ? sizeof(qarr.offsets) : 0)
    
    println("Original size: $(original_bytes) bytes")
    println("Quantized size: $(quantized_bytes) bytes")
    println("Compression ratio: $(round(compression_ratio(qarr), digits=2))x")
    println("=" ^ 60)
end

# Export types and functions
export QuantizationType, Q4_K, Q8_0, F16
export QuantizedArray
export quantize, dequantize
export compression_ratio, quantization_error, print_quantization_info
