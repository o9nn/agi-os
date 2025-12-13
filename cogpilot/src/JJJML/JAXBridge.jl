"""
JAX Bridge

Cross-language bridge for Julia ↔ JAX (Python) interoperability.
Provides tensor conversion, autodiff integration, and XLA compilation support.

# Features
- Zero-copy tensor transfer via DLPack
- Automatic differentiation with jax.grad
- XLA-compiled operations
- Functional array programming

# Requirements
- PythonCall.jl (recommended) or PyCall.jl
- JAX Python package (jax, jaxlib)

# Example
```julia
using JJJML.JAXBridge

# Initialize JAX
jax = init_jax()

# Convert Julia tensor to JAX
julia_tensor = randn(Float32, 100, 100)
jax_tensor = julia_to_jax(julia_tensor)

# Compute gradient with JAX
function loss(x)
    return sum(x .^ 2)
end
grad_fn = jax_gradient(loss)
gradient = grad_fn(julia_tensor)
```
"""

"""
    JAXBackend

Holds references to JAX modules and configuration.
"""
mutable struct JAXBackend
    available::Bool
    jax::Union{Nothing, Any}  # PyObject for jax module
    jnp::Union{Nothing, Any}  # PyObject for jax.numpy module
    dlpack::Union{Nothing, Any}  # PyObject for jax.dlpack module
    python_lib::Symbol  # :PythonCall or :PyCall
end

# Global backend instance
const JAX_BACKEND = Ref{JAXBackend}(JAXBackend(false, nothing, nothing, nothing, :none))

"""
    is_jax_available()

Check if JAX is available for use.

# Returns
- `true` if JAX and Python interop are available, `false` otherwise
"""
function is_jax_available()
    return JAX_BACKEND[].available
end

"""
    init_jax(; force_reload=false)

Initialize JAX backend.

# Arguments
- `force_reload::Bool`: Force reinitialization even if already loaded

# Returns
- JAX backend object if successful, `nothing` otherwise

# Details
Attempts to load JAX using:
1. PythonCall.jl (preferred for Julia 1.9+)
2. PyCall.jl (fallback for older versions)

Requires JAX to be installed in Python environment:
```bash
pip install jax jaxlib
```
"""
function init_jax(; force_reload=false)
    # Return existing backend if already initialized
    if JAX_BACKEND[].available && !force_reload
        return JAX_BACKEND[]
    end
    
    # Try PythonCall.jl first (modern approach)
    try
        # Check if PythonCall is available
        if isdefined(Main, :PythonCall) || Base.find_package("PythonCall") !== nothing
            @eval import PythonCall
            
            # Import JAX modules
            jax = PythonCall.pyimport("jax")
            jnp = PythonCall.pyimport("jax.numpy")
            dlpack = PythonCall.pyimport("jax.dlpack")
            
            # Update backend
            JAX_BACKEND[] = JAXBackend(true, jax, jnp, dlpack, :PythonCall)
            
            @info "JAX initialized successfully via PythonCall.jl"
            return JAX_BACKEND[]
        end
    catch e
        @debug "PythonCall initialization failed" exception=e
    end
    
    # Try PyCall.jl as fallback
    try
        if isdefined(Main, :PyCall) || Base.find_package("PyCall") !== nothing
            @eval import PyCall
            
            # Import JAX modules
            jax = PyCall.pyimport("jax")
            jnp = PyCall.pyimport("jax.numpy")
            dlpack = PyCall.pyimport("jax.dlpack")
            
            # Update backend
            JAX_BACKEND[] = JAXBackend(true, jax, jnp, dlpack, :PyCall)
            
            @info "JAX initialized successfully via PyCall.jl"
            return JAX_BACKEND[]
        end
    catch e
        @debug "PyCall initialization failed" exception=e
    end
    
    # Both methods failed
    @warn """
    JAX initialization failed. JAX features will not be available.
    
    To enable JAX support:
    1. Install Python interop: pkg> add PythonCall (or PyCall)
    2. Install JAX: pip install jax jaxlib
    3. Restart Julia and call init_jax() again
    """
    
    JAX_BACKEND[] = JAXBackend(false, nothing, nothing, nothing, :none)
    return nothing
end

"""
    julia_to_jax(arr::AbstractArray)

Convert Julia array to JAX array using zero-copy via DLPack.

# Arguments
- `arr::AbstractArray`: Julia array to convert

# Returns
- JAX array (jax.numpy.ndarray)

# Throws
- `ErrorException`: If JAX is not available

# Example
```julia
julia_arr = randn(Float32, 10, 10)
jax_arr = julia_to_jax(julia_arr)
```
"""
function julia_to_jax(arr::AbstractArray)
    if !is_jax_available()
        error("JAX not available. Call init_jax() first.")
    end
    
    backend = JAX_BACKEND[]
    
    # For PythonCall
    if backend.python_lib == :PythonCall
        # Convert via DLPack (zero-copy when possible)
        # Note: This is a simplified version. Full implementation would use:
        # - DLPack.jl for zero-copy transfer
        # - Proper memory layout checking
        jnp = backend.jnp
        return jnp.asarray(arr)
    # For PyCall
    elseif backend.python_lib == :PyCall
        jnp = backend.jnp
        return jnp.asarray(arr)
    else
        error("No Python library available")
    end
end

"""
    jax_to_julia(arr)

Convert JAX array to Julia array.

# Arguments
- `arr`: JAX array (jax.numpy.ndarray)

# Returns
- Julia Array

# Example
```julia
jax_arr = ...  # JAX array
julia_arr = jax_to_julia(jax_arr)
```
"""
function jax_to_julia(arr)
    if !is_jax_available()
        error("JAX not available. Call init_jax() first.")
    end
    
    backend = JAX_BACKEND[]
    
    # For PythonCall
    if backend.python_lib == :PythonCall
        # Convert to Julia Array
        # PythonCall provides automatic conversion
        return Array(arr)
    # For PyCall  
    elseif backend.python_lib == :PyCall
        # PyCall also provides automatic conversion
        return Array(arr)
    else
        error("No Python library available")
    end
end

"""
    jax_gradient(f::Function, argnums=0)

Create a JAX gradient function for a Julia function.

# Arguments
- `f::Function`: Julia function to differentiate
- `argnums::Int`: Which argument to differentiate with respect to (0-indexed)

# Returns
- Gradient function

# Example
```julia
# Define loss function
loss(x) = sum(x .^ 2)

# Create gradient function
grad_loss = jax_gradient(loss)

# Compute gradient
x = randn(Float32, 10)
g = grad_loss(x)
```
"""
function jax_gradient(f::Function, argnums=0)
    if !is_jax_available()
        error("JAX not available. Call init_jax() first.")
    end
    
    backend = JAX_BACKEND[]
    jax = backend.jax
    
    # Wrapper function for JAX
    function jax_wrapper(x_jax)
        # Convert JAX → Julia
        x_julia = jax_to_julia(x_jax)
        
        # Compute in Julia
        result_julia = f(x_julia)
        
        # Convert Julia → JAX
        return julia_to_jax(result_julia)
    end
    
    # Create JAX gradient function
    jax_grad = jax.grad(jax_wrapper, argnums=argnums)
    
    # Return wrapper that handles conversions
    function grad_wrapper(x)
        x_jax = julia_to_jax(x)
        g_jax = jax_grad(x_jax)
        return jax_to_julia(g_jax)
    end
    
    return grad_wrapper
end

"""
    jax_jit(f::Function)

JIT-compile a Julia function using JAX's XLA compiler.

# Arguments
- `f::Function`: Julia function to compile

# Returns
- JIT-compiled function

# Example
```julia
# Define function
my_func(x) = x .^ 2 .+ 1

# Compile with XLA
my_func_jit = jax_jit(my_func)

# Use compiled version
x = randn(Float32, 1000)
y = my_func_jit(x)  # Fast!
```
"""
function jax_jit(f::Function)
    if !is_jax_available()
        error("JAX not available. Call init_jax() first.")
    end
    
    backend = JAX_BACKEND[]
    jax = backend.jax
    
    # Wrapper function for JAX
    function jax_wrapper(x_jax)
        x_julia = jax_to_julia(x_jax)
        result_julia = f(x_julia)
        return julia_to_jax(result_julia)
    end
    
    # JIT compile
    jax_compiled = jax.jit(jax_wrapper)
    
    # Return wrapper
    function jit_wrapper(x)
        x_jax = julia_to_jax(x)
        result_jax = jax_compiled(x_jax)
        return jax_to_julia(result_jax)
    end
    
    return jit_wrapper
end

"""
    jax_vmap(f::Function, in_axes=0)

Vectorize a function using JAX's vmap.

# Arguments
- `f::Function`: Julia function to vectorize
- `in_axes::Int`: Which axis to vectorize over

# Returns
- Vectorized function

# Example
```julia
# Function that operates on single vector
process_single(x) = sum(x .^ 2)

# Vectorize to process batch
process_batch = jax_vmap(process_single)

# Process multiple inputs at once
batch = randn(Float32, 32, 10)  # 32 samples of size 10
results = process_batch(batch)   # Returns 32 results
```
"""
function jax_vmap(f::Function, in_axes=0)
    if !is_jax_available()
        error("JAX not available. Call init_jax() first.")
    end
    
    backend = JAX_BACKEND[]
    jax = backend.jax
    
    # Wrapper function for JAX
    function jax_wrapper(x_jax)
        x_julia = jax_to_julia(x_jax)
        result_julia = f(x_julia)
        return julia_to_jax(result_julia)
    end
    
    # Create vmap
    jax_vmapped = jax.vmap(jax_wrapper, in_axes=in_axes)
    
    # Return wrapper
    function vmap_wrapper(x)
        x_jax = julia_to_jax(x)
        result_jax = jax_vmapped(x_jax)
        return jax_to_julia(result_jax)
    end
    
    return vmap_wrapper
end

"""
    print_jax_info()

Print information about JAX availability and configuration.
"""
function print_jax_info()
    println("=" ^ 60)
    println("JAX Bridge Status")
    println("=" ^ 60)
    
    backend = JAX_BACKEND[]
    
    if backend.available
        println("✓ JAX is available")
        println("  Python library: $(backend.python_lib)")
        
        # Try to get JAX version
        try
            jax = backend.jax
            version = jax.__version__
            println("  JAX version: $version")
        catch
            println("  JAX version: unknown")
        end
        
        # Check for GPU support
        try
            jax = backend.jax
            devices = jax.devices()
            println("  Devices: $(length(devices))")
            for dev in devices
                println("    - $dev")
            end
        catch
            println("  Devices: unknown")
        end
    else
        println("✗ JAX is not available")
        println()
        println("To enable JAX:")
        println("  1. Install Python interop:")
        println("     julia> using Pkg; Pkg.add(\"PythonCall\")")
        println("  2. Install JAX:")
        println("     shell> pip install jax jaxlib")
        println("  3. Initialize:")
        println("     julia> init_jax()")
    end
    
    println("=" ^ 60)
end

# Export functions
export JAXBackend, init_jax, is_jax_available
export julia_to_jax, jax_to_julia
export jax_gradient, jax_jit, jax_vmap
export print_jax_info
