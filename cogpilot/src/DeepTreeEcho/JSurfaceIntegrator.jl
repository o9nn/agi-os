"""
J-Surface integrator for uniting gradient descent and evolution dynamics.

The J-surface is a Riemannian manifold formed by elementary differentials
(rooted trees), where both continuous gradient flow and discrete evolutionary
jumps can occur.
"""

"""
    JSurface

Represents the manifold of elementary differentials.

The J-surface is equipped with a Riemannian metric that measures distances
between different B-series configurations.

# Fields
- `trees::Vector{RootedTree}`: Rooted trees defining the surface
- `coefficients::Vector{Float64}`: Current position on the surface
- `metric_tensor::Matrix{Float64}`: Riemannian metric
- `order::Int`: Maximum order of trees
"""
struct JSurface
    trees::Vector{RootedTree}
    coefficients::Vector{Float64}
    metric_tensor::Matrix{Float64}
    order::Int
    
    function JSurface(trees::Vector{RootedTree}, coefficients::Vector{Float64})
        n = length(trees)
        @assert length(coefficients) == n "Coefficients must match number of trees"
        
        # Initialize metric tensor as identity (Euclidean metric)
        # Can be refined based on tree structure
        metric = Matrix{Float64}(I, n, n)
        
        order_val = isempty(trees) ? 0 : maximum(RootedTrees.order(t) for t in trees)
        
        new(trees, coefficients, metric, order_val)
    end
end

"""
    JSurfaceIntegrator

Integrator for dynamics on the J-surface.

Combines continuous gradient descent with discrete evolutionary jumps.

# Fields
- `surface::JSurface`: Current position on J-surface
- `gradient_step_size::Float64`: Step size for gradient descent
- `mutation_rate::Float64`: Rate of evolutionary mutations
- `velocity::Vector{Float64}`: Current velocity (for momentum methods)
- `history::Vector{JSurface}`: History of positions
"""
mutable struct JSurfaceIntegrator
    surface::JSurface
    gradient_step_size::Float64
    mutation_rate::Float64
    velocity::Vector{Float64}
    history::Vector{JSurface}
    
    function JSurfaceIntegrator(surface::JSurface,
                               gradient_step_size::Float64 = 0.01,
                               mutation_rate::Float64 = 0.1)
        n = length(surface.coefficients)
        velocity = zeros(Float64, n)
        history = JSurface[surface]
        new(surface, gradient_step_size, mutation_rate, velocity, history)
    end
end

"""
    compute_jsurface_metric(trees::Vector{RootedTree}) -> Matrix{Float64}

Compute the Riemannian metric tensor for a J-surface.

The metric is based on tree similarities and structural relationships.

# Arguments
- `trees::Vector{RootedTree}`: Trees defining the surface

# Returns
- `Matrix{Float64}`: Metric tensor

# Examples
```julia
trees = generate_all_trees(4)
metric = compute_jsurface_metric(trees)
```
"""
function compute_jsurface_metric(trees::Vector{RootedTree})
    n = length(trees)
    metric = zeros(Float64, n, n)
    
    # Diagonal elements: based on tree order and symmetry
    for i in 1:n
        o = RootedTrees.order(trees[i])
        s = RootedTrees.symmetry(trees[i])
        metric[i, i] = 1.0 / (o * s)
    end
    
    # Off-diagonal elements: based on tree similarity
    for i in 1:n
        for j in (i+1):n
            similarity = tree_similarity(trees[i], trees[j])
            metric[i, j] = 0.1 * similarity
            metric[j, i] = metric[i, j]
        end
    end
    
    return metric
end

"""
    compute_gradient(surface::JSurface, loss_function::Function) -> Vector{Float64}

Compute the gradient of a loss function on the J-surface.

Uses finite differences to approximate the gradient.

# Arguments
- `surface::JSurface`: Current position on surface
- `loss_function::Function`: Loss function to minimize (takes coefficients as input)

# Returns
- `Vector{Float64}`: Gradient vector

# Examples
```julia
loss(c) = sum(c.^2)
grad = compute_gradient(surface, loss)
```
"""
function compute_gradient(surface::JSurface, loss_function::Function)
    n = length(surface.coefficients)
    gradient = zeros(Float64, n)
    h = 1e-6  # Finite difference step
    
    # Current loss
    f0 = loss_function(surface.coefficients)
    
    # Compute partial derivatives
    for i in 1:n
        coeffs_plus = copy(surface.coefficients)
        coeffs_plus[i] += h
        f_plus = loss_function(coeffs_plus)
        
        gradient[i] = (f_plus - f0) / h
    end
    
    return gradient
end

"""
    gradient_step!(integrator::JSurfaceIntegrator, gradient::Vector{Float64})

Perform a gradient descent step on the J-surface.

Updates the position using the Riemannian gradient.

# Arguments
- `integrator::JSurfaceIntegrator`: Integrator to update
- `gradient::Vector{Float64}`: Gradient vector

# Examples
```julia
gradient_step!(integrator, grad)
```
"""
function gradient_step!(integrator::JSurfaceIntegrator, gradient::Vector{Float64})
    # Riemannian gradient descent
    # g_Riem = M^{-1} * g_Euclidean
    metric_inv = inv(integrator.surface.metric_tensor)
    riemannian_gradient = metric_inv * gradient
    
    # Update coefficients
    new_coeffs = integrator.surface.coefficients - integrator.gradient_step_size * riemannian_gradient
    
    # Create new surface
    new_surface = JSurface(integrator.surface.trees, new_coeffs)
    
    # Update integrator
    integrator.surface = new_surface
    push!(integrator.history, new_surface)
    
    return integrator
end

"""
    evolve_step!(integrator::JSurfaceIntegrator)

Perform an evolutionary mutation step on the J-surface.

Randomly perturbs coefficients according to the mutation rate.

# Arguments
- `integrator::JSurfaceIntegrator`: Integrator to update

# Examples
```julia
evolve_step!(integrator)
```
"""
function evolve_step!(integrator::JSurfaceIntegrator)
    n = length(integrator.surface.coefficients)
    new_coeffs = copy(integrator.surface.coefficients)
    
    # Mutate each coefficient with probability mutation_rate
    for i in 1:n
        if rand() < integrator.mutation_rate
            # Gaussian mutation
            noise = 0.1 * new_coeffs[i] * randn()
            new_coeffs[i] += noise
        end
    end
    
    # Create new surface
    new_surface = JSurface(integrator.surface.trees, new_coeffs)
    
    # Update integrator
    integrator.surface = new_surface
    push!(integrator.history, new_surface)
    
    return integrator
end

"""
    hybrid_step!(integrator::JSurfaceIntegrator, loss_function::Function)

Perform a hybrid gradient-evolution step on the J-surface.

Combines continuous gradient descent with discrete evolutionary jumps.

# Arguments
- `integrator::JSurfaceIntegrator`: Integrator to update
- `loss_function::Function`: Loss function to minimize

# Examples
```julia
loss(c) = sum(c.^2)
hybrid_step!(integrator, loss)
```
"""
function hybrid_step!(integrator::JSurfaceIntegrator, loss_function::Function)
    # Compute gradient
    gradient = compute_gradient(integrator.surface, loss_function)
    
    # Gradient descent step
    gradient_step!(integrator, gradient)
    
    # Evolutionary step with probability mutation_rate
    if rand() < integrator.mutation_rate
        evolve_step!(integrator)
    end
    
    return integrator
end

"""
    geodesic_distance(s1::JSurface, s2::JSurface) -> Float64

Compute the geodesic distance between two points on the J-surface.

Uses the Riemannian metric to compute the distance.

# Arguments
- `s1::JSurface`: First surface point
- `s2::JSurface`: Second surface point

# Returns
- `Float64`: Geodesic distance

# Examples
```julia
d = geodesic_distance(surface1, surface2)
```
"""
function geodesic_distance(s1::JSurface, s2::JSurface)
    @assert s1.trees == s2.trees "Surfaces must have same tree basis"
    
    # Difference vector
    Δ = s2.coefficients - s1.coefficients
    
    # Riemannian distance: sqrt(Δ^T M Δ)
    distance_sq = dot(Δ, s1.metric_tensor * Δ)
    
    return sqrt(max(0.0, distance_sq))
end

"""
    create_jsurface_from_genome(genome::BSeriesGenome) -> JSurface

Create a J-surface from a B-series genome.

# Arguments
- `genome::BSeriesGenome`: Genome to convert

# Returns
- `JSurface`: Corresponding J-surface

# Examples
```julia
surface = create_jsurface_from_genome(genome)
```
"""
function create_jsurface_from_genome(genome::BSeriesGenome)
    trees = collect(keys(genome.coefficients))
    coeffs = [genome.coefficients[t] for t in trees]
    return JSurface(trees, coeffs)
end

"""
    genome_from_jsurface(surface::JSurface) -> BSeriesGenome

Create a B-series genome from a J-surface.

# Arguments
- `surface::JSurface`: Surface to convert

# Returns
- `BSeriesGenome`: Corresponding genome

# Examples
```julia
genome = genome_from_jsurface(surface)
```
"""
function genome_from_jsurface(surface::JSurface)
    coefficients = OrderedDict{RootedTree, Float64}()
    
    for (i, tree) in enumerate(surface.trees)
        coefficients[tree] = surface.coefficients[i]
    end
    
    return BSeriesGenome(coefficients)
end

"""
    optimize_on_jsurface!(integrator::JSurfaceIntegrator,
                         loss_function::Function,
                         max_iterations::Int = 100,
                         tolerance::Float64 = 1e-6) -> JSurface

Optimize on the J-surface using hybrid gradient-evolution dynamics.

# Arguments
- `integrator::JSurfaceIntegrator`: Integrator to use
- `loss_function::Function`: Loss function to minimize
- `max_iterations::Int`: Maximum number of iterations
- `tolerance::Float64`: Convergence tolerance

# Returns
- `JSurface`: Optimized surface

# Examples
```julia
loss(c) = sum(c.^2)
optimized = optimize_on_jsurface!(integrator, loss, 100)
```
"""
function optimize_on_jsurface!(integrator::JSurfaceIntegrator,
                              loss_function::Function,
                              max_iterations::Int = 100,
                              tolerance::Float64 = 1e-6)
    prev_loss = loss_function(integrator.surface.coefficients)
    
    for iter in 1:max_iterations
        # Hybrid step
        hybrid_step!(integrator, loss_function)
        
        # Check convergence
        current_loss = loss_function(integrator.surface.coefficients)
        
        if abs(current_loss - prev_loss) < tolerance
            break
        end
        
        prev_loss = current_loss
    end
    
    return integrator.surface
end
