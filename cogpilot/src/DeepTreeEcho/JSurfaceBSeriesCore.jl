"""
    JSurfaceBSeriesCore

The reactor core that unites B-series ridges with J-surface elementary differentials.
This module implements the deep mathematical connection where:

1. **J-Surface Geometry**: Symplectic/Poisson manifolds for gradient flow
2. **B-Series Ridges**: Computational ridges in coefficient space
3. **Elementary Differentials**: Tree-indexed operators F(τ)
4. **Unified Dynamics**: ∂ψ/∂t = J(ψ)·∇H(ψ) + Σ b(τ)/σ(τ)·F(τ)(ψ)

# Mathematical Foundation

The J-surface provides the continuous geometric structure:
    
    ∂ψ/∂t = J(ψ) · ∇H(ψ)

Where J(ψ) is either:
- **Symplectic**: J = [0 I; -I 0] (energy-preserving)
- **Poisson**: J(ψ) depends on state (general structure)

The B-series provides the discrete approximation via elementary differentials:

    ψ_{n+1} = ψ_n + h Σ_{τ ∈ T} b(τ)/σ(τ) · F(τ)(ψ_n)

Where:
- T is the set of rooted trees from A000081
- b(τ) are ridge coefficients
- σ(τ) are symmetry factors
- F(τ) are elementary differentials

# The Reactor Core

The reactor core unifies these by treating the J-surface as the continuous
limit of the B-series as h → 0, and the B-series as the discrete realization
of the J-surface flow.
"""
module JSurfaceBSeriesCore

using LinearAlgebra
using Random

export JSurfaceBSeriesReactor
export create_reactor, reactor_step!, reactor_flow!
export compute_ridge_gradient, compute_jsurface_flow
export unite_continuous_discrete, get_reactor_status

"""
    JSurfaceBSeriesReactor

The core reactor that unites J-surface geometry with B-series ridges.

# Fields
- `dimension::Int`: State space dimension
- `jsurface_matrix::Matrix{Float64}`: J-surface structure matrix J(ψ)
- `hamiltonian::Function`: Energy function H(ψ)
- `trees::Vector{Vector{Int}}`: Rooted trees from A000081
- `ridge_coefficients::Vector{Float64}`: B-series coefficients b(τ)
- `symmetry_factors::Vector{Int}`: Tree symmetry factors σ(τ)
- `differentials::Vector{Function}`: Elementary differentials F(τ)
- `state::Vector{Float64}`: Current state ψ
- `energy::Float64`: Current energy H(ψ)
- `gradient::Vector{Float64}`: Current gradient ∇H(ψ)
- `flow_velocity::Vector{Float64}`: J(ψ)·∇H(ψ)
- `symplectic::Bool`: Whether J is symplectic
"""
mutable struct JSurfaceBSeriesReactor
    dimension::Int
    jsurface_matrix::Matrix{Float64}
    hamiltonian::Function
    trees::Vector{Vector{Int}}
    ridge_coefficients::Vector{Float64}
    symmetry_factors::Vector{Int}
    differentials::Vector{Function}
    state::Vector{Float64}
    energy::Float64
    gradient::Vector{Float64}
    flow_velocity::Vector{Float64}
    symplectic::Bool
    
    function JSurfaceBSeriesReactor(
        dimension::Int,
        trees::Vector{Vector{Int}},
        ridge_coefficients::Vector{Float64};
        symplectic::Bool=true,
        hamiltonian::Union{Function, Nothing}=nothing
    )
        # Create J-surface matrix
        J = create_jsurface_matrix(dimension, symplectic)
        
        # Default Hamiltonian (quadratic)
        if hamiltonian === nothing
            hamiltonian = ψ -> 0.5 * dot(ψ, ψ)
        end
        
        # Compute symmetry factors
        symmetry_factors = [compute_tree_symmetry(tree) for tree in trees]
        
        # Create elementary differential operators
        differentials = [create_elementary_differential(tree) for tree in trees]
        
        # Initialize state
        state = randn(dimension)
        energy = hamiltonian(state)
        gradient = compute_gradient(hamiltonian, state)
        flow_velocity = J * gradient
        
        new(dimension, J, hamiltonian, trees, ridge_coefficients,
            symmetry_factors, differentials, state, energy, gradient,
            flow_velocity, symplectic)
    end
end

"""
    create_jsurface_matrix(dimension::Int, symplectic::Bool)

Create the J-surface structure matrix.
"""
function create_jsurface_matrix(dimension::Int, symplectic::Bool)
    if symplectic
        # Symplectic structure: J = [0 I; -I 0]
        half_dim = dimension ÷ 2
        J = zeros(dimension, dimension)
        J[1:half_dim, (half_dim+1):end] = I(half_dim)
        J[(half_dim+1):end, 1:half_dim] = -I(half_dim)
        return J
    else
        # General Poisson structure (skew-symmetric)
        J = randn(dimension, dimension)
        J = J - J'  # Make skew-symmetric
        return J * 0.1
    end
end

"""
    compute_tree_symmetry(tree::Vector{Int})

Compute symmetry factor σ(τ) of a rooted tree.
"""
function compute_tree_symmetry(tree::Vector{Int})
    if isempty(tree)
        return 1
    end
    
    n = length(tree)
    
    # Simple cases
    if n == 1
        return 1
    elseif n == 2
        return 1
    elseif tree == [1, 2, 3]
        return 1  # Linear
    elseif tree == [1, 2, 2]
        return 2  # Two branches
    elseif tree == [1, 2, 2, 2]
        return 6  # Three branches (3!)
    elseif tree == [1, 2, 2, 2, 2]
        return 24  # Four branches (4!)
    else
        # Count children at each level
        level_counts = Dict{Int, Int}()
        for level in tree
            level_counts[level] = get(level_counts, level, 0) + 1
        end
        
        # Symmetry from repeated children
        symmetry = 1
        for (level, count) in level_counts
            if count > 1 && level > 1
                # Factorial for repeated children at same parent
                symmetry *= factorial(min(count, 10))  # Cap at 10! for safety
            end
        end
        
        return symmetry
    end
end

"""
    create_elementary_differential(tree::Vector{Int})

Create elementary differential operator F(τ) for tree τ.
"""
function create_elementary_differential(tree::Vector{Int})
    order = length(tree)
    
    function F_tau(f::Function, ψ::Vector{Float64})
        if order == 1
            # F(•)(ψ) = f(ψ)
            return f(ψ)
        elseif order == 2
            # F(••)(ψ) = f'(ψ)[f(ψ)]
            fψ = f(ψ)
            return directional_derivative(f, ψ, fψ)
        elseif order == 3
            # F(τ)(ψ) depends on tree structure
            fψ = f(ψ)
            if tree == [1, 2, 3]
                # Linear: f''(ψ)[f(ψ), f(ψ)]
                dfψ = directional_derivative(f, ψ, fψ)
                return directional_derivative(x -> directional_derivative(f, x, f(x)), ψ, fψ)
            elseif tree == [1, 2, 2]
                # Branched: f'(ψ)[f'(ψ)[f(ψ)]]
                dfψ = directional_derivative(f, ψ, fψ)
                return directional_derivative(f, ψ, dfψ)
            else
                # Default
                dfψ = directional_derivative(f, ψ, fψ)
                return directional_derivative(f, ψ, dfψ)
            end
        else
            # Higher order: recursive application
            fψ = f(ψ)
            result = fψ
            for i in 2:order
                result = directional_derivative(f, ψ, result)
            end
            return result
        end
    end
    
    return F_tau
end

"""
    directional_derivative(f::Function, ψ::Vector{Float64}, v::Vector{Float64})

Compute directional derivative f'(ψ)[v] using finite differences.
"""
function directional_derivative(f::Function, ψ::Vector{Float64}, v::Vector{Float64})
    ε = 1e-6
    v_normalized = v / (norm(v) + 1e-10)
    return (f(ψ + ε * v_normalized) - f(ψ - ε * v_normalized)) / (2ε)
end

"""
    compute_gradient(H::Function, ψ::Vector{Float64})

Compute gradient ∇H(ψ) using finite differences.
"""
function compute_gradient(H::Function, ψ::Vector{Float64})
    n = length(ψ)
    grad = zeros(n)
    ε = 1e-6
    
    for i in 1:n
        ψ_plus = copy(ψ)
        ψ_minus = copy(ψ)
        ψ_plus[i] += ε
        ψ_minus[i] -= ε
        grad[i] = (H(ψ_plus) - H(ψ_minus)) / (2ε)
    end
    
    return grad
end

"""
    create_reactor(dimension::Int, trees::Vector{Vector{Int}}, 
                  ridge_coefficients::Vector{Float64}; kwargs...)

Create a J-surface B-series reactor.
"""
function create_reactor(
    dimension::Int,
    trees::Vector{Vector{Int}},
    ridge_coefficients::Vector{Float64};
    symplectic::Bool=true,
    hamiltonian::Union{Function, Nothing}=nothing
)
    return JSurfaceBSeriesReactor(
        dimension, trees, ridge_coefficients;
        symplectic=symplectic, hamiltonian=hamiltonian
    )
end

"""
    compute_jsurface_flow(reactor::JSurfaceBSeriesReactor)

Compute J-surface flow: J(ψ)·∇H(ψ)
"""
function compute_jsurface_flow(reactor::JSurfaceBSeriesReactor)
    reactor.gradient = compute_gradient(reactor.hamiltonian, reactor.state)
    reactor.flow_velocity = reactor.jsurface_matrix * reactor.gradient
    return reactor.flow_velocity
end

"""
    compute_ridge_gradient(reactor::JSurfaceBSeriesReactor, f::Function)

Compute B-series ridge contribution: Σ b(τ)/σ(τ)·F(τ)(ψ)
"""
function compute_ridge_gradient(reactor::JSurfaceBSeriesReactor, f::Function)
    increment = zeros(reactor.dimension)
    
    for i in 1:length(reactor.trees)
        b_tau = reactor.ridge_coefficients[i]
        sigma_tau = reactor.symmetry_factors[i]
        F_tau = reactor.differentials[i]
        
        # Compute b(τ)/σ(τ) · F(τ)(ψ)
        contribution = (b_tau / sigma_tau) * F_tau(f, reactor.state)
        increment += contribution
    end
    
    return increment
end

"""
    unite_continuous_discrete(reactor::JSurfaceBSeriesReactor, f::Function, h::Float64)

Unite continuous J-surface flow with discrete B-series step:
    
    ψ_{n+1} = ψ_n + h · [J(ψ)·∇H(ψ) + Σ b(τ)/σ(τ)·F(τ)(ψ)]

Returns the combined increment.
"""
function unite_continuous_discrete(reactor::JSurfaceBSeriesReactor, f::Function, h::Float64)
    # Continuous part: J(ψ)·∇H(ψ)
    continuous_flow = compute_jsurface_flow(reactor)
    
    # Discrete part: Σ b(τ)/σ(τ)·F(τ)(ψ)
    discrete_increment = compute_ridge_gradient(reactor, f)
    
    # Unite
    total_increment = continuous_flow + discrete_increment
    
    return h * total_increment
end

"""
    reactor_step!(reactor::JSurfaceBSeriesReactor, f::Function, h::Float64)

Perform one reactor step, updating the state.
"""
function reactor_step!(reactor::JSurfaceBSeriesReactor, f::Function, h::Float64)
    # Compute unified increment
    increment = unite_continuous_discrete(reactor, f, h)
    
    # Update state
    reactor.state += increment
    
    # Update energy
    reactor.energy = reactor.hamiltonian(reactor.state)
    
    return reactor.state
end

"""
    reactor_flow!(reactor::JSurfaceBSeriesReactor, f::Function, 
                 num_steps::Int, h::Float64)

Evolve reactor for multiple steps.
"""
function reactor_flow!(reactor::JSurfaceBSeriesReactor, f::Function, 
                      num_steps::Int, h::Float64)
    energy_history = Float64[reactor.energy]
    
    for step in 1:num_steps
        reactor_step!(reactor, f, h)
        push!(energy_history, reactor.energy)
    end
    
    return energy_history
end

"""
    get_reactor_status(reactor::JSurfaceBSeriesReactor)

Get current reactor status.
"""
function get_reactor_status(reactor::JSurfaceBSeriesReactor)
    return Dict(
        "dimension" => reactor.dimension,
        "num_trees" => length(reactor.trees),
        "energy" => reactor.energy,
        "gradient_norm" => norm(reactor.gradient),
        "flow_norm" => norm(reactor.flow_velocity),
        "state_norm" => norm(reactor.state),
        "symplectic" => reactor.symplectic
    )
end

end  # module JSurfaceBSeriesCore
