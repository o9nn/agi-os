"""
    JSurfaceReactor

The J-Surface Reactor Core unites gradient descent and evolution dynamics
through elementary differentials from rooted trees.

The J-surface is a geometric structure that combines:
- Symplectic/Poisson geometry for gradient flow
- Evolutionary operators for population dynamics
- Elementary differentials for B-series integration
"""
module JSurfaceReactor

using LinearAlgebra
using Random

export JSurface, JSurfaceState
export gradient_flow!, evolution_step!, symplectic_integrate!
export create_jsurface, evaluate_gradient, apply_evolution

"""
    JSurface

Represents the J-surface structure matrix that governs the unified dynamics.

Fields:
- `structure_matrix::Matrix{Float64}`: The J matrix (skew-symmetric for symplectic)
- `hamiltonian::Function`: Energy function H(ψ)
- `evolution_operators::Vector{Function}`: Genetic operators
- `dimension::Int`: Phase space dimension
"""
struct JSurface
    structure_matrix::Matrix{Float64}
    hamiltonian::Function
    evolution_operators::Vector{Function}
    dimension::Int
    
    function JSurface(dim::Int; symplectic::Bool=true)
        # Create skew-symmetric structure matrix for symplectic geometry
        J = if symplectic
            # Standard symplectic form: J = [0 I; -I 0]
            n = div(dim, 2)
            [zeros(n, n) I(n); -I(n) zeros(n, n)]
        else
            # General Poisson structure
            randn(dim, dim)
        end
        
        # Ensure skew-symmetry
        J = (J - J') / 2
        
        # Default quadratic Hamiltonian
        H(ψ) = 0.5 * dot(ψ, ψ)
        
        # Default evolution operators
        ops = Function[
            crossover_operator,
            mutation_operator,
            selection_operator
        ]
        
        new(J, H, ops, dim)
    end
end

"""
    JSurfaceState

Current state on the J-surface including position, velocity, and population.

Fields:
- `position::Vector{Float64}`: Current point in phase space
- `velocity::Vector{Float64}`: Current velocity (∂ψ/∂t)
- `population::Vector{Vector{Float64}}`: Population of candidate solutions
- `fitness::Vector{Float64}`: Fitness values
- `generation::Int`: Current generation number
"""
mutable struct JSurfaceState
    position::Vector{Float64}
    velocity::Vector{Float64}
    population::Vector{Vector{Float64}}
    fitness::Vector{Float64}
    generation::Int
    
    function JSurfaceState(dim::Int, pop_size::Int=10)
        pos = randn(dim)
        vel = zeros(dim)
        pop = [randn(dim) for _ in 1:pop_size]
        fit = zeros(pop_size)
        new(pos, vel, pop, fit, 0)
    end
end

"""
    create_jsurface(dim::Int; kwargs...)

Create a J-surface with specified dimension and properties.

# Arguments
- `dim::Int`: Dimension of the phase space (should be even for symplectic)
- `symplectic::Bool=true`: Use symplectic structure
- `hamiltonian::Function=nothing`: Custom Hamiltonian function

# Returns
- `JSurface`: Configured J-surface structure
"""
function create_jsurface(dim::Int; 
                        symplectic::Bool=true,
                        hamiltonian::Union{Function,Nothing}=nothing)
    surface = JSurface(dim; symplectic=symplectic)
    
    if !isnothing(hamiltonian)
        # Replace default Hamiltonian
        surface = JSurface(
            surface.structure_matrix,
            hamiltonian,
            surface.evolution_operators,
            surface.dimension
        )
    end
    
    return surface
end

"""
    evaluate_gradient(surface::JSurface, ψ::Vector{Float64})

Compute the gradient of the Hamiltonian at point ψ.

# Arguments
- `surface::JSurface`: The J-surface structure
- `ψ::Vector{Float64}`: Current position

# Returns
- `Vector{Float64}`: Gradient ∇H(ψ)
"""
function evaluate_gradient(surface::JSurface, ψ::Vector{Float64})
    # Numerical gradient using finite differences
    h = 1e-8
    grad = similar(ψ)
    
    for i in 1:length(ψ)
        ψ_plus = copy(ψ)
        ψ_minus = copy(ψ)
        ψ_plus[i] += h
        ψ_minus[i] -= h
        
        grad[i] = (surface.hamiltonian(ψ_plus) - surface.hamiltonian(ψ_minus)) / (2h)
    end
    
    return grad
end

"""
    gradient_flow!(surface::JSurface, state::JSurfaceState, dt::Float64)

Perform one step of gradient flow on the J-surface.

The dynamics follow: ∂ψ/∂t = J(ψ) · ∇H(ψ)

# Arguments
- `surface::JSurface`: The J-surface structure
- `state::JSurfaceState`: Current state (modified in-place)
- `dt::Float64`: Time step
"""
function gradient_flow!(surface::JSurface, state::JSurfaceState, dt::Float64)
    # Compute gradient
    grad = evaluate_gradient(surface, state.position)
    
    # Apply J-surface structure: v = J · ∇H
    state.velocity = surface.structure_matrix * grad
    
    # Update position: ψ_{n+1} = ψ_n + dt · v
    state.position += dt * state.velocity
    
    return nothing
end

"""
    symplectic_integrate!(surface::JSurface, state::JSurfaceState, dt::Float64)

Perform symplectic integration preserving the geometric structure.

Uses the Störmer-Verlet method for symplectic systems.

# Arguments
- `surface::JSurface`: The J-surface structure
- `state::JSurfaceState`: Current state (modified in-place)
- `dt::Float64`: Time step
"""
function symplectic_integrate!(surface::JSurface, state::JSurfaceState, dt::Float64)
    n = div(surface.dimension, 2)
    
    # Split position into q and p
    q = state.position[1:n]
    p = state.position[n+1:end]
    
    # Störmer-Verlet integration
    # Half step for p
    grad_q = evaluate_gradient(surface, state.position)[1:n]
    p_half = p - (dt/2) * grad_q
    
    # Full step for q
    state.position[n+1:end] = p_half
    grad_p = evaluate_gradient(surface, state.position)[n+1:end]
    q_new = q + dt * grad_p
    
    # Half step for p
    state.position[1:n] = q_new
    grad_q_new = evaluate_gradient(surface, state.position)[1:n]
    p_new = p_half - (dt/2) * grad_q_new
    
    # Update state
    state.position[1:n] = q_new
    state.position[n+1:end] = p_new
    
    # Update velocity
    state.velocity = (state.position - [q; p]) / dt
    
    return nothing
end

"""
    evolution_step!(surface::JSurface, state::JSurfaceState; 
                   mutation_rate::Float64=0.1,
                   crossover_rate::Float64=0.7)

Perform one evolutionary step on the population.

# Arguments
- `surface::JSurface`: The J-surface structure
- `state::JSurfaceState`: Current state (modified in-place)
- `mutation_rate::Float64=0.1`: Probability of mutation
- `crossover_rate::Float64=0.7`: Probability of crossover
"""
function evolution_step!(surface::JSurface, state::JSurfaceState;
                        mutation_rate::Float64=0.1,
                        crossover_rate::Float64=0.7)
    pop_size = length(state.population)
    
    # Evaluate fitness
    for i in 1:pop_size
        state.fitness[i] = -surface.hamiltonian(state.population[i])
    end
    
    # Selection: tournament selection
    new_population = Vector{Vector{Float64}}()
    
    for _ in 1:pop_size
        # Tournament
        i1, i2 = rand(1:pop_size, 2)
        parent1 = state.fitness[i1] > state.fitness[i2] ? 
                  state.population[i1] : state.population[i2]
        
        i3, i4 = rand(1:pop_size, 2)
        parent2 = state.fitness[i3] > state.fitness[i4] ? 
                  state.population[i3] : state.population[i4]
        
        # Crossover
        offspring = if rand() < crossover_rate
            crossover_operator(parent1, parent2)
        else
            copy(parent1)
        end
        
        # Mutation
        if rand() < mutation_rate
            offspring = mutation_operator(offspring)
        end
        
        push!(new_population, offspring)
    end
    
    state.population = new_population
    state.generation += 1
    
    # Update position to best individual
    best_idx = argmax(state.fitness)
    state.position = state.population[best_idx]
    
    return nothing
end

"""
    apply_evolution(surface::JSurface, state::JSurfaceState, steps::Int; kwargs...)

Apply multiple evolution steps.

# Arguments
- `surface::JSurface`: The J-surface structure
- `state::JSurfaceState`: Current state (modified in-place)
- `steps::Int`: Number of evolution steps
- `kwargs...`: Additional parameters for evolution_step!

# Returns
- `Vector{Float64}`: History of best fitness values
"""
function apply_evolution(surface::JSurface, state::JSurfaceState, steps::Int; kwargs...)
    fitness_history = Float64[]
    
    for _ in 1:steps
        evolution_step!(surface, state; kwargs...)
        push!(fitness_history, maximum(state.fitness))
    end
    
    return fitness_history
end

# Evolution operators

"""
    crossover_operator(parent1::Vector{Float64}, parent2::Vector{Float64})

Single-point crossover between two parents.
"""
function crossover_operator(parent1::Vector{Float64}, parent2::Vector{Float64})
    n = length(parent1)
    point = rand(1:n-1)
    
    offspring = similar(parent1)
    offspring[1:point] = parent1[1:point]
    offspring[point+1:end] = parent2[point+1:end]
    
    return offspring
end

"""
    mutation_operator(individual::Vector{Float64}; strength::Float64=0.1)

Gaussian mutation of an individual.
"""
function mutation_operator(individual::Vector{Float64}; strength::Float64=0.1)
    mutated = copy(individual)
    n = length(individual)
    
    # Mutate one random gene
    idx = rand(1:n)
    mutated[idx] += strength * randn()
    
    return mutated
end

"""
    selection_operator(population::Vector{Vector{Float64}}, 
                      fitness::Vector{Float64}, 
                      k::Int=2)

Tournament selection of k individuals.
"""
function selection_operator(population::Vector{Vector{Float64}}, 
                           fitness::Vector{Float64}, 
                           k::Int=2)
    indices = rand(1:length(population), k)
    best_idx = indices[argmax(fitness[indices])]
    return population[best_idx]
end

end # module JSurfaceReactor
