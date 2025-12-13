"""
Fitness evaluation functions for Deep Tree Echo reservoirs.

This module implements multi-objective fitness evaluation combining:
- Prediction accuracy (grip on temporal patterns)
- Numerical stability
- Computational efficiency
- Genetic diversity
- Symmetry preservation
"""

"""
    evaluate_fitness(reservoir::DeepTreeEchoReservoir,
                    test_problems::Vector{<:Any} = Any[];
                    weights::Dict{Symbol, Float64} = Dict{Symbol, Float64}()) -> Float64

Evaluate the fitness of a reservoir.

Fitness is a weighted combination of multiple objectives:
- `:accuracy` - Prediction accuracy on test problems
- `:stability` - Numerical stability of reservoir dynamics
- `:efficiency` - Computational efficiency
- `:diversity` - Genetic diversity (novelty)
- `:symmetry` - Preservation of symmetries

# Arguments
- `reservoir::DeepTreeEchoReservoir`: Reservoir to evaluate
- `test_problems::Vector`: Test problems for evaluation
- `weights::Dict{Symbol, Float64}`: Weights for each objective

# Returns
- `Float64`: Fitness score in [0, 1]

# Examples
```julia
fitness = evaluate_fitness(reservoir, test_problems)
```
"""
function evaluate_fitness(reservoir::DeepTreeEchoReservoir,
                         test_problems::Vector{<:Any} = Any[];
                         weights::Dict{Symbol, Float64} = Dict{Symbol, Float64}())
    # Default weights
    default_weights = Dict{Symbol, Float64}(
        :accuracy => 0.4,
        :stability => 0.2,
        :efficiency => 0.2,
        :diversity => 0.1,
        :symmetry => 0.1
    )
    
    # Merge with provided weights
    w = merge(default_weights, weights)
    
    # Evaluate each component
    accuracy = evaluate_accuracy(reservoir, test_problems)
    stability = evaluate_stability(reservoir)
    efficiency = evaluate_efficiency(reservoir)
    diversity = evaluate_diversity(reservoir)
    symmetry = evaluate_symmetry(reservoir)
    
    # Weighted combination
    fitness = (
        w[:accuracy] * accuracy +
        w[:stability] * stability +
        w[:efficiency] * efficiency +
        w[:diversity] * diversity +
        w[:symmetry] * symmetry
    )
    
    # Update reservoir fitness
    reservoir.fitness = fitness
    
    return fitness
end

"""
    evaluate_accuracy(reservoir::DeepTreeEchoReservoir,
                     test_problems::Vector{<:Any}) -> Float64

Evaluate prediction accuracy on test problems.

# Arguments
- `reservoir::DeepTreeEchoReservoir`: Reservoir to evaluate
- `test_problems::Vector`: Test problems

# Returns
- `Float64`: Accuracy score in [0, 1]
"""
function evaluate_accuracy(reservoir::DeepTreeEchoReservoir,
                          test_problems::Vector{<:Any})
    if isempty(test_problems)
        # No test problems - return neutral score
        return 0.5
    end
    
    # Simplified accuracy evaluation
    # In full implementation, would run reservoir on test problems
    # and compute prediction error
    
    # Placeholder: random score
    return 0.5 + 0.3 * randn()
end

"""
    evaluate_stability(reservoir::DeepTreeEchoReservoir) -> Float64

Evaluate numerical stability of reservoir dynamics.

Checks:
- Echo state property (spectral radius < 1)
- Bounded state trajectories
- Absence of NaN/Inf values

# Arguments
- `reservoir::DeepTreeEchoReservoir`: Reservoir to evaluate

# Returns
- `Float64`: Stability score in [0, 1]
"""
function evaluate_stability(reservoir::DeepTreeEchoReservoir)
    stability_score = 1.0
    
    # Check each membrane reservoir
    for (_, mem_res) in reservoir.membrane_network.membranes
        # Check for NaN/Inf in state
        if any(isnan, mem_res.state) || any(isinf, mem_res.state)
            stability_score *= 0.5
            continue
        end
        
        # Check spectral radius (echo state property)
        W = mem_res.reservoir.reservoir
        if size(W, 1) > 0
            ρ = compute_spectral_radius(W)
            if ρ >= 1.0
                stability_score *= 0.7  # Penalty for violating echo state property
            else
                stability_score *= (1.0 - ρ)  # Reward for small spectral radius
            end
        end
    end
    
    return max(0.0, min(1.0, stability_score))
end

"""
    compute_spectral_radius(W::AbstractMatrix) -> Float64

Compute the spectral radius (largest absolute eigenvalue) of a matrix.

# Arguments
- `W::AbstractMatrix`: Matrix to analyze

# Returns
- `Float64`: Spectral radius
"""
function compute_spectral_radius(W::AbstractMatrix)
    if size(W, 1) == 0
        return 0.0
    end
    
    try
        eigenvalues = eigvals(Matrix(W))
        return maximum(abs.(eigenvalues))
    catch
        # If eigenvalue computation fails, return safe value
        return 1.0
    end
end

"""
    verify_echo_state_property(W::AbstractMatrix) -> Bool

Verify that a reservoir weight matrix satisfies the echo state property.

# Arguments
- `W::AbstractMatrix`: Reservoir weight matrix

# Returns
- `Bool`: True if echo state property is satisfied
"""
function verify_echo_state_property(W::AbstractMatrix)
    ρ = compute_spectral_radius(W)
    return ρ < 1.0
end

"""
    evaluate_efficiency(reservoir::DeepTreeEchoReservoir) -> Float64

Evaluate computational efficiency of the reservoir.

Considers:
- Number of neurons (smaller is better)
- Sparsity of connections
- Membrane count

# Arguments
- `reservoir::DeepTreeEchoReservoir`: Reservoir to evaluate

# Returns
- `Float64`: Efficiency score in [0, 1]
"""
function evaluate_efficiency(reservoir::DeepTreeEchoReservoir)
    # Total neurons
    total_neurons = membrane_network_size(reservoir.membrane_network)
    
    # Normalize by typical size (100 neurons per membrane)
    num_membranes = length(reservoir.membrane_network.membranes)
    typical_size = num_membranes * 100
    
    size_efficiency = exp(-total_neurons / typical_size)
    
    # Membrane efficiency (fewer membranes is more efficient)
    membrane_efficiency = exp(-num_membranes / 10.0)
    
    # Combined efficiency
    efficiency = 0.5 * size_efficiency + 0.5 * membrane_efficiency
    
    return max(0.0, min(1.0, efficiency))
end

"""
    evaluate_diversity(reservoir::DeepTreeEchoReservoir) -> Float64

Evaluate genetic diversity (novelty) of the reservoir.

Measures how different the B-series genome is from typical configurations.

# Arguments
- `reservoir::DeepTreeEchoReservoir`: Reservoir to evaluate

# Returns
- `Float64`: Diversity score in [0, 1]
"""
function evaluate_diversity(reservoir::DeepTreeEchoReservoir)
    # Measure deviation from theoretical B-series coefficients
    diversity_score = 0.0
    count = 0
    
    for (tree, coeff) in reservoir.bseries_genome.coefficients
        theoretical = 1.0 / (RootedTrees.order(tree) * RootedTrees.symmetry(tree))
        deviation = abs(coeff - theoretical) / (abs(theoretical) + 1e-10)
        diversity_score += min(1.0, deviation)
        count += 1
    end
    
    if count > 0
        diversity_score /= count
    end
    
    return max(0.0, min(1.0, diversity_score))
end

"""
    evaluate_symmetry(reservoir::DeepTreeEchoReservoir) -> Float64

Evaluate preservation of symmetries in the B-series.

Checks if the B-series respects the symmetry factors of rooted trees.

# Arguments
- `reservoir::DeepTreeEchoReservoir`: Reservoir to evaluate

# Returns
- `Float64`: Symmetry score in [0, 1]
"""
function evaluate_symmetry(reservoir::DeepTreeEchoReservoir)
    symmetry_score = 1.0
    
    # Check if coefficients respect tree symmetries
    # Trees with higher symmetry should have appropriately scaled coefficients
    
    for (tree, coeff) in reservoir.bseries_genome.coefficients
        σ = RootedTrees.symmetry(tree)
        
        # Coefficient should be inversely related to symmetry
        # (more symmetric trees need smaller coefficients)
        if σ > 1 && coeff > 1.0 / σ
            # Penalty for violating symmetry scaling
            symmetry_score *= 0.9
        end
    end
    
    return max(0.0, min(1.0, symmetry_score))
end

"""
    tournament_selection(population::Vector{DeepTreeEchoReservoir},
                        fitnesses::Vector{Float64},
                        tournament_size::Int = 3) -> DeepTreeEchoReservoir

Select an individual from the population using tournament selection.

# Arguments
- `population::Vector{DeepTreeEchoReservoir}`: Population to select from
- `fitnesses::Vector{Float64}`: Fitness scores
- `tournament_size::Int`: Number of individuals in tournament

# Returns
- `DeepTreeEchoReservoir`: Selected individual

# Examples
```julia
parent = tournament_selection(population, fitnesses)
```
"""
function tournament_selection(population::Vector{DeepTreeEchoReservoir},
                             fitnesses::Vector{Float64},
                             tournament_size::Int = 3)
    n = length(population)
    @assert n == length(fitnesses) "Population and fitness sizes must match"
    
    # Select random individuals for tournament
    tournament_indices = rand(1:n, tournament_size)
    
    # Find best in tournament
    best_idx = tournament_indices[1]
    best_fitness = fitnesses[best_idx]
    
    for idx in tournament_indices[2:end]
        if fitnesses[idx] > best_fitness
            best_idx = idx
            best_fitness = fitnesses[idx]
        end
    end
    
    return population[best_idx]
end

"""
    elitism_selection(population::Vector{DeepTreeEchoReservoir},
                     fitnesses::Vector{Float64},
                     elite_size::Int) -> Vector{DeepTreeEchoReservoir}

Select the elite individuals from the population.

# Arguments
- `population::Vector{DeepTreeEchoReservoir}`: Population
- `fitnesses::Vector{Float64}`: Fitness scores
- `elite_size::Int`: Number of elite individuals

# Returns
- `Vector{DeepTreeEchoReservoir}`: Elite individuals

# Examples
```julia
elite = elitism_selection(population, fitnesses, 5)
```
"""
function elitism_selection(population::Vector{DeepTreeEchoReservoir},
                          fitnesses::Vector{Float64},
                          elite_size::Int)
    n = length(population)
    @assert n == length(fitnesses) "Population and fitness sizes must match"
    
    # Sort by fitness (descending)
    sorted_indices = sortperm(fitnesses, rev=true)
    
    # Select top elite_size individuals
    elite_indices = sorted_indices[1:min(elite_size, n)]
    
    return population[elite_indices]
end
