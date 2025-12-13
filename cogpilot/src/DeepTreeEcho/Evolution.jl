"""
Evolutionary algorithms for Deep Tree Echo reservoir populations.

This module implements genetic algorithms for evolving populations of
reservoir computers through selection, crossover, mutation, and adaptation.
"""

"""
    ReservoirPopulation

Population of Deep Tree Echo reservoirs for evolutionary optimization.

# Fields
- `individuals::Vector{DeepTreeEchoReservoir}`: Population members
- `generation::Int`: Current generation number
- `elite_size::Int`: Number of elite individuals to preserve
- `mutation_rate::Float64`: Probability of mutation
- `crossover_rate::Float64`: Probability of crossover
- `best_fitness_history::Vector{Float64}`: Best fitness per generation
- `avg_fitness_history::Vector{Float64}`: Average fitness per generation
"""
mutable struct ReservoirPopulation
    individuals::Vector{DeepTreeEchoReservoir}
    generation::Int
    elite_size::Int
    mutation_rate::Float64
    crossover_rate::Float64
    best_fitness_history::Vector{Float64}
    avg_fitness_history::Vector{Float64}
    
    function ReservoirPopulation(
        individuals::Vector{DeepTreeEchoReservoir},
        elite_size::Int = 2,
        mutation_rate::Float64 = 0.1,
        crossover_rate::Float64 = 0.7
    )
        new(individuals, 0, elite_size, mutation_rate, crossover_rate,
            Float64[], Float64[])
    end
end

"""
    initialize_reservoir_population(;
        population_size::Union{Int,Nothing} = nothing,
        order::Union{Int,Nothing} = nothing,
        membrane_depth::Union{Int,Nothing} = nothing,
        reservoir_size::Union{Int,Nothing} = nothing,
        base_order::Int = 5
    ) -> ReservoirPopulation

Initialize a population of Deep Tree Echo reservoirs with A000081-aligned parameters.

# Arguments
- `population_size::Union{Int,Nothing}`: Number of individuals (auto-derived if nothing)
- `order::Union{Int,Nothing}`: B-series order (auto-derived if nothing)
- `membrane_depth::Union{Int,Nothing}`: Membrane hierarchy depth (auto-derived if nothing)
- `reservoir_size::Union{Int,Nothing}`: Reservoir size per membrane (auto-derived if nothing)
- `base_order::Int`: Base order for A000081 derivation (default: 5)

# Returns
- `ReservoirPopulation`: Initialized population with A000081-aligned parameters

# Examples
```julia
# Auto-derive all parameters
population = initialize_reservoir_population(base_order=5)

# Specify population size, auto-derive others
population = initialize_reservoir_population(
    population_size = 9,  # A000081[5]
    base_order = 5
)
```
"""
function initialize_reservoir_population(;
    population_size::Union{Int,Nothing} = nothing,
    order::Union{Int,Nothing} = nothing,
    membrane_depth::Union{Int,Nothing} = nothing,
    reservoir_size::Union{Int,Nothing} = nothing,
    base_order::Int = 5
)
    # Load parameter module
    include("A000081Parameters.jl")
    using .A000081Parameters
    
    # Derive population size from A000081[4] = 4 if not provided
    population_size = isnothing(population_size) ? A000081Parameters.A000081_SEQUENCE[4] : population_size
    
    individuals = [
        initialize_deep_tree_echo(
            order = order,
            membrane_depth = membrane_depth,
            reservoir_size = reservoir_size,
            base_order = base_order
        )
        for _ in 1:population_size
    ]
    
    return ReservoirPopulation(individuals)
end

"""
    evolve_generation!(population::ReservoirPopulation,
                      fitness_function::Function;
                      test_problems::Vector{<:Any} = Any[])

Evolve the population through one generation.

Process:
1. Evaluate fitness of all individuals
2. Select elite individuals
3. Tournament selection for parents
4. Crossover and mutation to create offspring
5. Replace population with elite + offspring

# Arguments
- `population::ReservoirPopulation`: Population to evolve
- `fitness_function::Function`: Fitness evaluation function
- `test_problems::Vector`: Test problems for fitness evaluation

# Examples
```julia
evolve_generation!(population, evaluate_fitness)
```
"""
function evolve_generation!(population::ReservoirPopulation,
                           fitness_function::Function;
                           test_problems::Vector{<:Any} = Any[])
    n = length(population.individuals)
    
    # 1. Evaluate fitness
    fitnesses = [
        fitness_function(ind, test_problems)
        for ind in population.individuals
    ]
    
    # Record statistics
    push!(population.best_fitness_history, maximum(fitnesses))
    push!(population.avg_fitness_history, mean(fitnesses))
    
    # 2. Select elite
    elite = elitism_selection(
        population.individuals,
        fitnesses,
        population.elite_size
    )
    
    # 3. Create offspring through selection, crossover, and mutation
    offspring = DeepTreeEchoReservoir[]
    
    while length(offspring) < n - length(elite)
        # Tournament selection for parents
        parent1 = tournament_selection(population.individuals, fitnesses)
        parent2 = tournament_selection(population.individuals, fitnesses)
        
        # Crossover
        if rand() < population.crossover_rate
            child = crossover_reservoirs(parent1, parent2)
        else
            child = clone_reservoir(parent1)
        end
        
        # Mutation
        if rand() < population.mutation_rate
            mutate_reservoir!(child)
        end
        
        push!(offspring, child)
    end
    
    # 4. Replace population
    population.individuals = vcat(elite, offspring)
    population.generation += 1
    
    return population
end

"""
    crossover_reservoirs(parent1::DeepTreeEchoReservoir,
                        parent2::DeepTreeEchoReservoir) -> DeepTreeEchoReservoir

Create offspring reservoir through genetic crossover.

Combines:
- B-series genomes via coefficient crossover
- Membrane topologies (inherits from better parent)
- Reservoir structures (hybrid)

# Arguments
- `parent1::DeepTreeEchoReservoir`: First parent
- `parent2::DeepTreeEchoReservoir`: Second parent

# Returns
- `DeepTreeEchoReservoir`: Offspring reservoir

# Examples
```julia
child = crossover_reservoirs(parent1, parent2)
```
"""
function crossover_reservoirs(parent1::DeepTreeEchoReservoir,
                             parent2::DeepTreeEchoReservoir)
    # Crossover B-series genomes
    child_genome = crossover(parent1.bseries_genome, parent2.bseries_genome)
    
    # Inherit rooted trees from parent1 (they should be the same)
    child_trees = copy(parent1.rooted_trees)
    
    # Inherit membrane network from better parent (simplified)
    if parent1.fitness >= parent2.fitness
        parent_network = parent1.membrane_network
    else
        parent_network = parent2.membrane_network
    end
    
    # Create new membrane network with same structure
    psystem = parent_network.topology
    reservoir_sizes = Dict(
        id => length(mem_res.state)
        for (id, mem_res) in parent_network.membranes
    )
    child_membrane_network = create_membrane_network(psystem, reservoir_sizes)
    
    # Create J-surface integrator
    child_jsurface = create_jsurface_from_genome(child_genome)
    child_integrator = JSurfaceIntegrator(child_jsurface)
    
    # Create offspring
    child = DeepTreeEchoReservoir(
        child_genome,
        child_trees,
        child_membrane_network,
        child_integrator,
        max(parent1.generation, parent2.generation) + 1,
        [parent1.id, parent2.id]
    )
    
    return child
end

"""
    mutate_reservoir!(reservoir::DeepTreeEchoReservoir)

Mutate a reservoir in place.

Applies mutations to:
- B-series coefficients
- Membrane topology (rare)
- Reservoir weights (rare)

# Arguments
- `reservoir::DeepTreeEchoReservoir`: Reservoir to mutate

# Examples
```julia
mutate_reservoir!(reservoir)
```
"""
function mutate_reservoir!(reservoir::DeepTreeEchoReservoir)
    # Mutate B-series genome
    mutate!(reservoir.bseries_genome, 0.1)
    
    # Update J-surface
    reservoir.jsurface_integrator.surface = create_jsurface_from_genome(
        reservoir.bseries_genome
    )
    
    # Occasionally mutate membrane topology (10% chance)
    if rand() < 0.1
        # Add random performance metrics to trigger adaptation
        performance_metrics = Dict(
            id => 0.5 + 0.3 * randn()
            for id in keys(reservoir.membrane_network.membranes)
        )
        adapt_topology!(reservoir.membrane_network, performance_metrics)
    end
    
    return reservoir
end

"""
    run_evolution(;
        population_size::Union{Int,Nothing} = nothing,
        max_generations::Int = 100,
        order::Union{Int,Nothing} = nothing,
        membrane_depth::Union{Int,Nothing} = nothing,
        reservoir_size::Union{Int,Nothing} = nothing,
        base_order::Int = 5,
        fitness_function::Function = evaluate_fitness,
        test_problems::Vector{<:Any} = Any[],
        convergence_threshold::Float64 = 0.95,
        verbose::Bool = true
    ) -> ReservoirPopulation

Run evolutionary optimization of reservoir population with A000081-aligned parameters.

# Arguments
- `population_size::Union{Int,Nothing}`: Population size (auto-derived if nothing)
- `max_generations::Int`: Maximum generations
- `order::Union{Int,Nothing}`: B-series order (auto-derived if nothing)
- `membrane_depth::Union{Int,Nothing}`: Membrane hierarchy depth (auto-derived if nothing)
- `reservoir_size::Union{Int,Nothing}`: Reservoir size (auto-derived if nothing)
- `base_order::Int`: Base order for A000081 derivation (default: 5)
- `fitness_function::Function`: Fitness evaluation function
- `test_problems::Vector`: Test problems
- `convergence_threshold::Float64`: Fitness threshold for early stopping
- `verbose::Bool`: Print progress

# Returns
- `ReservoirPopulation`: Evolved population

# Examples
```julia
# Auto-derive all parameters from A000081
population = run_evolution(base_order=5, max_generations=100)

# Specify some parameters
population = run_evolution(
    population_size = 9,  # A000081[5]
    max_generations = 100,
    verbose = true
)
```
"""
function run_evolution(;
    population_size::Union{Int,Nothing} = nothing,
    max_generations::Int = 100,
    order::Union{Int,Nothing} = nothing,
    membrane_depth::Union{Int,Nothing} = nothing,
    reservoir_size::Union{Int,Nothing} = nothing,
    base_order::Int = 5,
    fitness_function::Function = evaluate_fitness,
    test_problems::Vector{<:Any} = Any[],
    convergence_threshold::Float64 = 0.95,
    verbose::Bool = true
)
    # Initialize population with A000081-derived parameters
    population = initialize_reservoir_population(
        population_size = population_size,
        order = order,
        membrane_depth = membrane_depth,
        reservoir_size = reservoir_size,
        base_order = base_order
    )
    
    actual_pop_size = length(population.individuals)
    
    if verbose
        println("Initialized population of $actual_pop_size reservoirs (A000081-aligned)")
        println("B-series order: $order")
        println("Membrane depth: $membrane_depth")
        println("Reservoir size: $reservoir_size")
        println()
    end
    
    # Evolution loop
    for gen in 1:max_generations
        # Evolve one generation
        evolve_generation!(population, fitness_function, test_problems=test_problems)
        
        # Get statistics
        best_fitness = population.best_fitness_history[end]
        avg_fitness = population.avg_fitness_history[end]
        
        if verbose
            println("Generation $gen:")
            println("  Best fitness: $(round(best_fitness, digits=4))")
            println("  Avg fitness:  $(round(avg_fitness, digits=4))")
            println()
        end
        
        # Check convergence
        if best_fitness >= convergence_threshold
            if verbose
                println("Converged at generation $gen")
            end
            break
        end
    end
    
    return population
end

"""
    get_best_reservoir(population::ReservoirPopulation) -> DeepTreeEchoReservoir

Get the best individual from the population.

# Arguments
- `population::ReservoirPopulation`: Population to search

# Returns
- `DeepTreeEchoReservoir`: Best reservoir

# Examples
```julia
best = get_best_reservoir(population)
```
"""
function get_best_reservoir(population::ReservoirPopulation)
    fitnesses = [ind.fitness for ind in population.individuals]
    best_idx = argmax(fitnesses)
    return population.individuals[best_idx]
end

"""
    population_diversity(population::ReservoirPopulation) -> Float64

Compute genetic diversity of the population.

# Arguments
- `population::ReservoirPopulation`: Population to analyze

# Returns
- `Float64`: Diversity score

# Examples
```julia
div = population_diversity(population)
```
"""
function population_diversity(population::ReservoirPopulation)
    n = length(population.individuals)
    
    if n <= 1
        return 0.0
    end
    
    total_distance = 0.0
    count = 0
    
    for i in 1:n
        for j in (i+1):n
            # Genetic distance based on B-series genomes
            d = genome_distance(
                population.individuals[i].bseries_genome,
                population.individuals[j].bseries_genome
            )
            total_distance += d
            count += 1
        end
    end
    
    return total_distance / count
end
