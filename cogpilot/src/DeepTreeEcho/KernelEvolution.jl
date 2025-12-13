"""
    KernelEvolution Module

Population-based evolutionary algorithm for ontogenetic kernels.
Implements Phase 3 of the agent roadmap: Kernel Evolution System.

# Evolutionary Algorithm

1. **Evaluate Fitness**: Score all kernels
2. **Selection**: Tournament selection for parents
3. **Crossover**: Create offspring via genetic recombination
4. **Mutation**: Introduce random variations
5. **Elitism**: Preserve best individuals
6. **Replacement**: Form next generation

# Features

- Tournament selection with configurable size
- Elitism to preserve top performers
- Diversity preservation via novelty metric
- Adaptive mutation rates
- Generation statistics tracking
"""
module KernelEvolution

using LinearAlgebra
using Random
using Statistics

# Import OntogeneticKernel module
include("OntogeneticKernel.jl")
using .OntogeneticKernel

export EvolutionConfig, GenerationStats
export evolve_kernel_population!, tournament_selection
export population_diversity, clone_kernel
export print_generation_stats, print_population_status

"""
    EvolutionConfig

Configuration for evolutionary algorithm.
"""
struct EvolutionConfig
    population_size::Int      # Population size
    mutation_rate::Float64    # Base mutation rate
    crossover_rate::Float64   # Crossover probability
    elitism_rate::Float64     # Fraction to preserve via elitism
    tournament_size::Int      # Tournament selection size
    max_generations::Int      # Maximum generations
    fitness_threshold::Float64 # Convergence threshold
    
    function EvolutionConfig(;
        population_size::Int=20,
        mutation_rate::Float64=0.15,
        crossover_rate::Float64=0.8,
        elitism_rate::Float64=0.1,
        tournament_size::Int=3,
        max_generations::Int=50,
        fitness_threshold::Float64=0.9)
        
        new(population_size, mutation_rate, crossover_rate, elitism_rate,
            tournament_size, max_generations, fitness_threshold)
    end
end

"""
    GenerationStats

Statistics for a single generation.
"""
struct GenerationStats
    generation::Int
    best_fitness::Float64
    avg_fitness::Float64
    worst_fitness::Float64
    diversity::Float64
    avg_grip::Float64
    avg_stability::Float64
    avg_efficiency::Float64
    avg_novelty::Float64
    stage_distribution::Dict{Symbol, Int}
end

"""
    evolve_kernel_population!(population::Vector{Kernel},
                             config::EvolutionConfig;
                             domain_data=nothing,
                             verbose::Bool=true)

Evolve a population of kernels over multiple generations.

# Algorithm
1. Evaluate fitness for all kernels
2. Sort by fitness (best first)
3. Record generation statistics
4. Check convergence
5. Create next generation:
   - Elite kernels (preserved)
   - Offspring via crossover
   - Random mutations
6. Update development stages
7. Repeat

# Arguments
- `population::Vector{Kernel}`: Initial population (modified in place)
- `config::EvolutionConfig`: Evolution parameters
- `domain_data`: Optional domain-specific test data
- `verbose::Bool=true`: Print progress

# Returns
- `Vector{GenerationStats}`: Statistics for each generation

# Side Effects
Modifies population in place.
"""
function evolve_kernel_population!(population::Vector{Kernel},
                                  config::EvolutionConfig;
                                  domain_data=nothing,
                                  verbose::Bool=true)
    
    generations_stats = GenerationStats[]
    
    if verbose
        println("\n" * "="^60)
        println("KERNEL EVOLUTION")
        println("="^60)
        println("Population Size: $(config.population_size)")
        println("Max Generations: $(config.max_generations)")
        println("Mutation Rate: $(config.mutation_rate)")
        println("Crossover Rate: $(config.crossover_rate)")
        println("Elitism Rate: $(config.elitism_rate)")
        println("="^60 * "\n")
    end
    
    for gen in 1:config.max_generations
        # 1. Evaluate fitness for all kernels
        for kernel in population
            evaluate_kernel_fitness!(kernel, domain_data, population)
        end
        
        # 2. Sort by fitness (descending)
        sort!(population, by = k -> k.fitness, rev = true)
        
        # 3. Record generation statistics
        gen_stats = compute_generation_stats(population, gen)
        push!(generations_stats, gen_stats)
        
        # 4. Print progress
        if verbose && (gen % 5 == 0 || gen == 1)
            print_generation_stats(gen_stats)
        end
        
        # 5. Check convergence
        if gen_stats.best_fitness >= config.fitness_threshold
            if verbose
                println("\n✓ Converged at generation $gen!")
                println("  Best fitness: $(round(gen_stats.best_fitness, digits=4))")
            end
            break
        end
        
        # 6. Create next generation
        next_generation = Kernel[]
        
        # Elitism: preserve best individuals
        n_elite = max(1, Int(floor(config.elitism_rate * length(population))))
        append!(next_generation, population[1:n_elite])
        
        # Reproduction: crossover and mutation
        while length(next_generation) < config.population_size
            # Select parents
            parent1 = tournament_selection(population, config.tournament_size)
            parent2 = tournament_selection(population, config.tournament_size)
            
            # Crossover
            if rand() < config.crossover_rate
                offspring1, offspring2 = crossover(parent1, parent2)
                
                # Mutate offspring
                mutate!(offspring1, mutation_rate=config.mutation_rate)
                if length(next_generation) + 1 < config.population_size
                    mutate!(offspring2, mutation_rate=config.mutation_rate)
                    push!(next_generation, offspring2)
                end
                push!(next_generation, offspring1)
            else
                # Clone parent
                offspring = clone_kernel(parent1)
                mutate!(offspring, mutation_rate=config.mutation_rate)
                push!(next_generation, offspring)
            end
        end
        
        # Trim to exact population size
        if length(next_generation) > config.population_size
            next_generation = next_generation[1:config.population_size]
        end
        
        # 7. Update development stages and age
        for kernel in next_generation
            kernel.lifecycle.age += 1
            update_stage!(kernel)
        end
        
        # Replace population
        empty!(population)
        append!(population, next_generation)
    end
    
    if verbose
        println("\n" * "="^60)
        println("EVOLUTION COMPLETE")
        println("="^60)
        print_final_statistics(generations_stats)
        println()
    end
    
    return generations_stats
end

"""
    tournament_selection(population::Vector{Kernel}, 
                        tournament_size::Int)

Select a kernel via tournament selection.

# Arguments
- `population::Vector{Kernel}`: Population to select from
- `tournament_size::Int`: Number of individuals in tournament

# Returns
- `Kernel`: Selected kernel (winner of tournament)
"""
function tournament_selection(population::Vector{Kernel}, 
                             tournament_size::Int)
    
    # Sample tournament_size individuals
    tournament = sample(population, min(tournament_size, length(population)), replace=false)
    
    # Return best (highest fitness)
    return tournament[argmax([k.fitness for k in tournament])]
end

"""
    population_diversity(population::Vector{Kernel})

Compute genetic diversity of population.

# Returns
- `Float64`: Diversity measure (0.0 to 1.0)
"""
function population_diversity(population::Vector{Kernel})
    if length(population) <= 1
        return 0.0
    end
    
    # Average pairwise genetic distance
    distances = Float64[]
    
    for i in 1:length(population)
        for j in (i+1):length(population)
            dist = genetic_distance(population[i], population[j])
            push!(distances, dist)
        end
    end
    
    if isempty(distances)
        return 0.0
    end
    
    return mean(distances)
end

"""
    clone_kernel(kernel::Kernel)

Create a clone of a kernel.

# Arguments
- `kernel::Kernel`: Kernel to clone

# Returns
- `Kernel`: Cloned kernel with new ID
"""
function clone_kernel(kernel::Kernel)
    # Deep copy genome
    new_coeffs = copy(kernel.genome.coefficients)
    new_genome = KernelGenome(new_coeffs, kernel.genome.max_order)
    
    # Create clone with same lineage
    clone = Kernel(new_genome, copy(kernel.lineage))
    
    # Copy lifecycle (but reset age)
    clone.lifecycle.generation = kernel.lifecycle.generation
    clone.lifecycle.stage = kernel.lifecycle.stage
    clone.lifecycle.maturity = kernel.lifecycle.maturity
    clone.lifecycle.age = 0
    
    # Copy fitness components
    clone.fitness = kernel.fitness
    clone.grip = kernel.grip
    clone.stability = kernel.stability
    clone.efficiency = kernel.efficiency
    clone.novelty = kernel.novelty
    
    return clone
end

"""
    compute_generation_stats(population::Vector{Kernel}, 
                            generation::Int)

Compute statistics for current generation.
"""
function compute_generation_stats(population::Vector{Kernel}, 
                                 generation::Int)
    
    fitnesses = [k.fitness for k in population]
    grips = [k.grip for k in population]
    stabilities = [k.stability for k in population]
    efficiencies = [k.efficiency for k in population]
    novelties = [k.novelty for k in population]
    
    # Stage distribution
    stage_dist = Dict{Symbol, Int}()
    for kernel in population
        stage = kernel.lifecycle.stage
        stage_dist[stage] = get(stage_dist, stage, 0) + 1
    end
    
    return GenerationStats(
        generation,
        maximum(fitnesses),
        mean(fitnesses),
        minimum(fitnesses),
        population_diversity(population),
        mean(grips),
        mean(stabilities),
        mean(efficiencies),
        mean(novelties),
        stage_dist
    )
end

"""
    print_generation_stats(stats::GenerationStats)

Print generation statistics.
"""
function print_generation_stats(stats::GenerationStats)
    println("Generation $(stats.generation):")
    println("  Fitness: best=$(round(stats.best_fitness, digits=3)) " *
            "avg=$(round(stats.avg_fitness, digits=3)) " *
            "worst=$(round(stats.worst_fitness, digits=3))")
    println("  Diversity: $(round(stats.diversity, digits=3))")
    println("  Components: grip=$(round(stats.avg_grip, digits=3)) " *
            "stability=$(round(stats.avg_stability, digits=3)) " *
            "efficiency=$(round(stats.avg_efficiency, digits=3)) " *
            "novelty=$(round(stats.avg_novelty, digits=3))")
    
    # Print stage distribution
    stage_str = join(["$(k)=$(v)" for (k, v) in stats.stage_distribution], ", ")
    println("  Stages: $stage_str")
end

"""
    print_final_statistics(stats::Vector{GenerationStats})

Print final evolution statistics.
"""
function print_final_statistics(stats::Vector{GenerationStats})
    if isempty(stats)
        return
    end
    
    final = stats[end]
    initial = stats[1]
    
    println("Final Generation: $(final.generation)")
    println("  Best Fitness: $(round(final.best_fitness, digits=4))")
    println("  Improvement: $(round((final.best_fitness - initial.best_fitness), digits=4))")
    println("  Final Diversity: $(round(final.diversity, digits=3))")
    println()
    
    println("Evolution Trajectory:")
    println("  Gen | Best Fit | Avg Fit | Diversity")
    println("  " * "-"^42)
    for s in stats
        if s.generation % 10 == 0 || s.generation == 1 || s == final
            println("  $(lpad(s.generation, 3)) | " *
                   "$(rpad(round(s.best_fitness, digits=3), 8)) | " *
                   "$(rpad(round(s.avg_fitness, digits=3), 7)) | " *
                   "$(round(s.diversity, digits=3))")
        end
    end
end

"""
    print_population_status(population::Vector{Kernel})

Print detailed population status.
"""
function print_population_status(population::Vector{Kernel})
    println("\n" * "="^60)
    println("POPULATION STATUS")
    println("="^60)
    println("Size: $(length(population))")
    println()
    
    # Sort by fitness
    sorted_pop = sort(population, by=k->k.fitness, rev=true)
    
    println("Top 5 Kernels:")
    println("-"^60)
    for (i, kernel) in enumerate(sorted_pop[1:min(5, length(sorted_pop))])
        println("\n#$i. $(kernel.id)")
        println("    Stage: $(kernel.lifecycle.stage), Age: $(kernel.lifecycle.age), Gen: $(kernel.lifecycle.generation)")
        println("    Fitness: $(round(kernel.fitness, digits=3)) " *
               "(G:$(round(kernel.grip, digits=2)) " *
               "S:$(round(kernel.stability, digits=2)) " *
               "E:$(round(kernel.efficiency, digits=2)) " *
               "N:$(round(kernel.novelty, digits=2)))")
        println("    Genome: $(length(kernel.genome.coefficients)) terms, order $(kernel.genome.max_order)")
    end
    
    println("\n" * "="^60)
end

end # module KernelEvolution
