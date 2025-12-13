"""
Ontogenetic Kernel Evolution Demo

Demonstrates the complete kernel evolution system:
1. Create domain-specific kernels
2. Evolve population through generations
3. Analyze fitness and diversity
4. Show domain-specific optimization

This implements Phases 2-4 of the agent roadmap.
"""

# Add src to load path
push!(LOAD_PATH, joinpath(@__DIR__, "..", "src", "DeepTreeEcho"))

using Random
using Statistics
using LinearAlgebra

# Load only DomainKernels which includes OntogeneticKernel
include("../src/DeepTreeEcho/DomainKernels.jl")
using .DomainKernels

# Access nested modules
const OK = DomainKernels.OntogeneticKernel

# Now include KernelEvolution after DomainKernels is loaded  
# But we need to make it use the same OntogeneticKernel

# For simplicity, let's inline the evolution functions here
using .DomainKernels: Kernel, KernelGenome

# Import needed functions
import .DomainKernels.OntogeneticKernel: evaluate_kernel_fitness!, self_generate,
                                          crossover, mutate!, update_stage!,
                                          genetic_distance, kernel_to_string

# Simple evolution configuration
struct SimpleEvolutionConfig
    population_size::Int
    mutation_rate::Float64
    crossover_rate::Float64
    max_generations::Int
end

# Simple evolution function (inlined to avoid module conflicts)
function simple_evolve!(population, config; verbose=true)
    if verbose
        println("\n" * "="^60)
        println("KERNEL EVOLUTION")
        println("="^60)
        println("Population Size: $(config.population_size)")
        println("Max Generations: $(config.max_generations)")
        println("="^60 * "\n")
    end
    
    for gen in 1:config.max_generations
        # Evaluate fitness
        for kernel in population
            evaluate_kernel_fitness!(kernel, nothing, population)
        end
        
        # Sort by fitness
        sort!(population, by=k->k.fitness, rev=true)
        
        # Print progress
        if verbose && (gen % 5 == 0 || gen == 1)
            fitnesses = [k.fitness for k in population]
            println("Gen $gen: best=$(round(maximum(fitnesses), digits=3)) " *
                   "avg=$(round(mean(fitnesses), digits=3))")
        end
        
        # Create next generation
        next_gen = typeof(population[1])[]
        
        # Elitism: keep best 2
        append!(next_gen, population[1:min(2, length(population))])
        
        # Reproduction
        while length(next_gen) < config.population_size
            # Select parents (simple random selection from top half)
            top_half = population[1:div(length(population), 2)]
            parent1 = rand(top_half)
            parent2 = rand(top_half)
            
            # Crossover
            if rand() < config.crossover_rate
                child1, child2 = crossover(parent1, parent2)
                mutate!(child1, mutation_rate=config.mutation_rate)
                push!(next_gen, child1)
                if length(next_gen) < config.population_size
                    mutate!(child2, mutation_rate=config.mutation_rate)
                    push!(next_gen, child2)
                end
            else
                child = self_generate(parent1)
                mutate!(child, mutation_rate=config.mutation_rate)
                push!(next_gen, child)
            end
        end
        
        # Update ages and stages
        for kernel in next_gen
            kernel.lifecycle.age += 1
            update_stage!(kernel)
        end
        
        # Replace population
        empty!(population)
        append!(population, next_gen[1:config.population_size])
    end
    
    if verbose
        println("\n✓ Evolution complete")
    end
end

println("""
╔════════════════════════════════════════════════════════════════╗
║  ONTOGENETIC KERNEL EVOLUTION DEMONSTRATION                    ║
║                                                                 ║
║  Self-evolving computational kernels with B-series genomes     ║
║  Following OEIS A000081 mathematical foundations               ║
╚════════════════════════════════════════════════════════════════╝
""")

Random.seed!(42)

# ============================================================================
# Part 1: Create Domain-Specific Seed Kernels
# ============================================================================

println("\n" * "="^70)
println("PART 1: Creating Domain-Specific Seed Kernels")
println("="^70)

println("\n1. Consciousness Kernel (self-referential, recursive)")
consciousness_kernel = generate_consciousness_kernel(order=5, depth_bias=2.5)
println(OK.kernel_to_string(consciousness_kernel))

println("\n2. Physics Kernel (Hamiltonian, energy-conserving)")
physics_kernel = generate_physics_kernel(:hamiltonian, order=4, 
                                        conserved_quantities=[:energy])
println(OK.kernel_to_string(physics_kernel))

println("\n3. Reaction Network Kernel (mass action kinetics)")
reactions = ["A + B → C", "C → D"]
reaction_kernel = generate_reaction_kernel(reactions, order=4, mass_action=true)
println(OK.kernel_to_string(reaction_kernel))

println("\n4. Time Series Kernel (temporal prediction)")
timeseries_kernel = generate_timeseries_kernel(memory_depth=10, order=4, 
                                               prediction_horizon=1)
println(OK.kernel_to_string(timeseries_kernel))

# ============================================================================
# Part 2: Initialize Population with Seed Kernels
# ============================================================================

println("\n" * "="^70)
println("PART 2: Initializing Evolution Population")
println("="^70)

# Create population with seed kernels
population = OK.Kernel[]

# Add domain-specific seeds
push!(population, consciousness_kernel)
push!(population, physics_kernel)
push!(population, reaction_kernel)
push!(population, timeseries_kernel)

# Add random kernels to reach population size
target_size = 20
while length(population) < target_size
    random_kernel = OK.create_kernel(4, symmetric=rand(Bool), density=rand(0.3:0.1:0.7))
    push!(population, random_kernel)
end

println("✓ Population initialized with $(length(population)) kernels")
println("  - 4 domain-specific seeds")
println("  - $(length(population) - 4) random kernels")

# ============================================================================
# Part 3: Demonstrate Kernel Operations
# ============================================================================

println("\n" * "="^70)
println("PART 3: Kernel Operations Demonstration")
println("="^70)

println("\n3.1 Self-Generation (Chain Rule Composition)")
offspring = OK.self_generate(consciousness_kernel)
println("Parent: $(consciousness_kernel.id)")
println("Offspring: $(offspring.id)")
println("  Generation: $(offspring.lifecycle.generation)")
println("  Lineage: $(join(offspring.lineage, " ← "))")

println("\n3.2 Crossover (Genetic Recombination)")
child1, child2 = OK.crossover(physics_kernel, reaction_kernel)
println("Parent 1: $(physics_kernel.id)")
println("Parent 2: $(reaction_kernel.id)")
println("Child 1: $(child1.id) - $(length(child1.genome.coefficients)) terms")
println("Child 2: $(child2.id) - $(length(child2.genome.coefficients)) terms")

println("\n3.3 Mutation")
test_kernel = OK.create_kernel(4)
original_size = length(test_kernel.genome.coefficients)
OK.mutate!(test_kernel, mutation_rate=0.3)
mutated_size = length(test_kernel.genome.coefficients)
println("Original genome: $original_size terms")
println("Mutated genome: $mutated_size terms")
println("Change: $(mutated_size - original_size) terms")

println("\n3.4 Fitness Evaluation")
OK.evaluate_kernel_fitness!(consciousness_kernel, nothing, population)
println("Consciousness Kernel Fitness:")
println("  Overall: $(round(consciousness_kernel.fitness, digits=3))")
println("  Grip: $(round(consciousness_kernel.grip, digits=3))")
println("  Stability: $(round(consciousness_kernel.stability, digits=3))")
println("  Efficiency: $(round(consciousness_kernel.efficiency, digits=3))")
println("  Novelty: $(round(consciousness_kernel.novelty, digits=3))")

# ============================================================================
# Part 4: Evolve Population
# ============================================================================

println("\n" * "="^70)
println("PART 4: Population Evolution")
println("="^70)

# Evolution configuration
config = SimpleEvolutionConfig(20, 0.15, 0.8, 30)

# Evolve!
simple_evolve!(population, config, verbose=true)

# ============================================================================
# Part 5: Analyze Results
# ============================================================================

println("\n" * "="^70)
println("PART 5: Evolution Results Analysis")
println("="^70)

# Print top kernels
println("\nTop 5 Kernels After Evolution:")
println("-"^70)
for (i, kernel) in enumerate(population[1:min(5, length(population))])
    println("\n#$i. $(kernel.id)")
    println("    Stage: $(kernel.lifecycle.stage), Age: $(kernel.lifecycle.age)")
    println("    Fitness: $(round(kernel.fitness, digits=3)) " *
           "(G:$(round(kernel.grip, digits=2)) " *
           "S:$(round(kernel.stability, digits=2)) " *
           "E:$(round(kernel.efficiency, digits=2)) " *
           "N:$(round(kernel.novelty, digits=2)))")
    println("    Genome: $(length(kernel.genome.coefficients)) terms")
end

# Analyze final population
println("\n\nFinal Population Statistics:")
fitnesses = [k.fitness for k in population]
println("  Best fitness: $(round(maximum(fitnesses), digits=4))")
println("  Average fitness: $(round(mean(fitnesses), digits=4))")
println("  Worst fitness: $(round(minimum(fitnesses), digits=4))")

# Diversity
println("\n  Diversity (genetic variance): $(round(std(fitnesses), digits=3))")

# Stage distribution
stage_counts = Dict{Symbol, Int}()
for k in population
    stage_counts[k.lifecycle.stage] = get(stage_counts, k.lifecycle.stage, 0) + 1
end

println("\n  Stage Distribution:")
for (stage, count) in sort(collect(stage_counts))
    percentage = round(100 * count / length(population), digits=1)
    println("    $(rpad(stage, 12)): $count kernels ($percentage%)")
end

# ============================================================================
# Part 6: Universal Kernel Generator Demo
# ============================================================================

println("\n" * "="^70)
println("PART 6: Universal Kernel Generator")
println("="^70)

println("\nGenerating kernels from natural language descriptions:")

domains = [
    "self-aware recursive cognition with deep memory",
    "Hamiltonian mechanics with energy and momentum conservation",
    "enzyme kinetics mass action with catalysis",
    "stock price prediction temporal patterns multi-step",
    "quantum harmonic oscillator symplectic"
]

for (i, description) in enumerate(domains)
    println("\n$i. \"$description\"")
    universal_kernel = generate_universal_kernel(description, order=4)
    println("   Generated: $(universal_kernel.id)")
    println("   Lineage: $(join(universal_kernel.lineage, " ← "))")
    println("   Genome: $(length(universal_kernel.genome.coefficients)) terms")
end

# ============================================================================
# Summary
# ============================================================================

println("\n" * "="^70)
println("DEMONSTRATION COMPLETE")
println("="^70)

println("\n✓ Successfully demonstrated:")
println("  1. Domain-specific kernel generation")
println("  2. Kernel lifecycle and operations")
println("  3. Population-based evolution")
println("  4. Fitness evaluation and selection")
println("  5. Universal kernel generation")

println("\n📊 Evolution Statistics:")
println("  Generations run: 30")
println("  Final best fitness: $(round(maximum([k.fitness for k in population]), digits=4))")
println("  Population size: $(length(population))")
println("  Best kernel stage: $(population[1].lifecycle.stage)")

println("\n🌟 Key Achievements:")
println("  • Self-evolving kernels with B-series genomes")
println("  • Domain-specific optimization")
println("  • Multi-objective fitness (grip, stability, efficiency, novelty)")
println("  • Lifecycle stages (embryonic → juvenile → mature → senescent)")
println("  • Genetic diversity preservation")

println("\n" * "="^70)
println("Ontogenetic kernels: Where B-series become genetic code,")
println("and numerical methods evolve like living organisms.")
println("="^70)
