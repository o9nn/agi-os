"""
Tests for DeepTreeEcho module.
"""

using Test
using LinearAlgebra
using Random

# Load the module
include("../../src/DeepTreeEcho/DeepTreeEcho.jl")
using .DeepTreeEcho

Random.seed!(42)

@testset "DeepTreeEcho Tests" begin
    
    @testset "RootedTreeOps" begin
        # Test tree generation
        trees = generate_all_trees(3)
        @test length(trees) == 2  # A000081: a(3) = 2
        
        # Test tree similarity
        if length(trees) >= 2
            sim = tree_similarity(trees[1], trees[1])
            @test sim ≈ 1.0
        end
        
        # Test tree diversity
        if length(trees) >= 2
            div = tree_diversity(trees)
            @test 0.0 <= div <= 1.0
        end
    end
    
    @testset "BSeriesGenome" begin
        # Test genome initialization
        genome = initialize_bseries_genome(3)
        @test genome.order == 3
        @test length(genome.coefficients) > 0
        
        # Test genome cloning
        clone = clone_genome(genome)
        @test clone.id != genome.id
        @test length(clone.coefficients) == length(genome.coefficients)
        
        # Test crossover
        genome2 = initialize_bseries_genome(3)
        child = crossover(genome, genome2)
        @test child.generation == genome.generation + 1
        @test length(child.parents) == 2
        
        # Test mutation
        original_coeffs = copy(genome.coefficients)
        mutate!(genome, 1.0)  # 100% mutation rate
        # At least some coefficients should have changed
        changed = false
        for (tree, coeff) in genome.coefficients
            if coeff != original_coeffs[tree]
                changed = true
                break
            end
        end
        @test changed
    end
    
    @testset "JSurfaceIntegrator" begin
        # Create a simple J-surface
        trees = generate_all_trees(2)
        coeffs = ones(Float64, length(trees))
        surface = JSurface(trees, coeffs)
        
        @test length(surface.trees) == length(trees)
        @test length(surface.coefficients) == length(coeffs)
        
        # Test integrator creation
        integrator = JSurfaceIntegrator(surface)
        @test integrator.gradient_step_size > 0
        @test integrator.mutation_rate > 0
        
        # Test gradient computation
        loss(c) = sum(c.^2)
        grad = compute_gradient(surface, loss)
        @test length(grad) == length(coeffs)
        
        # Test gradient step
        gradient_step!(integrator, grad)
        @test length(integrator.history) == 2
    end
    
    @testset "MembraneReservoirBridge" begin
        # Create a simple P-system
        psystem = create_hierarchical_psystem(2, 2)
        @test length(psystem.membranes) > 1
        
        # Create membrane network
        reservoir_sizes = Dict(m.label => 10 for m in psystem.membranes)
        network = create_membrane_network(psystem, reservoir_sizes)
        
        @test length(network.membranes) > 0
        @test network.generation == 0
        
        # Test network step
        inputs = Dict(1 => [0.5])
        step!(network, inputs)
        
        # Test topology adaptation
        metrics = Dict(id => 0.5 for id in keys(network.membranes))
        adapt_topology!(network, metrics)
        @test network.generation == 1
    end
    
    @testset "DeepTreeEchoReservoir" begin
        # Test initialization
        reservoir = initialize_deep_tree_echo(
            order = 3,
            membrane_depth = 2,
            reservoir_size = 10
        )
        
        @test reservoir.order == 3
        @test length(reservoir.rooted_trees) > 0
        @test length(reservoir.membrane_network.membranes) > 0
        
        # Test reservoir info
        info = reservoir_info(reservoir)
        @test haskey(info, "id")
        @test haskey(info, "generation")
        @test haskey(info, "num_trees")
        
        # Test cloning
        clone = clone_reservoir(reservoir)
        @test clone.id != reservoir.id
        @test clone.order == reservoir.order
    end
    
    @testset "FitnessEvaluation" begin
        # Create a reservoir
        reservoir = initialize_deep_tree_echo(
            order = 2,
            membrane_depth = 2,
            reservoir_size = 10
        )
        
        # Test fitness evaluation
        fitness = evaluate_fitness(reservoir)
        @test 0.0 <= fitness <= 1.0
        
        # Test stability evaluation
        stability = evaluate_stability(reservoir)
        @test 0.0 <= stability <= 1.0
        
        # Test efficiency evaluation
        efficiency = evaluate_efficiency(reservoir)
        @test 0.0 <= efficiency <= 1.0
    end
    
    @testset "Evolution" begin
        # Initialize population
        population = initialize_reservoir_population(
            population_size = 5,
            order = 2,
            membrane_depth = 2,
            reservoir_size = 10
        )
        
        @test length(population.individuals) == 5
        @test population.generation == 0
        
        # Test evolution
        evolve_generation!(population, evaluate_fitness)
        @test population.generation == 1
        @test length(population.best_fitness_history) == 1
        @test length(population.avg_fitness_history) == 1
        
        # Test best reservoir selection
        best = get_best_reservoir(population)
        @test best isa DeepTreeEchoReservoir
        
        # Test population diversity
        div = population_diversity(population)
        @test div >= 0.0
    end
    
    @testset "Integration" begin
        # End-to-end test
        println("\nRunning integration test...")
        
        # Create reservoir
        reservoir = initialize_deep_tree_echo(
            order = 3,
            membrane_depth = 2,
            reservoir_size = 20
        )
        
        # Generate synthetic data
        t = 0:0.1:5
        signal = sin.(2π * 0.5 * t)
        training_data = reshape(signal[1:end-1], 1, :)
        targets = reshape(signal[2:end], 1, :)
        
        # Train
        train_reservoir!(reservoir, training_data, targets)
        
        # Evaluate fitness
        fitness = evaluate_fitness(reservoir)
        @test 0.0 <= fitness <= 1.0
        
        # Evolve
        evolve!(reservoir, training_data)
        @test reservoir.generation == 1
        
        println("Integration test passed!")
    end
end

println("\n✓ All DeepTreeEcho tests passed!")
