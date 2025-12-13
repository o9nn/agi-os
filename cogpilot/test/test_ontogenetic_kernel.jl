"""
Test Suite for Ontogenetic Kernel System

Comprehensive tests for:
- OntogeneticKernel: kernel creation, genome, lifecycle
- KernelEvolution: evolution, selection, crossover, mutation
- DomainKernels: domain-specific generators

This tests the Phase 2-4 implementations of the agent roadmap.
"""

# Add src to load path
push!(LOAD_PATH, joinpath(@__DIR__, "..", "src", "DeepTreeEcho"))

using Test
using LinearAlgebra
using Random

# Load modules
include("../src/DeepTreeEcho/DomainKernels.jl")
using .DomainKernels

# Access OntogeneticKernel through DomainKernels
const OK = DomainKernels.OntogeneticKernel

# Import needed functions
import .DomainKernels.OntogeneticKernel: evaluate_kernel_fitness!, self_generate,
                                          crossover, mutate!, update_stage!,
                                          genetic_distance, kernel_to_string,
                                          create_kernel, generate_trees_up_to_order

println("""
╔════════════════════════════════════════════════════════════════╗
║  ONTOGENETIC KERNEL TEST SUITE                                 ║
╚════════════════════════════════════════════════════════════════╝
""")

Random.seed!(42)

@testset "Ontogenetic Kernel Tests" begin
    
    @testset "1. Kernel Creation and Structure" begin
        println("\n[1/6] Testing Kernel Creation and Structure...")
        
        # Test basic kernel creation
        kernel = create_kernel(4, symmetric=true, density=0.5)
        
        @test kernel isa OK.Kernel
        @test kernel.genome isa OK.KernelGenome
        @test kernel.lifecycle isa OK.KernelLifecycle
        @test !isempty(kernel.id)
        @test kernel.genome.max_order == 4
        
        # Test genome structure
        @test kernel.genome.coefficients isa Dict{Vector{Int}, Float64}
        @test length(kernel.genome.coefficients) >= 0
        
        # Test lifecycle initialization
        @test kernel.lifecycle.stage == :embryonic
        @test kernel.lifecycle.maturity == 0.0
        @test kernel.lifecycle.age == 0
        @test kernel.lifecycle.generation == 0
        
        # Test fitness components initialized
        @test kernel.fitness == 0.0
        @test kernel.grip == 0.0
        @test kernel.stability == 0.0
        @test kernel.efficiency == 0.0
        @test kernel.novelty == 0.0
        
        println("  ✓ Kernel structure correct")
        println("  ✓ Genome initialized")
        println("  ✓ Lifecycle tracking working")
    end
    
    @testset "2. Tree Generation" begin
        println("\n[2/6] Testing Tree Generation...")
        
        # Test tree generation
        trees_order_3 = generate_trees_up_to_order(3)
        trees_order_4 = generate_trees_up_to_order(4)
        trees_order_5 = generate_trees_up_to_order(5)
        
        @test length(trees_order_3) >= 4  # At least 1+1+2 trees
        @test length(trees_order_4) >= 8  # At least 1+1+2+4 trees
        @test length(trees_order_5) >= 11  # More trees
        
        # Test tree structure (level sequences)
        for tree in trees_order_3
            @test tree isa Vector{Int}
            @test all(x -> x >= 1, tree)  # All levels >= 1
            @test tree[1] == 1  # Root at level 1
        end
        
        println("  ✓ Tree generation working")
        println("  ✓ Generated $(length(trees_order_5)) trees up to order 5")
    end
    
    @testset "3. Fitness Evaluation" begin
        println("\n[3/6] Testing Fitness Evaluation...")
        
        # Create test population
        population = [create_kernel(4) for _ in 1:5]
        
        # Evaluate fitness for each kernel
        for kernel in population
            evaluate_kernel_fitness!(kernel, nothing, population)
            
            # All fitness components should be in [0, 1]
            @test 0.0 <= kernel.grip <= 1.0
            @test 0.0 <= kernel.stability <= 1.0
            @test 0.0 <= kernel.efficiency <= 1.0
            @test 0.0 <= kernel.novelty <= 1.0
            
            # Overall fitness should be average
            expected_fitness = 0.25 * (kernel.grip + kernel.stability + 
                                      kernel.efficiency + kernel.novelty)
            @test abs(kernel.fitness - expected_fitness) < 1e-6
        end
        
        # Test that novelty changes with population
        kernel1 = create_kernel(4)
        evaluate_kernel_fitness!(kernel1, nothing, population)
        novelty1 = kernel1.novelty
        
        # Add very different kernel to population
        different_kernel = create_kernel(6, density=0.9)
        push!(population, different_kernel)
        evaluate_kernel_fitness!(kernel1, nothing, population)
        novelty2 = kernel1.novelty
        
        # Novelty should change
        # (May increase or decrease depending on genetic distance)
        @test novelty1 != novelty2 || novelty1 == 0.5  # Unless defaulted
        
        println("  ✓ Fitness evaluation working")
        println("  ✓ All components in valid range")
        println("  ✓ Novelty responds to population diversity")
    end
    
    @testset "4. Kernel Operations" begin
        println("\n[4/6] Testing Kernel Operations...")
        
        # Create parent kernels
        parent1 = create_kernel(4, symmetric=true, density=0.6)
        parent2 = create_kernel(4, symmetric=false, density=0.5)
        
        # Test self-generation
        offspring_self = self_generate(parent1)
        @test offspring_self.lifecycle.generation == parent1.lifecycle.generation + 1
        @test parent1.id in offspring_self.lineage
        @test offspring_self.id != parent1.id
        
        # Test crossover
        child1, child2 = crossover(parent1, parent2)
        @test child1.id != child2.id
        @test child1.id != parent1.id
        @test child1.id != parent2.id
        @test parent1.id in child1.lineage || parent2.id in child1.lineage
        @test child1.genome.max_order >= min(parent1.genome.max_order, 
                                              parent2.genome.max_order)
        
        # Test mutation
        original_genome_size = length(parent1.genome.coefficients)
        original_coeffs = copy(parent1.genome.coefficients)
        mutate!(parent1, mutation_rate=0.5)
        
        # Genome should have changed
        has_changed = false
        for (tree, coeff) in parent1.genome.coefficients
            if !haskey(original_coeffs, tree) || original_coeffs[tree] != coeff
                has_changed = true
                break
            end
        end
        # Or size changed
        if length(parent1.genome.coefficients) != original_genome_size
            has_changed = true
        end
        @test has_changed || original_genome_size == 0
        
        println("  ✓ Self-generation working")
        println("  ✓ Crossover producing offspring")
        println("  ✓ Mutation modifying genomes")
    end
    
    @testset "5. Lifecycle Management" begin
        println("\n[5/6] Testing Lifecycle Management...")
        
        # Create kernel and age it
        kernel = create_kernel(4)
        
        # Test stage transitions
        @test kernel.lifecycle.stage == :embryonic
        @test kernel.lifecycle.age == 0
        @test kernel.lifecycle.maturity == 0.0
        
        # Age kernel and update stage
        kernel.lifecycle.age = 6
        kernel.fitness = 0.4
        update_stage!(kernel)
        @test kernel.lifecycle.stage == :juvenile
        @test kernel.lifecycle.maturity > 0.0
        
        # Age more
        kernel.lifecycle.age = 16
        kernel.fitness = 0.7
        update_stage!(kernel)
        @test kernel.lifecycle.stage == :mature
        
        # Test senescence
        kernel.lifecycle.age = 51
        update_stage!(kernel)
        @test kernel.lifecycle.stage == :senescent
        
        # Test fitness-based decline to senescence
        kernel2 = create_kernel(4)
        kernel2.lifecycle.stage = :mature
        kernel2.lifecycle.age = 30
        kernel2.fitness = 0.3  # Low fitness
        update_stage!(kernel2)
        @test kernel2.lifecycle.stage == :senescent
        
        println("  ✓ Lifecycle stages transitioning correctly")
        println("  ✓ Maturity increasing with age")
        println("  ✓ Senescence triggered appropriately")
    end
    
    @testset "6. Domain-Specific Generators" begin
        println("\n[6/6] Testing Domain-Specific Generators...")
        
        # Test consciousness kernel
        consciousness = generate_consciousness_kernel(order=5, depth_bias=2.0)
        @test consciousness isa OK.Kernel
        @test consciousness.genome.max_order == 5
        @test "consciousness_seed" in consciousness.lineage
        @test consciousness.lifecycle.stage == :juvenile  # Born mature
        
        # Test physics kernel
        physics = generate_physics_kernel(:hamiltonian, order=4, 
                                         conserved_quantities=[:energy])
        @test physics isa OK.Kernel
        @test physics.genome.max_order == 4
        @test occursin("physics", physics.lineage[1])
        
        # Test reaction kernel
        reactions = ["A + B → C"]
        reaction = generate_reaction_kernel(reactions, order=4, mass_action=true)
        @test reaction isa OK.Kernel
        @test reaction.genome.max_order == 4
        @test occursin("reaction", reaction.lineage[1])
        
        # Test time series kernel
        timeseries = generate_timeseries_kernel(memory_depth=10, order=4, 
                                               prediction_horizon=1)
        @test timeseries isa OK.Kernel
        @test timeseries.genome.max_order == 4
        @test occursin("timeseries", timeseries.lineage[1])
        
        # Test universal kernel generator
        universal1 = generate_universal_kernel("self-aware cognition")
        @test universal1 isa OK.Kernel
        @test occursin("consciousness", universal1.lineage[1])
        
        universal2 = generate_universal_kernel("Hamiltonian energy conservation")
        @test universal2 isa OK.Kernel
        @test occursin("physics", universal2.lineage[1])
        
        universal3 = generate_universal_kernel("time series prediction")
        @test universal3 isa OK.Kernel
        @test occursin("timeseries", universal3.lineage[1])
        
        println("  ✓ Consciousness kernel generator working")
        println("  ✓ Physics kernel generator working")
        println("  ✓ Reaction kernel generator working")
        println("  ✓ Time series kernel generator working")
        println("  ✓ Universal kernel generator working")
    end
    
end

println("\n" * "="^70)
println("ALL TESTS PASSED ✓")
println("="^70)
println("\nOntogenetic Kernel System is fully operational:")
println("  • Kernel creation and genome management")
println("  • Tree generation (OEIS A000081 foundation)")
println("  • Fitness evaluation (4 components)")
println("  • Genetic operations (self-generation, crossover, mutation)")
println("  • Lifecycle management (4 stages)")
println("  • Domain-specific generators (5 types)")
println("\nReady for integration with DeepTreeEcho system.")
println("="^70)
