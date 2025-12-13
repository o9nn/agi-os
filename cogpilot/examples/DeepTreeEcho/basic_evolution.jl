"""
Basic example of Deep Tree Echo Reservoir evolution.

This example demonstrates:
1. Initializing a Deep Tree Echo Reservoir
2. Training with temporal data
3. Evolving a population of reservoirs
4. Analyzing the results
"""

using Pkg
Pkg.activate(joinpath(@__DIR__, "../.."))

# Load the DeepTreeEcho module
include("../../src/DeepTreeEcho/DeepTreeEcho.jl")
using .DeepTreeEcho

using LinearAlgebra
using Statistics
using Random

Random.seed!(42)

println("=" ^ 70)
println("Deep Tree Echo Reservoir Computer - Basic Evolution Example")
println("=" ^ 70)
println()

# ============================================================================
# Part 1: Initialize a single reservoir
# ============================================================================

println("Part 1: Initializing Deep Tree Echo Reservoir")
println("-" ^ 70)

reservoir = initialize_deep_tree_echo(
    order = 4,              # B-series order (rooted trees up to order 4)
    membrane_depth = 3,     # 3 levels of membrane hierarchy
    reservoir_size = 50     # 50 neurons per membrane
)

info = reservoir_info(reservoir)
println("Reservoir initialized:")
println("  ID: $(info["id"][1:8])...")
println("  Order: $(info["order"])")
println("  Number of rooted trees: $(info["num_trees"])")
println("  Number of membranes: $(info["num_membranes"])")
println("  Total neurons: $(info["total_neurons"])")
println()

# ============================================================================
# Part 2: Generate synthetic temporal data
# ============================================================================

println("Part 2: Generating synthetic temporal data")
println("-" ^ 70)

# Generate a simple sine wave with noise
t = 0:0.1:10
signal = sin.(2π * 0.5 * t) .+ 0.1 * randn(length(t))

# Prepare training data
training_data = reshape(signal[1:end-1], 1, :)
targets = reshape(signal[2:end], 1, :)

println("Generated $(size(training_data, 2)) time steps")
println("Signal range: [$(round(minimum(signal), digits=3)), $(round(maximum(signal), digits=3))]")
println()

# ============================================================================
# Part 3: Train the reservoir
# ============================================================================

println("Part 3: Training reservoir")
println("-" ^ 70)

train_reservoir!(reservoir, training_data, targets)
println("Training complete")
println()

# ============================================================================
# Part 4: Evaluate fitness
# ============================================================================

println("Part 4: Evaluating fitness")
println("-" ^ 70)

fitness = evaluate_fitness(reservoir)
println("Fitness: $(round(fitness, digits=4))")
println()

# ============================================================================
# Part 5: Evolve the reservoir
# ============================================================================

println("Part 5: Evolving single reservoir")
println("-" ^ 70)

println("Evolving for 10 generations...")
for gen in 1:10
    evolve!(reservoir, training_data)
    fitness = evaluate_fitness(reservoir)
    println("  Generation $gen: fitness = $(round(fitness, digits=4))")
end
println()

# ============================================================================
# Part 6: Evolve a population
# ============================================================================

println("Part 6: Evolving population of reservoirs")
println("-" ^ 70)

population = run_evolution(
    population_size = 10,
    max_generations = 20,
    order = 3,
    membrane_depth = 2,
    reservoir_size = 30,
    verbose = true
)

println()

# ============================================================================
# Part 7: Analyze results
# ============================================================================

println("Part 7: Analyzing evolution results")
println("-" ^ 70)

best = get_best_reservoir(population)
best_info = reservoir_info(best)

println("Best reservoir:")
println("  ID: $(best_info["id"][1:8])...")
println("  Generation: $(best_info["generation"])")
println("  Fitness: $(round(best.fitness, digits=4))")
println("  Total neurons: $(best_info["total_neurons"])")
println("  Lineage depth: $(best_info["lineage_depth"])")
println()

println("Population statistics:")
println("  Final generation: $(population.generation)")
println("  Best fitness: $(round(population.best_fitness_history[end], digits=4))")
println("  Avg fitness: $(round(population.avg_fitness_history[end], digits=4))")
println("  Diversity: $(round(population_diversity(population), digits=4))")
println()

# ============================================================================
# Part 8: Demonstrate B-series and rooted trees
# ============================================================================

println("Part 8: B-series genome and rooted trees (OEIS A000081)")
println("-" ^ 70)

println("Rooted trees by order:")
for order in 1:4
    trees_at_order = [t for t in best.rooted_trees if RootedTrees.order(t) == order]
    println("  Order $order: $(length(trees_at_order)) trees")
end
println()

println("Sample B-series coefficients:")
count = 0
for (tree, coeff) in best.bseries_genome.coefficients
    if count >= 5
        break
    end
    o = RootedTrees.order(tree)
    σ = RootedTrees.symmetry(tree)
    println("  Tree (order=$o, σ=$σ): coeff = $(round(coeff, digits=6))")
    count += 1
end
println("  ... ($(length(best.bseries_genome.coefficients)) total coefficients)")
println()

# ============================================================================
# Part 9: Membrane network structure
# ============================================================================

println("Part 9: Membrane network structure")
println("-" ^ 70)

println("Membrane hierarchy:")
for (mem_id, mem_res) in sort(collect(best.membrane_network.membranes), by=x->x[1])
    parent_id = mem_res.membrane.parent
    parent_str = parent_id === nothing ? "root" : "parent=$parent_id"
    state_size = length(mem_res.state)
    println("  Membrane $mem_id ($parent_str): $state_size neurons")
end
println()

# ============================================================================
# Summary
# ============================================================================

println("=" ^ 70)
println("Summary: Deep Tree Echo Reservoir Computer")
println("=" ^ 70)
println()
println("This example demonstrated the integration of:")
println("  ✓ B-Series ridges (Butcher coefficients as genetic code)")
println("  ✓ P-System reservoirs (membrane computing gardens)")
println("  ✓ Rooted trees (elementary differentials from OEIS A000081)")
println("  ✓ Echo State Networks (temporal pattern learning)")
println("  ✓ J-Surface dynamics (gradient-evolution integration)")
println()
println("The system successfully evolved through $(population.generation) generations,")
println("achieving a best fitness of $(round(population.best_fitness_history[end], digits=4)).")
println()
println("🌳 The tree remembers, and the echoes grow stronger.")
println()
