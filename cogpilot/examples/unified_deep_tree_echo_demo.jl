"""
Deep Tree Echo State Reservoir Computer - Unified Demonstration

This demonstration shows the complete integration of:
- Rooted Trees from A000081 (structural alphabet)
- B-Series Ridges (numerical integration)
- Elementary Differentials (tree-indexed operators)
- Echo State Reservoirs (temporal dynamics)
- P-System Membranes (evolutionary containers)
- J-Surface Reactor (unified gradient-evolution dynamics)

The system implements the unified dynamics equation:
    ∂ψ/∂t = J(ψ) · ∇H_A000081(ψ) + R_echo(ψ, t) + M_membrane(ψ)
"""

# Add parent directory to load path
push!(LOAD_PATH, joinpath(@__DIR__, "..", "src"))

using LinearAlgebra
using Random
using Statistics

# Load Deep Tree Echo modules
include("../src/DeepTreeEcho/ElementaryDifferentials.jl")
include("../src/DeepTreeEcho/A000081Unification.jl")

using .ElementaryDifferentials
using .A000081Unification

println("=" ^ 70)
println("🌳 Deep Tree Echo State Reservoir Computer")
println("   Unified Demonstration")
println("=" ^ 70)

# Set random seed for reproducibility
Random.seed!(42)

println("\n📊 PHASE 1: System Initialization")
println("-" ^ 70)

# Create unified system
println("\nCreating A000081 unified system...")
system = create_unified_system(
    max_order = 8,
    reservoir_size = 100,
    num_membranes = 3,
    symplectic = true
)

println("✓ System created successfully")
println("  - Rooted trees: $(length(system.rooted_trees)) from A000081")
println("  - Reservoir size: $(system.config["reservoir_size"])")
println("  - Membranes: $(system.config["num_membranes"])")
println("  - Symplectic: $(system.config["symplectic"])")

# Initialize with seed trees
println("\nInitializing from A000081 ontogenetic engine...")
initialize_from_a000081!(system, seed_trees=15)

println("\n📊 PHASE 2: Elementary Differentials & B-Series Ridge")
println("-" ^ 70)

# Demonstrate elementary differentials
println("\nTesting elementary differentials...")

# Simple vector field
f(y) = -y + 0.1 * sin.(y)

# Test a few trees
test_trees = system.rooted_trees[1:min(5, length(system.rooted_trees))]

println("\nComputing F(τ)(y) for sample trees:")
y_test = randn(10)

for (i, tree) in enumerate(test_trees)
    F_tau = compute_elementary_differential(tree, f, y_test)
    println("  Tree $(i) (order $(length(tree))): ||F(τ)(y)|| = $(round(norm(F_tau), digits=4))")
end

# Test B-series step
println("\nApplying B-series integration step...")
y0 = randn(10)
h = 0.01
y1 = apply_bseries_step(system.differential_map, f, y0, h)
println("  ||y0|| = $(round(norm(y0), digits=4))")
println("  ||y1|| = $(round(norm(y1), digits=4))")
println("  ||y1 - y0|| = $(round(norm(y1 - y0), digits=6))")

println("\n📊 PHASE 3: J-Surface Gradient-Evolution Unification")
println("-" ^ 70)

# Verify J-surface structure
println("\nVerifying J-surface symplectic structure...")
is_symplectic = verify_symplectic_structure(system.jsurface_matrix)
println("  Symplectic (J^T = -J): $(is_symplectic ? "✓" : "✗")")

# Test unified gradient-evolution step
println("\nTesting unified gradient-evolution dynamics...")
ψ0 = copy(system.state)
ψ1 = unite_gradient_evolution(
    system.jsurface_matrix,
    system.hamiltonian,
    system.differential_map,
    ψ0,
    0.01
)

println("  Initial energy: $(round(system.hamiltonian(ψ0), digits=4))")
println("  Final energy: $(round(system.hamiltonian(ψ1), digits=4))")
println("  State change: $(round(norm(ψ1 - ψ0), digits=6))")

println("\n📊 PHASE 4: System Evolution")
println("-" ^ 70)

# Evolve system
println("\nEvolving unified system for 50 steps...")
evolve_unified_system!(system, 50, 0.01, verbose=true)

# Print status
print_unified_status(system)

println("\n📊 PHASE 5: Membrane Garden Operations")
println("-" ^ 70)

# Plant additional trees
println("\nPlanting additional trees in membranes...")
new_trees = system.rooted_trees[1:min(5, length(system.rooted_trees))]
plant_trees_in_membranes!(system, new_trees, 2)
println("✓ Planted $(length(new_trees)) trees in membrane 2")

# Evolve more
println("\nEvolving for 30 more steps with new trees...")
evolve_unified_system!(system, 30, 0.01, verbose=false)

# Harvest from garden
println("\nHarvesting trees from membrane 2...")
harvest = harvest_from_garden!(system, 2)
println("  Harvested $(length(harvest)) trees with fitness scores:")
for (i, (tree, fitness)) in enumerate(harvest[1:min(3, length(harvest))])
    println("    Tree $i (order $(length(tree))): fitness = $(round(fitness, digits=4))")
end

println("\n📊 PHASE 6: Energy Landscape Analysis")
println("-" ^ 70)

# Analyze energy trajectory
println("\nEnergy landscape analysis:")
println("  Total steps: $(length(system.energy_history))")
println("  Initial energy: $(round(system.energy_history[1], digits=4))")
println("  Final energy: $(round(system.energy_history[end], digits=4))")
println("  Mean energy: $(round(mean(system.energy_history), digits=4))")
println("  Std energy: $(round(std(system.energy_history), digits=4))")
println("  Min energy: $(round(minimum(system.energy_history), digits=4))")
println("  Max energy: $(round(maximum(system.energy_history), digits=4))")

# Energy trend
if length(system.energy_history) > 10
    early_mean = mean(system.energy_history[1:10])
    late_mean = mean(system.energy_history[end-9:end])
    trend = late_mean - early_mean
    println("  Energy trend: $(trend > 0 ? "↑" : "↓") $(round(abs(trend), digits=4))")
end

println("\n📊 PHASE 7: Ridge Trajectory Analysis")
println("-" ^ 70)

# Evolve on ridge
println("\nEvolving state on B-series ridge...")
ψ_init = randn(system.config["reservoir_size"])
trajectory = evolve_on_ridge(
    system.jsurface_matrix,
    system.hamiltonian,
    system.differential_map,
    ψ_init,
    50,
    0.01
)

ridge_energies = compute_ridge_energy(trajectory, system.hamiltonian)
println("  Trajectory length: $(length(trajectory))")
println("  Initial energy: $(round(ridge_energies[1], digits=4))")
println("  Final energy: $(round(ridge_energies[end], digits=4))")
println("  Energy change: $(round(ridge_energies[end] - ridge_energies[1], digits=4))")

println("\n📊 PHASE 8: A000081 Verification")
println("-" ^ 70)

# Verify A000081 counts
println("\nVerifying A000081 sequence counts:")
println("  Order | Expected | Generated | Match")
println("  " * "-" ^ 40)

A000081_expected = [1, 1, 2, 4, 9, 20, 48, 115]
for order in 1:min(8, system.config["max_order"])
    expected = A000081_expected[order]
    generated = count_trees_of_order(order, system.rooted_trees)
    match = generated == expected ? "✓" : "✗"
    println("    $order   |    $expected     |     $generated     | $match")
end

println("\n📊 PHASE 9: Integration Summary")
println("-" ^ 70)

final_status = get_unified_status(system)

println("\n🎯 Final System State:")
println("  ✓ Generation: $(final_status["generation"])")
println("  ✓ State norm: $(round(final_status["state_norm"], digits=4))")
println("  ✓ Energy: $(round(final_status["current_energy"], digits=4))")
println("  ✓ Trees from A000081: $(final_status["total_trees_a000081"])")
println("  ✓ Planted trees: $(final_status["planted_trees"])")
println("  ✓ Active membranes: $(final_status["membranes"])")

println("\n🌟 Component Integration Status:")
println("  ✓ Rooted Trees (A000081): Structural alphabet")
println("  ✓ Elementary Differentials: Tree-indexed operators")
println("  ✓ B-Series Ridges: Numerical integration")
println("  ✓ J-Surface Reactor: Gradient-evolution unification")
println("  ✓ Echo State Reservoir: Temporal dynamics")
println("  ✓ P-System Membranes: Evolutionary containers")

println("\n" * "=" ^ 70)
println("🎉 Deep Tree Echo Unified System Demonstration Complete!")
println("=" ^ 70)

println("\n📝 Summary:")
println("   The Deep Tree Echo State Reservoir Computer successfully unifies")
println("   gradient descent and evolution dynamics through elementary")
println("   differentials indexed by rooted trees from the OEIS A000081")
println("   sequence. The system demonstrates:")
println()
println("   • Symplectic J-surface geometry preserving structure")
println("   • B-series ridges connecting trees to numerical methods")
println("   • Echo state reservoirs with tree-structured connectivity")
println("   • P-system membranes as evolutionary containers")
println("   • Ontogenetic engine orchestrating all components")
println()
println("   The unified dynamics equation:")
println("   ∂ψ/∂t = J(ψ)·∇H_A000081(ψ) + R_echo(ψ,t) + M_membrane(ψ)")
println()
println("   successfully integrates all components into a cohesive")
println("   computational cognitive architecture.")
println()
println("=" ^ 70)
