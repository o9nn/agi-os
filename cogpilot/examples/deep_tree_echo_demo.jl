"""
Deep Tree Echo State Reservoir Computer - Demonstration
This example demonstrates the integrated Deep Tree Echo system that unifies:
- Rooted trees from OEIS A000081
- B-series computational ridges
- Echo state reservoir computing
- P-system membrane computing
- J-surface gradient-evolution dynamics
- Membrane computing gardens
The system evolves through ontogenetic dynamics, growing and adapting
its structure based on feedback from rooted trees planted in membrane gardens.
"""
# Add parent directory to load path
push!(LOAD_PATH, joinpath(@__DIR__, "..", "src"))
using DeepTreeEcho
using Random
using LinearAlgebra
println("""
╔════════════════════════════════════════════════════════════════╗
║  DEEP TREE ECHO STATE RESERVOIR COMPUTER                       ║
║  Unified Cognitive Architecture                                ║
║                                                                ║
║  Components:                                                   ║
║  • A000081 Ontogenetic Engine                                 ║
║  • B-Series Computational Ridges                              ║
║  • J-Surface Reactor Core                                     ║
║  • P-System Membrane Reservoirs                               ║
║  • Membrane Computing Gardens                                 ║
╚════════════════════════════════════════════════════════════════╝
""")
# Set random seed for reproducibility
Random.seed!(42)
println("\n[1/5] Creating Deep Tree Echo System...")
println("─"^60)
# Create the system using A000081-derived parameters
# Base order 5: reservoir_size=17, max_tree_order=8, num_membranes=2
params = get_parameter_set(5, membrane_order=3, max_order_offset=3)
println("\n🌳 Using A000081-derived parameters:")
# Optionally show detailed parameter explanation (can be verbose)
# explain_parameters(params)  # Uncomment for detailed derivation info
println("  reservoir_size  = $(params.reservoir_size) (cumulative A000081[1:5])")
println("  max_tree_order  = $(params.max_tree_order)")
println("  num_membranes   = $(params.num_membranes) (A000081[3])")
println("  growth_rate     = $(round(params.growth_rate, digits=4)) (20/9)")
println("  mutation_rate   = $(round(params.mutation_rate, digits=4)) (1/9)")
println()
system = DeepTreeEchoSystem(
reservoir_size = params.reservoir_size,   # 17 (1+1+2+4+9)
max_tree_order = params.max_tree_order,   # 8
num_membranes = params.num_membranes,     # 2 (A000081[3])
symplectic = true,
growth_rate = params.growth_rate,         # ≈2.22 (20/9)
mutation_rate = params.mutation_rate      # ≈0.11 (1/9)
)
println("\n✓ System created with A000081-aligned configuration:")
for (key, value) in system.config
println("  • $key: $value")
end
println("\n[2/5] Initializing with A000081 seed trees...")
println("─"^60)
# Initialize with seed trees - use A000081[4] = 4 trees
# This aligns with the tree count at order 4
seed_count = A000081Parameters.A000081_SEQUENCE[4]  # 4 trees
initialize!(system, seed_trees=seed_count)
println("\n[3/5] Evolving system for 30 generations...")
println("─"^60)
# Evolve the system
evolve!(system, 30, dt=0.01, verbose=true)
println("\n[4/5] Processing input through the system...")
println("─"^60)
# Create test input
input = randn(10)
println("Input vector: $(round.(input, digits=2))")
# Process through system
output = process_input!(system, input)
println("Output vector: $(round.(output, digits=2))")
# Measure response
println("\nSystem response characteristics:")
println("  • Input norm: $(round(norm(input), digits=4))")
println("  • Output norm: $(round(norm(output), digits=4))")
println("  • Correlation: $(round(cor(input, output), digits=4))")
println("\n[5/5] Final system status...")
println("─"^60)
# Display comprehensive status
print_system_status(system)
# Get detailed statistics
println("\n" * "="^60)
println("DETAILED STATISTICS")
println("="^60)
status = get_system_status(system)
println("\nOntogenetic Engine (A000081):")
onto = status["ontogenetic"]
println("  • Generation: $(onto["generation"])")
println("  • Population: $(onto["population_size"]) trees")
println("  • Complexity: $(onto["complexity_level"])")
println("  • Fitness: $(round(onto["avg_fitness"], digits=4)) (avg)")
println("  • Diversity: $(round(onto["diversity"], digits=4))")
println("  • Total nodes: $(onto["total_nodes"])")
println("\nMembrane Gardens:")
for (membrane_id, stats) in status["garden"]
if stats["count"] > 0
println("  Membrane $membrane_id:")
println("    • Trees: $(stats["count"])")
println("    • Avg fitness: $(round(stats["avg_fitness"], digits=3))")
println("    • Avg age: $(round(stats["avg_age"], digits=1))")
println("    • Avg size: $(round(stats["avg_size"], digits=1))")
end
end
println("\nJ-Surface Reactor:")
println("  • Energy: $(round(status["jsurface_energy"], digits=6))")
println("  • Dimension: $(system.jsurface.dimension)")
println("  • Structure: $(system.config["symplectic"] ? "Symplectic" : "Poisson")")
println("\nB-Series Ridge:")
println("  • Order: $(status["ridge_order"])")
println("  • Trees: $(system.ridge.dimension)")
println("  • Coefficient norm: $(round(norm(system.ridge.coefficients), digits=4))")
println("\nSystem Integration:")
println("  • Total evolution steps: $(status["step_count"])")
println("  • Active membranes: $(status["num_membranes"])")
println("  • Trees planted: $(sum(s["count"] for s in values(status["garden"])))")
# Demonstrate adaptation
println("\n" * "="^60)
println("ADAPTIVE TOPOLOGY DEMONSTRATION")
println("="^60)
println("\nAdding new membrane...")
adapt_topology!(system, add_membrane=true)
println("\nPlanting trees in new membrane...")
new_membrane_id = maximum(keys(system.reservoir.membranes))
new_trees = generate_a000081_trees(system.generator, 4)
plant_trees!(system, new_trees[1:3], new_membrane_id)
println("✓ Planted $(length(new_trees[1:3])) trees in membrane $new_membrane_id")
# Evolve with new topology
println("\nEvolving with adapted topology...")
evolve!(system, 10, verbose=false)
println("\nFinal membrane structure:")
print_membrane_structure(system.reservoir)
# Harvest feedback
println("\n" * "="^60)
println("FEEDBACK HARVESTING")
println("="^60)
feedback = harvest_feedback!(system)
println("\nHarvested feedback from $(length(feedback)) membranes:")
for (membrane_id, fb) in feedback
println("  Membrane $membrane_id: $(length(fb)) values, norm=$(round(norm(fb), digits=4))")
end
# Save system state
println("\n" * "="^60)
println("SAVING SYSTEM STATE")
println("="^60)
output_file = joinpath(@__DIR__, "deep_tree_echo_state.txt")
save_system_state(system, output_file)
println("\n" * "="^60)
println("DEMONSTRATION COMPLETE")
println("="^60)
println("""
The Deep Tree Echo system has successfully:
✓ Generated rooted trees from OEIS A000081
✓ Constructed B-series computational ridges
✓ Initialized echo state reservoirs in membranes
✓ Planted trees in membrane computing gardens
✓ Evolved through ontogenetic dynamics
✓ Integrated gradient flow and evolution on J-surface
✓ Processed inputs through the unified architecture
✓ Adapted topology by adding membranes
✓ Harvested feedback from tree populations
✓ Demonstrated self-organization and emergence
The system embodies the principle that rooted trees, when planted
in membrane gardens and evolved through B-series ridges on a J-surface,
create a universal computational substrate capable of learning,
adaptation, and self-evolution.
A000081 sequence: The number of unlabeled rooted trees with n nodes
serves as the ontogenetic generator for the entire architecture,
unifying discrete tree structures with continuous dynamics.
""")
println("\nSystem state saved to: $output_file")
println("\nThank you for exploring Deep Tree Echo! 🌳🌊")