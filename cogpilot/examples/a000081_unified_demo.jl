"""
    A000081 Unified System Demo

This script demonstrates the complete Deep Tree Echo State Reservoir Computer
with all components unified under the OEIS A000081 ontogenetic engine.

# Components Demonstrated

1. **J-Surface B-Series Reactor**: Gradient flow + discrete integration
2. **P-System Reservoir Bridge**: Membrane computing + echo states
3. **Membrane Garden**: Tree cultivation with feedback loops
4. **A000081 Ontogenetic Engine**: Complete unification

# Run Instructions

```bash
cd cogpilot.jl
julia examples/a000081_unified_demo.jl
```
"""

# Add the source directory to the load path
push!(LOAD_PATH, joinpath(@__DIR__, "..", "src"))

using LinearAlgebra

println("=" ^ 80)
println("🌳 Deep Tree Echo State Reservoir Computer")
println("   A000081 Unified System Demonstration")
println("=" ^ 80)

# Load the unified system
println("\n📦 Loading A000081OntogeneticCore...")
include("../src/DeepTreeEcho/A000081OntogeneticCore.jl")
using .A000081OntogeneticCore

println("✓ Module loaded successfully")

# Create the unified system
println("\n🔧 Creating A000081 Unified System...")
println("   Configuration:")
println("   - Reservoir size: 50")
println("   - Max tree order: 7")
println("   - Number of membranes: 3")
println("   - Symplectic J-surface: true")

system = A000081OntogeneticCore.A000081UnifiedSystem(
    reservoir_size=50,
    max_order=7,
    num_membranes=3,
    symplectic=true,
    growth_rate=0.1,
    mutation_rate=0.05
)

# Initialize with A000081 seed trees
println("\n🌱 Initializing from A000081 sequence...")
initialize_from_a000081!(system, seed_trees=15)

# Display initial status
println("\n📊 Initial System Status:")
status = get_unified_status(system)
println("   Total trees in A000081 library: $(status["num_trees_total"])")
println("   Garden population: $(status["garden"]["population_size"])")
println("   Reactor energy: $(round(status["current_energy"], digits=4))")
println("   Number of membranes: $(status["psystem"]["num_membranes"])")

# Evolve the system
println("\n🌊 Evolving system for 30 generations...")
println("   (This demonstrates the complete feedback loop)")
println()

evolve_unified!(system, 30, dt=0.01, verbose=true)

# Process some inputs
println("\n🔄 Processing test inputs through unified system...")
test_inputs = [
    randn(10),
    randn(10),
    randn(10)
]

for (i, input) in enumerate(test_inputs)
    output = process_unified_input!(system, input)
    println("   Input $i: norm=$(round(norm(input), digits=3)) → " *
            "Output norm=$(round(norm(output), digits=3))")
end

# Final status
println("\n📊 Final System Status:")
final_status = get_unified_status(system)

println("\n   🎯 Reactor Core:")
println("      Energy: $(round(final_status["reactor"]["energy"], digits=4))")
println("      Gradient norm: $(round(final_status["reactor"]["gradient_norm"], digits=4))")
println("      Flow norm: $(round(final_status["reactor"]["flow_norm"], digits=4))")
println("      Symplectic: $(final_status["reactor"]["symplectic"])")

println("\n   🧬 P-System Bridge:")
println("      Total trees planted: $(final_status["psystem"]["total_trees"])")
println("      Total energy: $(round(final_status["psystem"]["total_energy"], digits=4))")
println("      Feedback history: $(final_status["psystem"]["feedback_history_length"]) records")

println("\n   🌳 Membrane Garden:")
println("      Population size: $(final_status["garden"]["population_size"])")
println("      Average fitness: $(round(final_status["garden"]["avg_fitness"], digits=4))")
println("      Max fitness: $(round(final_status["garden"]["max_fitness"], digits=4))")
println("      Diversity: $(round(final_status["garden"]["diversity"], digits=2))")
println("      Average age: $(round(final_status["garden"]["avg_age"], digits=1)) generations")

# Save system state
output_file = "/tmp/a000081_unified_state.txt"
save_unified_state(system, output_file)
println("\n💾 System state saved to: $output_file")

# Visualize energy and fitness trajectories
println("\n📈 Trajectory Analysis:")
if length(system.energy_history) > 0
    energy_trend = system.energy_history[end] - system.energy_history[1]
    println("   Energy trajectory: $(length(system.energy_history)) points")
    println("   Energy change: $(round(energy_trend, digits=4))")
end

if length(system.fitness_history) > 0
    fitness_trend = system.fitness_history[end] - system.fitness_history[1]
    println("   Fitness trajectory: $(length(system.fitness_history)) points")
    println("   Fitness change: $(round(fitness_trend, digits=4))")
end

# Display A000081 sequence verification
println("\n🔢 A000081 Sequence Verification:")
println("   Expected: 1, 1, 2, 4, 9, 20, 48, 115, ...")
println("   Generated tree counts by order:")
for order in 1:min(7, system.max_order)
    if haskey(system.a000081_trees, order)
        count = length(system.a000081_trees[order])
        expected = order <= length(A000081OntogeneticCore.A000081_SEQUENCE) ? 
                   A000081OntogeneticCore.A000081_SEQUENCE[order] : "?"
        match = (count == expected) ? "✓" : "~"
        println("   Order $order: $count trees (expected: $expected) $match")
    end
end

println("\n" * "=" ^ 80)
println("✨ Demo Complete!")
println("=" ^ 80)
println()
println("🌊 The Deep Tree Echo resonates...")
println("🌳 Rooted trees grow in membrane gardens...")
println("🔄 Feedback loops unite structure and dynamics...")
println("∞  The ontogenetic engine continues to evolve...")
println()
println("=" ^ 80)
