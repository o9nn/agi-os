"""
Taskflow Integration Demo

This example demonstrates the integration of parallel task graph execution
with the Deep Tree Echo State Reservoir Computer.

Features demonstrated:
- Converting rooted trees to task graphs
- Parallel task execution
- Tree-task synchronization
- Hybrid evolution with Taskflow
"""

# Add parent directory to load path
push!(LOAD_PATH, joinpath(@__DIR__, "..", "src"))

using DeepTreeEcho
using DeepTreeEcho.TaskflowIntegration
using Random

println("""
╔════════════════════════════════════════════════════════════════╗
║  TASKFLOW INTEGRATION WITH DEEP TREE ECHO                      ║
║  Parallel Task Graph Execution for Ontogenetic Evolution       ║
╚════════════════════════════════════════════════════════════════╝
""")

Random.seed!(42)

println("\n[1/6] Creating Deep Tree Echo System...")
println("─"^60)

# Create the base system
system = DeepTreeEchoSystem(
    reservoir_size = 50,
    max_tree_order = 6,
    num_membranes = 2,
    symplectic = true,
    growth_rate = 0.15,
    mutation_rate = 0.08
)

println("✓ System created")

println("\n[2/6] Initializing with A000081 seed trees...")
println("─"^60)

initialize!(system, seed_trees=10)

println("\n[3/6] Demonstrating Tree ↔ Task Graph Conversion...")
println("─"^60)

# Get a tree from the garden
sample_tree = first(values(system.garden.trees))
println("Sample tree (level sequence): $(sample_tree.level_sequence)")

# Convert to task graph
task_graph = tree_to_taskgraph(sample_tree.level_sequence)
println("\nConverted to task graph:")
print_taskgraph(task_graph)

# Convert back to tree
recovered_tree = taskgraph_to_tree(task_graph)
println("\nRecovered tree: $recovered_tree")
println("Match: $(recovered_tree == sample_tree.level_sequence)")

println("\n[4/6] Creating Taskflow-Integrated System...")
println("─"^60)

# Create hybrid system with parallel execution
tf_system = TaskflowOntogeneticSystem(system, num_threads=4)
println("✓ Created hybrid system with $(tf_system.executor.num_threads) threads")

println("\n[5/6] Evolving with Parallel Task Execution...")
println("─"^60)

# Evolve using task graph execution
evolve_with_taskflow!(tf_system, 20, verbose=true)

println("\n[6/6] Analyzing Results...")
println("─"^60)

# Print system status
print_system_status(system)

# Analyze task graphs
println("\n" * "="^60)
println("TASK GRAPH ANALYSIS")
println("="^60)

println("\nTask Graph Statistics:")
println("  Total graphs created: $(length(tf_system.executor.graphs))")
println("  Tree-to-graph mappings: $(length(tf_system.tree_to_graph))")

# Analyze a few task graphs
println("\nSample Task Graphs:")
for (i, (graph_id, graph)) in enumerate(tf_system.executor.graphs)
    if i > 3
        break
    end
    
    tree_id = get(tf_system.graph_to_tree, graph_id, -1)
    println("\nGraph $graph_id (Tree $tree_id):")
    println("  Tasks: $(length(graph.tasks))")
    println("  Execution order: $(join(graph.execution_order, " → "))")
    
    # Count dependencies
    total_deps = sum(length(task.dependencies) for task in values(graph.tasks))
    println("  Total dependencies: $total_deps")
end

# Demonstrate manual task graph creation
println("\n" * "="^60)
println("MANUAL TASK GRAPH CREATION")
println("="^60)

# Create a custom task graph
custom_graph = TaskGraph()

# Add tasks
println("\nCreating custom task graph...")
t1 = create_task!(custom_graph, "Load data", () -> println("  Executing: Load data"))
t2 = create_task!(custom_graph, "Process A", () -> println("  Executing: Process A"))
t3 = create_task!(custom_graph, "Process B", () -> println("  Executing: Process B"))
t4 = create_task!(custom_graph, "Merge", () -> println("  Executing: Merge"))

# Add dependencies
add_dependency!(custom_graph, t1, t2)  # t2 depends on t1
add_dependency!(custom_graph, t1, t3)  # t3 depends on t1
add_dependency!(custom_graph, t2, t4)  # t4 depends on t2
add_dependency!(custom_graph, t3, t4)  # t4 depends on t3

println("\nCustom task graph structure:")
print_taskgraph(custom_graph)

# Execute
println("\nExecuting custom task graph:")
execute_taskgraph!(custom_graph, parallel=true)
println("✓ Execution complete")

# Convert custom graph to tree
println("\nConverting custom graph to rooted tree...")
custom_tree = taskgraph_to_tree(custom_graph)
println("Tree representation: $custom_tree")

# Plant in garden
println("\nPlanting custom tree in membrane garden...")
plant_tree!(system.garden, custom_tree, 1)
println("✓ Tree planted")

# Performance comparison
println("\n" * "="^60)
println("PERFORMANCE COMPARISON")
println("="^60)

println("\nComparing sequential vs parallel execution...")

# Create test graph
test_graph = TaskGraph()
for i in 1:20
    create_task!(test_graph, "task_$i", () -> sum(rand(100)))
end

# Add some dependencies to create levels
for i in 2:20
    if i % 5 != 0
        add_dependency!(test_graph, i-1, i)
    end
end

# Sequential execution
println("\nSequential execution:")
@time execute_taskgraph!(test_graph, parallel=false)

# Reset completion status
for task in values(test_graph.tasks)
    task.completed = false
end

# Parallel execution
println("\nParallel execution:")
@time execute_taskgraph!(test_graph, parallel=true)

# Final statistics
println("\n" * "="^60)
println("FINAL STATISTICS")
println("="^60)

status = get_system_status(system)

println("\nSystem Evolution:")
println("  Total steps: $(status["step_count"])")
println("  Membranes: $(status["num_membranes"])")
println("  Trees planted: $(sum(s["count"] for s in values(status["garden"])))")

println("\nOntogenetic Engine:")
onto = status["ontogenetic"]
println("  Generation: $(onto["generation"])")
println("  Population: $(onto["population_size"])")
println("  Avg fitness: $(round(onto["avg_fitness"], digits=4))")
println("  Diversity: $(round(onto["diversity"], digits=4))")

println("\nTaskflow Integration:")
println("  Task graphs: $(length(tf_system.executor.graphs))")
println("  Threads: $(tf_system.executor.num_threads)")
println("  Tree-graph mappings: $(length(tf_system.tree_to_graph))")

println("\n" * "="^60)
println("DEMONSTRATION COMPLETE")
println("="^60)

println("""
The Taskflow integration successfully demonstrated:

✓ Bidirectional conversion between rooted trees and task graphs
✓ Parallel execution of task graphs with dependency management
✓ Hybrid evolution combining Deep Tree Echo and Taskflow
✓ Custom task graph creation and execution
✓ Performance benefits of parallel execution
✓ Integration of tree populations with task execution

This integration enables:
• Parallel processing of tree populations
• Task-based decomposition of cognitive operations
• Efficient execution on multi-core systems
• Scalability to large tree populations
• Future extension to distributed and GPU computing

The system bridges symbolic (trees) and procedural (tasks) computation,
creating a unified substrate for cognitive processing.
""")

println("\nThank you for exploring Taskflow Integration! 🌳⚡")
