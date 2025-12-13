"""
    Visualization

Visualization module for Deep Tree Echo State Reservoir Computer.

Provides functions to visualize:
- Rooted trees and tree populations
- J-surface energy landscapes
- Membrane hierarchies
- Evolution trajectories
- Task graphs
- System state over time

Requires: Plots.jl (optional dependency)
"""
module Visualization

using LinearAlgebra

export plot_tree, plot_tree_population, plot_jsurface_trajectory
export plot_membrane_hierarchy, plot_evolution_history
export plot_task_graph, plot_system_dashboard
export save_visualization

# Check if Plots.jl is available
const PLOTS_AVAILABLE = try
    using Plots
    true
catch
    false
end

if !PLOTS_AVAILABLE
    @warn "Plots.jl not available. Visualization functions will generate text output only."
end

"""
    plot_tree(level_sequence::Vector{Int}; title::String="Rooted Tree")

Visualize a single rooted tree.
"""
function plot_tree(level_sequence::Vector{Int}; title::String="Rooted Tree")
    if PLOTS_AVAILABLE
        # Create tree visualization using Plots.jl
        n = length(level_sequence)
        
        # Compute positions for nodes
        positions = compute_tree_layout(level_sequence)
        
        # Create plot
        p = plot(
            title=title,
            legend=false,
            axis=false,
            grid=false,
            size=(600, 400)
        )
        
        # Draw edges
        for i in 2:n
            parent_level = level_sequence[i] - 1
            for j in (i-1):-1:1
                if level_sequence[j] == parent_level
                    plot!(p, [positions[j][1], positions[i][1]], 
                            [positions[j][2], positions[i][2]],
                            color=:black, linewidth=2)
                    break
                end
            end
        end
        
        # Draw nodes
        scatter!(p, [pos[1] for pos in positions], 
                   [pos[2] for pos in positions],
                   markersize=10, color=:lightblue, 
                   markerstrokewidth=2, markerstrokecolor=:black)
        
        return p
    else
        # Text-based visualization
        println("\n$title:")
        print_tree_ascii(level_sequence)
        return nothing
    end
end

"""
    compute_tree_layout(level_sequence::Vector{Int})

Compute 2D positions for tree nodes.
"""
function compute_tree_layout(level_sequence::Vector{Int})
    n = length(level_sequence)
    positions = Vector{Tuple{Float64, Float64}}(undef, n)
    
    # Count nodes at each level
    max_level = maximum(level_sequence)
    level_counts = zeros(Int, max_level)
    level_indices = zeros(Int, max_level)
    
    for level in level_sequence
        level_counts[level] += 1
    end
    
    # Assign positions
    for i in 1:n
        level = level_sequence[i]
        level_indices[level] += 1
        
        # x position: spread nodes at same level
        x = (level_indices[level] - 1) - (level_counts[level] - 1) / 2
        
        # y position: level (inverted for top-down tree)
        y = -(level - 1)
        
        positions[i] = (x, y)
    end
    
    return positions
end

"""
    print_tree_ascii(level_sequence::Vector{Int})

Print ASCII representation of tree.
"""
function print_tree_ascii(level_sequence::Vector{Int})
    n = length(level_sequence)
    max_level = maximum(level_sequence)
    
    # Build tree structure
    for level in 1:max_level
        nodes_at_level = [i for i in 1:n if level_sequence[i] == level]
        indent = "  " ^ (level - 1)
        
        for node in nodes_at_level
            println("$(indent)├─ Node $node (level $level)")
        end
    end
end

"""
    plot_tree_population(trees::Dict; title::String="Tree Population")

Visualize a population of trees.
"""
function plot_tree_population(trees::Dict; title::String="Tree Population")
    if PLOTS_AVAILABLE
        # Create grid of tree plots
        n_trees = min(length(trees), 9)  # Max 9 trees in 3x3 grid
        tree_list = collect(values(trees))[1:n_trees]
        
        plots = []
        for (i, tree) in enumerate(tree_list)
            p = plot_tree(tree.level_sequence, title="Tree $(tree.tree_id)")
            push!(plots, p)
        end
        
        # Combine into grid
        return plot(plots..., layout=(3, 3), size=(1200, 1200))
    else
        println("\n$title:")
        for (tree_id, tree) in trees
            println("\nTree $tree_id (fitness=$(round(tree.fitness, digits=3))):")
            print_tree_ascii(tree.level_sequence)
        end
        return nothing
    end
end

"""
    plot_jsurface_trajectory(states::Vector; title::String="J-Surface Trajectory")

Plot trajectory on J-surface.
"""
function plot_jsurface_trajectory(states::Vector; title::String="J-Surface Trajectory")
    if PLOTS_AVAILABLE
        # Extract first 2-3 dimensions for visualization
        dim = min(3, length(states[1]))
        
        if dim == 2
            # 2D trajectory
            x = [s[1] for s in states]
            y = [s[2] for s in states]
            
            p = plot(x, y, 
                    title=title,
                    xlabel="Dimension 1",
                    ylabel="Dimension 2",
                    linewidth=2,
                    marker=:circle,
                    markersize=3,
                    legend=false)
        else
            # 3D trajectory
            x = [s[1] for s in states]
            y = [s[2] for s in states]
            z = [s[3] for s in states]
            
            p = plot(x, y, z,
                    title=title,
                    xlabel="Dimension 1",
                    ylabel="Dimension 2",
                    zlabel="Dimension 3",
                    linewidth=2,
                    marker=:circle,
                    markersize=3,
                    legend=false)
        end
        
        return p
    else
        println("\n$title:")
        println("Trajectory through $(length(states)) states")
        println("Initial state: $(round.(states[1][1:min(3, end)], digits=3))")
        println("Final state: $(round.(states[end][1:min(3, end)], digits=3))")
        return nothing
    end
end

"""
    plot_membrane_hierarchy(reservoir; title::String="Membrane Hierarchy")

Visualize membrane structure.
"""
function plot_membrane_hierarchy(reservoir; title::String="Membrane Hierarchy")
    if PLOTS_AVAILABLE
        # Create hierarchical layout
        # (Simplified: show as tree structure)
        
        p = plot(
            title=title,
            legend=false,
            axis=false,
            grid=false,
            size=(600, 400)
        )
        
        # Draw membranes as nodes
        # (Implementation would traverse membrane hierarchy)
        
        return p
    else
        println("\n$title:")
        print_membrane_structure_ascii(reservoir)
        return nothing
    end
end

"""
    print_membrane_structure_ascii(reservoir)

Print ASCII representation of membrane hierarchy.
"""
function print_membrane_structure_ascii(reservoir)
    println("Root membrane: $(reservoir.root_id)")
    
    for (membrane_id, membrane) in reservoir.membranes
        parent = membrane.parent_id
        children = membrane.children
        
        if parent === nothing
            println("├─ Membrane $membrane_id (root)")
        else
            println("  ├─ Membrane $membrane_id (parent: $parent)")
        end
        
        if !isempty(children)
            for child in children
                println("    └─ Child: $child")
            end
        end
    end
end

"""
    plot_evolution_history(history::Vector{Dict}; title::String="Evolution History")

Plot evolution metrics over time.
"""
function plot_evolution_history(history::Vector{Dict}; title::String="Evolution History")
    if PLOTS_AVAILABLE
        generations = [h["generation"] for h in history]
        fitness = [h["avg_fitness"] for h in history]
        diversity = [h["diversity"] for h in history]
        
        p1 = plot(generations, fitness,
                 title="Average Fitness",
                 xlabel="Generation",
                 ylabel="Fitness",
                 linewidth=2,
                 legend=false)
        
        p2 = plot(generations, diversity,
                 title="Population Diversity",
                 xlabel="Generation",
                 ylabel="Diversity",
                 linewidth=2,
                 legend=false,
                 color=:red)
        
        return plot(p1, p2, layout=(2, 1), size=(800, 600))
    else
        println("\n$title:")
        println("Generation | Fitness | Diversity")
        println("-" ^ 40)
        for h in history
            println("$(h["generation"]) | $(round(h["avg_fitness"], digits=3)) | $(round(h["diversity"], digits=3))")
        end
        return nothing
    end
end

"""
    plot_task_graph(graph; title::String="Task Graph")

Visualize task graph structure.
"""
function plot_task_graph(graph; title::String="Task Graph")
    if PLOTS_AVAILABLE
        # Compute layout for task graph
        positions = compute_dag_layout(graph)
        
        p = plot(
            title=title,
            legend=false,
            axis=false,
            grid=false,
            size=(800, 600)
        )
        
        # Draw edges (dependencies)
        for (task_id, task) in graph.tasks
            for dep_id in task.dependencies
                if haskey(positions, dep_id) && haskey(positions, task_id)
                    plot!(p, [positions[dep_id][1], positions[task_id][1]],
                            [positions[dep_id][2], positions[task_id][2]],
                            arrow=true, color=:gray, linewidth=2)
                end
            end
        end
        
        # Draw nodes (tasks)
        x_coords = [positions[id][1] for id in keys(graph.tasks)]
        y_coords = [positions[id][2] for id in keys(graph.tasks)]
        
        scatter!(p, x_coords, y_coords,
                markersize=15,
                color=:lightgreen,
                markerstrokewidth=2,
                markerstrokecolor=:black)
        
        # Add labels
        for (task_id, task) in graph.tasks
            pos = positions[task_id]
            annotate!(p, pos[1], pos[2], text(string(task_id), 10, :center))
        end
        
        return p
    else
        println("\n$title:")
        print_task_graph_ascii(graph)
        return nothing
    end
end

"""
    compute_dag_layout(graph)

Compute layout for directed acyclic graph.
"""
function compute_dag_layout(graph)
    # Use level-based layout
    levels = Dict{Int, Int}()
    
    # Compute levels
    for task_id in keys(graph.tasks)
        levels[task_id] = 0
    end
    
    for task_id in graph.execution_order
        task = graph.tasks[task_id]
        if !isempty(task.dependencies)
            max_dep_level = maximum(levels[dep_id] for dep_id in task.dependencies)
            levels[task_id] = max_dep_level + 1
        end
    end
    
    # Assign positions
    positions = Dict{Int, Tuple{Float64, Float64}}()
    level_counts = Dict{Int, Int}()
    
    for (task_id, level) in levels
        count = get(level_counts, level, 0)
        level_counts[level] = count + 1
        
        x = count * 2.0
        y = -level * 2.0
        
        positions[task_id] = (x, y)
    end
    
    return positions
end

"""
    print_task_graph_ascii(graph)

Print ASCII representation of task graph.
"""
function print_task_graph_ascii(graph)
    println("Tasks: $(length(graph.tasks))")
    println("Execution order: $(join(graph.execution_order, " → "))")
    println("\nDependencies:")
    
    for (task_id, task) in sort(collect(graph.tasks), by=x->x[1])
        deps = isempty(task.dependencies) ? "none" : join(task.dependencies, ", ")
        status = task.completed ? "✓" : "○"
        println("  $status Task $task_id: depends on [$deps]")
    end
end

"""
    plot_system_dashboard(system; title::String="System Dashboard")

Create comprehensive dashboard of system state.
"""
function plot_system_dashboard(system; title::String="System Dashboard")
    if PLOTS_AVAILABLE
        # Create multi-panel dashboard
        
        # Panel 1: Tree population
        p1 = plot_tree_population(system.garden.trees, title="Tree Population")
        
        # Panel 2: J-surface energy
        energy = system.jsurface.hamiltonian(system.jsurface_state.position)
        p2 = bar([energy], title="J-Surface Energy", legend=false, color=:blue)
        
        # Panel 3: Membrane structure
        p3 = plot_membrane_hierarchy(system.reservoir, title="Membranes")
        
        # Panel 4: System metrics
        stats = get_ontogenetic_statistics(system.ontogenetic_state)
        metrics = [stats["avg_fitness"], stats["diversity"], stats["complexity_level"] / 10]
        p4 = bar(["Fitness", "Diversity", "Complexity"], metrics,
                title="System Metrics", legend=false, color=:green)
        
        return plot(p1, p2, p3, p4, layout=(2, 2), size=(1200, 1000))
    else
        println("\n$title:")
        println("="^60)
        
        # Print text-based dashboard
        println("\n[Tree Population]")
        println("  Trees: $(length(system.garden.trees))")
        
        println("\n[J-Surface]")
        energy = system.jsurface.hamiltonian(system.jsurface_state.position)
        println("  Energy: $(round(energy, digits=6))")
        
        println("\n[Membranes]")
        println("  Count: $(length(system.reservoir.membranes))")
        
        println("\n[System Metrics]")
        stats = get_ontogenetic_statistics(system.ontogenetic_state)
        println("  Fitness: $(round(stats["avg_fitness"], digits=3))")
        println("  Diversity: $(round(stats["diversity"], digits=3))")
        println("  Complexity: $(stats["complexity_level"])")
        
        println("="^60)
        return nothing
    end
end

"""
    save_visualization(plot_object, filename::String)

Save visualization to file.
"""
function save_visualization(plot_object, filename::String)
    if PLOTS_AVAILABLE && plot_object !== nothing
        savefig(plot_object, filename)
        println("Visualization saved to $filename")
    else
        println("Cannot save: Plots.jl not available or no plot object")
    end
end

"""
    get_ontogenetic_statistics(state)

Helper to get statistics (imported from OntogeneticEngine).
"""
function get_ontogenetic_statistics(state)
    # This would normally be imported from OntogeneticEngine
    # Placeholder implementation
    return Dict(
        "generation" => get(state, :generation, 0),
        "avg_fitness" => 0.5,
        "diversity" => 0.7,
        "complexity_level" => 5
    )
end

end # module Visualization
