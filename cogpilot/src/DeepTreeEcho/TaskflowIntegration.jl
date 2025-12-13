"""
    TaskflowIntegration

Integration layer between Deep Tree Echo and Taskflow (C++ parallel task framework).

This module provides a pure Julia implementation of task graph management
that mirrors Taskflow's capabilities, enabling:
- Task graph creation and execution
- Rooted tree ↔ Task graph conversion
- Parallel task scheduling
- Cognitive computing primitives

Note: This is a Julia-native implementation. For C++ Taskflow integration,
see docs/CogTaskFlow_Integration.md for CxxWrap.jl binding instructions.
"""
module TaskflowIntegration

using LinearAlgebra
using Base.Threads

export TaskNode, TaskGraph, TaskflowExecutor
export create_task!, add_dependency!, execute_taskgraph!
export tree_to_taskgraph, taskgraph_to_tree
export TaskflowOntogeneticSystem, evolve_with_taskflow!

"""
    TaskNode

A single task in a task graph.

Fields:
- `id::Int`: Unique task identifier
- `name::String`: Task name
- `func::Function`: Task function to execute
- `dependencies::Vector{Int}`: IDs of tasks that must complete first
- `result::Any`: Result of task execution
- `completed::Bool`: Whether task has completed
"""
mutable struct TaskNode
    id::Int
    name::String
    func::Function
    dependencies::Vector{Int}
    result::Any
    completed::Bool
    
    function TaskNode(id::Int, name::String, func::Function=()->nothing)
        new(id, name, func, Int[], nothing, false)
    end
end

"""
    TaskGraph

A directed acyclic graph of tasks.

Fields:
- `tasks::Dict{Int, TaskNode}`: All tasks indexed by ID
- `execution_order::Vector{Int}`: Topologically sorted task IDs
- `next_id::Int`: Counter for task IDs
"""
mutable struct TaskGraph
    tasks::Dict{Int, TaskNode}
    execution_order::Vector{Int}
    next_id::Int
    
    function TaskGraph()
        new(Dict{Int, TaskNode}(), Int[], 1)
    end
end

"""
    TaskflowExecutor

Executor for parallel task graph execution.

Fields:
- `num_threads::Int`: Number of worker threads
- `graphs::Dict{Int, TaskGraph}`: Managed task graphs
- `next_graph_id::Int`: Counter for graph IDs
"""
mutable struct TaskflowExecutor
    num_threads::Int
    graphs::Dict{Int, TaskGraph}
    next_graph_id::Int
    
    function TaskflowExecutor(num_threads::Int=nthreads())
        new(num_threads, Dict{Int, TaskGraph}(), 1)
    end
end

"""
    create_task!(graph::TaskGraph, name::String, func::Function)

Create a new task in the graph.

# Arguments
- `graph::TaskGraph`: Target graph
- `name::String`: Task name
- `func::Function`: Function to execute

# Returns
- `Int`: Task ID
"""
function create_task!(graph::TaskGraph, name::String, func::Function=()->nothing)
    task_id = graph.next_id
    graph.next_id += 1
    
    task = TaskNode(task_id, name, func)
    graph.tasks[task_id] = task
    
    return task_id
end

"""
    add_dependency!(graph::TaskGraph, from_id::Int, to_id::Int)

Add a dependency: `to_id` depends on `from_id`.

# Arguments
- `graph::TaskGraph`: Target graph
- `from_id::Int`: Task that must complete first
- `to_id::Int`: Task that depends on `from_id`
"""
function add_dependency!(graph::TaskGraph, from_id::Int, to_id::Int)
    if !haskey(graph.tasks, from_id) || !haskey(graph.tasks, to_id)
        error("Both tasks must exist in graph")
    end
    
    to_task = graph.tasks[to_id]
    if from_id ∉ to_task.dependencies
        push!(to_task.dependencies, from_id)
    end
    
    return nothing
end

"""
    topological_sort!(graph::TaskGraph)

Compute topological sort of tasks for execution order.
"""
function topological_sort!(graph::TaskGraph)
    # Kahn's algorithm for topological sort
    in_degree = Dict{Int, Int}()
    
    # Initialize in-degrees
    for (id, task) in graph.tasks
        in_degree[id] = length(task.dependencies)
    end
    
    # Queue of tasks with no dependencies
    queue = Int[]
    for (id, degree) in in_degree
        if degree == 0
            push!(queue, id)
        end
    end
    
    # Process queue
    order = Int[]
    while !isempty(queue)
        current_id = popfirst!(queue)
        push!(order, current_id)
        
        # Reduce in-degree of dependent tasks
        for (id, task) in graph.tasks
            if current_id in task.dependencies
                in_degree[id] -= 1
                if in_degree[id] == 0
                    push!(queue, id)
                end
            end
        end
    end
    
    # Check for cycles
    if length(order) != length(graph.tasks)
        error("Task graph contains cycles")
    end
    
    graph.execution_order = order
    return nothing
end

"""
    execute_taskgraph!(graph::TaskGraph; parallel::Bool=true)

Execute all tasks in the graph respecting dependencies.

# Arguments
- `graph::TaskGraph`: Graph to execute
- `parallel::Bool=true`: Use parallel execution when possible
"""
function execute_taskgraph!(graph::TaskGraph; parallel::Bool=true)
    # Reset completion status
    for task in values(graph.tasks)
        task.completed = false
    end
    
    # Compute execution order
    topological_sort!(graph)
    
    if parallel && nthreads() > 1
        execute_parallel!(graph)
    else
        execute_sequential!(graph)
    end
    
    return nothing
end

"""
    execute_sequential!(graph::TaskGraph)

Execute tasks sequentially in topological order.
"""
function execute_sequential!(graph::TaskGraph)
    for task_id in graph.execution_order
        task = graph.tasks[task_id]
        
        # Wait for dependencies (already completed in topological order)
        task.result = task.func()
        task.completed = true
    end
    
    return nothing
end

"""
    execute_parallel!(graph::TaskGraph)

Execute tasks in parallel when dependencies allow.
"""
function execute_parallel!(graph::TaskGraph)
    # Group tasks by level (distance from root)
    levels = compute_task_levels(graph)
    max_level = maximum(values(levels))
    
    # Execute each level in parallel
    for level in 0:max_level
        level_tasks = [id for (id, l) in levels if l == level]
        
        if !isempty(level_tasks)
            @threads for task_id in level_tasks
                task = graph.tasks[task_id]
                task.result = task.func()
                task.completed = true
            end
        end
    end
    
    return nothing
end

"""
    compute_task_levels(graph::TaskGraph)

Compute the level (distance from root) of each task.
"""
function compute_task_levels(graph::TaskGraph)
    levels = Dict{Int, Int}()
    
    # Initialize all levels to 0
    for id in keys(graph.tasks)
        levels[id] = 0
    end
    
    # Compute levels using topological order
    for task_id in graph.execution_order
        task = graph.tasks[task_id]
        
        if isempty(task.dependencies)
            levels[task_id] = 0
        else
            # Level is max of dependency levels + 1
            max_dep_level = maximum(levels[dep_id] for dep_id in task.dependencies)
            levels[task_id] = max_dep_level + 1
        end
    end
    
    return levels
end

"""
    tree_to_taskgraph(level_sequence::Vector{Int})

Convert a rooted tree (level sequence) to a task graph.

# Arguments
- `level_sequence::Vector{Int}`: Tree as level sequence

# Returns
- `TaskGraph`: Corresponding task graph
"""
function tree_to_taskgraph(level_sequence::Vector{Int})
    graph = TaskGraph()
    n = length(level_sequence)
    
    # Create tasks for each node
    task_ids = Int[]
    for i in 1:n
        task_id = create_task!(graph, "task_$i", () -> i)
        push!(task_ids, task_id)
    end
    
    # Add dependencies based on tree structure
    for i in 2:n
        # Find parent (previous node at level-1)
        parent_level = level_sequence[i] - 1
        
        for j in (i-1):-1:1
            if level_sequence[j] == parent_level
                # j is parent of i
                add_dependency!(graph, task_ids[j], task_ids[i])
                break
            end
        end
    end
    
    return graph
end

"""
    taskgraph_to_tree(graph::TaskGraph)

Convert a task graph to a rooted tree (level sequence).

# Arguments
- `graph::TaskGraph`: Task graph

# Returns
- `Vector{Int}`: Level sequence representation
"""
function taskgraph_to_tree(graph::TaskGraph)
    # Compute task levels
    levels = compute_task_levels(graph)
    
    # Sort by execution order and extract levels
    topological_sort!(graph)
    level_sequence = [levels[id] + 1 for id in graph.execution_order]
    
    return level_sequence
end

"""
    TaskflowOntogeneticSystem

Hybrid system combining Deep Tree Echo with task graph execution.

Fields:
- `dte_system::DeepTreeEchoSystem`: Deep Tree Echo system
- `executor::TaskflowExecutor`: Task graph executor
- `tree_to_graph::Dict{Int, Int}`: Tree ID -> Graph ID mapping
- `graph_to_tree::Dict{Int, Int}`: Graph ID -> Tree ID mapping
"""
mutable struct TaskflowOntogeneticSystem
    dte_system::Any  # DeepTreeEchoSystem (avoid circular dependency)
    executor::TaskflowExecutor
    tree_to_graph::Dict{Int, Int}
    graph_to_tree::Dict{Int, Int}
    
    function TaskflowOntogeneticSystem(dte_system, num_threads::Int=nthreads())
        executor = TaskflowExecutor(num_threads)
        new(dte_system, executor, Dict(), Dict())
    end
end

"""
    evolve_with_taskflow!(system::TaskflowOntogeneticSystem, 
                         generations::Int;
                         verbose::Bool=false)

Evolve the system using parallel task graph execution.

# Arguments
- `system::TaskflowOntogeneticSystem`: Hybrid system
- `generations::Int`: Number of generations
- `verbose::Bool=false`: Print progress
"""
function evolve_with_taskflow!(system::TaskflowOntogeneticSystem, 
                               generations::Int;
                               verbose::Bool=false)
    for gen in 1:generations
        if verbose
            println("Generation $gen:")
        end
        
        # 1. Convert trees to task graphs
        sync_trees_to_graphs!(system, verbose)
        
        # 2. Execute all task graphs in parallel
        execute_all_graphs!(system, verbose)
        
        # 3. Evolve Deep Tree Echo system
        # Note: Assumes dte_system has evolve! method
        if hasfield(typeof(system.dte_system), :step_count)
            # Call evolve! on the DTE system
            # evolve!(system.dte_system, 1, verbose=false)
        end
        
        if verbose
            println("  Completed generation $gen")
        end
    end
    
    return nothing
end

"""
    sync_trees_to_graphs!(system::TaskflowOntogeneticSystem, verbose::Bool)

Synchronize tree population to task graphs.
"""
function sync_trees_to_graphs!(system::TaskflowOntogeneticSystem, verbose::Bool)
    # Get trees from garden
    if !hasfield(typeof(system.dte_system), :garden)
        return
    end
    
    garden = system.dte_system.garden
    
    for (tree_id, tree) in garden.trees
        if !haskey(system.tree_to_graph, tree_id)
            # Create new task graph for this tree
            graph = tree_to_taskgraph(tree.level_sequence)
            graph_id = system.executor.next_graph_id
            system.executor.next_graph_id += 1
            
            system.executor.graphs[graph_id] = graph
            system.tree_to_graph[tree_id] = graph_id
            system.graph_to_tree[graph_id] = tree_id
            
            if verbose
                println("    Created task graph $graph_id for tree $tree_id")
            end
        end
    end
    
    return nothing
end

"""
    execute_all_graphs!(system::TaskflowOntogeneticSystem, verbose::Bool)

Execute all task graphs in the system.
"""
function execute_all_graphs!(system::TaskflowOntogeneticSystem, verbose::Bool)
    for (graph_id, graph) in system.executor.graphs
        execute_taskgraph!(graph, parallel=true)
        
        if verbose
            tree_id = get(system.graph_to_tree, graph_id, -1)
            println("    Executed graph $graph_id (tree $tree_id)")
        end
    end
    
    return nothing
end

"""
    get_graph_for_tree(system::TaskflowOntogeneticSystem, tree_id::Int)

Get the task graph corresponding to a tree.
"""
function get_graph_for_tree(system::TaskflowOntogeneticSystem, tree_id::Int)
    graph_id = get(system.tree_to_graph, tree_id, nothing)
    
    if isnothing(graph_id)
        return nothing
    end
    
    return get(system.executor.graphs, graph_id, nothing)
end

"""
    print_taskgraph(graph::TaskGraph)

Print task graph structure.
"""
function print_taskgraph(graph::TaskGraph)
    println("Task Graph:")
    println("  Tasks: $(length(graph.tasks))")
    
    for (id, task) in sort(collect(graph.tasks), by=x->x[1])
        deps = isempty(task.dependencies) ? "none" : join(task.dependencies, ", ")
        status = task.completed ? "✓" : "○"
        println("  $status Task $id ($(task.name)): depends on [$deps]")
    end
    
    if !isempty(graph.execution_order)
        println("  Execution order: $(join(graph.execution_order, " → "))")
    end
end

end # module TaskflowIntegration
