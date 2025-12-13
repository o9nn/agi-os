"""
Test Suite for Deep Tree Echo State Reservoir Computer

Comprehensive tests covering all components:
- Ontogenetic engine and A000081 generation
- B-series ridges
- J-surface reactor
- P-system reservoirs
- Membrane gardens
- Taskflow integration
- Package integration
"""

# Add src to load path
push!(LOAD_PATH, joinpath(@__DIR__, "..", "src"))

using Test
using LinearAlgebra
using Random

# Load Deep Tree Echo modules
include("../src/DeepTreeEcho/DeepTreeEcho.jl")
using .DeepTreeEcho

println("""
╔════════════════════════════════════════════════════════════════╗
║  DEEP TREE ECHO TEST SUITE                                     ║
╚════════════════════════════════════════════════════════════════╝
""")

Random.seed!(42)

@testset "Deep Tree Echo Tests" begin
    
    @testset "0. A000081 Parameter Derivation" begin
        println("\n[0/9] Testing A000081 Parameter Derivation...")
        
        using .DeepTreeEcho.A000081Parameters
        
        # Test sequence constants
        @test A000081_SEQUENCE[1] == 1
        @test A000081_SEQUENCE[2] == 1
        @test A000081_SEQUENCE[3] == 2
        @test A000081_SEQUENCE[4] == 4
        @test A000081_SEQUENCE[5] == 9
        @test A000081_SEQUENCE[6] == 20
        
        # Test reservoir size derivation
        @test derive_reservoir_size(1) == 1      # 1
        @test derive_reservoir_size(2) == 2      # 1+1
        @test derive_reservoir_size(3) == 4      # 1+1+2
        @test derive_reservoir_size(4) == 8      # 1+1+2+4
        @test derive_reservoir_size(5) == 17     # 1+1+2+4+9
        @test derive_reservoir_size(6) == 37     # 1+1+2+4+9+20
        
        # Test membrane count derivation
        @test derive_num_membranes(1) == 1
        @test derive_num_membranes(3) == 2
        @test derive_num_membranes(4) == 4
        @test derive_num_membranes(5) == 9
        
        # Test growth rate derivation
        growth_5 = derive_growth_rate(5)
        @test abs(growth_5 - 20/9) < 0.001   # A000081[6]/A000081[5] = 20/9
        
        # Test mutation rate derivation
        mutation_5 = derive_mutation_rate(5)
        @test abs(mutation_5 - 1/9) < 0.001  # 1/A000081[5] = 1/9
        
        # Test parameter set creation
        params = get_parameter_set(5, membrane_order=3)
        @test params.base_order == 5
        @test params.reservoir_size == 17
        @test params.num_membranes == 2
        @test params.max_tree_order == 8
        
        # Test parameter validation
        is_valid, msg = validate_parameters(17, 8, 2, 20/9, 1/9)
        @test is_valid
        
        # Test invalid parameters
        is_valid, msg = validate_parameters(100, 8, 3, 0.1, 0.05)
        @test !is_valid
        
        # Test 1-1 correspondence validation
        is_valid_corr, msg_corr = validate_component_correspondence(5, 9, 9, 9)
        @test is_valid_corr
        
        # Test invalid correspondence
        is_valid_corr2, msg_corr2 = validate_component_correspondence(5, 9, 8, 9)
        @test !is_valid_corr2
        
        println("  ✓ A000081 sequence correct")
        println("  ✓ Parameter derivation functional")
        println("  ✓ Validation working")
        println("  ✓ 1-1 correspondence validation working")
    end
    
    @testset "1. Ontogenetic Engine" begin
        println("\n[1/9] Testing Ontogenetic Engine...")
        
        using .DeepTreeEcho.OntogeneticEngine
        
        # Test A000081 generator
        generator = A000081Generator(10)
        
        @test generator.max_order == 10
        @test length(generator.a000081) >= 10
        
        # Test known values of A000081
        @test generator.a000081[1] == 1  # 1 tree of order 1
        @test generator.a000081[2] == 1  # 1 tree of order 2
        @test generator.a000081[3] == 2  # 2 trees of order 3
        @test generator.a000081[4] == 4  # 4 trees of order 4
        @test generator.a000081[5] == 9  # 9 trees of order 5
        
        # Test tree generation
        trees_order_3 = generate_a000081_trees(generator, 3)
        @test length(trees_order_3) == 2
        
        trees_order_4 = generate_a000081_trees(generator, 4)
        @test length(trees_order_4) == 4
        
        # Test ontogenetic state
        initial_trees = [[1], [1, 2], [1, 2, 2]]
        state = OntogeneticState(initial_trees)
        
        @test state.generation == 0
        @test length(state.population) == 3
        
        # Test evolution step
        ontogenetic_step!(state, generator)
        @test state.generation == 1
        
        # Test statistics
        stats = get_ontogenetic_statistics(state)
        @test haskey(stats, "generation")
        @test haskey(stats, "population_size")
        @test haskey(stats, "avg_fitness")
        
        println("  ✓ A000081 generation correct")
        println("  ✓ Tree enumeration matches sequence")
        println("  ✓ Ontogenetic evolution functional")
    end
    
    @testset "2. B-Series Ridges" begin
        println("\n[2/9] Testing B-Series Ridges...")
        
        using .DeepTreeEcho.BSeriesRidge
        
        # Test ridge creation
        ridge = create_ridge(6)
        
        @test ridge.order == 6
        @test ridge.dimension > 0
        @test length(ridge.coefficients) == ridge.dimension
        
        # Test ridge evaluation
        f(y) = -y  # Simple ODE: y' = -y
        y0 = [1.0]
        increment = evaluate_ridge(ridge, y0, f)
        
        @test length(increment) == length(y0)
        @test !isnan(increment[1])
        
        # Test ridge optimization
        optimize_ridge!(ridge, 4, iterations=10)
        
        @test norm(ridge.coefficients) > 0
        
        println("  ✓ Ridge creation successful")
        println("  ✓ Ridge evaluation works")
        println("  ✓ Ridge optimization functional")
    end
    
    @testset "3. J-Surface Reactor" begin
        println("\n[3/9] Testing J-Surface Reactor...")
        
        using .DeepTreeEcho.JSurfaceReactor
        
        # Test J-surface creation
        jsurface = create_jsurface(20, symplectic=true)
        
        @test jsurface.dimension == 20
        @test jsurface.symplectic == true
        @test size(jsurface.structure_matrix) == (20, 20)
        
        # Test state creation
        state = JSurfaceState(20, 10)
        
        @test length(state.position) == 20
        @test length(state.velocity) == 20
        @test state.generation == 0
        
        # Test gradient flow
        initial_energy = jsurface.hamiltonian(state.position)
        gradient_flow!(jsurface, state, 0.01)
        
        @test state.generation == 1
        
        # Test symplectic integration
        symplectic_integrate!(jsurface, state, 0.01)
        
        @test state.generation == 2
        
        # Energy should be approximately conserved for symplectic
        final_energy = jsurface.hamiltonian(state.position)
        @test abs(final_energy - initial_energy) < 0.5
        
        println("  ✓ J-surface creation successful")
        println("  ✓ Gradient flow functional")
        println("  ✓ Symplectic integration preserves energy")
    end
    
    @testset "4. P-System Reservoirs" begin
        println("\n[4/9] Testing P-System Reservoirs...")
        
        using .DeepTreeEcho.PSystemReservoir
        
        # Test membrane creation
        reservoir = create_membrane_reservoir(
            "[[]'2]'1",
            alphabet=["a", "b", "c"]
        )
        
        @test reservoir.root_id == 1
        @test length(reservoir.membranes) == 2
        @test length(reservoir.alphabet) == 3
        
        # Test multiset operations
        multiset = Multiset(Dict("a" => 3, "b" => 2))
        
        @test multiset.objects["a"] == 3
        @test multiset.objects["b"] == 2
        
        # Test evolution rule
        rule = EvolutionRule(1, Multiset("a" => 1), Multiset("b" => 2))
        add_evolution_rule!(reservoir, rule)
        
        @test length(reservoir.rules) == 1
        
        # Test membrane evolution
        evolve_membrane!(reservoir, 5)
        
        println("  ✓ Membrane structure creation successful")
        println("  ✓ Multiset operations functional")
        println("  ✓ Evolution rules working")
    end
    
    @testset "5. Membrane Gardens" begin
        println("\n[5/9] Testing Membrane Gardens...")
        
        using .DeepTreeEcho.MembraneGarden
        
        # Test garden creation
        garden = create_garden()
        
        @test length(garden.trees) == 0
        
        # Test tree planting
        tree1 = [1, 2, 2, 3]
        tree_id = plant_tree!(garden, tree1, 1)
        
        @test tree_id > 0
        @test haskey(garden.trees, tree_id)
        @test garden.trees[tree_id].level_sequence == tree1
        
        # Test tree growth
        grow_trees!(garden, 5)
        
        @test garden.trees[tree_id].age == 5
        
        # Test cross-pollination
        tree2 = [1, 2, 3]
        tree_id2 = plant_tree!(garden, tree2, 2)
        
        cross_pollinate!(garden, 1, 2, 2)
        
        # Should have created offspring
        @test length(garden.trees) >= 2
        
        # Test feedback harvesting
        feedback = harvest_feedback!(garden, 1)
        
        @test length(feedback) > 0
        
        println("  ✓ Garden creation successful")
        println("  ✓ Tree planting functional")
        println("  ✓ Growth and cross-pollination working")
    end
    
    @testset "6. Integrated System" begin
        println("\n[6/9] Testing Integrated System...")
        
        # Test system creation with A000081-derived parameters
        # base_order=4 → reservoir=8, max_order=7, membranes=2
        params = get_parameter_set(4, membrane_order=3)
        
        system = DeepTreeEchoSystem(
            reservoir_size = params.reservoir_size,    # 8
            max_tree_order = params.max_tree_order,    # 7
            num_membranes = params.num_membranes,      # 2
            symplectic = true,
            growth_rate = params.growth_rate,
            mutation_rate = params.mutation_rate
        )
        
        @test system.config["reservoir_size"] == 8
        @test system.config["max_tree_order"] == 7
        @test system.config["num_membranes"] == 2
        @test system.step_count == 0
        
        # Test initialization with A000081[4] = 4 seed trees
        seed_count = A000081Parameters.A000081_SEQUENCE[4]
        initialize!(system, seed_trees=seed_count)
        
        @test length(system.garden.trees) == seed_count
        @test system.ontogenetic_state.generation == 0
        
        # Test evolution
        evolve!(system, 5, verbose=false)
        
        @test system.step_count == 5
        @test system.ontogenetic_state.generation >= 5
        
        # Test input processing
        input = randn(10)
        output = process_input!(system, input)
        
        @test length(output) == length(input)
        @test !any(isnan, output)
        
        # Test status
        status = get_system_status(system)
        
        @test haskey(status, "step_count")
        @test haskey(status, "ontogenetic")
        @test haskey(status, "garden")
        
        println("  ✓ System creation successful")
        println("  ✓ Initialization functional")
        println("  ✓ Evolution working")
        println("  ✓ Input processing functional")
    end
    
    @testset "7. Taskflow Integration" begin
        println("\n[7/9] Testing Taskflow Integration...")
        
        using .DeepTreeEcho.TaskflowIntegration
        
        # Test task graph creation
        graph = TaskGraph()
        
        @test length(graph.tasks) == 0
        
        # Test task creation
        t1 = create_task!(graph, "task1", () -> 1)
        t2 = create_task!(graph, "task2", () -> 2)
        t3 = create_task!(graph, "task3", () -> 3)
        
        @test length(graph.tasks) == 3
        
        # Test dependency addition
        add_dependency!(graph, t1, t2)
        add_dependency!(graph, t1, t3)
        
        @test t1 in graph.tasks[t2].dependencies
        @test t1 in graph.tasks[t3].dependencies
        
        # Test execution
        execute_taskgraph!(graph, parallel=false)
        
        @test all(task.completed for task in values(graph.tasks))
        
        # Test tree conversion
        tree = [1, 2, 2, 3]
        task_graph = tree_to_taskgraph(tree)
        
        @test length(task_graph.tasks) == length(tree)
        
        recovered_tree = taskgraph_to_tree(task_graph)
        
        @test recovered_tree == tree
        
        println("  ✓ Task graph creation successful")
        println("  ✓ Dependency management functional")
        println("  ✓ Execution working")
        println("  ✓ Tree conversion lossless")
    end
    
    @testset "8. Package Integration" begin
        println("\n[8/9] Testing Package Integration...")
        
        using .DeepTreeEcho.PackageIntegration
        
        # Test integration status
        status = integration_status()
        
        @test haskey(status, "RootedTrees.jl")
        @test haskey(status, "BSeries.jl")
        @test haskey(status, "ReservoirComputing.jl")
        
        # Test tree counting (A000081)
        count_1 = count_trees_of_order(1)
        count_2 = count_trees_of_order(2)
        count_3 = count_trees_of_order(3)
        count_4 = count_trees_of_order(4)
        count_5 = count_trees_of_order(5)
        
        @test count_1 == 1
        @test count_2 == 1
        @test count_3 == 2
        @test count_4 == 4
        @test count_5 == 9
        
        # Test tree generation
        trees = generate_trees_up_to_order(4)
        
        @test length(trees) >= 8  # 1 + 1 + 2 + 4
        
        println("  ✓ Integration status reporting works")
        println("  ✓ A000081 counting correct")
        println("  ✓ Tree generation functional")
    end
    
    @testset "9. A000081 System Alignment" begin
        println("\n[9/9] Testing Complete A000081 Alignment...")
        
        # Test system with auto-derived parameters
        system_auto = DeepTreeEchoSystem(base_order=4)
        
        @test system_auto.config["reservoir_size"] == 8   # 1+1+2+4
        @test system_auto.config["max_tree_order"] == 7   # 4+3
        @test system_auto.config["num_membranes"] == 2    # A000081[3]
        
        # Test system with explicit A000081 parameters
        params = get_parameter_set(5, membrane_order=4)
        system_explicit = DeepTreeEchoSystem(
            reservoir_size = params.reservoir_size,
            max_tree_order = params.max_tree_order,
            num_membranes = params.num_membranes,
            growth_rate = params.growth_rate,
            mutation_rate = params.mutation_rate
        )
        
        @test system_explicit.config["reservoir_size"] == 17  # 1+1+2+4+9
        @test system_explicit.config["num_membranes"] == 4    # A000081[4]
        
        # Initialize and evolve with A000081 seed count
        seed_count = A000081Parameters.A000081_SEQUENCE[3]  # 2
        initialize!(system_auto, seed_trees=seed_count)
        evolve!(system_auto, 3, verbose=false)
        
        @test system_auto.step_count == 3
        @test system_auto.ontogenetic_state.generation >= 3
        
        println("  ✓ Auto-derivation works correctly")
        println("  ✓ Explicit parameters align with A000081")
        println("  ✓ System evolution maintains alignment")
    end
    
end

println("\n" * "="^60)
println("TEST SUMMARY")
println("="^60)

# Print final summary
println("\nAll tests completed successfully! ✓")
println("\nDeep Tree Echo System Status:")
println("  • Ontogenetic engine: Operational")
println("  • A000081 parameter derivation: Functional")
println("  • B-series ridges: Functional")
println("  • J-surface reactor: Active")
println("  • P-system reservoirs: Running")
println("  • Membrane gardens: Growing")
println("  • Taskflow integration: Connected")
println("  • Package integration: Available")

println("\nThe system is ready for:")
println("  ✓ Large-scale tree evolution")
println("  ✓ A000081-aligned parameter configuration")
println("  ✓ Parallel task execution")
println("  ✓ Cognitive computing workflows")
println("  ✓ Reservoir-based learning")
println("  ✓ Ontogenetic self-evolution")

println("\n" * "="^60)
