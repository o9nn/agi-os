using Test
using PSystems
using PSystems: get_multiset, is_active, applicable_rules

@testset "Simple Simulation" begin
    # System: a -> b -> c (chain of rewrites)
    system = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a", "b", "c"],
        initial_multisets=Dict(1 => Multiset("a" => 1)),
        rules=[
            Rule(1, Multiset("a" => 1), Multiset("b" => 1)),
            Rule(1, Multiset("b" => 1), Multiset("c" => 1))
        ]
    )
    
    result = simulate(system, max_steps=10)
    
    @test result.steps <= 10
    @test result.halted == true
    @test get_multiset(result.final_config, 1) == Multiset("c" => 1)
end

@testset "Maximal Parallelism" begin
    # System: a -> b{2} (doubling)
    system = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a", "b"],
        initial_multisets=Dict(1 => Multiset("a" => 3)),
        rules=[
            Rule(1, Multiset("a" => 1), Multiset("b" => 2))
        ]
    )
    
    result = simulate(system, max_steps=1)
    
    @test result.steps == 1
    # All 3 a's should be consumed in parallel
    @test get_multiset(result.final_config, 1) == Multiset("b" => 6)
end

@testset "Communication Rules" begin
    # Parent-child communication
    skin = Membrane(1, 1, nothing)
    inner = Membrane(2, 2, 1)
    
    system = PSystem(
        membranes=[skin, inner],
        alphabet=["a", "b"],
        initial_multisets=Dict(2 => Multiset("a" => 1)),
        rules=[
            # Send from inner to outer
            Rule(2, Multiset("a" => 1), Multiset("b" => 1), :out)
        ]
    )
    
    result = simulate(system, max_steps=10)
    
    @test get_multiset(result.final_config, 1) == Multiset("b" => 1)
    @test get_multiset(result.final_config, 2) == Multiset()
end

@testset "Membrane Dissolution" begin
    skin = Membrane(1, 1, nothing)
    inner = Membrane(2, 2, 1)
    
    system = PSystem(
        membranes=[skin, inner],
        alphabet=["a", "b", "c"],
        initial_multisets=Dict(
            1 => Multiset("a" => 1),
            2 => Multiset("b" => 2, "c" => 1)
        ),
        rules=[
            # Dissolve inner membrane when consuming c
            Rule(2, Multiset("c" => 1), Multiset(), :here, true)
        ]
    )
    
    result = simulate(system, max_steps=10)
    
    # Inner membrane should be dissolved
    @test !is_active(result.final_config, 2)
    # Objects from inner should move to skin
    @test get_multiset(result.final_config, 1) == Multiset("a" => 1, "b" => 2)
end

@testset "Trace Recording" begin
    system = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a", "b"],
        initial_multisets=Dict(1 => Multiset("a" => 2)),
        rules=[
            Rule(1, Multiset("a" => 1), Multiset("b" => 1))
        ]
    )
    
    result = simulate(system, max_steps=10, trace=true)
    
    @test length(result.trace) > 0
    @test result.trace[1].step == 0
    @test get_multiset(result.trace[1], 1) == Multiset("a" => 2)
    @test get_multiset(result.trace[end], 1) == Multiset("b" => 2)
end

@testset "Custom Halt Condition" begin
    system = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a"],
        initial_multisets=Dict(1 => Multiset("a" => 5)),
        rules=[
            Rule(1, Multiset("a" => 1), Multiset("a" => 2))  # Doubling
        ]
    )
    
    # Halt when we have >= 20 objects
    halt_fn(config) = length(get_multiset(config, 1)) >= 20
    
    result = simulate(system, max_steps=100, halt_condition=halt_fn)
    
    @test result.halted == true
    @test length(get_multiset(result.final_config, 1)) >= 20
    @test result.steps < 100  # Should halt before max steps
end

@testset "Applicable Rules Selection" begin
    system = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a", "b", "c"],
        initial_multisets=Dict(1 => Multiset("a" => 2, "b" => 1)),
        rules=[
            Rule(1, Multiset("a" => 1), Multiset("c" => 1)),
            Rule(1, Multiset("b" => 1), Multiset("c" => 2))
        ]
    )
    
    config = Configuration(system)
    applicable = applicable_rules(system, config, 1)
    
    @test length(applicable) == 2
end

@testset "Priority Rules" begin
    system = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a", "b", "c"],
        initial_multisets=Dict(1 => Multiset("a" => 1)),
        rules=[
            Rule(1, Multiset("a" => 1), Multiset("b" => 1), :here, false, 1),
            Rule(1, Multiset("a" => 1), Multiset("c" => 1), :here, false, 2)
        ]
    )
    
    result = simulate(system, max_steps=1)
    
    # Higher priority rule (c) should be applied
    @test get_multiset(result.final_config, 1) == Multiset("c" => 1)
end
