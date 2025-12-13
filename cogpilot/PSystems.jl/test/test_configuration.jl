using Test
using PSystems
using PSystems: get_multiset, set_multiset!, is_active, dissolve!, is_halted

@testset "Configuration Construction" begin
    system = PSystem(
        membranes=[Membrane(1, 1, nothing), Membrane(2, 2, 1)],
        alphabet=["a", "b"],
        initial_multisets=Dict(
            1 => Multiset("a" => 2),
            2 => Multiset("b" => 1)
        ),
        rules=Rule[]
    )
    
    config = Configuration(system)
    
    @test config.step == 0
    @test length(config.active_membranes) == 2
    @test is_active(config, 1)
    @test is_active(config, 2)
    @test get_multiset(config, 1) == Multiset("a" => 2)
    @test get_multiset(config, 2) == Multiset("b" => 1)
end

@testset "Configuration Operations" begin
    system = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a"],
        initial_multisets=Dict(1 => Multiset("a" => 1)),
        rules=Rule[]
    )
    
    config = Configuration(system)
    
    # Set multiset
    new_ms = Multiset("b" => 3)
    set_multiset!(config, 1, new_ms)
    @test get_multiset(config, 1) == new_ms
    
    # Dissolve
    @test is_active(config, 1)
    dissolve!(config, 1)
    @test !is_active(config, 1)
end

@testset "Configuration Copy" begin
    system = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a"],
        initial_multisets=Dict(1 => Multiset("a" => 2)),
        rules=Rule[]
    )
    
    config1 = Configuration(system)
    config2 = copy(config1)
    
    # Same values
    @test config2.step == config1.step
    @test get_multiset(config2, 1) == get_multiset(config1, 1)
    
    # Different objects
    set_multiset!(config2, 1, Multiset("b" => 5))
    @test get_multiset(config2, 1) != get_multiset(config1, 1)
end

@testset "Is Halted" begin
    # System with no applicable rules
    system1 = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a", "b"],
        initial_multisets=Dict(1 => Multiset("a" => 1)),
        rules=[Rule(1, Multiset("b" => 1), Multiset("c" => 1))]
    )
    
    config1 = Configuration(system1)
    @test is_halted(config1, system1) == true
    
    # System with applicable rule
    system2 = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a", "b"],
        initial_multisets=Dict(1 => Multiset("a" => 1)),
        rules=[Rule(1, Multiset("a" => 1), Multiset("b" => 1))]
    )
    
    config2 = Configuration(system2)
    @test is_halted(config2, system2) == false
end
