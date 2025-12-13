using Test
using PSystems
using PSystems: get_membrane, get_rules_for_membrane, get_skin_membrane, get_children, validate

@testset "PSystem Construction" begin
    membranes = [Membrane(1, 1, nothing)]
    alphabet = ["a", "b"]
    initial_multisets = Dict(1 => Multiset("a" => 2))
    rules = [Rule(1, Multiset("a" => 1), Multiset("b" => 1))]
    
    system = PSystem(
        membranes=membranes,
        alphabet=alphabet,
        initial_multisets=initial_multisets,
        rules=rules
    )
    
    @test length(system.membranes) == 1
    @test length(system.alphabet) == 2
    @test length(system.rules) == 1
    @test haskey(system.initial_multisets, 1)
end

@testset "PSystem Utilities" begin
    skin = Membrane(1, 1, nothing)
    inner1 = Membrane(2, 2, 1)
    inner2 = Membrane(3, 2, 1)
    
    system = PSystem(
        membranes=[skin, inner1, inner2],
        alphabet=["a"],
        initial_multisets=Dict(1 => Multiset("a" => 1)),
        rules=Rule[]
    )
    
    # Get membrane
    @test get_membrane(system, 1) == skin
    @test get_membrane(system, 2) == inner1
    @test get_membrane(system, 99) === nothing
    
    # Get skin
    @test get_skin_membrane(system) == skin
    
    # Get children
    children = get_children(system, 1)
    @test length(children) == 2
    @test inner1 in children
    @test inner2 in children
end

@testset "PSystem Validation" begin
    # Valid system
    system1 = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a"],
        initial_multisets=Dict(1 => Multiset("a" => 1)),
        rules=[Rule(1, Multiset("a" => 1), Multiset("a" => 1))]
    )
    @test validate(system1) == true
    
    # Invalid: initial multiset for non-existent membrane
    @test_throws ArgumentError PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a"],
        initial_multisets=Dict(99 => Multiset("a" => 1)),
        rules=Rule[]
    )
end

@testset "Get Rules for Membrane" begin
    rules = [
        Rule(1, Multiset("a" => 1), Multiset("b" => 1)),
        Rule(2, Multiset("c" => 1), Multiset("d" => 1)),
        Rule(1, Multiset("b" => 1), Multiset("c" => 1))
    ]
    
    system = PSystem(
        membranes=[Membrane(1, 1, nothing), Membrane(2, 2, 1)],
        alphabet=["a", "b", "c", "d"],
        initial_multisets=Dict(1 => Multiset()),
        rules=rules
    )
    
    rules_for_1 = get_rules_for_membrane(system, 1)
    @test length(rules_for_1) == 2
    @test rules[1] in rules_for_1
    @test rules[3] in rules_for_1
    
    rules_for_2 = get_rules_for_membrane(system, 2)
    @test length(rules_for_2) == 1
    @test rules[2] in rules_for_2
end
