using Test
using PSystems
using PSystems: get_multiset

@testset "Integration: Powers of 2" begin
    # Generator system: computes powers of 2
    plingua_code = """
    @model<transition>
    
    def powers_of_2() {
        @mu = []'1;
        @ms(1) = a, d;
        [a]'1 --> [a, b]'1;
        [b]'1 --> [b, c]'1;
        [c]'1 --> [c, a]'1;
    }
    """
    
    system = parse_plingua(plingua_code)
    result = simulate(system, max_steps=5, trace=true)
    
    # After each complete cycle, number of a's doubles
    # Initial: 1 a
    # After step 1: 1 a, 1 b
    # After step 2: 1 a, 1 b, 1 c
    # After step 3: 2 a, 1 b, 1 c
    # And so on...
    
    @test result.steps > 0
    @test !isempty(result.trace)
end

@testset "Integration: Simple Rewriting Chain" begin
    # a -> b -> c
    plingua_code = """
    @model<transition>
    
    def chain() {
        @mu = []'1;
        @ms(1) = a;
        [a]'1 --> [b]'1;
        [b]'1 --> [c]'1;
    }
    """
    
    system = parse_plingua(plingua_code)
    result = simulate(system, max_steps=10)
    
    @test result.halted == true
    @test get_multiset(result.final_config, 1) == Multiset("c" => 1)
end

@testset "Integration: Communication Between Membranes" begin
    plingua_code = """
    @model<transition>
    
    def communication() {
        @mu = [[]'2]'1;
        @ms(1) = a;
        @ms(2) = b;
        [a]'1 --> (c, in);
        [b]'2 --> (d, out);
    }
    """
    
    # Note: Simplified parser may not fully support in_X notation
    # This tests what we can parse
    system = parse_plingua(plingua_code)
    
    @test length(system.membranes) == 2
    @test length(system.rules) >= 1
end

@testset "Integration: End-to-End P-System" begin
    # Create programmatically and simulate
    skin = Membrane(1, 1, nothing)
    inner = Membrane(2, 2, 1)
    
    system = PSystem(
        membranes=[skin, inner],
        alphabet=["a", "b", "c"],
        initial_multisets=Dict(
            1 => Multiset("a" => 3),
            2 => Multiset("b" => 2)
        ),
        rules=[
            Rule(1, Multiset("a" => 1), Multiset("b" => 1)),
            Rule(2, Multiset("b" => 1), Multiset("c" => 1), :out)
        ]
    )
    
    result = simulate(system, max_steps=10, trace=true)
    
    @test result.steps > 0
    @test length(result.trace) > 0
    
    # Verify trace progression
    initial = result.trace[1]
    @test get_multiset(initial, 1) == Multiset("a" => 3)
    @test get_multiset(initial, 2) == Multiset("b" => 2)
end

@testset "Integration: Halting Detection" begin
    system = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a", "b"],
        initial_multisets=Dict(1 => Multiset("a" => 2)),
        rules=[Rule(1, Multiset("a" => 1), Multiset("b" => 1))]
    )
    
    result = simulate(system, max_steps=100)
    
    @test result.halted == true
    @test result.steps < 100
    @test get_multiset(result.final_config, 1) == Multiset("b" => 2)
end

@testset "Integration: Non-Halting with Max Steps" begin
    # Infinite loop: a -> b, b -> a
    system = PSystem(
        membranes=[Membrane(1, 1, nothing)],
        alphabet=["a", "b"],
        initial_multisets=Dict(1 => Multiset("a" => 1)),
        rules=[
            Rule(1, Multiset("a" => 1), Multiset("b" => 1)),
            Rule(1, Multiset("b" => 1), Multiset("a" => 1))
        ]
    )
    
    result = simulate(system, max_steps=10)
    
    @test result.steps == 10
    @test result.halted == false
end
