using Test
using PSystems

@testset "Parse Simple System" begin
    plingua_code = """
    @model<transition>
    
    def main() {
        @mu = []'1;
        @ms(1) = a{2}, b;
        [a]'1 --> [b]'1;
    }
    """
    
    system = parse_plingua(plingua_code)
    
    @test length(system.membranes) == 1
    @test system.membranes[1].label == 1
    @test "a" in system.alphabet
    @test "b" in system.alphabet
    @test haskey(system.initial_multisets, 1)
    @test get(system.initial_multisets[1], "a") == 2
    @test get(system.initial_multisets[1], "b") == 1
    @test length(system.rules) == 1
end

@testset "Parse Nested Membranes" begin
    plingua_code = """
    @model<transition>
    
    def nested() {
        @mu = [[]'2]'1;
        @ms(1) = a;
        @ms(2) = b;
    }
    """
    
    system = parse_plingua(plingua_code)
    
    @test length(system.membranes) == 2
    
    # Find membranes
    skin = nothing
    inner = nothing
    for m in system.membranes
        if m.label == 1
            skin = m
        elseif m.label == 2
            inner = m
        end
    end
    
    @test skin !== nothing
    @test inner !== nothing
    @test is_skin(skin)
    @test !is_skin(inner)
    @test inner.parent == skin.id
end

@testset "Parse Communication Rule" begin
    plingua_code = """
    @model<transition>
    
    def comm() {
        @mu = [[]'2]'1;
        @ms(2) = a;
        [a]'2 --> (b, out);
    }
    """
    
    system = parse_plingua(plingua_code)
    
    @test length(system.rules) == 1
    rule = system.rules[1]
    @test rule.membrane_label == 2
    @test get(rule.lhs, "a") == 1
    @test get(rule.rhs, "b") == 1
    @test rule.target == :out
end

@testset "Parse Multiple Rules" begin
    plingua_code = """
    @model<transition>
    
    def multi() {
        @mu = []'1;
        @ms(1) = a, b;
        [a]'1 --> [c]'1;
        [b]'1 --> [d]'1;
    }
    """
    
    system = parse_plingua(plingua_code)
    
    @test length(system.rules) == 2
end

@testset "Parse Multiset with Multiplicities" begin
    plingua_code = """
    @model<transition>
    
    def mult() {
        @mu = []'1;
        @ms(1) = a{5}, b{2}, c;
    }
    """
    
    system = parse_plingua(plingua_code)
    
    ms = system.initial_multisets[1]
    @test get(ms, "a") == 5
    @test get(ms, "b") == 2
    @test get(ms, "c") == 1
end

@testset "Parse Rule with Multiplicities" begin
    plingua_code = """
    @model<transition>
    
    def rule_mult() {
        @mu = []'1;
        @ms(1) = a{3};
        [a{2}]'1 --> [b{5}]'1;
    }
    """
    
    system = parse_plingua(plingua_code)
    
    @test length(system.rules) == 1
    rule = system.rules[1]
    @test get(rule.lhs, "a") == 2
    @test get(rule.rhs, "b") == 5
end
