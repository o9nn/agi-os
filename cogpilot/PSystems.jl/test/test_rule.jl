using Test
using PSystems
using PSystems: is_applicable, apply_rule

@testset "Rule Construction" begin
    lhs = Multiset("a" => 1)
    rhs = Multiset("b" => 2)
    
    # Basic rule
    r1 = Rule(1, lhs, rhs)
    @test r1.membrane_label == 1
    @test r1.lhs == lhs
    @test r1.rhs == rhs
    @test r1.target == :here
    @test r1.dissolve == false
    @test r1.priority == 0
    
    # Rule with target
    r2 = Rule(1, lhs, rhs, :out)
    @test r2.target == :out
    
    r3 = Rule(1, lhs, rhs, (:in, 2))
    @test r3.target == (:in, 2)
    
    # Rule with dissolution
    r4 = Rule(1, lhs, rhs, :here, true)
    @test r4.dissolve == true
    
    # Rule with priority
    r5 = Rule(1, lhs, rhs, :here, false, 5)
    @test r5.priority == 5
end

@testset "Rule Applicability" begin
    rule = Rule(1, Multiset("a" => 2, "b" => 1), Multiset("c" => 1))
    
    # Applicable
    ms1 = Multiset("a" => 3, "b" => 2)
    @test is_applicable(rule, ms1)
    
    # Not applicable (not enough a's)
    ms2 = Multiset("a" => 1, "b" => 2)
    @test !is_applicable(rule, ms2)
    
    # Not applicable (no b's)
    ms3 = Multiset("a" => 5)
    @test !is_applicable(rule, ms3)
    
    # Exactly matches
    ms4 = Multiset("a" => 2, "b" => 1)
    @test is_applicable(rule, ms4)
end

@testset "Rule Application" begin
    rule = Rule(1, Multiset("a" => 2), Multiset("b" => 3))
    
    # Apply successfully
    ms1 = Multiset("a" => 3, "c" => 1)
    result = apply_rule(rule, ms1)
    @test result !== nothing
    @test get(result, "a") == 1
    @test get(result, "b") == 3
    @test get(result, "c") == 1
    
    # Cannot apply
    ms2 = Multiset("a" => 1)
    result2 = apply_rule(rule, ms2)
    @test result2 === nothing
end

@testset "Rule Equality" begin
    r1 = Rule(1, Multiset("a" => 1), Multiset("b" => 1))
    r2 = Rule(1, Multiset("a" => 1), Multiset("b" => 1))
    r3 = Rule(2, Multiset("a" => 1), Multiset("b" => 1))
    
    @test r1 == r2
    @test r1 != r3
end
