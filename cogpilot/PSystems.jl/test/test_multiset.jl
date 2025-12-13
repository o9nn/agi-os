using Test
using PSystems

@testset "Multiset Operations" begin
    # Construction
    m1 = Multiset("a" => 2, "b" => 1)
    @test get(m1, "a") == 2
    @test get(m1, "b") == 1
    @test get(m1, "c") == 0
    
    # Empty multiset
    m_empty = Multiset()
    @test isempty(m_empty)
    @test length(m_empty) == 0
    
    # Length
    @test length(m1) == 3  # 2 a's + 1 b
    
    # Addition
    m2 = Multiset("a" => 1, "c" => 2)
    m3 = m1 + m2
    @test get(m3, "a") == 3
    @test get(m3, "b") == 1
    @test get(m3, "c") == 2
    
    # Subtraction
    m4 = m3 - m1
    @test get(m4, "a") == 1
    @test get(m4, "b") == 0
    @test get(m4, "c") == 2
    
    # Subtraction failure
    m5 = m1 - m3
    @test m5 === nothing
    
    # Scalar multiplication
    m6 = 2 * Multiset("a" => 1, "b" => 2)
    @test get(m6, "a") == 2
    @test get(m6, "b") == 4
    
    # Subset
    @test m1 ⊆ m3
    @test !(m3 ⊆ m1)
    @test m_empty ⊆ m1
    
    # Equality
    @test m1 == Multiset("a" => 2, "b" => 1)
    @test m1 != m2
    
    # Copy
    m7 = copy(m1)
    @test m7 == m1
    @test m7.objects !== m1.objects  # Different object
end

@testset "Multiset String Macro" begin
    m = multiset"a{2}, b, c{3}"
    @test get(m, "a") == 2
    @test get(m, "b") == 1
    @test get(m, "c") == 3
    
    m_single = multiset"a"
    @test get(m_single, "a") == 1
    
    m_empty = multiset""
    @test isempty(m_empty)
end
