using Test
using PSystems
using PSystems: is_skin, is_elementary, add_child!, remove_child!

@testset "Membrane Construction" begin
    # Skin membrane
    skin = Membrane(1, 1, nothing)
    @test skin.id == 1
    @test skin.label == 1
    @test skin.parent === nothing
    @test isempty(skin.children)
    @test is_skin(skin)
    @test is_elementary(skin)
    
    # Nested membrane
    inner = Membrane(2, 2, 1)
    @test inner.id == 2
    @test inner.label == 2
    @test inner.parent == 1
    @test !is_skin(inner)
end

@testset "Membrane Relationships" begin
    parent = Membrane(1, 1, nothing)
    child1 = Membrane(2, 2, 1)
    child2 = Membrane(3, 2, 1)
    
    # Add children
    add_child!(parent, child1.id)
    add_child!(parent, child2.id)
    
    @test length(parent.children) == 2
    @test 2 in parent.children
    @test 3 in parent.children
    @test !is_elementary(parent)
    
    # Add duplicate (should not duplicate)
    add_child!(parent, child1.id)
    @test length(parent.children) == 2
    
    # Remove child
    remove_child!(parent, child1.id)
    @test length(parent.children) == 1
    @test !(2 in parent.children)
    @test 3 in parent.children
end
