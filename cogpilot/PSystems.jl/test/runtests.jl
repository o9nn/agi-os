using SafeTestsets, Test

const GROUP = get(ENV, "GROUP", "All")

@testset "PSystems.jl Tests" begin
    if GROUP == "All" || GROUP == "Core"
        @safetestset "Multiset Tests" begin
            include("test_multiset.jl")
        end
        
        @safetestset "Membrane Tests" begin
            include("test_membrane.jl")
        end
        
        @safetestset "Rule Tests" begin
            include("test_rule.jl")
        end
        
        @safetestset "PSystem Tests" begin
            include("test_psystem.jl")
        end
        
        @safetestset "Configuration Tests" begin
            include("test_configuration.jl")
        end
        
        @safetestset "Simulator Tests" begin
            include("test_simulator.jl")
        end
        
        @safetestset "P-Lingua Lexer Tests" begin
            include("test_plingua_lexer.jl")
        end
        
        @safetestset "P-Lingua Parser Tests" begin
            include("test_plingua_parser.jl")
        end
        
        @safetestset "Integration Tests" begin
            include("test_integration.jl")
        end
    end
end
