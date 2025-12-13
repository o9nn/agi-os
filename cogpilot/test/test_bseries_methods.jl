"""
Test suite for JJJML B-Series Methods

Tests numerical integration methods:
- Explicit Euler
- Explicit Midpoint
- Heun's method
- RK4 (Classical Runge-Kutta)
- Dormand-Prince (adaptive)
"""

using Test
using LinearAlgebra

# Include JJJML module
include(joinpath(@__DIR__, "..", "src", "JJJML", "JJJML.jl"))
using .JJJML

@testset "B-Series Methods Tests" begin
    
    # Test problem: exponential decay dy/dt = -y, y(0) = 1
    # Exact solution: y(t) = exp(-t)
    
    @testset "Explicit Euler" begin
        method = ExplicitEuler()
        f = y -> -y
        y0 = [1.0]
        
        # Single step
        y1 = bseries_step(method, f, y0, 0.1)
        @test y1 isa Vector
        @test length(y1) == 1
        
        # Expected: y1 = y0 + h*f(y0) = 1 + 0.1*(-1) = 0.9
        @test y1[1] ≈ 0.9
        
        # Integration
        times, solution = integrate(method, f, y0, (0.0, 1.0), 0.1)
        @test length(times) >= 11  # At least 11 points
        @test length(solution) >= 11
        @test solution[1][1] ≈ 1.0
        
        # Should decay (approximately)
        @test solution[end][1] < solution[1][1]
    end
    
    @testset "Explicit Midpoint" begin
        method = ExplicitMidpoint()
        f = y -> -y
        y0 = [1.0]
        
        # Single step
        y1 = bseries_step(method, f, y0, 0.1)
        @test y1 isa Vector
        
        # Integration
        times, solution = integrate(method, f, y0, (0.0, 1.0), 0.1)
        @test length(times) >= 10
        
        # Should be more accurate than Euler
        exact = exp(-1.0)
        @test abs(solution[end][1] - exact) < 0.01
    end
    
    @testset "Heun's Method" begin
        method = Heun()
        f = y -> -y
        y0 = [1.0]
        
        # Single step
        y1 = bseries_step(method, f, y0, 0.1)
        @test y1 isa Vector
        
        # Integration
        times, solution = integrate(method, f, y0, (0.0, 1.0), 0.1)
        @test length(times) >= 10
        
        # Should be accurate (order 2 method)
        exact = exp(-1.0)
        @test abs(solution[end][1] - exact) < 0.01
    end
    
    @testset "RK4 (Classical Runge-Kutta)" begin
        method = RK4()
        f = y -> -y
        y0 = [1.0]
        
        # Single step
        y1 = bseries_step(method, f, y0, 0.1)
        @test y1 isa Vector
        
        # Integration
        times, solution = integrate(method, f, y0, (0.0, 1.0), 0.1)
        @test length(times) >= 10
        
        # Should be very accurate (order 4 method)
        exact = exp(-1.0)
        error = abs(solution[end][1] - exact)
        @test error < 1e-5
        
        # Test on harmonic oscillator: d²y/dt² = -y
        # Convert to system: dy1/dt = y2, dy2/dt = -y1
        function harmonic(y)
            return [y[2], -y[1]]
        end
        
        y0_osc = [1.0, 0.0]  # Start at y=1, v=0
        times, solution = integrate(method, harmonic, y0_osc, (0.0, 2π), 0.1)
        
        # After one period, should return to initial state
        @test abs(solution[end][1] - 1.0) < 1e-4
        @test abs(solution[end][2] - 0.0) < 1e-4
    end
    
    @testset "Dormand-Prince" begin
        method = DormandPrince()
        f = y -> -y
        y0 = [1.0]
        
        # Single step (returns y_new and error estimate)
        y1, error = bseries_step(method, f, y0, 0.1)
        @test y1 isa Vector
        @test error isa Vector
        @test length(y1) == length(y0)
        @test length(error) == length(y0)
        
        # Fixed step integration
        times, solution = integrate(method, f, y0, (0.0, 1.0), 0.1)
        @test length(times) >= 10  # At least 10 points
        
        # Should be very accurate (order 5 method)
        exact = exp(-1.0)
        error_val = abs(solution[end][1] - exact)
        @test error_val < 1e-6
        
        # Adaptive integration
        times_adaptive, solution_adaptive, stats = integrate_adaptive(
            method, f, y0, (0.0, 1.0),
            rtol=1e-6, atol=1e-8
        )
        
        @test length(times_adaptive) > 0
        @test stats.num_steps > 0
        @test stats.num_accepted > 0
        
        # Should be very accurate with adaptive stepping
        error_adaptive = abs(solution_adaptive[end][1] - exact)
        @test error_adaptive < 1e-6
        
        # Test that adaptive uses fewer steps than fixed for easy problem
        # (This may not always be true, but should be for smooth exponential decay)
        @test length(times_adaptive) <= 20  # Reasonable number of steps
    end
    
    @testset "Method Comparison" begin
        # Test all methods on same problem
        f = y -> -y
        y0 = [1.0]
        t_span = (0.0, 1.0)
        dt = 0.1
        exact = exp(-1.0)
        
        methods = [
            ("Euler", ExplicitEuler()),
            ("Midpoint", ExplicitMidpoint()),
            ("Heun", Heun()),
            ("RK4", RK4()),
        ]
        
        errors = Float64[]
        
        for (name, method) in methods
            times, solution = integrate(method, f, y0, t_span, dt)
            error = abs(solution[end][1] - exact)
            push!(errors, error)
        end
        
        # Higher order methods should be more accurate
        @test errors[4] < errors[3]  # RK4 < Heun
        @test errors[3] < errors[1]  # Heun < Euler
        @test errors[2] < errors[1]  # Midpoint < Euler
    end
    
    @testset "Stiff Problem (Van der Pol)" begin
        # Van der Pol oscillator: y'' + μ(y² - 1)y' + y = 0
        # As system: dy1/dt = y2, dy2/dt = μ(1 - y1²)y2 - y1
        
        μ = 1.0  # Mild stiffness
        function vanderpol(y)
            return [y[2], μ*(1 - y[1]^2)*y[2] - y[1]]
        end
        
        y0 = [2.0, 0.0]
        
        # RK4 should handle this
        method = RK4()
        times, solution = integrate(method, vanderpol, y0, (0.0, 10.0), 0.01)
        
        @test length(times) > 0
        @test all(isfinite.(solution[end]))
        
        # Dormand-Prince adaptive should also work
        method_dp = DormandPrince()
        times_dp, solution_dp, stats = integrate_adaptive(
            method_dp, vanderpol, y0, (0.0, 10.0),
            rtol=1e-4, atol=1e-6
        )
        
        @test length(times_dp) > 0
        @test all(isfinite.(solution_dp[end]))
        @test stats.num_accepted > 0
    end
    
end

println("\n" * "="^60)
println("All B-Series Methods tests passed! ✓")
println("="^60)
