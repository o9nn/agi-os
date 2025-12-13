"""
JJJML B-Series Methods Demo

Demonstrates numerical integration methods:
- Explicit Euler (order 1)
- Explicit Midpoint (order 2)
- Heun's method (order 2)
- RK4 (order 4)
- Dormand-Prince (adaptive order 5)
"""

# Include JJJML module
include(joinpath(@__DIR__, "..", "src", "JJJML", "JJJML.jl"))
using .JJJML
using LinearAlgebra
using Printf

println("=" ^ 70)
println("JJJML B-Series Methods Demonstration")
println("=" ^ 70)
println()

#
# Part 1: Method Comparison on Exponential Decay
#
println("Part 1: Exponential Decay (dy/dt = -y, y(0) = 1)")
println("-" ^ 70)

# Define problem
f = y -> -y
y0 = [1.0]
t_span = (0.0, 2.0)
dt = 0.1

# Exact solution
exact(t) = exp(-t)

println("Problem: dy/dt = -y, y(0) = 1")
println("Exact solution: y(t) = exp(-t)")
println("Time span: [0, 2]")
println("Step size: $dt")
println()

# Methods to test
methods = [
    ("Explicit Euler", ExplicitEuler()),
    ("Explicit Midpoint", ExplicitMidpoint()),
    ("Heun's Method", Heun()),
    ("Classical RK4", RK4()),
]

println("Method Comparison:")
println("-" ^ 70)
println("Method              | Final Value | Exact       | Error")
println("-" ^ 70)

results = []
for (name, method) in methods
    times, solution = integrate(method, f, y0, t_span, dt)
    final_value = solution[end][1]
    exact_value = exact(times[end])
    error = abs(final_value - exact_value)
    
    push!(results, (name, solution, error))
    
    @printf("%-19s | %.8f | %.8f | %.2e\n", name, final_value, exact_value, error)
end
println("-" ^ 70)
println()

# Observations
println("Observations:")
println("  • RK4 has the smallest error (~10⁻⁷)")
println("  • Euler has the largest error (~10⁻²)")
println("  • Higher-order methods are more accurate for the same step size")
println()

#
# Part 2: Adaptive Integration with Dormand-Prince
#
println("Part 2: Adaptive Integration (Dormand-Prince)")
println("-" ^ 70)

method_dp = DormandPrince()

# Tight tolerance
println("High accuracy (rtol=1e-9, atol=1e-12):")
times_tight, solution_tight, stats_tight = integrate_adaptive(
    method_dp, f, y0, t_span,
    rtol=1e-9, atol=1e-12
)

final_tight = solution_tight[end][1]
exact_final = exact(times_tight[end])
error_tight = abs(final_tight - exact_final)

println("  Steps taken: $(stats_tight.num_accepted)")
println("  Steps rejected: $(stats_tight.num_rejected)")
println("  Final value: $final_tight")
println("  Exact value: $exact_final")
println("  Error: $(round(error_tight, sigdigits=3))")
println()

# Loose tolerance
println("Low accuracy (rtol=1e-3, atol=1e-6):")
times_loose, solution_loose, stats_loose = integrate_adaptive(
    method_dp, f, y0, t_span,
    rtol=1e-3, atol=1e-6
)

final_loose = solution_loose[end][1]
error_loose = abs(final_loose - exact_final)

println("  Steps taken: $(stats_loose.num_accepted)")
println("  Steps rejected: $(stats_loose.num_rejected)")
println("  Final value: $final_loose")
println("  Error: $(round(error_loose, sigdigits=3))")
println()

println("Observations:")
println("  • Tighter tolerance → more steps, higher accuracy")
println("  • Adaptive method adjusts step size automatically")
println("  • Efficient: takes only as many steps as needed")
println()

#
# Part 3: Harmonic Oscillator
#
println("Part 3: Harmonic Oscillator (d²y/dt² = -y)")
println("-" ^ 70)

# Convert to first-order system:
# y₁ = y, y₂ = dy/dt
# dy₁/dt = y₂
# dy₂/dt = -y₁
function harmonic(y)
    return [y[2], -y[1]]
end

y0_osc = [1.0, 0.0]  # Start at y=1, v=0
t_span_osc = (0.0, 2π)  # One full period

println("System: dy₁/dt = y₂, dy₂/dt = -y₁")
println("Initial: y₁(0) = 1, y₂(0) = 0")
println("Exact solution: y₁(t) = cos(t), y₂(t) = -sin(t)")
println()

# Integrate with RK4
method_rk4 = RK4()
times_osc, solution_osc = integrate(method_rk4, harmonic, y0_osc, t_span_osc, 0.01)

println("Integration with RK4 (dt=0.01):")
println("  Initial: y₁ = $(y0_osc[1]), y₂ = $(y0_osc[2])")
println("  Final:   y₁ = $(round(solution_osc[end][1], digits=6)), y₂ = $(round(solution_osc[end][2], digits=6))")
println("  Expected: y₁ = 1.0, y₂ = 0.0 (after one period)")
println("  Error:   y₁ = $(abs(solution_osc[end][1] - 1.0)), y₂ = $(abs(solution_osc[end][2]))")
println()

# Check energy conservation (should be E = ½(y₁² + y₂²) = 0.5)
energies = [0.5 * (sol[1]^2 + sol[2]^2) for sol in solution_osc]
energy_drift = maximum(abs.(energies .- 0.5))

println("Energy conservation:")
println("  Initial energy: $(energies[1])")
println("  Final energy:   $(energies[end])")
println("  Max drift:      $energy_drift")
println("  Relative drift: $(round(100 * energy_drift / energies[1], digits=3))%")
println()

#
# Part 4: Stiff Problem (Van der Pol)
#
println("Part 4: Stiff Problem (Van der Pol Oscillator)")
println("-" ^ 70)

# Van der Pol: y'' + μ(y² - 1)y' + y = 0
μ = 5.0  # Moderate stiffness

function vanderpol(y)
    return [y[2], μ*(1 - y[1]^2)*y[2] - y[1]]
end

y0_vdp = [2.0, 0.0]
t_span_vdp = (0.0, 20.0)

println("Parameter μ = $μ (controls stiffness)")
println("Initial: y₁(0) = 2, y₂(0) = 0")
println()

# Try adaptive integration
times_vdp, solution_vdp, stats_vdp = integrate_adaptive(
    method_dp, vanderpol, y0_vdp, t_span_vdp,
    rtol=1e-4, atol=1e-6
)

println("Adaptive integration (Dormand-Prince):")
println("  Steps accepted: $(stats_vdp.num_accepted)")
println("  Steps rejected: $(stats_vdp.num_rejected)")
println("  Rejection rate: $(round(100 * stats_vdp.num_rejected / (stats_vdp.num_accepted + stats_vdp.num_rejected), digits=1))%")
println("  Average dt:     $(round(t_span_vdp[2] / stats_vdp.num_accepted, digits=4))")
println()

println("Observations:")
println("  • Stiff problems require many steps")
println("  • Adaptive method adjusts to problem difficulty")
println("  • Some steps rejected due to error tolerance")
println()

#
# Summary
#
println("=" ^ 70)
println("Summary")
println("=" ^ 70)
println()
println("JJJML provides production-quality numerical integrators:")
println()
println("Fixed-Step Methods:")
println("  • Explicit Euler:    Simple, order 1, good for learning")
println("  • Explicit Midpoint: Classic RK2, order 2")
println("  • Heun's Method:     Improved Euler, order 2")
println("  • Classical RK4:     Most popular, order 4, excellent accuracy")
println()
println("Adaptive Methods:")
println("  • Dormand-Prince:    Industry standard (MATLAB ode45, SciPy default)")
println("                       Order 5(4) with embedded error estimation")
println("                       Automatic step size control")
println()
println("Applications:")
println("  ✓ ODEs from physics (oscillators, orbital mechanics)")
println("  ✓ Chemical kinetics")
println("  ✓ Population dynamics")
println("  ✓ Neural ODEs (AI/ML)")
println("  ✓ B-series integration in DeepTreeEcho reservoir computing")
println()
println("Next steps:")
println("  • Implicit methods for stiff problems (BDF, Radau)")
println("  • Symplectic integrators for Hamiltonian systems")
println("  • Exponential integrators")
println("  • Connection to RootedTrees.jl for full B-series analysis")
