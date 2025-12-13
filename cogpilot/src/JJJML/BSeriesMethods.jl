"""
Enhanced B-Series Methods

Implements classic numerical integration methods as B-series:
- Explicit Euler (order 1)
- Explicit Midpoint (order 2)
- Heun's method (order 2)
- Classical Runge-Kutta (RK4, order 4)
- Dormand-Prince (order 5)

Each method is represented as a B-series with specific coefficients.
"""

using LinearAlgebra

"""
    ExplicitEuler{T}

Explicit Euler method: y₁ = y₀ + h·f(y₀)

B-series representation:
- Order 1
- Single tree: ∅ with coefficient 1
"""
struct ExplicitEuler{T}
    kernel::BSeriesKernel{T}
    
    function ExplicitEuler{T}() where T
        genome = Dict{RootedTree, T}()
        genome[RootedTree([1])] = T(1)
        kernel = BSeriesKernel{T}(genome, 1)
        new{T}(kernel)
    end
end

ExplicitEuler() = ExplicitEuler{Float64}()

"""
    bseries_step(method::ExplicitEuler, f, y, h)

Take one step with Explicit Euler.
"""
function bseries_step(method::ExplicitEuler, f, y, h)
    return y + h * f(y)
end

"""
    ExplicitMidpoint{T}

Explicit Midpoint method (RK2):
k₁ = f(y₀)
k₂ = f(y₀ + h/2·k₁)
y₁ = y₀ + h·k₂

B-series representation: Order 2
"""
struct ExplicitMidpoint{T}
    kernel::BSeriesKernel{T}
    
    function ExplicitMidpoint{T}() where T
        genome = Dict{RootedTree, T}()
        # Order 1 tree
        genome[RootedTree([1])] = T(0)  # No order-1 contribution
        # Order 2 tree: [∅]
        genome[RootedTree([1, 2])] = T(1)
        kernel = BSeriesKernel{T}(genome, 2)
        new{T}(kernel)
    end
end

ExplicitMidpoint() = ExplicitMidpoint{Float64}()

"""
    bseries_step(method::ExplicitMidpoint, f, y, h)

Take one step with Explicit Midpoint.
"""
function bseries_step(method::ExplicitMidpoint, f, y, h)
    k1 = f(y)
    k2 = f(y + (h/2) * k1)
    return y + h * k2
end

"""
    Heun{T}

Heun's method (improved Euler):
k₁ = f(y₀)
k₂ = f(y₀ + h·k₁)
y₁ = y₀ + h/2·(k₁ + k₂)

B-series representation: Order 2
"""
struct Heun{T}
    kernel::BSeriesKernel{T}
    
    function Heun{T}() where T
        genome = Dict{RootedTree, T}()
        # Order 1 tree
        genome[RootedTree([1])] = T(1/2)
        # Order 2 tree
        genome[RootedTree([1, 2])] = T(1/2)
        kernel = BSeriesKernel{T}(genome, 2)
        new{T}(kernel)
    end
end

Heun() = Heun{Float64}()

"""
    bseries_step(method::Heun, f, y, h)

Take one step with Heun's method.
"""
function bseries_step(method::Heun, f, y, h)
    k1 = f(y)
    k2 = f(y + h * k1)
    return y + (h/2) * (k1 + k2)
end

"""
    RK4{T}

Classical 4th-order Runge-Kutta:
k₁ = f(y₀)
k₂ = f(y₀ + h/2·k₁)
k₃ = f(y₀ + h/2·k₂)
k₄ = f(y₀ + h·k₃)
y₁ = y₀ + h/6·(k₁ + 2k₂ + 2k₃ + k₄)

Most popular method in practice!
"""
struct RK4{T}
    kernel::BSeriesKernel{T}
    
    function RK4{T}() where T
        genome = Dict{RootedTree, T}()
        # NOTE: These are simplified B-series coefficients for demonstration.
        # The full RK4 B-series expansion has many more trees (8 trees up to order 4).
        # In practice, we use the direct RK4 formula in bseries_step() which is exact.
        # This B-series representation is kept for educational purposes.
        genome[RootedTree([1])] = T(1/6)
        genome[RootedTree([1, 2])] = T(1/3)
        genome[RootedTree([1, 2, 2])] = T(1/6)
        genome[RootedTree([1, 2, 3])] = T(1/6)
        kernel = BSeriesKernel{T}(genome, 4)
        new{T}(kernel)
    end
end

RK4() = RK4{Float64}()

"""
    bseries_step(method::RK4, f, y, h)

Take one step with classical RK4.
"""
function bseries_step(method::RK4, f, y, h)
    k1 = f(y)
    k2 = f(y + (h/2) * k1)
    k3 = f(y + (h/2) * k2)
    k4 = f(y + h * k3)
    return y + (h/6) * (k1 + 2*k2 + 2*k3 + k4)
end

"""
    DormandPrince{T}

Dormand-Prince method (RK45):
Embedded RK method with order 4 and 5 error estimation.

Most popular adaptive method in practice!
Used by default in MATLAB's ode45 and SciPy's solve_ivp.
"""
struct DormandPrince{T}
    # Butcher tableau coefficients
    a21::T; a31::T; a32::T
    a41::T; a42::T; a43::T
    a51::T; a52::T; a53::T; a54::T
    a61::T; a62::T; a63::T; a64::T; a65::T
    a71::T; a72::T; a73::T; a74::T; a75::T; a76::T
    
    # Order 5 weights
    b1::T; b2::T; b3::T; b4::T; b5::T; b6::T; b7::T
    
    # Order 4 weights (for error estimation)
    b1_star::T; b2_star::T; b3_star::T; b4_star::T; b5_star::T; b6_star::T; b7_star::T
    
    function DormandPrince{T}() where T
        # Dormand-Prince coefficients (DOPRI5)
        new{T}(
            # a coefficients
            T(1/5),
            T(3/40), T(9/40),
            T(44/45), T(-56/15), T(32/9),
            T(19372/6561), T(-25360/2187), T(64448/6561), T(-212/729),
            T(9017/3168), T(-355/33), T(46732/5247), T(49/176), T(-5103/18656),
            T(35/384), T(0), T(500/1113), T(125/192), T(-2187/6784), T(11/84),
            # b coefficients (order 5)
            T(35/384), T(0), T(500/1113), T(125/192), T(-2187/6784), T(11/84), T(0),
            # b* coefficients (order 4, for error estimation)
            T(5179/57600), T(0), T(7571/16695), T(393/640), T(-92097/339200), T(187/2100), T(1/40)
        )
    end
end

DormandPrince() = DormandPrince{Float64}()

"""
    bseries_step(method::DormandPrince, f, y, h)

Take one step with Dormand-Prince.
Returns (y_new, y_error) where y_error estimates the local truncation error.
"""
function bseries_step(method::DormandPrince, f, y, h)
    # Compute stages
    k1 = f(y)
    k2 = f(y + h * method.a21 * k1)
    k3 = f(y + h * (method.a31 * k1 + method.a32 * k2))
    k4 = f(y + h * (method.a41 * k1 + method.a42 * k2 + method.a43 * k3))
    k5 = f(y + h * (method.a51 * k1 + method.a52 * k2 + method.a53 * k3 + method.a54 * k4))
    k6 = f(y + h * (method.a61 * k1 + method.a62 * k2 + method.a63 * k3 + method.a64 * k4 + method.a65 * k5))
    k7 = f(y + h * (method.a71 * k1 + method.a72 * k2 + method.a73 * k3 + method.a74 * k4 + method.a75 * k5 + method.a76 * k6))
    
    # Order 5 solution
    y_new = y + h * (method.b1 * k1 + method.b3 * k3 + method.b4 * k4 + method.b5 * k5 + method.b6 * k6)
    
    # Order 4 solution (for error estimation)
    y_4 = y + h * (method.b1_star * k1 + method.b3_star * k3 + method.b4_star * k4 + 
                   method.b5_star * k5 + method.b6_star * k6 + method.b7_star * k7)
    
    # Error estimate
    y_error = y_new - y_4
    
    return y_new, y_error
end

"""
    integrate(method, f, y0, t_span, dt)

Integrate ODE dy/dt = f(y) from t_span[1] to t_span[2] with step size dt.

# Arguments
- `method`: Integration method (ExplicitEuler, RK4, etc.)
- `f`: RHS function
- `y0`: Initial condition
- `t_span`: (t_start, t_end)
- `dt`: Step size

# Returns
- `(times, solution)`: Arrays of time points and solution values
"""
function integrate(method, f, y0, t_span::Tuple{Real, Real}, dt::Real)
    t_start, t_end = t_span
    t = t_start
    y = copy(y0)
    
    times = [t]
    solution = [copy(y)]
    
    while t < t_end
        h = min(dt, t_end - t)
        
        # Take step (handle methods with/without error estimation)
        result = bseries_step(method, f, y, h)
        if result isa Tuple
            y, _ = result  # Discard error estimate for now
        else
            y = result
        end
        
        t += h
        push!(times, t)
        push!(solution, copy(y))
    end
    
    return times, solution
end

"""
    integrate_adaptive(method::DormandPrince, f, y0, t_span; 
                      rtol=1e-6, atol=1e-8, dt_init=0.01)

Adaptive integration using Dormand-Prince with automatic step size control.

# Arguments
- `method::DormandPrince`: Dormand-Prince method
- `f`: RHS function
- `y0`: Initial condition
- `t_span`: (t_start, t_end)
- `rtol`: Relative tolerance
- `atol`: Absolute tolerance
- `dt_init`: Initial step size guess

# Returns
- `(times, solution, stats)`: Times, solution, and statistics
"""
function integrate_adaptive(method::DormandPrince, f, y0, t_span::Tuple{Real, Real};
                           rtol=1e-6, atol=1e-8, dt_init=0.01)
    t_start, t_end = t_span
    t = t_start
    y = copy(y0)
    dt = dt_init
    
    times = [t]
    solution = [copy(y)]
    
    # Statistics
    num_steps = 0
    num_rejected = 0
    
    safety_factor = 0.9
    min_factor = 0.2
    max_factor = 5.0
    
    while t < t_end
        # Don't overshoot
        if t + dt > t_end
            dt = t_end - t
        end
        
        # Take step
        y_new, error = bseries_step(method, f, y, dt)
        
        # Estimate error
        scale = atol .+ rtol .* max.(abs.(y), abs.(y_new))
        error_norm = norm(error ./ scale) / sqrt(length(y))
        
        num_steps += 1
        
        if error_norm <= 1.0
            # Accept step
            y = y_new
            t += dt
            push!(times, t)
            push!(solution, copy(y))
            
            # Update step size for next step
            if error_norm > 0
                factor = safety_factor * (1.0 / error_norm)^0.2
                factor = clamp(factor, min_factor, max_factor)
                dt *= factor
            else
                dt *= max_factor
            end
        else
            # Reject step
            num_rejected += 1
            
            # Reduce step size
            factor = max(safety_factor * (1.0 / error_norm)^0.25, min_factor)
            dt *= factor
        end
        
        # Safety check
        if num_steps > 10000
            @warn "Maximum number of steps reached"
            break
        end
    end
    
    stats = (
        num_steps = num_steps,
        num_accepted = num_steps - num_rejected,
        num_rejected = num_rejected,
        final_dt = dt
    )
    
    return times, solution, stats
end

# Export methods and functions
export ExplicitEuler, ExplicitMidpoint, Heun, RK4, DormandPrince
export bseries_step, integrate, integrate_adaptive
