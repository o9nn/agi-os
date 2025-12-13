#!/usr/bin/env julia

"""
Membrane Dissolution Example

Demonstrates membrane dissolution - when a membrane dissolves,
its contents are transferred to its parent membrane.
"""

using PSystems

println("=" ^ 60)
println("Membrane Dissolution Example")
println("=" ^ 60)

# Create nested membrane structure
skin = Membrane(1, 1, nothing)
inner = Membrane(2, 2, 1)

system = PSystem(
    membranes=[skin, inner],
    alphabet=["trigger", "resource", "product"],
    initial_multisets=Dict(
        1 => Multiset("resource" => 5),
        2 => Multiset("trigger" => 1, "resource" => 3)
    ),
    rules=[
        # In inner membrane: when trigger is consumed, produce product AND dissolve
        Rule(2, Multiset("trigger" => 1), Multiset("product" => 2), :here, true),
        
        # In skin: resources convert to products
        Rule(1, Multiset("resource" => 1), Multiset("product" => 1))
    ]
)

println("\nSystem Overview:")
print_system(system)

println("\nInitial State:")
println("  Skin membrane (1): $(system.initial_multisets[1])")
println("  Inner membrane (2): $(system.initial_multisets[2])")
println("  (Inner membrane will dissolve when 'trigger' is consumed)")

println("\nRunning simulation...")
result = simulate(system, max_steps=10, trace=true)

# Track dissolution
println("\nStep-by-Step Evolution:")
println("-" ^ 70)
dissolution_step = -1
for (i, config) in enumerate(result.trace)
    step_num = i - 1
    println("\nStep $step_num:")
    
    if is_active(config, 1)
        ms1 = get_multiset(config, 1)
        println("  Membrane 1 (skin): $ms1")
    else
        println("  Membrane 1 (skin): [DISSOLVED]")
    end
    
    if is_active(config, 2)
        ms2 = get_multiset(config, 2)
        println("  Membrane 2 (inner): $ms2")
    else
        println("  Membrane 2 (inner): [DISSOLVED]")
        if dissolution_step == -1
            dissolution_step = step_num
        end
    end
end

println("\n" * "=" ^ 70)
println("Summary:")
println("  Dissolution occurred at step: $dissolution_step")
println("  Final skin contents: $(get_multiset(result.final_config, 1))")
println("\nNote: When the inner membrane dissolved, its 'resource' objects")
println("      were transferred to the skin membrane.")
