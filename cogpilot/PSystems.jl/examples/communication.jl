#!/usr/bin/env julia

"""
Communication Between Membranes

Demonstrates how objects can move between nested membranes
using communication rules (out and in).

Structure: [inner membrane 2 inside skin membrane 1]
"""

using PSystems

println("=" ^ 60)
println("Membrane Communication Example")
println("=" ^ 60)

# Create nested membrane structure
skin = Membrane(1, 1, nothing)
inner = Membrane(2, 2, 1)

system = PSystem(
    membranes=[skin, inner],
    alphabet=["a", "b", "c", "d"],
    initial_multisets=Dict(
        1 => Multiset("a" => 3),
        2 => Multiset("b" => 2)
    ),
    rules=[
        # In skin: a -> c (simple rewriting)
        Rule(1, Multiset("a" => 1), Multiset("c" => 1)),
        
        # In inner: b sends result to parent (out)
        Rule(2, Multiset("b" => 1), Multiset("d" => 1), :out)
    ]
)

println("\nSystem Overview:")
print_system(system)

println("\nInitial Configuration:")
println("  Membrane 1 (skin): $(system.initial_multisets[1])")
println("  Membrane 2 (inner): $(system.initial_multisets[2])")

println("\nRunning simulation...")
result = simulate(system, max_steps=10, trace=true)

# Show membrane contents at each step
println("\nConfiguration Evolution:")
println("Step | Skin Membrane (1)     | Inner Membrane (2)")
println("-" ^ 60)
for (i, config) in enumerate(result.trace)
    step_num = i - 1
    ms1 = get_multiset(config, 1)
    ms2 = get_multiset(config, 2)
    
    active1 = is_active(config, 1) ? "" : " [dissolved]"
    active2 = is_active(config, 2) ? "" : " [dissolved]"
    
    println("$(lpad(step_num, 4)) | $(rpad(string(ms1) * active1, 21)) | $(ms2)$active2")
end

println("\nFinal Result:")
println("  Membrane 1: $(get_multiset(result.final_config, 1))")
println("  Membrane 2: $(get_multiset(result.final_config, 2))")
println("\nNote: Objects moved from inner membrane to skin via 'out' communication.")
