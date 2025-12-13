#!/usr/bin/env julia

"""
Powers of 2 Generator

A classic P-System example that generates powers of 2.
Starting with one 'a' object, the system doubles the count 
in each cycle through objects a -> b -> c -> a.

After n complete cycles, there will be 2^n copies of object 'a'.
"""

using PSystems

println("=" ^ 60)
println("Powers of 2 Generator")
println("=" ^ 60)

# Define using programmatic API
skin = Membrane(1, 1, nothing)

system = PSystem(
    membranes=[skin],
    alphabet=["a", "b", "c"],
    initial_multisets=Dict(1 => Multiset("a" => 1)),
    rules=[
        Rule(1, Multiset("a" => 1), Multiset("a" => 1, "b" => 1)),  # a -> a, b
        Rule(1, Multiset("b" => 1), Multiset("b" => 1, "c" => 1)),  # b -> b, c
        Rule(1, Multiset("c" => 1), Multiset("c" => 1, "a" => 1))   # c -> c, a
    ]
)

println("\nSystem Overview:")
print_system(system)

println("\nRunning simulation for 10 steps...")
result = simulate(system, max_steps=10, trace=true)

# Analyze the growth of 'a' objects
println("\nGrowth of 'a' objects over time:")
println("Step | Count of 'a'")
println("-" ^ 20)
for (i, config) in enumerate(result.trace)
    ms = get_multiset(config, 1)
    count_a = get(ms, "a", 0)
    step_num = i - 1
    power_of_2 = count_a > 0 ? log2(count_a) : 0
    println("$(lpad(step_num, 4)) | $(lpad(count_a, 6))  (≈ 2^$(round(power_of_2, digits=2)))")
end

println("\nFinal configuration: $(get_multiset(result.final_config, 1))")
