#!/usr/bin/env julia

"""
Simple Rewriting Example

Demonstrates basic P-System with sequential rewriting rules:
a -> b -> c -> d

This shows the fundamental operation of P-Systems where objects
are transformed according to evolution rules.
"""

using PSystems

println("=" ^ 60)
println("Simple Rewriting Example")
println("=" ^ 60)

# Define the P-System using P-Lingua
plingua_code = """
@model<transition>

def simple_rewriting() {
    @mu = []'1;
    @ms(1) = a;
    
    [a]'1 --> [b]'1;
    [b]'1 --> [c]'1;
    [c]'1 --> [d]'1;
}
"""

println("\nP-Lingua Code:")
println(plingua_code)

# Parse and create the system
system = parse_plingua(plingua_code)

println("\nSystem Overview:")
print_system(system)

# Simulate with trace
println("\nRunning simulation...")
result = simulate(system, max_steps=10, trace=true)

# Print the trace
print_trace(result)

println("\nFinal Result:")
println("Steps taken: $(result.steps)")
println("Computation halted: $(result.halted)")
println("Final configuration: $(get_multiset(result.final_config, 1))")
