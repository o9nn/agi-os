"""
Utility functions for PSystems.
"""

"""
    print_trace(result::SimulationResult)

Print the computation trace in a readable format.
"""
function print_trace(result::SimulationResult)
    if isempty(result.trace)
        println("No trace available (run with trace=true)")
        return
    end
    
    println("Computation Trace:")
    println("=" ^ 60)
    
    for (i, config) in enumerate(result.trace)
        println("\nStep $( i-1):")
        println("-" ^ 60)
        
        for membrane_id in sort(collect(keys(config.multisets)))
            if is_active(config, membrane_id)
                ms = config.multisets[membrane_id]
                println("  Membrane $membrane_id: $ms")
            else
                println("  Membrane $membrane_id: [dissolved]")
            end
        end
    end
    
    println("\n" * "=" ^ 60)
    println("Total steps: $(result.steps)")
    println("Halted: $(result.halted)")
end

"""
    print_system(system::PSystem)

Print a detailed view of a P-System.
"""
function print_system(system::PSystem)
    println("P-System Definition:")
    println("=" ^ 60)
    
    println("\nAlphabet: {$(join(system.alphabet, ", "))}")
    
    println("\nMembrane Structure:")
    for membrane in system.membranes
        parent_str = membrane.parent === nothing ? "none (skin)" : string(membrane.parent)
        children_str = isempty(membrane.children) ? "none" : join(membrane.children, ", ")
        println("  Membrane $(membrane.id) [label=$(membrane.label)]")
        println("    Parent: $parent_str")
        println("    Children: $children_str")
    end
    
    println("\nInitial Configuration:")
    for (membrane_id, multiset) in sort(collect(system.initial_multisets))
        println("  Membrane $membrane_id: $multiset")
    end
    
    println("\nEvolution Rules:")
    for (i, rule) in enumerate(system.rules)
        println("  Rule $i: $rule")
    end
    
    println("=" ^ 60)
end
