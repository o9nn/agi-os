"""
    PSystems

A Julia implementation of P-Systems (Membrane Computing) with P-Lingua language support.

P-Systems are bio-inspired computational models based on the structure and functioning
of living cells. This package provides:

- Core P-System data structures (membranes, multisets, rules)
- P-Lingua language parser
- P-System simulation engine
- Examples and utilities

# Exports

## Core Types
- `Membrane`: Represents a membrane in the system
- `Multiset`: Collection of objects with multiplicities
- `Rule`: Evolution rule for membranes
- `PSystem`: Complete P-System definition
- `Configuration`: State of the P-System at a given time

## Parser
- `parse_plingua`: Parse P-Lingua code into a PSystem

## Simulation
- `simulate`: Run P-System computation
- `step!`: Execute single computation step

## Utilities
- `Multiset(::Dict)`: Create multiset from dictionary
- `Multiset(::Pair...)`: Create multiset from pairs

# Examples

```julia
using PSystems

# Create a simple P-System
system = PSystem(
    membranes = [Membrane(1, 1, nothing)],
    alphabet = ["a", "b"],
    initial_multisets = Dict(1 => Multiset("a" => 2)),
    rules = [Rule(1, Multiset("a" => 1), Multiset("b" => 2))]
)

# Simulate
result = simulate(system, max_steps=10)
```
"""
module PSystems

using DataStructures: OrderedDict, DefaultDict
using OrderedCollections: OrderedDict as ODict
using Random

# Core data structures
include("multiset.jl")
include("membrane.jl")
include("rule.jl")
include("psystem.jl")
include("configuration.jl")

# P-Lingua parser
include("plingua/lexer.jl")
include("plingua/parser.jl")
include("plingua/ast.jl")

# Simulation engine
include("simulator.jl")

# Utilities
include("utils.jl")

# Exports
export Membrane, Multiset, Rule, PSystem, Configuration
export parse_plingua
export simulate, step!, applicable_rules
export get_multiset, set_multiset!, is_active, is_halted
export is_applicable, apply_rule
export is_skin, is_elementary, add_child!, remove_child!
export get_membrane, get_rules_for_membrane, get_skin_membrane, get_children, validate
export print_trace, print_system
export @multiset_str

end # module PSystems
