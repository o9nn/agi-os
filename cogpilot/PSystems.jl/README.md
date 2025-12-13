# PSystems.jl

[![Build Status](https://github.com/SciML/ModelingToolkitStandardLibrary.jl/workflows/CI/badge.svg)](https://github.com/SciML/ModelingToolkitStandardLibrary.jl/actions?query=workflow%3ACI)

A Julia implementation of **P-Systems Membrane Computing** with support for the **P-Lingua** language.

## Overview

PSystems.jl provides a modern, efficient implementation of P-Systems (membrane systems) - a bio-inspired computational model based on the structure and functioning of living cells. This package includes:

- **P-Systems Core**: Data structures for membranes, multisets, and evolution rules
- **P-Lingua Parser**: Full support for the P-Lingua modeling language
- **Simulator**: Efficient execution engine for P-Systems computations
- **Examples**: Collection of classic P-System models

## Features

### P-Systems Components

- **Membrane Structures**: Hierarchical, nested membrane configurations
- **Multisets**: Object collections with multiplicities
- **Evolution Rules**: Rewriting rules with priorities
- **Communication**: Object transport between membranes
- **Dissolution**: Membrane dissolution capabilities

### P-Lingua Language Support

P-Lingua is a domain-specific language for defining P-Systems. This package provides:

- Full lexer and parser for P-Lingua syntax
- Support for:
  - Membrane structure definitions
  - Initial multiset configurations
  - Evolution rule specifications
  - Priority relations
  - Multiple P-System variants

### Simulation Engine

- Non-deterministic rule application
- Maximal parallelism semantics
- Configurable execution strategies
- Computation traces and results
- Halting condition detection

## Installation

```julia
using Pkg
Pkg.add("PSystems")
```

## Quick Start

### Example 1: Simple P-System

```julia
using PSystems

# Define a simple P-System programmatically
system = PSystem(
    membranes = [Membrane(1, parent=0), Membrane(2, parent=1)],
    alphabet = ["a", "b", "c"],
    initial_multisets = Dict(
        1 => Multiset(Dict("a" => 2, "b" => 1)),
        2 => Multiset(Dict("c" => 3))
    ),
    rules = [
        Rule(membrane=1, lhs=Multiset("a"=>1), rhs=Multiset("b"=>2)),
        Rule(membrane=2, lhs=Multiset("c"=>1), rhs=Multiset("a"=>1), target=1)
    ]
)

# Run the simulation
result = simulate(system, max_steps=100)
println("Computation result: ", result)
```

### Example 2: Using P-Lingua

```julia
using PSystems

# Parse a P-Lingua file
plingua_code = """
@model<transition>

def main() {
    @mu = [[]'2]'1;
    
    @ms(1) = a{2}, b;
    @ms(2) = c{3};
    
    [a]'1 --> [b{2}]'1;
    [c]'2 --> (a, out);
}
"""

# Parse and create P-System
system = parse_plingua(plingua_code)

# Simulate
result = simulate(system)
println("Final configuration: ", result.final_multisets)
```

### Example 3: Generator System (Computing Powers of 2)

```julia
using PSystems

plingua_code = """
@model<transition>

def powers_of_2() {
    @mu = []'1;
    @ms(1) = a, d;
    
    [a --> a, b]'1;
    [b --> b, c]'1;
    [c --> c, a]'1;
    [d --> d]'1 > [a --> a]'1;
}
"""

system = parse_plingua(plingua_code)
result = simulate(system, max_steps=10)

# Result will contain 2^n copies of object 'a' after n steps
```

## P-Lingua Syntax Overview

P-Lingua provides a concise way to define P-Systems:

```
@model<type>              # System type (transition, probabilistic, etc.)

def system_name() {
    @mu = structure;      # Membrane structure
    @ms(id) = multiset;   # Initial multisets
    
    [lhs]'label --> [rhs]'label;  # Evolution rules
    rule1 > rule2;                # Priority relations
}
```

### Membrane Structure Notation

- `[]'1` - Single membrane with label 1
- `[[]'2]'1` - Nested membrane: membrane 2 inside membrane 1
- `[[]'2 []'3]'1` - Parallel membranes: both 2 and 3 inside 1

### Multiset Notation

- `a` - Single object 'a'
- `a{3}` - Three copies of 'a'
- `a, b{2}, c` - Multiset with a, two b's, and c

### Rule Notation

- `[a]'1 --> [b]'1` - Object 'a' evolves to 'b' in membrane 1
- `[a]'1 --> (b, out)` - Object 'a' in membrane 1 sends 'b' to parent
- `[a]'1 --> (b, in_2)` - Object 'a' sends 'b' into child membrane 2
- `[a]'1 --> []'1` - Membrane 1 dissolves when consuming 'a'

## Architecture

### Core Data Structures

```julia
# Membrane: represents a single membrane
struct Membrane
    id::Int
    label::Int
    parent::Union{Int, Nothing}
end

# Multiset: collection of objects with multiplicities
struct Multiset
    objects::Dict{String, Int}
end

# Rule: evolution rule for a membrane
struct Rule
    membrane::Int
    lhs::Multiset
    rhs::Multiset
    target::Union{Int, Symbol, Nothing}  # :out, :in_X, or membrane id
    dissolve::Bool
    priority::Int
end

# PSystem: complete P-System definition
struct PSystem
    membranes::Vector{Membrane}
    alphabet::Vector{String}
    initial_multisets::Dict{Int, Multiset}
    rules::Vector{Rule}
end
```

### Simulation Process

1. **Configuration**: Current multisets in each membrane
2. **Rule Selection**: Choose applicable rules (non-deterministically)
3. **Rule Application**: Apply selected rules in maximal parallelism
4. **Update**: Generate new configuration
5. **Termination**: Check halting conditions
6. **Repeat**: Continue until halting or max steps

## Advanced Features

### Non-Determinism

The simulator handles non-deterministic choices when multiple rules can be applied:

```julia
result = simulate(system, strategy=:random)  # Random choice
result = simulate(system, strategy=:first)   # First applicable rule
```

### Computation Traces

```julia
result = simulate(system, trace=true)

for (step, config) in enumerate(result.trace)
    println("Step $step: ", config)
end
```

### Custom Halting Conditions

```julia
halt_fn(config) = all(m -> isempty(config.multisets[m]), config.membranes)
result = simulate(system, halt_condition=halt_fn)
```

## Examples Included

The package includes several classic P-System examples:

1. **Generator Systems**: Compute sequences (powers of 2, Fibonacci, etc.)
2. **Decision Problems**: SAT solvers, graph problems
3. **Arithmetic**: Addition, multiplication with membrane computing
4. **Sorting Networks**: Parallel sorting algorithms

See `examples/` directory for complete implementations.

## References

### P-Systems
- Păun, G. (2000). "Computing with Membranes". Journal of Computer and System Sciences.
- Păun, G. (2002). "Membrane Computing: An Introduction". Springer-Verlag.

### P-Lingua
- García-Quismondo, M., et al. (2009). "P-Lingua: A Programming Language for Membrane Computing".
- The P-Lingua Project: http://www.p-lingua.org/

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

MIT License - see LICENSE.md for details.

## Acknowledgments

Part of the SciML ecosystem for scientific machine learning and numerical simulation.
