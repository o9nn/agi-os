# PSystems.jl Examples

This directory contains example P-Systems demonstrating various computational models.

## Examples

### 1. simple_rewriting.jl
Basic rewriting rules demonstrating sequential object transformations.

### 2. powers_of_2.jl
Generator system that computes powers of 2 using membrane computing.

### 3. fibonacci.jl
Generates the Fibonacci sequence using P-System rules.

### 4. communication.jl
Demonstrates object communication between nested membranes.

### 5. dissolution.jl
Shows membrane dissolution and object redistribution.

## Running Examples

```julia
using PSystems

# Include an example
include("examples/simple_rewriting.jl")

# Or run directly
julia examples/simple_rewriting.jl
```
