#!/usr/bin/env julia
"""
Script to install and integrate SciML packages from the monorepo.

This script:
1. Adds the local monorepo packages to the main project
2. Ensures all dependencies are properly installed
3. Verifies the packages can be loaded
"""

using Pkg

println("""
╔════════════════════════════════════════════════════════════════╗
║  SciML Package Integration Script                              ║
║  Installing RootedTrees.jl, BSeries.jl, ReservoirComputing.jl ║
╚════════════════════════════════════════════════════════════════╝
""")

# Get the project root
project_root = dirname(@__DIR__)

# Package paths in the monorepo
packages = [
    ("RootedTrees", joinpath(project_root, "RootedTrees.jl")),
    ("BSeries", joinpath(project_root, "BSeries.jl")),
    ("ReservoirComputing", joinpath(project_root, "ReservoirComputing.jl"))
]

# Activate the main project
Pkg.activate(project_root)

println("\n📦 Installing SciML packages from monorepo...\n")

for (name, path) in packages
    println("  → Adding $name from $path")
    try
        # Add the package from the local path
        Pkg.develop(path=path)
        println("    ✓ $name added successfully")
    catch e
        println("    ✗ Failed to add $name: $e")
    end
end

println("\n📦 Installing dependencies...\n")
try
    Pkg.instantiate()
    println("    ✓ Dependencies installed")
catch e
    println("    ✗ Failed to install dependencies: $e")
end

println("\n🧪 Testing package loading...\n")

# Test loading each package
for (name, _) in packages
    println("  → Loading $name...")
    try
        if name == "RootedTrees"
            @eval using RootedTrees
        elseif name == "BSeries"
            @eval using BSeries
        elseif name == "ReservoirComputing"
            @eval using ReservoirComputing
        end
        println("    ✓ $name loaded successfully")
    catch e
        println("    ✗ Failed to load $name: $e")
    end
end

println("""

╔════════════════════════════════════════════════════════════════╗
║  ✓ SciML Package Integration Complete                         ║
╚════════════════════════════════════════════════════════════════╝

The following packages are now available:
  • RootedTrees.jl - Rooted tree operations
  • BSeries.jl - B-series methods
  • ReservoirComputing.jl - Echo state networks

Next steps:
  1. Run DeepTreeEcho tests: julia --project=. test/test_deep_tree_echo.jl
  2. Try the examples: julia --project=. examples/kernel_evolution_demo.jl
  3. Check integration: julia --project=. -e 'using RootedTrees, BSeries, ReservoirComputing'
""")
