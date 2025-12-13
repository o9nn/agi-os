#!/usr/bin/env julia

using DifferentialEquations, Test, SafeTestsets
@time begin
    # All default algorithm tests have been moved to their respective packages
    # This package now only re-exports the solver packages
    @test true
end
