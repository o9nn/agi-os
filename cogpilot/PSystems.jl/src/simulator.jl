"""
    SimulationResult

Result of a P-System computation.

# Fields
- `final_config::Configuration`: Final configuration
- `trace::Vector{Configuration}`: Computation trace (if enabled)
- `steps::Int`: Number of steps executed
- `halted::Bool`: Whether computation halted naturally
"""
struct SimulationResult
    final_config::Configuration
    trace::Vector{Configuration}
    steps::Int
    halted::Bool
end

"""
    applicable_rules(system::PSystem, config::Configuration, membrane_id::Int)

Find all rules applicable to a membrane in the current configuration.
"""
function applicable_rules(system::PSystem, config::Configuration, membrane_id::Int)
    if !is_active(config, membrane_id)
        return Rule[]
    end
    
    membrane = get_membrane(system, membrane_id)
    if membrane === nothing
        return Rule[]
    end
    
    multiset = get_multiset(config, membrane_id)
    rules = get_rules_for_membrane(system, membrane.label)
    
    return filter(r -> is_applicable(r, multiset), rules)
end

"""
    select_rules(applicable::Vector{Rule}, multiset::Multiset; strategy=:maximal)

Select which rules to apply from the set of applicable rules.

# Strategies
- `:maximal`: Apply maximal parallel multiset (as many rules as possible)
- `:random`: Randomly select applicable rules
- `:first`: Apply first applicable rule
"""
function select_rules(applicable::Vector{Rule}, multiset::Multiset; strategy=:maximal)
    if isempty(applicable)
        return Rule[]
    end
    
    if strategy == :first
        return [first(applicable)]
    elseif strategy == :random
        return [rand(applicable)]
    elseif strategy == :maximal
        return maximal_parallel_selection(applicable, multiset)
    else
        throw(ArgumentError("Unknown selection strategy: $strategy"))
    end
end

"""
    maximal_parallel_selection(rules::Vector{Rule}, multiset::Multiset)

Select a maximal parallel multiset of rules.
This implements the non-deterministic maximal parallelism semantics.
"""
function maximal_parallel_selection(rules::Vector{Rule}, multiset::Multiset)
    # Sort by priority (higher first)
    sorted_rules = sort(rules, by=r -> r.priority, rev=true)
    
    selected = Rule[]
    remaining = copy(multiset)
    
    # Greedy selection respecting priorities
    for rule in sorted_rules
        # Try to apply this rule as many times as possible
        while is_applicable(rule, remaining)
            result = remaining - rule.lhs
            if result !== nothing
                push!(selected, rule)
                remaining = result
            else
                break
            end
        end
    end
    
    return selected
end

"""
    step!(system::PSystem, config::Configuration; strategy=:maximal)

Execute one computation step, modifying the configuration in-place.
Returns `true` if computation can continue, `false` if halted.
"""
function step!(system::PSystem, config::Configuration; strategy=:maximal)
    if is_halted(config, system)
        return false
    end
    
    # New multisets for next configuration
    next_multisets = Dict{Int,Multiset}()
    for (id, ms) in config.multisets
        next_multisets[id] = copy(ms)
    end
    
    membranes_to_dissolve = Int[]
    
    # Process each active membrane
    for membrane in system.membranes
        if !is_active(config, membrane.id)
            continue
        end
        
        # Find applicable rules
        applicable = applicable_rules(system, config, membrane.id)
        if isempty(applicable)
            continue
        end
        
        # Select rules to apply
        selected = select_rules(applicable, get_multiset(config, membrane.id), strategy=strategy)
        
        # Apply selected rules
        current_multiset = copy(get_multiset(config, membrane.id))
        
        for rule in selected
            # Consume left-hand side
            result = current_multiset - rule.lhs
            if result === nothing
                continue  # Should not happen if selection is correct
            end
            current_multiset = result
            
            # Produce right-hand side
            if rule.target == :here
                # Objects stay in same membrane
                current_multiset = current_multiset + rule.rhs
            elseif rule.target == :out
                # Objects go to parent membrane
                if membrane.parent !== nothing && is_active(config, membrane.parent)
                    parent_ms = get(next_multisets, membrane.parent, Multiset())
                    next_multisets[membrane.parent] = parent_ms + rule.rhs
                end
                # If no parent (skin), objects are lost to environment
            elseif rule.target isa Tuple && rule.target[1] == :in
                # Objects go to child membrane
                child_id = rule.target[2]
                # Find actual child membrane ID (may need to look up by label)
                if is_active(config, child_id)
                    child_ms = get(next_multisets, child_id, Multiset())
                    next_multisets[child_id] = child_ms + rule.rhs
                end
            end
            
            # Check for dissolution
            if rule.dissolve
                push!(membranes_to_dissolve, membrane.id)
                
                # Send all remaining objects to parent
                if membrane.parent !== nothing && is_active(config, membrane.parent)
                    parent_ms = get(next_multisets, membrane.parent, Multiset())
                    next_multisets[membrane.parent] = parent_ms + current_multiset
                end
                
                current_multiset = Multiset()  # Empty the membrane
                break  # Stop processing rules for this membrane
            end
        end
        
        # Update membrane multiset
        next_multisets[membrane.id] = current_multiset
    end
    
    # Apply dissolution
    for membrane_id in membranes_to_dissolve
        dissolve!(config, membrane_id)
    end
    
    # Update configuration
    config.multisets = next_multisets
    config.step += 1
    
    return true
end

"""
    simulate(system::PSystem; max_steps=100, trace=false, strategy=:maximal, halt_condition=nothing)

Simulate a P-System computation.

# Arguments
- `system::PSystem`: The P-System to simulate
- `max_steps::Int=100`: Maximum number of steps to execute
- `trace::Bool=false`: Whether to record the computation trace
- `strategy::Symbol=:maximal`: Rule selection strategy
- `halt_condition::Union{Function, Nothing}=nothing`: Custom halting condition

# Returns
- `SimulationResult`: Result containing final configuration and optional trace

# Examples

```julia
# Basic simulation
result = simulate(system)

# With trace
result = simulate(system, trace=true)

# Custom halting
halt_fn(config) = all(m -> isempty(config.multisets[m]), config.active_membranes)
result = simulate(system, halt_condition=halt_fn)
```
"""
function simulate(
    system::PSystem;
    max_steps::Int = 100,
    trace::Bool = false,
    strategy::Symbol = :maximal,
    halt_condition::Union{Function,Nothing} = nothing
)
    # Validate system
    validate(system)
    
    # Initialize configuration
    config = Configuration(system)
    
    # Trace storage
    trace_vec = Configuration[]
    if trace
        push!(trace_vec, copy(config))
    end
    
    # Main simulation loop
    steps = 0
    halted = false
    
    while steps < max_steps
        # Check custom halt condition
        if halt_condition !== nothing && halt_condition(config)
            halted = true
            break
        end
        
        # Execute step
        can_continue = step!(system, config, strategy=strategy)
        steps += 1
        
        # Record trace
        if trace
            push!(trace_vec, copy(config))
        end
        
        # Check if halted
        if !can_continue
            halted = true
            break
        end
    end
    
    return SimulationResult(config, trace_vec, steps, halted)
end
