"""
B-Series genome representation and genetic operations.

This module implements B-series coefficients as genetic code for temporal
integration, with evolutionary operators for optimization.
"""

using UUIDs

"""
    BSeriesGenome

Genetic representation of a B-series method.

The B-series coefficients serve as the "DNA" of the numerical integrator,
encoding how elementary differentials (rooted trees) are combined.

# Fields
- `id::String`: Unique identifier
- `coefficients::OrderedDict{RootedTree, Float64}`: B-series coefficients
- `fitness::Float64`: Fitness score
- `generation::Int`: Generation number
- `parents::Vector{String}`: Parent IDs
- `order::Int`: Order of the method
"""
mutable struct BSeriesGenome
    id::String
    coefficients::OrderedDict{RootedTree, Float64}
    fitness::Float64
    generation::Int
    parents::Vector{String}
    order::Int
    
    function BSeriesGenome(coefficients::OrderedDict{RootedTree, Float64},
                          generation::Int = 0,
                          parents::Vector{String} = String[])
        id = string(uuid4())
        order_val = isempty(coefficients) ? 0 : maximum(RootedTrees.order(t) for t in keys(coefficients))
        new(id, coefficients, 0.0, generation, parents, order_val)
    end
end

"""
    BSeriesPopulation

Population of B-series genomes for evolutionary optimization.

# Fields
- `individuals::Vector{BSeriesGenome}`: Population members
- `generation::Int`: Current generation number
- `elite_size::Int`: Number of elite individuals to preserve
- `mutation_rate::Float64`: Probability of mutation
- `crossover_rate::Float64`: Probability of crossover
"""
mutable struct BSeriesPopulation
    individuals::Vector{BSeriesGenome}
    generation::Int
    elite_size::Int
    mutation_rate::Float64
    crossover_rate::Float64
    
    function BSeriesPopulation(individuals::Vector{BSeriesGenome},
                              elite_size::Int = 2,
                              mutation_rate::Float64 = 0.1,
                              crossover_rate::Float64 = 0.7)
        new(individuals, 0, elite_size, mutation_rate, crossover_rate)
    end
end

"""
    initialize_bseries_genome(order::Int) -> BSeriesGenome

Initialize a B-series genome with random coefficients up to given order.

# Arguments
- `order::Int`: Maximum order of rooted trees to include

# Returns
- `BSeriesGenome`: Initialized genome

# Examples
```julia
genome = initialize_bseries_genome(4)
```
"""
function initialize_bseries_genome(order::Int)
    coefficients = OrderedDict{RootedTree, Float64}()
    
    # Generate all trees up to the given order
    for o in 1:order
        trees = generate_all_trees(o)
        for t in trees
            # Initialize with random coefficients near theoretical values
            theoretical = 1.0 / (RootedTrees.order(t) * RootedTrees.symmetry(t))
            noise = 0.1 * randn()
            coefficients[t] = theoretical + noise
        end
    end
    
    return BSeriesGenome(coefficients)
end

"""
    initialize_population(order::Int, population_size::Int) -> BSeriesPopulation

Initialize a population of B-series genomes.

# Arguments
- `order::Int`: Maximum order for B-series
- `population_size::Int`: Number of individuals in population

# Returns
- `BSeriesPopulation`: Initialized population

# Examples
```julia
population = initialize_population(4, 20)
```
"""
function initialize_population(order::Int, population_size::Int)
    individuals = [initialize_bseries_genome(order) for _ in 1:population_size]
    return BSeriesPopulation(individuals)
end

"""
    crossover(parent1::BSeriesGenome, parent2::BSeriesGenome) -> BSeriesGenome

Perform genetic crossover between two B-series genomes.

Uses single-point crossover on the coefficient arrays.

# Arguments
- `parent1::BSeriesGenome`: First parent
- `parent2::BSeriesGenome`: Second parent

# Returns
- `BSeriesGenome`: Offspring genome

# Examples
```julia
child = crossover(parent1, parent2)
```
"""
function crossover(parent1::BSeriesGenome, parent2::BSeriesGenome)
    # Ensure both parents have the same trees
    trees1 = collect(keys(parent1.coefficients))
    trees2 = collect(keys(parent2.coefficients))
    
    # Use intersection of trees
    common_trees = intersect(trees1, trees2)
    
    if isempty(common_trees)
        # If no common trees, clone parent1
        return BSeriesGenome(
            copy(parent1.coefficients),
            max(parent1.generation, parent2.generation) + 1,
            [parent1.id, parent2.id]
        )
    end
    
    # Single-point crossover
    crossover_point = rand(1:length(common_trees))
    
    offspring_coeffs = OrderedDict{RootedTree, Float64}()
    
    for (i, t) in enumerate(common_trees)
        if i <= crossover_point
            offspring_coeffs[t] = parent1.coefficients[t]
        else
            offspring_coeffs[t] = parent2.coefficients[t]
        end
    end
    
    return BSeriesGenome(
        offspring_coeffs,
        max(parent1.generation, parent2.generation) + 1,
        [parent1.id, parent2.id]
    )
end

"""
    mutate!(genome::BSeriesGenome, mutation_rate::Float64)

Mutate a B-series genome in place.

Each coefficient has probability `mutation_rate` of being perturbed by
Gaussian noise.

# Arguments
- `genome::BSeriesGenome`: Genome to mutate
- `mutation_rate::Float64`: Probability of mutation per coefficient

# Examples
```julia
mutate!(genome, 0.1)
```
"""
function mutate!(genome::BSeriesGenome, mutation_rate::Float64)
    for (tree, coeff) in genome.coefficients
        if rand() < mutation_rate
            # Add Gaussian noise (±10% of current value)
            noise = 0.1 * coeff * randn()
            genome.coefficients[tree] = coeff + noise
        end
    end
    
    return genome
end

"""
    clone_genome(genome::BSeriesGenome) -> BSeriesGenome

Create a deep copy of a B-series genome.

# Arguments
- `genome::BSeriesGenome`: Genome to clone

# Returns
- `BSeriesGenome`: Cloned genome

# Examples
```julia
clone = clone_genome(original)
```
"""
function clone_genome(genome::BSeriesGenome)
    return BSeriesGenome(
        copy(genome.coefficients),
        genome.generation,
        copy(genome.parents)
    )
end

"""
    genome_distance(g1::BSeriesGenome, g2::BSeriesGenome) -> Float64

Compute genetic distance between two B-series genomes.

Distance is measured as the Euclidean distance between coefficient vectors.

# Arguments
- `g1::BSeriesGenome`: First genome
- `g2::BSeriesGenome`: Second genome

# Returns
- `Float64`: Genetic distance

# Examples
```julia
d = genome_distance(genome1, genome2)
```
"""
function genome_distance(g1::BSeriesGenome, g2::BSeriesGenome)
    common_trees = intersect(keys(g1.coefficients), keys(g2.coefficients))
    
    if isempty(common_trees)
        return Inf
    end
    
    distance_sq = 0.0
    for t in common_trees
        diff = g1.coefficients[t] - g2.coefficients[t]
        distance_sq += diff^2
    end
    
    return sqrt(distance_sq)
end

"""
    population_diversity(population::BSeriesPopulation) -> Float64

Compute genetic diversity of a population.

Diversity is the average pairwise genetic distance.

# Arguments
- `population::BSeriesPopulation`: Population to analyze

# Returns
- `Float64`: Diversity score

# Examples
```julia
div = population_diversity(population)
```
"""
function population_diversity(population::BSeriesPopulation)
    n = length(population.individuals)
    
    if n <= 1
        return 0.0
    end
    
    total_distance = 0.0
    count = 0
    
    for i in 1:n
        for j in (i+1):n
            total_distance += genome_distance(
                population.individuals[i],
                population.individuals[j]
            )
            count += 1
        end
    end
    
    return total_distance / count
end

"""
    to_bseries(genome::BSeriesGenome) -> TruncatedBSeries

Convert a B-series genome to a TruncatedBSeries object.

# Arguments
- `genome::BSeriesGenome`: Genome to convert

# Returns
- `TruncatedBSeries`: B-series representation

# Examples
```julia
series = to_bseries(genome)
```
"""
function to_bseries(genome::BSeriesGenome)
    return TruncatedBSeries(genome.coefficients)
end

"""
    from_bseries(series::TruncatedBSeries) -> BSeriesGenome

Create a B-series genome from a TruncatedBSeries object.

# Arguments
- `series::TruncatedBSeries`: B-series to convert

# Returns
- `BSeriesGenome`: Genome representation

# Examples
```julia
genome = from_bseries(series)
```
"""
function from_bseries(series::TruncatedBSeries)
    coefficients = OrderedDict{RootedTree, Float64}()
    
    for (tree, coeff) in series
        coefficients[tree] = Float64(coeff)
    end
    
    return BSeriesGenome(coefficients)
end
