"""
Population-Based Training Algorithms

Implements various population-based training methods for multi-agent systems
including evolutionary algorithms, particle swarm optimization, and competitive coevolution.

This module supports Task 4.2.3 requirements for population-based training methods.
"""

import asyncio
import logging
import time
import uuid
import random
import math
from typing import Dict, List, Any, Optional, Tuple, Callable
from dataclasses import dataclass, field
from enum import Enum
from collections import defaultdict

logger = logging.getLogger(__name__)


class PopulationAlgorithm(Enum):
    """Types of population-based algorithms."""
    GENETIC_ALGORITHM = "genetic_algorithm"
    PARTICLE_SWARM = "particle_swarm"
    DIFFERENTIAL_EVOLUTION = "differential_evolution"
    COMPETITIVE_COEVOLUTION = "competitive_coevolution"
    COOPERATIVE_COEVOLUTION = "cooperative_coevolution"
    MULTI_OBJECTIVE_NSGA = "multi_objective_nsga"
    CULTURAL_EVOLUTION = "cultural_evolution"


@dataclass
class PopulationMember:
    """Member of an evolutionary population."""
    member_id: str
    parameters: Dict[str, Any]
    fitness_scores: Dict[str, float] = field(default_factory=dict)
    age: int = 0
    parents: List[str] = field(default_factory=list)
    offspring_count: int = 0
    social_interactions: int = 0
    cultural_knowledge: Dict[str, Any] = field(default_factory=dict)
    dominates: List[str] = field(default_factory=list)  # For NSGA-II
    dominated_by: int = 0  # For NSGA-II
    crowding_distance: float = 0.0  # For NSGA-II


@dataclass
class PopulationConfig:
    """Configuration for population-based algorithms."""
    algorithm_type: PopulationAlgorithm = PopulationAlgorithm.GENETIC_ALGORITHM
    population_size: int = 50
    max_generations: int = 100
    
    # Genetic Algorithm parameters
    mutation_rate: float = 0.1
    crossover_rate: float = 0.8
    selection_pressure: float = 2.0
    elite_percentage: float = 0.1
    
    # Particle Swarm parameters
    inertia_weight: float = 0.9
    cognitive_coefficient: float = 2.0
    social_coefficient: float = 2.0
    velocity_clamp: float = 1.0
    
    # Differential Evolution parameters
    scaling_factor: float = 0.5
    differential_probability: float = 0.9
    
    # Coevolution parameters
    species_count: int = 3
    migration_rate: float = 0.1
    cooperation_bonus: float = 0.2
    
    # Multi-objective parameters
    objective_weights: Dict[str, float] = field(default_factory=lambda: {"fitness": 1.0})
    pareto_tournament_size: int = 2
    
    # Cultural Evolution parameters
    cultural_learning_rate: float = 0.1
    knowledge_sharing_probability: float = 0.3


class PopulationBasedTrainer:
    """
    Main class for population-based training algorithms.
    Supports multiple algorithms and can be integrated with multi-agent training.
    """

    def __init__(self, config: PopulationConfig):
        self.config = config
        self.population: Dict[str, PopulationMember] = {}
        self.generation = 0
        self.best_member: Optional[PopulationMember] = None
        self.population_history: List[Dict[str, Any]] = []
        
        # Algorithm-specific data structures
        self.particle_velocities: Dict[str, Dict[str, float]] = {}  # For PSO
        self.global_best: Dict[str, Any] = {}  # For PSO
        self.species_populations: Dict[int, List[str]] = {}  # For coevolution
        self.pareto_fronts: List[List[str]] = []  # For NSGA-II
        self.cultural_knowledge_base: Dict[str, Any] = {}  # For cultural evolution
        
        logger.info(f"Initialized PopulationBasedTrainer with {config.algorithm_type}")

    def initialize_population(self, parameter_ranges: Dict[str, Tuple[float, float]]) -> None:
        """
        Initialize population with random parameters within specified ranges.
        
        Args:
            parameter_ranges: Dictionary mapping parameter names to (min, max) ranges
        """
        logger.info(f"Initializing population of {self.config.population_size} members")
        
        for i in range(self.config.population_size):
            member_id = f"pop_{uuid.uuid4().hex[:8]}"
            
            # Generate random parameters within ranges
            parameters = {}
            for param_name, (min_val, max_val) in parameter_ranges.items():
                parameters[param_name] = random.uniform(min_val, max_val)
            
            member = PopulationMember(
                member_id=member_id,
                parameters=parameters,
                fitness_scores={"primary": 0.0}
            )
            
            self.population[member_id] = member
            
            # Initialize algorithm-specific data
            if self.config.algorithm_type == PopulationAlgorithm.PARTICLE_SWARM:
                self.particle_velocities[member_id] = {
                    param: random.uniform(-0.1, 0.1) for param in parameters.keys()
                }
        
        # Initialize species for coevolution
        if "coevolution" in self.config.algorithm_type.value:
            self._initialize_species()
        
        logger.info(f"Population initialized with {len(self.population)} members")

    def _initialize_species(self):
        """Initialize species for coevolutionary algorithms."""
        population_ids = list(self.population.keys())
        species_size = len(population_ids) // self.config.species_count
        
        for i in range(self.config.species_count):
            start_idx = i * species_size
            end_idx = start_idx + species_size if i < self.config.species_count - 1 else len(population_ids)
            self.species_populations[i] = population_ids[start_idx:end_idx]

    async def evolve_generation(self, fitness_function: Callable) -> Dict[str, Any]:
        """
        Evolve the population for one generation using the configured algorithm.
        
        Args:
            fitness_function: Function to evaluate member fitness
            
        Returns:
            Generation statistics
        """
        start_time = time.time()
        
        # Evaluate current population
        await self._evaluate_population(fitness_function)
        
        # Apply the selected algorithm
        if self.config.algorithm_type == PopulationAlgorithm.GENETIC_ALGORITHM:
            offspring = await self._genetic_algorithm_step()
        elif self.config.algorithm_type == PopulationAlgorithm.PARTICLE_SWARM:
            offspring = await self._particle_swarm_step()
        elif self.config.algorithm_type == PopulationAlgorithm.DIFFERENTIAL_EVOLUTION:
            offspring = await self._differential_evolution_step()
        elif self.config.algorithm_type == PopulationAlgorithm.COMPETITIVE_COEVOLUTION:
            offspring = await self._competitive_coevolution_step(fitness_function)
        elif self.config.algorithm_type == PopulationAlgorithm.COOPERATIVE_COEVOLUTION:
            offspring = await self._cooperative_coevolution_step(fitness_function)
        elif self.config.algorithm_type == PopulationAlgorithm.MULTI_OBJECTIVE_NSGA:
            offspring = await self._nsga_ii_step()
        elif self.config.algorithm_type == PopulationAlgorithm.CULTURAL_EVOLUTION:
            offspring = await self._cultural_evolution_step()
        else:
            raise ValueError(f"Unknown algorithm: {self.config.algorithm_type}")
        
        # Update population with offspring
        self._update_population(offspring)
        
        # Calculate generation statistics
        stats = self._calculate_generation_stats()
        stats['evolution_time'] = time.time() - start_time
        
        self.generation += 1
        self.population_history.append(stats)
        
        logger.info(f"Generation {self.generation} completed: {stats}")
        return stats

    async def _evaluate_population(self, fitness_function: Callable):
        """Evaluate fitness for all population members."""
        tasks = []
        for member in self.population.values():
            tasks.append(self._evaluate_member(member, fitness_function))
        
        await asyncio.gather(*tasks)
        
        # Update best member
        best_fitness = -float('inf')
        for member in self.population.values():
            primary_fitness = member.fitness_scores.get("primary", 0.0)
            if primary_fitness > best_fitness:
                best_fitness = primary_fitness
                self.best_member = member

    async def _evaluate_member(self, member: PopulationMember, fitness_function: Callable):
        """Evaluate fitness for a single member."""
        try:
            if asyncio.iscoroutinefunction(fitness_function):
                fitness_scores = await fitness_function(member.parameters)
            else:
                fitness_scores = fitness_function(member.parameters)
            
            if isinstance(fitness_scores, dict):
                member.fitness_scores = fitness_scores
            else:
                member.fitness_scores = {"primary": float(fitness_scores)}
                
        except Exception as e:
            logger.warning(f"Failed to evaluate member {member.member_id}: {e}")
            member.fitness_scores = {"primary": -1.0}

    async def _genetic_algorithm_step(self) -> List[PopulationMember]:
        """Execute one step of genetic algorithm."""
        offspring = []
        
        # Sort population by fitness
        sorted_members = sorted(self.population.values(), 
                              key=lambda m: m.fitness_scores.get("primary", 0.0), 
                              reverse=True)
        
        # Keep elite members
        elite_count = max(1, int(len(sorted_members) * self.config.elite_percentage))
        offspring.extend(sorted_members[:elite_count])
        
        # Generate offspring through selection, crossover, and mutation
        while len(offspring) < self.config.population_size:
            # Tournament selection
            parent1 = self._tournament_selection(sorted_members)
            parent2 = self._tournament_selection(sorted_members)
            
            if random.random() < self.config.crossover_rate:
                child1, child2 = self._crossover(parent1, parent2)
                offspring.extend([child1, child2])
            else:
                offspring.extend([self._clone_member(parent1), self._clone_member(parent2)])
        
        # Apply mutation
        for i in range(elite_count, len(offspring)):
            if random.random() < self.config.mutation_rate:
                offspring[i] = self._mutate(offspring[i])
        
        return offspring[:self.config.population_size]

    async def _particle_swarm_step(self) -> List[PopulationMember]:
        """Execute one step of particle swarm optimization."""
        updated_particles = []
        
        for member in self.population.values():
            # Update velocity and position
            velocity = self.particle_velocities[member.member_id]
            
            for param_name in member.parameters.keys():
                # PSO velocity update equation
                inertia_term = self.config.inertia_weight * velocity[param_name]
                
                cognitive_term = (self.config.cognitive_coefficient * 
                                random.random() * 
                                (self.best_member.parameters.get(param_name, 0) - member.parameters[param_name]))
                
                social_term = (self.config.social_coefficient * 
                             random.random() * 
                             (self.global_best.get(param_name, 0) - member.parameters[param_name]))
                
                new_velocity = inertia_term + cognitive_term + social_term
                
                # Clamp velocity
                new_velocity = max(-self.config.velocity_clamp, 
                                 min(self.config.velocity_clamp, new_velocity))
                
                velocity[param_name] = new_velocity
                member.parameters[param_name] += new_velocity
            
            updated_particles.append(member)
        
        # Update global best
        if self.best_member:
            self.global_best = self.best_member.parameters.copy()
        
        return updated_particles

    async def _differential_evolution_step(self) -> List[PopulationMember]:
        """Execute one step of differential evolution."""
        offspring = []
        population_list = list(self.population.values())
        
        for i, target in enumerate(population_list):
            # Select three random members different from target
            candidates = [m for j, m in enumerate(population_list) if j != i]
            if len(candidates) >= 3:
                a, b, c = random.sample(candidates, 3)
                
                # Create mutant vector
                mutant_params = {}
                for param_name in target.parameters.keys():
                    mutant_value = (a.parameters[param_name] + 
                                  self.config.scaling_factor * 
                                  (b.parameters[param_name] - c.parameters[param_name]))
                    mutant_params[param_name] = mutant_value
                
                # Crossover
                trial_params = {}
                for param_name in target.parameters.keys():
                    if (random.random() < self.config.differential_probability or 
                        param_name == random.choice(list(target.parameters.keys()))):
                        trial_params[param_name] = mutant_params[param_name]
                    else:
                        trial_params[param_name] = target.parameters[param_name]
                
                # Create trial member
                trial_member = PopulationMember(
                    member_id=f"trial_{uuid.uuid4().hex[:8]}",
                    parameters=trial_params,
                    parents=[target.member_id]
                )
                
                offspring.append(trial_member)
            else:
                offspring.append(self._clone_member(target))
        
        return offspring

    async def _competitive_coevolution_step(self, fitness_function: Callable) -> List[PopulationMember]:
        """Execute competitive coevolution between species."""
        all_offspring = []
        
        # Evolve each species separately
        for species_id, member_ids in self.species_populations.items():
            species_members = [self.population[mid] for mid in member_ids]
            
            # Evaluate species through competition with other species
            for member in species_members:
                competitor_species = [s for s in range(self.config.species_count) if s != species_id]
                competition_scores = []
                
                for comp_species_id in competitor_species:
                    competitor_ids = self.species_populations[comp_species_id]
                    competitor = self.population[random.choice(competitor_ids)]
                    
                    # Simulate competition (this would be task-specific)
                    score = await self._simulate_competition(member, competitor)
                    competition_scores.append(score)
                
                member.fitness_scores["competitive"] = sum(competition_scores) / len(competition_scores)
            
            # Apply genetic algorithm within species
            species_config = PopulationConfig(
                population_size=len(species_members),
                mutation_rate=self.config.mutation_rate,
                crossover_rate=self.config.crossover_rate
            )
            
            species_ga = PopulationBasedTrainer(species_config)
            species_ga.population = {m.member_id: m for m in species_members}
            
            species_offspring = await species_ga._genetic_algorithm_step()
            all_offspring.extend(species_offspring)
        
        # Perform migration between species
        if random.random() < self.config.migration_rate:
            self._perform_migration(all_offspring)
        
        return all_offspring

    async def _cooperative_coevolution_step(self, fitness_function: Callable) -> List[PopulationMember]:
        """Execute cooperative coevolution between species."""
        all_offspring = []
        
        # Evaluate species through cooperation
        for species_id, member_ids in self.species_populations.items():
            species_members = [self.population[mid] for mid in member_ids]
            
            for member in species_members:
                # Form cooperative teams with members from other species
                team_partners = []
                for other_species_id in range(self.config.species_count):
                    if other_species_id != species_id:
                        partner_ids = self.species_populations[other_species_id]
                        partner = self.population[random.choice(partner_ids)]
                        team_partners.append(partner)
                
                # Evaluate cooperative performance
                cooperation_score = await self._simulate_cooperation(member, team_partners)
                
                # Apply cooperation bonus
                base_fitness = member.fitness_scores.get("primary", 0.0)
                member.fitness_scores["cooperative"] = base_fitness + (cooperation_score * self.config.cooperation_bonus)
            
            # Evolve species based on cooperative fitness
            species_config = PopulationConfig(population_size=len(species_members))
            species_ga = PopulationBasedTrainer(species_config)
            species_ga.population = {m.member_id: m for m in species_members}
            
            species_offspring = await species_ga._genetic_algorithm_step()
            all_offspring.extend(species_offspring)
        
        return all_offspring

    async def _nsga_ii_step(self) -> List[PopulationMember]:
        """Execute NSGA-II multi-objective optimization step."""
        # Calculate domination relationships
        self._calculate_domination()
        
        # Perform fast non-dominated sorting
        fronts = self._fast_non_dominated_sort()
        
        # Calculate crowding distances
        for front in fronts:
            self._calculate_crowding_distance(front)
        
        # Generate offspring through genetic operations
        offspring = []
        population_list = list(self.population.values())
        
        while len(offspring) < self.config.population_size:
            parent1 = self._nsga_tournament_selection(population_list)
            parent2 = self._nsga_tournament_selection(population_list)
            
            if random.random() < self.config.crossover_rate:
                child1, child2 = self._crossover(parent1, parent2)
                offspring.extend([child1, child2])
            else:
                offspring.extend([self._clone_member(parent1), self._clone_member(parent2)])
        
        return offspring[:self.config.population_size]

    async def _cultural_evolution_step(self) -> List[PopulationMember]:
        """Execute cultural evolution with knowledge sharing."""
        offspring = []
        
        # Update cultural knowledge base
        self._update_cultural_knowledge()
        
        # Apply cultural learning to population members
        for member in self.population.values():
            # Learn from cultural knowledge base
            if random.random() < self.config.knowledge_sharing_probability:
                member = self._apply_cultural_learning(member)
            
            # Social learning from high-performing individuals
            if random.random() < self.config.cultural_learning_rate:
                mentor = self._select_cultural_mentor()
                if mentor:
                    member = self._learn_from_mentor(member, mentor)
            
            offspring.append(member)
        
        # Apply standard genetic operations with cultural bias
        enhanced_offspring = await self._genetic_algorithm_step()
        
        return enhanced_offspring

    async def _simulate_competition(self, member1: PopulationMember, member2: PopulationMember) -> float:
        """
        Simulate competition between two members.
        In a real implementation, this would involve actual task performance.
        """
        # Simple competition simulation based on parameter differences
        score1 = sum(member1.parameters.values())
        score2 = sum(member2.parameters.values())
        
        # Add some randomness
        noise1 = random.gauss(0, 0.1)
        noise2 = random.gauss(0, 0.1)
        
        return 1.0 if (score1 + noise1) > (score2 + noise2) else 0.0

    async def _simulate_cooperation(self, member: PopulationMember, partners: List[PopulationMember]) -> float:
        """
        Simulate cooperation between members.
        In a real implementation, this would involve collaborative task performance.
        """
        # Simple cooperation simulation
        individual_contribution = sum(member.parameters.values()) / len(member.parameters)
        
        team_synergy = 0.0
        for partner in partners:
            partner_contribution = sum(partner.parameters.values()) / len(partner.parameters)
            synergy = min(individual_contribution, partner_contribution) * 0.1
            team_synergy += synergy
        
        return individual_contribution + (team_synergy / len(partners))

    def _tournament_selection(self, sorted_members: List[PopulationMember]) -> PopulationMember:
        """Perform tournament selection."""
        tournament_size = min(3, len(sorted_members))
        tournament = random.sample(sorted_members, tournament_size)
        return max(tournament, key=lambda m: m.fitness_scores.get("primary", 0.0))

    def _crossover(self, parent1: PopulationMember, parent2: PopulationMember) -> Tuple[PopulationMember, PopulationMember]:
        """Perform crossover between two parents."""
        child1_params = {}
        child2_params = {}
        
        for param_name in parent1.parameters.keys():
            if random.random() < 0.5:
                child1_params[param_name] = parent1.parameters[param_name]
                child2_params[param_name] = parent2.parameters[param_name]
            else:
                child1_params[param_name] = parent2.parameters[param_name]
                child2_params[param_name] = parent1.parameters[param_name]
        
        child1 = PopulationMember(
            member_id=f"child_{uuid.uuid4().hex[:8]}",
            parameters=child1_params,
            parents=[parent1.member_id, parent2.member_id]
        )
        
        child2 = PopulationMember(
            member_id=f"child_{uuid.uuid4().hex[:8]}",
            parameters=child2_params,
            parents=[parent1.member_id, parent2.member_id]
        )
        
        return child1, child2

    def _mutate(self, member: PopulationMember) -> PopulationMember:
        """Apply mutation to a member."""
        mutated_params = member.parameters.copy()
        
        for param_name, value in mutated_params.items():
            if random.random() < self.config.mutation_rate:
                # Gaussian mutation
                mutation_strength = abs(value) * 0.1 or 0.1
                mutated_params[param_name] = value + random.gauss(0, mutation_strength)
        
        return PopulationMember(
            member_id=f"mutated_{uuid.uuid4().hex[:8]}",
            parameters=mutated_params,
            parents=[member.member_id]
        )

    def _clone_member(self, member: PopulationMember) -> PopulationMember:
        """Create a clone of a member."""
        return PopulationMember(
            member_id=f"clone_{uuid.uuid4().hex[:8]}",
            parameters=member.parameters.copy(),
            parents=[member.member_id]
        )

    def _calculate_domination(self):
        """Calculate domination relationships for NSGA-II."""
        population_list = list(self.population.values())
        
        for member in population_list:
            member.dominates = []
            member.dominated_by = 0
            
            for other in population_list:
                if member.member_id != other.member_id:
                    if self._dominates(member, other):
                        member.dominates.append(other.member_id)
                    elif self._dominates(other, member):
                        member.dominated_by += 1

    def _dominates(self, member1: PopulationMember, member2: PopulationMember) -> bool:
        """Check if member1 dominates member2 in multi-objective space."""
        better_in_at_least_one = False
        
        for objective in self.config.objective_weights.keys():
            score1 = member1.fitness_scores.get(objective, 0.0)
            score2 = member2.fitness_scores.get(objective, 0.0)
            
            if score1 < score2:  # Worse in this objective
                return False
            elif score1 > score2:  # Better in this objective
                better_in_at_least_one = True
        
        return better_in_at_least_one

    def _fast_non_dominated_sort(self) -> List[List[str]]:
        """Perform fast non-dominated sorting for NSGA-II."""
        fronts = []
        current_front = []
        
        # Find first front (non-dominated solutions)
        for member in self.population.values():
            if member.dominated_by == 0:
                current_front.append(member.member_id)
        
        fronts.append(current_front)
        
        # Find subsequent fronts
        while current_front:
            next_front = []
            for member_id in current_front:
                member = self.population[member_id]
                for dominated_id in member.dominates:
                    dominated = self.population[dominated_id]
                    dominated.dominated_by -= 1
                    if dominated.dominated_by == 0:
                        next_front.append(dominated_id)
            
            if next_front:
                fronts.append(next_front)
            current_front = next_front
        
        self.pareto_fronts = fronts
        return fronts

    def _calculate_crowding_distance(self, front: List[str]):
        """Calculate crowding distance for members in a front."""
        if len(front) <= 2:
            for member_id in front:
                self.population[member_id].crowding_distance = float('inf')
            return
        
        # Initialize distances
        for member_id in front:
            self.population[member_id].crowding_distance = 0.0
        
        # Calculate distance for each objective
        for objective in self.config.objective_weights.keys():
            # Sort front by objective value
            front_sorted = sorted(front, 
                                key=lambda mid: self.population[mid].fitness_scores.get(objective, 0.0))
            
            # Set boundary points to infinite distance
            self.population[front_sorted[0]].crowding_distance = float('inf')
            self.population[front_sorted[-1]].crowding_distance = float('inf')
            
            # Calculate objective range
            min_obj = self.population[front_sorted[0]].fitness_scores.get(objective, 0.0)
            max_obj = self.population[front_sorted[-1]].fitness_scores.get(objective, 0.0)
            obj_range = max_obj - min_obj
            
            if obj_range == 0:
                continue
            
            # Calculate crowding distances
            for i in range(1, len(front_sorted) - 1):
                prev_obj = self.population[front_sorted[i-1]].fitness_scores.get(objective, 0.0)
                next_obj = self.population[front_sorted[i+1]].fitness_scores.get(objective, 0.0)
                
                distance = (next_obj - prev_obj) / obj_range
                self.population[front_sorted[i]].crowding_distance += distance

    def _nsga_tournament_selection(self, population_list: List[PopulationMember]) -> PopulationMember:
        """NSGA-II tournament selection based on rank and crowding distance."""
        tournament = random.sample(population_list, self.config.pareto_tournament_size)
        
        # Find the front rank for each tournament member
        member_ranks = {}
        for i, front in enumerate(self.pareto_fronts):
            for member_id in front:
                member_ranks[member_id] = i
        
        # Select based on rank and crowding distance
        best_member = tournament[0]
        best_rank = member_ranks.get(best_member.member_id, float('inf'))
        
        for member in tournament[1:]:
            member_rank = member_ranks.get(member.member_id, float('inf'))
            
            if (member_rank < best_rank or 
                (member_rank == best_rank and member.crowding_distance > best_member.crowding_distance)):
                best_member = member
                best_rank = member_rank
        
        return best_member

    def _update_population(self, offspring: List[PopulationMember]):
        """Update population with offspring."""
        # Create new population dictionary
        new_population = {}
        for member in offspring:
            new_population[member.member_id] = member
            member.age += 1
        
        self.population = new_population

    def _calculate_generation_stats(self) -> Dict[str, Any]:
        """Calculate statistics for the current generation."""
        if not self.population:
            return {}
        
        fitness_values = [m.fitness_scores.get("primary", 0.0) for m in self.population.values()]
        
        stats = {
            'generation': self.generation,
            'population_size': len(self.population),
            'best_fitness': max(fitness_values) if fitness_values else 0.0,
            'average_fitness': sum(fitness_values) / len(fitness_values) if fitness_values else 0.0,
            'worst_fitness': min(fitness_values) if fitness_values else 0.0,
            'fitness_std': math.sqrt(sum((x - sum(fitness_values) / len(fitness_values))**2 
                                       for x in fitness_values) / len(fitness_values)) if len(fitness_values) > 1 else 0.0,
            'algorithm': self.config.algorithm_type.value
        }
        
        if self.best_member:
            stats['best_parameters'] = self.best_member.parameters.copy()
        
        return stats

    def _perform_migration(self, offspring: List[PopulationMember]):
        """Perform migration between species."""
        migration_count = max(1, int(len(offspring) * self.config.migration_rate))
        
        for _ in range(migration_count):
            # Select random members from different species
            source_species = random.randint(0, self.config.species_count - 1)
            target_species = random.randint(0, self.config.species_count - 1)
            
            if source_species != target_species:
                source_members = [m for m in offspring 
                                if m.member_id in self.species_populations.get(source_species, [])]
                target_members = [m for m in offspring 
                                if m.member_id in self.species_populations.get(target_species, [])]
                
                if source_members and target_members:
                    migrant = random.choice(source_members)
                    # Move migrant to target species
                    self.species_populations[source_species].remove(migrant.member_id)
                    self.species_populations[target_species].append(migrant.member_id)

    def _update_cultural_knowledge(self):
        """Update the cultural knowledge base with high-performing strategies."""
        if not self.population:
            return
        
        # Extract knowledge from top performers
        sorted_members = sorted(self.population.values(), 
                              key=lambda m: m.fitness_scores.get("primary", 0.0), 
                              reverse=True)
        
        top_performers = sorted_members[:max(1, len(sorted_members) // 4)]
        
        # Analyze successful parameter patterns
        param_averages = defaultdict(list)
        for member in top_performers:
            for param_name, value in member.parameters.items():
                param_averages[param_name].append(value)
        
        # Update cultural knowledge
        for param_name, values in param_averages.items():
            self.cultural_knowledge_base[param_name] = {
                'mean': sum(values) / len(values),
                'std': math.sqrt(sum((x - sum(values) / len(values))**2 for x in values) / len(values)),
                'min': min(values),
                'max': max(values)
            }

    def _apply_cultural_learning(self, member: PopulationMember) -> PopulationMember:
        """Apply cultural learning to a population member."""
        learned_params = member.parameters.copy()
        
        for param_name in learned_params.keys():
            if param_name in self.cultural_knowledge_base:
                knowledge = self.cultural_knowledge_base[param_name]
                
                # Blend current value with cultural knowledge
                cultural_value = knowledge['mean']
                learning_rate = self.config.cultural_learning_rate
                
                learned_params[param_name] = (
                    (1 - learning_rate) * learned_params[param_name] + 
                    learning_rate * cultural_value
                )
        
        return PopulationMember(
            member_id=f"cultural_{uuid.uuid4().hex[:8]}",
            parameters=learned_params,
            parents=[member.member_id],
            cultural_knowledge=self.cultural_knowledge_base.copy()
        )

    def _select_cultural_mentor(self) -> Optional[PopulationMember]:
        """Select a high-performing member as a mentor."""
        if not self.population:
            return None
        
        sorted_members = sorted(self.population.values(), 
                              key=lambda m: m.fitness_scores.get("primary", 0.0), 
                              reverse=True)
        
        # Select from top 20%
        top_count = max(1, len(sorted_members) // 5)
        return random.choice(sorted_members[:top_count])

    def _learn_from_mentor(self, student: PopulationMember, mentor: PopulationMember) -> PopulationMember:
        """Apply social learning from a mentor."""
        learned_params = student.parameters.copy()
        
        # Learn selectively from mentor's successful strategies
        for param_name in learned_params.keys():
            if random.random() < 0.5:  # 50% chance to learn each parameter
                mentor_value = mentor.parameters.get(param_name, learned_params[param_name])
                learning_strength = random.uniform(0.1, 0.3)
                
                learned_params[param_name] = (
                    (1 - learning_strength) * learned_params[param_name] + 
                    learning_strength * mentor_value
                )
        
        learned_member = PopulationMember(
            member_id=f"mentored_{uuid.uuid4().hex[:8]}",
            parameters=learned_params,
            parents=[student.member_id, mentor.member_id]
        )
        
        learned_member.social_interactions = student.social_interactions + 1
        
        return learned_member

    def get_population_statistics(self) -> Dict[str, Any]:
        """Get comprehensive population statistics."""
        if not self.population:
            return {}
        
        current_stats = self._calculate_generation_stats()
        
        return {
            'current_generation': self.generation,
            'algorithm_type': self.config.algorithm_type.value,
            'population_status': current_stats,
            'best_ever': {
                'fitness': self.best_member.fitness_scores if self.best_member else {},
                'parameters': self.best_member.parameters if self.best_member else {}
            },
            'diversity_metrics': self._calculate_population_diversity(),
            'convergence_metrics': self._calculate_convergence_metrics(),
            'generation_history': self.population_history
        }

    def _calculate_population_diversity(self) -> Dict[str, float]:
        """Calculate population diversity metrics."""
        if not self.population or len(self.population) < 2:
            return {'genetic_diversity': 0.0}
        
        # Calculate genetic diversity as average pairwise distance
        members = list(self.population.values())
        total_distance = 0.0
        pair_count = 0
        
        for i in range(len(members)):
            for j in range(i + 1, len(members)):
                distance = self._calculate_genetic_distance(members[i], members[j])
                total_distance += distance
                pair_count += 1
        
        genetic_diversity = total_distance / pair_count if pair_count > 0 else 0.0
        
        return {
            'genetic_diversity': genetic_diversity,
            'effective_population_size': self._calculate_effective_population_size()
        }

    def _calculate_genetic_distance(self, member1: PopulationMember, member2: PopulationMember) -> float:
        """Calculate genetic distance between two members."""
        distance = 0.0
        param_count = 0
        
        for param_name in member1.parameters.keys():
            if param_name in member2.parameters:
                diff = abs(member1.parameters[param_name] - member2.parameters[param_name])
                distance += diff * diff
                param_count += 1
        
        return math.sqrt(distance / param_count) if param_count > 0 else 0.0

    def _calculate_effective_population_size(self) -> float:
        """Calculate effective population size based on fitness variance."""
        fitness_values = [m.fitness_scores.get("primary", 0.0) for m in self.population.values()]
        
        if len(fitness_values) < 2:
            return float(len(fitness_values))
        
        mean_fitness = sum(fitness_values) / len(fitness_values)
        fitness_variance = sum((f - mean_fitness)**2 for f in fitness_values) / len(fitness_values)
        
        if fitness_variance == 0:
            return float(len(fitness_values))
        
        # Effective population size approximation
        return mean_fitness * mean_fitness / fitness_variance

    def _calculate_convergence_metrics(self) -> Dict[str, float]:
        """Calculate convergence metrics."""
        if len(self.population_history) < 2:
            return {'convergence_rate': 0.0}
        
        recent_generations = self.population_history[-5:]
        best_fitnesses = [g['best_fitness'] for g in recent_generations]
        
        if len(best_fitnesses) < 2:
            return {'convergence_rate': 0.0}
        
        # Calculate improvement rate
        fitness_improvements = []
        for i in range(1, len(best_fitnesses)):
            improvement = best_fitnesses[i] - best_fitnesses[i-1]
            fitness_improvements.append(improvement)
        
        convergence_rate = sum(fitness_improvements) / len(fitness_improvements)
        
        return {
            'convergence_rate': convergence_rate,
            'stagnation_generations': self._count_stagnation_generations()
        }

    def _count_stagnation_generations(self) -> int:
        """Count consecutive generations without improvement."""
        if not self.population_history:
            return 0
        
        stagnation_count = 0
        current_best = self.population_history[-1]['best_fitness']
        
        for i in range(len(self.population_history) - 2, -1, -1):
            if self.population_history[i]['best_fitness'] >= current_best - 1e-6:
                stagnation_count += 1
            else:
                break
        
        return stagnation_count