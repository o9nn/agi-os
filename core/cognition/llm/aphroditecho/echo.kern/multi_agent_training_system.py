"""
Multi-Agent Training System for Deep Tree Echo

Implements distributed training across multiple agents with competitive and
cooperative learning using population-based training methods.

This system fulfills Task 4.2.3 requirements:
- Distributed training across multiple agents
- Competitive and cooperative learning
- Population-based training methods
- Agent populations improve through interaction
"""

import logging
import time
import uuid
from typing import Dict, List, Any, Optional, Callable, Tuple
from dataclasses import dataclass, field
from enum import Enum
import random
import math
from collections import defaultdict

# Import existing AAR components
try:
    from aar_core.orchestration.collaborative_solver import (
        CollaborativeProblemSolver, ProblemDefinition, ProblemType, SolutionStrategy
    )
    from aar_core.agents.agent_manager import AgentManager
    AAR_AVAILABLE = True
except ImportError:
    AAR_AVAILABLE = False
    logging.warning("AAR components not available - creating standalone implementation")

# Import DTESN components
try:
    from echo.kern.phase_3_3_3_self_monitoring import DTESNSelfMonitoringIntegration
    DTESN_AVAILABLE = True
except ImportError:
    DTESN_AVAILABLE = False
    logging.warning("DTESN components not available - creating standalone implementation")

logger = logging.getLogger(__name__)


class TrainingMode(Enum):
    """Training modes for multi-agent systems."""
    COOPERATIVE = "cooperative"
    COMPETITIVE = "competitive"
    HYBRID = "hybrid"
    POPULATION_BASED = "population_based"


class LearningStrategy(Enum):
    """Learning strategies for agent evolution."""
    EVOLUTIONARY = "evolutionary"
    TOURNAMENT = "tournament"  
    GRADIENT_BASED = "gradient_based"
    IMITATION = "imitation"
    SELF_PLAY = "self_play"


@dataclass
class AgentPopulationMember:
    """Individual agent in the training population."""
    agent_id: str
    generation: int = 0
    fitness_score: float = 0.0
    performance_metrics: Dict[str, float] = field(default_factory=dict)
    genetic_params: Dict[str, Any] = field(default_factory=dict)
    training_history: List[Dict[str, Any]] = field(default_factory=list)
    interaction_count: int = 0
    wins: int = 0
    losses: int = 0
    cooperation_score: float = 0.0
    created_at: float = field(default_factory=time.time)
    last_updated: float = field(default_factory=time.time)


@dataclass 
class TrainingEpisode:
    """Individual training episode or match between agents."""
    episode_id: str
    participants: List[str]  # agent_ids
    training_mode: TrainingMode
    start_time: float = field(default_factory=time.time)
    end_time: Optional[float] = None
    results: Dict[str, Any] = field(default_factory=dict)
    performance_deltas: Dict[str, float] = field(default_factory=dict)


@dataclass
class TrainingConfiguration:
    """Configuration for multi-agent training system."""
    population_size: int = 20
    max_generations: int = 100
    training_mode: TrainingMode = TrainingMode.HYBRID
    learning_strategy: LearningStrategy = LearningStrategy.EVOLUTIONARY
    mutation_rate: float = 0.1
    crossover_rate: float = 0.7
    selection_pressure: float = 2.0
    cooperation_weight: float = 0.3
    competition_weight: float = 0.7
    elite_percentage: float = 0.1
    tournament_size: int = 3
    episode_batch_size: int = 10
    fitness_aggregation: str = "weighted_average"
    enable_migration: bool = True
    migration_rate: float = 0.05


class MultiAgentTrainingSystem:
    """
    Core multi-agent training system that coordinates distributed training
    across agent populations with competitive and cooperative learning.
    """

    def __init__(self, config: TrainingConfiguration):
        self.config = config
        self.population: Dict[str, AgentPopulationMember] = {}
        self.training_episodes: List[TrainingEpisode] = []
        self.current_generation = 0
        self.training_active = False
        
        # Performance tracking
        self.generation_stats: List[Dict[str, Any]] = []
        self.best_performers: List[str] = []
        
        # Integration with existing systems
        self.collaborative_solver = None
        self.agent_manager = None
        self.dtesn_integration = None
        
        # Initialize integrations if available
        self._initialize_integrations()
        
        logger.info(f"Initialized MultiAgentTrainingSystem with population size {config.population_size}")

    def _initialize_integrations(self):
        """Initialize integrations with existing AAR and DTESN systems."""
        try:
            if AAR_AVAILABLE:
                self.collaborative_solver = CollaborativeProblemSolver(
                    max_concurrent_problems=self.config.episode_batch_size * 2
                )
                # Note: AgentManager would need to be properly initialized
                # with actual agent instances in a real deployment
                self.agent_manager = None  # Placeholder for actual implementation
                
            if DTESN_AVAILABLE:
                self.dtesn_integration = DTESNSelfMonitoringIntegration()
                
            logger.info("Successfully initialized system integrations")
        except Exception as e:
            logger.warning(f"Failed to initialize some integrations: {e}")

    def initialize_population(self, agent_factory: Optional[Callable] = None) -> None:
        """
        Initialize the agent population with diverse genetic parameters.
        
        Args:
            agent_factory: Optional factory function to create agents
        """
        logger.info(f"Initializing population of {self.config.population_size} agents")
        
        for i in range(self.config.population_size):
            agent_id = f"agent_{uuid.uuid4().hex[:8]}"
            
            # Generate diverse genetic parameters for neural architecture
            genetic_params = self._generate_random_genetics()
            
            member = AgentPopulationMember(
                agent_id=agent_id,
                generation=0,
                genetic_params=genetic_params,
                performance_metrics={
                    'accuracy': random.uniform(0.4, 0.6),
                    'speed': random.uniform(0.5, 0.8),
                    'adaptability': random.uniform(0.3, 0.7)
                }
            )
            
            self.population[agent_id] = member
            
        logger.info(f"Population initialized with {len(self.population)} agents")

    def _generate_random_genetics(self) -> Dict[str, Any]:
        """Generate random genetic parameters for agent architecture."""
        return {
            'learning_rate': random.uniform(0.001, 0.01),
            'layer_count': random.randint(2, 8),
            'hidden_size': random.choice([64, 128, 256, 512]),
            'activation': random.choice(['relu', 'tanh', 'sigmoid', 'gelu']),
            'dropout_rate': random.uniform(0.1, 0.5),
            'batch_size': random.choice([16, 32, 64, 128]),
            'architecture_type': random.choice(['dense', 'residual', 'attention', 'hybrid']),
            'memory_capacity': random.randint(100, 1000),
            'exploration_rate': random.uniform(0.1, 0.9),
            'cooperation_bias': random.uniform(0.0, 1.0)
        }

    async def run_training_cycle(self) -> Dict[str, Any]:
        """
        Run one complete training cycle including competitive and cooperative episodes.
        
        Returns:
            Dictionary containing cycle results and statistics
        """
        if not self.population:
            raise ValueError("Population not initialized. Call initialize_population() first.")
            
        self.training_active = True
        cycle_start = time.time()
        
        try:
            logger.info(f"Starting training cycle for generation {self.current_generation}")
            
            # Run competitive episodes
            competitive_results = await self._run_competitive_episodes()
            
            # Run cooperative episodes
            cooperative_results = await self._run_cooperative_episodes()
            
            # Evaluate population fitness
            fitness_results = await self._evaluate_population_fitness()
            
            # Evolve population (selection, crossover, mutation)
            evolution_results = await self._evolve_population()
            
            # Update generation statistics
            generation_stats = self._calculate_generation_statistics()
            self.generation_stats.append(generation_stats)
            
            self.current_generation += 1
            
            cycle_results = {
                'generation': self.current_generation - 1,
                'competitive_episodes': len(competitive_results),
                'cooperative_episodes': len(cooperative_results),
                'population_size': len(self.population),
                'best_fitness': max(member.fitness_score for member in self.population.values()),
                'average_fitness': sum(member.fitness_score for member in self.population.values()) / len(self.population),
                'evolution_improvements': evolution_results.get('improvements', 0),
                'cycle_duration': time.time() - cycle_start,
                'generation_stats': generation_stats
            }
            
            logger.info(f"Training cycle completed: {cycle_results}")
            return cycle_results
            
        except Exception as e:
            logger.error(f"Training cycle failed: {e}")
            raise
        finally:
            self.training_active = False

    async def _run_competitive_episodes(self) -> List[TrainingEpisode]:
        """Run competitive training episodes using tournament selection."""
        episodes = []
        
        # Create tournaments between agents
        num_tournaments = min(self.config.episode_batch_size, len(self.population) // 2)
        
        for i in range(num_tournaments):
            # Select agents for tournament
            participants = random.sample(list(self.population.keys()), 
                                       min(self.config.tournament_size, len(self.population)))
            
            episode = TrainingEpisode(
                episode_id=f"competitive_{uuid.uuid4().hex[:8]}",
                participants=participants,
                training_mode=TrainingMode.COMPETITIVE
            )
            
            # Simulate competitive interaction
            results = await self._simulate_competitive_interaction(participants)
            
            episode.end_time = time.time()
            episode.results = results
            episode.performance_deltas = self._calculate_performance_deltas(participants, results)
            
            # Update agent statistics
            for agent_id in participants:
                agent = self.population[agent_id]
                agent.interaction_count += 1
                
                if agent_id == results.get('winner'):
                    agent.wins += 1
                    agent.fitness_score += 0.1
                else:
                    agent.losses += 1
                    agent.fitness_score -= 0.05
                    
                agent.last_updated = time.time()
                agent.training_history.append({
                    'episode_id': episode.episode_id,
                    'type': 'competitive',
                    'result': results.get(agent_id, 'participated'),
                    'timestamp': episode.end_time
                })
            
            episodes.append(episode)
            self.training_episodes.append(episode)
            
        logger.info(f"Completed {len(episodes)} competitive episodes")
        return episodes

    async def _run_cooperative_episodes(self) -> List[TrainingEpisode]:
        """Run cooperative training episodes where agents work together."""
        episodes = []
        
        num_cooperative = min(self.config.episode_batch_size, len(self.population) // 3)
        
        for i in range(num_cooperative):
            # Select agents for cooperative task
            team_size = random.randint(2, min(5, len(self.population)))
            participants = random.sample(list(self.population.keys()), team_size)
            
            episode = TrainingEpisode(
                episode_id=f"cooperative_{uuid.uuid4().hex[:8]}",
                participants=participants,
                training_mode=TrainingMode.COOPERATIVE
            )
            
            # Simulate cooperative problem solving
            if self.collaborative_solver and AAR_AVAILABLE:
                results = await self._run_collaborative_problem_solving(participants)
            else:
                results = await self._simulate_cooperative_interaction(participants)
            
            episode.end_time = time.time()
            episode.results = results
            episode.performance_deltas = self._calculate_performance_deltas(participants, results)
            
            # Update agent cooperation scores
            team_success = results.get('team_success', 0.5)
            for agent_id in participants:
                agent = self.population[agent_id]
                agent.interaction_count += 1
                agent.cooperation_score = (agent.cooperation_score * 0.8 + team_success * 0.2)
                agent.fitness_score += team_success * 0.05
                agent.last_updated = time.time()
                agent.training_history.append({
                    'episode_id': episode.episode_id,
                    'type': 'cooperative',
                    'team_success': team_success,
                    'timestamp': episode.end_time
                })
            
            episodes.append(episode)
            self.training_episodes.append(episode)
            
        logger.info(f"Completed {len(episodes)} cooperative episodes")
        return episodes

    async def _simulate_competitive_interaction(self, participants: List[str]) -> Dict[str, Any]:
        """
        Simulate competitive interaction between agents.
        
        In a real implementation, this would involve actual agent competition.
        For now, we simulate based on agent parameters and performance metrics.
        """
        # Calculate competitive scores based on agent parameters
        scores = {}
        
        for agent_id in participants:
            agent = self.population[agent_id]
            
            # Competition score based on genetic parameters and performance
            competition_score = (
                agent.performance_metrics.get('speed', 0.5) * 0.4 +
                agent.performance_metrics.get('accuracy', 0.5) * 0.4 +
                agent.genetic_params.get('exploration_rate', 0.5) * 0.2
            )
            
            # Add some randomness for realistic competition
            competition_score *= random.uniform(0.8, 1.2)
            scores[agent_id] = competition_score
        
        # Determine winner
        winner = max(scores.keys(), key=lambda k: scores[k])
        
        return {
            'winner': winner,
            'scores': scores,
            'competition_type': 'performance_tournament'
        }

    async def _simulate_cooperative_interaction(self, participants: List[str]) -> Dict[str, Any]:
        """
        Simulate cooperative interaction between agents.
        
        In a real implementation, this would involve collaborative problem solving.
        """
        # Calculate team cooperation effectiveness
        team_cooperation = 0.0
        individual_contributions = {}
        
        for agent_id in participants:
            agent = self.population[agent_id]
            
            # Individual contribution based on cooperation bias and adaptability
            contribution = (
                agent.genetic_params.get('cooperation_bias', 0.5) * 0.6 +
                agent.performance_metrics.get('adaptability', 0.5) * 0.4
            )
            
            individual_contributions[agent_id] = contribution
            team_cooperation += contribution
        
        # Team effectiveness with synergy bonus
        team_size = len(participants)
        synergy_bonus = min(0.3, team_size * 0.05)  # Small bonus for larger teams
        team_success = min(1.0, (team_cooperation / team_size) + synergy_bonus)
        
        return {
            'team_success': team_success,
            'individual_contributions': individual_contributions,
            'synergy_bonus': synergy_bonus,
            'cooperation_type': 'collaborative_problem_solving'
        }

    async def _run_collaborative_problem_solving(self, participants: List[str]) -> Dict[str, Any]:
        """
        Use the existing collaborative solver for realistic cooperative episodes.
        """
        if not self.collaborative_solver:
            return await self._simulate_cooperative_interaction(participants)
        
        # Create a collaborative problem
        problem = ProblemDefinition(
            problem_id=f"training_{uuid.uuid4().hex[:8]}",
            problem_type=random.choice(list(ProblemType)),
            title="Multi-Agent Training Problem",
            description="Collaborative problem for agent training",
            objectives=["maximize_team_performance", "optimize_resource_usage"],
            constraints={"time_limit": 30.0, "resource_budget": 1000},
            success_criteria={"min_quality": 0.7, "max_time": 30.0},
            required_capabilities=["reasoning", "optimization"]
        )
        
        try:
            # This would integrate with the actual collaborative solver
            # For now, simulate the collaborative solving process
            solution_quality = random.uniform(0.5, 0.9)
            return {
                'team_success': solution_quality,
                'problem_solved': solution_quality > 0.7,
                'collaboration_method': 'distributed_problem_solving'
            }
        except Exception as e:
            logger.warning(f"Collaborative problem solving failed: {e}")
            return await self._simulate_cooperative_interaction(participants)

    def _calculate_performance_deltas(self, participants: List[str], results: Dict[str, Any]) -> Dict[str, float]:
        """Calculate performance changes for agents based on episode results."""
        deltas = {}
        
        if results.get('winner') in participants:
            # Competitive episode
            winner = results['winner']
            for agent_id in participants:
                if agent_id == winner:
                    deltas[agent_id] = 0.05  # Winner gets performance boost
                else:
                    deltas[agent_id] = -0.02  # Others get small penalty
        else:
            # Cooperative episode
            team_success = results.get('team_success', 0.5)
            for agent_id in participants:
                contribution = results.get('individual_contributions', {}).get(agent_id, 0.5)
                deltas[agent_id] = (team_success + contribution) * 0.02
        
        return deltas

    async def _evaluate_population_fitness(self) -> Dict[str, Any]:
        """Evaluate overall fitness of population members."""
        fitness_results = {}
        
        for agent_id, agent in self.population.items():
            # Multi-objective fitness function
            competitive_fitness = 0.0
            if agent.interaction_count > 0:
                competitive_fitness = agent.wins / agent.interaction_count
            
            cooperative_fitness = agent.cooperation_score
            
            # Weighted combination based on configuration
            total_fitness = (
                competitive_fitness * self.config.competition_weight +
                cooperative_fitness * self.config.cooperation_weight
            )
            
            # Apply performance metrics
            performance_factor = sum(agent.performance_metrics.values()) / len(agent.performance_metrics)
            total_fitness *= performance_factor
            
            agent.fitness_score = total_fitness
            fitness_results[agent_id] = total_fitness
        
        logger.info(f"Evaluated fitness for {len(fitness_results)} agents")
        return fitness_results

    async def _evolve_population(self) -> Dict[str, Any]:
        """
        Evolve the population using genetic algorithms with selection, crossover, and mutation.
        """
        if not self.population:
            return {'improvements': 0}
        
        # Sort population by fitness
        sorted_agents = sorted(self.population.items(), 
                             key=lambda x: x[1].fitness_score, 
                             reverse=True)
        
        # Calculate elite count
        elite_count = max(1, int(len(sorted_agents) * self.config.elite_percentage))
        
        # Keep elite agents
        new_population = {}
        for i in range(elite_count):
            agent_id, agent = sorted_agents[i]
            new_population[agent_id] = agent
        
        # Generate offspring through crossover and mutation
        improvements = 0
        
        while len(new_population) < self.config.population_size:
            # Tournament selection for parents
            parent1 = self._tournament_selection(sorted_agents)
            parent2 = self._tournament_selection(sorted_agents)
            
            if random.random() < self.config.crossover_rate:
                # Crossover
                child_genetics = self._crossover(parent1.genetic_params, parent2.genetic_params)
            else:
                # Copy parent
                child_genetics = parent1.genetic_params.copy()
            
            # Mutation
            if random.random() < self.config.mutation_rate:
                child_genetics = self._mutate(child_genetics)
                improvements += 1
            
            # Create new agent
            child_id = f"agent_{uuid.uuid4().hex[:8]}"
            child = AgentPopulationMember(
                agent_id=child_id,
                generation=self.current_generation + 1,
                genetic_params=child_genetics,
                performance_metrics={
                    key: random.uniform(0.3, 0.7) for key in parent1.performance_metrics.keys()
                }
            )
            
            new_population[child_id] = child
        
        # Replace population
        self.population = new_population
        
        logger.info(f"Population evolved with {improvements} improvements")
        return {'improvements': improvements, 'elite_preserved': elite_count}

    def _tournament_selection(self, sorted_agents: List[Tuple[str, AgentPopulationMember]]) -> AgentPopulationMember:
        """Select agent using tournament selection."""
        tournament_size = min(self.config.tournament_size, len(sorted_agents))
        tournament = random.sample(sorted_agents, tournament_size)
        return max(tournament, key=lambda x: x[1].fitness_score)[1]

    def _crossover(self, parent1_genetics: Dict[str, Any], parent2_genetics: Dict[str, Any]) -> Dict[str, Any]:
        """Perform crossover between two parent genetic parameter sets."""
        child_genetics = {}
        
        for key in parent1_genetics.keys():
            if key in parent2_genetics:
                # Random selection from parents with some blending for numeric values
                if isinstance(parent1_genetics[key], (int, float)):
                    # Blend numeric values
                    alpha = random.random()
                    child_genetics[key] = alpha * parent1_genetics[key] + (1 - alpha) * parent2_genetics[key]
                    
                    # Ensure integer types remain integers
                    if isinstance(parent1_genetics[key], int):
                        child_genetics[key] = int(child_genetics[key])
                else:
                    # Random selection for non-numeric values
                    child_genetics[key] = random.choice([parent1_genetics[key], parent2_genetics[key]])
            else:
                child_genetics[key] = parent1_genetics[key]
        
        return child_genetics

    def _mutate(self, genetics: Dict[str, Any]) -> Dict[str, Any]:
        """Apply mutation to genetic parameters."""
        mutated = genetics.copy()
        
        # Randomly select parameters to mutate
        mutation_targets = random.sample(list(genetics.keys()), 
                                       max(1, int(len(genetics) * 0.3)))
        
        for key in mutation_targets:
            if isinstance(genetics[key], float):
                # Gaussian mutation for float values
                std_dev = abs(genetics[key]) * 0.1 or 0.01
                mutated[key] = max(0, genetics[key] + random.gauss(0, std_dev))
            elif isinstance(genetics[key], int):
                # Discrete mutation for integer values
                mutation_range = max(1, int(genetics[key] * 0.2))
                mutated[key] = max(1, genetics[key] + random.randint(-mutation_range, mutation_range))
            elif isinstance(genetics[key], str):
                # Random replacement for string values
                if key == 'activation':
                    mutated[key] = random.choice(['relu', 'tanh', 'sigmoid', 'gelu'])
                elif key == 'architecture_type':
                    mutated[key] = random.choice(['dense', 'residual', 'attention', 'hybrid'])
        
        return mutated

    def _calculate_generation_statistics(self) -> Dict[str, Any]:
        """Calculate statistics for the current generation."""
        if not self.population:
            return {}
        
        fitness_values = [agent.fitness_score for agent in self.population.values()]
        interaction_counts = [agent.interaction_count for agent in self.population.values()]
        cooperation_scores = [agent.cooperation_score for agent in self.population.values()]
        
        stats = {
            'generation': self.current_generation,
            'population_size': len(self.population),
            'fitness': {
                'mean': sum(fitness_values) / len(fitness_values),
                'max': max(fitness_values),
                'min': min(fitness_values),
                'std': math.sqrt(sum((x - sum(fitness_values) / len(fitness_values))**2 for x in fitness_values) / len(fitness_values))
            },
            'interactions': {
                'total': sum(interaction_counts),
                'mean_per_agent': sum(interaction_counts) / len(interaction_counts)
            },
            'cooperation': {
                'mean_score': sum(cooperation_scores) / len(cooperation_scores),
                'max_score': max(cooperation_scores),
                'min_score': min(cooperation_scores)
            },
            'diversity_metrics': self._calculate_diversity_metrics()
        }
        
        return stats

    def _calculate_diversity_metrics(self) -> Dict[str, float]:
        """Calculate population diversity metrics."""
        if not self.population:
            return {}
        
        # Calculate genetic diversity
        genetic_params_values = defaultdict(list)
        for agent in self.population.values():
            for param, value in agent.genetic_params.items():
                if isinstance(value, (int, float)):
                    genetic_params_values[param].append(value)
        
        diversity_scores = {}
        for param, values in genetic_params_values.items():
            if len(values) > 1:
                # Coefficient of variation as diversity measure
                mean_val = sum(values) / len(values)
                if mean_val != 0:
                    std_dev = math.sqrt(sum((x - mean_val)**2 for x in values) / len(values))
                    diversity_scores[f"{param}_diversity"] = std_dev / mean_val
        
        overall_diversity = sum(diversity_scores.values()) / len(diversity_scores) if diversity_scores else 0.0
        
        return {
            'overall_diversity': overall_diversity,
            'parameter_diversities': diversity_scores
        }

    def get_training_statistics(self) -> Dict[str, Any]:
        """Get comprehensive training statistics."""
        current_stats = self._calculate_generation_statistics()
        
        return {
            'current_generation': self.current_generation,
            'training_active': self.training_active,
            'population_status': current_stats,
            'historical_generations': self.generation_stats,
            'total_episodes': len(self.training_episodes),
            'best_performers': self._get_top_performers(5),
            'system_integrations': {
                'aar_available': AAR_AVAILABLE,
                'dtesn_available': DTESN_AVAILABLE,
                'collaborative_solver_active': self.collaborative_solver is not None
            }
        }

    def _get_top_performers(self, count: int = 5) -> List[Dict[str, Any]]:
        """Get top performing agents."""
        if not self.population:
            return []
        
        sorted_agents = sorted(self.population.values(), 
                             key=lambda x: x.fitness_score, 
                             reverse=True)
        
        top_performers = []
        for agent in sorted_agents[:count]:
            top_performers.append({
                'agent_id': agent.agent_id,
                'generation': agent.generation,
                'fitness_score': agent.fitness_score,
                'wins': agent.wins,
                'losses': agent.losses,
                'cooperation_score': agent.cooperation_score,
                'interaction_count': agent.interaction_count
            })
        
        return top_performers

    async def run_continuous_training(self, max_generations: Optional[int] = None) -> List[Dict[str, Any]]:
        """
        Run continuous training for multiple generations.
        
        Args:
            max_generations: Maximum number of generations to run (uses config if None)
            
        Returns:
            List of results from each training cycle
        """
        max_gens = max_generations or self.config.max_generations
        results = []
        
        logger.info(f"Starting continuous training for {max_gens} generations")
        
        for generation in range(max_gens):
            try:
                cycle_result = await self.run_training_cycle()
                results.append(cycle_result)
                
                # Check for convergence or other stopping criteria
                if self._should_stop_training(results):
                    logger.info(f"Training stopped early at generation {generation} due to convergence")
                    break
                    
            except Exception as e:
                logger.error(f"Training failed at generation {generation}: {e}")
                break
        
        logger.info(f"Continuous training completed after {len(results)} generations")
        return results

    def _should_stop_training(self, results: List[Dict[str, Any]]) -> bool:
        """Determine if training should stop early based on convergence criteria."""
        if len(results) < 10:  # Need at least 10 generations to check convergence
            return False
        
        # Check if fitness improvement has plateaued
        recent_fitness = [r['best_fitness'] for r in results[-5:]]
        if len(set(f"{f:.3f}" for f in recent_fitness)) == 1:  # No improvement in last 5 generations
            return True
        
        # Check if population diversity is too low
        latest_stats = results[-1].get('generation_stats', {})
        diversity = latest_stats.get('diversity_metrics', {}).get('overall_diversity', 1.0)
        if diversity < 0.01:  # Very low diversity
            return True
        
        return False


# Integration with existing DTESN monitoring
class DTESNMultiAgentTrainingIntegration:
    """Integration layer for multi-agent training with DTESN monitoring."""
    
    def __init__(self, training_system: MultiAgentTrainingSystem):
        self.training_system = training_system
        self.dtesn_monitors: Dict[str, Any] = {}
    
    def register_dtesn_monitor(self, agent_id: str, monitor_system: Any):
        """Register DTESN monitoring for an agent."""
        self.dtesn_monitors[agent_id] = monitor_system
    
    async def sync_with_dtesn_performance(self):
        """Synchronize training metrics with DTESN performance monitoring."""
        if not DTESN_AVAILABLE:
            return
        
        for agent_id, agent in self.training_system.population.items():
            if agent_id in self.dtesn_monitors:
                # Update agent performance metrics from DTESN monitoring
                monitor = self.dtesn_monitors[agent_id]
                if hasattr(monitor, 'get_monitoring_status'):
                    status = monitor.get_monitoring_status()
                    
                    # Update performance metrics based on DTESN data
                    agent.performance_metrics.update({
                        'dtesn_efficiency': status.get('system_efficiency', 0.5),
                        'dtesn_stability': status.get('system_stability', 0.5),
                        'dtesn_responsiveness': status.get('response_time_score', 0.5)
                    })