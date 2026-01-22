from abc import ABC, abstractmethod
from typing import List, Dict, Any, Tuple, Optional
from dataclasses import dataclass
import uuid
@dataclass
class EvolutionConfig:
    population_size: int = 100
    mutation_rate: float = 0.01
    crossover_rate: float = 0.7
    selection_pressure: float = 0.8
    elitism_ratio: float = 0.1
    max_generations: int = 1000
    fitness_threshold: float = 0.95
    tournament_size: int = 3
    diversity_threshold: float = 0.1
class Individual(ABC):
    def __init__(self, genome: Dict[str, Any]):
        self.id = str(uuid.uuid4())
        self.genome = genome
        self.fitness: float = 0.0
        self.age: int = 0
        self.generation: int = 0
        self.parent_ids: List[str] = []
        self.performance_history: List[float] = []
    @abstractmethod
    async def evaluate_fitness(self, evaluator: 'FitnessEvaluator') -> float:
        pass
    @abstractmethod
    def mutate(self, mutation_rate: float) -> 'Individual':
        pass
    @abstractmethod
    def crossover(self, other: 'Individual') -> Tuple['Individual', 'Individual']:
        pass
    @abstractmethod
    def distance(self, other: 'Individual') -> float:
        pass
    def clone(self) -> 'Individual':
        clone = self.__class__(self.genome.copy())
        clone.fitness = self.fitness
        clone.age = self.age
        clone.generation = self.generation
        clone.parent_ids = self.parent_ids.copy()
        clone.performance_history = self.performance_history.copy()
        return clone
class Population:
    def __init__(self, individuals: List[Individual]=None):
        self.individuals: List[Individual] = individuals or []
        self.generation: int = 0
        self.best_fitness: float = 0.0
        self.average_fitness: float = 0.0
        self.diversity: float = 0.0
    def add_individual(self, individual: Individual) -> None:
        self.individuals.append(individual)
    def remove_individual(self, individual: Individual) -> None:
        if individual in self.individuals:
            self.individuals.remove(individual)
    def size(self) -> int:
        return len(self.individuals)
    def get_best_individual(self) -> Optional[Individual]:
        if not self.individuals:
            return None
        return max(self.individuals, key=lambda x: x.fitness)
    def get_worst_individual(self) -> Optional[Individual]:
        if not self.individuals:
            return None
        return min(self.individuals, key=lambda x: x.fitness)
    def calculate_statistics(self) -> Dict[str, float]:
        if not self.individuals:
            return {'best': 0.0, 'worst': 0.0, 'average': 0.0, 'diversity': 0.0}
        fitnesses = [ind.fitness for ind in self.individuals]
        self.best_fitness = max(fitnesses)
        worst_fitness = min(fitnesses)
        self.average_fitness = sum(fitnesses) / len(fitnesses)
        if len(self.individuals) > 1:
            distances = []
            for i, ind1 in enumerate(self.individuals):
                for ind2 in self.individuals[i + 1:]:
                    distances.append(ind1.distance(ind2))
            self.diversity = sum(distances) / len(distances) if distances else 0.0
        else:
            self.diversity = 0.0
        return {'best': self.best_fitness, 'worst': worst_fitness, 'average': self.average_fitness, 'diversity': self.diversity}
class FitnessEvaluator(ABC):
    @abstractmethod
    async def evaluate(self, individual: Individual) -> float:
        pass
    @abstractmethod
    async def batch_evaluate(self, individuals: List[Individual]) -> List[float]:
        pass
class EvolutionObserver(ABC):
    @abstractmethod
    async def on_generation_start(self, generation: int, population: Population) -> None:
        pass
    @abstractmethod
    async def on_generation_end(self, generation: int, population: Population) -> None:
        pass
    @abstractmethod
    async def on_evolution_complete(self, final_population: Population) -> None:
        pass
class EvolutionaryOperator(ABC):
    @abstractmethod
    def apply(self, population: Population, config: EvolutionConfig) -> Population:
        pass