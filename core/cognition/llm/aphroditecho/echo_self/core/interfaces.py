from abc import ABC, abstractmethod
from typing import Dict, Any, Tuple
class Individual(ABC):
    def __init__(self, genome: Dict[str, Any]):
        self.genome = genome
        self.fitness = 0.0
        self.age = 0
        self.performance_history = []
    @abstractmethod
    async def evaluate_fitness(self, environment) -> float:
        pass
    @abstractmethod
    def mutate(self, mutation_rate: float) -> 'Individual':
        pass
    def crossover(self, other: 'Individual') -> Tuple['Individual', 'Individual']:
        child1_genome = {}
        child2_genome = {}
        for key in self.genome:
            if key in other.genome:
                import random
                if random.random() < 0.5:
                    child1_genome[key] = self.genome[key]
                    child2_genome[key] = other.genome[key]
                else:
                    child1_genome[key] = other.genome[key]
                    child2_genome[key] = self.genome[key]
            else:
                child1_genome[key] = self.genome[key]
        for key in other.genome:
            if key not in self.genome:
                child2_genome[key] = other.genome[key]
        child1 = self.__class__(child1_genome)
        child2 = self.__class__(child2_genome)
        return (child1, child2)
class FitnessEvaluator(ABC):
    @abstractmethod
    async def evaluate(self, individual: Individual, environment: Dict) -> float:
        pass
    @abstractmethod
    def get_evaluation_criteria(self) -> Dict[str, Any]:
        pass
class SimpleIndividual(Individual):
    async def evaluate_fitness(self, environment) -> float:
        if not self.genome:
            return 0.0
        fitness = 0.0
        for key, value in self.genome.items():
            if isinstance(value, (int, float)):
                fitness += abs(value)
        self.fitness = fitness
        return fitness
    def mutate(self, mutation_rate: float) -> 'Individual':
        import random
        import copy
        new_genome = copy.deepcopy(self.genome)
        for key, value in new_genome.items():
            if random.random() < mutation_rate:
                if isinstance(value, float):
                    new_genome[key] = value + random.gauss(0, 0.1)
                elif isinstance(value, int):
                    new_genome[key] = max(1, value + random.randint(-1, 1))
                elif isinstance(value, str):
                    new_genome[key] = value + random.choice('abcdefghijklmnopqrstuvwxyz')
        mutated_individual = SimpleIndividual(new_genome)
        mutated_individual.age = self.age + 1
        return mutated_individual
class SimpleFitnessEvaluator(FitnessEvaluator):
    async def evaluate(self, individual: Individual, environment: Dict) -> float:
        return await individual.evaluate_fitness(environment)
    def get_evaluation_criteria(self) -> Dict[str, Any]:
        return {'type': 'simple', 'description': 'Basic fitness evaluation using individual method'}