import asyncio
import unittest
import logging
import sys
import os
from typing import Dict, Any, List
sys.path.append(os.path.dirname(os.path.dirname(__file__)))
from core.interfaces import Individual, FitnessEvaluator, EvolutionConfig
from core.evolution_engine import EchoSelfEvolutionEngine
from core.operators import MutationOperator, SelectionOperator, CrossoverOperator
from neural.topology_individual import NeuralTopologyIndividual
class SimpleFitnessEvaluator(FitnessEvaluator):
    async def evaluate(self, individual: Individual) -> float:
        genome = individual.genome
        layers = genome.get('layers', [])
        if not layers:
            return 0.0
        complexity_score = min(len(layers) / 10.0, 1.0)
        size_score = 0.0
        for layer in layers:
            size = layer.get('size', 64)
            if 16 <= size <= 256:
                size_score += 0.1
        fitness = 0.3 * complexity_score + 0.7 * min(size_score, 1.0)
        return max(0.1, min(1.0, fitness))
    async def batch_evaluate(self, individuals: List[Individual]) -> List[float]:
        fitnesses = []
        for individual in individuals:
            fitness = await self.evaluate(individual)
            fitnesses.append(fitness)
        return fitnesses
def create_simple_network_genome() -> Dict[str, Any]:
    import random
    num_layers = random.randint(2, 4)
    layers = []
    for i in range(num_layers):
        layer = {'type': random.choice(['dense', 'lstm', 'attention']), 'size': random.choice([32, 64, 128, 256])}
        layers.append(layer)
    connections = []
    for i in range(num_layers - 1):
        connections.append({'from': i, 'to': i + 1, 'weight': random.uniform(-1.0, 1.0), 'type': 'direct'})
    activation_functions = {}
    for i in range(num_layers):
        activation_functions[str(i)] = random.choice(['relu', 'tanh', 'sigmoid'])
    parameters = {'learning_rate': random.uniform(0.001, 0.01), 'batch_size': random.choice([16, 32, 64]), 'optimizer': random.choice(['adam', 'sgd'])}
    return {'layers': layers, 'connections': connections, 'activation_functions': activation_functions, 'parameters': parameters}
class TestEvolutionEngine(unittest.TestCase):
    def setUp(self):
        self.config = EvolutionConfig(population_size=10, max_generations=5, mutation_rate=0.1, crossover_rate=0.7, elitism_ratio=0.1)
        self.fitness_evaluator = SimpleFitnessEvaluator()
        self.engine = EchoSelfEvolutionEngine(config=self.config, fitness_evaluator=self.fitness_evaluator, individual_class=NeuralTopologyIndividual)
    async def async_setUp(self):
        await self.engine.initialize_population(create_simple_network_genome)
    def test_individual_creation(self):
        genome = create_simple_network_genome()
        individual = NeuralTopologyIndividual(genome)
        self.assertIsNotNone(individual)
        self.assertGreater(len(individual.genome['layers']), 0)
        self.assertIn('connections', individual.genome)
        self.assertIn('parameters', individual.genome)
    def test_individual_mutation(self):
        genome = create_simple_network_genome()
        individual = NeuralTopologyIndividual(genome)
        mutated = individual.mutate(0.5)
        self.assertIsNotNone(mutated)
        self.assertNotEqual(individual.id, mutated.id)
        self.assertEqual(len(mutated.parent_ids), 1)
        self.assertEqual(mutated.parent_ids[0], individual.id)
    def test_individual_crossover(self):
        genome1 = create_simple_network_genome()
        genome2 = create_simple_network_genome()
        individual1 = NeuralTopologyIndividual(genome1)
        individual2 = NeuralTopologyIndividual(genome2)
        child1, child2 = individual1.crossover(individual2)
        self.assertIsNotNone(child1)
        self.assertIsNotNone(child2)
        self.assertNotEqual(child1.id, individual1.id)
        self.assertNotEqual(child2.id, individual2.id)
    def test_individual_distance(self):
        genome1 = create_simple_network_genome()
        genome2 = create_simple_network_genome()
        individual1 = NeuralTopologyIndividual(genome1)
        individual2 = NeuralTopologyIndividual(genome2)
        distance = individual1.distance(individual2)
        self.assertIsInstance(distance, float)
        self.assertGreaterEqual(distance, 0.0)
    async def test_fitness_evaluation(self):
        genome = create_simple_network_genome()
        individual = NeuralTopologyIndividual(genome)
        fitness = await self.fitness_evaluator.evaluate(individual)
        self.assertIsInstance(fitness, float)
        self.assertGreaterEqual(fitness, 0.0)
        self.assertLessEqual(fitness, 1.0)
    def test_evolutionary_operators(self):
        from core.interfaces import Population
        population = Population()
        for _ in range(5):
            genome = create_simple_network_genome()
            individual = NeuralTopologyIndividual(genome)
            individual.fitness = 0.5
            population.add_individual(individual)
        selector = SelectionOperator('tournament')
        selected = selector.apply(population, self.config)
        self.assertEqual(selected.size(), self.config.population_size)
        crossover_op = CrossoverOperator('uniform')
        offspring = crossover_op.apply(selected, self.config)
        self.assertGreater(offspring.size(), 0)
        mutator = MutationOperator()
        mutated = mutator.apply(offspring, self.config)
        self.assertGreater(mutated.size(), 0)
class TestEvolutionIntegration(unittest.TestCase):
    async def test_complete_evolution_run(self):
        config = EvolutionConfig(population_size=6, max_generations=3, mutation_rate=0.2, crossover_rate=0.8)
        fitness_evaluator = SimpleFitnessEvaluator()
        engine = EchoSelfEvolutionEngine(config=config, fitness_evaluator=fitness_evaluator, individual_class=NeuralTopologyIndividual)
        await engine.initialize_population(create_simple_network_genome)
        final_population = await engine.evolve(num_generations=3)
        self.assertIsNotNone(final_population)
        self.assertEqual(final_population.size(), config.population_size)
        best_individual = final_population.get_best_individual()
        self.assertIsNotNone(best_individual)
        self.assertGreater(best_individual.fitness, 0.0)
        stats = engine.get_evolution_statistics()
        self.assertGreater(len(stats), 0)
        print('\nEvolution completed:')
        print(f'Final best fitness: {best_individual.fitness:.4f}')
        print(f'Best individual summary: {best_individual.get_network_summary()}')
async def run_async_tests():
    print('Running Echo-Self Evolution Engine Tests...\n')
    test_case = TestEvolutionEngine()
    test_case.setUp()
    await test_case.async_setUp()
    print('Testing individual creation...')
    test_case.test_individual_creation()
    print('✓ Individual creation test passed')
    print('Testing individual mutation...')
    test_case.test_individual_mutation()
    print('✓ Individual mutation test passed')
    print('Testing individual crossover...')
    test_case.test_individual_crossover()
    print('✓ Individual crossover test passed')
    print('Testing genetic distance...')
    test_case.test_individual_distance()
    print('✓ Genetic distance test passed')
    print('Testing fitness evaluation...')
    await test_case.test_fitness_evaluation()
    print('✓ Fitness evaluation test passed')
    print('Testing evolutionary operators...')
    test_case.test_evolutionary_operators()
    print('✓ Evolutionary operators test passed')
    print('\nTesting complete evolution process...')
    integration_test = TestEvolutionIntegration()
    await integration_test.test_complete_evolution_run()
    print('✓ Complete evolution test passed')
    print('\n✅ All tests passed! Echo-Self Evolution Engine can evolve simple neural network topologies.')
def main():
    logging.basicConfig(level=logging.INFO)
    asyncio.run(run_async_tests())
if __name__ == '__main__':
    main()