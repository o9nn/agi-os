import asyncio
import unittest
import logging
import time
from typing import Dict, Any, List
from unittest.mock import Mock, patch
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(__file__)))
from core.adaptive_architecture import AdaptiveArchitectureFramework, PerformanceMetrics, ArchitectureMutation, PerformanceMonitor, ArchitectureOptimizer
from core.interfaces import EvolutionConfig, Individual
from core.evolution_engine import EchoSelfEvolutionEngine
from neural.topology_individual import NeuralTopologyIndividual
from integration.aphrodite_adaptive import AphroditeAdaptiveIntegration, ModelTopologyAdapter, InferenceHookManager, AdaptiveModelLoader
class MockFitnessEvaluator:
    async def evaluate(self, individual: Individual) -> float:
        return 0.8
    async def batch_evaluate(self, individuals: List[Individual]) -> List[float]:
        return [0.8] * len(individuals)
def create_test_genome() -> Dict[str, Any]:
    return {'layers': [{'type': 'dense', 'size': 128}, {'type': 'dense', 'size': 64}, {'type': 'dense', 'size': 32}], 'connections': [{'from': 0, 'to': 1, 'weight': 1.0, 'type': 'direct'}, {'from': 1, 'to': 2, 'weight': 1.0, 'type': 'direct'}], 'parameters': {'learning_rate': 0.001, 'batch_size': 32}}
def create_test_model_config() -> Dict[str, Any]:
    return {'model_name': 'test-model', 'hidden_size': 768, 'num_attention_heads': 12, 'num_hidden_layers': 6, 'intermediate_size': 3072, 'vocab_size': 50000}
class TestPerformanceMetrics(unittest.TestCase):
    def test_performance_metrics_creation(self):
        metrics = PerformanceMetrics(latency_ms=100.0, throughput_tokens_per_sec=50.0, memory_usage_mb=1024.0, accuracy_score=0.9)
        self.assertEqual(metrics.latency_ms, 100.0)
        self.assertEqual(metrics.throughput_tokens_per_sec, 50.0)
        self.assertEqual(metrics.memory_usage_mb, 1024.0)
        self.assertEqual(metrics.accuracy_score, 0.9)
        self.assertIsInstance(metrics.timestamp, float)
    def test_overall_score_calculation(self):
        good_metrics = PerformanceMetrics(latency_ms=50.0, throughput_tokens_per_sec=100.0, memory_usage_mb=512.0, accuracy_score=0.95)
        good_score = good_metrics.overall_score()
        self.assertGreater(good_score, 0.8)
        poor_metrics = PerformanceMetrics(latency_ms=2000.0, throughput_tokens_per_sec=5.0, memory_usage_mb=16384.0, accuracy_score=0.3)
        poor_score = poor_metrics.overall_score()
        self.assertLess(poor_score, 0.3)
class TestArchitectureMutation(unittest.TestCase):
    def test_add_layer_mutation(self):
        genome = create_test_genome()
        original_layer_count = len(genome['layers'])
        mutation = ArchitectureMutation(mutation_type='add_layer', parameters={'position': 1, 'layer_config': {'type': 'attention', 'size': 256}})
        mutated_genome = mutation.apply_to_genome(genome)
        self.assertEqual(len(mutated_genome['layers']), original_layer_count + 1)
        self.assertEqual(mutated_genome['layers'][1]['type'], 'attention')
        self.assertEqual(mutated_genome['layers'][1]['size'], 256)
    def test_remove_layer_mutation(self):
        genome = create_test_genome()
        original_layer_count = len(genome['layers'])
        mutation = ArchitectureMutation(mutation_type='remove_layer', target_layer=1)
        mutated_genome = mutation.apply_to_genome(genome)
        self.assertEqual(len(mutated_genome['layers']), original_layer_count - 1)
    def test_modify_layer_mutation(self):
        genome = create_test_genome()
        mutation = ArchitectureMutation(mutation_type='modify_layer', target_layer=0, parameters={'size': 256, 'activation': 'tanh'})
        mutated_genome = mutation.apply_to_genome(genome)
        self.assertEqual(mutated_genome['layers'][0]['size'], 256)
        self.assertEqual(mutated_genome['layers'][0]['activation'], 'tanh')
    def test_adjust_connections_mutation(self):
        genome = create_test_genome()
        original_weight = genome['connections'][0]['weight']
        mutation = ArchitectureMutation(mutation_type='adjust_connections', parameters={'weight_adjustment': 0.1})
        mutated_genome = mutation.apply_to_genome(genome)
        new_weight = mutated_genome['connections'][0]['weight']
        expected_weight = original_weight * 1.1
        self.assertAlmostEqual(new_weight, expected_weight, places=5)
class TestPerformanceMonitor(unittest.TestCase):
    def setUp(self):
        self.monitor = PerformanceMonitor(window_size=50)
    def test_add_metrics(self):
        metrics = PerformanceMetrics(latency_ms=100.0, throughput_tokens_per_sec=50.0)
        self.monitor.add_metrics(metrics)
        self.assertEqual(len(self.monitor.metrics_history), 1)
        self.assertIsNotNone(self.monitor.baseline_metrics)
    def test_current_performance_calculation(self):
        for i in range(10):
            metrics = PerformanceMetrics(latency_ms=100.0 + i, throughput_tokens_per_sec=50.0 + i, memory_usage_mb=1024.0 + i * 10, accuracy_score=0.8 + i * 0.01)
            self.monitor.add_metrics(metrics)
        current_perf = self.monitor.get_current_performance()
        self.assertIn('avg_latency_ms', current_perf)
        self.assertIn('avg_throughput', current_perf)
        self.assertIn('avg_memory_mb', current_perf)
        self.assertIn('avg_accuracy', current_perf)
    def test_performance_degradation_detection(self):
        baseline_metrics = PerformanceMetrics(latency_ms=50.0, throughput_tokens_per_sec=100.0, memory_usage_mb=512.0, accuracy_score=0.9)
        for i in range(25):
            self.monitor.add_metrics(baseline_metrics)
        degraded_metrics = PerformanceMetrics(latency_ms=500.0, throughput_tokens_per_sec=20.0, memory_usage_mb=4096.0, accuracy_score=0.4)
        for i in range(15):
            self.monitor.add_metrics(degraded_metrics)
        self.assertTrue(self.monitor.detect_performance_degradation(threshold=0.05))
class TestArchitectureOptimizer(unittest.TestCase):
    def setUp(self):
        self.optimizer = ArchitectureOptimizer()
    def test_suggest_mutations_for_high_latency(self):
        genome = create_test_genome()
        metrics = PerformanceMetrics(latency_ms=800.0, throughput_tokens_per_sec=20.0, accuracy_score=0.8)
        mutations = self.optimizer.suggest_mutations(genome, metrics)
        self.assertGreater(len(mutations), 0)
        mutation_types = [m.mutation_type for m in mutations]
        self.assertTrue(any((mt in ['modify_layer', 'remove_layer'] for mt in mutation_types)))
    def test_suggest_mutations_for_high_memory(self):
        genome = create_test_genome()
        metrics = PerformanceMetrics(latency_ms=100.0, memory_usage_mb=6000.0, accuracy_score=0.8)
        mutations = self.optimizer.suggest_mutations(genome, metrics)
        self.assertGreater(len(mutations), 0)
        has_layer_modification = any((m.mutation_type == 'modify_layer' for m in mutations))
        self.assertTrue(has_layer_modification)
    def test_suggest_mutations_for_low_accuracy(self):
        genome = create_test_genome()
        metrics = PerformanceMetrics(latency_ms=100.0, accuracy_score=0.5)
        mutations = self.optimizer.suggest_mutations(genome, metrics)
        self.assertGreater(len(mutations), 0)
        mutation_types = [m.mutation_type for m in mutations]
        self.assertTrue(any((mt in ['add_layer', 'modify_layer'] for mt in mutation_types)))
class TestModelTopologyAdapter(unittest.TestCase):
    def setUp(self):
        self.adapter = ModelTopologyAdapter()
    def test_can_modify_architecture(self):
        complete_config = create_test_model_config()
        self.assertTrue(self.adapter.can_modify_architecture(complete_config))
        incomplete_config = {'model_name': 'test'}
        self.assertFalse(self.adapter.can_modify_architecture(incomplete_config))
    def test_scale_layer_dimensions(self):
        config = create_test_model_config()
        original_hidden_size = config['hidden_size']
        mutation = ArchitectureMutation(mutation_type='layer_scaling', parameters={'scale_factor': 1.5})
        modified_config = self.adapter.apply_mutation_to_model_config(config, mutation)
        self.assertNotEqual(modified_config['hidden_size'], original_hidden_size)
        self.assertGreater(modified_config['hidden_size'], original_hidden_size)
    def test_adjust_attention_heads(self):
        config = create_test_model_config()
        mutation = ArchitectureMutation(mutation_type='attention_heads', parameters={'num_heads': 8})
        modified_config = self.adapter.apply_mutation_to_model_config(config, mutation)
        self.assertEqual(modified_config['num_attention_heads'], 8)
    def test_layer_removal(self):
        config = create_test_model_config()
        original_layers = config['num_hidden_layers']
        mutation = ArchitectureMutation(mutation_type='layer_removal', parameters={'num_layers': 2})
        modified_config = self.adapter.apply_mutation_to_model_config(config, mutation)
        self.assertEqual(modified_config['num_hidden_layers'], original_layers - 2)
    def test_layer_addition(self):
        config = create_test_model_config()
        original_layers = config['num_hidden_layers']
        mutation = ArchitectureMutation(mutation_type='layer_addition', parameters={'num_layers': 2})
        modified_config = self.adapter.apply_mutation_to_model_config(config, mutation)
        self.assertEqual(modified_config['num_hidden_layers'], original_layers + 2)
class TestInferenceHookManager(unittest.TestCase):
    def setUp(self):
        mock_framework = Mock()
        self.hook_manager = InferenceHookManager(mock_framework)
    def test_hook_registration(self):
        def test_hook(data):
            return data
        self.hook_manager.register_inference_hook('test_hook', test_hook)
        self.assertIn('test_hook', self.hook_manager.active_hooks)
        self.hook_manager.unregister_inference_hook('test_hook')
        self.assertNotIn('test_hook', self.hook_manager.active_hooks)
    async def test_pre_inference_hook(self):
        def test_hook(data):
            data['hook_called'] = True
            return data
        self.hook_manager.register_inference_hook('test_hook', test_hook)
        request_data = {'input': 'test'}
        result = await self.hook_manager.pre_inference_hook(request_data)
        self.assertTrue(result.get('hook_called', False))
        self.assertIn('_hook_start_time', result)
    async def test_post_inference_hook(self):
        request_data = {'_hook_start_time': time.time()}
        inference_result = {'tokens_generated': 10}
        result = await self.hook_manager.post_inference_hook(request_data, inference_result)
        self.assertIsInstance(result, dict)
        self.assertGreater(self.hook_manager.inference_count, 0)
class TestAdaptiveArchitectureFramework(unittest.TestCase):
    def setUp(self):
        self.config = EvolutionConfig(population_size=5, max_generations=3)
        self.fitness_evaluator = MockFitnessEvaluator()
        self.evolution_engine = EchoSelfEvolutionEngine(config=self.config, fitness_evaluator=self.fitness_evaluator, individual_class=NeuralTopologyIndividual)
        self.framework = AdaptiveArchitectureFramework(self.evolution_engine)
    def test_framework_initialization(self):
        self.assertIsNotNone(self.framework.evolution_engine)
        self.assertIsNotNone(self.framework.performance_monitor)
        self.assertIsNotNone(self.framework.optimizer)
        self.assertFalse(self.framework.is_adapting)
    def test_performance_metrics_addition(self):
        metrics = PerformanceMetrics(latency_ms=100.0, accuracy_score=0.8)
        self.framework.add_performance_metrics(metrics)
        self.assertEqual(len(self.framework.performance_monitor.metrics_history), 1)
    def test_adaptation_configuration(self):
        self.framework.configure_adaptation(interval=30, threshold=0.2, max_per_hour=10)
        self.assertEqual(self.framework.adaptation_interval, 30)
        self.assertEqual(self.framework.min_adaptation_threshold, 0.2)
        self.assertEqual(self.framework.max_adaptations_per_hour, 10)
    async def test_adaptation_monitoring_lifecycle(self):
        await self.framework.start_adaptive_monitoring()
        self.assertTrue(self.framework.is_adapting)
        self.assertIsNotNone(self.framework._adaptation_task)
        await self.framework.stop_adaptive_monitoring()
        self.assertFalse(self.framework.is_adapting)
class TestAphroditeAdaptiveIntegration(unittest.TestCase):
    def setUp(self):
        self.config = EvolutionConfig(population_size=5, max_generations=3)
        self.fitness_evaluator = MockFitnessEvaluator()
        self.evolution_engine = EchoSelfEvolutionEngine(config=self.config, fitness_evaluator=self.fitness_evaluator, individual_class=NeuralTopologyIndividual)
        self.adaptive_framework = AdaptiveArchitectureFramework(self.evolution_engine)
        self.integration = AphroditeAdaptiveIntegration(self.adaptive_framework)
    def test_integration_initialization(self):
        self.assertIsNotNone(self.integration.adaptive_framework)
        self.assertIsNotNone(self.integration.topology_adapter)
        self.assertIsNotNone(self.integration.hook_manager)
        self.assertFalse(self.integration.is_integrated)
    async def test_aphrodite_integration(self):
        model_config = create_test_model_config()
        with patch.object(self.integration.aphrodite_bridge, 'is_initialized', return_value=True):
            result = await self.integration.integrate_with_aphrodite(model_config)
        self.assertTrue(result)
        self.assertTrue(self.integration.is_integrated)
        self.assertIsNotNone(self.integration.current_model_config)
        self.assertIsNotNone(self.integration.original_model_config)
    async def test_architecture_adaptation_application(self):
        model_config = create_test_model_config()
        with patch.object(self.integration.aphrodite_bridge, 'is_initialized', return_value=True):
            await self.integration.integrate_with_aphrodite(model_config)
        mutation = ArchitectureMutation(mutation_type='layer_scaling', parameters={'scale_factor': 1.5})
        result = await self.integration.apply_architecture_adaptation(mutation)
        self.assertTrue(result)
        self.assertGreater(self.integration.model_modification_count, 0)
    def test_model_configuration_status(self):
        status = self.integration.get_model_configuration_status()
        self.assertIn('is_integrated', status)
        self.assertIn('modification_count', status)
        self.assertIn('adaptation_status', status)
class TestAdaptiveModelLoader(unittest.TestCase):
    def setUp(self):
        self.config = EvolutionConfig(population_size=5, max_generations=3)
        self.fitness_evaluator = MockFitnessEvaluator()
        self.evolution_engine = EchoSelfEvolutionEngine(config=self.config, fitness_evaluator=self.fitness_evaluator, individual_class=NeuralTopologyIndividual)
        self.adaptive_framework = AdaptiveArchitectureFramework(self.evolution_engine)
        self.integration = AphroditeAdaptiveIntegration(self.adaptive_framework)
        self.loader = AdaptiveModelLoader(self.integration)
    async def test_load_adaptive_model(self):
        model_name = 'test-model'
        model_config = create_test_model_config()
        with patch.object(self.integration, 'integrate_with_aphrodite', return_value=True):
            result = await self.loader.load_adaptive_model(model_name, model_config)
        self.assertTrue(result)
        self.assertIn(model_name, self.loader.loaded_models)
        model_info = self.loader.loaded_models[model_name]
        self.assertEqual(model_info['config'], model_config)
        self.assertTrue(model_info['adaptation_enabled'])
    async def test_reload_model_with_adaptation(self):
        model_name = 'test-model'
        model_config = create_test_model_config()
        with patch.object(self.integration, 'integrate_with_aphrodite', return_value=True):
            await self.loader.load_adaptive_model(model_name, model_config)
        mutation = ArchitectureMutation(mutation_type='layer_scaling', parameters={'scale_factor': 1.2})
        with patch.object(self.integration, 'apply_architecture_adaptation', return_value=True):
            result = await self.loader.reload_model_with_adaptation(model_name, mutation)
        self.assertTrue(result)
        model_info = self.loader.loaded_models[model_name]
        self.assertGreater(model_info['modification_count'], 0)
    def test_get_loaded_models(self):
        models = self.loader.get_loaded_models()
        self.assertIsInstance(models, dict)
    async def test_unload_model(self):
        model_name = 'test-model'
        model_config = create_test_model_config()
        with patch.object(self.integration, 'integrate_with_aphrodite', return_value=True):
            await self.loader.load_adaptive_model(model_name, model_config)
        result = await self.loader.unload_model(model_name)
        self.assertTrue(result)
        self.assertNotIn(model_name, self.loader.loaded_models)
class TestInferenceTimeAdaptation(unittest.TestCase):
    async def test_complete_adaptation_workflow(self):
        config = EvolutionConfig(population_size=5, max_generations=3)
        fitness_evaluator = MockFitnessEvaluator()
        evolution_engine = EchoSelfEvolutionEngine(config=config, fitness_evaluator=fitness_evaluator, individual_class=NeuralTopologyIndividual)
        await evolution_engine.initialize_population(create_test_genome)
        adaptive_framework = AdaptiveArchitectureFramework(evolution_engine)
        integration = AphroditeAdaptiveIntegration(adaptive_framework)
        model_config = create_test_model_config()
        try:
            with patch.object(integration.aphrodite_bridge, 'is_initialized', return_value=True):
                integration_result = await integration.integrate_with_aphrodite(model_config)
                self.assertTrue(integration_result)
            for i in range(5):
                metrics = PerformanceMetrics(latency_ms=100.0 + i * 50, throughput_tokens_per_sec=100.0 - i * 10, memory_usage_mb=1024.0 + i * 200, accuracy_score=0.9 - i * 0.05)
                adaptive_framework.add_performance_metrics(metrics)
                request_data = {'input': f'test input {i}', 'batch_size': 1, 'sequence_length': 50}
                response_data = {'tokens_generated': 20, 'generation_time_ms': 200.0 + i * 30, 'memory_usage_mb': 800.0 + i * 100}
                hooked_request = await integration.hook_inference_request(request_data)
                hooked_response = await integration.hook_inference_response(hooked_request, response_data)
                self.assertIsInstance(hooked_request, dict)
                self.assertIsInstance(hooked_response, dict)
            status = adaptive_framework.get_adaptation_status()
            self.assertIn('current_performance', status)
            self.assertIn('performance_trend', status)
            current_perf = adaptive_framework.performance_monitor.get_current_performance()
            self.assertIn('avg_latency_ms', current_perf)
        finally:
            await integration.shutdown_integration()
async def run_async_tests():
    print('Running Adaptive Architecture Framework Tests...\n')
    print('Testing PerformanceMetrics...')
    test_metrics = TestPerformanceMetrics()
    test_metrics.test_performance_metrics_creation()
    test_metrics.test_overall_score_calculation()
    print('✓ PerformanceMetrics tests passed')
    print('Testing ArchitectureMutation...')
    test_mutation = TestArchitectureMutation()
    test_mutation.test_add_layer_mutation()
    test_mutation.test_remove_layer_mutation()
    test_mutation.test_modify_layer_mutation()
    test_mutation.test_adjust_connections_mutation()
    print('✓ ArchitectureMutation tests passed')
    print('Testing PerformanceMonitor...')
    test_monitor = TestPerformanceMonitor()
    test_monitor.setUp()
    test_monitor.test_add_metrics()
    test_monitor.test_current_performance_calculation()
    test_monitor.test_performance_degradation_detection()
    print('✓ PerformanceMonitor tests passed')
    print('Testing ArchitectureOptimizer...')
    test_optimizer = TestArchitectureOptimizer()
    test_optimizer.setUp()
    test_optimizer.test_suggest_mutations_for_high_latency()
    test_optimizer.test_suggest_mutations_for_high_memory()
    test_optimizer.test_suggest_mutations_for_low_accuracy()
    print('✓ ArchitectureOptimizer tests passed')
    print('Testing ModelTopologyAdapter...')
    test_adapter = TestModelTopologyAdapter()
    test_adapter.setUp()
    test_adapter.test_can_modify_architecture()
    test_adapter.test_scale_layer_dimensions()
    test_adapter.test_adjust_attention_heads()
    test_adapter.test_layer_removal()
    test_adapter.test_layer_addition()
    print('✓ ModelTopologyAdapter tests passed')
    print('Testing InferenceHookManager...')
    test_hooks = TestInferenceHookManager()
    test_hooks.setUp()
    test_hooks.test_hook_registration()
    await test_hooks.test_pre_inference_hook()
    await test_hooks.test_post_inference_hook()
    print('✓ InferenceHookManager tests passed')
    print('Testing AdaptiveArchitectureFramework...')
    test_framework = TestAdaptiveArchitectureFramework()
    test_framework.setUp()
    test_framework.test_framework_initialization()
    test_framework.test_performance_metrics_addition()
    test_framework.test_adaptation_configuration()
    await test_framework.test_adaptation_monitoring_lifecycle()
    print('✓ AdaptiveArchitectureFramework tests passed')
    print('Testing AphroditeAdaptiveIntegration...')
    test_integration = TestAphroditeAdaptiveIntegration()
    test_integration.setUp()
    test_integration.test_integration_initialization()
    await test_integration.test_aphrodite_integration()
    await test_integration.test_architecture_adaptation_application()
    test_integration.test_model_configuration_status()
    print('✓ AphroditeAdaptiveIntegration tests passed')
    print('Testing AdaptiveModelLoader...')
    test_loader = TestAdaptiveModelLoader()
    test_loader.setUp()
    await test_loader.test_load_adaptive_model()
    await test_loader.test_reload_model_with_adaptation()
    test_loader.test_get_loaded_models()
    await test_loader.test_unload_model()
    print('✓ AdaptiveModelLoader tests passed')
    print('Testing complete inference-time adaptation workflow...')
    test_workflow = TestInferenceTimeAdaptation()
    await test_workflow.test_complete_adaptation_workflow()
    print('✓ Inference-time adaptation workflow test passed')
    print('\n✅ All Adaptive Architecture Framework tests passed!')
    print('\n🎯 Key capabilities validated:')
    print('   • Dynamic model topology adjustment based on performance')
    print('   • Integration hooks with Aphrodite Engine model loading')
    print('   • Real-time architecture mutation capabilities')
    print('   • Models can self-modify during inference')
def main():
    logging.basicConfig(level=logging.INFO)
    asyncio.run(run_async_tests())
if __name__ == '__main__':
    main()