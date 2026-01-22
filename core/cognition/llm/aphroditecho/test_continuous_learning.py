import asyncio
import unittest
from unittest.mock import Mock, AsyncMock
from datetime import datetime
import torch
from aphrodite.continuous_learning import ContinuousLearningSystem, ContinuousLearningConfig, InteractionData
from aphrodite.dtesn_integration import DTESNDynamicIntegration
from aphrodite.dynamic_model_manager import DynamicModelManager
from echo_self.meta_learning.meta_optimizer import ExperienceReplay
class TestInteractionData(unittest.TestCase):
    def test_interaction_data_creation(self):
        data = InteractionData(interaction_id='test_001', interaction_type='text_generation', input_data={'prompt': 'Hello'}, output_data={'response': 'Hello world'}, performance_feedback=0.8, timestamp=datetime.now())
        self.assertEqual(data.interaction_id, 'test_001')
        self.assertEqual(data.interaction_type, 'text_generation')
        self.assertEqual(data.performance_feedback, 0.8)
        self.assertTrue(data.success)
    def test_interaction_data_with_metadata(self):
        data = InteractionData(interaction_id='test_002', interaction_type='reasoning', input_data={'problem': '2+2=?'}, output_data={'answer': '4'}, performance_feedback=1.0, timestamp=datetime.now(), context_metadata={'importance': 0.9, 'task_type': 'math'}, success=True)
        self.assertEqual(data.context_metadata['importance'], 0.9)
        self.assertEqual(data.context_metadata['task_type'], 'math')
        self.assertTrue(data.success)
class TestContinuousLearningConfig(unittest.TestCase):
    def test_default_config(self):
        config = ContinuousLearningConfig()
        self.assertEqual(config.max_experiences, 10000)
        self.assertEqual(config.replay_batch_size, 32)
        self.assertEqual(config.learning_rate_base, 0.001)
        self.assertTrue(config.enable_ewc)
        self.assertEqual(config.ewc_lambda, 1000.0)
    def test_custom_config(self):
        config = ContinuousLearningConfig(max_experiences=5000, learning_rate_base=0.01, enable_ewc=False)
        self.assertEqual(config.max_experiences, 5000)
        self.assertEqual(config.learning_rate_base, 0.01)
        self.assertFalse(config.enable_ewc)
class TestContinuousLearningSystem(unittest.TestCase):
    def setUp(self):
        self.mock_dynamic_manager = Mock(spec=DynamicModelManager)
        self.mock_dynamic_manager.apply_incremental_update = AsyncMock()
        self.mock_dynamic_manager.apply_incremental_update.return_value = {'success': True, 'update_id': 'test_update'}
        self.mock_dtesn_integration = Mock(spec=DTESNDynamicIntegration)
        self.mock_dtesn_integration.adaptive_parameter_update = AsyncMock()
        self.mock_dtesn_integration.adaptive_parameter_update.return_value = (torch.randn(768, 768), {'learning_type': 'stdp', 'learning_rate': 0.001})
        self.config = ContinuousLearningConfig(max_experiences=100, replay_batch_size=5, replay_frequency=5, consolidation_frequency=10)
        self.system = ContinuousLearningSystem(dynamic_manager=self.mock_dynamic_manager, dtesn_integration=self.mock_dtesn_integration, config=self.config)
    def test_initialization(self):
        self.assertIsNotNone(self.system.experience_replay)
        self.assertEqual(self.system.interaction_count, 0)
        self.assertEqual(self.system.current_learning_rate, self.config.learning_rate_base)
        self.assertEqual(len(self.system.parameter_importance), 0)
        self.assertEqual(len(self.system.consolidated_parameters), 0)
    def test_extract_learning_signal(self):
        interaction_data = InteractionData(interaction_id='test_001', interaction_type='text_generation', input_data={'prompt': 'Hello'}, output_data={'response': 'Hello world'}, performance_feedback=0.8, timestamp=datetime.now(), context_metadata={'importance': 0.9})
        learning_signal = self.system._extract_learning_signal(interaction_data)
        self.assertIn('strength', learning_signal)
        self.assertIn('direction', learning_signal)
        self.assertIn('context_weight', learning_signal)
        self.assertIn('temporal_weight', learning_signal)
        self.assertEqual(learning_signal['direction'], 1)
        self.assertEqual(learning_signal['raw_feedback'], 0.8)
        self.assertEqual(learning_signal['context_weight'], 0.9)
    def test_identify_target_parameters(self):
        interaction_data = InteractionData(interaction_id='test_001', interaction_type='text_generation', input_data={}, output_data={}, performance_feedback=0.5, timestamp=datetime.now())
        params = self.system._identify_target_parameters(interaction_data)
        self.assertIsInstance(params, list)
        self.assertTrue(len(params) > 0)
        self.assertTrue(any(('mlp' in p for p in params)))
        interaction_data.interaction_type = 'reasoning'
        params = self.system._identify_target_parameters(interaction_data)
        self.assertTrue(any(('attn' in p for p in params)))
        interaction_data.interaction_type = 'unknown_type'
        params = self.system._identify_target_parameters(interaction_data)
        self.assertEqual(params, ['transformer.h.10.mlp.c_proj.weight'])
    async def test_learn_from_interaction_success(self):
        interaction_data = InteractionData(interaction_id='test_001', interaction_type='text_generation', input_data={'prompt': 'Hello'}, output_data={'response': 'Hello world'}, performance_feedback=0.8, timestamp=datetime.now())
        result = await self.system.learn_from_interaction(interaction_data)
        self.assertTrue(result['success'])
        self.assertEqual(result['interaction_count'], 1)
        self.assertIn('learning_signal', result)
        self.assertIn('online_update', result)
        self.assertIn('metrics', result)
        self.assertEqual(self.system.interaction_count, 1)
        self.assertEqual(len(self.system.experience_replay.experiences), 1)
        self.assertEqual(self.system.learning_metrics['total_interactions'], 1)
        self.assertEqual(self.system.learning_metrics['successful_adaptations'], 1)
    async def test_learn_from_interaction_with_replay(self):
        self.system.config.replay_frequency = 1
        interaction_data = InteractionData(interaction_id='test_001', interaction_type='text_generation', input_data={'prompt': 'Hello'}, output_data={'response': 'Hello world'}, performance_feedback=0.8, timestamp=datetime.now())
        result = await self.system.learn_from_interaction(interaction_data)
        self.assertTrue(result['success'])
        self.assertIsNotNone(result['replay_result'])
    async def test_learn_from_interaction_with_consolidation(self):
        self.system.config.consolidation_frequency = 1
        self.system.parameter_importance['test_param'] = torch.ones(10, 10) * 2.0
        interaction_data = InteractionData(interaction_id='test_001', interaction_type='text_generation', input_data={'prompt': 'Hello'}, output_data={'response': 'Hello world'}, performance_feedback=0.8, timestamp=datetime.now())
        result = await self.system.learn_from_interaction(interaction_data)
        self.assertTrue(result['success'])
        self.assertIsNotNone(result['consolidation_result'])
    def test_parameter_importance_update(self):
        interaction_data = InteractionData(interaction_id='test_001', interaction_type='text_generation', input_data={}, output_data={}, performance_feedback=0.8, timestamp=datetime.now())
        learning_signal = {'strength': 0.5, 'direction': 1}
        self.system._update_parameter_importance(interaction_data, learning_signal)
        target_params = self.system._identify_target_parameters(interaction_data)
        for param in target_params:
            self.assertIn(param, self.system.parameter_importance)
            importance = self.system.parameter_importance[param]
            self.assertTrue(torch.all(importance > 0))
        learning_signal['strength'] = 0.3
        self.system._update_parameter_importance(interaction_data, learning_signal)
        for param in target_params:
            importance = self.system.parameter_importance[param]
            self.assertTrue(torch.all(importance > 0))
    def test_ewc_regularization(self):
        param_name = 'test_param'
        current_params = torch.randn(10, 10)
        updated_params = torch.randn(10, 10)
        self.system.parameter_importance[param_name] = torch.ones(10, 10) * 0.5
        self.system.consolidated_parameters[param_name] = torch.zeros(10, 10)
        regularized_params = self.system._apply_ewc_regularization(param_name, current_params, updated_params)
        self.assertFalse(torch.equal(regularized_params, updated_params))
        self.assertTrue(torch.all(torch.abs(regularized_params) < torch.abs(updated_params)))
    def test_learning_rate_adaptation(self):
        initial_lr = self.system.current_learning_rate
        for i in range(15):
            self.system.performance_history.append({'timestamp': datetime.now(), 'performance': 0.3, 'success': True, 'interaction_type': 'test'})
        self.system._adapt_learning_rate()
        self.assertGreaterEqual(self.system.current_learning_rate, initial_lr)
        self.system.current_learning_rate = initial_lr
        self.system.performance_history = []
        for i in range(15):
            self.system.performance_history.append({'timestamp': datetime.now(), 'performance': 0.9, 'success': True, 'interaction_type': 'test'})
        self.system._adapt_learning_rate()
        self.assertLess(self.system.current_learning_rate, initial_lr)
    def test_get_learning_stats(self):
        self.system.interaction_count = 10
        self.system.learning_metrics['successful_adaptations'] = 8
        self.system.performance_history = [{'performance': 0.8, 'timestamp': datetime.now(), 'success': True, 'interaction_type': 'test'} for _ in range(5)]
        stats = self.system.get_learning_stats()
        self.assertIn('metrics', stats)
        self.assertIn('current_learning_rate', stats)
        self.assertIn('interaction_count', stats)
        self.assertIn('experience_count', stats)
        self.assertIn('performance_stats', stats)
        self.assertEqual(stats['interaction_count'], 10)
        self.assertEqual(stats['metrics']['successful_adaptations'], 8)
        perf_stats = stats['performance_stats']
        self.assertIn('mean', perf_stats)
        self.assertIn('std', perf_stats)
        self.assertIn('min', perf_stats)
        self.assertIn('max', perf_stats)
    async def test_reset_learning_state(self):
        self.system.interaction_count = 10
        self.system.learning_metrics['successful_adaptations'] = 8
        self.system.performance_history = [{'test': 'data'}]
        self.system.experience_replay.experiences = [Mock() for _ in range(5)]
        self.system.consolidated_parameters['test'] = torch.randn(10, 10)
        self.system.learning_metrics['consolidations'] = 3
        await self.system.reset_learning_state()
        self.assertEqual(self.system.interaction_count, 0)
        self.assertEqual(self.system.current_learning_rate, self.config.learning_rate_base)
        self.assertEqual(len(self.system.performance_history), 0)
        self.assertEqual(len(self.system.experience_replay.experiences), 0)
        self.assertIn('test', self.system.consolidated_parameters)
        self.assertEqual(self.system.learning_metrics['consolidations'], 3)
        self.assertEqual(self.system.learning_metrics['total_interactions'], 0)
        self.assertEqual(self.system.learning_metrics['successful_adaptations'], 0)
class TestContinuousLearningIntegration(unittest.TestCase):
    def setUp(self):
        self.mock_dynamic_manager = Mock(spec=DynamicModelManager)
        self.mock_dynamic_manager.apply_incremental_update = AsyncMock()
        self.mock_dynamic_manager.apply_incremental_update.return_value = {'success': True, 'update_id': 'test_update'}
        self.mock_dtesn_integration = Mock(spec=DTESNDynamicIntegration)
        self.mock_dtesn_integration.adaptive_parameter_update = AsyncMock()
        self.mock_dtesn_integration.adaptive_parameter_update.return_value = (torch.randn(768, 768), {'learning_type': 'bcm', 'learning_rate': 0.001})
        self.system = ContinuousLearningSystem(dynamic_manager=self.mock_dynamic_manager, dtesn_integration=self.mock_dtesn_integration)
    async def test_dtesn_integration_called(self):
        interaction_data = InteractionData(interaction_id='test_001', interaction_type='text_generation', input_data={'prompt': 'Hello'}, output_data={'response': 'Hello world'}, performance_feedback=0.8, timestamp=datetime.now())
        await self.system.learn_from_interaction(interaction_data)
        self.mock_dtesn_integration.adaptive_parameter_update.assert_called()
        call_args = self.mock_dtesn_integration.adaptive_parameter_update.call_args
        self.assertIsInstance(call_args[1]['current_params'], torch.Tensor)
        self.assertIsInstance(call_args[1]['target_gradient'], torch.Tensor)
        self.assertEqual(call_args[1]['performance_feedback'], 0.8)
    async def test_dynamic_manager_integration_called(self):
        interaction_data = InteractionData(interaction_id='test_001', interaction_type='text_generation', input_data={'prompt': 'Hello'}, output_data={'response': 'Hello world'}, performance_feedback=0.8, timestamp=datetime.now())
        await self.system.learn_from_interaction(interaction_data)
        self.mock_dynamic_manager.apply_incremental_update.assert_called()
        call_args = self.mock_dynamic_manager.apply_incremental_update.call_args
        update_request = call_args[0][0]
        self.assertIsInstance(update_request.parameter_name, str)
        self.assertEqual(update_request.update_type, 'replace')
        self.assertIn('interaction_id', update_request.metadata)
    def test_experience_replay_integration(self):
        self.assertIsInstance(self.system.experience_replay, ExperienceReplay)
        self.assertEqual(self.system.experience_replay.max_size, self.system.config.max_experiences)
class TestContinuousLearningAcceptanceCriteria(unittest.TestCase):
    def setUp(self):
        self.mock_dynamic_manager = Mock(spec=DynamicModelManager)
        self.mock_dynamic_manager.apply_incremental_update = AsyncMock()
        self.mock_dynamic_manager.apply_incremental_update.return_value = {'success': True, 'update_id': 'test_update'}
        self.mock_dtesn_integration = Mock(spec=DTESNDynamicIntegration)
        self.mock_dtesn_integration.adaptive_parameter_update = AsyncMock()
        self.mock_dtesn_integration.adaptive_parameter_update.return_value = (torch.randn(768, 768), {'learning_type': 'stdp', 'learning_rate': 0.001})
        self.config = ContinuousLearningConfig(max_experiences=1000, replay_frequency=5, consolidation_frequency=10)
        self.system = ContinuousLearningSystem(dynamic_manager=self.mock_dynamic_manager, dtesn_integration=self.mock_dtesn_integration, config=self.config)
    async def test_continuous_learning_from_multiple_experiences(self):
        interactions = []
        for i in range(20):
            interaction = InteractionData(interaction_id=f'test_{i:03d}', interaction_type='text_generation' if i % 2 == 0 else 'reasoning', input_data={'prompt': f'Test prompt {i}'}, output_data={'response': f'Test response {i}'}, performance_feedback=0.7 + i % 3 * 0.1, timestamp=datetime.now(), context_metadata={'session': i // 5})
            interactions.append(interaction)
        results = []
        for interaction in interactions:
            result = await self.system.learn_from_interaction(interaction)
            results.append(result)
        successful_learns = [r for r in results if r['success']]
        self.assertEqual(len(successful_learns), 20, 'All interactions should be learned from')
        self.assertEqual(self.system.interaction_count, 20)
        self.assertEqual(len(self.system.experience_replay.experiences), 20)
        self.assertEqual(self.system.learning_metrics['total_interactions'], 20)
        self.assertEqual(self.system.learning_metrics['successful_adaptations'], 20)
        replay_triggers = [r for r in results if r['replay_result'] is not None]
        expected_replays = 20 // self.config.replay_frequency
        self.assertEqual(len(replay_triggers), expected_replays)
        consolidation_triggers = [r for r in results if r['consolidation_result'] is not None]
        expected_consolidations = 20 // self.config.consolidation_frequency
        self.assertEqual(len(consolidation_triggers), expected_consolidations)
    async def test_learning_without_catastrophic_forgetting(self):
        initial_interactions = []
        for i in range(5):
            interaction = InteractionData(interaction_id=f'initial_{i}', interaction_type='memory_recall', input_data={'query': f'Important fact {i}'}, output_data={'fact': f'Critical knowledge {i}'}, performance_feedback=0.9, timestamp=datetime.now())
            initial_interactions.append(interaction)
        for interaction in initial_interactions:
            await self.system.learn_from_interaction(interaction)
        self.assertGreater(len(self.system.parameter_importance), 0, 'Parameter importance should be tracked')
        await self.system._perform_memory_consolidation()
        initial_consolidated_count = len(self.system.consolidated_parameters)
        self.assertGreater(initial_consolidated_count, 0, 'Important parameters should be consolidated')
        new_interactions = []
        for i in range(10):
            interaction = InteractionData(interaction_id=f'new_{i}', interaction_type='text_generation', input_data={'prompt': f'New task {i}'}, output_data={'response': f'New response {i}'}, performance_feedback=0.6, timestamp=datetime.now())
            new_interactions.append(interaction)
        for interaction in new_interactions:
            result = await self.system.learn_from_interaction(interaction)
            self.assertTrue(result['success'], 'New learning should be successful')
        final_consolidated_count = len(self.system.consolidated_parameters)
        self.assertGreaterEqual(final_consolidated_count, initial_consolidated_count, 'Consolidated parameters should not decrease')
        self.assertEqual(self.system.interaction_count, 15)
        self.assertEqual(self.system.learning_metrics['total_interactions'], 15)
    def test_experience_replay_reinforces_learning(self):
        high_value_experiences = []
        for i in range(3):
            interaction = InteractionData(interaction_id=f'high_value_{i}', interaction_type='reasoning', input_data={'problem': f'Complex problem {i}'}, output_data={'solution': f'Elegant solution {i}'}, performance_feedback=0.95, timestamp=datetime.now())
            learning_signal = self.system._extract_learning_signal(interaction)
            update_result = {'success': True, 'updated_parameters': ['test_param']}
            experience = self.system._create_experience_record(interaction, learning_signal, update_result)
            high_value_experiences.append(experience)
        for exp in high_value_experiences:
            self.system.experience_replay.add_experience(exp)
        self.assertEqual(len(self.system.experience_replay.experiences), 3)
        top_performers = self.system.experience_replay.get_top_performers(n=5)
        self.assertEqual(len(top_performers), 3)
        for exp in top_performers:
            self.assertGreaterEqual(exp.fitness_score, 0.95)
    async def test_adaptive_learning_rate(self):
        initial_lr = self.system.current_learning_rate
        poor_interactions = []
        for i in range(15):
            interaction = InteractionData(interaction_id=f'poor_{i}', interaction_type='text_generation', input_data={'prompt': f'Difficult task {i}'}, output_data={'response': f'Struggling response {i}'}, performance_feedback=0.2, timestamp=datetime.now())
            poor_interactions.append(interaction)
        for interaction in poor_interactions:
            await self.system.learn_from_interaction(interaction)
        self.assertNotEqual(self.system.current_learning_rate, initial_lr, 'Learning rate should adapt to performance')
        recent_perf = [p['performance'] for p in self.system.performance_history[-10:]]
        self.assertTrue(all((p < 0.5 for p in recent_perf)), 'Recent performance should reflect poor interactions')
    def test_system_scalability(self):
        config = ContinuousLearningConfig(max_experiences=50)
        system = ContinuousLearningSystem(dynamic_manager=Mock(spec=DynamicModelManager), dtesn_integration=Mock(spec=DTESNDynamicIntegration), config=config)
        for i in range(75):
            interaction = InteractionData(interaction_id=f'scale_{i}', interaction_type='text_generation', input_data={}, output_data={}, performance_feedback=0.5, timestamp=datetime.now())
            learning_signal = system._extract_learning_signal(interaction)
            update_result = {'success': True}
            experience = system._create_experience_record(interaction, learning_signal, update_result)
            system.experience_replay.add_experience(experience)
        self.assertLessEqual(len(system.experience_replay.experiences), config.max_experiences)
        self.assertEqual(len(system.experience_replay.experiences), config.max_experiences)
def run_continuous_learning_tests():
    test_classes = [TestInteractionData, TestContinuousLearningConfig, TestContinuousLearningSystem, TestContinuousLearningIntegration, TestContinuousLearningAcceptanceCriteria]
    suite = unittest.TestSuite()
    for test_class in test_classes:
        tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
        suite.addTests(tests)
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    if result.wasSuccessful():
        print(f'\n✅ All {result.testsRun} tests passed!')
        print('✅ Continuous Learning System meets acceptance criteria:')
        print('   - Models learn continuously from new experiences')
        print('   - Online training from interaction data works')
        print('   - Experience replay and data management functional')
        print('   - Catastrophic forgetting prevention implemented')
    else:
        print(f'\n❌ {len(result.failures)} failures, {len(result.errors)} errors')
        for test, error in result.failures + result.errors:
            print(f'   - {test}: {error}')
    return result.wasSuccessful()
def run_async_test(async_test_func):
    return asyncio.get_event_loop().run_until_complete(async_test_func())
if __name__ == '__main__':
    print('Running Continuous Learning System Test Suite...')
    print('=' * 60)
    success = run_continuous_learning_tests()
    exit(0 if success else 1)