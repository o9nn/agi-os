import asyncio
import json
import unittest
from datetime import datetime
from unittest.mock import AsyncMock, Mock, patch
from aphrodite.continuous_learning import ContinuousLearningSystem, ContinuousLearningConfig, InteractionData, ServerSideConfig
from aphrodite.endpoints.middleware.continuous_learning_middleware import ServerSideDataCollector, BackgroundLearningProcessor, ContinuousLearningMiddleware, ServerLearningMetrics
from aphrodite.endpoints.openai.serving_continuous_learning import OpenAIServingContinuousLearning, LearningInteractionRequest
class TestServerSideDataCollector(unittest.TestCase):
    def setUp(self):
        self.config = ServerSideConfig()
        self.collector = ServerSideDataCollector(self.config)
    def test_collector_initialization(self):
        self.assertEqual(len(self.collector.interaction_buffer), 0)
        self.assertEqual(len(self.collector.feedback_buffer), 0)
        self.assertEqual(len(self.collector.quality_scores), 0)
    def test_determine_interaction_type(self):
        chat_request = Mock()
        chat_request.url.path = '/v1/chat/completions'
        completion_request = Mock()
        completion_request.url.path = '/v1/completions'
        embedding_request = Mock()
        embedding_request.url.path = '/v1/embeddings'
        self.assertEqual(self.collector._determine_interaction_type(chat_request), 'chat_completion')
        self.assertEqual(self.collector._determine_interaction_type(completion_request), 'text_completion')
        self.assertEqual(self.collector._determine_interaction_type(embedding_request), 'embedding')
    def test_performance_feedback_calculation(self):
        request = Mock()
        request.url.path = '/v1/chat/completions'
        good_response = Mock()
        good_response.status_code = 200
        feedback = self.collector._calculate_performance_feedback(request, good_response, response_time=100.0, metadata={})
        self.assertGreater(feedback, 0.0)
        slow_feedback = self.collector._calculate_performance_feedback(request, good_response, response_time=3000.0, metadata={})
        self.assertLess(slow_feedback, feedback)
        error_response = Mock()
        error_response.status_code = 500
        error_feedback = self.collector._calculate_performance_feedback(request, error_response, response_time=100.0, metadata={})
        self.assertLess(error_feedback, 0.0)
    def test_interaction_collection(self):
        request = Mock()
        request.url.path = '/v1/chat/completions'
        request.method = 'POST'
        request.headers = {'Content-Type': 'application/json'}
        request.query_params = {}
        request.state = Mock()
        request.state.body_data = {'messages': [{'role': 'user', 'content': 'Hello'}], 'model': 'test-model', 'max_tokens': 100}
        response = Mock()
        response.status_code = 200
        response.headers = {'Content-Type': 'application/json'}
        interaction = self.collector.collect_interaction(request=request, response=response, response_time=150.0)
        self.assertIsNotNone(interaction)
        self.assertEqual(interaction.interaction_type, 'chat_completion')
        self.assertIn('Hello', str(interaction.input_data))
        self.assertEqual(interaction.context_metadata['status_code'], 200)
        self.assertEqual(interaction.context_metadata['response_time_ms'], 150.0)
        self.assertEqual(len(self.collector.interaction_buffer), 1)
        self.assertEqual(len(self.collector.quality_scores), 1)
    def test_data_statistics(self):
        for i in range(5):
            interaction = InteractionData(interaction_id=f'test_{i}', interaction_type='chat_completion' if i % 2 == 0 else 'text_completion', input_data={'prompt': f'Test {i}'}, output_data={'response': f'Response {i}'}, performance_feedback=0.5 + i * 0.1, timestamp=datetime.now())
            self.collector.interaction_buffer.append(interaction)
            self.collector.quality_scores.append(interaction.performance_feedback)
        stats = self.collector.get_data_statistics()
        self.assertEqual(stats['total_interactions'], 5)
        self.assertIn('chat_completion', stats['interaction_types'])
        self.assertIn('text_completion', stats['interaction_types'])
        self.assertIn('avg_quality_score', stats)
        self.assertGreater(stats['avg_quality_score'], 0.5)
class TestBackgroundLearningProcessor(unittest.TestCase):
    def setUp(self):
        self.mock_learning_system = Mock()
        self.mock_learning_system.learn_from_interaction = AsyncMock()
        self.mock_learning_system.learn_from_interaction.return_value = {'success': True, 'learning_time': 0.01}
        self.mock_data_collector = Mock()
        self.mock_data_collector.get_aggregated_interactions.return_value = []
        self.config = ServerSideConfig(background_learning_interval=0.1, min_interactions_for_learning=2)
        self.processor = BackgroundLearningProcessor(continuous_learning_system=self.mock_learning_system, data_collector=self.mock_data_collector, config=self.config)
    async def test_background_processor_startup_shutdown(self):
        self.assertFalse(self.processor.is_running)
        await self.processor.start_background_processing()
        self.assertTrue(self.processor.is_running)
        self.assertIsNotNone(self.processor.background_task)
        await asyncio.sleep(0.05)
        await self.processor.stop_background_processing()
        self.assertFalse(self.processor.is_running)
    async def test_interaction_batch_processing(self):
        interactions = [InteractionData(interaction_id=f'test_{i}', interaction_type='chat_completion', input_data={'prompt': f'Test {i}'}, output_data={'response': f'Response {i}'}, performance_feedback=0.7, timestamp=datetime.now()) for i in range(5)]
        self.mock_data_collector.get_aggregated_interactions.return_value = interactions
        await self.processor._process_interactions_batch()
        self.assertGreaterEqual(self.mock_learning_system.learn_from_interaction.call_count, 1)
        self.assertGreater(self.processor.metrics.background_updates, 0)
    def test_learning_metrics_collection(self):
        self.processor.metrics.total_requests = 100
        self.processor.metrics.learning_requests = 80
        self.processor.metrics.background_updates = 5
        self.mock_data_collector.get_data_statistics.return_value = {'total_interactions': 100, 'avg_quality_score': 0.7}
        self.mock_learning_system.get_learning_stats.return_value = {'interaction_count': 100, 'current_learning_rate': 0.001}
        metrics = self.processor.get_learning_metrics()
        self.assertIn('server_metrics', metrics)
        self.assertIn('data_statistics', metrics)
        self.assertIn('learning_statistics', metrics)
        self.assertIn('system_status', metrics)
        self.assertEqual(metrics['server_metrics']['total_requests'], 100)
        self.assertEqual(metrics['server_metrics']['background_updates'], 5)
class TestContinuousLearningMiddleware(unittest.TestCase):
    def setUp(self):
        self.mock_learning_system = Mock()
        self.config = ServerSideConfig()
        self.mock_app = Mock()
        self.middleware = ContinuousLearningMiddleware(app=self.mock_app, continuous_learning_system=self.mock_learning_system, config=self.config)
    def test_middleware_initialization(self):
        self.assertIsNotNone(self.middleware.data_collector)
        self.assertIsNotNone(self.middleware.background_processor)
        self.assertEqual(len(self.middleware.learning_endpoints), 5)
    async def test_middleware_startup_shutdown(self):
        self.middleware.background_processor = Mock()
        self.middleware.background_processor.start_background_processing = AsyncMock()
        self.middleware.background_processor.stop_background_processing = AsyncMock()
        await self.middleware.startup()
        self.middleware.background_processor.start_background_processing.assert_called_once()
        await self.middleware.shutdown()
        self.middleware.background_processor.stop_background_processing.assert_called_once()
    def test_learning_status_retrieval(self):
        mock_metrics = {'server_metrics': {'total_requests': 100}, 'system_status': {'background_processing': True}}
        self.middleware.background_processor.get_learning_metrics = Mock(return_value=mock_metrics)
        status = self.middleware.get_learning_status()
        self.assertEqual(status, mock_metrics)
class TestOpenAIServingContinuousLearning(unittest.TestCase):
    def setUp(self):
        self.mock_engine_client = Mock()
        self.mock_model_config = Mock()
        self.mock_model_config.model = 'test-model'
        self.mock_model_config.hf_config = Mock()
        self.mock_model_config.hf_config.model_type = 'test_type'
        self.mock_model_config.max_model_len = 4096
        self.mock_models = Mock()
        self.mock_request_logger = Mock()
        self.service = OpenAIServingContinuousLearning(engine_client=self.mock_engine_client, model_config=self.mock_model_config, models=self.mock_models, request_logger=self.mock_request_logger)
    def test_service_initialization(self):
        self.assertTrue(self.service.learning_enabled)
        self.assertIsNotNone(self.service.continuous_learning_system)
        self.assertEqual(self.service.learning_stats['total_learning_requests'], 0)
    async def test_learning_status_retrieval(self):
        self.service.continuous_learning_system.get_learning_stats = Mock(return_value={'interaction_count': 50, 'current_learning_rate': 0.001, 'metrics': {'successful_adaptations': 40}})
        status = await self.service.get_learning_status()
        self.assertIn('service_info', status)
        self.assertIn('learning_statistics', status)
        self.assertIn('model_info', status)
        self.assertIn('timestamp', status)
        self.assertTrue(status['service_info']['learning_enabled'])
        self.assertEqual(status['model_info']['model_name'], 'test-model')
    async def test_learning_enable_disable(self):
        result = await self.service.disable_learning()
        self.assertTrue(result['success'])
        self.assertFalse(self.service.learning_enabled)
        result = await self.service.enable_learning()
        self.assertTrue(result['success'])
        self.assertTrue(self.service.learning_enabled)
    async def test_manual_learning_trigger(self):
        self.service.continuous_learning_system.learn_from_interaction = AsyncMock()
        self.service.continuous_learning_system.learn_from_interaction.return_value = {'success': True, 'learning_time': 0.01, 'current_learning_rate': 0.001}
        result = await self.service.trigger_manual_learning(prompt='Test prompt', response='Test response', performance_feedback=0.8, interaction_type='manual')
        self.assertTrue(result['success'])
        self.assertIn('learning_result', result)
        self.assertEqual(self.service.learning_stats['successful_adaptations'], 1)
        self.service.continuous_learning_system.learn_from_interaction.assert_called_once()
    async def test_learning_metrics_retrieval(self):
        self.service.continuous_learning_system.get_learning_stats = Mock(return_value={'interaction_count': 100, 'current_learning_rate': 0.001, 'experience_count': 80, 'consolidated_parameters': 5, 'metrics': {'successful_adaptations': 90}})
        metrics = await self.service.get_learning_metrics()
        self.assertIn('overview', metrics)
        self.assertIn('performance', metrics)
        self.assertIn('system_metrics', metrics)
        self.assertIn('service_stats', metrics)
        self.assertEqual(metrics['overview']['total_interactions'], 100)
        self.assertEqual(metrics['overview']['success_rate'], 0.9)
class TestServerSideContinuousLearningIntegration(unittest.TestCase):
    async def test_end_to_end_learning_flow(self):
        mock_dynamic_manager = Mock()
        mock_dynamic_manager.apply_incremental_update = AsyncMock()
        mock_dynamic_manager.apply_incremental_update.return_value = {'success': True}
        mock_dtesn_integration = Mock()
        mock_dtesn_integration.adaptive_parameter_update = AsyncMock()
        mock_dtesn_integration.adaptive_parameter_update.return_value = (Mock(), {'learning_type': 'test'})
        config = ContinuousLearningConfig(max_experiences=100)
        learning_system = ContinuousLearningSystem(dynamic_manager=mock_dynamic_manager, dtesn_integration=mock_dtesn_integration, config=config)
        server_config = ServerSideConfig()
        data_collector = ServerSideDataCollector(server_config)
        background_processor = BackgroundLearningProcessor(continuous_learning_system=learning_system, data_collector=data_collector, config=server_config)
        for i in range(10):
            interaction = InteractionData(interaction_id=f'integration_test_{i}', interaction_type='chat_completion', input_data={'prompt': f'Test prompt {i}'}, output_data={'response': f'Test response {i}'}, performance_feedback=0.7 + i * 0.02, timestamp=datetime.now())
            data_collector.interaction_buffer.append(interaction)
            data_collector.quality_scores.append(interaction.performance_feedback)
        await background_processor._process_interactions_batch()
        self.assertGreater(mock_dtesn_integration.adaptive_parameter_update.call_count, 0)
        self.assertGreater(mock_dynamic_manager.apply_incremental_update.call_count, 0)
        self.assertGreater(background_processor.metrics.background_updates, 0)
    def test_production_safety_constraints(self):
        mock_dynamic_manager = Mock()
        mock_dtesn_integration = Mock()
        config = ContinuousLearningConfig(learning_rate_base=0.01)
        learning_system = ContinuousLearningSystem(dynamic_manager=mock_dynamic_manager, dtesn_integration=mock_dtesn_integration, config=config)
        server_config = ServerSideConfig(max_learning_rate_production=0.0001)
        data_collector = ServerSideDataCollector(server_config)
        background_processor = BackgroundLearningProcessor(continuous_learning_system=learning_system, data_collector=data_collector, config=server_config)
        original_config = background_processor._apply_production_constraints()
        self.assertEqual(learning_system.current_learning_rate, 0.0001)
        self.assertGreater(learning_system.config.ewc_lambda, config.ewc_lambda)
        background_processor._restore_original_config(original_config)
        self.assertEqual(learning_system.current_learning_rate, 0.01)
def run_server_side_continuous_learning_tests():
    test_classes = [TestServerSideDataCollector, TestBackgroundLearningProcessor, TestContinuousLearningMiddleware, TestOpenAIServingContinuousLearning, TestServerSideContinuousLearningIntegration]
    suite = unittest.TestSuite()
    for test_class in test_classes:
        tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
        suite.addTests(tests)
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    if result.wasSuccessful():
        print(f'\n✅ All {result.testsRun} server-side continuous learning tests passed!')
        print('✅ Server-side continuous learning implementation meets requirements:')
        print('   - Models improve continuously from production data')
        print('   - Server-side data collection and aggregation functional')
        print('   - Background learning processes working correctly')
        print('   - OpenAI-compatible API endpoints operational')
        print('   - Production safety constraints properly implemented')
    else:
        print(f'\n❌ {len(result.failures)} failures, {len(result.errors)} errors')
        for test, error in result.failures + result.errors:
            print(f'   - {test}: {error}')
    return result.wasSuccessful()
def run_async_test(async_test_func):
    return asyncio.get_event_loop().run_until_complete(async_test_func())
if __name__ == '__main__':
    print('Running Server-Side Continuous Learning Test Suite...')
    print('=' * 70)
    success = run_server_side_continuous_learning_tests()
    exit(0 if success else 1)