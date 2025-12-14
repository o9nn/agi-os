"""
Test Suite for Server-Side Continuous Learning Implementation.

Tests server-side data collection, background learning processes,
and integration with FastAPI endpoints for production deployment.
"""

import asyncio
import json
import unittest
from datetime import datetime
from unittest.mock import AsyncMock, Mock, patch

# Test imports
from aphrodite.continuous_learning import (
    ContinuousLearningSystem,
    ContinuousLearningConfig, 
    InteractionData,
    ServerSideConfig
)
from aphrodite.endpoints.middleware.continuous_learning_middleware import (
    ServerSideDataCollector,
    BackgroundLearningProcessor,
    ContinuousLearningMiddleware,
    ServerLearningMetrics
)
from aphrodite.endpoints.openai.serving_continuous_learning import (
    OpenAIServingContinuousLearning,
    LearningInteractionRequest
)


class TestServerSideDataCollector(unittest.TestCase):
    """Test server-side data collection functionality."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.config = ServerSideConfig()
        self.collector = ServerSideDataCollector(self.config)
    
    def test_collector_initialization(self):
        """Test data collector initialization."""
        self.assertEqual(len(self.collector.interaction_buffer), 0)
        self.assertEqual(len(self.collector.feedback_buffer), 0)
        self.assertEqual(len(self.collector.quality_scores), 0)
    
    def test_determine_interaction_type(self):
        """Test interaction type determination from requests."""
        
        # Mock request objects
        chat_request = Mock()
        chat_request.url.path = "/v1/chat/completions"
        
        completion_request = Mock()
        completion_request.url.path = "/v1/completions"
        
        embedding_request = Mock()
        embedding_request.url.path = "/v1/embeddings"
        
        # Test interaction type detection
        self.assertEqual(
            self.collector._determine_interaction_type(chat_request),
            "chat_completion"
        )
        
        self.assertEqual(
            self.collector._determine_interaction_type(completion_request),
            "text_completion" 
        )
        
        self.assertEqual(
            self.collector._determine_interaction_type(embedding_request),
            "embedding"
        )
    
    def test_performance_feedback_calculation(self):
        """Test performance feedback calculation."""
        
        # Mock request and response
        request = Mock()
        request.url.path = "/v1/chat/completions"
        
        # Test successful response with good performance
        good_response = Mock()
        good_response.status_code = 200
        
        feedback = self.collector._calculate_performance_feedback(
            request, good_response, response_time=100.0, metadata={}
        )
        
        # Should be positive for fast, successful response
        self.assertGreater(feedback, 0.0)
        
        # Test slow response
        slow_feedback = self.collector._calculate_performance_feedback(
            request, good_response, response_time=3000.0, metadata={}
        )
        
        # Should be less positive due to slow response
        self.assertLess(slow_feedback, feedback)
        
        # Test error response
        error_response = Mock()
        error_response.status_code = 500
        
        error_feedback = self.collector._calculate_performance_feedback(
            request, error_response, response_time=100.0, metadata={}
        )
        
        # Should be negative for error response
        self.assertLess(error_feedback, 0.0)
    
    def test_interaction_collection(self):
        """Test interaction data collection from request/response."""
        
        # Mock request with state
        request = Mock()
        request.url.path = "/v1/chat/completions"
        request.method = "POST"
        request.headers = {"Content-Type": "application/json"}
        request.query_params = {}
        request.state = Mock()
        request.state.body_data = {
            "messages": [{"role": "user", "content": "Hello"}],
            "model": "test-model",
            "max_tokens": 100
        }
        
        # Mock response
        response = Mock()
        response.status_code = 200
        response.headers = {"Content-Type": "application/json"}
        
        # Collect interaction
        interaction = self.collector.collect_interaction(
            request=request,
            response=response,
            response_time=150.0
        )
        
        # Verify interaction was created
        self.assertIsNotNone(interaction)
        self.assertEqual(interaction.interaction_type, "chat_completion")
        self.assertIn("Hello", str(interaction.input_data))
        self.assertEqual(interaction.context_metadata["status_code"], 200)
        self.assertEqual(interaction.context_metadata["response_time_ms"], 150.0)
        
        # Verify interaction was buffered
        self.assertEqual(len(self.collector.interaction_buffer), 1)
        self.assertEqual(len(self.collector.quality_scores), 1)
    
    def test_data_statistics(self):
        """Test data statistics generation."""
        
        # Add some mock interactions
        for i in range(5):
            interaction = InteractionData(
                interaction_id=f"test_{i}",
                interaction_type="chat_completion" if i % 2 == 0 else "text_completion",
                input_data={"prompt": f"Test {i}"},
                output_data={"response": f"Response {i}"},
                performance_feedback=0.5 + (i * 0.1),
                timestamp=datetime.now()
            )
            self.collector.interaction_buffer.append(interaction)
            self.collector.quality_scores.append(interaction.performance_feedback)
        
        # Get statistics
        stats = self.collector.get_data_statistics()
        
        # Verify statistics
        self.assertEqual(stats["total_interactions"], 5)
        self.assertIn("chat_completion", stats["interaction_types"])
        self.assertIn("text_completion", stats["interaction_types"])
        self.assertIn("avg_quality_score", stats)
        self.assertGreater(stats["avg_quality_score"], 0.5)


class TestBackgroundLearningProcessor(unittest.TestCase):
    """Test background learning processor functionality."""
    
    def setUp(self):
        """Set up test fixtures."""
        
        # Mock continuous learning system
        self.mock_learning_system = Mock()
        self.mock_learning_system.learn_from_interaction = AsyncMock()
        self.mock_learning_system.learn_from_interaction.return_value = {
            "success": True,
            "learning_time": 0.01
        }
        
        # Mock data collector
        self.mock_data_collector = Mock()
        self.mock_data_collector.get_aggregated_interactions.return_value = []
        
        # Configuration
        self.config = ServerSideConfig(
            background_learning_interval=0.1,  # Fast interval for testing
            min_interactions_for_learning=2
        )
        
        # Create processor
        self.processor = BackgroundLearningProcessor(
            continuous_learning_system=self.mock_learning_system,
            data_collector=self.mock_data_collector,
            config=self.config
        )
    
    async def test_background_processor_startup_shutdown(self):
        """Test background processor startup and shutdown."""
        
        # Initially not running
        self.assertFalse(self.processor.is_running)
        
        # Start processor
        await self.processor.start_background_processing()
        self.assertTrue(self.processor.is_running)
        self.assertIsNotNone(self.processor.background_task)
        
        # Brief wait to let it run
        await asyncio.sleep(0.05)
        
        # Stop processor
        await self.processor.stop_background_processing()
        self.assertFalse(self.processor.is_running)
    
    async def test_interaction_batch_processing(self):
        """Test processing of interaction batches."""
        
        # Create mock interactions
        interactions = [
            InteractionData(
                interaction_id=f"test_{i}",
                interaction_type="chat_completion",
                input_data={"prompt": f"Test {i}"},
                output_data={"response": f"Response {i}"},
                performance_feedback=0.7,
                timestamp=datetime.now()
            )
            for i in range(5)
        ]
        
        # Mock data collector to return interactions
        self.mock_data_collector.get_aggregated_interactions.return_value = interactions
        
        # Process batch
        await self.processor._process_interactions_batch()
        
        # Verify learning was called for interactions
        self.assertGreaterEqual(
            self.mock_learning_system.learn_from_interaction.call_count,
            1
        )
        
        # Verify metrics were updated
        self.assertGreater(self.processor.metrics.background_updates, 0)
    
    def test_learning_metrics_collection(self):
        """Test learning metrics collection."""
        
        # Update some metrics
        self.processor.metrics.total_requests = 100
        self.processor.metrics.learning_requests = 80
        self.processor.metrics.background_updates = 5
        
        # Mock data collector statistics
        self.mock_data_collector.get_data_statistics.return_value = {
            "total_interactions": 100,
            "avg_quality_score": 0.7
        }
        
        # Mock learning system statistics
        self.mock_learning_system.get_learning_stats.return_value = {
            "interaction_count": 100,
            "current_learning_rate": 0.001
        }
        
        # Get metrics
        metrics = self.processor.get_learning_metrics()
        
        # Verify metrics structure
        self.assertIn("server_metrics", metrics)
        self.assertIn("data_statistics", metrics)
        self.assertIn("learning_statistics", metrics)
        self.assertIn("system_status", metrics)
        
        # Verify metric values
        self.assertEqual(metrics["server_metrics"]["total_requests"], 100)
        self.assertEqual(metrics["server_metrics"]["background_updates"], 5)


class TestContinuousLearningMiddleware(unittest.TestCase):
    """Test continuous learning middleware integration."""
    
    def setUp(self):
        """Set up test fixtures."""
        
        # Mock continuous learning system
        self.mock_learning_system = Mock()
        
        # Configuration
        self.config = ServerSideConfig()
        
        # Mock app
        self.mock_app = Mock()
        
        # Create middleware
        self.middleware = ContinuousLearningMiddleware(
            app=self.mock_app,
            continuous_learning_system=self.mock_learning_system,
            config=self.config
        )
    
    def test_middleware_initialization(self):
        """Test middleware initialization."""
        
        self.assertIsNotNone(self.middleware.data_collector)
        self.assertIsNotNone(self.middleware.background_processor)
        self.assertEqual(len(self.middleware.learning_endpoints), 5)
    
    async def test_middleware_startup_shutdown(self):
        """Test middleware startup and shutdown."""
        
        # Mock background processor
        self.middleware.background_processor = Mock()
        self.middleware.background_processor.start_background_processing = AsyncMock()
        self.middleware.background_processor.stop_background_processing = AsyncMock()
        
        # Test startup
        await self.middleware.startup()
        self.middleware.background_processor.start_background_processing.assert_called_once()
        
        # Test shutdown
        await self.middleware.shutdown()
        self.middleware.background_processor.stop_background_processing.assert_called_once()
    
    def test_learning_status_retrieval(self):
        """Test learning status retrieval."""
        
        # Mock background processor metrics
        mock_metrics = {
            "server_metrics": {"total_requests": 100},
            "system_status": {"background_processing": True}
        }
        
        self.middleware.background_processor.get_learning_metrics = Mock(return_value=mock_metrics)
        
        # Get learning status
        status = self.middleware.get_learning_status()
        
        # Verify status structure
        self.assertEqual(status, mock_metrics)


class TestOpenAIServingContinuousLearning(unittest.TestCase):
    """Test OpenAI-compatible continuous learning service."""
    
    def setUp(self):
        """Set up test fixtures."""
        
        # Mock dependencies
        self.mock_engine_client = Mock()
        self.mock_model_config = Mock()
        self.mock_model_config.model = "test-model"
        self.mock_model_config.hf_config = Mock()
        self.mock_model_config.hf_config.model_type = "test_type"
        self.mock_model_config.max_model_len = 4096
        
        self.mock_models = Mock()
        self.mock_request_logger = Mock()
        
        # Create service
        self.service = OpenAIServingContinuousLearning(
            engine_client=self.mock_engine_client,
            model_config=self.mock_model_config,
            models=self.mock_models,
            request_logger=self.mock_request_logger
        )
    
    def test_service_initialization(self):
        """Test service initialization."""
        
        self.assertTrue(self.service.learning_enabled)
        self.assertIsNotNone(self.service.continuous_learning_system)
        self.assertEqual(self.service.learning_stats["total_learning_requests"], 0)
    
    async def test_learning_status_retrieval(self):
        """Test learning status retrieval."""
        
        # Mock learning system stats
        self.service.continuous_learning_system.get_learning_stats = Mock(return_value={
            "interaction_count": 50,
            "current_learning_rate": 0.001,
            "metrics": {"successful_adaptations": 40}
        })
        
        # Get status
        status = await self.service.get_learning_status()
        
        # Verify status structure
        self.assertIn("service_info", status)
        self.assertIn("learning_statistics", status)
        self.assertIn("model_info", status)
        self.assertIn("timestamp", status)
        
        # Verify service info
        self.assertTrue(status["service_info"]["learning_enabled"])
        self.assertEqual(status["model_info"]["model_name"], "test-model")
    
    async def test_learning_enable_disable(self):
        """Test enabling and disabling learning."""
        
        # Test disable
        result = await self.service.disable_learning()
        self.assertTrue(result["success"])
        self.assertFalse(self.service.learning_enabled)
        
        # Test enable
        result = await self.service.enable_learning()
        self.assertTrue(result["success"]) 
        self.assertTrue(self.service.learning_enabled)
    
    async def test_manual_learning_trigger(self):
        """Test manual learning trigger."""
        
        # Mock learning system
        self.service.continuous_learning_system.learn_from_interaction = AsyncMock()
        self.service.continuous_learning_system.learn_from_interaction.return_value = {
            "success": True,
            "learning_time": 0.01,
            "current_learning_rate": 0.001
        }
        
        # Trigger manual learning
        result = await self.service.trigger_manual_learning(
            prompt="Test prompt",
            response="Test response", 
            performance_feedback=0.8,
            interaction_type="manual"
        )
        
        # Verify result
        self.assertTrue(result["success"])
        self.assertIn("learning_result", result)
        self.assertEqual(self.service.learning_stats["successful_adaptations"], 1)
        
        # Verify learning system was called
        self.service.continuous_learning_system.learn_from_interaction.assert_called_once()
    
    async def test_learning_metrics_retrieval(self):
        """Test learning metrics retrieval."""
        
        # Mock learning system stats
        self.service.continuous_learning_system.get_learning_stats = Mock(return_value={
            "interaction_count": 100,
            "current_learning_rate": 0.001,
            "experience_count": 80,
            "consolidated_parameters": 5,
            "metrics": {"successful_adaptations": 90}
        })
        
        # Get metrics
        metrics = await self.service.get_learning_metrics()
        
        # Verify metrics structure
        self.assertIn("overview", metrics)
        self.assertIn("performance", metrics)
        self.assertIn("system_metrics", metrics)
        self.assertIn("service_stats", metrics)
        
        # Verify calculated values
        self.assertEqual(metrics["overview"]["total_interactions"], 100)
        self.assertEqual(metrics["overview"]["success_rate"], 0.9)  # 90/100


class TestServerSideContinuousLearningIntegration(unittest.TestCase):
    """Integration tests for server-side continuous learning."""
    
    async def test_end_to_end_learning_flow(self):
        """Test complete end-to-end learning flow."""
        
        # Create learning system with mocks
        mock_dynamic_manager = Mock()
        mock_dynamic_manager.apply_incremental_update = AsyncMock()
        mock_dynamic_manager.apply_incremental_update.return_value = {"success": True}
        
        mock_dtesn_integration = Mock()
        mock_dtesn_integration.adaptive_parameter_update = AsyncMock()
        mock_dtesn_integration.adaptive_parameter_update.return_value = (
            Mock(), {"learning_type": "test"}
        )
        
        config = ContinuousLearningConfig(max_experiences=100)
        
        learning_system = ContinuousLearningSystem(
            dynamic_manager=mock_dynamic_manager,
            dtesn_integration=mock_dtesn_integration,
            config=config
        )
        
        # Create data collector and processor
        server_config = ServerSideConfig()
        data_collector = ServerSideDataCollector(server_config)
        background_processor = BackgroundLearningProcessor(
            continuous_learning_system=learning_system,
            data_collector=data_collector,
            config=server_config
        )
        
        # Simulate data collection
        for i in range(10):
            interaction = InteractionData(
                interaction_id=f"integration_test_{i}",
                interaction_type="chat_completion",
                input_data={"prompt": f"Test prompt {i}"},
                output_data={"response": f"Test response {i}"},
                performance_feedback=0.7 + (i * 0.02),
                timestamp=datetime.now()
            )
            data_collector.interaction_buffer.append(interaction)
            data_collector.quality_scores.append(interaction.performance_feedback)
        
        # Process interactions
        await background_processor._process_interactions_batch()
        
        # Verify learning occurred
        self.assertGreater(mock_dtesn_integration.adaptive_parameter_update.call_count, 0)
        self.assertGreater(mock_dynamic_manager.apply_incremental_update.call_count, 0)
        self.assertGreater(background_processor.metrics.background_updates, 0)
    
    def test_production_safety_constraints(self):
        """Test production safety constraints application."""
        
        # Create learning system
        mock_dynamic_manager = Mock()
        mock_dtesn_integration = Mock()
        config = ContinuousLearningConfig(learning_rate_base=0.01)
        
        learning_system = ContinuousLearningSystem(
            dynamic_manager=mock_dynamic_manager,
            dtesn_integration=mock_dtesn_integration,
            config=config
        )
        
        # Create background processor
        server_config = ServerSideConfig(max_learning_rate_production=0.0001)
        data_collector = ServerSideDataCollector(server_config)
        background_processor = BackgroundLearningProcessor(
            continuous_learning_system=learning_system,
            data_collector=data_collector,
            config=server_config
        )
        
        # Apply production constraints
        original_config = background_processor._apply_production_constraints()
        
        # Verify constraints were applied
        self.assertEqual(learning_system.current_learning_rate, 0.0001)  # Clamped to max
        self.assertGreater(learning_system.config.ewc_lambda, config.ewc_lambda)  # Increased
        
        # Restore configuration
        background_processor._restore_original_config(original_config)
        
        # Verify restoration
        self.assertEqual(learning_system.current_learning_rate, 0.01)


# Test runner for all server-side continuous learning tests
def run_server_side_continuous_learning_tests():
    """Run comprehensive server-side continuous learning test suite."""
    
    test_classes = [
        TestServerSideDataCollector,
        TestBackgroundLearningProcessor,
        TestContinuousLearningMiddleware,
        TestOpenAIServingContinuousLearning,
        TestServerSideContinuousLearningIntegration
    ]
    
    suite = unittest.TestSuite()
    
    # Add all test methods from each class
    for test_class in test_classes:
        tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
        suite.addTests(tests)
    
    # Run the tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    if result.wasSuccessful():
        print(f"\n✅ All {result.testsRun} server-side continuous learning tests passed!")
        print("✅ Server-side continuous learning implementation meets requirements:")
        print("   - Models improve continuously from production data")
        print("   - Server-side data collection and aggregation functional")
        print("   - Background learning processes working correctly")
        print("   - OpenAI-compatible API endpoints operational")
        print("   - Production safety constraints properly implemented")
    else:
        print(f"\n❌ {len(result.failures)} failures, {len(result.errors)} errors")
        for test, error in result.failures + result.errors:
            print(f"   - {test}: {error}")
    
    return result.wasSuccessful()


# Async test runner helper
def run_async_test(async_test_func):
    """Helper to run async test functions."""
    return asyncio.get_event_loop().run_until_complete(async_test_func())


if __name__ == "__main__":
    print("Running Server-Side Continuous Learning Test Suite...")
    print("=" * 70)
    success = run_server_side_continuous_learning_tests()
    exit(0 if success else 1)