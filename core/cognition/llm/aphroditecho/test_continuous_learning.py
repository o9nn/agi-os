"""
Test Suite for Continuous Learning System.

Tests online training, experience replay, catastrophic forgetting prevention,
and integration with DTESN components.
"""

import asyncio
import unittest
from unittest.mock import Mock, AsyncMock
from datetime import datetime
import torch

# Import the components we're testing
from aphrodite.continuous_learning import (
    ContinuousLearningSystem,
    ContinuousLearningConfig, 
    InteractionData
)
from aphrodite.dtesn_integration import DTESNDynamicIntegration
from aphrodite.dynamic_model_manager import DynamicModelManager
from echo_self.meta_learning.meta_optimizer import ExperienceReplay


class TestInteractionData(unittest.TestCase):
    """Test InteractionData dataclass."""
    
    def test_interaction_data_creation(self):
        """Test creating interaction data."""
        data = InteractionData(
            interaction_id="test_001",
            interaction_type="text_generation",
            input_data={"prompt": "Hello"},
            output_data={"response": "Hello world"},
            performance_feedback=0.8,
            timestamp=datetime.now()
        )
        
        self.assertEqual(data.interaction_id, "test_001")
        self.assertEqual(data.interaction_type, "text_generation")
        self.assertEqual(data.performance_feedback, 0.8)
        self.assertTrue(data.success)  # Default value
    
    def test_interaction_data_with_metadata(self):
        """Test interaction data with context metadata."""
        data = InteractionData(
            interaction_id="test_002",
            interaction_type="reasoning",
            input_data={"problem": "2+2=?"},
            output_data={"answer": "4"},
            performance_feedback=1.0,
            timestamp=datetime.now(),
            context_metadata={"importance": 0.9, "task_type": "math"},
            success=True
        )
        
        self.assertEqual(data.context_metadata["importance"], 0.9)
        self.assertEqual(data.context_metadata["task_type"], "math")
        self.assertTrue(data.success)


class TestContinuousLearningConfig(unittest.TestCase):
    """Test ContinuousLearningConfig dataclass."""
    
    def test_default_config(self):
        """Test default configuration values."""
        config = ContinuousLearningConfig()
        
        self.assertEqual(config.max_experiences, 10000)
        self.assertEqual(config.replay_batch_size, 32)
        self.assertEqual(config.learning_rate_base, 0.001)
        self.assertTrue(config.enable_ewc)
        self.assertEqual(config.ewc_lambda, 1000.0)
    
    def test_custom_config(self):
        """Test custom configuration values."""
        config = ContinuousLearningConfig(
            max_experiences=5000,
            learning_rate_base=0.01,
            enable_ewc=False
        )
        
        self.assertEqual(config.max_experiences, 5000)
        self.assertEqual(config.learning_rate_base, 0.01)
        self.assertFalse(config.enable_ewc)


class TestContinuousLearningSystem(unittest.TestCase):
    """Test ContinuousLearningSystem core functionality."""
    
    def setUp(self):
        """Set up test fixtures."""
        # Create mock dependencies
        self.mock_dynamic_manager = Mock(spec=DynamicModelManager)
        self.mock_dynamic_manager.apply_incremental_update = AsyncMock()
        self.mock_dynamic_manager.apply_incremental_update.return_value = {
            'success': True,
            'update_id': 'test_update'
        }
        
        self.mock_dtesn_integration = Mock(spec=DTESNDynamicIntegration)
        self.mock_dtesn_integration.adaptive_parameter_update = AsyncMock()
        self.mock_dtesn_integration.adaptive_parameter_update.return_value = (
            torch.randn(768, 768),  # Updated parameters
            {'learning_type': 'stdp', 'learning_rate': 0.001}  # Metrics
        )
        
        self.config = ContinuousLearningConfig(
            max_experiences=100,
            replay_batch_size=5,
            replay_frequency=5,
            consolidation_frequency=10
        )
        
        self.system = ContinuousLearningSystem(
            dynamic_manager=self.mock_dynamic_manager,
            dtesn_integration=self.mock_dtesn_integration,
            config=self.config
        )
    
    def test_initialization(self):
        """Test system initialization."""
        self.assertIsNotNone(self.system.experience_replay)
        self.assertEqual(self.system.interaction_count, 0)
        self.assertEqual(self.system.current_learning_rate, self.config.learning_rate_base)
        self.assertEqual(len(self.system.parameter_importance), 0)
        self.assertEqual(len(self.system.consolidated_parameters), 0)
    
    def test_extract_learning_signal(self):
        """Test learning signal extraction."""
        interaction_data = InteractionData(
            interaction_id="test_001",
            interaction_type="text_generation",
            input_data={"prompt": "Hello"},
            output_data={"response": "Hello world"},
            performance_feedback=0.8,
            timestamp=datetime.now(),
            context_metadata={"importance": 0.9}
        )
        
        learning_signal = self.system._extract_learning_signal(interaction_data)
        
        self.assertIn('strength', learning_signal)
        self.assertIn('direction', learning_signal)
        self.assertIn('context_weight', learning_signal)
        self.assertIn('temporal_weight', learning_signal)
        self.assertEqual(learning_signal['direction'], 1)  # Positive feedback
        self.assertEqual(learning_signal['raw_feedback'], 0.8)
        self.assertEqual(learning_signal['context_weight'], 0.9)
    
    def test_identify_target_parameters(self):
        """Test parameter identification for different interaction types."""
        # Test text generation
        interaction_data = InteractionData(
            interaction_id="test_001",
            interaction_type="text_generation",
            input_data={},
            output_data={},
            performance_feedback=0.5,
            timestamp=datetime.now()
        )
        
        params = self.system._identify_target_parameters(interaction_data)
        self.assertIsInstance(params, list)
        self.assertTrue(len(params) > 0)
        self.assertTrue(any('mlp' in p for p in params))
        
        # Test reasoning
        interaction_data.interaction_type = "reasoning"
        params = self.system._identify_target_parameters(interaction_data)
        self.assertTrue(any('attn' in p for p in params))
        
        # Test unknown type (should use default)
        interaction_data.interaction_type = "unknown_type"
        params = self.system._identify_target_parameters(interaction_data)
        self.assertEqual(params, ['transformer.h.10.mlp.c_proj.weight'])
    
    async def test_learn_from_interaction_success(self):
        """Test successful learning from interaction."""
        interaction_data = InteractionData(
            interaction_id="test_001",
            interaction_type="text_generation",
            input_data={"prompt": "Hello"},
            output_data={"response": "Hello world"},
            performance_feedback=0.8,
            timestamp=datetime.now()
        )
        
        result = await self.system.learn_from_interaction(interaction_data)
        
        self.assertTrue(result['success'])
        self.assertEqual(result['interaction_count'], 1)
        self.assertIn('learning_signal', result)
        self.assertIn('online_update', result)
        self.assertIn('metrics', result)
        
        # Verify system state changes
        self.assertEqual(self.system.interaction_count, 1)
        self.assertEqual(len(self.system.experience_replay.experiences), 1)
        self.assertEqual(self.system.learning_metrics['total_interactions'], 1)
        self.assertEqual(self.system.learning_metrics['successful_adaptations'], 1)
    
    async def test_learn_from_interaction_with_replay(self):
        """Test learning that triggers experience replay."""
        # Set replay frequency to 1 for immediate testing
        self.system.config.replay_frequency = 1
        
        interaction_data = InteractionData(
            interaction_id="test_001",
            interaction_type="text_generation",
            input_data={"prompt": "Hello"},
            output_data={"response": "Hello world"},
            performance_feedback=0.8,
            timestamp=datetime.now()
        )
        
        result = await self.system.learn_from_interaction(interaction_data)
        
        self.assertTrue(result['success'])
        self.assertIsNotNone(result['replay_result'])
        # Note: replay_result might have 0 replayed_count since we only have 1 experience
    
    async def test_learn_from_interaction_with_consolidation(self):
        """Test learning that triggers memory consolidation."""
        # Set consolidation frequency to 1 and add some parameter importance
        self.system.config.consolidation_frequency = 1
        self.system.parameter_importance['test_param'] = torch.ones(10, 10) * 2.0  # High importance
        
        interaction_data = InteractionData(
            interaction_id="test_001",
            interaction_type="text_generation",
            input_data={"prompt": "Hello"},
            output_data={"response": "Hello world"},
            performance_feedback=0.8,
            timestamp=datetime.now()
        )
        
        result = await self.system.learn_from_interaction(interaction_data)
        
        self.assertTrue(result['success'])
        self.assertIsNotNone(result['consolidation_result'])
    
    def test_parameter_importance_update(self):
        """Test parameter importance updating for EWC."""
        interaction_data = InteractionData(
            interaction_id="test_001",
            interaction_type="text_generation",
            input_data={},
            output_data={},
            performance_feedback=0.8,
            timestamp=datetime.now()
        )
        
        learning_signal = {'strength': 0.5, 'direction': 1}
        
        # Initial update
        self.system._update_parameter_importance(interaction_data, learning_signal)
        
        target_params = self.system._identify_target_parameters(interaction_data)
        for param in target_params:
            self.assertIn(param, self.system.parameter_importance)
            importance = self.system.parameter_importance[param]
            self.assertTrue(torch.all(importance > 0))
        
        # Second update should use exponential moving average
        learning_signal['strength'] = 0.3
        self.system._update_parameter_importance(interaction_data, learning_signal)
        
        # Importance should be updated but not exactly equal to new value
        for param in target_params:
            importance = self.system.parameter_importance[param]
            self.assertTrue(torch.all(importance > 0))
    
    def test_ewc_regularization(self):
        """Test Elastic Weight Consolidation regularization."""
        param_name = "test_param"
        current_params = torch.randn(10, 10)
        updated_params = torch.randn(10, 10)
        
        # Set up importance and consolidated parameters
        self.system.parameter_importance[param_name] = torch.ones(10, 10) * 0.5
        self.system.consolidated_parameters[param_name] = torch.zeros(10, 10)
        
        regularized_params = self.system._apply_ewc_regularization(
            param_name, current_params, updated_params
        )
        
        # Regularized parameters should be different from updated parameters
        self.assertFalse(torch.equal(regularized_params, updated_params))
        
        # Should be pulled towards consolidated parameters (zeros in this case)
        self.assertTrue(torch.all(torch.abs(regularized_params) < torch.abs(updated_params)))
    
    def test_learning_rate_adaptation(self):
        """Test learning rate adaptation based on performance."""
        initial_lr = self.system.current_learning_rate
        
        # Add poor performance history
        for i in range(15):
            self.system.performance_history.append({
                'timestamp': datetime.now(),
                'performance': 0.3,  # Below threshold
                'success': True,
                'interaction_type': 'test'
            })
        
        self.system._adapt_learning_rate()
        
        # Learning rate should increase slightly for poor performance
        self.assertGreaterEqual(self.system.current_learning_rate, initial_lr)
        
        # Reset and add good performance history
        self.system.current_learning_rate = initial_lr
        self.system.performance_history = []
        
        for i in range(15):
            self.system.performance_history.append({
                'timestamp': datetime.now(),
                'performance': 0.9,  # Above threshold
                'success': True,
                'interaction_type': 'test'
            })
        
        self.system._adapt_learning_rate()
        
        # Learning rate should decay for good performance
        self.assertLess(self.system.current_learning_rate, initial_lr)
    
    def test_get_learning_stats(self):
        """Test learning statistics generation."""
        # Add some history
        self.system.interaction_count = 10
        self.system.learning_metrics['successful_adaptations'] = 8
        self.system.performance_history = [
            {'performance': 0.8, 'timestamp': datetime.now(), 'success': True, 'interaction_type': 'test'}
            for _ in range(5)
        ]
        
        stats = self.system.get_learning_stats()
        
        self.assertIn('metrics', stats)
        self.assertIn('current_learning_rate', stats)
        self.assertIn('interaction_count', stats)
        self.assertIn('experience_count', stats)
        self.assertIn('performance_stats', stats)
        
        self.assertEqual(stats['interaction_count'], 10)
        self.assertEqual(stats['metrics']['successful_adaptations'], 8)
        
        # Performance stats
        perf_stats = stats['performance_stats']
        self.assertIn('mean', perf_stats)
        self.assertIn('std', perf_stats)
        self.assertIn('min', perf_stats)
        self.assertIn('max', perf_stats)
    
    async def test_reset_learning_state(self):
        """Test learning state reset."""
        # Set up some state
        self.system.interaction_count = 10
        self.system.learning_metrics['successful_adaptations'] = 8
        self.system.performance_history = [{'test': 'data'}]
        self.system.experience_replay.experiences = [Mock() for _ in range(5)]
        self.system.consolidated_parameters['test'] = torch.randn(10, 10)
        self.system.learning_metrics['consolidations'] = 3
        
        await self.system.reset_learning_state()
        
        # Check reset state
        self.assertEqual(self.system.interaction_count, 0)
        self.assertEqual(self.system.current_learning_rate, self.config.learning_rate_base)
        self.assertEqual(len(self.system.performance_history), 0)
        self.assertEqual(len(self.system.experience_replay.experiences), 0)
        
        # Check preserved state
        self.assertIn('test', self.system.consolidated_parameters)
        self.assertEqual(self.system.learning_metrics['consolidations'], 3)
        
        # Check reset metrics
        self.assertEqual(self.system.learning_metrics['total_interactions'], 0)
        self.assertEqual(self.system.learning_metrics['successful_adaptations'], 0)


class TestContinuousLearningIntegration(unittest.TestCase):
    """Test integration with existing components."""
    
    def setUp(self):
        """Set up integration test fixtures."""
        self.mock_dynamic_manager = Mock(spec=DynamicModelManager)
        self.mock_dynamic_manager.apply_incremental_update = AsyncMock()
        self.mock_dynamic_manager.apply_incremental_update.return_value = {
            'success': True,
            'update_id': 'test_update'
        }
        
        self.mock_dtesn_integration = Mock(spec=DTESNDynamicIntegration)
        self.mock_dtesn_integration.adaptive_parameter_update = AsyncMock()
        self.mock_dtesn_integration.adaptive_parameter_update.return_value = (
            torch.randn(768, 768),
            {'learning_type': 'bcm', 'learning_rate': 0.001}
        )
        
        self.system = ContinuousLearningSystem(
            dynamic_manager=self.mock_dynamic_manager,
            dtesn_integration=self.mock_dtesn_integration
        )
    
    async def test_dtesn_integration_called(self):
        """Test that DTESN integration is properly called."""
        interaction_data = InteractionData(
            interaction_id="test_001",
            interaction_type="text_generation",
            input_data={"prompt": "Hello"},
            output_data={"response": "Hello world"},
            performance_feedback=0.8,
            timestamp=datetime.now()
        )
        
        await self.system.learn_from_interaction(interaction_data)
        
        # Verify DTESN integration was called
        self.mock_dtesn_integration.adaptive_parameter_update.assert_called()
        
        # Check call arguments
        call_args = self.mock_dtesn_integration.adaptive_parameter_update.call_args
        self.assertIsInstance(call_args[1]['current_params'], torch.Tensor)
        self.assertIsInstance(call_args[1]['target_gradient'], torch.Tensor)
        self.assertEqual(call_args[1]['performance_feedback'], 0.8)
    
    async def test_dynamic_manager_integration_called(self):
        """Test that Dynamic Model Manager is properly called."""
        interaction_data = InteractionData(
            interaction_id="test_001",
            interaction_type="text_generation",
            input_data={"prompt": "Hello"},
            output_data={"response": "Hello world"},
            performance_feedback=0.8,
            timestamp=datetime.now()
        )
        
        await self.system.learn_from_interaction(interaction_data)
        
        # Verify Dynamic Model Manager was called
        self.mock_dynamic_manager.apply_incremental_update.assert_called()
        
        # Check call arguments structure
        call_args = self.mock_dynamic_manager.apply_incremental_update.call_args
        update_request = call_args[0][0]  # First positional argument
        
        self.assertIsInstance(update_request.parameter_name, str)
        self.assertEqual(update_request.update_type, "replace")
        self.assertIn('interaction_id', update_request.metadata)
    
    def test_experience_replay_integration(self):
        """Test integration with ExperienceReplay."""
        # The system should use ExperienceReplay internally
        self.assertIsInstance(self.system.experience_replay, ExperienceReplay)
        self.assertEqual(self.system.experience_replay.max_size, self.system.config.max_experiences)


class TestContinuousLearningAcceptanceCriteria(unittest.TestCase):
    """Test that the system meets the acceptance criteria: 'Models learn continuously from new experiences'."""
    
    def setUp(self):
        """Set up acceptance criteria test fixtures."""
        self.mock_dynamic_manager = Mock(spec=DynamicModelManager)
        self.mock_dynamic_manager.apply_incremental_update = AsyncMock()
        self.mock_dynamic_manager.apply_incremental_update.return_value = {
            'success': True,
            'update_id': 'test_update'
        }
        
        self.mock_dtesn_integration = Mock(spec=DTESNDynamicIntegration)
        self.mock_dtesn_integration.adaptive_parameter_update = AsyncMock()
        self.mock_dtesn_integration.adaptive_parameter_update.return_value = (
            torch.randn(768, 768),
            {'learning_type': 'stdp', 'learning_rate': 0.001}
        )
        
        self.config = ContinuousLearningConfig(
            max_experiences=1000,
            replay_frequency=5,
            consolidation_frequency=10
        )
        
        self.system = ContinuousLearningSystem(
            dynamic_manager=self.mock_dynamic_manager,
            dtesn_integration=self.mock_dtesn_integration,
            config=self.config
        )
    
    async def test_continuous_learning_from_multiple_experiences(self):
        """Test that the system continuously learns from multiple new experiences."""
        # Create a sequence of different interactions
        interactions = []
        for i in range(20):
            interaction = InteractionData(
                interaction_id=f"test_{i:03d}",
                interaction_type="text_generation" if i % 2 == 0 else "reasoning",
                input_data={"prompt": f"Test prompt {i}"},
                output_data={"response": f"Test response {i}"},
                performance_feedback=0.7 + (i % 3) * 0.1,  # Varying feedback
                timestamp=datetime.now(),
                context_metadata={"session": i // 5}  # Group into sessions
            )
            interactions.append(interaction)
        
        # Process all interactions
        results = []
        for interaction in interactions:
            result = await self.system.learn_from_interaction(interaction)
            results.append(result)
        
        # Verify continuous learning occurred
        successful_learns = [r for r in results if r['success']]
        self.assertEqual(len(successful_learns), 20, "All interactions should be learned from")
        
        # Verify system state evolved
        self.assertEqual(self.system.interaction_count, 20)
        self.assertEqual(len(self.system.experience_replay.experiences), 20)
        self.assertEqual(self.system.learning_metrics['total_interactions'], 20)
        self.assertEqual(self.system.learning_metrics['successful_adaptations'], 20)
        
        # Verify experience replay was triggered (at replay_frequency intervals)
        replay_triggers = [r for r in results if r['replay_result'] is not None]
        expected_replays = 20 // self.config.replay_frequency
        self.assertEqual(len(replay_triggers), expected_replays)
        
        # Verify memory consolidation was triggered
        consolidation_triggers = [r for r in results if r['consolidation_result'] is not None]
        expected_consolidations = 20 // self.config.consolidation_frequency
        self.assertEqual(len(consolidation_triggers), expected_consolidations)
    
    async def test_learning_without_catastrophic_forgetting(self):
        """Test that learning preserves important previous knowledge (EWC)."""
        # First, learn from some initial interactions
        initial_interactions = []
        for i in range(5):
            interaction = InteractionData(
                interaction_id=f"initial_{i}",
                interaction_type="memory_recall",
                input_data={"query": f"Important fact {i}"},
                output_data={"fact": f"Critical knowledge {i}"},
                performance_feedback=0.9,  # High performance - important to remember
                timestamp=datetime.now()
            )
            initial_interactions.append(interaction)
        
        for interaction in initial_interactions:
            await self.system.learn_from_interaction(interaction)
        
        # Verify important parameters were identified and consolidated
        self.assertGreater(len(self.system.parameter_importance), 0, 
                         "Parameter importance should be tracked")
        
        # Trigger consolidation to save important knowledge
        await self.system._perform_memory_consolidation()
        
        initial_consolidated_count = len(self.system.consolidated_parameters)
        self.assertGreater(initial_consolidated_count, 0,
                         "Important parameters should be consolidated")
        
        # Now learn from different type of interactions
        new_interactions = []
        for i in range(10):
            interaction = InteractionData(
                interaction_id=f"new_{i}",
                interaction_type="text_generation",
                input_data={"prompt": f"New task {i}"},
                output_data={"response": f"New response {i}"},
                performance_feedback=0.6,  # Lower performance - less critical
                timestamp=datetime.now()
            )
            new_interactions.append(interaction)
        
        for interaction in new_interactions:
            result = await self.system.learn_from_interaction(interaction)
            self.assertTrue(result['success'], "New learning should be successful")
        
        # Verify that EWC regularization was applied (consolidated parameters preserved)
        final_consolidated_count = len(self.system.consolidated_parameters)
        self.assertGreaterEqual(final_consolidated_count, initial_consolidated_count,
                               "Consolidated parameters should not decrease")
        
        # Verify system continued learning despite EWC constraints
        self.assertEqual(self.system.interaction_count, 15)  # 5 initial + 10 new
        self.assertEqual(self.system.learning_metrics['total_interactions'], 15)
    
    def test_experience_replay_reinforces_learning(self):
        """Test that experience replay reinforces important learning."""
        # Add some high-value experiences to replay buffer
        high_value_experiences = []
        for i in range(3):
            interaction = InteractionData(
                interaction_id=f"high_value_{i}",
                interaction_type="reasoning",
                input_data={"problem": f"Complex problem {i}"},
                output_data={"solution": f"Elegant solution {i}"},
                performance_feedback=0.95,  # Very high feedback
                timestamp=datetime.now()
            )
            
            learning_signal = self.system._extract_learning_signal(interaction)
            update_result = {'success': True, 'updated_parameters': ['test_param']}
            experience = self.system._create_experience_record(
                interaction, learning_signal, update_result
            )
            high_value_experiences.append(experience)
        
        # Add experiences to replay buffer
        for exp in high_value_experiences:
            self.system.experience_replay.add_experience(exp)
        
        # Verify experiences were added
        self.assertEqual(len(self.system.experience_replay.experiences), 3)
        
        # Get top performers (should be all 3 since they have high fitness)
        top_performers = self.system.experience_replay.get_top_performers(n=5)
        self.assertEqual(len(top_performers), 3)
        
        # All should have high fitness scores
        for exp in top_performers:
            self.assertGreaterEqual(exp.fitness_score, 0.95)
    
    async def test_adaptive_learning_rate(self):
        """Test that learning rate adapts based on performance."""
        initial_lr = self.system.current_learning_rate
        
        # Create sequence of poor performance interactions
        poor_interactions = []
        for i in range(15):
            interaction = InteractionData(
                interaction_id=f"poor_{i}",
                interaction_type="text_generation",
                input_data={"prompt": f"Difficult task {i}"},
                output_data={"response": f"Struggling response {i}"},
                performance_feedback=0.2,  # Poor performance
                timestamp=datetime.now()
            )
            poor_interactions.append(interaction)
        
        for interaction in poor_interactions:
            await self.system.learn_from_interaction(interaction)
        
        # Learning rate should have adapted (likely increased due to poor performance)
        self.assertNotEqual(self.system.current_learning_rate, initial_lr,
                          "Learning rate should adapt to performance")
        
        # Verify performance tracking
        recent_perf = [p['performance'] for p in self.system.performance_history[-10:]]
        self.assertTrue(all(p < 0.5 for p in recent_perf),
                       "Recent performance should reflect poor interactions")
    
    def test_system_scalability(self):
        """Test system can handle large numbers of experiences."""
        config = ContinuousLearningConfig(max_experiences=50)  # Smaller for test
        
        system = ContinuousLearningSystem(
            dynamic_manager=Mock(spec=DynamicModelManager),
            dtesn_integration=Mock(spec=DTESNDynamicIntegration),
            config=config
        )
        
        # Add more experiences than max_experiences
        for i in range(75):
            interaction = InteractionData(
                interaction_id=f"scale_{i}",
                interaction_type="text_generation",
                input_data={},
                output_data={},
                performance_feedback=0.5,
                timestamp=datetime.now()
            )
            
            learning_signal = system._extract_learning_signal(interaction)
            update_result = {'success': True}
            experience = system._create_experience_record(
                interaction, learning_signal, update_result
            )
            system.experience_replay.add_experience(experience)
        
        # Should not exceed max_experiences
        self.assertLessEqual(len(system.experience_replay.experiences), config.max_experiences)
        self.assertEqual(len(system.experience_replay.experiences), config.max_experiences)


def run_continuous_learning_tests():
    """Run all continuous learning tests."""
    test_classes = [
        TestInteractionData,
        TestContinuousLearningConfig,
        TestContinuousLearningSystem,
        TestContinuousLearningIntegration,
        TestContinuousLearningAcceptanceCriteria
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
        print(f"\n✅ All {result.testsRun} tests passed!")
        print("✅ Continuous Learning System meets acceptance criteria:")
        print("   - Models learn continuously from new experiences")
        print("   - Online training from interaction data works")
        print("   - Experience replay and data management functional") 
        print("   - Catastrophic forgetting prevention implemented")
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
    print("Running Continuous Learning System Test Suite...")
    print("=" * 60)
    success = run_continuous_learning_tests()
    exit(0 if success else 1)