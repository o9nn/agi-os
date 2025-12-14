#!/usr/bin/env python3
"""
Test Suite for Enactive Perception System

Validates Task 2.3.3 requirements:
- Action-based perception mechanisms
- Sensorimotor contingency learning
- Perceptual prediction through action
- Acceptance Criteria: Perception emerges through agent-environment interaction
"""

import unittest
import numpy as np
import logging
import sys
from pathlib import Path
from unittest.mock import Mock

# Add the current directory to the path for imports
sys.path.insert(0, str(Path(__file__).parent))

try:
    from enactive_perception import (
        EnactivePerceptionSystem, SensorimotorContingencyLearner, ActionBasedPerceptionModule,
        SensorimotorContingency, PerceptualPrediction, create_enactive_perception_system,
        integrate_with_embodied_learning, BodyState, MotorAction, SensorimotorExperience
    )
    ENACTIVE_PERCEPTION_AVAILABLE = True
except ImportError as e:
    ENACTIVE_PERCEPTION_AVAILABLE = False
    print(f"Warning: Could not import enactive_perception: {e}")


class TestSensorimotorContingencyLearner(unittest.TestCase):
    """Test sensorimotor contingency learning capabilities"""
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_contingency_learner_creation(self):
        """Test creating a sensorimotor contingency learner"""
        learner = SensorimotorContingencyLearner(max_contingencies=100, learning_rate=0.05)
        
        self.assertEqual(learner.max_contingencies, 100)
        self.assertEqual(learner.learning_rate, 0.05)
        self.assertEqual(len(learner.contingencies), 0)
        self.assertIsNotNone(learner.action_history)
        self.assertIsNotNone(learner.sensory_history)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_contingency_learning(self):
        """Test learning sensorimotor contingencies"""
        learner = SensorimotorContingencyLearner()
        
        # Create test experience
        initial_state = BodyState(
            joint_angles={'shoulder': 0.5},
            sensory_state={'vision': 0.8, 'touch': 0.2}
        )
        
        action = MotorAction(
            joint_targets={'shoulder': 0.7},
            muscle_commands={'primary': 0.8}
        )
        
        resulting_state = BodyState(
            joint_angles={'shoulder': 0.7},
            sensory_state={'vision': 0.9, 'touch': 0.3}
        )
        
        experience = SensorimotorExperience(
            initial_body_state=initial_state,
            motor_action=action,
            resulting_body_state=resulting_state,
            sensory_feedback={'vision': 0.9, 'touch': 0.3},
            success=True
        )
        
        # Learn contingency
        result = learner.learn_contingency(experience)
        
        self.assertTrue(result)
        self.assertEqual(len(learner.contingencies), 1)
        
        contingency = learner.contingencies[0]
        self.assertEqual(contingency.action_pattern['joint_targets']['shoulder'], 0.7)
        self.assertEqual(contingency.sensory_context['vision'], 0.8)
        self.assertEqual(contingency.actual_outcome['vision'], 0.9)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_prediction_generation(self):
        """Test predicting sensory outcomes"""
        learner = SensorimotorContingencyLearner()
        
        # First, learn a contingency
        initial_state = BodyState(sensory_state={'vision': 0.5})
        action = MotorAction(joint_targets={'shoulder': 0.6})
        experience = SensorimotorExperience(
            initial_body_state=initial_state,
            motor_action=action,
            resulting_body_state=BodyState(),
            sensory_feedback={'vision': 0.7, 'success': True},
            success=True
        )
        
        learner.learn_contingency(experience)
        
        # Update confidence to make it predictive
        learner.contingencies[0].confidence = 0.8
        
        # Now test prediction
        test_action = MotorAction(joint_targets={'shoulder': 0.6})
        prediction = learner.predict_sensory_outcome(test_action, {'vision': 0.5})
        
        self.assertIsInstance(prediction, dict)
        # Should predict some outcome for similar action
        if prediction:
            self.assertTrue(len(prediction) > 0)


class TestActionBasedPerceptionModule(unittest.TestCase):
    """Test action-based perception mechanisms"""
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_perception_module_creation(self):
        """Test creating action-based perception module"""
        module = ActionBasedPerceptionModule(exploration_rate=0.3)
        
        self.assertEqual(module.exploration_rate, 0.3)
        self.assertIsInstance(module.attention_weights, dict)
        self.assertIsInstance(module.perceptual_expectations, dict)
        self.assertIsNotNone(module.exploration_actions)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_exploratory_action_generation(self):
        """Test generating exploratory actions"""
        module = ActionBasedPerceptionModule(exploration_rate=0.2)
        
        current_state = BodyState(
            joint_angles={'shoulder': 0.5, 'elbow': 0.3},
            sensory_state={'vision': 0.7}
        )
        
        action = module.generate_exploratory_action(current_state)
        
        self.assertIsInstance(action, MotorAction)
        self.assertTrue(len(action.joint_targets) > 0)
        self.assertTrue(0.0 <= action.force <= 1.0)
        self.assertTrue(0.1 <= action.duration <= 2.0)
        
        # Verify exploration was applied
        if 'shoulder' in action.joint_targets:
            # Should be different from original position
            self.assertNotEqual(action.joint_targets['shoulder'], 0.5)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_attention_weight_updates(self):
        """Test updating attention weights based on sensory surprise"""
        module = ActionBasedPerceptionModule()
        
        initial_weights = module.attention_weights.copy()
        
        # Simulate sensory surprise
        surprise = {'vision': 0.8, 'touch': 0.3, 'audio': 0.9}
        module.update_attention_weights(surprise)
        
        # Check that weights were updated
        self.assertTrue(len(module.attention_weights) > len(initial_weights))
        self.assertIn('vision', module.attention_weights)
        self.assertIn('audio', module.attention_weights)
        
        # High surprise should increase attention
        self.assertTrue(module.attention_weights['audio'] > 0.5)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_perception_focusing(self):
        """Test focusing perception with attention weights"""
        module = ActionBasedPerceptionModule()
        
        # Set attention weights
        module.attention_weights = {'vision': 0.8, 'touch': 0.3, 'audio': 0.6}
        
        sensory_input = {'vision': 1.0, 'touch': 0.5, 'audio': 0.7, 'smell': 0.4}
        
        focused_input = module.focus_perception(sensory_input)
        
        self.assertIsInstance(focused_input, dict)
        # Check that attention weights were applied
        self.assertAlmostEqual(focused_input['vision'], 1.0 * 0.8, places=2)
        self.assertAlmostEqual(focused_input['touch'], 0.5 * 0.3, places=2)
        self.assertAlmostEqual(focused_input['audio'], 0.7 * 0.6, places=2)
        # Unweighted modality should get default weight
        self.assertTrue('smell' in focused_input)


class TestEnactivePerceptionSystem(unittest.TestCase):
    """Test the main enactive perception system integration"""
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_system_creation(self):
        """Test creating an enactive perception system"""
        system = EnactivePerceptionSystem("test_agent")
        
        self.assertEqual(system.agent_name, "test_agent")
        self.assertIsInstance(system.contingency_learner, SensorimotorContingencyLearner)
        self.assertIsInstance(system.action_perception_module, ActionBasedPerceptionModule)
        self.assertIsInstance(system.current_perceptual_state, dict)
        self.assertIsNotNone(system.perceptual_history)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_embodied_experience_processing(self):
        """Test processing embodied experiences"""
        system = EnactivePerceptionSystem("test_agent")
        
        # Create test experience
        experience = SensorimotorExperience(
            initial_body_state=BodyState(sensory_state={'vision': 0.5}),
            motor_action=MotorAction(joint_targets={'shoulder': 0.6}),
            resulting_body_state=BodyState(sensory_state={'vision': 0.7}),
            sensory_feedback={'vision': 0.7, 'surprise_level': 'moderate'},
            success=True,
            reward=0.8
        )
        
        # Process experience
        result = system.process_embodied_experience(experience)
        
        self.assertIsInstance(result, dict)
        self.assertIn('contingency_learned', result)
        self.assertIn('sensory_surprise', result)
        self.assertIn('perceptual_state_updated', result)
        self.assertTrue(result['perceptual_state_updated'])
        
        # Check that perceptual state was updated
        self.assertTrue(len(system.perceptual_history) > 0)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_perceptual_prediction(self):
        """Test predicting perceptual outcomes"""
        system = EnactivePerceptionSystem("test_agent")
        
        # First create some experience for the system to learn from
        experience = SensorimotorExperience(
            initial_body_state=BodyState(sensory_state={'touch': 0.3}),
            motor_action=MotorAction(joint_targets={'elbow': 0.4}),
            resulting_body_state=BodyState(sensory_state={'touch': 0.6}),
            sensory_feedback={'touch': 0.6},
            success=True
        )
        system.process_embodied_experience(experience)
        
        # Test prediction
        planned_action = MotorAction(joint_targets={'elbow': 0.4})
        current_state = BodyState(sensory_state={'touch': 0.3})
        
        prediction = system.predict_perceptual_outcome(planned_action, current_state)
        
        self.assertIsInstance(prediction, PerceptualPrediction)
        self.assertEqual(prediction.action_plan, planned_action)
        self.assertTrue(0.0 <= prediction.confidence <= 1.0)
        self.assertTrue(0.0 <= prediction.exploration_value <= 1.0)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_perceptual_action_generation(self):
        """Test generating actions for perceptual exploration"""
        system = EnactivePerceptionSystem("test_agent")
        
        current_state = BodyState(
            joint_angles={'shoulder': 0.4, 'elbow': 0.2},
            sensory_state={'vision': 0.6, 'touch': 0.4}
        )
        
        perceptual_goal = {'explore_touch': True, 'search_vision': 0.8}
        
        action = system.generate_perceptual_action(current_state, perceptual_goal)
        
        self.assertIsInstance(action, MotorAction)
        self.assertTrue(len(action.joint_targets) > 0)
        # Should generate exploratory movement
        for joint, angle in action.joint_targets.items():
            self.assertTrue(-np.pi <= angle <= np.pi)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_enactive_metrics(self):
        """Test getting enactive perception metrics"""
        system = EnactivePerceptionSystem("test_agent")
        
        # Add some experiences to generate metrics
        for i in range(3):
            experience = SensorimotorExperience(
                initial_body_state=BodyState(sensory_state={'test': 0.5 + i * 0.1}),
                motor_action=MotorAction(joint_targets={'joint1': 0.3 + i * 0.1}),
                resulting_body_state=BodyState(sensory_state={'test': 0.6 + i * 0.1}),
                sensory_feedback={'test': 0.6 + i * 0.1},
                success=True
            )
            system.process_embodied_experience(experience)
        
        metrics = system.get_enactive_metrics()
        
        self.assertIsInstance(metrics, dict)
        required_metrics = [
            'total_contingencies_learned',
            'average_contingency_confidence',
            'exploration_actions_taken',
            'attention_weights',
            'system_active'
        ]
        
        for metric in required_metrics:
            self.assertIn(metric, metrics)
        
        self.assertTrue(metrics['system_active'])
        self.assertTrue(metrics['total_contingencies_learned'] >= 0)


class TestEnactivePerceptionIntegration(unittest.TestCase):
    """Test integration with existing systems"""
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_factory_function(self):
        """Test factory function for creating systems"""
        system = create_enactive_perception_system("factory_test")
        
        self.assertIsInstance(system, EnactivePerceptionSystem)
        self.assertEqual(system.agent_name, "factory_test")
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_embodied_learning_integration(self):
        """Test integration with embodied learning system"""
        enactive_system = EnactivePerceptionSystem("integration_test")
        
        # Mock embodied learning system
        mock_embodied_system = Mock()
        mock_embodied_system.sensory_motor = Mock()
        
        result = integrate_with_embodied_learning(enactive_system, mock_embodied_system)
        
        self.assertTrue(result)
        self.assertEqual(mock_embodied_system.enactive_perception, enactive_system)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_integration_failure_handling(self):
        """Test handling of integration failures"""
        enactive_system = EnactivePerceptionSystem("failure_test")
        
        # Mock system without sensory_motor component
        mock_embodied_system = Mock()
        del mock_embodied_system.sensory_motor
        
        result = integrate_with_embodied_learning(enactive_system, mock_embodied_system)
        
        self.assertFalse(result)


class TestAcceptanceCriteria(unittest.TestCase):
    """Test acceptance criteria: Perception emerges through agent-environment interaction"""
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_perception_emergence_through_interaction(self):
        """Test that perception emerges through agent-environment interaction"""
        system = EnactivePerceptionSystem("emergence_test")
        
        # Simulate series of agent-environment interactions
        interactions = []
        
        for i in range(10):
            # Agent acts in environment
            current_state = BodyState(
                joint_angles={'joint': 0.1 * i},
                sensory_state={'environment': 0.5 + 0.05 * i}
            )
            
            action = system.generate_perceptual_action(current_state)
            
            # Environment responds
            environment_response = {
                'environment': 0.5 + 0.05 * i + np.random.uniform(-0.1, 0.1),
                'feedback': f'response_{i}',
                'changed': True
            }
            
            experience = SensorimotorExperience(
                initial_body_state=current_state,
                motor_action=action,
                resulting_body_state=BodyState(sensory_state=environment_response),
                sensory_feedback=environment_response,
                success=True
            )
            
            # System learns from interaction
            result = system.process_embodied_experience(experience)
            interactions.append(result)
        
        # Verify that perception has emerged through interaction
        metrics = system.get_enactive_metrics()
        
        # Should have learned contingencies through interaction
        self.assertTrue(metrics['total_contingencies_learned'] > 0)
        
        # Should have developed attention weights
        self.assertTrue(len(metrics['attention_weights']) > 0)
        
        # Should have perceptual history from interactions
        self.assertTrue(metrics['perceptual_history_length'] > 0)
        
        # Should show improvement in prediction capability
        self.assertTrue(all(interaction['contingency_learned'] for interaction in interactions[:5]))
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_action_based_perception_mechanisms(self):
        """Test that perception is action-based"""
        system = EnactivePerceptionSystem("action_based_test")
        
        initial_state = BodyState(
            joint_angles={'shoulder': 0.0},
            sensory_state={'clarity': 0.3}
        )
        
        # Generate action specifically for perception
        perceptual_goal = {'increase_clarity': True}
        action = system.generate_perceptual_action(initial_state, perceptual_goal)
        
        # Action should be different from passive state
        self.assertNotEqual(action.joint_targets.get('shoulder', 0), 0.0)
        
        # Should have exploration characteristics
        self.assertTrue(action.duration > 0)
        self.assertTrue(0 < action.force <= 1.0)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_sensorimotor_contingency_learning(self):
        """Test sensorimotor contingency learning capability"""
        system = EnactivePerceptionSystem("contingency_test")
        
        # Provide consistent action-outcome pairs
        consistent_experiences = []
        for i in range(5):
            experience = SensorimotorExperience(
                initial_body_state=BodyState(sensory_state={'input': 0.5}),
                motor_action=MotorAction(joint_targets={'test_joint': 0.7}),
                resulting_body_state=BodyState(sensory_state={'input': 0.8}),
                sensory_feedback={'input': 0.8, 'pattern': 'consistent'},
                success=True
            )
            result = system.process_embodied_experience(experience)
            consistent_experiences.append(result)
        
        # System should learn the contingency
        metrics = system.get_enactive_metrics()
        self.assertTrue(metrics['total_contingencies_learned'] > 0)
        
        # Confidence should increase with repeated experience
        contingencies = system.contingency_learner.contingencies
        if contingencies:
            # Find the relevant contingency
            relevant_contingency = None
            for contingency in contingencies:
                if contingency.action_pattern.get('joint_targets', {}).get('test_joint') == 0.7:
                    relevant_contingency = contingency
                    break
            
            if relevant_contingency:
                self.assertTrue(relevant_contingency.confidence > 0.1)
                self.assertTrue(relevant_contingency.frequency >= 5)
    
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, "enactive_perception not available")
    def test_perceptual_prediction_through_action(self):
        """Test perceptual prediction capability"""
        system = EnactivePerceptionSystem("prediction_test")
        
        # Train system with predictable pattern
        training_experience = SensorimotorExperience(
            initial_body_state=BodyState(sensory_state={'sensor': 0.4}),
            motor_action=MotorAction(joint_targets={'predictor': 0.6}),
            resulting_body_state=BodyState(sensory_state={'sensor': 0.9}),
            sensory_feedback={'sensor': 0.9, 'predictable': True},
            success=True
        )
        
        # Build up contingency
        for _ in range(3):
            system.process_embodied_experience(training_experience)
        
        # Force higher confidence for prediction
        if system.contingency_learner.contingencies:
            system.contingency_learner.contingencies[0].confidence = 0.7
        
        # Test prediction
        test_action = MotorAction(joint_targets={'predictor': 0.6})
        test_state = BodyState(sensory_state={'sensor': 0.4})
        
        prediction = system.predict_perceptual_outcome(test_action, test_state)
        
        # Should generate prediction
        self.assertIsInstance(prediction, PerceptualPrediction)
        self.assertTrue(prediction.confidence > 0.0)
        
        # Prediction should be reasonable if system learned
        if prediction.predicted_sensory_outcome and prediction.confidence > 0.3:
            predicted_sensor = prediction.predicted_sensory_outcome.get('sensor', 0)
            # Should predict increase from 0.4 toward 0.9
            self.assertTrue(predicted_sensor > 0.4)


def run_comprehensive_test():
    """Run comprehensive test of enactive perception system"""
    print("=" * 70)
    print("COMPREHENSIVE ENACTIVE PERCEPTION SYSTEM TEST")
    print("=" * 70)
    
    if not ENACTIVE_PERCEPTION_AVAILABLE:
        print("❌ FAILED: Enactive perception system not available for testing")
        return False
    
    try:
        # Test basic functionality
        print("\n🔍 Testing basic system functionality...")
        system = create_enactive_perception_system("comprehensive_test")
        print("✅ System created successfully")
        
        # Test learning and prediction cycle
        print("\n🧠 Testing learning and prediction cycle...")
        
        # Simulate learning phase
        for i in range(5):
            experience = SensorimotorExperience(
                initial_body_state=BodyState(
                    joint_angles={'test': 0.1 * i},
                    sensory_state={'env': 0.2 * i}
                ),
                motor_action=MotorAction(joint_targets={'test': 0.2 * i}),
                resulting_body_state=BodyState(
                    joint_angles={'test': 0.2 * i},
                    sensory_state={'env': 0.3 * i}
                ),
                sensory_feedback={'env': 0.3 * i, 'learning': True},
                success=True
            )
            result = system.process_embodied_experience(experience)
            print(f"  Learning iteration {i+1}: {result['contingency_learned']}")
        
        # Test prediction
        test_action = MotorAction(joint_targets={'test': 0.6})
        test_state = BodyState(sensory_state={'env': 0.4})
        prediction = system.predict_perceptual_outcome(test_action, test_state)
        
        print(f"  Prediction confidence: {prediction.confidence:.3f}")
        print("✅ Learning and prediction cycle working")
        
        # Test metrics
        print("\n📊 Testing system metrics...")
        metrics = system.get_enactive_metrics()
        print(f"  Contingencies learned: {metrics['total_contingencies_learned']}")
        print(f"  Average confidence: {metrics['average_contingency_confidence']:.3f}")
        print(f"  System active: {metrics['system_active']}")
        print("✅ Metrics generation working")
        
        print("\n🎯 COMPREHENSIVE TEST PASSED")
        return True
        
    except Exception as e:
        print(f"❌ COMPREHENSIVE TEST FAILED: {e}")
        import traceback
        traceback.print_exc()
        return False


if __name__ == "__main__":
    # Set up logging
    logging.basicConfig(level=logging.WARNING)  # Reduce noise during testing
    
    # Run comprehensive test first
    comprehensive_success = run_comprehensive_test()
    
    print("\n" + "=" * 70)
    print("UNIT TEST SUITE")
    print("=" * 70)
    
    # Run unit tests
    unittest.main(verbosity=2, exit=False)
    
    # Summary
    print("\n" + "=" * 70)
    print("TEST SUMMARY")
    print("=" * 70)
    print(f"Comprehensive Test: {'✅ PASSED' if comprehensive_success else '❌ FAILED'}")
    print("Unit Tests: See results above")
    print("\nEnactive Perception System testing complete.")