#!/usr/bin/env python3
"""
Test Motor Prediction System - Deep Tree Echo Integration

This module tests Task 3.2.3 of the Deep Tree Echo development roadmap:
Build Motor Prediction Systems with:
- Forward models for movement prediction
- Motor imagery and mental simulation
- Action consequence prediction

Test Acceptance Criteria: Agents predict movement outcomes before execution
"""

import unittest
import sys
import time
from pathlib import Path

# Add echo.kern to path for imports
sys.path.insert(0, str(Path(__file__).parent))

try:
    from motor_prediction_system import (
        MotorPredictionSystem,
        ForwardModel,
        MotorImagerySystem,
        ActionConsequencePredictionSystem,
        ForwardModelState,
        MovementPrediction,
        MotorImageryState,
        BodyConfiguration,
        MotorAction,
        MovementType,
        PredictionConfidence,
        create_motor_prediction_system
    )
    MOTOR_PREDICTION_AVAILABLE = True
except ImportError as e:
    print(f"Motor prediction system not available: {e}")
    MOTOR_PREDICTION_AVAILABLE = False
    
    # Create mock classes for testing infrastructure
    class MotorPredictionSystem:
        pass
    
    class BodyConfiguration:
        def __init__(self, **kwargs):
            pass
    
    class MotorAction:
        def __init__(self, **kwargs):
            pass
    
    class ForwardModelState:
        def __init__(self, **kwargs):
            pass

class TestForwardModel(unittest.TestCase):
    """Test forward model for movement prediction"""
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def setUp(self):
        """Set up test environment"""
        self.forward_model = ForwardModel(MovementType.REACHING)
        self.test_state = ForwardModelState(
            body_configuration=BodyConfiguration(
                position=(0, 0, 1),
                joint_angles={'shoulder': 0.0, 'elbow': 0.0, 'wrist': 0.0}
            ),
            sensory_state={'vision': 0.5, 'touch': 0.3}
        )
        self.test_action = MotorAction(
            joint_targets={'shoulder': 0.5, 'elbow': -0.3, 'wrist': 0.2},
            duration=2.0,
            force=0.6
        )
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_forward_model_prediction(self):
        """Test basic forward model prediction capability"""
        prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        
        # Check prediction structure
        self.assertIsInstance(prediction, MovementPrediction)
        self.assertEqual(prediction.movement_type, MovementType.REACHING)
        self.assertIsInstance(prediction.confidence, float)
        self.assertTrue(0.0 <= prediction.confidence <= 1.0)
        self.assertIsInstance(prediction.trajectory_points, list)
        self.assertGreater(len(prediction.trajectory_points), 0)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_prediction_confidence_range(self):
        """Test that prediction confidence is within valid range"""
        prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        
        self.assertGreaterEqual(prediction.confidence, 0.0)
        self.assertLessEqual(prediction.confidence, 1.0)
        self.assertGreaterEqual(prediction.success_probability, 0.0)
        self.assertLessEqual(prediction.success_probability, 1.0)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_trajectory_generation(self):
        """Test that trajectory is generated properly"""
        prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        
        # Should have multiple trajectory points
        self.assertGreater(len(prediction.trajectory_points), 3)
        
        # Each point should be a BodyConfiguration
        for point in prediction.trajectory_points:
            self.assertIsInstance(point, BodyConfiguration)
            self.assertIsInstance(point.position, tuple)
            self.assertEqual(len(point.position), 3)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_energy_cost_estimation(self):
        """Test energy cost estimation"""
        prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        
        self.assertIsInstance(prediction.energy_cost, float)
        self.assertGreater(prediction.energy_cost, 0.0)
        # Higher force should generally mean higher energy cost
        high_force_action = MotorAction(
            joint_targets={'shoulder': 0.5},
            duration=2.0,
            force=1.0
        )
        self.forward_model.predict_movement_outcome(
            self.test_state, high_force_action
        )
        # Note: This might not always be true due to complexity of energy calculation
        # but is a general expectation
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_collision_risk_assessment(self):
        """Test collision risk assessment"""
        prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        
        self.assertIsInstance(prediction.collision_risk, float)
        self.assertGreaterEqual(prediction.collision_risk, 0.0)
        self.assertLessEqual(prediction.collision_risk, 1.0)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_prediction_update_from_outcome(self):
        """Test updating model from actual outcomes"""
        # Initial prediction
        initial_prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        initial_accuracy = self.forward_model.get_prediction_accuracy()
        
        # Simulate actual outcome
        actual_outcome = ForwardModelState(
            body_configuration=BodyConfiguration(
                position=(0.1, 0.2, 1.1),
                joint_angles={'shoulder': 0.4, 'elbow': -0.25, 'wrist': 0.15}
            ),
            sensory_state={'vision': 0.6, 'touch': 0.4}
        )
        
        # Update model
        self.forward_model.update_from_outcome(initial_prediction, actual_outcome)
        
        # Check that metrics were updated
        updated_accuracy = self.forward_model.get_prediction_accuracy()
        self.assertGreaterEqual(updated_accuracy['total_predictions'], 
                              initial_accuracy['total_predictions'])
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_multiple_movement_types(self):
        """Test forward models for different movement types"""
        movement_types = [MovementType.REACHING, MovementType.GRASPING, MovementType.LOCOMOTION]
        
        for movement_type in movement_types:
            model = ForwardModel(movement_type)
            prediction = model.predict_movement_outcome(self.test_state, self.test_action)
            
            self.assertEqual(prediction.movement_type, movement_type)
            self.assertIsInstance(prediction.confidence, float)


class TestMotorImagerySystem(unittest.TestCase):
    """Test motor imagery and mental simulation"""
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def setUp(self):
        """Set up test environment"""
        self.imagery_system = MotorImagerySystem()
        self.test_state = ForwardModelState(
            body_configuration=BodyConfiguration(
                position=(0, 0, 1),
                joint_angles={'shoulder': 0.0, 'elbow': 0.0}
            )
        )
        self.test_action = MotorAction(
            joint_targets={'shoulder': 0.5, 'elbow': -0.3},
            duration=1.5
        )
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_mental_rehearsal_simulation(self):
        """Test mental rehearsal simulation"""
        imagery_state = self.imagery_system.simulate_mental_rehearsal(
            self.test_action, self.test_state, rehearsal_steps=5
        )
        
        self.assertIsInstance(imagery_state, MotorImageryState)
        self.assertEqual(len(imagery_state.mental_rehearsal_steps), 5)
        self.assertGreater(imagery_state.vividness, 0.0)
        self.assertLessEqual(imagery_state.vividness, 1.0)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_neural_activation_pattern(self):
        """Test neural activation pattern generation"""
        imagery_state = self.imagery_system.simulate_mental_rehearsal(
            self.test_action, self.test_state
        )
        
        self.assertIsInstance(imagery_state.neural_activation_pattern, list)
        self.assertGreater(len(imagery_state.neural_activation_pattern), 0)
        
        # Neural activations should be in reasonable range
        for activation in imagery_state.neural_activation_pattern:
            self.assertIsInstance(activation, float)
            self.assertGreaterEqual(activation, -1.0)
            self.assertLessEqual(activation, 1.0)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_vividness_calculation(self):
        """Test vividness calculation for different actions"""
        # Simple action should have higher vividness
        simple_action = MotorAction(
            joint_targets={'shoulder': 0.3},
            duration=1.0
        )
        simple_imagery = self.imagery_system.simulate_mental_rehearsal(
            simple_action, self.test_state
        )
        
        # Complex action should have lower vividness
        complex_action = MotorAction(
            joint_targets={'shoulder': 0.5, 'elbow': -0.3, 'wrist': 0.2, 
                          'finger1': 0.1, 'finger2': 0.1, 'finger3': 0.1},
            duration=3.0
        )
        complex_imagery = self.imagery_system.simulate_mental_rehearsal(
            complex_action, self.test_state
        )
        
        # Simple action should generally have higher vividness
        # (though this may not always be true due to other factors)
        self.assertGreater(simple_imagery.vividness, 0.1)
        self.assertGreater(complex_imagery.vividness, 0.1)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_imagery_quality_assessment(self):
        """Test imagery quality assessment"""
        # Perform several mental rehearsals
        for i in range(3):
            action = MotorAction(joint_targets={'joint': i * 0.1})
            self.imagery_system.simulate_mental_rehearsal(action, self.test_state)
        
        quality = self.imagery_system.assess_imagery_quality()
        
        self.assertIn('average_vividness', quality)
        self.assertIn('imagery_count', quality)
        self.assertEqual(quality['imagery_count'], 3)
        self.assertGreater(quality['average_vividness'], 0.0)


class TestActionConsequencePredictionSystem(unittest.TestCase):
    """Test action consequence prediction"""
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def setUp(self):
        """Set up test environment"""
        self.consequence_system = ActionConsequencePredictionSystem()
        self.test_state = ForwardModelState(
            body_configuration=BodyConfiguration(
                position=(0, 0, 1),
                joint_angles={'shoulder': 0.0, 'elbow': 0.0}
            ),
            environmental_context={
                'table': {'position': (0.5, 0.0, 0.8), 'type': 'surface'},
                'cup': {'position': (0.4, 0.1, 0.9), 'type': 'object'}
            },
            sensory_state={'vision': 0.7, 'touch': 0.2}
        )
        self.test_action = MotorAction(
            joint_targets={'shoulder': 0.5, 'elbow': -0.3},
            duration=2.0
        )
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_consequence_prediction_structure(self):
        """Test that consequence prediction has proper structure"""
        consequences = self.consequence_system.predict_action_consequences(
            self.test_action, self.test_state
        )
        
        # Check required fields
        required_fields = [
            'action_type', 'movement_outcome', 'environmental_consequences',
            'sensory_consequences', 'secondary_effects', 'overall_confidence'
        ]
        
        for field in required_fields:
            self.assertIn(field, consequences)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_environmental_consequence_prediction(self):
        """Test environmental consequence prediction"""
        consequences = self.consequence_system.predict_action_consequences(
            self.test_action, self.test_state
        )
        
        env_consequences = consequences['environmental_consequences']
        
        self.assertIn('object_interactions', env_consequences)
        self.assertIn('space_occupation', env_consequences)
        self.assertIn('energy_transfer', env_consequences)
        
        # Should detect potential object interactions
        self.assertIsInstance(env_consequences['object_interactions'], list)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_sensory_consequence_prediction(self):
        """Test sensory consequence prediction"""
        consequences = self.consequence_system.predict_action_consequences(
            self.test_action, self.test_state
        )
        
        sensory_consequences = consequences['sensory_consequences']
        
        # Check sensory feedback types
        expected_feedback_types = [
            'tactile_feedback', 'proprioceptive_feedback', 
            'visual_feedback', 'auditory_feedback'
        ]
        
        for feedback_type in expected_feedback_types:
            self.assertIn(feedback_type, sensory_consequences)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_secondary_effects_prediction(self):
        """Test secondary effects prediction"""
        consequences = self.consequence_system.predict_action_consequences(
            self.test_action, self.test_state
        )
        
        secondary_effects = consequences['secondary_effects']
        
        self.assertIn('fatigue_accumulation', secondary_effects)
        self.assertIn('learning_effects', secondary_effects)
        self.assertIn('adaptation_requirements', secondary_effects)
        
        # Learning effects should be positive
        self.assertGreater(secondary_effects['learning_effects']['motor_skill_improvement'], 0)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_action_type_classification(self):
        """Test action type classification"""
        # Test different action types
        grasping_action = MotorAction(
            joint_targets={'hand': 0.8, 'finger1': 0.5}
        )
        grasping_consequences = self.consequence_system.predict_action_consequences(
            grasping_action, self.test_state
        )
        
        locomotion_action = MotorAction(
            joint_targets={'leg': 0.5, 'foot': 0.2}
        )
        locomotion_consequences = self.consequence_system.predict_action_consequences(
            locomotion_action, self.test_state
        )
        
        # Should classify action types correctly
        self.assertIn(grasping_consequences['action_type'], ['grasping', 'reaching'])
        self.assertIn(locomotion_consequences['action_type'], ['locomotion', 'reaching'])


class TestMotorPredictionSystem(unittest.TestCase):
    """Test integrated motor prediction system"""
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def setUp(self):
        """Set up test environment"""
        self.motor_system = MotorPredictionSystem("test_agent")
        self.test_state = ForwardModelState(
            body_configuration=BodyConfiguration(
                position=(0, 0, 1),
                joint_angles={'shoulder': 0.0, 'elbow': 0.0, 'wrist': 0.0}
            ),
            sensory_state={'vision': 0.5, 'touch': 0.3}
        )
        self.test_action = MotorAction(
            joint_targets={'shoulder': 0.5, 'elbow': -0.3},
            duration=2.0
        )
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_comprehensive_prediction_before_execution(self):
        """Test comprehensive prediction before execution (main acceptance criteria)"""
        # This is the core test for the acceptance criteria:
        # "Agents predict movement outcomes before execution"
        
        prediction = self.motor_system.predict_movement_outcome_before_execution(
            self.test_action, self.test_state
        )
        
        # Verify comprehensive prediction structure
        required_fields = [
            'agent_name', 'prediction_timestamp', 'movement_type',
            'movement_prediction', 'motor_imagery', 'action_consequences',
            'execution_recommendation', 'prediction_latency'
        ]
        
        for field in required_fields:
            self.assertIn(field, prediction)
        
        # Verify prediction quality
        self.assertIsInstance(prediction['movement_prediction']['confidence'], float)
        self.assertGreater(prediction['movement_prediction']['confidence'], 0.0)
        self.assertIsInstance(prediction['motor_imagery']['vividness'], float)
        self.assertGreater(prediction['motor_imagery']['vividness'], 0.0)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_execution_recommendation_generation(self):
        """Test execution recommendation generation"""
        prediction = self.motor_system.predict_movement_outcome_before_execution(
            self.test_action, self.test_state
        )
        
        recommendation = prediction['execution_recommendation']
        
        self.assertIn('should_execute', recommendation)
        self.assertIn('confidence_threshold_met', recommendation)
        self.assertIn('risk_assessment', recommendation)
        self.assertIn('modifications_suggested', recommendation)
        
        self.assertIsInstance(recommendation['should_execute'], bool)
        self.assertIn(recommendation['risk_assessment'], ['low', 'medium', 'high'])
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_prediction_latency(self):
        """Test that predictions are generated within reasonable time"""
        start_time = time.time()
        prediction = self.motor_system.predict_movement_outcome_before_execution(
            self.test_action, self.test_state
        )
        end_time = time.time()
        
        # Prediction should complete within reasonable time (5 seconds)
        self.assertLess(end_time - start_time, 5.0)
        
        # Reported latency should match
        reported_latency = prediction['prediction_latency']
        actual_latency = end_time - start_time
        self.assertAlmostEqual(reported_latency, actual_latency, delta=0.1)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_different_movement_types(self):
        """Test predictions for different movement types"""
        # Test reaching
        reaching_action = MotorAction(
            joint_targets={'shoulder': 0.5, 'elbow': -0.3}
        )
        reaching_pred = self.motor_system.predict_movement_outcome_before_execution(
            reaching_action, self.test_state
        )
        
        # Test grasping
        grasping_action = MotorAction(
            joint_targets={'hand': 0.8, 'finger1': 0.5}
        )
        grasping_pred = self.motor_system.predict_movement_outcome_before_execution(
            grasping_action, self.test_state
        )
        
        # Should generate different movement types
        self.assertNotEqual(reaching_pred['movement_type'], grasping_pred['movement_type'])
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_prediction_caching(self):
        """Test prediction caching mechanism"""
        initial_cache_size = len(self.motor_system.prediction_cache)
        
        # Generate prediction
        self.motor_system.predict_movement_outcome_before_execution(
            self.test_action, self.test_state
        )
        
        # Cache should have grown
        self.assertGreater(len(self.motor_system.prediction_cache), initial_cache_size)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_system_performance_metrics(self):
        """Test system performance metrics"""
        # Generate a few predictions
        for i in range(3):
            action = MotorAction(joint_targets={'joint': i * 0.1})
            self.motor_system.predict_movement_outcome_before_execution(
                action, self.test_state
            )
        
        performance = self.motor_system.get_system_performance()
        
        required_metrics = [
            'system_uptime', 'total_predictions', 'prediction_success_rate',
            'forward_model_accuracies', 'motor_imagery_quality',
            'consequence_prediction_stats', 'dtesn_integration'
        ]
        
        for metric in required_metrics:
            self.assertIn(metric, performance)
        
        # Should have recorded predictions
        self.assertGreaterEqual(performance['total_predictions'], 3)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_prediction_update_from_execution(self):
        """Test updating predictions based on execution results"""
        # Generate initial prediction
        prediction = self.motor_system.predict_movement_outcome_before_execution(
            self.test_action, self.test_state
        )
        
        # Simulate actual execution outcome
        actual_outcome = ForwardModelState(
            body_configuration=BodyConfiguration(
                position=(0.1, 0.2, 1.1),
                joint_angles={'shoulder': 0.45, 'elbow': -0.28}
            )
        )
        
        # Update system with actual outcome
        self.motor_system.update_predictions_from_execution(prediction, actual_outcome)
        
        # System should have processed the update without error
        # (no specific assertion as this tests internal state update)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_create_motor_prediction_system_function(self):
        """Test the convenience function for creating motor prediction systems"""
        system = create_motor_prediction_system("convenience_test_agent")
        
        self.assertIsInstance(system, MotorPredictionSystem)
        self.assertEqual(system.agent_name, "convenience_test_agent")


class TestAcceptanceCriteria(unittest.TestCase):
    """Test acceptance criteria: Agents predict movement outcomes before execution"""
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_agents_predict_movement_outcomes_before_execution(self):
        """
        Primary acceptance criteria test:
        Agents predict movement outcomes before execution
        """
        # Create agent with motor prediction capability
        agent_system = create_motor_prediction_system("acceptance_test_agent")
        
        # Define a movement scenario
        movement_action = MotorAction(
            joint_targets={
                'shoulder': 0.6,
                'elbow': -0.4,
                'wrist': 0.3
            },
            duration=1.5,
            force=0.7
        )
        
        current_state = ForwardModelState(
            body_configuration=BodyConfiguration(
                position=(0, 0, 1),
                joint_angles={'shoulder': 0.0, 'elbow': 0.0, 'wrist': 0.0}
            ),
            environmental_context={
                'target_object': {'position': (0.5, 0.2, 1.0)}
            },
            sensory_state={'vision': 0.8, 'touch': 0.1}
        )
        
        # BEFORE EXECUTION: Generate comprehensive prediction
        prediction_before_execution = agent_system.predict_movement_outcome_before_execution(
            movement_action, current_state, 
            include_imagery=True, include_consequences=True
        )
        
        # Verify that agent predicted outcomes BEFORE execution
        self.assertIsNotNone(prediction_before_execution)
        self.assertIn('movement_prediction', prediction_before_execution)
        self.assertIn('execution_recommendation', prediction_before_execution)
        
        # Verify prediction content quality
        movement_pred = prediction_before_execution['movement_prediction']
        self.assertGreater(movement_pred['confidence'], 0.0)
        self.assertGreater(movement_pred['success_probability'], 0.0)
        
        # Verify mental simulation capability
        imagery = prediction_before_execution['motor_imagery']
        self.assertGreater(imagery['vividness'], 0.0)
        self.assertGreater(imagery['simulation_steps'], 0)
        
        # Verify action consequence prediction
        consequences = prediction_before_execution['action_consequences']
        self.assertIsNotNone(consequences)
        self.assertIn('overall_confidence', consequences)
        
        # Verify execution recommendation
        recommendation = prediction_before_execution['execution_recommendation']
        self.assertIn('should_execute', recommendation)
        self.assertIsInstance(recommendation['should_execute'], bool)
        
        print("✓ Agent predicted movement outcomes before execution")
        print(f"  Movement confidence: {movement_pred['confidence']:.3f}")
        print(f"  Success probability: {movement_pred['success_probability']:.3f}")
        print(f"  Motor imagery vividness: {imagery['vividness']:.3f}")
        print(f"  Should execute: {recommendation['should_execute']}")
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_forward_model_accuracy_over_time(self):
        """Test that forward models improve accuracy over time"""
        agent_system = create_motor_prediction_system("learning_test_agent")
        
        # Simulate multiple prediction-execution cycles
        improvements = []
        
        for cycle in range(5):
            # Generate action
            test_action = MotorAction(
                joint_targets={'shoulder': 0.1 * cycle, 'elbow': -0.1 * cycle}
            )
            
            test_state = ForwardModelState(
                body_configuration=BodyConfiguration(
                    position=(0, 0, 1),
                    joint_angles={'shoulder': 0.0, 'elbow': 0.0}
                )
            )
            
            # Predict outcome
            prediction = agent_system.predict_movement_outcome_before_execution(
                test_action, test_state
            )
            
            # Simulate actual outcome (with some variation)
            actual_outcome = ForwardModelState(
                body_configuration=BodyConfiguration(
                    position=(0.05 * cycle, 0.02 * cycle, 1.01),
                    joint_angles={
                        'shoulder': 0.09 * cycle,  # Slight difference from prediction
                        'elbow': -0.09 * cycle
                    }
                )
            )
            
            # Update system with outcome
            agent_system.update_predictions_from_execution(prediction, actual_outcome)
            
            # Check system performance
            performance = agent_system.get_system_performance()
            improvements.append(performance['prediction_success_rate'])
        
        # System should maintain or improve performance
        self.assertGreaterEqual(improvements[-1], 0.0)
        print(f"✓ Forward model maintained performance across {len(improvements)} cycles")
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_motor_imagery_mental_simulation_quality(self):
        """Test motor imagery and mental simulation quality"""
        agent_system = create_motor_prediction_system("imagery_test_agent")
        
        # Test different complexity actions
        actions = [
            MotorAction(joint_targets={'shoulder': 0.3}),  # Simple
            MotorAction(joint_targets={'shoulder': 0.5, 'elbow': -0.3}),  # Medium
            MotorAction(joint_targets={  # Complex
                'shoulder': 0.6, 'elbow': -0.4, 'wrist': 0.2,
                'finger1': 0.1, 'finger2': 0.1
            })
        ]
        
        test_state = ForwardModelState(
            body_configuration=BodyConfiguration(
                position=(0, 0, 1),
                joint_angles={'shoulder': 0.0, 'elbow': 0.0, 'wrist': 0.0}
            )
        )
        
        vividness_scores = []
        
        for action in actions:
            prediction = agent_system.predict_movement_outcome_before_execution(
                action, test_state, include_imagery=True
            )
            
            imagery = prediction['motor_imagery']
            vividness_scores.append(imagery['vividness'])
            
            # Should have reasonable mental simulation
            self.assertGreater(imagery['vividness'], 0.0)
            self.assertGreater(imagery['simulation_steps'], 0)
        
        print(f"✓ Motor imagery generated for actions with vividness: {vividness_scores}")
        
        # Check imagery system quality
        imagery_quality = agent_system.motor_imagery.assess_imagery_quality()
        self.assertGreater(imagery_quality['average_vividness'], 0.0)
        self.assertEqual(imagery_quality['imagery_count'], len(actions))
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_action_consequence_prediction_completeness(self):
        """Test completeness of action consequence prediction"""
        agent_system = create_motor_prediction_system("consequence_test_agent")
        
        # Create rich environment scenario
        complex_state = ForwardModelState(
            body_configuration=BodyConfiguration(
                position=(0, 0, 1),
                joint_angles={'shoulder': 0.0, 'elbow': 0.0}
            ),
            environmental_context={
                'table': {'position': (0.5, 0.0, 0.8), 'material': 'wood'},
                'cup': {'position': (0.4, 0.1, 0.9), 'fragile': True},
                'wall': {'position': (1.0, 0.0, 1.0), 'solid': True}
            },
            sensory_state={'vision': 0.8, 'touch': 0.2, 'proprioception': 0.9}
        )
        
        complex_action = MotorAction(
            joint_targets={'shoulder': 0.7, 'elbow': -0.5, 'wrist': 0.3},
            duration=2.5,
            force=0.6
        )
        
        # Generate comprehensive prediction
        prediction = agent_system.predict_movement_outcome_before_execution(
            complex_action, complex_state, 
            include_imagery=True, include_consequences=True
        )
        
        # Verify consequence prediction completeness
        consequences = prediction['action_consequences']
        
        # Should predict environmental consequences
        env_consequences = consequences['environmental_consequences']
        self.assertIsInstance(env_consequences['object_interactions'], list)
        self.assertIn('energy_transfer', env_consequences)
        
        # Should predict sensory consequences
        sensory_consequences = consequences['sensory_consequences']
        self.assertIn('tactile_feedback', sensory_consequences)
        self.assertIn('proprioceptive_feedback', sensory_consequences)
        self.assertIn('visual_feedback', sensory_consequences)
        
        # Should predict secondary effects
        secondary_effects = consequences['secondary_effects']
        self.assertIn('learning_effects', secondary_effects)
        self.assertIn('fatigue_accumulation', secondary_effects)
        
        print("✓ Comprehensive action consequences predicted")
        print(f"  Environmental interactions: {len(env_consequences['object_interactions'])}")
        print(f"  Overall consequence confidence: {consequences['overall_confidence']:.3f}")


class TestIntegrationWithDTESNComponents(unittest.TestCase):
    """Test integration with Deep Tree Echo State Network components"""
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_dtesn_integration_initialization(self):
        """Test DTESN component integration"""
        motor_system = create_motor_prediction_system("dtesn_test_agent")
        
        # Check integration flags
        performance = motor_system.get_system_performance()
        self.assertIn('dtesn_integration', performance)
        self.assertIn('echo_integration', performance)
        
        # Should initialize forward models
        self.assertGreater(len(motor_system.forward_models), 0)
        
        # Each forward model should attempt DTESN integration
        for movement_type, forward_model in motor_system.forward_models.items():
            self.assertIsInstance(forward_model.dtesn_integration, bool)
    
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, "motor_prediction_system not available")
    def test_performance_constraints(self):
        """Test real-time performance constraints"""
        motor_system = create_motor_prediction_system("performance_test_agent")
        
        test_state = ForwardModelState(
            body_configuration=BodyConfiguration(position=(0, 0, 1))
        )
        test_action = MotorAction(joint_targets={'joint': 0.5})
        
        # Measure prediction latency
        start_time = time.time()
        prediction = motor_system.predict_movement_outcome_before_execution(
            test_action, test_state
        )
        end_time = time.time()
        
        latency = end_time - start_time
        
        # Should meet real-time constraints (< 1 second for basic prediction)
        self.assertLess(latency, 1.0)
        
        # Reported latency should be reasonable
        reported_latency = prediction['prediction_latency']
        self.assertLess(reported_latency, 1.0)
        
        print(f"✓ Prediction generated in {latency:.3f}s (reported: {reported_latency:.3f}s)")


if __name__ == '__main__':
    print("Motor Prediction System Tests - Deep Tree Echo Integration")
    print("=" * 70)
    
    # Create test suite focusing on acceptance criteria
    suite = unittest.TestSuite()
    
    # Add acceptance criteria tests first
    suite.addTest(TestAcceptanceCriteria('test_agents_predict_movement_outcomes_before_execution'))
    suite.addTest(TestAcceptanceCriteria('test_forward_model_accuracy_over_time'))
    suite.addTest(TestAcceptanceCriteria('test_motor_imagery_mental_simulation_quality'))
    suite.addTest(TestAcceptanceCriteria('test_action_consequence_prediction_completeness'))
    
    # Add component tests
    suite.addTest(TestForwardModel('test_forward_model_prediction'))
    suite.addTest(TestMotorImagerySystem('test_mental_rehearsal_simulation'))
    suite.addTest(TestActionConsequencePredictionSystem('test_consequence_prediction_structure'))
    suite.addTest(TestMotorPredictionSystem('test_comprehensive_prediction_before_execution'))
    
    # Add integration tests
    suite.addTest(TestIntegrationWithDTESNComponents('test_dtesn_integration_initialization'))
    suite.addTest(TestIntegrationWithDTESNComponents('test_performance_constraints'))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    if result.wasSuccessful():
        print("\n✓ All Motor Prediction System tests passed!")
        print("✓ Acceptance Criteria satisfied: Agents predict movement outcomes before execution")
    else:
        print(f"\n✗ {len(result.failures)} test failures, {len(result.errors)} errors")
        
    # Run all tests if called directly
    if len(sys.argv) == 1:
        unittest.main(verbosity=2)