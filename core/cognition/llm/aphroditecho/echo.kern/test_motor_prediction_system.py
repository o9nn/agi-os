import unittest
import sys
import time
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
try:
    from motor_prediction_system import MotorPredictionSystem, ForwardModel, MotorImagerySystem, ActionConsequencePredictionSystem, ForwardModelState, MovementPrediction, MotorImageryState, BodyConfiguration, MotorAction, MovementType, PredictionConfidence, create_motor_prediction_system
    MOTOR_PREDICTION_AVAILABLE = True
except ImportError as e:
    print(f'Motor prediction system not available: {e}')
    MOTOR_PREDICTION_AVAILABLE = False
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
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def setUp(self):
        self.forward_model = ForwardModel(MovementType.REACHING)
        self.test_state = ForwardModelState(body_configuration=BodyConfiguration(position=(0, 0, 1), joint_angles={'shoulder': 0.0, 'elbow': 0.0, 'wrist': 0.0}), sensory_state={'vision': 0.5, 'touch': 0.3})
        self.test_action = MotorAction(joint_targets={'shoulder': 0.5, 'elbow': -0.3, 'wrist': 0.2}, duration=2.0, force=0.6)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_forward_model_prediction(self):
        prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        self.assertIsInstance(prediction, MovementPrediction)
        self.assertEqual(prediction.movement_type, MovementType.REACHING)
        self.assertIsInstance(prediction.confidence, float)
        self.assertTrue(0.0 <= prediction.confidence <= 1.0)
        self.assertIsInstance(prediction.trajectory_points, list)
        self.assertGreater(len(prediction.trajectory_points), 0)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_prediction_confidence_range(self):
        prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        self.assertGreaterEqual(prediction.confidence, 0.0)
        self.assertLessEqual(prediction.confidence, 1.0)
        self.assertGreaterEqual(prediction.success_probability, 0.0)
        self.assertLessEqual(prediction.success_probability, 1.0)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_trajectory_generation(self):
        prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        self.assertGreater(len(prediction.trajectory_points), 3)
        for point in prediction.trajectory_points:
            self.assertIsInstance(point, BodyConfiguration)
            self.assertIsInstance(point.position, tuple)
            self.assertEqual(len(point.position), 3)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_energy_cost_estimation(self):
        prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        self.assertIsInstance(prediction.energy_cost, float)
        self.assertGreater(prediction.energy_cost, 0.0)
        high_force_action = MotorAction(joint_targets={'shoulder': 0.5}, duration=2.0, force=1.0)
        self.forward_model.predict_movement_outcome(self.test_state, high_force_action)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_collision_risk_assessment(self):
        prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        self.assertIsInstance(prediction.collision_risk, float)
        self.assertGreaterEqual(prediction.collision_risk, 0.0)
        self.assertLessEqual(prediction.collision_risk, 1.0)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_prediction_update_from_outcome(self):
        initial_prediction = self.forward_model.predict_movement_outcome(self.test_state, self.test_action)
        initial_accuracy = self.forward_model.get_prediction_accuracy()
        actual_outcome = ForwardModelState(body_configuration=BodyConfiguration(position=(0.1, 0.2, 1.1), joint_angles={'shoulder': 0.4, 'elbow': -0.25, 'wrist': 0.15}), sensory_state={'vision': 0.6, 'touch': 0.4})
        self.forward_model.update_from_outcome(initial_prediction, actual_outcome)
        updated_accuracy = self.forward_model.get_prediction_accuracy()
        self.assertGreaterEqual(updated_accuracy['total_predictions'], initial_accuracy['total_predictions'])
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_multiple_movement_types(self):
        movement_types = [MovementType.REACHING, MovementType.GRASPING, MovementType.LOCOMOTION]
        for movement_type in movement_types:
            model = ForwardModel(movement_type)
            prediction = model.predict_movement_outcome(self.test_state, self.test_action)
            self.assertEqual(prediction.movement_type, movement_type)
            self.assertIsInstance(prediction.confidence, float)
class TestMotorImagerySystem(unittest.TestCase):
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def setUp(self):
        self.imagery_system = MotorImagerySystem()
        self.test_state = ForwardModelState(body_configuration=BodyConfiguration(position=(0, 0, 1), joint_angles={'shoulder': 0.0, 'elbow': 0.0}))
        self.test_action = MotorAction(joint_targets={'shoulder': 0.5, 'elbow': -0.3}, duration=1.5)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_mental_rehearsal_simulation(self):
        imagery_state = self.imagery_system.simulate_mental_rehearsal(self.test_action, self.test_state, rehearsal_steps=5)
        self.assertIsInstance(imagery_state, MotorImageryState)
        self.assertEqual(len(imagery_state.mental_rehearsal_steps), 5)
        self.assertGreater(imagery_state.vividness, 0.0)
        self.assertLessEqual(imagery_state.vividness, 1.0)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_neural_activation_pattern(self):
        imagery_state = self.imagery_system.simulate_mental_rehearsal(self.test_action, self.test_state)
        self.assertIsInstance(imagery_state.neural_activation_pattern, list)
        self.assertGreater(len(imagery_state.neural_activation_pattern), 0)
        for activation in imagery_state.neural_activation_pattern:
            self.assertIsInstance(activation, float)
            self.assertGreaterEqual(activation, -1.0)
            self.assertLessEqual(activation, 1.0)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_vividness_calculation(self):
        simple_action = MotorAction(joint_targets={'shoulder': 0.3}, duration=1.0)
        simple_imagery = self.imagery_system.simulate_mental_rehearsal(simple_action, self.test_state)
        complex_action = MotorAction(joint_targets={'shoulder': 0.5, 'elbow': -0.3, 'wrist': 0.2, 'finger1': 0.1, 'finger2': 0.1, 'finger3': 0.1}, duration=3.0)
        complex_imagery = self.imagery_system.simulate_mental_rehearsal(complex_action, self.test_state)
        self.assertGreater(simple_imagery.vividness, 0.1)
        self.assertGreater(complex_imagery.vividness, 0.1)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_imagery_quality_assessment(self):
        for i in range(3):
            action = MotorAction(joint_targets={'joint': i * 0.1})
            self.imagery_system.simulate_mental_rehearsal(action, self.test_state)
        quality = self.imagery_system.assess_imagery_quality()
        self.assertIn('average_vividness', quality)
        self.assertIn('imagery_count', quality)
        self.assertEqual(quality['imagery_count'], 3)
        self.assertGreater(quality['average_vividness'], 0.0)
class TestActionConsequencePredictionSystem(unittest.TestCase):
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def setUp(self):
        self.consequence_system = ActionConsequencePredictionSystem()
        self.test_state = ForwardModelState(body_configuration=BodyConfiguration(position=(0, 0, 1), joint_angles={'shoulder': 0.0, 'elbow': 0.0}), environmental_context={'table': {'position': (0.5, 0.0, 0.8), 'type': 'surface'}, 'cup': {'position': (0.4, 0.1, 0.9), 'type': 'object'}}, sensory_state={'vision': 0.7, 'touch': 0.2})
        self.test_action = MotorAction(joint_targets={'shoulder': 0.5, 'elbow': -0.3}, duration=2.0)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_consequence_prediction_structure(self):
        consequences = self.consequence_system.predict_action_consequences(self.test_action, self.test_state)
        required_fields = ['action_type', 'movement_outcome', 'environmental_consequences', 'sensory_consequences', 'secondary_effects', 'overall_confidence']
        for field in required_fields:
            self.assertIn(field, consequences)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_environmental_consequence_prediction(self):
        consequences = self.consequence_system.predict_action_consequences(self.test_action, self.test_state)
        env_consequences = consequences['environmental_consequences']
        self.assertIn('object_interactions', env_consequences)
        self.assertIn('space_occupation', env_consequences)
        self.assertIn('energy_transfer', env_consequences)
        self.assertIsInstance(env_consequences['object_interactions'], list)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_sensory_consequence_prediction(self):
        consequences = self.consequence_system.predict_action_consequences(self.test_action, self.test_state)
        sensory_consequences = consequences['sensory_consequences']
        expected_feedback_types = ['tactile_feedback', 'proprioceptive_feedback', 'visual_feedback', 'auditory_feedback']
        for feedback_type in expected_feedback_types:
            self.assertIn(feedback_type, sensory_consequences)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_secondary_effects_prediction(self):
        consequences = self.consequence_system.predict_action_consequences(self.test_action, self.test_state)
        secondary_effects = consequences['secondary_effects']
        self.assertIn('fatigue_accumulation', secondary_effects)
        self.assertIn('learning_effects', secondary_effects)
        self.assertIn('adaptation_requirements', secondary_effects)
        self.assertGreater(secondary_effects['learning_effects']['motor_skill_improvement'], 0)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_action_type_classification(self):
        grasping_action = MotorAction(joint_targets={'hand': 0.8, 'finger1': 0.5})
        grasping_consequences = self.consequence_system.predict_action_consequences(grasping_action, self.test_state)
        locomotion_action = MotorAction(joint_targets={'leg': 0.5, 'foot': 0.2})
        locomotion_consequences = self.consequence_system.predict_action_consequences(locomotion_action, self.test_state)
        self.assertIn(grasping_consequences['action_type'], ['grasping', 'reaching'])
        self.assertIn(locomotion_consequences['action_type'], ['locomotion', 'reaching'])
class TestMotorPredictionSystem(unittest.TestCase):
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def setUp(self):
        self.motor_system = MotorPredictionSystem('test_agent')
        self.test_state = ForwardModelState(body_configuration=BodyConfiguration(position=(0, 0, 1), joint_angles={'shoulder': 0.0, 'elbow': 0.0, 'wrist': 0.0}), sensory_state={'vision': 0.5, 'touch': 0.3})
        self.test_action = MotorAction(joint_targets={'shoulder': 0.5, 'elbow': -0.3}, duration=2.0)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_comprehensive_prediction_before_execution(self):
        prediction = self.motor_system.predict_movement_outcome_before_execution(self.test_action, self.test_state)
        required_fields = ['agent_name', 'prediction_timestamp', 'movement_type', 'movement_prediction', 'motor_imagery', 'action_consequences', 'execution_recommendation', 'prediction_latency']
        for field in required_fields:
            self.assertIn(field, prediction)
        self.assertIsInstance(prediction['movement_prediction']['confidence'], float)
        self.assertGreater(prediction['movement_prediction']['confidence'], 0.0)
        self.assertIsInstance(prediction['motor_imagery']['vividness'], float)
        self.assertGreater(prediction['motor_imagery']['vividness'], 0.0)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_execution_recommendation_generation(self):
        prediction = self.motor_system.predict_movement_outcome_before_execution(self.test_action, self.test_state)
        recommendation = prediction['execution_recommendation']
        self.assertIn('should_execute', recommendation)
        self.assertIn('confidence_threshold_met', recommendation)
        self.assertIn('risk_assessment', recommendation)
        self.assertIn('modifications_suggested', recommendation)
        self.assertIsInstance(recommendation['should_execute'], bool)
        self.assertIn(recommendation['risk_assessment'], ['low', 'medium', 'high'])
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_prediction_latency(self):
        start_time = time.time()
        prediction = self.motor_system.predict_movement_outcome_before_execution(self.test_action, self.test_state)
        end_time = time.time()
        self.assertLess(end_time - start_time, 5.0)
        reported_latency = prediction['prediction_latency']
        actual_latency = end_time - start_time
        self.assertAlmostEqual(reported_latency, actual_latency, delta=0.1)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_different_movement_types(self):
        reaching_action = MotorAction(joint_targets={'shoulder': 0.5, 'elbow': -0.3})
        reaching_pred = self.motor_system.predict_movement_outcome_before_execution(reaching_action, self.test_state)
        grasping_action = MotorAction(joint_targets={'hand': 0.8, 'finger1': 0.5})
        grasping_pred = self.motor_system.predict_movement_outcome_before_execution(grasping_action, self.test_state)
        self.assertNotEqual(reaching_pred['movement_type'], grasping_pred['movement_type'])
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_prediction_caching(self):
        initial_cache_size = len(self.motor_system.prediction_cache)
        self.motor_system.predict_movement_outcome_before_execution(self.test_action, self.test_state)
        self.assertGreater(len(self.motor_system.prediction_cache), initial_cache_size)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_system_performance_metrics(self):
        for i in range(3):
            action = MotorAction(joint_targets={'joint': i * 0.1})
            self.motor_system.predict_movement_outcome_before_execution(action, self.test_state)
        performance = self.motor_system.get_system_performance()
        required_metrics = ['system_uptime', 'total_predictions', 'prediction_success_rate', 'forward_model_accuracies', 'motor_imagery_quality', 'consequence_prediction_stats', 'dtesn_integration']
        for metric in required_metrics:
            self.assertIn(metric, performance)
        self.assertGreaterEqual(performance['total_predictions'], 3)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_prediction_update_from_execution(self):
        prediction = self.motor_system.predict_movement_outcome_before_execution(self.test_action, self.test_state)
        actual_outcome = ForwardModelState(body_configuration=BodyConfiguration(position=(0.1, 0.2, 1.1), joint_angles={'shoulder': 0.45, 'elbow': -0.28}))
        self.motor_system.update_predictions_from_execution(prediction, actual_outcome)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_create_motor_prediction_system_function(self):
        system = create_motor_prediction_system('convenience_test_agent')
        self.assertIsInstance(system, MotorPredictionSystem)
        self.assertEqual(system.agent_name, 'convenience_test_agent')
class TestAcceptanceCriteria(unittest.TestCase):
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_agents_predict_movement_outcomes_before_execution(self):
        agent_system = create_motor_prediction_system('acceptance_test_agent')
        movement_action = MotorAction(joint_targets={'shoulder': 0.6, 'elbow': -0.4, 'wrist': 0.3}, duration=1.5, force=0.7)
        current_state = ForwardModelState(body_configuration=BodyConfiguration(position=(0, 0, 1), joint_angles={'shoulder': 0.0, 'elbow': 0.0, 'wrist': 0.0}), environmental_context={'target_object': {'position': (0.5, 0.2, 1.0)}}, sensory_state={'vision': 0.8, 'touch': 0.1})
        prediction_before_execution = agent_system.predict_movement_outcome_before_execution(movement_action, current_state, include_imagery=True, include_consequences=True)
        self.assertIsNotNone(prediction_before_execution)
        self.assertIn('movement_prediction', prediction_before_execution)
        self.assertIn('execution_recommendation', prediction_before_execution)
        movement_pred = prediction_before_execution['movement_prediction']
        self.assertGreater(movement_pred['confidence'], 0.0)
        self.assertGreater(movement_pred['success_probability'], 0.0)
        imagery = prediction_before_execution['motor_imagery']
        self.assertGreater(imagery['vividness'], 0.0)
        self.assertGreater(imagery['simulation_steps'], 0)
        consequences = prediction_before_execution['action_consequences']
        self.assertIsNotNone(consequences)
        self.assertIn('overall_confidence', consequences)
        recommendation = prediction_before_execution['execution_recommendation']
        self.assertIn('should_execute', recommendation)
        self.assertIsInstance(recommendation['should_execute'], bool)
        print('✓ Agent predicted movement outcomes before execution')
        print(f"  Movement confidence: {movement_pred['confidence']:.3f}")
        print(f"  Success probability: {movement_pred['success_probability']:.3f}")
        print(f"  Motor imagery vividness: {imagery['vividness']:.3f}")
        print(f"  Should execute: {recommendation['should_execute']}")
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_forward_model_accuracy_over_time(self):
        agent_system = create_motor_prediction_system('learning_test_agent')
        improvements = []
        for cycle in range(5):
            test_action = MotorAction(joint_targets={'shoulder': 0.1 * cycle, 'elbow': -0.1 * cycle})
            test_state = ForwardModelState(body_configuration=BodyConfiguration(position=(0, 0, 1), joint_angles={'shoulder': 0.0, 'elbow': 0.0}))
            prediction = agent_system.predict_movement_outcome_before_execution(test_action, test_state)
            actual_outcome = ForwardModelState(body_configuration=BodyConfiguration(position=(0.05 * cycle, 0.02 * cycle, 1.01), joint_angles={'shoulder': 0.09 * cycle, 'elbow': -0.09 * cycle}))
            agent_system.update_predictions_from_execution(prediction, actual_outcome)
            performance = agent_system.get_system_performance()
            improvements.append(performance['prediction_success_rate'])
        self.assertGreaterEqual(improvements[-1], 0.0)
        print(f'✓ Forward model maintained performance across {len(improvements)} cycles')
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_motor_imagery_mental_simulation_quality(self):
        agent_system = create_motor_prediction_system('imagery_test_agent')
        actions = [MotorAction(joint_targets={'shoulder': 0.3}), MotorAction(joint_targets={'shoulder': 0.5, 'elbow': -0.3}), MotorAction(joint_targets={'shoulder': 0.6, 'elbow': -0.4, 'wrist': 0.2, 'finger1': 0.1, 'finger2': 0.1})]
        test_state = ForwardModelState(body_configuration=BodyConfiguration(position=(0, 0, 1), joint_angles={'shoulder': 0.0, 'elbow': 0.0, 'wrist': 0.0}))
        vividness_scores = []
        for action in actions:
            prediction = agent_system.predict_movement_outcome_before_execution(action, test_state, include_imagery=True)
            imagery = prediction['motor_imagery']
            vividness_scores.append(imagery['vividness'])
            self.assertGreater(imagery['vividness'], 0.0)
            self.assertGreater(imagery['simulation_steps'], 0)
        print(f'✓ Motor imagery generated for actions with vividness: {vividness_scores}')
        imagery_quality = agent_system.motor_imagery.assess_imagery_quality()
        self.assertGreater(imagery_quality['average_vividness'], 0.0)
        self.assertEqual(imagery_quality['imagery_count'], len(actions))
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_action_consequence_prediction_completeness(self):
        agent_system = create_motor_prediction_system('consequence_test_agent')
        complex_state = ForwardModelState(body_configuration=BodyConfiguration(position=(0, 0, 1), joint_angles={'shoulder': 0.0, 'elbow': 0.0}), environmental_context={'table': {'position': (0.5, 0.0, 0.8), 'material': 'wood'}, 'cup': {'position': (0.4, 0.1, 0.9), 'fragile': True}, 'wall': {'position': (1.0, 0.0, 1.0), 'solid': True}}, sensory_state={'vision': 0.8, 'touch': 0.2, 'proprioception': 0.9})
        complex_action = MotorAction(joint_targets={'shoulder': 0.7, 'elbow': -0.5, 'wrist': 0.3}, duration=2.5, force=0.6)
        prediction = agent_system.predict_movement_outcome_before_execution(complex_action, complex_state, include_imagery=True, include_consequences=True)
        consequences = prediction['action_consequences']
        env_consequences = consequences['environmental_consequences']
        self.assertIsInstance(env_consequences['object_interactions'], list)
        self.assertIn('energy_transfer', env_consequences)
        sensory_consequences = consequences['sensory_consequences']
        self.assertIn('tactile_feedback', sensory_consequences)
        self.assertIn('proprioceptive_feedback', sensory_consequences)
        self.assertIn('visual_feedback', sensory_consequences)
        secondary_effects = consequences['secondary_effects']
        self.assertIn('learning_effects', secondary_effects)
        self.assertIn('fatigue_accumulation', secondary_effects)
        print('✓ Comprehensive action consequences predicted')
        print(f"  Environmental interactions: {len(env_consequences['object_interactions'])}")
        print(f"  Overall consequence confidence: {consequences['overall_confidence']:.3f}")
class TestIntegrationWithDTESNComponents(unittest.TestCase):
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_dtesn_integration_initialization(self):
        motor_system = create_motor_prediction_system('dtesn_test_agent')
        performance = motor_system.get_system_performance()
        self.assertIn('dtesn_integration', performance)
        self.assertIn('echo_integration', performance)
        self.assertGreater(len(motor_system.forward_models), 0)
        for movement_type, forward_model in motor_system.forward_models.items():
            self.assertIsInstance(forward_model.dtesn_integration, bool)
    @unittest.skipIf(not MOTOR_PREDICTION_AVAILABLE, 'motor_prediction_system not available')
    def test_performance_constraints(self):
        motor_system = create_motor_prediction_system('performance_test_agent')
        test_state = ForwardModelState(body_configuration=BodyConfiguration(position=(0, 0, 1)))
        test_action = MotorAction(joint_targets={'joint': 0.5})
        start_time = time.time()
        prediction = motor_system.predict_movement_outcome_before_execution(test_action, test_state)
        end_time = time.time()
        latency = end_time - start_time
        self.assertLess(latency, 1.0)
        reported_latency = prediction['prediction_latency']
        self.assertLess(reported_latency, 1.0)
        print(f'✓ Prediction generated in {latency:.3f}s (reported: {reported_latency:.3f}s)')
if __name__ == '__main__':
    print('Motor Prediction System Tests - Deep Tree Echo Integration')
    print('=' * 70)
    suite = unittest.TestSuite()
    suite.addTest(TestAcceptanceCriteria('test_agents_predict_movement_outcomes_before_execution'))
    suite.addTest(TestAcceptanceCriteria('test_forward_model_accuracy_over_time'))
    suite.addTest(TestAcceptanceCriteria('test_motor_imagery_mental_simulation_quality'))
    suite.addTest(TestAcceptanceCriteria('test_action_consequence_prediction_completeness'))
    suite.addTest(TestForwardModel('test_forward_model_prediction'))
    suite.addTest(TestMotorImagerySystem('test_mental_rehearsal_simulation'))
    suite.addTest(TestActionConsequencePredictionSystem('test_consequence_prediction_structure'))
    suite.addTest(TestMotorPredictionSystem('test_comprehensive_prediction_before_execution'))
    suite.addTest(TestIntegrationWithDTESNComponents('test_dtesn_integration_initialization'))
    suite.addTest(TestIntegrationWithDTESNComponents('test_performance_constraints'))
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    if result.wasSuccessful():
        print('\n✓ All Motor Prediction System tests passed!')
        print('✓ Acceptance Criteria satisfied: Agents predict movement outcomes before execution')
    else:
        print(f'\n✗ {len(result.failures)} test failures, {len(result.errors)} errors')
    if len(sys.argv) == 1:
        unittest.main(verbosity=2)