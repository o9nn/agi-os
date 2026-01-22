import unittest
import numpy as np
import logging
import sys
from pathlib import Path
from unittest.mock import Mock
sys.path.insert(0, str(Path(__file__).parent))
try:
    from enactive_perception import EnactivePerceptionSystem, SensorimotorContingencyLearner, ActionBasedPerceptionModule, SensorimotorContingency, PerceptualPrediction, create_enactive_perception_system, integrate_with_embodied_learning, BodyState, MotorAction, SensorimotorExperience
    ENACTIVE_PERCEPTION_AVAILABLE = True
except ImportError as e:
    ENACTIVE_PERCEPTION_AVAILABLE = False
    print(f'Warning: Could not import enactive_perception: {e}')
class TestSensorimotorContingencyLearner(unittest.TestCase):
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_contingency_learner_creation(self):
        learner = SensorimotorContingencyLearner(max_contingencies=100, learning_rate=0.05)
        self.assertEqual(learner.max_contingencies, 100)
        self.assertEqual(learner.learning_rate, 0.05)
        self.assertEqual(len(learner.contingencies), 0)
        self.assertIsNotNone(learner.action_history)
        self.assertIsNotNone(learner.sensory_history)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_contingency_learning(self):
        learner = SensorimotorContingencyLearner()
        initial_state = BodyState(joint_angles={'shoulder': 0.5}, sensory_state={'vision': 0.8, 'touch': 0.2})
        action = MotorAction(joint_targets={'shoulder': 0.7}, muscle_commands={'primary': 0.8})
        resulting_state = BodyState(joint_angles={'shoulder': 0.7}, sensory_state={'vision': 0.9, 'touch': 0.3})
        experience = SensorimotorExperience(initial_body_state=initial_state, motor_action=action, resulting_body_state=resulting_state, sensory_feedback={'vision': 0.9, 'touch': 0.3}, success=True)
        result = learner.learn_contingency(experience)
        self.assertTrue(result)
        self.assertEqual(len(learner.contingencies), 1)
        contingency = learner.contingencies[0]
        self.assertEqual(contingency.action_pattern['joint_targets']['shoulder'], 0.7)
        self.assertEqual(contingency.sensory_context['vision'], 0.8)
        self.assertEqual(contingency.actual_outcome['vision'], 0.9)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_prediction_generation(self):
        learner = SensorimotorContingencyLearner()
        initial_state = BodyState(sensory_state={'vision': 0.5})
        action = MotorAction(joint_targets={'shoulder': 0.6})
        experience = SensorimotorExperience(initial_body_state=initial_state, motor_action=action, resulting_body_state=BodyState(), sensory_feedback={'vision': 0.7, 'success': True}, success=True)
        learner.learn_contingency(experience)
        learner.contingencies[0].confidence = 0.8
        test_action = MotorAction(joint_targets={'shoulder': 0.6})
        prediction = learner.predict_sensory_outcome(test_action, {'vision': 0.5})
        self.assertIsInstance(prediction, dict)
        if prediction:
            self.assertTrue(len(prediction) > 0)
class TestActionBasedPerceptionModule(unittest.TestCase):
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_perception_module_creation(self):
        module = ActionBasedPerceptionModule(exploration_rate=0.3)
        self.assertEqual(module.exploration_rate, 0.3)
        self.assertIsInstance(module.attention_weights, dict)
        self.assertIsInstance(module.perceptual_expectations, dict)
        self.assertIsNotNone(module.exploration_actions)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_exploratory_action_generation(self):
        module = ActionBasedPerceptionModule(exploration_rate=0.2)
        current_state = BodyState(joint_angles={'shoulder': 0.5, 'elbow': 0.3}, sensory_state={'vision': 0.7})
        action = module.generate_exploratory_action(current_state)
        self.assertIsInstance(action, MotorAction)
        self.assertTrue(len(action.joint_targets) > 0)
        self.assertTrue(0.0 <= action.force <= 1.0)
        self.assertTrue(0.1 <= action.duration <= 2.0)
        if 'shoulder' in action.joint_targets:
            self.assertNotEqual(action.joint_targets['shoulder'], 0.5)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_attention_weight_updates(self):
        module = ActionBasedPerceptionModule()
        initial_weights = module.attention_weights.copy()
        surprise = {'vision': 0.8, 'touch': 0.3, 'audio': 0.9}
        module.update_attention_weights(surprise)
        self.assertTrue(len(module.attention_weights) > len(initial_weights))
        self.assertIn('vision', module.attention_weights)
        self.assertIn('audio', module.attention_weights)
        self.assertTrue(module.attention_weights['audio'] > 0.5)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_perception_focusing(self):
        module = ActionBasedPerceptionModule()
        module.attention_weights = {'vision': 0.8, 'touch': 0.3, 'audio': 0.6}
        sensory_input = {'vision': 1.0, 'touch': 0.5, 'audio': 0.7, 'smell': 0.4}
        focused_input = module.focus_perception(sensory_input)
        self.assertIsInstance(focused_input, dict)
        self.assertAlmostEqual(focused_input['vision'], 1.0 * 0.8, places=2)
        self.assertAlmostEqual(focused_input['touch'], 0.5 * 0.3, places=2)
        self.assertAlmostEqual(focused_input['audio'], 0.7 * 0.6, places=2)
        self.assertTrue('smell' in focused_input)
class TestEnactivePerceptionSystem(unittest.TestCase):
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_system_creation(self):
        system = EnactivePerceptionSystem('test_agent')
        self.assertEqual(system.agent_name, 'test_agent')
        self.assertIsInstance(system.contingency_learner, SensorimotorContingencyLearner)
        self.assertIsInstance(system.action_perception_module, ActionBasedPerceptionModule)
        self.assertIsInstance(system.current_perceptual_state, dict)
        self.assertIsNotNone(system.perceptual_history)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_embodied_experience_processing(self):
        system = EnactivePerceptionSystem('test_agent')
        experience = SensorimotorExperience(initial_body_state=BodyState(sensory_state={'vision': 0.5}), motor_action=MotorAction(joint_targets={'shoulder': 0.6}), resulting_body_state=BodyState(sensory_state={'vision': 0.7}), sensory_feedback={'vision': 0.7, 'surprise_level': 'moderate'}, success=True, reward=0.8)
        result = system.process_embodied_experience(experience)
        self.assertIsInstance(result, dict)
        self.assertIn('contingency_learned', result)
        self.assertIn('sensory_surprise', result)
        self.assertIn('perceptual_state_updated', result)
        self.assertTrue(result['perceptual_state_updated'])
        self.assertTrue(len(system.perceptual_history) > 0)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_perceptual_prediction(self):
        system = EnactivePerceptionSystem('test_agent')
        experience = SensorimotorExperience(initial_body_state=BodyState(sensory_state={'touch': 0.3}), motor_action=MotorAction(joint_targets={'elbow': 0.4}), resulting_body_state=BodyState(sensory_state={'touch': 0.6}), sensory_feedback={'touch': 0.6}, success=True)
        system.process_embodied_experience(experience)
        planned_action = MotorAction(joint_targets={'elbow': 0.4})
        current_state = BodyState(sensory_state={'touch': 0.3})
        prediction = system.predict_perceptual_outcome(planned_action, current_state)
        self.assertIsInstance(prediction, PerceptualPrediction)
        self.assertEqual(prediction.action_plan, planned_action)
        self.assertTrue(0.0 <= prediction.confidence <= 1.0)
        self.assertTrue(0.0 <= prediction.exploration_value <= 1.0)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_perceptual_action_generation(self):
        system = EnactivePerceptionSystem('test_agent')
        current_state = BodyState(joint_angles={'shoulder': 0.4, 'elbow': 0.2}, sensory_state={'vision': 0.6, 'touch': 0.4})
        perceptual_goal = {'explore_touch': True, 'search_vision': 0.8}
        action = system.generate_perceptual_action(current_state, perceptual_goal)
        self.assertIsInstance(action, MotorAction)
        self.assertTrue(len(action.joint_targets) > 0)
        for joint, angle in action.joint_targets.items():
            self.assertTrue(-np.pi <= angle <= np.pi)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_enactive_metrics(self):
        system = EnactivePerceptionSystem('test_agent')
        for i in range(3):
            experience = SensorimotorExperience(initial_body_state=BodyState(sensory_state={'test': 0.5 + i * 0.1}), motor_action=MotorAction(joint_targets={'joint1': 0.3 + i * 0.1}), resulting_body_state=BodyState(sensory_state={'test': 0.6 + i * 0.1}), sensory_feedback={'test': 0.6 + i * 0.1}, success=True)
            system.process_embodied_experience(experience)
        metrics = system.get_enactive_metrics()
        self.assertIsInstance(metrics, dict)
        required_metrics = ['total_contingencies_learned', 'average_contingency_confidence', 'exploration_actions_taken', 'attention_weights', 'system_active']
        for metric in required_metrics:
            self.assertIn(metric, metrics)
        self.assertTrue(metrics['system_active'])
        self.assertTrue(metrics['total_contingencies_learned'] >= 0)
class TestEnactivePerceptionIntegration(unittest.TestCase):
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_factory_function(self):
        system = create_enactive_perception_system('factory_test')
        self.assertIsInstance(system, EnactivePerceptionSystem)
        self.assertEqual(system.agent_name, 'factory_test')
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_embodied_learning_integration(self):
        enactive_system = EnactivePerceptionSystem('integration_test')
        mock_embodied_system = Mock()
        mock_embodied_system.sensory_motor = Mock()
        result = integrate_with_embodied_learning(enactive_system, mock_embodied_system)
        self.assertTrue(result)
        self.assertEqual(mock_embodied_system.enactive_perception, enactive_system)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_integration_failure_handling(self):
        enactive_system = EnactivePerceptionSystem('failure_test')
        mock_embodied_system = Mock()
        del mock_embodied_system.sensory_motor
        result = integrate_with_embodied_learning(enactive_system, mock_embodied_system)
        self.assertFalse(result)
class TestAcceptanceCriteria(unittest.TestCase):
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_perception_emergence_through_interaction(self):
        system = EnactivePerceptionSystem('emergence_test')
        interactions = []
        for i in range(10):
            current_state = BodyState(joint_angles={'joint': 0.1 * i}, sensory_state={'environment': 0.5 + 0.05 * i})
            action = system.generate_perceptual_action(current_state)
            environment_response = {'environment': 0.5 + 0.05 * i + np.random.uniform(-0.1, 0.1), 'feedback': f'response_{i}', 'changed': True}
            experience = SensorimotorExperience(initial_body_state=current_state, motor_action=action, resulting_body_state=BodyState(sensory_state=environment_response), sensory_feedback=environment_response, success=True)
            result = system.process_embodied_experience(experience)
            interactions.append(result)
        metrics = system.get_enactive_metrics()
        self.assertTrue(metrics['total_contingencies_learned'] > 0)
        self.assertTrue(len(metrics['attention_weights']) > 0)
        self.assertTrue(metrics['perceptual_history_length'] > 0)
        self.assertTrue(all((interaction['contingency_learned'] for interaction in interactions[:5])))
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_action_based_perception_mechanisms(self):
        system = EnactivePerceptionSystem('action_based_test')
        initial_state = BodyState(joint_angles={'shoulder': 0.0}, sensory_state={'clarity': 0.3})
        perceptual_goal = {'increase_clarity': True}
        action = system.generate_perceptual_action(initial_state, perceptual_goal)
        self.assertNotEqual(action.joint_targets.get('shoulder', 0), 0.0)
        self.assertTrue(action.duration > 0)
        self.assertTrue(0 < action.force <= 1.0)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_sensorimotor_contingency_learning(self):
        system = EnactivePerceptionSystem('contingency_test')
        consistent_experiences = []
        for i in range(5):
            experience = SensorimotorExperience(initial_body_state=BodyState(sensory_state={'input': 0.5}), motor_action=MotorAction(joint_targets={'test_joint': 0.7}), resulting_body_state=BodyState(sensory_state={'input': 0.8}), sensory_feedback={'input': 0.8, 'pattern': 'consistent'}, success=True)
            result = system.process_embodied_experience(experience)
            consistent_experiences.append(result)
        metrics = system.get_enactive_metrics()
        self.assertTrue(metrics['total_contingencies_learned'] > 0)
        contingencies = system.contingency_learner.contingencies
        if contingencies:
            relevant_contingency = None
            for contingency in contingencies:
                if contingency.action_pattern.get('joint_targets', {}).get('test_joint') == 0.7:
                    relevant_contingency = contingency
                    break
            if relevant_contingency:
                self.assertTrue(relevant_contingency.confidence > 0.1)
                self.assertTrue(relevant_contingency.frequency >= 5)
    @unittest.skipIf(not ENACTIVE_PERCEPTION_AVAILABLE, 'enactive_perception not available')
    def test_perceptual_prediction_through_action(self):
        system = EnactivePerceptionSystem('prediction_test')
        training_experience = SensorimotorExperience(initial_body_state=BodyState(sensory_state={'sensor': 0.4}), motor_action=MotorAction(joint_targets={'predictor': 0.6}), resulting_body_state=BodyState(sensory_state={'sensor': 0.9}), sensory_feedback={'sensor': 0.9, 'predictable': True}, success=True)
        for _ in range(3):
            system.process_embodied_experience(training_experience)
        if system.contingency_learner.contingencies:
            system.contingency_learner.contingencies[0].confidence = 0.7
        test_action = MotorAction(joint_targets={'predictor': 0.6})
        test_state = BodyState(sensory_state={'sensor': 0.4})
        prediction = system.predict_perceptual_outcome(test_action, test_state)
        self.assertIsInstance(prediction, PerceptualPrediction)
        self.assertTrue(prediction.confidence > 0.0)
        if prediction.predicted_sensory_outcome and prediction.confidence > 0.3:
            predicted_sensor = prediction.predicted_sensory_outcome.get('sensor', 0)
            self.assertTrue(predicted_sensor > 0.4)
def run_comprehensive_test():
    print('=' * 70)
    print('COMPREHENSIVE ENACTIVE PERCEPTION SYSTEM TEST')
    print('=' * 70)
    if not ENACTIVE_PERCEPTION_AVAILABLE:
        print('❌ FAILED: Enactive perception system not available for testing')
        return False
    try:
        print('\n🔍 Testing basic system functionality...')
        system = create_enactive_perception_system('comprehensive_test')
        print('✅ System created successfully')
        print('\n🧠 Testing learning and prediction cycle...')
        for i in range(5):
            experience = SensorimotorExperience(initial_body_state=BodyState(joint_angles={'test': 0.1 * i}, sensory_state={'env': 0.2 * i}), motor_action=MotorAction(joint_targets={'test': 0.2 * i}), resulting_body_state=BodyState(joint_angles={'test': 0.2 * i}, sensory_state={'env': 0.3 * i}), sensory_feedback={'env': 0.3 * i, 'learning': True}, success=True)
            result = system.process_embodied_experience(experience)
            print(f"  Learning iteration {i + 1}: {result['contingency_learned']}")
        test_action = MotorAction(joint_targets={'test': 0.6})
        test_state = BodyState(sensory_state={'env': 0.4})
        prediction = system.predict_perceptual_outcome(test_action, test_state)
        print(f'  Prediction confidence: {prediction.confidence:.3f}')
        print('✅ Learning and prediction cycle working')
        print('\n📊 Testing system metrics...')
        metrics = system.get_enactive_metrics()
        print(f"  Contingencies learned: {metrics['total_contingencies_learned']}")
        print(f"  Average confidence: {metrics['average_contingency_confidence']:.3f}")
        print(f"  System active: {metrics['system_active']}")
        print('✅ Metrics generation working')
        print('\n🎯 COMPREHENSIVE TEST PASSED')
        return True
    except Exception as e:
        print(f'❌ COMPREHENSIVE TEST FAILED: {e}')
        import traceback
        traceback.print_exc()
        return False
if __name__ == '__main__':
    logging.basicConfig(level=logging.WARNING)
    comprehensive_success = run_comprehensive_test()
    print('\n' + '=' * 70)
    print('UNIT TEST SUITE')
    print('=' * 70)
    unittest.main(verbosity=2, exit=False)
    print('\n' + '=' * 70)
    print('TEST SUMMARY')
    print('=' * 70)
    print(f"Comprehensive Test: {('✅ PASSED' if comprehensive_success else '❌ FAILED')}")
    print('Unit Tests: See results above')
    print('\nEnactive Perception System testing complete.')