import unittest
import sys
import time
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
try:
    from feedback_control_system import FeedbackControlSystem, RealTimeFeedbackCorrector, AdaptiveController, BalanceStabilityManager, FeedbackState, CorrectiveAction, FeedbackType, ControlMode, create_feedback_control_system
    from motor_prediction_system import BodyConfiguration, MotorAction, ForwardModelState
    FEEDBACK_CONTROL_AVAILABLE = True
except ImportError as e:
    print(f'Feedback control system not available: {e}')
    FEEDBACK_CONTROL_AVAILABLE = False
    class FeedbackControlSystem:
        def __init__(self, name='test'):
            self.agent_name = name
        def process_feedback_state(self, *args, **kwargs):
            return {'system_status': {'balance_maintained': True, 'corrections_applied': False}}
class TestAcceptanceCriteria(unittest.TestCase):
    def setUp(self):
        if not FEEDBACK_CONTROL_AVAILABLE:
            self.skipTest('Feedback control system not available')
    def test_agents_maintain_balance_and_correct_movements(self):
        print('\nPrimary acceptance criteria test:')
        control_system = create_feedback_control_system('balance_test_agent')
        off_balance_state = BodyConfiguration(position=(0.15, 0.1, 0.95), joint_angles={'shoulder': 0.3, 'elbow': -0.2, 'hip': 0.1, 'knee': -0.05, 'ankle': 0.08})
        balanced_state = BodyConfiguration(position=(0.0, 0.0, 1.0), joint_angles={'shoulder': 0.0, 'elbow': 0.0, 'hip': 0.0, 'knee': 0.0, 'ankle': 0.0})
        sensor_data = {'balance_sensor': 0.25, 'pressure_left': 0.8, 'pressure_right': 0.2, 'vestibular_x': 0.15, 'vestibular_y': 0.1}
        response = control_system.process_feedback_state(off_balance_state, balanced_state, sensor_data)
        self.assertIsInstance(response, dict)
        self.assertIn('system_status', response)
        self.assertIn('feedback_processing_results', response)
        system_status = response['system_status']
        results = response['feedback_processing_results']
        corrections_applied = system_status['corrections_applied'] or 'balance_correction' in results
        self.assertTrue(corrections_applied, 'System should apply corrections for significant imbalance')
        stability_score = results.get('stability_score', 0.0)
        self.assertIsInstance(stability_score, (int, float))
        self.assertGreaterEqual(stability_score, 0.0)
        self.assertLessEqual(stability_score, 1.0)
        if stability_score < 0.8:
            self.assertIn('balance_strategy', results)
            balance_strategy = results['balance_strategy']
            self.assertIn(balance_strategy, [None, 'ankle_strategy', 'hip_strategy', 'step_strategy', 'emergency_stop'])
        self.assertIn('proprioceptive_feedback', results)
        proprioceptive_info = results['proprioceptive_feedback']
        self.assertIn('joint_position_sense', proprioceptive_info)
        print('✓ Agent maintained balance and applied corrections')
        print(f'  Stability score: {stability_score:.3f}')
        print(f'  Corrections applied: {corrections_applied}')
        print(f"  Balance strategy: {results.get('balance_strategy', 'None')}")
        if 'corrective_action' in results:
            corrections = results['corrective_action']
            print(f'  Joint corrections: {len(corrections)} joints')
    def test_real_time_feedback_correction(self):
        print('\nTest real-time feedback correction:')
        corrector = RealTimeFeedbackCorrector(correction_threshold=0.03)
        current_body = BodyConfiguration(position=(0.1, 0.0, 1.0), joint_angles={'shoulder': 0.2, 'elbow': -0.1})
        predicted_body = BodyConfiguration(position=(0.0, 0.0, 1.0), joint_angles={'shoulder': 0.0, 'elbow': 0.0})
        feedback_state = FeedbackState(feedback_type=FeedbackType.PROPRIOCEPTIVE, current_body_state=current_body, predicted_body_state=predicted_body)
        corrections = []
        for i in range(5):
            correction = corrector.process_feedback(feedback_state)
            corrections.append(correction)
            time.sleep(0.1)
        valid_corrections = [c for c in corrections if c is not None]
        self.assertGreater(len(valid_corrections), 0, 'Real-time corrector should generate corrections for errors')
        for correction in valid_corrections:
            self.assertIsInstance(correction, CorrectiveAction)
            self.assertIsInstance(correction.joint_corrections, dict)
            self.assertGreater(len(correction.joint_corrections), 0)
            self.assertGreaterEqual(correction.confidence, 0.0)
            self.assertLessEqual(correction.confidence, 1.0)
        performance = corrector.get_correction_performance()
        self.assertIsInstance(performance, dict)
        self.assertIn('total_corrections', performance)
        self.assertGreaterEqual(performance['total_corrections'], 0)
        print('✓ Real-time feedback correction functional')
        print(f'  Corrections generated: {len(valid_corrections)}/5 attempts')
        print(f"  Total corrections: {performance['total_corrections']}")
    def test_adaptive_control_based_on_proprioception(self):
        print('\nTest adaptive control based on proprioception:')
        adaptive_controller = AdaptiveController(adaptation_rate=0.2)
        body_state = BodyConfiguration(position=(0.05, -0.02, 1.0), joint_angles={'shoulder': 0.15, 'elbow': -0.1, 'hip': 0.05, 'knee': -0.03})
        proprioceptive_info = adaptive_controller.process_proprioceptive_feedback(body_state)
        self.assertIsInstance(proprioceptive_info, dict)
        self.assertIn('joint_position_sense', proprioceptive_info)
        self.assertIn('body_position_sense', proprioceptive_info)
        self.assertIn('movement_sense', proprioceptive_info)
        joint_sense = proprioceptive_info['joint_position_sense']
        for joint_name, joint_info in joint_sense.items():
            self.assertIn('sensed_angle', joint_info)
            self.assertIn('confidence', joint_info)
            self.assertGreaterEqual(joint_info['confidence'], 0.0)
            self.assertLessEqual(joint_info['confidence'], 1.0)
        initial_params = adaptive_controller.control_parameters.copy()
        feedback_state = FeedbackState(feedback_type=FeedbackType.PROPRIOCEPTIVE, current_body_state=body_state)
        performance_metrics = {'success_rate': 0.6, 'stability_score': 0.7}
        adapted_params = adaptive_controller.adapt_control_parameters(feedback_state, performance_metrics)
        self.assertIsInstance(adapted_params, dict)
        self.assertIn('proportional_gain', adapted_params)
        param_changes = 0
        for param_name in initial_params:
            if abs(adapted_params[param_name] - initial_params[param_name]) > 1e-06:
                param_changes += 1
        self.assertGreater(param_changes, 0, 'Control parameters should adapt based on performance')
        adaptation_perf = adaptive_controller.get_adaptation_performance()
        self.assertIn('adaptation_count', adaptation_perf)
        self.assertGreater(adaptation_perf['adaptation_count'], 0)
        print('✓ Adaptive control based on proprioception functional')
        print(f'  Parameter adaptations: {param_changes}')
        print(f"  Adaptation count: {adaptation_perf['adaptation_count']}")
    def test_balance_and_stability_maintenance(self):
        print('\nTest balance and stability maintenance:')
        balance_manager = BalanceStabilityManager(stability_threshold=0.7)
        stable_body = BodyConfiguration(position=(0.0, 0.0, 1.0), joint_angles={'hip': 0.0, 'knee': 0.0, 'ankle': 0.0})
        stable_assessment = balance_manager.assess_stability(stable_body)
        self.assertIsInstance(stable_assessment, dict)
        self.assertIn('current_stability', stable_assessment)
        self.assertGreaterEqual(stable_assessment['current_stability'], 0.7, 'Stable configuration should have high stability score')
        self.assertFalse(stable_assessment.get('emergency_required', False))
        unstable_body = BodyConfiguration(position=(0.3, 0.2, 0.8), joint_angles={'hip': 0.4, 'knee': -0.2, 'ankle': 0.3})
        unstable_assessment = balance_manager.assess_stability(unstable_body)
        self.assertLess(unstable_assessment['current_stability'], 0.7, 'Unstable configuration should have low stability score')
        balance_strategy = unstable_assessment.get('balance_strategy_needed')
        if unstable_assessment['current_stability'] < 0.5:
            self.assertIsNotNone(balance_strategy, 'Low stability should trigger balance strategy')
            self.assertIn(balance_strategy, ['ankle_strategy', 'hip_strategy', 'step_strategy', 'emergency_stop'])
        if balance_strategy:
            balance_correction = balance_manager.generate_balance_correction(unstable_assessment)
            if balance_correction:
                self.assertIsInstance(balance_correction, CorrectiveAction)
                self.assertGreater(len(balance_correction.joint_corrections), 0)
                self.assertEqual(balance_correction.action_type, balance_strategy)
        stability_perf = balance_manager.get_stability_performance()
        self.assertIn('total_stability_events', stability_perf)
        self.assertIn('average_stability_score', stability_perf)
        print('✓ Balance and stability maintenance functional')
        print(f"  Stable config stability: {stable_assessment['current_stability']:.3f}")
        print(f"  Unstable config stability: {unstable_assessment['current_stability']:.3f}")
        print(f'  Balance strategy for unstable: {balance_strategy}')
class TestFeedbackControlSystem(unittest.TestCase):
    def setUp(self):
        if not FEEDBACK_CONTROL_AVAILABLE:
            self.skipTest('Feedback control system not available')
        self.control_system = create_feedback_control_system('test_agent')
    def tearDown(self):
        if hasattr(self, 'control_system'):
            self.control_system.stop_real_time_control()
    def test_system_initialization(self):
        self.assertIsInstance(self.control_system, FeedbackControlSystem)
        self.assertEqual(self.control_system.agent_name, 'test_agent')
        self.assertIsNotNone(self.control_system.feedback_corrector)
        self.assertIsNotNone(self.control_system.adaptive_controller)
        self.assertIsNotNone(self.control_system.balance_manager)
        performance = self.control_system.get_comprehensive_performance()
        self.assertIn('system_metrics', performance)
        self.assertIn('feedback_correction', performance)
        self.assertIn('balance_stability', performance)
        self.assertIn('adaptive_control', performance)
        self.assertIn('integration_status', performance)
    def test_feedback_state_processing(self):
        current_state = BodyConfiguration(position=(0.08, 0.05, 1.0), joint_angles={'shoulder': 0.1, 'elbow': -0.05, 'hip': 0.03})
        predicted_state = BodyConfiguration(position=(0.0, 0.0, 1.0), joint_angles={'shoulder': 0.0, 'elbow': 0.0, 'hip': 0.0})
        sensor_data = {'balance_sensor': 0.05, 'pressure_left': 0.55, 'pressure_right': 0.45}
        response = self.control_system.process_feedback_state(current_state, predicted_state, sensor_data)
        self.assertIsInstance(response, dict)
        self.assertIn('agent_name', response)
        self.assertIn('feedback_processing_results', response)
        self.assertIn('system_status', response)
        self.assertIn('performance_summary', response)
        results = response['feedback_processing_results']
        self.assertIn('stability_score', results)
        self.assertIn('proprioceptive_feedback', results)
        status = response['system_status']
        self.assertIn('balance_maintained', status)
        self.assertIn('corrections_applied', status)
        self.assertIn('emergency_required', status)
        self.assertEqual(response['agent_name'], 'test_agent')
    def test_real_time_control_loop(self):
        test_state = BodyConfiguration(position=(0.05, 0.0, 1.0), joint_angles={'hip': 0.02, 'knee': -0.01})
        self.control_system.process_feedback_state(test_state)
        self.control_system.start_real_time_control()
        self.assertTrue(self.control_system.active)
        time.sleep(0.5)
        metrics_before = self.control_system.system_metrics['control_cycles']
        time.sleep(0.5)
        metrics_after = self.control_system.system_metrics['control_cycles']
        self.assertGreater(metrics_after, metrics_before, 'Control loop should be processing cycles')
        self.control_system.stop_real_time_control()
        self.assertFalse(self.control_system.active)
    def test_system_performance_tracking(self):
        for i in range(5):
            body_state = BodyConfiguration(position=(0.01 * i, 0.01 * i, 1.0), joint_angles={'hip': 0.01 * i})
            self.control_system.process_feedback_state(body_state)
            time.sleep(0.1)
        performance = self.control_system.get_comprehensive_performance()
        system_metrics = performance['system_metrics']
        self.assertGreaterEqual(system_metrics['control_cycles'], 0)
        self.assertGreater(system_metrics['start_time'], 0)
        feedback_perf = performance['feedback_correction']
        self.assertIn('total_corrections', feedback_perf)
        balance_perf = performance['balance_stability']
        self.assertIn('total_stability_events', balance_perf)
        adaptive_perf = performance['adaptive_control']
        self.assertIn('adaptation_count', adaptive_perf)
        integration_status = performance['integration_status']
        self.assertIn('dtesn_integration', integration_status)
        self.assertIn('real_time_active', integration_status)
    def test_corrective_action_application(self):
        corrective_action = CorrectiveAction(joint_corrections={'shoulder': 0.1, 'elbow': -0.05}, force_corrections={'grip': 0.2}, priority=0.8, confidence=0.9)
        initial_corrections = len(self.control_system.active_corrections)
        self.control_system._apply_corrective_action(corrective_action)
        self.assertGreater(len(self.control_system.active_corrections), initial_corrections)
        time.sleep(1.1)
        test_state = BodyConfiguration(position=(0, 0, 1), joint_angles={})
        self.control_system.process_feedback_state(test_state)
    def test_emergency_response(self):
        critical_state = BodyConfiguration(position=(0.5, 0.3, 0.7), joint_angles={'hip': 0.6, 'knee': -0.4, 'ankle': 0.5, 'shoulder': 0.8})
        response = self.control_system.process_feedback_state(critical_state)
        results = response['feedback_processing_results']
        status = response['system_status']
        stability_score = results.get('stability_score', 1.0)
        if stability_score < 0.3:
            self.assertTrue(status.get('emergency_required', False) or results.get('balance_strategy') == 'emergency_stop', 'Critical instability should trigger emergency response')
class TestComponentIntegration(unittest.TestCase):
    def setUp(self):
        if not FEEDBACK_CONTROL_AVAILABLE:
            self.skipTest('Feedback control system not available')
    def test_feedback_corrector_and_adaptive_controller_integration(self):
        corrector = RealTimeFeedbackCorrector()
        controller = AdaptiveController()
        body_state = BodyConfiguration(position=(0.1, 0.0, 1.0), joint_angles={'shoulder': 0.2, 'elbow': -0.1})
        feedback_state = FeedbackState(feedback_type=FeedbackType.PROPRIOCEPTIVE, current_body_state=body_state)
        correction = corrector.process_feedback(feedback_state)
        proprioceptive_info = controller.process_proprioceptive_feedback(body_state)
        if correction:
            self.assertIsInstance(correction.joint_corrections, dict)
        self.assertIsInstance(proprioceptive_info, dict)
        self.assertIn('joint_position_sense', proprioceptive_info)
    def test_balance_manager_and_corrector_integration(self):
        balance_manager = BalanceStabilityManager()
        corrector = RealTimeFeedbackCorrector()
        unstable_state = BodyConfiguration(position=(0.2, 0.1, 0.9), joint_angles={'hip': 0.3, 'knee': -0.1, 'ankle': 0.2})
        stability_assessment = balance_manager.assess_stability(unstable_state)
        if stability_assessment.get('balance_strategy_needed'):
            balance_correction = balance_manager.generate_balance_correction(stability_assessment)
            if balance_correction:
                self.assertIsInstance(balance_correction, CorrectiveAction)
                self.assertIsInstance(balance_correction.joint_corrections, dict)
        feedback_state = FeedbackState(feedback_type=FeedbackType.STABILITY, current_body_state=unstable_state)
        feedback_correction = corrector.process_feedback(feedback_state)
        if feedback_correction and stability_assessment.get('balance_strategy_needed'):
            self.assertIsInstance(feedback_correction.joint_corrections, dict)
def run_performance_test():
    if not FEEDBACK_CONTROL_AVAILABLE:
        print('⚠️  Performance test skipped: Feedback control system not available')
        return
    print('\n' + '=' * 60)
    print('Performance Test - Real-time Constraints')
    print('=' * 60)
    control_system = create_feedback_control_system('performance_test_agent')
    body_state = BodyConfiguration(position=(0.05, 0.02, 1.0), joint_angles={'shoulder': 0.1, 'elbow': -0.05, 'hip': 0.03})
    processing_times = []
    for i in range(100):
        start_time = time.time()
        response = control_system.process_feedback_state(body_state)
        processing_time = time.time() - start_time
        processing_times.append(processing_time)
    avg_latency = sum(processing_times) / len(processing_times)
    max_latency = max(processing_times)
    print('Feedback Processing Performance:')
    print(f'  Average latency: {avg_latency * 1000:.2f}ms')
    print(f'  Maximum latency: {max_latency * 1000:.2f}ms')
    print(f"  Real-time requirement (≤20ms): {('✓ PASS' if max_latency <= 0.02 else '✗ FAIL')}")
    control_system.start_real_time_control()
    start_cycles = control_system.system_metrics['control_cycles']
    time.sleep(1.0)
    end_cycles = control_system.system_metrics['control_cycles']
    actual_frequency = end_cycles - start_cycles
    target_frequency = control_system.control_loop_frequency
    control_system.stop_real_time_control()
    print('\nReal-time Control Loop Performance:')
    print(f'  Target frequency: {target_frequency}Hz')
    print(f'  Actual frequency: {actual_frequency}Hz')
    print(f'  Performance ratio: {actual_frequency / target_frequency:.2f}')
    print(f"  Real-time requirement (≥90% target): {('✓ PASS' if actual_frequency >= target_frequency * 0.9 else '✗ FAIL')}")
if __name__ == '__main__':
    print('Feedback Control System Tests - Deep Tree Echo Integration')
    print('=' * 70)
    unittest.main(argv=[''], exit=False, verbosity=2)
    run_performance_test()
    print('\n' + '=' * 70)
    print('Test execution completed.')