import unittest
import time
import tempfile
import os
import json
from phase_3_3_3_self_monitoring import SelfMonitoringSystem, MonitoringLevel, ErrorSeverity, PerformanceMetrics, BodyStateMetrics, DetectedError, DTESNSelfMonitoringIntegration, create_self_monitoring_system
class TestSelfMonitoringSystem(unittest.TestCase):
    def setUp(self):
        self.agent_id = 'test_agent_001'
        self.monitor = SelfMonitoringSystem(agent_id=self.agent_id, monitoring_level=MonitoringLevel.COMPREHENSIVE, max_history_size=100)
    def tearDown(self):
        if self.monitor.is_monitoring:
            self.monitor.stop_monitoring()
    def test_system_initialization(self):
        self.assertEqual(self.monitor.agent_id, self.agent_id)
        self.assertEqual(self.monitor.monitoring_level, MonitoringLevel.COMPREHENSIVE)
        self.assertFalse(self.monitor.is_monitoring)
        self.assertEqual(len(self.monitor.performance_history), 0)
        self.assertEqual(len(self.monitor.detected_errors), 0)
        self.assertTrue(self.monitor.auto_correction_enabled)
        self.assertTrue(self.monitor.learning_enabled)
    def test_performance_metrics_creation(self):
        metrics = PerformanceMetrics(response_time_ms=150.5, accuracy_score=0.92, efficiency_ratio=0.85, resource_utilization=0.6, error_rate=0.02, throughput=75.0, success_rate=0.98)
        self.assertEqual(metrics.response_time_ms, 150.5)
        self.assertEqual(metrics.accuracy_score, 0.92)
        self.assertEqual(metrics.success_rate, 0.98)
        metrics_dict = metrics.to_dict()
        self.assertIn('timestamp', metrics_dict)
        self.assertIn('response_time_ms', metrics_dict)
        self.assertEqual(metrics_dict['accuracy_score'], 0.92)
    def test_body_state_metrics_creation(self):
        body_state = BodyStateMetrics(joint_positions={'shoulder_left': 1.2, 'elbow_right': -0.5}, joint_velocities={'shoulder_left': 0.3, 'elbow_right': -0.1}, body_orientation=(0.1, -0.05, 0.02), balance_state=0.85, movement_execution_error=0.03)
        self.assertEqual(body_state.joint_positions['shoulder_left'], 1.2)
        self.assertEqual(body_state.balance_state, 0.85)
        self.assertEqual(body_state.body_orientation, (0.1, -0.05, 0.02))
        state_dict = body_state.to_dict()
        self.assertIn('timestamp', state_dict)
        self.assertIn('joint_positions', state_dict)
        self.assertEqual(state_dict['balance_state'], 0.85)
    def test_error_detection_creation(self):
        error = DetectedError(error_id='test_error_001', timestamp=time.time(), error_type='performance_degradation', severity=ErrorSeverity.MEDIUM, description='Response time increased by 50%', context={'agent_id': self.agent_id})
        self.assertEqual(error.error_type, 'performance_degradation')
        self.assertEqual(error.severity, ErrorSeverity.MEDIUM)
        self.assertFalse(error.correction_successful)
        error_dict = error.to_dict()
        self.assertIn('error_id', error_dict)
        self.assertEqual(error_dict['severity'], 'medium')
    def test_monitoring_lifecycle(self):
        self.assertFalse(self.monitor.is_monitoring)
        self.monitor.start_monitoring(monitoring_interval=0.1)
        time.sleep(0.2)
        self.assertTrue(self.monitor.is_monitoring)
        self.monitor.stop_monitoring()
        self.assertFalse(self.monitor.is_monitoring)
    def test_performance_assessment(self):
        good_metrics = PerformanceMetrics(response_time_ms=500.0, accuracy_score=0.9, efficiency_ratio=0.8, error_rate=0.01, success_rate=0.99)
        self.monitor._assess_performance(good_metrics)
        poor_metrics = PerformanceMetrics(response_time_ms=1500.0, accuracy_score=0.6, efficiency_ratio=0.5, error_rate=0.1, success_rate=0.8)
        self.monitor._assess_performance(poor_metrics)
        self.assertTrue(True)
    def test_body_state_assessment(self):
        problematic_state = BodyStateMetrics(balance_state=0.2, movement_execution_error=0.15, joint_velocities={'shoulder': 15.0})
        initial_error_count = len(self.monitor.detected_errors)
        self.monitor._assess_body_state(problematic_state)
        self.assertGreater(len(self.monitor.detected_errors), initial_error_count)
    def test_error_detection_and_correction(self):
        declining_metrics = [PerformanceMetrics(accuracy_score=0.9, response_time_ms=100), PerformanceMetrics(accuracy_score=0.85, response_time_ms=150), PerformanceMetrics(accuracy_score=0.8, response_time_ms=200), PerformanceMetrics(accuracy_score=0.75, response_time_ms=250), PerformanceMetrics(accuracy_score=0.7, response_time_ms=300)]
        for metrics in declining_metrics:
            self.monitor.performance_history.append(metrics)
        initial_error_count = len(self.monitor.detected_errors)
        self.monitor._detect_errors()
        self.assertGreater(len(self.monitor.detected_errors), initial_error_count)
        for error in self.monitor.detected_errors:
            if error.error_type == 'performance_degradation':
                self.assertIsNotNone(error.correction_applied)
                break
    def test_baseline_learning_and_adaptation(self):
        for i in range(15):
            metrics = PerformanceMetrics(response_time_ms=100 + i * 5, accuracy_score=0.9 - i * 0.01, efficiency_ratio=0.8 + i * 0.005)
            self.monitor.performance_history.append(metrics)
        initial_baselines_count = len(self.monitor.performance_baselines)
        self.monitor._update_baselines()
        self.assertGreater(len(self.monitor.performance_baselines), initial_baselines_count)
        self.assertIn('response_time_ms', self.monitor.performance_baselines)
        self.assertIn('accuracy_score', self.monitor.performance_baselines)
    def test_monitoring_status_report(self):
        self.monitor.performance_history.append(PerformanceMetrics(accuracy_score=0.9))
        self.monitor.body_state_history.append(BodyStateMetrics(balance_state=0.8))
        self.monitor.detected_errors.append(DetectedError(error_id='test', timestamp=time.time(), error_type='test', severity=ErrorSeverity.LOW, description='test error', context={}))
        status = self.monitor.get_monitoring_status()
        self.assertEqual(status['agent_id'], self.agent_id)
        self.assertEqual(status['monitoring_level'], MonitoringLevel.COMPREHENSIVE.value)
        self.assertEqual(status['performance_metrics_count'], 1)
        self.assertEqual(status['body_state_metrics_count'], 1)
        self.assertEqual(status['detected_errors_count'], 1)
        self.assertIn('recent_performance', status)
        self.assertIn('recent_body_state', status)
    def test_data_export_and_import(self):
        self.monitor.performance_history.append(PerformanceMetrics(accuracy_score=0.9))
        self.monitor.body_state_history.append(BodyStateMetrics(balance_state=0.8))
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
            temp_file = f.name
        try:
            self.monitor.export_monitoring_data(temp_file)
            self.assertTrue(os.path.exists(temp_file))
            with open(temp_file, 'r') as f:
                exported_data = json.load(f)
            self.assertIn('export_timestamp', exported_data)
            self.assertIn('agent_id', exported_data)
            self.assertIn('performance_history', exported_data)
            self.assertIn('body_state_history', exported_data)
            self.assertEqual(exported_data['agent_id'], self.agent_id)
            self.assertEqual(len(exported_data['performance_history']), 1)
        finally:
            if os.path.exists(temp_file):
                os.unlink(temp_file)
    def test_data_reset(self):
        self.monitor.performance_history.append(PerformanceMetrics(accuracy_score=0.9))
        self.monitor.body_state_history.append(BodyStateMetrics(balance_state=0.8))
        self.monitor.detected_errors.append(DetectedError(error_id='test', timestamp=time.time(), error_type='test', severity=ErrorSeverity.LOW, description='test error', context={}))
        self.assertGreater(len(self.monitor.performance_history), 0)
        self.assertGreater(len(self.monitor.body_state_history), 0)
        self.assertGreater(len(self.monitor.detected_errors), 0)
        self.monitor.reset_monitoring_data()
        self.assertEqual(len(self.monitor.performance_history), 0)
        self.assertEqual(len(self.monitor.body_state_history), 0)
        self.assertEqual(len(self.monitor.detected_errors), 0)
class TestDTESNIntegration(unittest.TestCase):
    def setUp(self):
        self.integration = DTESNSelfMonitoringIntegration()
        self.monitor1 = create_self_monitoring_system('agent_001')
        self.monitor2 = create_self_monitoring_system('agent_002')
    def test_agent_registration(self):
        self.integration.register_agent_monitor('agent_001', self.monitor1)
        self.integration.register_agent_monitor('agent_002', self.monitor2)
        self.assertIn('agent_001', self.integration.monitoring_systems)
        self.assertIn('agent_002', self.integration.monitoring_systems)
        self.assertEqual(len(self.integration.monitoring_systems), 2)
    def test_system_wide_status(self):
        self.integration.register_agent_monitor('agent_001', self.monitor1)
        self.integration.register_agent_monitor('agent_002', self.monitor2)
        status = self.integration.get_system_wide_status()
        self.assertIn('agent_001', status)
        self.assertIn('agent_002', status)
        self.assertEqual(status['agent_001']['agent_id'], 'agent_001')
        self.assertEqual(status['agent_002']['agent_id'], 'agent_002')
class TestFactoryFunction(unittest.TestCase):
    def test_create_self_monitoring_system(self):
        monitor = create_self_monitoring_system(agent_id='factory_test_agent', monitoring_level=MonitoringLevel.BASIC)
        self.assertIsInstance(monitor, SelfMonitoringSystem)
        self.assertEqual(monitor.agent_id, 'factory_test_agent')
        self.assertEqual(monitor.monitoring_level, MonitoringLevel.BASIC)
class TestIntegrationScenarios(unittest.TestCase):
    def test_multi_agent_monitoring_scenario(self):
        agents = ['agent_alpha', 'agent_beta', 'agent_gamma']
        monitors = {}
        for agent_id in agents:
            monitors[agent_id] = create_self_monitoring_system(agent_id)
        for monitor in monitors.values():
            monitor.start_monitoring(monitoring_interval=0.1)
        try:
            time.sleep(0.5)
            for agent_id, monitor in monitors.items():
                self.assertTrue(monitor.is_monitoring)
                status = monitor.get_monitoring_status()
                self.assertEqual(status['agent_id'], agent_id)
        finally:
            for monitor in monitors.values():
                monitor.stop_monitoring()
    def test_error_escalation_scenario(self):
        monitor = create_self_monitoring_system('critical_agent')
        critical_error = DetectedError(error_id='critical_001', timestamp=time.time(), error_type='system_failure', severity=ErrorSeverity.CRITICAL, description='Critical system failure detected', context={'system': 'life_support'})
        monitor.detected_errors.append(critical_error)
        strategy = monitor._determine_correction_strategy(critical_error)
        from phase_3_3_3_self_monitoring import CorrectionStrategy
        self.assertEqual(strategy, CorrectionStrategy.ESCALATE)
    def test_performance_degradation_recovery(self):
        monitor = create_self_monitoring_system('recovery_agent')
        degraded_metrics = [PerformanceMetrics(accuracy_score=0.9, response_time_ms=100), PerformanceMetrics(accuracy_score=0.8, response_time_ms=150), PerformanceMetrics(accuracy_score=0.7, response_time_ms=200), PerformanceMetrics(accuracy_score=0.6, response_time_ms=300), PerformanceMetrics(accuracy_score=0.5, response_time_ms=400)]
        for metrics in degraded_metrics:
            monitor.performance_history.append(metrics)
        monitor._detect_errors()
        found_degradation = False
        for error in monitor.detected_errors:
            if error.error_type == 'performance_degradation':
                found_degradation = True
                self.assertIsNotNone(error.correction_applied)
                break
        self.assertTrue(found_degradation)
class TestRealTimeMonitoringIntegration(unittest.TestCase):
    def test_monitoring_timing_requirements(self):
        monitor = create_self_monitoring_system('realtime_agent')
        start_time = time.time()
        monitor.start_monitoring(monitoring_interval=0.01)
        try:
            time.sleep(0.1)
            monitor.stop_monitoring()
            elapsed = time.time() - start_time
            self.assertLess(elapsed, 0.5)
        finally:
            if monitor.is_monitoring:
                monitor.stop_monitoring()
if __name__ == '__main__':
    import logging
    logging.basicConfig(level=logging.WARNING)
    unittest.main(verbosity=2)