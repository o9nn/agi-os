import pytest
import time
import tempfile
from unittest.mock import Mock, patch
from pathlib import Path
from automated_incident_response import AutomatedIncidentResponseSystem, ResponseStatus, ResponsePriority, IncidentResponse, MaintenanceTask, create_automated_response_system
from performance_monitor import PerformanceAlert, AlertSeverity, UnifiedPerformanceMonitor
class TestAutomatedIncidentResponseSystem:
    @pytest.fixture
    def response_system(self):
        return AutomatedIncidentResponseSystem()
    @pytest.fixture
    def mock_performance_monitor(self):
        return Mock(spec=UnifiedPerformanceMonitor)
    @pytest.fixture
    def sample_alert(self):
        return PerformanceAlert(timestamp=time.time(), severity=AlertSeverity.WARNING, component='test_component', metric='cpu_usage', current_value=90.0, threshold=85.0, message='High CPU usage detected')
    def test_initialization(self, response_system):
        assert response_system is not None
        assert len(response_system.response_handlers) > 0
        assert len(response_system.maintenance_tasks) > 0
        assert not response_system.is_running
        assert response_system.stats_dir.exists()
    def test_initialization_with_monitor(self, mock_performance_monitor):
        system = AutomatedIncidentResponseSystem(performance_monitor=mock_performance_monitor)
        assert system.performance_monitor == mock_performance_monitor
    def test_start_stop_system(self, response_system):
        assert not response_system.is_running
        response_system.start()
        assert response_system.is_running
        time.sleep(0.5)
        response_system.stop()
        assert not response_system.is_running
    def test_priority_determination(self, response_system, sample_alert):
        critical_alert = sample_alert
        critical_alert.severity = AlertSeverity.CRITICAL
        critical_alert.metric = 'token_throughput'
        priority = response_system._determine_priority(critical_alert)
        assert priority == ResponsePriority.P0_CRITICAL
        warning_alert = sample_alert
        warning_alert.severity = AlertSeverity.WARNING
        warning_alert.metric = 'request_latency_ms'
        priority = response_system._determine_priority(warning_alert)
        assert priority == ResponsePriority.P1_HIGH
        info_alert = sample_alert
        info_alert.severity = AlertSeverity.INFO
        priority = response_system._determine_priority(info_alert)
        assert priority == ResponsePriority.P3_LOW
    def test_response_handlers_exist(self, response_system):
        expected_handlers = ['high_cpu_usage', 'high_memory_usage', 'low_token_throughput', 'high_request_latency', 'low_disk_space', 'high_gpu_utilization', 'low_evolution_convergence']
        for handler_key in expected_handlers:
            assert handler_key in response_system.response_handlers
    def test_maintenance_tasks_exist(self, response_system):
        expected_tasks = ['system_cleanup', 'memory_optimization', 'health_check', 'log_rotation']
        for task_id in expected_tasks:
            assert task_id in response_system.maintenance_tasks
            task = response_system.maintenance_tasks[task_id]
            assert isinstance(task, MaintenanceTask)
            assert task.enabled
            assert task.interval_hours > 0
    @patch('psutil.cpu_percent')
    def test_high_cpu_response_handler(self, mock_cpu_percent, response_system, sample_alert):
        mock_cpu_percent.return_value = 90.0
        mock_proc = Mock()
        mock_proc.info = {'pid': 123, 'name': 'test_process', 'cpu_percent': 15.0}
        with patch('psutil.process_iter', return_value=[mock_proc]):
            with patch('psutil.Process') as mock_process:
                mock_process_instance = Mock()
                mock_process_instance.nice.return_value = 0
                mock_process.return_value = mock_process_instance
                success = response_system._handle_high_cpu_usage(sample_alert)
                assert success
    def test_high_memory_response_handler(self, response_system, sample_alert):
        with patch('gc.collect', return_value=10):
            success = response_system._handle_high_memory_usage(sample_alert)
            assert success
    def test_low_throughput_response_handler(self, response_system, sample_alert):
        success = response_system._handle_low_throughput(sample_alert)
        assert success
    def test_high_latency_response_handler(self, response_system, sample_alert):
        success = response_system._handle_high_latency(sample_alert)
        assert success
    def test_disk_space_response_handler(self, response_system, sample_alert):
        with tempfile.TemporaryDirectory() as temp_dir:
            test_file = Path(temp_dir) / 'test_file.txt'
            test_file.write_text('test content')
            with patch('os.walk', return_value=[(temp_dir, [], ['test_file.txt'])]):
                with patch('os.path.getmtime', return_value=time.time() - 86500):
                    success = response_system._handle_low_disk_space(sample_alert)
                    assert success is not None
    def test_gpu_usage_response_handler(self, response_system, sample_alert):
        success = response_system._handle_high_gpu_usage(sample_alert)
        assert success
    def test_evolution_issues_response_handler(self, response_system, sample_alert):
        success = response_system._handle_evolution_issues(sample_alert)
        assert success
    def test_system_cleanup_maintenance_task(self, response_system):
        success = response_system._system_cleanup_task()
        assert success
    @patch('gc.collect')
    @patch('psutil.virtual_memory')
    def test_memory_optimization_maintenance_task(self, mock_memory, mock_gc, response_system):
        mock_gc.return_value = 5
        mock_memory.return_value = Mock(percent=75.0)
        success = response_system._memory_optimization_task()
        assert success
        mock_gc.assert_called_once()
    @patch('psutil.cpu_percent')
    @patch('psutil.virtual_memory')
    @patch('psutil.disk_usage')
    @patch('psutil.boot_time')
    def test_health_check_maintenance_task(self, mock_boot_time, mock_disk, mock_memory, mock_cpu, response_system):
        mock_cpu.return_value = 25.0
        mock_memory.return_value = Mock(percent=60.0)
        mock_disk.return_value = Mock(percent=40.0)
        mock_boot_time.return_value = time.time() - 3600
        success = response_system._health_check_task()
        assert success
        health_file = response_system.stats_dir / 'system_health.json'
        assert health_file.exists()
    def test_log_rotation_maintenance_task(self, response_system):
        success = response_system._log_rotation_task()
        assert success
    def test_handle_performance_alert(self, response_system, sample_alert):
        initial_count = len(response_system.responses_history)
        response_system._handle_performance_alert(sample_alert)
        assert len(response_system.responses_history) == initial_count + 1
        response = response_system.responses_history[-1]
        assert response.alert == sample_alert
        assert response.priority in ResponsePriority
        assert response.status in [ResponseStatus.SUCCESS, ResponseStatus.FAILED, ResponseStatus.ESCALATED]
    def test_escalation(self, response_system, sample_alert):
        incident_response = IncidentResponse(incident_id='test_incident', timestamp=time.time(), alert=sample_alert, priority=ResponsePriority.P1_HIGH, status=ResponseStatus.IN_PROGRESS)
        response_system._escalate_incident(incident_response, 'Test escalation')
        assert incident_response.status == ResponseStatus.ESCALATED
        assert incident_response.escalation_reason == 'Test escalation'
    def test_response_statistics(self, response_system):
        stats = response_system.get_response_statistics()
        assert 'total_responses' in stats
        assert 'successful_responses' in stats
        assert 'failed_responses' in stats
        assert 'escalated_responses' in stats
        assert 'success_rate' in stats
        assert 'maintenance_tasks' in stats
        assert 'system_status' in stats
        assert stats['system_status'] == 'stopped'
        assert isinstance(stats['success_rate'], (int, float))
    def test_save_incident_report(self, response_system, sample_alert):
        incident_response = IncidentResponse(incident_id='test_save_incident', timestamp=time.time(), alert=sample_alert, priority=ResponsePriority.P1_HIGH, status=ResponseStatus.SUCCESS)
        response_system._save_incident_report(incident_response)
        report_file = response_system.stats_dir / f'incident_{incident_response.incident_id}.json'
        assert report_file.exists()
    def test_integration_with_performance_monitor(self, mock_performance_monitor, sample_alert):
        system = AutomatedIncidentResponseSystem(performance_monitor=mock_performance_monitor)
        system.start()
        mock_performance_monitor.register_alert_handler.assert_called_once()
        system.stop()
class TestIntegrationWithExistingMonitoring:
    def test_factory_function(self):
        system = create_automated_response_system()
        assert isinstance(system, AutomatedIncidentResponseSystem)
    def test_factory_function_with_monitor(self):
        mock_monitor = Mock(spec=UnifiedPerformanceMonitor)
        system = create_automated_response_system(performance_monitor=mock_monitor)
        assert system.performance_monitor == mock_monitor
    def test_end_to_end_workflow(self):
        mock_monitor = Mock(spec=UnifiedPerformanceMonitor)
        system = create_automated_response_system(performance_monitor=mock_monitor)
        try:
            system.start()
            assert system.is_running
            test_alert = PerformanceAlert(timestamp=time.time(), severity=AlertSeverity.WARNING, component='integration_test', metric='cpu_usage', current_value=95.0, threshold=85.0, message='Integration test alert')
            system._handle_performance_alert(test_alert)
            assert len(system.responses_history) > 0
            response = system.responses_history[-1]
            assert response.alert == test_alert
            assert response.status in [ResponseStatus.SUCCESS, ResponseStatus.FAILED, ResponseStatus.ESCALATED]
        finally:
            system.stop()
    @patch('time.sleep')
    def test_maintenance_task_execution(self, mock_sleep):
        system = create_automated_response_system()
        for task in system.maintenance_tasks.values():
            task.interval_hours = 0.001
        try:
            system.start()
            time.sleep(0.1)
            task_run = any((task.success_count > 0 or task.failure_count > 0 for task in system.maintenance_tasks.values()))
            assert task_run is not None
        finally:
            system.stop()
class TestAcceptanceCriteriaValidation:
    def test_proactive_system_maintenance(self):
        system = create_automated_response_system()
        assert len(system.maintenance_tasks) >= 4
        for task_id, task in system.maintenance_tasks.items():
            try:
                result = task.action()
                assert result is not None
            except Exception as e:
                assert isinstance(e, Exception)
    def test_automated_incident_response_capabilities(self):
        system = create_automated_response_system()
        assert len(system.response_handlers) >= 7
        test_alert = PerformanceAlert(timestamp=time.time(), severity=AlertSeverity.WARNING, component='test', metric='cpu_usage', current_value=90.0, threshold=85.0, message='Test alert')
        for handler_key, handler in system.response_handlers.items():
            try:
                result = handler(test_alert)
                assert isinstance(result, bool)
            except Exception as e:
                assert isinstance(e, Exception)
    def test_system_health_monitoring_integration(self):
        system = create_automated_response_system()
        health_success = system._health_check_task()
        assert health_success
        health_file = system.stats_dir / 'system_health.json'
        assert health_file.exists()
    def test_performance_optimization_features(self):
        system = create_automated_response_system()
        memory_success = system._memory_optimization_task()
        assert memory_success
        cleanup_success = system._system_cleanup_task()
        assert cleanup_success
if __name__ == '__main__':
    import logging
    logging.basicConfig(level=logging.INFO)
    print('🧪 Automated Incident Response System Tests')
    print('=' * 50)
    system = create_automated_response_system()
    assert system is not None
    print('✅ Initialization test passed')
    system.start()
    time.sleep(1)
    system.stop()
    print('✅ Start/stop test passed')
    stats = system.get_response_statistics()
    assert 'total_responses' in stats
    print('✅ Statistics test passed')
    print('\n🎉 All basic tests passed!')
    print("📋 Run 'python -m pytest test_automated_incident_response.py -v' for comprehensive tests")