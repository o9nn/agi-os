import pytest
import time
import json
import tempfile
from pathlib import Path
from performance_monitor import UnifiedPerformanceMonitor, PerformanceMetrics, PerformanceAlert, AlertSeverity, create_default_monitor
from performance_integration import IntegratedPerformanceSystem, EchoDashIntegration, AphroditeMetricsCollector, DTESNProfilerIntegration, EchoSelfIntegration, create_integrated_system
class TestPerformanceMetrics:
    def test_metrics_initialization(self):
        timestamp = time.time()
        metrics = PerformanceMetrics(timestamp=timestamp)
        assert metrics.timestamp == timestamp
        assert metrics.token_throughput == 0.0
        assert metrics.cpu_usage == 0.0
        assert metrics.component_id == 'unknown'
        assert isinstance(metrics.metadata, dict)
    def test_metrics_with_values(self):
        timestamp = time.time()
        metrics = PerformanceMetrics(timestamp=timestamp, token_throughput=100.0, cpu_usage=75.0, component_id='test_component')
        assert metrics.timestamp == timestamp
        assert metrics.token_throughput == 100.0
        assert metrics.cpu_usage == 75.0
        assert metrics.component_id == 'test_component'
class TestPerformanceAlert:
    def test_alert_creation(self):
        timestamp = time.time()
        alert = PerformanceAlert(timestamp=timestamp, severity=AlertSeverity.WARNING, component='test_component', metric='cpu_usage', current_value=85.0, threshold=80.0, message='CPU usage exceeded threshold')
        assert alert.timestamp == timestamp
        assert alert.severity == AlertSeverity.WARNING
        assert alert.component == 'test_component'
        assert alert.metric == 'cpu_usage'
        assert alert.current_value == 85.0
        assert alert.threshold == 80.0
        assert 'CPU usage exceeded threshold' in alert.message
class TestUnifiedPerformanceMonitor:
    @pytest.fixture
    def monitor(self):
        return UnifiedPerformanceMonitor()
    def test_monitor_initialization(self, monitor):
        assert not monitor.is_monitoring
        assert monitor.monitor_thread is None
        assert len(monitor.component_collectors) == 5
        assert len(monitor.metrics_history) == 0
        assert len(monitor.alerts_history) == 0
    def test_register_collector(self, monitor):
        def test_collector():
            return {'test_metric': 42.0}
        monitor.register_collector('test_component', test_collector)
        assert 'test_component' in monitor.component_collectors
        assert monitor.component_collectors['test_component'] == test_collector
    def test_register_alert_handler(self, monitor):
        handler_called = []
        def test_handler(alert):
            handler_called.append(alert)
        monitor.register_alert_handler(test_handler)
        assert test_handler in monitor.alert_handlers
    def test_system_metrics_collection(self, monitor):
        metrics = monitor._collect_system_metrics()
        assert isinstance(metrics, dict)
        assert 'cpu_usage' in metrics
        assert 'memory_usage' in metrics
        assert 'disk_usage' in metrics
        assert 'timestamp' in metrics
        assert isinstance(metrics['cpu_usage'], float)
    def test_threshold_violation_detection(self, monitor):
        timestamp = time.time()
        metrics = PerformanceMetrics(timestamp=timestamp, cpu_usage=95.0, request_latency_ms=1500.0, component_id='test')
        alerts = monitor._check_threshold_violations(metrics)
        assert len(alerts) >= 2
        cpu_alert = next((a for a in alerts if a.metric == 'cpu_usage'), None)
        latency_alert = next((a for a in alerts if a.metric == 'request_latency_ms'), None)
        assert cpu_alert is not None
        assert cpu_alert.severity == AlertSeverity.WARNING
        assert cpu_alert.current_value == 95.0
        assert latency_alert is not None
        assert latency_alert.severity == AlertSeverity.WARNING
        assert latency_alert.current_value == 1500.0
    def test_performance_degradation_detection(self, monitor):
        timestamps = [time.time() - i for i in range(10, 0, -1)]
        for i, timestamp in enumerate(timestamps):
            metrics = PerformanceMetrics(timestamp=timestamp, token_throughput=100.0 - i * 5, component_id='test')
            monitor.metrics_history.append(metrics)
        latest_metrics = PerformanceMetrics(timestamp=time.time(), token_throughput=50.0, component_id='test')
        alerts = monitor._check_performance_degradation(latest_metrics)
        degradation_alert = next((a for a in alerts if a.metric == 'token_throughput'), None)
        if degradation_alert:
            assert degradation_alert.severity == AlertSeverity.WARNING
            assert 'degradation trend' in degradation_alert.message.lower()
    def test_trend_calculation(self, monitor):
        declining_values = [100, 95, 90, 85, 80]
        trend = monitor._calculate_trend(declining_values)
        assert trend < 0
        improving_values = [50, 60, 70, 80, 90]
        trend = monitor._calculate_trend(improving_values)
        assert trend > 0
        stable_values = [75, 75, 75, 75, 75]
        trend = monitor._calculate_trend(stable_values)
        assert abs(trend) < 0.01
    def test_performance_summary(self, monitor):
        timestamp = time.time()
        metrics = PerformanceMetrics(timestamp=timestamp, cpu_usage=50.0)
        monitor.metrics_history.append(metrics)
        alert = PerformanceAlert(timestamp=timestamp, severity=AlertSeverity.INFO, component='test', metric='test_metric', current_value=10.0, threshold=20.0, message='Test alert')
        monitor.alerts_history.append(alert)
        summary = monitor.get_performance_summary()
        assert 'timestamp' in summary
        assert summary['monitoring_active'] == False
        assert summary['metrics_count'] == 1
        assert summary['alerts_count'] == 1
        assert summary['current_metrics'] is not None
    @pytest.mark.asyncio
    async def test_monitoring_lifecycle(self, monitor):
        assert not monitor.is_monitoring
        monitor.start_monitoring()
        time.sleep(0.1)
        assert monitor.is_monitoring
        assert monitor.monitor_thread is not None
        monitor.stop_monitoring()
        time.sleep(0.1)
        assert not monitor.is_monitoring
class TestAphroditeMetricsCollector:
    def test_collector_initialization(self):
        collector = AphroditeMetricsCollector()
        assert collector.last_collection_time == 0
    def test_metrics_collection(self):
        collector = AphroditeMetricsCollector()
        metrics = collector.collect_metrics()
        assert isinstance(metrics, dict)
        assert 'token_throughput' in metrics
        assert 'request_latency_ms' in metrics
        assert 'gpu_utilization' in metrics
        assert 'collection_time' in metrics
        assert isinstance(metrics['token_throughput'], (int, float))
        assert metrics['token_throughput'] > 0
        assert isinstance(metrics['gpu_utilization'], (int, float))
        assert 0 <= metrics['gpu_utilization'] <= 100
class TestDTESNProfilerIntegration:
    def test_profiler_initialization(self):
        profiler = DTESNProfilerIntegration()
        assert not profiler.profiling_active
    def test_dtesn_metrics_collection(self):
        profiler = DTESNProfilerIntegration()
        metrics = profiler.collect_dtesn_metrics()
        assert isinstance(metrics, dict)
        assert 'membrane_evolution_rate' in metrics
        assert 'reservoir_dynamics' in metrics
        assert 'membrane_level' in metrics
        assert 'oeis_a000081_level' in metrics
        assert isinstance(metrics['membrane_level'], int)
        assert metrics['membrane_level'] >= 1
        assert metrics['oeis_a000081_level'] == metrics['membrane_level']
class TestEchoSelfIntegration:
    def test_echo_self_initialization(self):
        integration = EchoSelfIntegration()
        assert not integration.evolution_active
    def test_echo_self_metrics_collection(self):
        integration = EchoSelfIntegration()
        metrics = integration.collect_echo_self_metrics()
        assert isinstance(metrics, dict)
        assert 'evolution_convergence' in metrics
        assert 'fitness_improvement' in metrics
        assert 'agent_performance' in metrics
        assert 'self_monitoring_active' in metrics
        assert 0 <= metrics['evolution_convergence'] <= 1
        assert 0 <= metrics['agent_performance'] <= 1
class TestEchoDashIntegration:
    @pytest.fixture
    def temp_stats_dir(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            yield temp_dir
    def test_echo_dash_initialization(self, temp_stats_dir):
        monitor = UnifiedPerformanceMonitor()
        integration = EchoDashIntegration(monitor, temp_stats_dir)
        assert integration.monitor == monitor
        assert integration.stats_dir == Path(temp_stats_dir)
        assert integration.stats_dir.exists()
    def test_dashboard_metrics_export(self, temp_stats_dir):
        monitor = UnifiedPerformanceMonitor()
        integration = EchoDashIntegration(monitor, temp_stats_dir)
        timestamp = time.time()
        metrics = PerformanceMetrics(timestamp=timestamp, cpu_usage=50.0, memory_usage=60.0, token_throughput=100.0)
        monitor.metrics_history.append(metrics)
        dashboard_data = integration.export_metrics_for_dashboard()
        assert isinstance(dashboard_data, dict)
        assert 'timestamp' in dashboard_data
        assert 'system' in dashboard_data
        assert 'process' in dashboard_data
        assert 'dtesn' in dashboard_data
        assert 'echo_self' in dashboard_data
        assert 'embodied' in dashboard_data
        assert 'alerts' in dashboard_data
        assert dashboard_data['system']['cpu_percent'] == 50.0
        assert dashboard_data['system']['memory_percent'] == 60.0
    def test_alert_handling(self, temp_stats_dir):
        monitor = UnifiedPerformanceMonitor()
        integration = EchoDashIntegration(monitor, temp_stats_dir)
        alert = PerformanceAlert(timestamp=time.time(), severity=AlertSeverity.WARNING, component='test', metric='cpu_usage', current_value=90.0, threshold=85.0, message='Test alert')
        integration._handle_echo_alert(alert)
        alert_files = list(Path(temp_stats_dir).glob('alert_*.json'))
        assert len(alert_files) >= 1
        with open(alert_files[0], 'r') as f:
            saved_alert = json.load(f)
        assert saved_alert['severity'] == 'warning'
        assert saved_alert['component'] == 'test'
        assert saved_alert['metric'] == 'cpu_usage'
class TestIntegratedPerformanceSystem:
    def test_system_initialization(self):
        system = IntegratedPerformanceSystem()
        assert system.monitor is not None
        assert system.echo_dash is not None
        assert system.aphrodite_collector is not None
        assert system.dtesn_profiler is not None
        assert system.echo_self is not None
    def test_system_with_config(self):
        config = {'test_setting': 'test_value'}
        system = IntegratedPerformanceSystem(config)
        assert system.config == config
    def test_enhanced_collectors_registration(self):
        system = IntegratedPerformanceSystem()
        assert 'aphrodite' in system.monitor.component_collectors
        assert 'dtesn' in system.monitor.component_collectors
        assert 'echo_self' in system.monitor.component_collectors
        aphrodite_metrics = system.monitor.component_collectors['aphrodite']()
        dtesn_metrics = system.monitor.component_collectors['dtesn']()
        echo_self_metrics = system.monitor.component_collectors['echo_self']()
        assert isinstance(aphrodite_metrics, dict)
        assert isinstance(dtesn_metrics, dict)
        assert isinstance(echo_self_metrics, dict)
    def test_comprehensive_status(self):
        system = IntegratedPerformanceSystem()
        status = system.get_comprehensive_status()
        assert isinstance(status, dict)
        assert 'system_status' in status
        assert 'performance_summary' in status
        assert 'dashboard_data' in status
        assert 'component_status' in status
        component_status = status['component_status']
        assert component_status['aphrodite_collector'] == 'active'
        assert component_status['dtesn_profiler'] == 'active'
        assert component_status['echo_self'] == 'active'
        assert component_status['echo_dash_integration'] == 'active'
    def test_performance_report_export(self):
        system = IntegratedPerformanceSystem()
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as temp_file:
            temp_path = temp_file.name
        try:
            system.export_performance_report(temp_path)
            assert Path(temp_path).exists()
            with open(temp_path, 'r') as f:
                report_data = json.load(f)
            assert isinstance(report_data, dict)
            assert 'system_status' in report_data
            assert 'performance_summary' in report_data
        finally:
            Path(temp_path).unlink(missing_ok=True)
    @pytest.mark.asyncio
    async def test_system_lifecycle(self):
        system = IntegratedPerformanceSystem()
        system.start()
        time.sleep(0.1)
        assert system.monitor.is_monitoring
        system.stop()
        time.sleep(0.1)
        assert not system.monitor.is_monitoring
class TestFactoryFunctions:
    def test_create_default_monitor(self):
        monitor = create_default_monitor()
        assert isinstance(monitor, UnifiedPerformanceMonitor)
        assert len(monitor.alert_handlers) >= 1
    def test_create_integrated_system(self):
        system = create_integrated_system()
        assert isinstance(system, IntegratedPerformanceSystem)
        assert system.config == {}
    def test_create_integrated_system_with_config_file(self):
        config_data = {'test_key': 'test_value', 'monitoring_interval': 0.5}
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as temp_file:
            json.dump(config_data, temp_file)
            config_path = temp_file.name
        try:
            system = create_integrated_system(config_path)
            assert system.config == config_data
        finally:
            Path(config_path).unlink(missing_ok=True)
class TestPerformanceAcceptanceCriteria:
    def test_real_time_metrics_collection(self):
        system = IntegratedPerformanceSystem()
        system.start()
        try:
            time.sleep(2)
            current_metrics = system.monitor.get_current_metrics()
            assert current_metrics is not None
            assert current_metrics.timestamp > 0
            assert current_metrics.cpu_usage >= 0
            assert current_metrics.token_throughput >= 0
            assert current_metrics.membrane_evolution_rate >= 0
            assert current_metrics.evolution_convergence >= 0
        finally:
            system.stop()
    def test_automated_performance_analysis(self):
        monitor = UnifiedPerformanceMonitor()
        high_latency_metrics = PerformanceMetrics(timestamp=time.time(), request_latency_ms=1500.0, cpu_usage=95.0, component_id='test')
        alerts = monitor._check_threshold_violations(high_latency_metrics)
        assert len(alerts) >= 2
        for i in range(10):
            degrading_metrics = PerformanceMetrics(timestamp=time.time() - (10 - i), token_throughput=100.0 - i * 5, component_id='test')
            monitor.metrics_history.append(degrading_metrics)
        trend_alerts = monitor._check_performance_degradation(high_latency_metrics)
        assert isinstance(trend_alerts, list)
    def test_alert_system_for_degradation(self):
        system = IntegratedPerformanceSystem()
        captured_alerts = []
        def capture_alerts(alert):
            captured_alerts.append(alert)
        system.monitor.register_alert_handler(capture_alerts)
        degraded_metrics = PerformanceMetrics(timestamp=time.time(), cpu_usage=95.0, request_latency_ms=2000.0, token_throughput=30.0, component_id='test')
        alerts = system.monitor._analyze_performance(degraded_metrics)
        assert len(alerts) >= 3
        critical_alerts = [a for a in alerts if a.severity == AlertSeverity.CRITICAL]
        warning_alerts = [a for a in alerts if a.severity == AlertSeverity.WARNING]
        assert len(critical_alerts) >= 1
        assert len(warning_alerts) >= 1
    def test_comprehensive_monitoring_coverage(self):
        system = IntegratedPerformanceSystem()
        system.start()
        time.sleep(1)
        system.stop()
        status = system.get_comprehensive_status()
        component_status = status['component_status']
        required_components = ['aphrodite_collector', 'dtesn_profiler', 'echo_self', 'echo_dash_integration']
        for component in required_components:
            assert component in component_status
            assert component_status[component] == 'active'
        if status['performance_summary']['current_metrics']:
            metrics = status['performance_summary']['current_metrics']
            assert 'token_throughput' in metrics
            assert 'request_latency_ms' in metrics
            assert 'gpu_utilization' in metrics
            assert 'membrane_evolution_rate' in metrics
            assert 'reservoir_dynamics' in metrics
            assert 'evolution_convergence' in metrics
            assert 'fitness_improvement' in metrics
            assert 'cpu_usage' in metrics
            assert 'memory_usage' in metrics
            assert 'sensory_motor_latency' in metrics
            assert 'proprioceptive_accuracy' in metrics
if __name__ == '__main__':
    pytest.main([__file__, '-v'])