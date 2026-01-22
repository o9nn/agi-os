import pytest
import time
import threading
from unittest.mock import Mock, patch, MagicMock
from dataclasses import asdict
from aphrodite.monitoring.backend_monitor import BackendPerformanceMonitor, BackendMetrics, PerformanceThresholds, AlertMessage, create_backend_monitor
class TestBackendPerformanceMonitor:
    @pytest.fixture
    def monitor(self):
        return BackendPerformanceMonitor(collection_interval=0.1, metrics_history_size=10, enable_deep_tree_echo=False)
    @pytest.fixture
    def sample_metrics(self):
        return BackendMetrics(timestamp=time.time(), cpu_usage_percent=45.5, memory_usage_percent=62.3, memory_usage_gb=8.2, disk_io_read_mb=12.5, disk_io_write_mb=8.3, network_io_recv_mb=15.2, network_io_sent_mb=9.8, token_throughput=125.5, request_latency_p50=45.2, request_latency_p95=128.7, request_latency_p99=256.3, active_requests=6, queued_requests=2, gpu_memory_usage_percent=72.1, kv_cache_usage_percent=58.9, requests_per_second=22.4, error_rate_percent=1.8, success_rate_percent=98.2)
    def test_monitor_initialization(self, monitor):
        assert not monitor.is_monitoring
        assert monitor.collection_interval == 0.1
        assert monitor.metrics_history_size == 10
        assert not monitor.enable_deep_tree_echo
        assert len(monitor.metrics_history) == 0
        assert len(monitor.alerts_history) == 0
        assert isinstance(monitor.thresholds, PerformanceThresholds)
    def test_start_stop_monitoring(self, monitor):
        monitor.start_monitoring()
        assert monitor.is_monitoring
        assert monitor.monitor_thread is not None
        assert monitor.monitor_thread.is_alive()
        time.sleep(0.25)
        assert len(monitor.metrics_history) > 0
        monitor.stop_monitoring()
        assert not monitor.is_monitoring
    def test_metrics_collection(self, monitor):
        with patch('psutil.cpu_percent', return_value=75.5), patch('psutil.virtual_memory') as mock_memory, patch('psutil.disk_io_counters') as mock_disk, patch('psutil.net_io_counters') as mock_network:
            mock_memory.return_value.percent = 68.2
            mock_memory.return_value.used = 8589934592
            mock_disk.return_value.read_bytes = 1048576000
            mock_disk.return_value.write_bytes = 524288000
            mock_network.return_value.bytes_recv = 2097152000
            mock_network.return_value.bytes_sent = 1048576000
            metrics = monitor._collect_current_metrics()
            assert metrics.cpu_usage_percent == 75.5
            assert metrics.memory_usage_percent == 68.2
            assert metrics.memory_usage_gb == 8.0
            assert isinstance(metrics.timestamp, float)
            assert metrics.token_throughput > 0
    def test_alert_generation(self, monitor, sample_metrics):
        monitor.thresholds.max_cpu_usage = 40.0
        monitor.thresholds.max_memory_usage = 50.0
        monitor.thresholds.max_error_rate = 1.0
        alerts = monitor._analyze_performance(sample_metrics)
        assert len(alerts) >= 2
        alert_metrics = {alert.metric_name for alert in alerts}
        assert 'cpu_usage' in alert_metrics
        assert 'memory_usage' in alert_metrics
        cpu_alert = next((a for a in alerts if a.metric_name == 'cpu_usage'))
        assert cpu_alert.severity in ['WARNING', 'CRITICAL']
        assert cpu_alert.current_value == sample_metrics.cpu_usage_percent
        assert cpu_alert.threshold == monitor.thresholds.max_cpu_usage
    def test_alert_handler_registration(self, monitor):
        alerts_received = []
        def test_handler(alert):
            alerts_received.append(alert)
        monitor.register_alert_handler(test_handler)
        assert len(monitor.alert_handlers) == 1
        test_alert = AlertMessage(timestamp=time.time(), severity='WARNING', metric_name='test_metric', current_value=100.0, threshold=50.0, message='Test alert', component='test')
        monitor._process_alert(test_alert)
        assert len(alerts_received) == 1
        assert alerts_received[0].metric_name == 'test_metric'
    def test_component_collector_registration(self, monitor):
        def test_collector():
            return {'test_metric': 42.0, 'another_metric': 'test_value'}
        monitor.register_component_collector('test_component', test_collector)
        assert 'test_component' in monitor.component_collectors
        result = monitor.component_collectors['test_component']()
        assert result['test_metric'] == 42.0
        assert result['another_metric'] == 'test_value'
    def test_performance_summary(self, monitor, sample_metrics):
        monitor.metrics_history.append(sample_metrics)
        summary = monitor.get_performance_summary()
        assert 'timestamp' in summary
        assert 'status' in summary
        assert 'metrics' in summary
        assert 'alerts' in summary
        assert 'performance_status' in summary
        metrics = summary['metrics']
        assert 'cpu_usage' in metrics
        assert 'memory_usage' in metrics
        assert 'token_throughput' in metrics
    def test_metrics_history_retrieval(self, monitor):
        current_time = time.time()
        for i in range(5):
            metrics = BackendMetrics(timestamp=current_time - i * 60, cpu_usage_percent=50.0 + i, memory_usage_percent=60.0, memory_usage_gb=8.0, disk_io_read_mb=10.0, disk_io_write_mb=5.0, network_io_recv_mb=15.0, network_io_sent_mb=8.0, token_throughput=100.0, request_latency_p50=50.0, request_latency_p95=100.0, request_latency_p99=200.0, active_requests=5, queued_requests=1, gpu_memory_usage_percent=70.0, kv_cache_usage_percent=60.0, requests_per_second=20.0, error_rate_percent=1.0, success_rate_percent=99.0)
            monitor.metrics_history.append(metrics)
        history = monitor.get_metrics_history(minutes=10)
        assert len(history) == 5
        history = monitor.get_metrics_history(minutes=2)
        assert len(history) <= 3
    def test_regression_detection(self, monitor):
        monitor.performance_baselines['test_metric'] = 100.0
        for i in range(monitor.regression_window_size):
            metrics = BackendMetrics(timestamp=time.time(), cpu_usage_percent=50.0, memory_usage_percent=60.0, memory_usage_gb=8.0, disk_io_read_mb=10.0, disk_io_write_mb=5.0, network_io_recv_mb=15.0, network_io_sent_mb=8.0, token_throughput=90.0 - i, request_latency_p50=50.0, request_latency_p95=100.0, request_latency_p99=200.0, active_requests=5, queued_requests=1, gpu_memory_usage_percent=70.0, kv_cache_usage_percent=60.0, requests_per_second=20.0, error_rate_percent=1.0, success_rate_percent=99.0)
            monitor.metrics_history.append(metrics)
        latest_metrics = monitor.metrics_history[-1]
        monitor._check_regression(latest_metrics)
    def test_prometheus_metrics_integration(self, monitor):
        assert hasattr(monitor, 'prom_cpu_usage')
        assert hasattr(monitor, 'prom_memory_usage')
        assert hasattr(monitor, 'prom_requests_per_sec')
        test_metrics = sample_metrics = BackendMetrics(timestamp=time.time(), cpu_usage_percent=75.5, memory_usage_percent=68.2, memory_usage_gb=8.2, disk_io_read_mb=12.5, disk_io_write_mb=8.3, network_io_recv_mb=15.2, network_io_sent_mb=9.8, token_throughput=125.5, request_latency_p50=45.2, request_latency_p95=128.7, request_latency_p99=256.3, active_requests=6, queued_requests=2, gpu_memory_usage_percent=72.1, kv_cache_usage_percent=58.9, requests_per_second=22.4, error_rate_percent=1.8, success_rate_percent=98.2)
        monitor._update_prometheus_metrics(test_metrics)
    def test_export_functionality(self, monitor, sample_metrics):
        monitor.metrics_history.append(sample_metrics)
        test_alert = AlertMessage(timestamp=time.time(), severity='WARNING', metric_name='test_metric', current_value=100.0, threshold=50.0, message='Test alert', component='test')
        monitor.alerts_history.append(test_alert)
        exported = monitor.export_metrics_to_dict()
        assert 'metrics_history' in exported
        assert 'alerts_history' in exported
        assert 'summary' in exported
        assert len(exported['metrics_history']) == 1
        assert len(exported['alerts_history']) == 1
        metrics_data = exported['metrics_history'][0]
        assert 'timestamp' in metrics_data
        assert 'cpu_usage' in metrics_data
        assert 'token_throughput' in metrics_data
    def test_factory_function(self):
        monitor = create_backend_monitor(collection_interval=2.0, enable_deep_tree_echo=False)
        assert isinstance(monitor, BackendPerformanceMonitor)
        assert monitor.collection_interval == 2.0
        assert not monitor.enable_deep_tree_echo
    @pytest.mark.asyncio
    async def test_concurrent_access(self, monitor):
        results = []
        def collect_metrics():
            for _ in range(10):
                metrics = monitor.get_current_metrics()
                results.append(metrics is not None)
                time.sleep(0.01)
        monitor.start_monitoring()
        time.sleep(0.1)
        threads = [threading.Thread(target=collect_metrics) for _ in range(3)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()
        monitor.stop_monitoring()
        assert len(results) == 30
    def test_deep_tree_echo_integration(self):
        monitor = BackendPerformanceMonitor(enable_deep_tree_echo=True, collection_interval=0.1)
        assert hasattr(monitor, 'prom_aar_agents')
        assert hasattr(monitor, 'prom_dtesn_rate')
        assert hasattr(monitor, 'prom_evolution_score')
        echo_metrics = monitor._get_echo_metrics()
        assert isinstance(echo_metrics, dict)
        assert 'aar_agents' in echo_metrics
        assert 'dtesn_rate' in echo_metrics
        assert 'evolution_score' in echo_metrics
    def test_memory_management(self, monitor):
        for i in range(monitor.metrics_history_size * 2):
            metrics = BackendMetrics(timestamp=time.time() + i, cpu_usage_percent=50.0, memory_usage_percent=60.0, memory_usage_gb=8.0, disk_io_read_mb=10.0, disk_io_write_mb=5.0, network_io_recv_mb=15.0, network_io_sent_mb=8.0, token_throughput=100.0, request_latency_p50=50.0, request_latency_p95=100.0, request_latency_p99=200.0, active_requests=5, queued_requests=1, gpu_memory_usage_percent=70.0, kv_cache_usage_percent=60.0, requests_per_second=20.0, error_rate_percent=1.0, success_rate_percent=99.0)
            monitor.metrics_history.append(metrics)
        assert len(monitor.metrics_history) <= monitor.metrics_history_size
if __name__ == '__main__':
    pytest.main([__file__])