"""
Tests for Backend Performance Monitor.

Comprehensive tests for the backend performance monitoring system including
metrics collection, alerting, and integration with existing infrastructure.
"""

import pytest
import time
import threading
from unittest.mock import Mock, patch, MagicMock
from dataclasses import asdict

from aphrodite.monitoring.backend_monitor import (
    BackendPerformanceMonitor,
    BackendMetrics,
    PerformanceThresholds,
    AlertMessage,
    create_backend_monitor
)


class TestBackendPerformanceMonitor:
    """Test suite for BackendPerformanceMonitor."""

    @pytest.fixture
    def monitor(self):
        """Create a test monitor instance."""
        return BackendPerformanceMonitor(
            collection_interval=0.1,  # Fast collection for testing
            metrics_history_size=10,
            enable_deep_tree_echo=False  # Disable for basic tests
        )

    @pytest.fixture
    def sample_metrics(self):
        """Create sample metrics for testing."""
        return BackendMetrics(
            timestamp=time.time(),
            cpu_usage_percent=45.5,
            memory_usage_percent=62.3,
            memory_usage_gb=8.2,
            disk_io_read_mb=12.5,
            disk_io_write_mb=8.3,
            network_io_recv_mb=15.2,
            network_io_sent_mb=9.8,
            token_throughput=125.5,
            request_latency_p50=45.2,
            request_latency_p95=128.7,
            request_latency_p99=256.3,
            active_requests=6,
            queued_requests=2,
            gpu_memory_usage_percent=72.1,
            kv_cache_usage_percent=58.9,
            requests_per_second=22.4,
            error_rate_percent=1.8,
            success_rate_percent=98.2,
        )

    def test_monitor_initialization(self, monitor):
        """Test monitor initialization."""
        assert not monitor.is_monitoring
        assert monitor.collection_interval == 0.1
        assert monitor.metrics_history_size == 10
        assert not monitor.enable_deep_tree_echo
        assert len(monitor.metrics_history) == 0
        assert len(monitor.alerts_history) == 0
        assert isinstance(monitor.thresholds, PerformanceThresholds)

    def test_start_stop_monitoring(self, monitor):
        """Test starting and stopping monitoring."""
        # Start monitoring
        monitor.start_monitoring()
        assert monitor.is_monitoring
        assert monitor.monitor_thread is not None
        assert monitor.monitor_thread.is_alive()

        # Give it a moment to collect some data
        time.sleep(0.25)

        # Should have collected some metrics
        assert len(monitor.metrics_history) > 0

        # Stop monitoring
        monitor.stop_monitoring()
        assert not monitor.is_monitoring

    def test_metrics_collection(self, monitor):
        """Test metrics collection functionality."""
        # Mock system metrics
        with patch('psutil.cpu_percent', return_value=75.5), \
             patch('psutil.virtual_memory') as mock_memory, \
             patch('psutil.disk_io_counters') as mock_disk, \
             patch('psutil.net_io_counters') as mock_network:

            # Setup mocks
            mock_memory.return_value.percent = 68.2
            mock_memory.return_value.used = 8589934592  # 8GB
            mock_disk.return_value.read_bytes = 1048576000  # 1000MB
            mock_disk.return_value.write_bytes = 524288000   # 500MB
            mock_network.return_value.bytes_recv = 2097152000  # 2000MB
            mock_network.return_value.bytes_sent = 1048576000   # 1000MB

            # Collect metrics
            metrics = monitor._collect_current_metrics()

            # Verify metrics
            assert metrics.cpu_usage_percent == 75.5
            assert metrics.memory_usage_percent == 68.2
            assert metrics.memory_usage_gb == 8.0
            assert isinstance(metrics.timestamp, float)
            assert metrics.token_throughput > 0

    def test_alert_generation(self, monitor, sample_metrics):
        """Test alert generation based on thresholds."""
        # Set low thresholds to trigger alerts
        monitor.thresholds.max_cpu_usage = 40.0
        monitor.thresholds.max_memory_usage = 50.0
        monitor.thresholds.max_error_rate = 1.0

        # Analyze performance (should generate alerts)
        alerts = monitor._analyze_performance(sample_metrics)

        # Should generate alerts for CPU, memory, and error rate
        assert len(alerts) >= 2
        
        alert_metrics = {alert.metric_name for alert in alerts}
        assert "cpu_usage" in alert_metrics
        assert "memory_usage" in alert_metrics

        # Check alert properties
        cpu_alert = next(a for a in alerts if a.metric_name == "cpu_usage")
        assert cpu_alert.severity in ["WARNING", "CRITICAL"]
        assert cpu_alert.current_value == sample_metrics.cpu_usage_percent
        assert cpu_alert.threshold == monitor.thresholds.max_cpu_usage

    def test_alert_handler_registration(self, monitor):
        """Test custom alert handler registration."""
        alerts_received = []

        def test_handler(alert):
            alerts_received.append(alert)

        monitor.register_alert_handler(test_handler)
        assert len(monitor.alert_handlers) == 1

        # Trigger an alert
        test_alert = AlertMessage(
            timestamp=time.time(),
            severity="WARNING",
            metric_name="test_metric",
            current_value=100.0,
            threshold=50.0,
            message="Test alert",
            component="test"
        )

        monitor._process_alert(test_alert)

        # Verify handler was called
        assert len(alerts_received) == 1
        assert alerts_received[0].metric_name == "test_metric"

    def test_component_collector_registration(self, monitor):
        """Test custom component collector registration."""
        def test_collector():
            return {"test_metric": 42.0, "another_metric": "test_value"}

        monitor.register_component_collector("test_component", test_collector)
        assert "test_component" in monitor.component_collectors

        # Verify collector can be called
        result = monitor.component_collectors["test_component"]()
        assert result["test_metric"] == 42.0
        assert result["another_metric"] == "test_value"

    def test_performance_summary(self, monitor, sample_metrics):
        """Test performance summary generation."""
        # Add some test data
        monitor.metrics_history.append(sample_metrics)

        summary = monitor.get_performance_summary()

        assert "timestamp" in summary
        assert "status" in summary
        assert "metrics" in summary
        assert "alerts" in summary
        assert "performance_status" in summary

        # Check metrics data
        metrics = summary["metrics"]
        assert "cpu_usage" in metrics
        assert "memory_usage" in metrics
        assert "token_throughput" in metrics

    def test_metrics_history_retrieval(self, monitor):
        """Test metrics history retrieval."""
        # Add test metrics with different timestamps
        current_time = time.time()
        for i in range(5):
            metrics = BackendMetrics(
                timestamp=current_time - (i * 60),  # 1 minute intervals
                cpu_usage_percent=50.0 + i,
                memory_usage_percent=60.0,
                memory_usage_gb=8.0,
                disk_io_read_mb=10.0,
                disk_io_write_mb=5.0,
                network_io_recv_mb=15.0,
                network_io_sent_mb=8.0,
                token_throughput=100.0,
                request_latency_p50=50.0,
                request_latency_p95=100.0,
                request_latency_p99=200.0,
                active_requests=5,
                queued_requests=1,
                gpu_memory_usage_percent=70.0,
                kv_cache_usage_percent=60.0,
                requests_per_second=20.0,
                error_rate_percent=1.0,
                success_rate_percent=99.0,
            )
            monitor.metrics_history.append(metrics)

        # Get recent history (should return all 5)
        history = monitor.get_metrics_history(minutes=10)
        assert len(history) == 5

        # Get shorter history (should return fewer)
        history = monitor.get_metrics_history(minutes=2)
        assert len(history) <= 3  # Depending on timing

    def test_regression_detection(self, monitor):
        """Test basic regression detection."""
        # Set up baseline
        monitor.performance_baselines["test_metric"] = 100.0

        # Add metrics showing degradation
        for i in range(monitor.regression_window_size):
            metrics = BackendMetrics(
                timestamp=time.time(),
                cpu_usage_percent=50.0,
                memory_usage_percent=60.0,
                memory_usage_gb=8.0,
                disk_io_read_mb=10.0,
                disk_io_write_mb=5.0,
                network_io_recv_mb=15.0,
                network_io_sent_mb=8.0,
                token_throughput=90.0 - i,  # Decreasing throughput
                request_latency_p50=50.0,
                request_latency_p95=100.0,
                request_latency_p99=200.0,
                active_requests=5,
                queued_requests=1,
                gpu_memory_usage_percent=70.0,
                kv_cache_usage_percent=60.0,
                requests_per_second=20.0,
                error_rate_percent=1.0,
                success_rate_percent=99.0,
            )
            monitor.metrics_history.append(metrics)

        # Check for regression
        latest_metrics = monitor.metrics_history[-1]
        monitor._check_regression(latest_metrics)

        # Should have detected regression and logged it
        # (This is a basic test - more sophisticated regression detection 
        # is handled by the dedicated RegressionDetector class)

    def test_prometheus_metrics_integration(self, monitor):
        """Test Prometheus metrics integration."""
        # Check that Prometheus metrics are set up
        assert hasattr(monitor, 'prom_cpu_usage')
        assert hasattr(monitor, 'prom_memory_usage')
        assert hasattr(monitor, 'prom_requests_per_sec')

        # Test metrics update
        test_metrics = sample_metrics = BackendMetrics(
            timestamp=time.time(),
            cpu_usage_percent=75.5,
            memory_usage_percent=68.2,
            memory_usage_gb=8.2,
            disk_io_read_mb=12.5,
            disk_io_write_mb=8.3,
            network_io_recv_mb=15.2,
            network_io_sent_mb=9.8,
            token_throughput=125.5,
            request_latency_p50=45.2,
            request_latency_p95=128.7,
            request_latency_p99=256.3,
            active_requests=6,
            queued_requests=2,
            gpu_memory_usage_percent=72.1,
            kv_cache_usage_percent=58.9,
            requests_per_second=22.4,
            error_rate_percent=1.8,
            success_rate_percent=98.2,
        )

        # Should not raise exception
        monitor._update_prometheus_metrics(test_metrics)

    def test_export_functionality(self, monitor, sample_metrics):
        """Test metrics export functionality."""
        # Add test data
        monitor.metrics_history.append(sample_metrics)
        
        test_alert = AlertMessage(
            timestamp=time.time(),
            severity="WARNING", 
            metric_name="test_metric",
            current_value=100.0,
            threshold=50.0,
            message="Test alert",
            component="test"
        )
        monitor.alerts_history.append(test_alert)

        # Export data
        exported = monitor.export_metrics_to_dict()

        assert "metrics_history" in exported
        assert "alerts_history" in exported
        assert "summary" in exported

        # Verify structure
        assert len(exported["metrics_history"]) == 1
        assert len(exported["alerts_history"]) == 1

        metrics_data = exported["metrics_history"][0]
        assert "timestamp" in metrics_data
        assert "cpu_usage" in metrics_data
        assert "token_throughput" in metrics_data

    def test_factory_function(self):
        """Test the factory function."""
        monitor = create_backend_monitor(
            collection_interval=2.0,
            enable_deep_tree_echo=False
        )
        
        assert isinstance(monitor, BackendPerformanceMonitor)
        assert monitor.collection_interval == 2.0
        assert not monitor.enable_deep_tree_echo

    @pytest.mark.asyncio
    async def test_concurrent_access(self, monitor):
        """Test thread-safe concurrent access."""
        results = []
        
        def collect_metrics():
            for _ in range(10):
                metrics = monitor.get_current_metrics()
                results.append(metrics is not None)
                time.sleep(0.01)
        
        # Start monitoring
        monitor.start_monitoring()
        time.sleep(0.1)  # Let it collect some data
        
        # Run concurrent access
        threads = [threading.Thread(target=collect_metrics) for _ in range(3)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()
        
        monitor.stop_monitoring()
        
        # Should not have crashed and should have some results
        assert len(results) == 30  # 3 threads × 10 calls each

    def test_deep_tree_echo_integration(self):
        """Test Deep Tree Echo components integration."""
        monitor = BackendPerformanceMonitor(
            enable_deep_tree_echo=True,
            collection_interval=0.1
        )
        
        # Should have Echo-related Prometheus metrics
        assert hasattr(monitor, 'prom_aar_agents')
        assert hasattr(monitor, 'prom_dtesn_rate')
        assert hasattr(monitor, 'prom_evolution_score')
        
        # Test Echo metrics collection
        echo_metrics = monitor._get_echo_metrics()
        assert isinstance(echo_metrics, dict)
        assert 'aar_agents' in echo_metrics
        assert 'dtesn_rate' in echo_metrics
        assert 'evolution_score' in echo_metrics

    def test_memory_management(self, monitor):
        """Test that memory usage is controlled."""
        # Fill up metrics history beyond limit
        for i in range(monitor.metrics_history_size * 2):
            metrics = BackendMetrics(
                timestamp=time.time() + i,
                cpu_usage_percent=50.0,
                memory_usage_percent=60.0,
                memory_usage_gb=8.0,
                disk_io_read_mb=10.0,
                disk_io_write_mb=5.0,
                network_io_recv_mb=15.0,
                network_io_sent_mb=8.0,
                token_throughput=100.0,
                request_latency_p50=50.0,
                request_latency_p95=100.0,
                request_latency_p99=200.0,
                active_requests=5,
                queued_requests=1,
                gpu_memory_usage_percent=70.0,
                kv_cache_usage_percent=60.0,
                requests_per_second=20.0,
                error_rate_percent=1.0,
                success_rate_percent=99.0,
            )
            monitor.metrics_history.append(metrics)

        # Should not exceed max size
        assert len(monitor.metrics_history) <= monitor.metrics_history_size


if __name__ == "__main__":
    pytest.main([__file__])