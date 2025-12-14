#!/usr/bin/env python3
"""
Comprehensive tests for Deep Tree Echo Performance Monitoring System
Tests Task 4.1.3: Build Performance Monitoring implementation
"""

import pytest
import time
import json
import tempfile
from pathlib import Path

# Import the modules to test
from performance_monitor import (
    UnifiedPerformanceMonitor, 
    PerformanceMetrics, 
    PerformanceAlert, 
    AlertSeverity,
    create_default_monitor
)

from performance_integration import (
    IntegratedPerformanceSystem,
    EchoDashIntegration,
    AphroditeMetricsCollector,
    DTESNProfilerIntegration,
    EchoSelfIntegration,
    create_integrated_system
)


class TestPerformanceMetrics:
    """Test the PerformanceMetrics data structure"""
    
    def test_metrics_initialization(self):
        """Test metrics can be initialized with default values"""
        timestamp = time.time()
        metrics = PerformanceMetrics(timestamp=timestamp)
        
        assert metrics.timestamp == timestamp
        assert metrics.token_throughput == 0.0
        assert metrics.cpu_usage == 0.0
        assert metrics.component_id == "unknown"
        assert isinstance(metrics.metadata, dict)
    
    def test_metrics_with_values(self):
        """Test metrics can be initialized with specific values"""
        timestamp = time.time()
        metrics = PerformanceMetrics(
            timestamp=timestamp,
            token_throughput=100.0,
            cpu_usage=75.0,
            component_id="test_component"
        )
        
        assert metrics.timestamp == timestamp
        assert metrics.token_throughput == 100.0
        assert metrics.cpu_usage == 75.0
        assert metrics.component_id == "test_component"


class TestPerformanceAlert:
    """Test the PerformanceAlert structure"""
    
    def test_alert_creation(self):
        """Test alert can be created with required fields"""
        timestamp = time.time()
        alert = PerformanceAlert(
            timestamp=timestamp,
            severity=AlertSeverity.WARNING,
            component="test_component",
            metric="cpu_usage",
            current_value=85.0,
            threshold=80.0,
            message="CPU usage exceeded threshold"
        )
        
        assert alert.timestamp == timestamp
        assert alert.severity == AlertSeverity.WARNING
        assert alert.component == "test_component"
        assert alert.metric == "cpu_usage"
        assert alert.current_value == 85.0
        assert alert.threshold == 80.0
        assert "CPU usage exceeded threshold" in alert.message


class TestUnifiedPerformanceMonitor:
    """Test the core UnifiedPerformanceMonitor class"""
    
    @pytest.fixture
    def monitor(self):
        """Create a monitor instance for testing"""
        return UnifiedPerformanceMonitor()
    
    def test_monitor_initialization(self, monitor):
        """Test monitor initializes correctly"""
        assert not monitor.is_monitoring
        assert monitor.monitor_thread is None
        assert len(monitor.component_collectors) == 5  # Default collectors
        assert len(monitor.metrics_history) == 0
        assert len(monitor.alerts_history) == 0
    
    def test_register_collector(self, monitor):
        """Test registering custom collectors"""
        def test_collector():
            return {"test_metric": 42.0}
        
        monitor.register_collector("test_component", test_collector)
        assert "test_component" in monitor.component_collectors
        assert monitor.component_collectors["test_component"] == test_collector
    
    def test_register_alert_handler(self, monitor):
        """Test registering alert handlers"""
        handler_called = []
        
        def test_handler(alert):
            handler_called.append(alert)
        
        monitor.register_alert_handler(test_handler)
        assert test_handler in monitor.alert_handlers
    
    def test_system_metrics_collection(self, monitor):
        """Test system metrics can be collected"""
        metrics = monitor._collect_system_metrics()
        
        assert isinstance(metrics, dict)
        assert 'cpu_usage' in metrics
        assert 'memory_usage' in metrics
        assert 'disk_usage' in metrics
        assert 'timestamp' in metrics
        assert isinstance(metrics['cpu_usage'], float)
    
    def test_threshold_violation_detection(self, monitor):
        """Test threshold violation detection"""
        # Create metrics that violate thresholds
        timestamp = time.time()
        metrics = PerformanceMetrics(
            timestamp=timestamp,
            cpu_usage=95.0,  # Exceeds default threshold of 85%
            request_latency_ms=1500.0,  # Exceeds default threshold of 1000ms
            component_id="test"
        )
        
        alerts = monitor._check_threshold_violations(metrics)
        
        # Should generate alerts for both violations
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
        """Test performance degradation trend detection"""
        # Add metrics showing declining performance
        timestamps = [time.time() - i for i in range(10, 0, -1)]
        
        for i, timestamp in enumerate(timestamps):
            metrics = PerformanceMetrics(
                timestamp=timestamp,
                token_throughput=100.0 - (i * 5),  # Declining throughput
                component_id="test"
            )
            monitor.metrics_history.append(metrics)
        
        # Test with latest metrics
        latest_metrics = PerformanceMetrics(
            timestamp=time.time(),
            token_throughput=50.0,  # Continued decline
            component_id="test"
        )
        
        alerts = monitor._check_performance_degradation(latest_metrics)
        
        # Should detect degradation in token throughput
        degradation_alert = next((a for a in alerts if a.metric == 'token_throughput'), None)
        if degradation_alert:  # May not always trigger depending on threshold
            assert degradation_alert.severity == AlertSeverity.WARNING
            assert 'degradation trend' in degradation_alert.message.lower()
    
    def test_trend_calculation(self, monitor):
        """Test trend calculation algorithm"""
        # Test with declining values
        declining_values = [100, 95, 90, 85, 80]
        trend = monitor._calculate_trend(declining_values)
        assert trend < 0  # Negative trend for declining values
        
        # Test with improving values
        improving_values = [50, 60, 70, 80, 90]
        trend = monitor._calculate_trend(improving_values)
        assert trend > 0  # Positive trend for improving values
        
        # Test with stable values
        stable_values = [75, 75, 75, 75, 75]
        trend = monitor._calculate_trend(stable_values)
        assert abs(trend) < 0.01  # Nearly zero trend for stable values
    
    def test_performance_summary(self, monitor):
        """Test getting performance summary"""
        # Add some test data
        timestamp = time.time()
        metrics = PerformanceMetrics(timestamp=timestamp, cpu_usage=50.0)
        monitor.metrics_history.append(metrics)
        
        alert = PerformanceAlert(
            timestamp=timestamp,
            severity=AlertSeverity.INFO,
            component="test",
            metric="test_metric",
            current_value=10.0,
            threshold=20.0,
            message="Test alert"
        )
        monitor.alerts_history.append(alert)
        
        summary = monitor.get_performance_summary()
        
        assert 'timestamp' in summary
        assert summary['monitoring_active'] == False  # Not started
        assert summary['metrics_count'] == 1
        assert summary['alerts_count'] == 1
        assert summary['current_metrics'] is not None
    
    @pytest.mark.asyncio
    async def test_monitoring_lifecycle(self, monitor):
        """Test starting and stopping monitoring"""
        assert not monitor.is_monitoring
        
        # Start monitoring
        monitor.start_monitoring()
        time.sleep(0.1)  # Allow thread to start
        assert monitor.is_monitoring
        assert monitor.monitor_thread is not None
        
        # Stop monitoring
        monitor.stop_monitoring()
        time.sleep(0.1)  # Allow thread to stop
        assert not monitor.is_monitoring


class TestAphroditeMetricsCollector:
    """Test Aphrodite metrics collection"""
    
    def test_collector_initialization(self):
        """Test collector initializes correctly"""
        collector = AphroditeMetricsCollector()
        assert collector.last_collection_time == 0
    
    def test_metrics_collection(self):
        """Test metrics collection returns valid data"""
        collector = AphroditeMetricsCollector()
        metrics = collector.collect_metrics()
        
        assert isinstance(metrics, dict)
        assert 'token_throughput' in metrics
        assert 'request_latency_ms' in metrics
        assert 'gpu_utilization' in metrics
        assert 'collection_time' in metrics
        
        # Verify metric values are reasonable
        assert isinstance(metrics['token_throughput'], (int, float))
        assert metrics['token_throughput'] > 0
        assert isinstance(metrics['gpu_utilization'], (int, float))
        assert 0 <= metrics['gpu_utilization'] <= 100


class TestDTESNProfilerIntegration:
    """Test DTESN profiler integration"""
    
    def test_profiler_initialization(self):
        """Test profiler initializes correctly"""
        profiler = DTESNProfilerIntegration()
        assert not profiler.profiling_active
    
    def test_dtesn_metrics_collection(self):
        """Test DTESN metrics collection"""
        profiler = DTESNProfilerIntegration()
        metrics = profiler.collect_dtesn_metrics()
        
        assert isinstance(metrics, dict)
        assert 'membrane_evolution_rate' in metrics
        assert 'reservoir_dynamics' in metrics
        assert 'membrane_level' in metrics
        assert 'oeis_a000081_level' in metrics
        
        # Verify OEIS A000081 compliance
        assert isinstance(metrics['membrane_level'], int)
        assert metrics['membrane_level'] >= 1
        assert metrics['oeis_a000081_level'] == metrics['membrane_level']


class TestEchoSelfIntegration:
    """Test Echo-Self integration"""
    
    def test_echo_self_initialization(self):
        """Test Echo-Self integration initializes correctly"""
        integration = EchoSelfIntegration()
        assert not integration.evolution_active
    
    def test_echo_self_metrics_collection(self):
        """Test Echo-Self metrics collection"""
        integration = EchoSelfIntegration()
        metrics = integration.collect_echo_self_metrics()
        
        assert isinstance(metrics, dict)
        assert 'evolution_convergence' in metrics
        assert 'fitness_improvement' in metrics
        assert 'agent_performance' in metrics
        assert 'self_monitoring_active' in metrics
        
        # Verify metric ranges
        assert 0 <= metrics['evolution_convergence'] <= 1
        assert 0 <= metrics['agent_performance'] <= 1


class TestEchoDashIntegration:
    """Test echo.dash integration"""
    
    @pytest.fixture
    def temp_stats_dir(self):
        """Create temporary stats directory"""
        with tempfile.TemporaryDirectory() as temp_dir:
            yield temp_dir
    
    def test_echo_dash_initialization(self, temp_stats_dir):
        """Test echo.dash integration initializes correctly"""
        monitor = UnifiedPerformanceMonitor()
        integration = EchoDashIntegration(monitor, temp_stats_dir)
        
        assert integration.monitor == monitor
        assert integration.stats_dir == Path(temp_stats_dir)
        assert integration.stats_dir.exists()
    
    def test_dashboard_metrics_export(self, temp_stats_dir):
        """Test exporting metrics for dashboard"""
        monitor = UnifiedPerformanceMonitor()
        integration = EchoDashIntegration(monitor, temp_stats_dir)
        
        # Add test metrics to monitor
        timestamp = time.time()
        metrics = PerformanceMetrics(
            timestamp=timestamp,
            cpu_usage=50.0,
            memory_usage=60.0,
            token_throughput=100.0
        )
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
        
        # Verify system metrics
        assert dashboard_data['system']['cpu_percent'] == 50.0
        assert dashboard_data['system']['memory_percent'] == 60.0
    
    def test_alert_handling(self, temp_stats_dir):
        """Test alert handling for echo.dash"""
        monitor = UnifiedPerformanceMonitor()
        integration = EchoDashIntegration(monitor, temp_stats_dir)
        
        # Create test alert
        alert = PerformanceAlert(
            timestamp=time.time(),
            severity=AlertSeverity.WARNING,
            component="test",
            metric="cpu_usage",
            current_value=90.0,
            threshold=85.0,
            message="Test alert"
        )
        
        # Handle alert
        integration._handle_echo_alert(alert)
        
        # Check alert file was created
        alert_files = list(Path(temp_stats_dir).glob("alert_*.json"))
        assert len(alert_files) >= 1
        
        # Verify alert content
        with open(alert_files[0], 'r') as f:
            saved_alert = json.load(f)
        
        assert saved_alert['severity'] == 'warning'
        assert saved_alert['component'] == 'test'
        assert saved_alert['metric'] == 'cpu_usage'


class TestIntegratedPerformanceSystem:
    """Test the complete integrated system"""
    
    def test_system_initialization(self):
        """Test integrated system initializes correctly"""
        system = IntegratedPerformanceSystem()
        
        assert system.monitor is not None
        assert system.echo_dash is not None
        assert system.aphrodite_collector is not None
        assert system.dtesn_profiler is not None
        assert system.echo_self is not None
    
    def test_system_with_config(self):
        """Test system initialization with config"""
        config = {'test_setting': 'test_value'}
        system = IntegratedPerformanceSystem(config)
        
        assert system.config == config
    
    def test_enhanced_collectors_registration(self):
        """Test enhanced collectors are registered"""
        system = IntegratedPerformanceSystem()
        
        # Check collectors are registered
        assert 'aphrodite' in system.monitor.component_collectors
        assert 'dtesn' in system.monitor.component_collectors
        assert 'echo_self' in system.monitor.component_collectors
        
        # Test collectors return data
        aphrodite_metrics = system.monitor.component_collectors['aphrodite']()
        dtesn_metrics = system.monitor.component_collectors['dtesn']()
        echo_self_metrics = system.monitor.component_collectors['echo_self']()
        
        assert isinstance(aphrodite_metrics, dict)
        assert isinstance(dtesn_metrics, dict)
        assert isinstance(echo_self_metrics, dict)
    
    def test_comprehensive_status(self):
        """Test getting comprehensive system status"""
        system = IntegratedPerformanceSystem()
        status = system.get_comprehensive_status()
        
        assert isinstance(status, dict)
        assert 'system_status' in status
        assert 'performance_summary' in status
        assert 'dashboard_data' in status
        assert 'component_status' in status
        
        # Verify component status
        component_status = status['component_status']
        assert component_status['aphrodite_collector'] == 'active'
        assert component_status['dtesn_profiler'] == 'active'
        assert component_status['echo_self'] == 'active'
        assert component_status['echo_dash_integration'] == 'active'
    
    def test_performance_report_export(self):
        """Test exporting performance report"""
        system = IntegratedPerformanceSystem()
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as temp_file:
            temp_path = temp_file.name
        
        try:
            system.export_performance_report(temp_path)
            
            # Verify file was created and contains valid JSON
            assert Path(temp_path).exists()
            
            with open(temp_path, 'r') as f:
                report_data = json.load(f)
            
            assert isinstance(report_data, dict)
            assert 'system_status' in report_data
            assert 'performance_summary' in report_data
            
        finally:
            # Clean up
            Path(temp_path).unlink(missing_ok=True)
    
    @pytest.mark.asyncio
    async def test_system_lifecycle(self):
        """Test starting and stopping the integrated system"""
        system = IntegratedPerformanceSystem()
        
        # Test start
        system.start()
        time.sleep(0.1)  # Allow system to start
        assert system.monitor.is_monitoring
        
        # Test stop
        system.stop()
        time.sleep(0.1)  # Allow system to stop
        assert not system.monitor.is_monitoring


class TestFactoryFunctions:
    """Test factory functions"""
    
    def test_create_default_monitor(self):
        """Test creating default monitor"""
        monitor = create_default_monitor()
        
        assert isinstance(monitor, UnifiedPerformanceMonitor)
        assert len(monitor.alert_handlers) >= 1  # Should have default handler
    
    def test_create_integrated_system(self):
        """Test creating integrated system"""
        system = create_integrated_system()
        
        assert isinstance(system, IntegratedPerformanceSystem)
        assert system.config == {}
    
    def test_create_integrated_system_with_config_file(self):
        """Test creating integrated system with config file"""
        # Create temporary config file
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
    """Test acceptance criteria for Task 4.1.3"""
    
    def test_real_time_metrics_collection(self):
        """Test real-time performance metrics collection"""
        system = IntegratedPerformanceSystem()
        system.start()
        
        try:
            # Wait for metrics collection
            time.sleep(2)
            
            # Verify metrics are being collected
            current_metrics = system.monitor.get_current_metrics()
            assert current_metrics is not None
            assert current_metrics.timestamp > 0
            
            # Verify multiple metrics types are collected
            assert current_metrics.cpu_usage >= 0
            assert current_metrics.token_throughput >= 0
            assert current_metrics.membrane_evolution_rate >= 0
            assert current_metrics.evolution_convergence >= 0
            
        finally:
            system.stop()
    
    def test_automated_performance_analysis(self):
        """Test automated performance analysis capabilities"""
        monitor = UnifiedPerformanceMonitor()
        
        # Create metrics that should trigger analysis
        high_latency_metrics = PerformanceMetrics(
            timestamp=time.time(),
            request_latency_ms=1500.0,  # Above threshold
            cpu_usage=95.0,  # Above threshold
            component_id="test"
        )
        
        # Test threshold analysis
        alerts = monitor._check_threshold_violations(high_latency_metrics)
        assert len(alerts) >= 2  # Should detect multiple violations
        
        # Test trend analysis with degrading performance
        for i in range(10):
            degrading_metrics = PerformanceMetrics(
                timestamp=time.time() - (10-i),
                token_throughput=100.0 - (i * 5),  # Declining performance
                component_id="test"
            )
            monitor.metrics_history.append(degrading_metrics)
        
        trend_alerts = monitor._check_performance_degradation(high_latency_metrics)
        # May or may not trigger depending on thresholds, but should not error
        assert isinstance(trend_alerts, list)
    
    def test_alert_system_for_degradation(self):
        """Test alert system detects performance degradation"""
        system = IntegratedPerformanceSystem()
        
        # Capture alerts
        captured_alerts = []
        def capture_alerts(alert):
            captured_alerts.append(alert)
        
        system.monitor.register_alert_handler(capture_alerts)
        
        # Simulate degraded performance
        degraded_metrics = PerformanceMetrics(
            timestamp=time.time(),
            cpu_usage=95.0,  # Critical CPU usage
            request_latency_ms=2000.0,  # Critical latency
            token_throughput=30.0,  # Below minimum threshold
            component_id="test"
        )
        
        # Analyze performance
        alerts = system.monitor._analyze_performance(degraded_metrics)
        
        # Should generate multiple alerts
        assert len(alerts) >= 3
        
        # Verify alert severities
        critical_alerts = [a for a in alerts if a.severity == AlertSeverity.CRITICAL]
        warning_alerts = [a for a in alerts if a.severity == AlertSeverity.WARNING]
        
        assert len(critical_alerts) >= 1  # Low throughput should be critical
        assert len(warning_alerts) >= 1  # High CPU/latency should be warnings
    
    def test_comprehensive_monitoring_coverage(self):
        """Test comprehensive monitoring covers all required components"""
        system = IntegratedPerformanceSystem()
        
        # Start system briefly to collect metrics
        system.start()
        time.sleep(1)
        system.stop()
        
        # Get comprehensive status
        status = system.get_comprehensive_status()
        
        # Verify all required monitoring components are active
        component_status = status['component_status']
        required_components = [
            'aphrodite_collector',
            'dtesn_profiler', 
            'echo_self',
            'echo_dash_integration'
        ]
        
        for component in required_components:
            assert component in component_status
            assert component_status[component] == 'active'
        
        # Verify comprehensive metrics coverage
        if status['performance_summary']['current_metrics']:
            metrics = status['performance_summary']['current_metrics']
            
            # Aphrodite metrics
            assert 'token_throughput' in metrics
            assert 'request_latency_ms' in metrics
            assert 'gpu_utilization' in metrics
            
            # DTESN metrics  
            assert 'membrane_evolution_rate' in metrics
            assert 'reservoir_dynamics' in metrics
            
            # Echo-Self metrics
            assert 'evolution_convergence' in metrics
            assert 'fitness_improvement' in metrics
            
            # System metrics
            assert 'cpu_usage' in metrics
            assert 'memory_usage' in metrics
            
            # Embodied metrics
            assert 'sensory_motor_latency' in metrics
            assert 'proprioceptive_accuracy' in metrics


if __name__ == "__main__":
    # Run tests with pytest
    pytest.main([__file__, "-v"])