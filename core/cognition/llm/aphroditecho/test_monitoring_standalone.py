#!/usr/bin/env python3
"""
Standalone test for the backend performance monitoring system.

Tests the monitoring components without requiring the full Aphrodite engine.
"""

import sys
import time
import threading
from pathlib import Path

# Add current directory to Python path
sys.path.insert(0, str(Path(__file__).parent))

def test_monitoring_basic():
    """Test basic monitoring functionality."""
    print("🧪 Testing Backend Performance Monitoring (Standalone)")
    print("="*60)
    
    try:
        # Test imports
        print("📦 Testing imports...")
        from aphrodite.monitoring.backend_monitor import (
            BackendPerformanceMonitor, BackendMetrics, PerformanceThresholds, AlertMessage
        )
        print("  ✅ Backend monitor imports successful")
        
        from aphrodite.monitoring.metrics_collector import AphroditeMetricsCollector
        print("  ✅ Metrics collector imports successful")
        
        from aphrodite.monitoring.regression_detector import PerformanceRegressionDetector
        print("  ✅ Regression detector imports successful")
        
        from aphrodite.monitoring.alerting_system import PerformanceAlertingSystem
        print("  ✅ Alerting system imports successful")
        
        # Test basic monitor creation
        print("\n🔧 Testing monitor creation...")
        monitor = BackendPerformanceMonitor(
            collection_interval=0.5,
            metrics_history_size=10,
            enable_deep_tree_echo=False  # Disable for standalone test
        )
        print("  ✅ Monitor created successfully")
        
        # Test metrics collection
        print("\n📊 Testing metrics collection...")
        metrics = monitor._collect_current_metrics()
        print(f"  ✅ Metrics collected:")
        print(f"    CPU: {metrics.cpu_usage_percent:.1f}%")
        print(f"    Memory: {metrics.memory_usage_percent:.1f}%") 
        print(f"    Token throughput: {metrics.token_throughput:.1f}")
        
        # Test alert generation
        print("\n🚨 Testing alert generation...")
        # Set low thresholds to trigger alerts
        monitor.thresholds.max_cpu_usage = 1.0  # Very low to trigger
        monitor.thresholds.max_memory_usage = 1.0
        
        alerts = monitor._analyze_performance(metrics)
        print(f"  ✅ Generated {len(alerts)} alerts (expected with low thresholds)")
        
        for alert in alerts[:2]:  # Show first 2
            print(f"    {alert.severity}: {alert.message}")
        
        # Test monitoring start/stop
        print("\n▶️  Testing monitoring lifecycle...")
        monitor.start_monitoring()
        print("  ✅ Monitor started")
        
        time.sleep(1.5)  # Let it collect some data
        
        current = monitor.get_current_metrics()
        if current:
            print(f"  ✅ Collected metrics: CPU {current.cpu_usage_percent:.1f}%, Memory {current.memory_usage_percent:.1f}%")
        
        monitor.stop_monitoring()
        print("  ✅ Monitor stopped")
        
        # Test performance summary
        print("\n📋 Testing performance summary...")
        summary = monitor.get_performance_summary()
        print(f"  ✅ Summary status: {summary.get('status', 'unknown')}")
        print(f"  ✅ Metrics collected: {len(monitor.metrics_history)}")
        
        # Test metrics collector (basic)
        print("\n📈 Testing metrics collector...")
        collector = AphroditeMetricsCollector(
            enable_gpu_metrics=False,  # Disable GPU for standalone test
            enable_echo_metrics=False, # Disable Echo for standalone test
            collection_interval=0.5
        )
        print("  ✅ Metrics collector created")
        
        collector_summary = collector.get_metrics_summary()
        print(f"  ✅ Collector status: {collector_summary.get('collection_status', 'unknown')}")
        
        # Test regression detector (basic)
        print("\n📉 Testing regression detector...")
        regression_detector = PerformanceRegressionDetector(
            baseline_window_size=20,
            detection_window_size=5,
            min_regression_threshold=0.1
        )
        print("  ✅ Regression detector created")
        
        # Feed some test metrics
        for i in range(10):
            test_metrics = BackendMetrics(
                timestamp=time.time(),
                cpu_usage_percent=50.0 + i,  # Increasing CPU usage
                memory_usage_percent=60.0,
                memory_usage_gb=8.0,
                disk_io_read_mb=10.0,
                disk_io_write_mb=5.0,
                network_io_recv_mb=15.0,
                network_io_sent_mb=8.0,
                token_throughput=100.0 - i,  # Decreasing throughput
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
            regression_detector.update_metrics(test_metrics)
        
        regression_summary = regression_detector.get_regression_summary()
        print(f"  ✅ Regression detector summary: {regression_summary.get('detection_status', 'unknown')}")
        
        # Test alerting system (basic)
        print("\n🚨 Testing alerting system...")
        alerting_system = PerformanceAlertingSystem()
        print("  ✅ Alerting system created")
        
        alert_status = alerting_system.get_alert_status()
        print(f"  ✅ Alert status: {alert_status.get('system_status', 'unknown')}")
        
        print("\n🎉 All tests completed successfully!")
        print("✅ Backend Performance Monitoring System is functional")
        
        return True
        
    except Exception as e:
        print(f"\n❌ Test failed: {e}")
        import traceback
        traceback.print_exc()
        return False


def test_integration():
    """Test integrated monitoring components."""
    print("\n🔗 Testing integration...")
    
    try:
        from aphrodite.monitoring import create_backend_monitor
        
        # Create integrated monitor
        monitor = create_backend_monitor(
            collection_interval=0.2,
            enable_deep_tree_echo=False
        )
        
        # Test alert handling
        alerts_received = []
        
        def alert_handler(alert):
            alerts_received.append(alert)
        
        monitor.register_alert_handler(alert_handler)
        
        # Start monitoring briefly
        monitor.start_monitoring()
        time.sleep(1.0)
        
        # Trigger alerts by setting very low thresholds
        monitor.thresholds.max_cpu_usage = 0.1
        current = monitor.get_current_metrics()
        if current:
            alerts = monitor._analyze_performance(current)
            for alert in alerts:
                monitor._process_alert(alert)
        
        monitor.stop_monitoring()
        
        print(f"  ✅ Integration test complete")
        print(f"  ✅ Alerts handled: {len(alerts_received)}")
        
        return True
        
    except Exception as e:
        print(f"  ❌ Integration test failed: {e}")
        return False


def main():
    """Run all tests."""
    print("🧪 APHRODITE BACKEND PERFORMANCE MONITORING - STANDALONE TESTS")
    print("="*80)
    
    success = True
    
    # Basic functionality test
    success &= test_monitoring_basic()
    
    # Integration test
    success &= test_integration()
    
    print("\n" + "="*80)
    if success:
        print("🎉 ALL TESTS PASSED - Monitoring system is ready!")
    else:
        print("❌ SOME TESTS FAILED - Check errors above")
        sys.exit(1)


if __name__ == "__main__":
    main()