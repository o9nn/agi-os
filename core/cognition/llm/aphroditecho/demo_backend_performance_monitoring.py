#!/usr/bin/env python3
"""
Comprehensive Demo: Backend Performance Monitoring System

This demo showcases the complete backend performance monitoring system
implemented for Phase 8 - SSR-Focused MLOps & Production Observability.

Features demonstrated:
- Real-time performance metrics collection
- Automated performance analysis and alerting
- Performance regression detection for model updates  
- Deep Tree Echo component monitoring
- Integration with existing Prometheus infrastructure
"""

import asyncio
import time
import json
from typing import Dict, Any

from loguru import logger

# Import the monitoring components
from aphrodite.monitoring import (
    BackendPerformanceMonitor,
    AphroditeMetricsCollector,
    PerformanceRegressionDetector,
    PerformanceAlertingSystem,
    create_backend_monitor,
    create_metrics_collector,
    create_regression_detector,
    create_alerting_system,
)


class BackendMonitoringDemo:
    """
    Comprehensive demonstration of the backend performance monitoring system.
    """
    
    def __init__(self):
        self.backend_monitor = None
        self.metrics_collector = None
        self.regression_detector = None
        self.alerting_system = None
        
        # Demo state
        self.demo_start_time = time.time()
        self.alerts_received = []
        self.regressions_detected = []
        
    def setup_monitoring_system(self):
        """Setup all monitoring components."""
        logger.info("🚀 Setting up Backend Performance Monitoring System")
        
        # 1. Create backend monitor
        logger.info("📊 Initializing Backend Performance Monitor...")
        self.backend_monitor = create_backend_monitor(
            collection_interval=1.0,
            metrics_history_size=100,
            enable_deep_tree_echo=True
        )
        
        # 2. Create metrics collector  
        logger.info("📈 Initializing Advanced Metrics Collector...")
        self.metrics_collector = create_metrics_collector(
            enable_gpu_metrics=True,
            enable_echo_metrics=True,
            collection_interval=1.0
        )
        
        # 3. Create regression detector
        logger.info("🔍 Initializing Performance Regression Detector...")
        self.regression_detector = create_regression_detector(
            baseline_window_size=50,
            detection_window_size=10,
            min_regression_threshold=0.10,  # 10% regression threshold
            confidence_threshold=0.7
        )
        
        # 4. Create alerting system
        logger.info("🚨 Initializing Performance Alerting System...")
        self.alerting_system = create_alerting_system(
            enable_alert_correlation=True
        )
        
        # Setup alert handlers
        self.backend_monitor.register_alert_handler(self._handle_performance_alert)
        self.regression_detector.register_alert_callback(self._handle_regression_alert)
        
        # Connect alerting system
        self.backend_monitor.register_alert_handler(self.alerting_system.process_performance_alert)
        self.regression_detector.register_alert_callback(self.alerting_system.process_regression_alert)
        
        logger.info("✅ Monitoring system setup complete!")
    
    def start_monitoring(self):
        """Start all monitoring components."""
        logger.info("🎯 Starting monitoring components...")
        
        # Start backend monitor
        self.backend_monitor.start_monitoring()
        logger.info("  ✓ Backend monitor started")
        
        # Start metrics collector
        self.metrics_collector.start_collection()
        logger.info("  ✓ Metrics collector started")
        
        # Start alerting system
        self.alerting_system.start()
        logger.info("  ✓ Alerting system started")
        
        logger.info("🚀 All monitoring components are now active!")
    
    def stop_monitoring(self):
        """Stop all monitoring components."""
        logger.info("🛑 Stopping monitoring components...")
        
        if self.backend_monitor:
            self.backend_monitor.stop_monitoring()
            logger.info("  ✓ Backend monitor stopped")
        
        if self.metrics_collector:
            self.metrics_collector.stop_collection()
            logger.info("  ✓ Metrics collector stopped")
        
        if self.alerting_system:
            self.alerting_system.stop()
            logger.info("  ✓ Alerting system stopped")
        
        logger.info("✅ All monitoring components stopped")
    
    def _handle_performance_alert(self, alert):
        """Handle performance alerts for demo tracking."""
        self.alerts_received.append(alert)
        logger.warning(f"🚨 Performance Alert: {alert.message}")
    
    def _handle_regression_alert(self, regression):
        """Handle regression alerts for demo tracking."""
        self.regressions_detected.append(regression)
        logger.error(f"📉 Regression Alert: {regression.message}")
    
    def demonstrate_basic_monitoring(self):
        """Demonstrate basic monitoring capabilities."""
        logger.info("🔄 Demonstrating Basic Monitoring...")
        
        print("\n" + "="*60)
        print("📊 BASIC MONITORING DEMONSTRATION")
        print("="*60)
        
        # Wait for some data collection
        logger.info("Collecting initial metrics...")
        time.sleep(3.0)
        
        # Get current metrics
        current_metrics = self.backend_monitor.get_current_metrics()
        if current_metrics:
            print(f"\n📈 Current System Metrics (timestamp: {current_metrics.timestamp:.1f}):")
            print(f"  CPU Usage: {current_metrics.cpu_usage_percent:.1f}%")
            print(f"  Memory Usage: {current_metrics.memory_usage_percent:.1f}%") 
            print(f"  Token Throughput: {current_metrics.token_throughput:.1f} tokens/s")
            print(f"  Request Latency (P95): {current_metrics.request_latency_p95:.1f}ms")
            print(f"  Active Requests: {current_metrics.active_requests}")
            print(f"  Error Rate: {current_metrics.error_rate_percent:.2f}%")
            
            if current_metrics.aar_agents_active > 0:
                print(f"\n🌳 Deep Tree Echo Metrics:")
                print(f"  AAR Agents Active: {current_metrics.aar_agents_active}")
                print(f"  DTESN Processing Rate: {current_metrics.dtesn_processing_rate:.1f}")
                print(f"  Echo-Self Evolution Score: {current_metrics.echo_self_evolution_score:.2f}")
        else:
            print("❌ No metrics available yet")
        
        # Get performance summary
        summary = self.backend_monitor.get_performance_summary()
        print(f"\n📋 Performance Status: {summary['status'].upper()}")
        print(f"  Monitoring Active: {summary['performance_status']['is_monitoring']}")
        print(f"  Metrics Collected: {summary['performance_status']['metrics_collected']}")
        print(f"  Collection Interval: {summary['performance_status']['collection_interval']}s")
    
    def demonstrate_metrics_collection(self):
        """Demonstrate advanced metrics collection."""
        logger.info("📊 Demonstrating Advanced Metrics Collection...")
        
        print("\n" + "="*60)
        print("📊 ADVANCED METRICS COLLECTION") 
        print("="*60)
        
        # Get metrics collector status
        collector_summary = self.metrics_collector.get_metrics_summary()
        print(f"\n🔧 Metrics Collector Status:")
        print(f"  Collection Active: {collector_summary['collection_status']}")
        print(f"  Data Sources:")
        print(f"    Engine Stats: {collector_summary['sources']['engine_stats']}")
        print(f"    GPU Metrics: {collector_summary['sources']['gpu_metrics']}")
        print(f"    Echo Components: {collector_summary['sources']['echo_components']}")
        print(f"  Custom Sources: {collector_summary['sources']['custom_sources']}")
        print(f"  Custom Collectors: {collector_summary['sources']['custom_collectors']}")
        
        # Get detailed metrics
        latest_metrics = self.metrics_collector.get_latest_metrics()
        if latest_metrics:
            print(f"\n📈 Latest Collected Metrics:")
            print(f"  Timestamp: {latest_metrics.timestamp:.1f}")
            
            if latest_metrics.engine_stats:
                print(f"  Engine:")
                print(f"    Running Requests: {latest_metrics.engine_stats.num_running_sys}")
                print(f"    GPU Cache Usage: {latest_metrics.engine_stats.gpu_cache_usage_sys:.1%}")
                print(f"    Total Tokens: {latest_metrics.engine_stats.num_tokens_iter}")
            
            if latest_metrics.gpu_utilization:
                print(f"  GPU:")
                print(f"    Utilization: {latest_metrics.gpu_utilization}")
                print(f"    Memory Usage: {latest_metrics.gpu_memory_usage}")
            
            if latest_metrics.aar_metrics:
                print(f"  AAR System:")
                for key, value in latest_metrics.aar_metrics.items():
                    print(f"    {key}: {value}")
        
        # Export collected data
        exported_data = self.metrics_collector.export_metrics_data()
        print(f"\n💾 Exported Data Summary:")
        print(f"  Export Timestamp: {exported_data.get('timestamp', 'N/A')}")
        print(f"  Engine Stats Available: {'engine_stats' in exported_data}")
        print(f"  GPU Metrics Available: {'gpu_metrics' in exported_data}")
        print(f"  Echo Metrics Available: {'echo_metrics' in exported_data}")
    
    def demonstrate_alerting_system(self):
        """Demonstrate alerting capabilities."""
        logger.info("🚨 Demonstrating Alerting System...")
        
        print("\n" + "="*60)
        print("🚨 ALERTING SYSTEM DEMONSTRATION")
        print("="*60)
        
        # Get alerting system status
        alert_status = self.alerting_system.get_alert_status()
        print(f"\n📊 Alerting System Status:")
        print(f"  System Status: {alert_status['system_status']}")
        print(f"  Active Alerts: {alert_status['active_alerts']}")
        print(f"  Total Notifications: {alert_status['total_notifications']}")
        print(f"  Resolved Alerts: {alert_status['resolved_alerts']}")
        
        recent_alerts = alert_status['recent_alerts_1h']
        print(f"\n📈 Recent Alerts (1 hour):")
        print(f"  Total: {recent_alerts['total']}")
        print(f"  By Severity: {recent_alerts['by_severity']}")
        print(f"  By Type: {recent_alerts['by_type']}")
        
        # Get active alerts
        active_alerts = self.alerting_system.get_active_alerts()
        if active_alerts:
            print(f"\n🚨 Active Alerts ({len(active_alerts)}):")
            for alert in active_alerts[:3]:  # Show first 3
                print(f"  Alert ID: {alert['alert_id']}")
                print(f"    Severity: {alert['severity']}")
                print(f"    Title: {alert['title']}")
                print(f"    Delivered: {alert['delivered_channels']}")
        else:
            print("\n✅ No active alerts")
        
        # Show demo alerts received
        if self.alerts_received:
            print(f"\n📨 Demo Alerts Received ({len(self.alerts_received)}):")
            for alert in self.alerts_received[-3:]:  # Show last 3
                print(f"  {alert.severity}: {alert.message}")
    
    def demonstrate_regression_detection(self):
        """Demonstrate regression detection capabilities.""" 
        logger.info("📉 Demonstrating Regression Detection...")
        
        print("\n" + "="*60)
        print("📉 REGRESSION DETECTION DEMONSTRATION")
        print("="*60)
        
        # Simulate a model update for regression correlation
        self.regression_detector.register_model_update(
            model_name="test-llama-model",
            version="v2.1.0",
            update_type="standard", 
            metadata={"demo": "simulation"}
        )
        print("📝 Simulated model update: test-llama-model v2.1.0")
        
        # Feed metrics to regression detector
        logger.info("Feeding metrics to regression detector...")
        for i in range(15):
            current_metrics = self.backend_monitor.get_current_metrics()
            if current_metrics:
                self.regression_detector.update_metrics(current_metrics)
            time.sleep(0.2)
        
        # Get regression summary
        regression_summary = self.regression_detector.get_regression_summary()
        print(f"\n🔍 Regression Detection Status:")
        print(f"  Detection Active: {regression_summary['detection_status']}")
        print(f"  Monitored Metrics: {regression_summary['baseline_status']['total_metrics']}")
        print(f"  Stable Baselines: {regression_summary['baseline_status']['stable_baselines']}")
        
        recent_regressions = regression_summary['recent_regressions']
        print(f"\n📊 Recent Regressions (1 hour):")
        print(f"  Total: {recent_regressions['total_last_hour']}")
        print(f"  By Severity: {recent_regressions['by_severity']}")
        
        # Get baseline status
        baseline_status = self.regression_detector.get_baseline_status()
        print(f"\n📈 Performance Baselines:")
        for metric_name, status in list(baseline_status.items())[:5]:  # Show first 5
            print(f"  {metric_name}:")
            print(f"    Baseline: {status['baseline_value']:.2f}")
            print(f"    Data Points: {status['data_points']}")
            print(f"    Stable: {status['is_stable']}")
        
        # Show demo regressions detected
        if self.regressions_detected:
            print(f"\n📉 Demo Regressions Detected ({len(self.regressions_detected)}):")
            for regression in self.regressions_detected[-2:]:  # Show last 2
                print(f"  {regression.severity.value}: {regression.message}")
    
    def demonstrate_api_integration(self):
        """Demonstrate API endpoint integration."""
        logger.info("🌐 Demonstrating API Integration...")
        
        print("\n" + "="*60)
        print("🌐 API INTEGRATION DEMONSTRATION")
        print("="*60)
        
        # Simulate API endpoint functionality
        print("\n📡 Available API Endpoints:")
        endpoints = [
            "GET /monitoring/health - Health check",
            "GET /monitoring/metrics/current - Current metrics",
            "GET /monitoring/metrics/history - Historical metrics",
            "GET /monitoring/summary - Performance summary",
            "GET /monitoring/alerts/recent - Recent alerts",
            "GET /monitoring/alerts/active - Active alerts",
            "GET /monitoring/regression/recent - Recent regressions",
            "GET /monitoring/regression/baselines - Performance baselines",
            "GET /monitoring/dashboard/data - Dashboard data",
            "GET /monitoring/prometheus/metrics - Prometheus metrics",
        ]
        
        for endpoint in endpoints:
            print(f"  • {endpoint}")
        
        # Simulate dashboard data
        print(f"\n📊 Dashboard Data Example:")
        dashboard_data = self._generate_dashboard_data()
        print(json.dumps(dashboard_data, indent=2))
    
    def _generate_dashboard_data(self) -> Dict[str, Any]:
        """Generate sample dashboard data."""
        current_metrics = self.backend_monitor.get_current_metrics()
        
        if not current_metrics:
            return {"error": "No metrics available"}
        
        return {
            "timestamp": current_metrics.timestamp,
            "current_metrics": {
                "cpu_usage": current_metrics.cpu_usage_percent,
                "memory_usage": current_metrics.memory_usage_percent,
                "token_throughput": current_metrics.token_throughput,
                "request_latency": current_metrics.request_latency_p95,
                "error_rate": current_metrics.error_rate_percent,
                "active_requests": current_metrics.active_requests,
            },
            "echo_metrics": {
                "aar_agents": current_metrics.aar_agents_active,
                "dtesn_rate": current_metrics.dtesn_processing_rate,
                "evolution_score": current_metrics.echo_self_evolution_score,
            },
            "alerts_summary": {
                "active_count": len(self.alerting_system.get_active_alerts()),
                "recent_1h": len(self.alerts_received),
                "regressions_1h": len(self.regressions_detected),
            },
            "system_health": {
                "monitoring_active": self.backend_monitor.is_monitoring,
                "uptime_seconds": time.time() - self.demo_start_time,
                "data_points": len(self.backend_monitor.metrics_history),
            }
        }
    
    def run_comprehensive_demo(self):
        """Run the complete monitoring system demonstration."""
        try:
            print("\n" + "="*80)
            print("🚀 APHRODITE ENGINE BACKEND PERFORMANCE MONITORING DEMO")
            print("Phase 8 - SSR-Focused MLOps & Production Observability")
            print("="*80)
            
            # Setup
            self.setup_monitoring_system()
            
            # Start monitoring
            self.start_monitoring()
            
            # Wait for initial data collection
            logger.info("⏱️  Collecting initial performance data...")
            time.sleep(5.0)
            
            # Run demonstrations
            self.demonstrate_basic_monitoring()
            time.sleep(2.0)
            
            self.demonstrate_metrics_collection()
            time.sleep(2.0)
            
            self.demonstrate_alerting_system()
            time.sleep(2.0)
            
            self.demonstrate_regression_detection()
            time.sleep(2.0)
            
            self.demonstrate_api_integration()
            
            # Final status
            print("\n" + "="*60)
            print("📊 FINAL MONITORING STATUS")
            print("="*60)
            
            uptime = time.time() - self.demo_start_time
            print(f"\n✅ Demo completed successfully!")
            print(f"  Demo Duration: {uptime:.1f} seconds")
            print(f"  Metrics Collected: {len(self.backend_monitor.metrics_history)}")
            print(f"  Alerts Generated: {len(self.alerts_received)}")
            print(f"  Regressions Detected: {len(self.regressions_detected)}")
            
            # Performance summary
            summary = self.backend_monitor.get_performance_summary()
            print(f"  System Status: {summary['status'].upper()}")
            print(f"  Monitoring Components: All Active ✅")
            
        except KeyboardInterrupt:
            logger.info("Demo interrupted by user")
        except Exception as e:
            logger.error(f"Demo failed: {e}")
            raise
        finally:
            # Cleanup
            self.stop_monitoring()
            print(f"\n🏁 Demo completed. All monitoring components stopped.")


def main():
    """Main demo execution."""
    # Configure logging
    logger.remove()
    logger.add(
        lambda msg: print(msg, end=""),
        format="<green>{time:HH:mm:ss}</green> | <level>{level: <8}</level> | {message}",
        level="INFO"
    )
    
    # Run demo
    demo = BackendMonitoringDemo()
    demo.run_comprehensive_demo()


if __name__ == "__main__":
    main()