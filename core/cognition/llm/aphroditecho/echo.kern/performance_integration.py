#!/usr/bin/env python3
"""
Deep Tree Echo Performance Integration
Integrates unified performance monitoring with existing echo.dash and Aphrodite systems
"""

import logging
import time
import json
from pathlib import Path
from typing import Dict, Any, Optional
from dataclasses import asdict

try:
    from .performance_monitor import UnifiedPerformanceMonitor, PerformanceAlert, AlertSeverity

# Import DTESN cache manager for integration
try:
    from aphrodite.endpoints.openai.dtesn_cache_manager import get_cache_manager, DTESNServerSideCacheManager
    DTESN_CACHE_AVAILABLE = True
except ImportError:
    DTESN_CACHE_AVAILABLE = False
    get_cache_manager = None
    DTESNServerSideCacheManager = None
except ImportError:
    from performance_monitor import UnifiedPerformanceMonitor, PerformanceAlert, AlertSeverity

# Setup logging
logger = logging.getLogger(__name__)


class EchoDashIntegration:
    """Integration with echo.dash monitoring system"""
    
    def __init__(self, monitor: UnifiedPerformanceMonitor, stats_dir: str = "/tmp/echo_stats"):
        self.monitor = monitor
        self.stats_dir = Path(stats_dir)
        self.stats_dir.mkdir(parents=True, exist_ok=True)
        
        # Register with performance monitor
        self.monitor.register_alert_handler(self._handle_echo_alert)
        
        logger.info(f"EchoDash integration initialized, stats_dir: {self.stats_dir}")
    
    def _get_dtesn_cache_metrics(self) -> Dict[str, Any]:
        """Get DTESN cache performance metrics for dashboard integration"""
        if not DTESN_CACHE_AVAILABLE:
            return {
                'available': False,
                'error': 'DTESN cache manager not available'
            }
        
        try:
            cache_manager = get_cache_manager()
            if not cache_manager:
                return {
                    'available': False,
                    'error': 'DTESN cache manager not initialized'
                }
            
            cache_metrics = cache_manager.get_performance_metrics()
            
            return {
                'available': True,
                'hit_ratio': cache_metrics['hit_ratio'],
                'performance_improvement_percent': cache_metrics['performance_improvement_percent'],
                'total_requests': cache_metrics['total_requests'],
                'cache_hits': cache_metrics['cache_hits'],
                'cache_misses': cache_metrics['cache_misses'],
                'memory_usage_mb': cache_metrics['memory_usage_bytes'] / (1024 * 1024),
                'memory_entries': cache_metrics['cache_levels']['memory_entries'],
                'compressed_entries': cache_metrics['cache_levels']['compressed_entries'],
                'redis_enabled': cache_metrics['cache_levels']['redis_enabled'],
                'cache_strategy': cache_metrics['cache_strategy'],
                'avg_processing_time_ms': cache_metrics['avg_processing_time_ms'],
                'avg_cache_retrieval_time_ms': cache_metrics['avg_cache_retrieval_time_ms']
            }
            
        except Exception as e:
            logger.error(f"Failed to get DTESN cache metrics: {e}")
            return {
                'available': False,
                'error': f'Failed to retrieve metrics: {str(e)}'
            }
    
    def _handle_echo_alert(self, alert: PerformanceAlert) -> None:
        """Handle performance alerts for echo.dash integration"""
        try:
            # Save alert to echo.dash compatible format
            alert_file = self.stats_dir / f"alert_{int(alert.timestamp)}.json"
            
            # Convert alert to JSON-serializable format
            alert_dict = asdict(alert)
            alert_dict['severity'] = alert.severity.value  # Convert enum to string
            
            with open(alert_file, 'w') as f:
                json.dump(alert_dict, f, indent=2)
            
            # Log for echo.dash monitor to pick up
            logger.warning(f"ECHO_DASH_ALERT: {alert.severity.value}:{alert.component}:{alert.metric}:{alert.current_value}")
            
        except Exception as e:
            logger.error(f"Error handling echo.dash alert: {e}")
    
    def export_metrics_for_dashboard(self) -> Dict[str, Any]:
        """Export metrics in format compatible with echo.dash dashboard"""
        current_metrics = self.monitor.get_current_metrics()
        recent_alerts = self.monitor.get_recent_alerts(1)
        
        if not current_metrics:
            return {}
        
        # Convert to echo.dash compatible format
        dashboard_data = {
            'timestamp': current_metrics.timestamp,
            'system': {
                'cpu_percent': current_metrics.cpu_usage,
                'memory_percent': current_metrics.memory_usage,
                'disk_percent': current_metrics.disk_usage
            },
            'process': {
                'performance': {
                    'token_throughput': current_metrics.token_throughput,
                    'request_latency': current_metrics.request_latency_ms,
                    'gpu_utilization': current_metrics.gpu_utilization
                }
            },
            'dtesn': {
                'membrane_evolution_rate': current_metrics.membrane_evolution_rate,
                'reservoir_dynamics': current_metrics.reservoir_dynamics,
                'membrane_level': current_metrics.membrane_level
            },
            'echo_self': {
                'evolution_convergence': current_metrics.evolution_convergence,
                'fitness_improvement': current_metrics.fitness_improvement,
                'agent_performance': current_metrics.agent_performance
            },
            'dtesn_cache': self._get_dtesn_cache_metrics(),
            'embodied': {
                'sensory_motor_latency': current_metrics.sensory_motor_latency,
                'proprioceptive_accuracy': current_metrics.proprioceptive_accuracy
            },
            'alerts': {
                'count': len(recent_alerts),
                'critical_count': len([a for a in recent_alerts if a.severity == AlertSeverity.CRITICAL]),
                'warning_count': len([a for a in recent_alerts if a.severity == AlertSeverity.WARNING])
            },
            'team_member': 'PERFORMANCE_MONITOR',
            'uptime': time.time() - current_metrics.timestamp if current_metrics else 0
        }
        
        return dashboard_data
    
    def save_dashboard_metrics(self) -> None:
        """Save metrics for echo.dash to consume"""
        try:
            dashboard_data = self.export_metrics_for_dashboard()
            
            if dashboard_data:
                metrics_file = self.stats_dir / "performance_metrics.json"
                with open(metrics_file, 'w') as f:
                    json.dump(dashboard_data, f, indent=2)
                
                logger.debug("Dashboard metrics saved")
        except Exception as e:
            logger.error(f"Error saving dashboard metrics: {e}")


class AphroditeMetricsCollector:
    """Enhanced collector for Aphrodite Engine metrics"""
    
    def __init__(self):
        self.last_collection_time = 0
        logger.info("Aphrodite metrics collector initialized")
    
    def collect_metrics(self) -> Dict[str, Any]:
        """Collect comprehensive Aphrodite Engine metrics"""
        try:
            metrics = {}
            current_time = time.time()
            
            # Try to collect from Aphrodite's metrics system if available
            metrics.update(self._collect_aphrodite_stats())
            metrics.update(self._collect_gpu_metrics())
            metrics.update(self._collect_request_metrics())
            
            metrics['collection_time'] = current_time
            self.last_collection_time = current_time
            
            return metrics
            
        except Exception as e:
            logger.error(f"Error collecting Aphrodite metrics: {e}")
            return {}
    
    def _collect_aphrodite_stats(self) -> Dict[str, Any]:
        """Collect Aphrodite engine statistics"""
        try:
            # TODO: Integrate with actual Aphrodite metrics API
            # This would connect to aphrodite.v1.metrics.stats
            
            # Simulate realistic metrics for now
            return {
                'token_throughput': 95.5 + (time.time() % 10),  # Variable throughput
                'request_latency_ms': 45.0 + (time.time() % 20),  # Variable latency
                'active_requests': 5,
                'completed_requests': 1000 + int(time.time() % 100),
                'kv_cache_usage': 65.0 + (time.time() % 30),
                'scheduler_efficiency': 0.89
            }
        except Exception as e:
            logger.error(f"Error collecting Aphrodite stats: {e}")
            return {}
    
    def _collect_gpu_metrics(self) -> Dict[str, Any]:
        """Collect GPU utilization metrics"""
        try:
            # TODO: Integrate with actual GPU monitoring (nvidia-ml-py or similar)
            # For now, return simulated GPU metrics
            
            return {
                'gpu_utilization': 78.5 + (time.time() % 15),  # Variable GPU usage
                'gpu_memory_usage': 82.0 + (time.time() % 10),
                'gpu_temperature': 65.0 + (time.time() % 5),
                'gpu_power_draw': 250.0 + (time.time() % 20)
            }
        except Exception as e:
            logger.error(f"Error collecting GPU metrics: {e}")
            return {}
    
    def _collect_request_metrics(self) -> Dict[str, Any]:
        """Collect request processing metrics"""
        try:
            # TODO: Integrate with actual Aphrodite request tracking
            
            return {
                'requests_per_second': 12.5 + (time.time() % 5),
                'average_tokens_per_request': 150.0 + (time.time() % 50),
                'queue_length': max(0, int(5 + (time.time() % 10) - 7)),
                'error_rate': 0.02 + (time.time() % 0.05)
            }
        except Exception as e:
            logger.error(f"Error collecting request metrics: {e}")
            return {}


class DTESNProfilerIntegration:
    """Integration with DTESN performance profiling framework"""
    
    def __init__(self):
        self.profiling_active = False
        logger.info("DTESN profiler integration initialized")
    
    def collect_dtesn_metrics(self) -> Dict[str, Any]:
        """Collect DTESN performance profiling metrics"""
        try:
            # TODO: Integrate with actual DTESN profiling framework
            # This would connect to the C-based profiling system described in
            # echo.kern/docs/tools/performance-profiling.md
            
            metrics = {}
            
            # Simulate membrane computing metrics
            membrane_level = int(3 + (time.time() % 5))  # Levels 3-7
            
            metrics.update({
                'membrane_evolution_rate': 22.0 + (time.time() % 8),  # evolutions/sec
                'reservoir_dynamics': 0.82 + (time.time() % 0.15),  # dynamics score
                'membrane_level': membrane_level,
                'p_system_transitions': 45 + int(time.time() % 20),  # transitions/sec
                'esn_update_time_ns': 1250000 + int(time.time() % 500000),  # nanoseconds
                'memory_allocation_efficiency': 0.88 + (time.time() % 0.10),
                'cache_hit_ratio': 0.91 + (time.time() % 0.08)
            })
            
            # OEIS A000081 compliance metrics (rooted tree enumeration)
            metrics['oeis_a000081_level'] = membrane_level
            metrics['tree_enumeration_accuracy'] = 0.995 + (time.time() % 0.004)
            
            return metrics
            
        except Exception as e:
            logger.error(f"Error collecting DTESN metrics: {e}")
            return {}


class EchoSelfIntegration:
    """Integration with Echo-Self evolution engine"""
    
    def __init__(self):
        self.evolution_active = False
        logger.info("Echo-Self integration initialized")
    
    def collect_echo_self_metrics(self) -> Dict[str, Any]:
        """Collect Echo-Self evolution metrics"""
        try:
            # TODO: Integrate with actual Echo-Self monitoring system
            # This would connect to echo.kern/phase_3_3_3_self_monitoring.py
            
            metrics = {
                'evolution_convergence': 0.75 + (time.time() % 0.20),  # convergence score
                'fitness_improvement': 0.08 + (time.time() % 0.15),  # improvement rate
                'agent_performance': 0.80 + (time.time() % 0.18),  # performance score
                'self_monitoring_active': True,
                'adaptation_cycles': int(50 + (time.time() % 30)),
                'mutation_rate': 0.15 + (time.time() % 0.10),
                'selection_pressure': 0.65 + (time.time() % 0.20),
                'population_diversity': 0.72 + (time.time() % 0.25)
            }
            
            return metrics
            
        except Exception as e:
            logger.error(f"Error collecting Echo-Self metrics: {e}")
            return {}


class IntegratedPerformanceSystem:
    """
    Complete integrated performance monitoring system
    Coordinates all components and provides unified interface
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        
        # Initialize core monitor
        self.monitor = UnifiedPerformanceMonitor()
        
        # Initialize component integrations
        self.echo_dash = EchoDashIntegration(self.monitor)
        self.aphrodite_collector = AphroditeMetricsCollector()
        self.dtesn_profiler = DTESNProfilerIntegration()
        self.echo_self = EchoSelfIntegration()
        
        # Register enhanced collectors
        self._register_enhanced_collectors()
        
        # Setup alerting
        self._setup_alert_handlers()
        
        logger.info("Integrated Performance System initialized")
    
    def _register_enhanced_collectors(self):
        """Register enhanced collectors with the monitor"""
        # Override default collectors with enhanced versions
        self.monitor.register_collector("aphrodite", self._collect_enhanced_aphrodite)
        self.monitor.register_collector("dtesn", self._collect_enhanced_dtesn)
        self.monitor.register_collector("echo_self", self._collect_enhanced_echo_self)
    
    def _collect_enhanced_aphrodite(self) -> Dict[str, Any]:
        """Enhanced Aphrodite metrics collection"""
        return self.aphrodite_collector.collect_metrics()
    
    def _collect_enhanced_dtesn(self) -> Dict[str, Any]:
        """Enhanced DTESN metrics collection"""
        return self.dtesn_profiler.collect_dtesn_metrics()
    
    def _collect_enhanced_echo_self(self) -> Dict[str, Any]:
        """Enhanced Echo-Self metrics collection"""
        return self.echo_self.collect_echo_self_metrics()
    
    def _setup_alert_handlers(self):
        """Setup alert handling system"""
        def comprehensive_alert_handler(alert: PerformanceAlert):
            """Comprehensive alert handling"""
            
            # Log structured alert
            logger.warning(
                f"Performance Alert - {alert.severity.value.upper()}: "
                f"{alert.component}.{alert.metric} = {alert.current_value:.2f} "
                f"(threshold: {alert.threshold:.2f}) - {alert.message}"
            )
            
            # Save alert for external systems
            self.echo_dash.save_dashboard_metrics()
            
            # TODO: Add integration with external alert systems
            # - Prometheus alerts
            # - Slack notifications
            # - Email alerts for critical issues
        
        self.monitor.register_alert_handler(comprehensive_alert_handler)
    
    def start(self) -> None:
        """Start the integrated performance monitoring system"""
        try:
            self.monitor.start_monitoring()
            logger.info("Integrated Performance System started")
        except Exception as e:
            logger.error(f"Error starting integrated performance system: {e}")
            raise
    
    def stop(self) -> None:
        """Stop the integrated performance monitoring system"""
        try:
            self.monitor.stop_monitoring()
            logger.info("Integrated Performance System stopped")
        except Exception as e:
            logger.error(f"Error stopping integrated performance system: {e}")
    
    def get_comprehensive_status(self) -> Dict[str, Any]:
        """Get comprehensive system status"""
        base_summary = self.monitor.get_performance_summary()
        dashboard_data = self.echo_dash.export_metrics_for_dashboard()
        
        return {
            'system_status': 'active' if self.monitor.is_monitoring else 'inactive',
            'performance_summary': base_summary,
            'dashboard_data': dashboard_data,
            'component_status': {
                'aphrodite_collector': 'active',
                'dtesn_profiler': 'active', 
                'echo_self': 'active',
                'echo_dash_integration': 'active'
            }
        }
    
    def export_performance_report(self, filepath: str) -> None:
        """Export comprehensive performance report"""
        status = self.get_comprehensive_status()
        
        with open(filepath, 'w') as f:
            json.dump(status, f, indent=2)
        
        logger.info(f"Performance report exported to {filepath}")


def create_integrated_system(config_path: Optional[str] = None) -> IntegratedPerformanceSystem:
    """Create and configure integrated performance system"""
    config = {}
    
    if config_path and Path(config_path).exists():
        with open(config_path, 'r') as f:
            config = json.load(f)
    
    return IntegratedPerformanceSystem(config)


if __name__ == "__main__":
    # Demo the integrated system
    system = create_integrated_system()
    
    try:
        print("Starting integrated performance monitoring system...")
        system.start()
        
        # Run for 60 seconds
        time.sleep(60)
        
        # Get status
        status = system.get_comprehensive_status()
        print(f"\nSystem Status: {status['system_status']}")
        print(f"Metrics collected: {status['performance_summary']['metrics_count']}")
        print(f"Alerts generated: {status['performance_summary']['alerts_count']}")
        
        # Export report
        system.export_performance_report("/tmp/performance_report.json")
        print("Performance report exported")
        
    finally:
        system.stop()
        print("System stopped")