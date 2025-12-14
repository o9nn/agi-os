"""
Performance monitoring integration for DTESN data processing pipelines.

Integrates the enhanced data processing pipeline with the echo.kern performance 
monitoring system to provide comprehensive observability and alerting.
"""

import asyncio
import logging
import os
import sys
import time
from dataclasses import asdict
from typing import Any, Callable, Dict, List, Optional

# Add echo.kern to path for performance monitoring imports
echo_kern_path = os.path.join(
    os.path.dirname(__file__), "..", "..", "..", "..", "echo.kern"
)
if echo_kern_path not in sys.path:
    sys.path.insert(0, echo_kern_path)

try:
    from performance_integration import (IntegratedPerformanceSystem,
                                         create_integrated_system)
    from performance_monitor import (AlertSeverity, PerformanceMetrics,
                                     UnifiedPerformanceMonitor)
    PERFORMANCE_MONITORING_AVAILABLE = True
except ImportError as e:
    logging.warning(f"Performance monitoring not available: {e}")
    PERFORMANCE_MONITORING_AVAILABLE = False
    
    # Mock classes for fallback
    class UnifiedPerformanceMonitor:
        def __init__(self, *args, **kwargs): pass
        def start_monitoring(self): pass
        def stop_monitoring(self): pass
        def get_current_metrics(self): return {}
        def register_collector(self, name, func): pass
    
    class PerformanceMetrics:
        def __init__(self, **kwargs):
            for k, v in kwargs.items():
                setattr(self, k, v)
    
    class AlertSeverity:
        INFO = "info"
        WARNING = "warning"
        CRITICAL = "critical"

from aphrodite.endpoints.deep_tree_echo.data_pipeline import (
    DataProcessingPipeline)

logger = logging.getLogger(__name__)


class DTESNPerformanceCollector:
    """
    Performance metrics collector for DTESN data processing pipelines.
    
    Integrates with echo.kern performance monitoring to provide comprehensive
    metrics collection and analysis for data processing operations.
    """
    
    def __init__(self, pipeline: DataProcessingPipeline):
        """Initialize DTESN performance collector."""
        self.pipeline = pipeline
        self.collection_history: List[Dict[str, Any]] = []
        self.alert_thresholds = {
            "processing_rate_min": 100.0,  # items/second
            "memory_usage_max": 2048.0,    # MB
            "cpu_utilization_max": 85.0,   # percent
            "worker_utilization_min": 0.3, # 30% minimum utilization
            "batch_efficiency_min": 0.7,   # 70% minimum efficiency
        }
        
    def collect_pipeline_metrics(self) -> PerformanceMetrics:
        """Collect comprehensive metrics from data processing pipeline."""
        pipeline_metrics = self.pipeline.get_performance_metrics()
        
        # Convert to echo.kern PerformanceMetrics format
        metrics = PerformanceMetrics(
            timestamp=time.time(),
            
            # Processing throughput metrics
            token_throughput=pipeline_metrics["throughput"]["avg_processing_rate"],
            request_latency_ms=self._calculate_avg_latency(),
            
            # Resource utilization
            cpu_utilization=pipeline_metrics["resources"]["cpu_utilization"],
            memory_usage=pipeline_metrics["resources"]["memory_usage_mb"],
            
            # DTESN-specific metrics
            membrane_evolution_rate=0.0,  # Will be populated by DTESN processor
            reservoir_dynamics=pipeline_metrics["parallelization"]["worker_utilization"] * 100,
            membrane_level=int(pipeline_metrics["batching"]["avg_batch_size"]),
            
            # Custom processing metrics
            processing_queue_depth=pipeline_metrics["batching"]["queue_depth"],
            parallel_workers_active=pipeline_metrics["parallelization"]["active_workers"],
            batch_processing_rate=self._calculate_batch_rate(),
            
            # Performance indicators
            system_health_score=self._calculate_health_score(pipeline_metrics),
        )
        
        # Store for trend analysis
        self.collection_history.append({
            "timestamp": metrics.timestamp,
            "metrics": asdict(metrics),
            "raw_pipeline_metrics": pipeline_metrics
        })
        
        # Keep only recent history (last 1000 entries)
        if len(self.collection_history) > 1000:
            self.collection_history = self.collection_history[-1000:]
        
        return metrics
    
    def _calculate_avg_latency(self) -> float:
        """Calculate average processing latency from pipeline metrics."""
        if hasattr(self.pipeline, '_processing_times') and self.pipeline._processing_times:
            return sum(self.pipeline._processing_times[-50:]) / len(self.pipeline._processing_times[-50:])
        return 0.0
    
    def _calculate_batch_rate(self) -> float:
        """Calculate batch processing rate (batches/second)."""
        if len(self.collection_history) < 2:
            return 0.0
        
        recent = self.collection_history[-10:]  # Last 10 collections
        if len(recent) < 2:
            return 0.0
        
        time_span = recent[-1]["timestamp"] - recent[0]["timestamp"]
        if time_span <= 0:
            return 0.0
        
        # Estimate batches processed based on items and batch size
        total_items = sum(
            entry["raw_pipeline_metrics"]["throughput"]["items_processed"] 
            for entry in recent
        )
        avg_batch_size = sum(
            entry["raw_pipeline_metrics"]["batching"]["avg_batch_size"] 
            for entry in recent
        ) / len(recent)
        
        if avg_batch_size > 0:
            estimated_batches = total_items / avg_batch_size
            return estimated_batches / time_span
        
        return 0.0
    
    def _calculate_health_score(self, pipeline_metrics: Dict[str, Any]) -> float:
        """Calculate overall system health score (0-100)."""
        score_components = []
        
        # Processing rate health (0-30 points)
        processing_rate = pipeline_metrics["throughput"]["avg_processing_rate"]
        rate_score = min(30, (processing_rate / 1000) * 30) if processing_rate > 0 else 0
        score_components.append(rate_score)
        
        # Worker utilization health (0-25 points)
        worker_util = pipeline_metrics["parallelization"]["worker_utilization"]
        util_score = 25 * min(1.0, worker_util / 0.8)  # Optimal at 80% utilization
        score_components.append(util_score)
        
        # Memory health (0-25 points)
        memory_usage = pipeline_metrics["resources"]["memory_usage_mb"]
        memory_limit = self.alert_thresholds["memory_usage_max"]
        memory_score = max(0, 25 * (1 - memory_usage / memory_limit))
        score_components.append(memory_score)
        
        # CPU health (0-20 points)
        cpu_util = pipeline_metrics["resources"]["cpu_utilization"]
        cpu_limit = self.alert_thresholds["cpu_utilization_max"]
        cpu_score = max(0, 20 * (1 - cpu_util / cpu_limit))
        score_components.append(cpu_score)
        
        return sum(score_components)
    
    def check_alert_conditions(self, metrics: PerformanceMetrics) -> List[Dict[str, Any]]:
        """Check for alert conditions and return list of alerts."""
        alerts = []
        
        # Processing rate too low
        if metrics.token_throughput < self.alert_thresholds["processing_rate_min"]:
            alerts.append({
                "severity": AlertSeverity.WARNING,
                "message": f"Low processing rate: {metrics.token_throughput:.1f} items/s "
                          f"(min: {self.alert_thresholds['processing_rate_min']})",
                "metric": "processing_rate",
                "value": metrics.token_throughput,
                "threshold": self.alert_thresholds["processing_rate_min"]
            })
        
        # Memory usage too high
        if metrics.memory_usage > self.alert_thresholds["memory_usage_max"]:
            alerts.append({
                "severity": AlertSeverity.CRITICAL,
                "message": f"High memory usage: {metrics.memory_usage:.1f}MB "
                          f"(max: {self.alert_thresholds['memory_usage_max']})",
                "metric": "memory_usage",
                "value": metrics.memory_usage,
                "threshold": self.alert_thresholds["memory_usage_max"]
            })
        
        # CPU utilization too high
        if metrics.cpu_utilization > self.alert_thresholds["cpu_utilization_max"]:
            alerts.append({
                "severity": AlertSeverity.WARNING,
                "message": f"High CPU utilization: {metrics.cpu_utilization:.1f}% "
                          f"(max: {self.alert_thresholds['cpu_utilization_max']}%)",
                "metric": "cpu_utilization",
                "value": metrics.cpu_utilization,
                "threshold": self.alert_thresholds["cpu_utilization_max"]
            })
        
        # Worker utilization too low (indicates poor parallelization)
        worker_util = getattr(metrics, 'parallel_workers_active', 0) / max(1, 
            getattr(metrics, 'reservoir_dynamics', 1) / 100)
        if worker_util < self.alert_thresholds["worker_utilization_min"]:
            alerts.append({
                "severity": AlertSeverity.INFO,
                "message": f"Low worker utilization: {worker_util:.2f} "
                          f"(min: {self.alert_thresholds['worker_utilization_min']})",
                "metric": "worker_utilization",
                "value": worker_util,
                "threshold": self.alert_thresholds["worker_utilization_min"]
            })
        
        return alerts


class IntegratedDataPipelineMonitor:
    """
    Integrated monitoring system for DTESN data processing pipelines.
    
    Combines data pipeline monitoring with echo.kern performance system
    for comprehensive observability and automated alerting.
    """
    
    def __init__(
        self,
        pipeline: DataProcessingPipeline,
        enable_echo_integration: bool = True
    ):
        """Initialize integrated pipeline monitor."""
        self.pipeline = pipeline
        self.enable_echo_integration = enable_echo_integration and PERFORMANCE_MONITORING_AVAILABLE
        
        # Initialize collectors
        self.dtesn_collector = DTESNPerformanceCollector(pipeline)
        
        # Initialize monitoring systems
        if self.enable_echo_integration:
            try:
                self.performance_monitor = UnifiedPerformanceMonitor(
                    collection_interval=1.0,
                    enable_alerting=True
                )
                self.integrated_system = None  # Will be created if needed
            except Exception as e:
                logger.warning(f"Failed to initialize echo.kern monitoring: {e}")
                self.enable_echo_integration = False
                self.performance_monitor = None
        else:
            self.performance_monitor = None
        
        # Monitoring state
        self._monitoring_task = None
        self._is_monitoring = False
        self._custom_alert_handlers: List[Callable] = []
        
    async def start_monitoring(self):
        """Start integrated monitoring."""
        if self._is_monitoring:
            return
        
        self._is_monitoring = True
        
        # Start echo.kern monitoring if available
        if self.performance_monitor:
            # Register custom collector
            self.performance_monitor.register_collector(
                "dtesn_pipeline", 
                self.dtesn_collector.collect_pipeline_metrics
            )
            
            # Register alert handler
            self.performance_monitor.register_alert_handler(self._handle_alerts)
            
            # Start monitoring
            self.performance_monitor.start_monitoring()
        
        # Start custom monitoring task
        self._monitoring_task = asyncio.create_task(self._monitor_pipeline())
        
        logger.info("Integrated data pipeline monitoring started")
    
    async def stop_monitoring(self):
        """Stop integrated monitoring."""
        if not self._is_monitoring:
            return
        
        self._is_monitoring = False
        
        # Stop monitoring task
        if self._monitoring_task:
            self._monitoring_task.cancel()
            try:
                await self._monitoring_task
            except asyncio.CancelledError:
                pass
        
        # Stop echo.kern monitoring
        if self.performance_monitor:
            self.performance_monitor.stop_monitoring()
        
        logger.info("Integrated data pipeline monitoring stopped")
    
    async def _monitor_pipeline(self):
        """Background monitoring task for pipeline-specific metrics."""
        while self._is_monitoring:
            try:
                # Collect metrics
                metrics = self.dtesn_collector.collect_pipeline_metrics()
                
                # Check for alerts
                alerts = self.dtesn_collector.check_alert_conditions(metrics)
                
                # Handle alerts
                for alert in alerts:
                    await self._process_alert(alert)
                
                # Log periodic status
                if len(self.dtesn_collector.collection_history) % 60 == 0:  # Every minute
                    self._log_status_summary(metrics)
                
                await asyncio.sleep(1.0)  # Collection interval
                
            except Exception as e:
                logger.error(f"Pipeline monitoring error: {e}")
                await asyncio.sleep(5.0)
    
    async def _handle_alerts(self, alert_data: Dict[str, Any]):
        """Handle alerts from echo.kern monitoring system."""
        await self._process_alert({
            "severity": alert_data.get("severity", AlertSeverity.INFO),
            "message": alert_data.get("message", "Unknown alert"),
            "source": "echo_kern",
            "data": alert_data
        })
    
    async def _process_alert(self, alert: Dict[str, Any]):
        """Process and handle alert."""
        severity = alert.get("severity", AlertSeverity.INFO)
        message = alert.get("message", "Unknown alert")
        
        # Log alert
        if severity == AlertSeverity.CRITICAL:
            logger.error(f"CRITICAL ALERT: {message}")
        elif severity == AlertSeverity.WARNING:
            logger.warning(f"WARNING: {message}")
        else:
            logger.info(f"INFO: {message}")
        
        # Call custom alert handlers
        for handler in self._custom_alert_handlers:
            try:
                await handler(alert)
            except Exception as e:
                logger.error(f"Alert handler failed: {e}")
    
    def _log_status_summary(self, metrics: PerformanceMetrics):
        """Log periodic status summary."""
        logger.info(
            f"Pipeline Status - "
            f"Rate: {metrics.token_throughput:.1f} items/s, "
            f"CPU: {metrics.cpu_utilization:.1f}%, "
            f"Memory: {metrics.memory_usage:.1f}MB, "
            f"Health: {getattr(metrics, 'system_health_score', 0):.1f}/100"
        )
    
    def register_alert_handler(self, handler: Callable):
        """Register custom alert handler."""
        self._custom_alert_handlers.append(handler)
    
    def get_comprehensive_status(self) -> Dict[str, Any]:
        """Get comprehensive monitoring status."""
        status = {
            "monitoring_active": self._is_monitoring,
            "echo_integration_enabled": self.enable_echo_integration,
            "pipeline_metrics": self.pipeline.get_performance_metrics(),
        }
        
        if self.performance_monitor and hasattr(self.performance_monitor, 'get_current_metrics'):
            status["echo_kern_metrics"] = self.performance_monitor.get_current_metrics()
        
        if len(self.dtesn_collector.collection_history) > 0:
            status["metrics_history_count"] = len(self.dtesn_collector.collection_history)
            status["last_collection"] = self.dtesn_collector.collection_history[-1]["timestamp"]
        
        return status
    
    def export_performance_report(self, filepath: Optional[str] = None) -> Dict[str, Any]:
        """Export comprehensive performance report."""
        if not filepath:
            timestamp = int(time.time())
            filepath = f"/tmp/dtesn_pipeline_report_{timestamp}.json"
        
        report = {
            "report_timestamp": time.time(),
            "pipeline_configuration": asdict(self.pipeline.config),
            "current_status": self.get_comprehensive_status(),
            "metrics_history": self.dtesn_collector.collection_history[-100:],  # Last 100 entries
            "alert_thresholds": self.dtesn_collector.alert_thresholds,
        }
        
        # Add echo.kern performance summary if available
        if self.performance_monitor and hasattr(self.performance_monitor, 'get_performance_summary'):
            report["echo_performance_summary"] = self.performance_monitor.get_performance_summary()
        
        # Save to file
        import json
        try:
            with open(filepath, 'w') as f:
                json.dump(report, f, indent=2, default=str)
            logger.info(f"Performance report exported to: {filepath}")
        except Exception as e:
            logger.error(f"Failed to export report: {e}")
        
        return report


async def create_integrated_pipeline_monitor(
    pipeline: DataProcessingPipeline,
    enable_echo_integration: bool = True
) -> IntegratedDataPipelineMonitor:
    """
    Factory function to create and start integrated pipeline monitoring.
    
    Args:
        pipeline: Data processing pipeline to monitor
        enable_echo_integration: Whether to integrate with echo.kern monitoring
        
    Returns:
        Started IntegratedDataPipelineMonitor instance
    """
    monitor = IntegratedDataPipelineMonitor(pipeline, enable_echo_integration)
    await monitor.start_monitoring()
    return monitor