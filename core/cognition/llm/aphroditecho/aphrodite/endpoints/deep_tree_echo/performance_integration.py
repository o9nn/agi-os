import asyncio
import logging
import os
import sys
import time
from dataclasses import asdict
from typing import Any, Callable, Dict, List, Optional
echo_kern_path = os.path.join(os.path.dirname(__file__), '..', '..', '..', '..', 'echo.kern')
if echo_kern_path not in sys.path:
    sys.path.insert(0, echo_kern_path)
try:
    from performance_integration import IntegratedPerformanceSystem, create_integrated_system
    from performance_monitor import AlertSeverity, PerformanceMetrics, UnifiedPerformanceMonitor
    PERFORMANCE_MONITORING_AVAILABLE = True
except ImportError as e:
    logging.warning(f'Performance monitoring not available: {e}')
    PERFORMANCE_MONITORING_AVAILABLE = False
    class UnifiedPerformanceMonitor:
        def __init__(self, *args, **kwargs):
            pass
        def start_monitoring(self):
            pass
        def stop_monitoring(self):
            pass
        def get_current_metrics(self):
            return {}
        def register_collector(self, name, func):
            pass
    class PerformanceMetrics:
        def __init__(self, **kwargs):
            for k, v in kwargs.items():
                setattr(self, k, v)
    class AlertSeverity:
        INFO = 'info'
        WARNING = 'warning'
        CRITICAL = 'critical'
from aphrodite.endpoints.deep_tree_echo.data_pipeline import DataProcessingPipeline
logger = logging.getLogger(__name__)
class DTESNPerformanceCollector:
    def __init__(self, pipeline: DataProcessingPipeline):
        self.pipeline = pipeline
        self.collection_history: List[Dict[str, Any]] = []
        self.alert_thresholds = {'processing_rate_min': 100.0, 'memory_usage_max': 2048.0, 'cpu_utilization_max': 85.0, 'worker_utilization_min': 0.3, 'batch_efficiency_min': 0.7}
    def collect_pipeline_metrics(self) -> PerformanceMetrics:
        pipeline_metrics = self.pipeline.get_performance_metrics()
        metrics = PerformanceMetrics(timestamp=time.time(), token_throughput=pipeline_metrics['throughput']['avg_processing_rate'], request_latency_ms=self._calculate_avg_latency(), cpu_utilization=pipeline_metrics['resources']['cpu_utilization'], memory_usage=pipeline_metrics['resources']['memory_usage_mb'], membrane_evolution_rate=0.0, reservoir_dynamics=pipeline_metrics['parallelization']['worker_utilization'] * 100, membrane_level=int(pipeline_metrics['batching']['avg_batch_size']), processing_queue_depth=pipeline_metrics['batching']['queue_depth'], parallel_workers_active=pipeline_metrics['parallelization']['active_workers'], batch_processing_rate=self._calculate_batch_rate(), system_health_score=self._calculate_health_score(pipeline_metrics))
        self.collection_history.append({'timestamp': metrics.timestamp, 'metrics': asdict(metrics), 'raw_pipeline_metrics': pipeline_metrics})
        if len(self.collection_history) > 1000:
            self.collection_history = self.collection_history[-1000:]
        return metrics
    def _calculate_avg_latency(self) -> float:
        if hasattr(self.pipeline, '_processing_times') and self.pipeline._processing_times:
            return sum(self.pipeline._processing_times[-50:]) / len(self.pipeline._processing_times[-50:])
        return 0.0
    def _calculate_batch_rate(self) -> float:
        if len(self.collection_history) < 2:
            return 0.0
        recent = self.collection_history[-10:]
        if len(recent) < 2:
            return 0.0
        time_span = recent[-1]['timestamp'] - recent[0]['timestamp']
        if time_span <= 0:
            return 0.0
        total_items = sum((entry['raw_pipeline_metrics']['throughput']['items_processed'] for entry in recent))
        avg_batch_size = sum((entry['raw_pipeline_metrics']['batching']['avg_batch_size'] for entry in recent)) / len(recent)
        if avg_batch_size > 0:
            estimated_batches = total_items / avg_batch_size
            return estimated_batches / time_span
        return 0.0
    def _calculate_health_score(self, pipeline_metrics: Dict[str, Any]) -> float:
        score_components = []
        processing_rate = pipeline_metrics['throughput']['avg_processing_rate']
        rate_score = min(30, processing_rate / 1000 * 30) if processing_rate > 0 else 0
        score_components.append(rate_score)
        worker_util = pipeline_metrics['parallelization']['worker_utilization']
        util_score = 25 * min(1.0, worker_util / 0.8)
        score_components.append(util_score)
        memory_usage = pipeline_metrics['resources']['memory_usage_mb']
        memory_limit = self.alert_thresholds['memory_usage_max']
        memory_score = max(0, 25 * (1 - memory_usage / memory_limit))
        score_components.append(memory_score)
        cpu_util = pipeline_metrics['resources']['cpu_utilization']
        cpu_limit = self.alert_thresholds['cpu_utilization_max']
        cpu_score = max(0, 20 * (1 - cpu_util / cpu_limit))
        score_components.append(cpu_score)
        return sum(score_components)
    def check_alert_conditions(self, metrics: PerformanceMetrics) -> List[Dict[str, Any]]:
        alerts = []
        if metrics.token_throughput < self.alert_thresholds['processing_rate_min']:
            alerts.append({'severity': AlertSeverity.WARNING, 'message': f"Low processing rate: {metrics.token_throughput:.1f} items/s (min: {self.alert_thresholds['processing_rate_min']})", 'metric': 'processing_rate', 'value': metrics.token_throughput, 'threshold': self.alert_thresholds['processing_rate_min']})
        if metrics.memory_usage > self.alert_thresholds['memory_usage_max']:
            alerts.append({'severity': AlertSeverity.CRITICAL, 'message': f"High memory usage: {metrics.memory_usage:.1f}MB (max: {self.alert_thresholds['memory_usage_max']})", 'metric': 'memory_usage', 'value': metrics.memory_usage, 'threshold': self.alert_thresholds['memory_usage_max']})
        if metrics.cpu_utilization > self.alert_thresholds['cpu_utilization_max']:
            alerts.append({'severity': AlertSeverity.WARNING, 'message': f"High CPU utilization: {metrics.cpu_utilization:.1f}% (max: {self.alert_thresholds['cpu_utilization_max']}%)", 'metric': 'cpu_utilization', 'value': metrics.cpu_utilization, 'threshold': self.alert_thresholds['cpu_utilization_max']})
        worker_util = getattr(metrics, 'parallel_workers_active', 0) / max(1, getattr(metrics, 'reservoir_dynamics', 1) / 100)
        if worker_util < self.alert_thresholds['worker_utilization_min']:
            alerts.append({'severity': AlertSeverity.INFO, 'message': f"Low worker utilization: {worker_util:.2f} (min: {self.alert_thresholds['worker_utilization_min']})", 'metric': 'worker_utilization', 'value': worker_util, 'threshold': self.alert_thresholds['worker_utilization_min']})
        return alerts
class IntegratedDataPipelineMonitor:
    def __init__(self, pipeline: DataProcessingPipeline, enable_echo_integration: bool=True):
        self.pipeline = pipeline
        self.enable_echo_integration = enable_echo_integration and PERFORMANCE_MONITORING_AVAILABLE
        self.dtesn_collector = DTESNPerformanceCollector(pipeline)
        if self.enable_echo_integration:
            try:
                self.performance_monitor = UnifiedPerformanceMonitor(collection_interval=1.0, enable_alerting=True)
                self.integrated_system = None
            except Exception as e:
                logger.warning(f'Failed to initialize echo.kern monitoring: {e}')
                self.enable_echo_integration = False
                self.performance_monitor = None
        else:
            self.performance_monitor = None
        self._monitoring_task = None
        self._is_monitoring = False
        self._custom_alert_handlers: List[Callable] = []
    async def start_monitoring(self):
        if self._is_monitoring:
            return
        self._is_monitoring = True
        if self.performance_monitor:
            self.performance_monitor.register_collector('dtesn_pipeline', self.dtesn_collector.collect_pipeline_metrics)
            self.performance_monitor.register_alert_handler(self._handle_alerts)
            self.performance_monitor.start_monitoring()
        self._monitoring_task = asyncio.create_task(self._monitor_pipeline())
        logger.info('Integrated data pipeline monitoring started')
    async def stop_monitoring(self):
        if not self._is_monitoring:
            return
        self._is_monitoring = False
        if self._monitoring_task:
            self._monitoring_task.cancel()
            try:
                await self._monitoring_task
            except asyncio.CancelledError:
                pass
        if self.performance_monitor:
            self.performance_monitor.stop_monitoring()
        logger.info('Integrated data pipeline monitoring stopped')
    async def _monitor_pipeline(self):
        while self._is_monitoring:
            try:
                metrics = self.dtesn_collector.collect_pipeline_metrics()
                alerts = self.dtesn_collector.check_alert_conditions(metrics)
                for alert in alerts:
                    await self._process_alert(alert)
                if len(self.dtesn_collector.collection_history) % 60 == 0:
                    self._log_status_summary(metrics)
                await asyncio.sleep(1.0)
            except Exception as e:
                logger.error(f'Pipeline monitoring error: {e}')
                await asyncio.sleep(5.0)
    async def _handle_alerts(self, alert_data: Dict[str, Any]):
        await self._process_alert({'severity': alert_data.get('severity', AlertSeverity.INFO), 'message': alert_data.get('message', 'Unknown alert'), 'source': 'echo_kern', 'data': alert_data})
    async def _process_alert(self, alert: Dict[str, Any]):
        severity = alert.get('severity', AlertSeverity.INFO)
        message = alert.get('message', 'Unknown alert')
        if severity == AlertSeverity.CRITICAL:
            logger.error(f'CRITICAL ALERT: {message}')
        elif severity == AlertSeverity.WARNING:
            logger.warning(f'WARNING: {message}')
        else:
            logger.info(f'INFO: {message}')
        for handler in self._custom_alert_handlers:
            try:
                await handler(alert)
            except Exception as e:
                logger.error(f'Alert handler failed: {e}')
    def _log_status_summary(self, metrics: PerformanceMetrics):
        logger.info(f"Pipeline Status - Rate: {metrics.token_throughput:.1f} items/s, CPU: {metrics.cpu_utilization:.1f}%, Memory: {metrics.memory_usage:.1f}MB, Health: {getattr(metrics, 'system_health_score', 0):.1f}/100")
    def register_alert_handler(self, handler: Callable):
        self._custom_alert_handlers.append(handler)
    def get_comprehensive_status(self) -> Dict[str, Any]:
        status = {'monitoring_active': self._is_monitoring, 'echo_integration_enabled': self.enable_echo_integration, 'pipeline_metrics': self.pipeline.get_performance_metrics()}
        if self.performance_monitor and hasattr(self.performance_monitor, 'get_current_metrics'):
            status['echo_kern_metrics'] = self.performance_monitor.get_current_metrics()
        if len(self.dtesn_collector.collection_history) > 0:
            status['metrics_history_count'] = len(self.dtesn_collector.collection_history)
            status['last_collection'] = self.dtesn_collector.collection_history[-1]['timestamp']
        return status
    def export_performance_report(self, filepath: Optional[str]=None) -> Dict[str, Any]:
        if not filepath:
            timestamp = int(time.time())
            filepath = f'/tmp/dtesn_pipeline_report_{timestamp}.json'
        report = {'report_timestamp': time.time(), 'pipeline_configuration': asdict(self.pipeline.config), 'current_status': self.get_comprehensive_status(), 'metrics_history': self.dtesn_collector.collection_history[-100:], 'alert_thresholds': self.dtesn_collector.alert_thresholds}
        if self.performance_monitor and hasattr(self.performance_monitor, 'get_performance_summary'):
            report['echo_performance_summary'] = self.performance_monitor.get_performance_summary()
        import json
        try:
            with open(filepath, 'w') as f:
                json.dump(report, f, indent=2, default=str)
            logger.info(f'Performance report exported to: {filepath}')
        except Exception as e:
            logger.error(f'Failed to export report: {e}')
        return report
async def create_integrated_pipeline_monitor(pipeline: DataProcessingPipeline, enable_echo_integration: bool=True) -> IntegratedDataPipelineMonitor:
    monitor = IntegratedDataPipelineMonitor(pipeline, enable_echo_integration)
    await monitor.start_monitoring()
    return monitor