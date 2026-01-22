import time
import logging
import threading
import json
import psutil
import statistics
from dataclasses import dataclass, field, asdict
from typing import Dict, List, Optional, Callable, Any
from collections import deque
from enum import Enum
logger = logging.getLogger(__name__)
class AlertSeverity(Enum):
    INFO = 'info'
    WARNING = 'warning'
    CRITICAL = 'critical'
@dataclass
class PerformanceMetrics:
    timestamp: float
    token_throughput: float = 0.0
    request_latency_ms: float = 0.0
    gpu_utilization: float = 0.0
    kv_cache_usage: float = 0.0
    membrane_evolution_rate: float = 0.0
    reservoir_dynamics: float = 0.0
    membrane_level: int = 0
    evolution_convergence: float = 0.0
    fitness_improvement: float = 0.0
    agent_performance: float = 0.0
    cpu_usage: float = 0.0
    memory_usage: float = 0.0
    disk_usage: float = 0.0
    sensory_motor_latency: float = 0.0
    proprioceptive_accuracy: float = 0.0
    component_id: str = 'unknown'
    metadata: Dict[str, Any] = field(default_factory=dict)
@dataclass
class PerformanceAlert:
    timestamp: float
    severity: AlertSeverity
    component: str
    metric: str
    current_value: float
    threshold: float
    message: str
    metadata: Dict[str, Any] = field(default_factory=dict)
@dataclass
class PerformanceThresholds:
    max_request_latency: float = 1000.0
    max_sensory_motor_latency: float = 100.0
    max_cpu_usage: float = 85.0
    max_memory_usage: float = 90.0
    max_gpu_utilization: float = 95.0
    min_token_throughput: float = 50.0
    min_evolution_convergence: float = 0.7
    min_proprioceptive_accuracy: float = 0.8
    degradation_window_size: int = 10
    degradation_threshold: float = 0.15
class UnifiedPerformanceMonitor:
    def __init__(self, config_path: Optional[str]=None):
        self.config_path = config_path
        self.thresholds = PerformanceThresholds()
        self.metrics_history: deque = deque(maxlen=1000)
        self.alerts_history: deque = deque(maxlen=100)
        self.component_collectors: Dict[str, Callable] = {}
        self.alert_handlers: List[Callable] = []
        self.is_monitoring = False
        self.monitor_thread: Optional[threading.Thread] = None
        self.monitor_interval = 1.0
        self.baseline_metrics: Dict[str, float] = {}
        self.performance_trends: Dict[str, deque] = {}
        self._initialize_default_collectors()
        logger.info('UnifiedPerformanceMonitor initialized')
    def _initialize_default_collectors(self):
        self.register_collector('system', self._collect_system_metrics)
        self.register_collector('aphrodite', self._collect_aphrodite_metrics)
        self.register_collector('dtesn', self._collect_dtesn_metrics)
        self.register_collector('echo_self', self._collect_echo_self_metrics)
        self.register_collector('embodied', self._collect_embodied_metrics)
    def register_collector(self, component: str, collector_func: Callable) -> None:
        self.component_collectors[component] = collector_func
        logger.debug(f'Registered collector for component: {component}')
    def register_alert_handler(self, handler_func: Callable) -> None:
        self.alert_handlers.append(handler_func)
        logger.debug('Registered alert handler')
    def start_monitoring(self) -> None:
        if self.is_monitoring:
            logger.warning('Monitoring is already running')
            return
        self.is_monitoring = True
        self.monitor_thread = threading.Thread(target=self._monitoring_loop, daemon=True)
        self.monitor_thread.start()
        logger.info('Performance monitoring started')
    def stop_monitoring(self) -> None:
        if not self.is_monitoring:
            logger.warning('Monitoring is not running')
            return
        self.is_monitoring = False
        if self.monitor_thread:
            self.monitor_thread.join(timeout=5.0)
        logger.info('Performance monitoring stopped')
    def _monitoring_loop(self) -> None:
        logger.info('Performance monitoring loop started')
        while self.is_monitoring:
            try:
                current_metrics = self._collect_all_metrics()
                if current_metrics:
                    self.metrics_history.append(current_metrics)
                    alerts = self._analyze_performance(current_metrics)
                    for alert in alerts:
                        self._handle_alert(alert)
                    self._update_trends(current_metrics)
            except Exception as e:
                logger.error(f'Error in monitoring loop: {e}')
            time.sleep(self.monitor_interval)
        logger.info('Performance monitoring loop stopped')
    def _collect_all_metrics(self) -> Optional[PerformanceMetrics]:
        timestamp = time.time()
        combined_metrics = PerformanceMetrics(timestamp=timestamp)
        for component, collector in self.component_collectors.items():
            try:
                component_metrics = collector()
                if component_metrics:
                    self._merge_metrics(combined_metrics, component_metrics, component)
            except Exception as e:
                logger.error(f'Error collecting metrics from {component}: {e}')
        return combined_metrics
    def _merge_metrics(self, target: PerformanceMetrics, source: Dict[str, Any], component: str) -> None:
        for key, value in source.items():
            if hasattr(target, key) and isinstance(value, (int, float)):
                setattr(target, key, value)
        target.metadata[component] = source
    def _collect_system_metrics(self) -> Dict[str, Any]:
        try:
            return {'cpu_usage': psutil.cpu_percent(interval=None), 'memory_usage': psutil.virtual_memory().percent, 'disk_usage': psutil.disk_usage('/').percent, 'timestamp': time.time()}
        except Exception as e:
            logger.error(f'Error collecting system metrics: {e}')
            return {}
    def _collect_aphrodite_metrics(self) -> Dict[str, Any]:
        try:
            return {'token_throughput': 100.0, 'request_latency_ms': 50.0, 'gpu_utilization': 75.0, 'kv_cache_usage': 60.0, 'timestamp': time.time()}
        except Exception as e:
            logger.error(f'Error collecting Aphrodite metrics: {e}')
            return {}
    def _collect_dtesn_metrics(self) -> Dict[str, Any]:
        try:
            return {'membrane_evolution_rate': 25.0, 'reservoir_dynamics': 0.85, 'membrane_level': 3, 'timestamp': time.time()}
        except Exception as e:
            logger.error(f'Error collecting DTESN metrics: {e}')
            return {}
    def _collect_echo_self_metrics(self) -> Dict[str, Any]:
        try:
            return {'evolution_convergence': 0.78, 'fitness_improvement': 0.12, 'agent_performance': 0.82, 'timestamp': time.time()}
        except Exception as e:
            logger.error(f'Error collecting Echo-Self metrics: {e}')
            return {}
    def _collect_embodied_metrics(self) -> Dict[str, Any]:
        try:
            return {'sensory_motor_latency': 25.0, 'proprioceptive_accuracy': 0.85, 'timestamp': time.time()}
        except Exception as e:
            logger.error(f'Error collecting embodied metrics: {e}')
            return {}
    def _analyze_performance(self, metrics: PerformanceMetrics) -> List[PerformanceAlert]:
        alerts = []
        alerts.extend(self._check_threshold_violations(metrics))
        alerts.extend(self._check_performance_degradation(metrics))
        return alerts
    def _check_threshold_violations(self, metrics: PerformanceMetrics) -> List[PerformanceAlert]:
        alerts = []
        checks = [('request_latency_ms', metrics.request_latency_ms, self.thresholds.max_request_latency), ('sensory_motor_latency', metrics.sensory_motor_latency, self.thresholds.max_sensory_motor_latency), ('cpu_usage', metrics.cpu_usage, self.thresholds.max_cpu_usage), ('memory_usage', metrics.memory_usage, self.thresholds.max_memory_usage), ('gpu_utilization', metrics.gpu_utilization, self.thresholds.max_gpu_utilization)]
        for metric_name, current_value, threshold in checks:
            if current_value > threshold:
                alert = PerformanceAlert(timestamp=metrics.timestamp, severity=AlertSeverity.WARNING, component=metrics.component_id, metric=metric_name, current_value=current_value, threshold=threshold, message=f'{metric_name} ({current_value:.2f}) exceeds threshold ({threshold:.2f})')
                alerts.append(alert)
        min_checks = [('token_throughput', metrics.token_throughput, self.thresholds.min_token_throughput), ('evolution_convergence', metrics.evolution_convergence, self.thresholds.min_evolution_convergence), ('proprioceptive_accuracy', metrics.proprioceptive_accuracy, self.thresholds.min_proprioceptive_accuracy)]
        for metric_name, current_value, min_threshold in min_checks:
            if current_value < min_threshold:
                alert = PerformanceAlert(timestamp=metrics.timestamp, severity=AlertSeverity.CRITICAL, component=metrics.component_id, metric=metric_name, current_value=current_value, threshold=min_threshold, message=f'{metric_name} ({current_value:.2f}) below minimum threshold ({min_threshold:.2f})')
                alerts.append(alert)
        return alerts
    def _check_performance_degradation(self, metrics: PerformanceMetrics) -> List[PerformanceAlert]:
        alerts = []
        if len(self.metrics_history) < self.thresholds.degradation_window_size:
            return alerts
        recent_metrics = list(self.metrics_history)[-self.thresholds.degradation_window_size:]
        trend_metrics = ['token_throughput', 'evolution_convergence', 'proprioceptive_accuracy']
        for metric_name in trend_metrics:
            values = [getattr(m, metric_name, 0.0) for m in recent_metrics]
            if len(values) >= 2:
                trend = self._calculate_trend(values)
                current_value = getattr(metrics, metric_name, 0.0)
                if trend < -self.thresholds.degradation_threshold * current_value:
                    alert = PerformanceAlert(timestamp=metrics.timestamp, severity=AlertSeverity.WARNING, component=metrics.component_id, metric=metric_name, current_value=current_value, threshold=trend, message=f'{metric_name} showing degradation trend (slope: {trend:.4f})', metadata={'trend_slope': trend, 'window_size': len(values)})
                    alerts.append(alert)
        return alerts
    def _calculate_trend(self, values: List[float]) -> float:
        if len(values) < 2:
            return 0.0
        n = len(values)
        x = list(range(n))
        x_mean = statistics.mean(x)
        y_mean = statistics.mean(values)
        numerator = sum(((x[i] - x_mean) * (values[i] - y_mean) for i in range(n)))
        denominator = sum(((x[i] - x_mean) ** 2 for i in range(n)))
        if denominator == 0:
            return 0.0
        return numerator / denominator
    def _update_trends(self, metrics: PerformanceMetrics) -> None:
        if not self.baseline_metrics:
            self.baseline_metrics = {'token_throughput': metrics.token_throughput, 'evolution_convergence': metrics.evolution_convergence, 'proprioceptive_accuracy': metrics.proprioceptive_accuracy, 'request_latency_ms': metrics.request_latency_ms}
        for metric_name in self.baseline_metrics.keys():
            if metric_name not in self.performance_trends:
                self.performance_trends[metric_name] = deque(maxlen=100)
            current_value = getattr(metrics, metric_name, 0.0)
            self.performance_trends[metric_name].append(current_value)
    def _handle_alert(self, alert: PerformanceAlert) -> None:
        self.alerts_history.append(alert)
        logger.log(logging.WARNING if alert.severity == AlertSeverity.WARNING else logging.CRITICAL, f'Performance Alert: {alert.message}')
        for handler in self.alert_handlers:
            try:
                handler(alert)
            except Exception as e:
                logger.error(f'Error in alert handler: {e}')
    def get_current_metrics(self) -> Optional[PerformanceMetrics]:
        if self.metrics_history:
            return self.metrics_history[-1]
        return None
    def get_recent_alerts(self, hours: int=1) -> List[PerformanceAlert]:
        cutoff_time = time.time() - hours * 3600
        return [alert for alert in self.alerts_history if alert.timestamp > cutoff_time]
    def get_performance_summary(self) -> Dict[str, Any]:
        current_metrics = self.get_current_metrics()
        recent_alerts = self.get_recent_alerts(1)
        def serialize_alert(alert):
            alert_dict = asdict(alert)
            alert_dict['severity'] = alert.severity.value
            return alert_dict
        summary = {'timestamp': time.time(), 'monitoring_active': self.is_monitoring, 'metrics_count': len(self.metrics_history), 'alerts_count': len(recent_alerts), 'current_metrics': asdict(current_metrics) if current_metrics else None, 'recent_alerts': [serialize_alert(alert) for alert in recent_alerts], 'performance_trends': {name: list(trend)[-10:] if trend else [] for name, trend in self.performance_trends.items()}}
        return summary
    def save_metrics_to_file(self, filepath: str) -> None:
        def serialize_alert(alert):
            alert_dict = asdict(alert)
            alert_dict['severity'] = alert.severity.value
            return alert_dict
        data = {'timestamp': time.time(), 'metrics': [asdict(m) for m in self.metrics_history], 'alerts': [serialize_alert(a) for a in self.alerts_history]}
        with open(filepath, 'w') as f:
            json.dump(data, f, indent=2)
        logger.info(f'Metrics saved to {filepath}')
def create_default_monitor() -> UnifiedPerformanceMonitor:
    monitor = UnifiedPerformanceMonitor()
    def default_alert_handler(alert: PerformanceAlert):
        print(f'🚨 {alert.severity.value.upper()}: {alert.message}')
    monitor.register_alert_handler(default_alert_handler)
    return monitor
if __name__ == '__main__':
    monitor = create_default_monitor()
    try:
        print('Starting performance monitoring...')
        monitor.start_monitoring()
        time.sleep(30)
        summary = monitor.get_performance_summary()
        print('\nPerformance Summary:')
        print(f"- Metrics collected: {summary['metrics_count']}")
        print(f"- Alerts generated: {summary['alerts_count']}")
    finally:
        monitor.stop_monitoring()
        print('Performance monitoring stopped')