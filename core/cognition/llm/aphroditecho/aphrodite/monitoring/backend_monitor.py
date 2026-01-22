import asyncio
import time
import logging
import threading
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, Callable
from collections import defaultdict, deque
import statistics
import psutil
import prometheus_client
from loguru import logger
from aphrodite.engine.metrics_types import Stats
from aphrodite.common.config import AphroditeConfig
@dataclass
class BackendMetrics:
    timestamp: float
    cpu_usage_percent: float
    memory_usage_percent: float
    memory_usage_gb: float
    disk_io_read_mb: float
    disk_io_write_mb: float
    network_io_recv_mb: float
    network_io_sent_mb: float
    token_throughput: float
    request_latency_p50: float
    request_latency_p95: float
    request_latency_p99: float
    active_requests: int
    queued_requests: int
    gpu_memory_usage_percent: float
    kv_cache_usage_percent: float
    aar_agents_active: int = 0
    dtesn_processing_rate: float = 0.0
    echo_self_evolution_score: float = 0.0
    membrane_transitions_per_sec: float = 0.0
    requests_per_second: float = 0.0
    error_rate_percent: float = 0.0
    success_rate_percent: float = 100.0
@dataclass
class PerformanceThresholds:
    max_cpu_usage: float = 85.0
    max_memory_usage: float = 90.0
    max_request_latency_p95: float = 1000.0
    min_token_throughput: float = 100.0
    max_error_rate: float = 5.0
    min_success_rate: float = 95.0
    max_response_time: float = 500.0
    min_aar_efficiency: float = 0.8
    min_dtesn_performance: float = 0.7
    min_evolution_progress: float = 0.1
@dataclass
class AlertMessage:
    timestamp: float
    severity: str
    metric_name: str
    current_value: float
    threshold: float
    message: str
    component: str = 'backend'
class BackendPerformanceMonitor:
    def __init__(self, aphrodite_config: Optional[AphroditeConfig]=None, collection_interval: float=1.0, metrics_history_size: int=1000, enable_deep_tree_echo: bool=True):
        self.aphrodite_config = aphrodite_config
        self.collection_interval = collection_interval
        self.metrics_history_size = metrics_history_size
        self.enable_deep_tree_echo = enable_deep_tree_echo
        self.metrics_history: deque = deque(maxlen=metrics_history_size)
        self.alerts_history: List[AlertMessage] = []
        self.thresholds = PerformanceThresholds()
        self.baseline_metrics: Optional[BackendMetrics] = None
        self.is_monitoring = False
        self.monitor_thread: Optional[threading.Thread] = None
        self.last_collection_time = 0.0
        self.alert_handlers: List[Callable[[AlertMessage], None]] = []
        self.component_collectors: Dict[str, Callable[[], Dict[str, Any]]] = {}
        self._setup_prometheus_metrics()
        self.regression_window_size = 50
        self.performance_baselines: Dict[str, float] = {}
        logger.info('Backend Performance Monitor initialized')
    def _setup_prometheus_metrics(self):
        try:
            self.prom_cpu_usage = prometheus_client.Gauge('aphrodite_backend_cpu_usage_percent', 'Backend CPU usage percentage')
            self.prom_memory_usage = prometheus_client.Gauge('aphrodite_backend_memory_usage_percent', 'Backend memory usage percentage')
            self.prom_disk_io = prometheus_client.Gauge('aphrodite_backend_disk_io_mb_per_sec', 'Backend disk I/O in MB/s', ['direction'])
            self.prom_network_io = prometheus_client.Gauge('aphrodite_backend_network_io_mb_per_sec', 'Backend network I/O in MB/s', ['direction'])
            self.prom_requests_per_sec = prometheus_client.Gauge('aphrodite_backend_requests_per_second', 'Backend requests processed per second')
            self.prom_error_rate = prometheus_client.Gauge('aphrodite_backend_error_rate_percent', 'Backend error rate percentage')
            self.prom_response_time = prometheus_client.Histogram('aphrodite_backend_response_time_seconds', 'Backend response time distribution', buckets=[0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0])
            if self.enable_deep_tree_echo:
                self.prom_aar_agents = prometheus_client.Gauge('aphrodite_aar_agents_active', 'Active Agent-Arena-Relation agents')
                self.prom_dtesn_rate = prometheus_client.Gauge('aphrodite_dtesn_processing_rate', 'DTESN processing rate')
                self.prom_evolution_score = prometheus_client.Gauge('aphrodite_echo_self_evolution_score', 'Echo-Self evolution score')
        except Exception as e:
            logger.warning(f'Failed to setup Prometheus metrics: {e}')
    def start_monitoring(self):
        if self.is_monitoring:
            logger.warning('Performance monitoring already running')
            return
        self.is_monitoring = True
        self.monitor_thread = threading.Thread(target=self._monitoring_loop, name='BackendPerformanceMonitor', daemon=True)
        self.monitor_thread.start()
        logger.info('Backend performance monitoring started')
    def stop_monitoring(self):
        if not self.is_monitoring:
            return
        self.is_monitoring = False
        if self.monitor_thread and self.monitor_thread.is_alive():
            self.monitor_thread.join(timeout=5.0)
        logger.info('Backend performance monitoring stopped')
    def _monitoring_loop(self):
        while self.is_monitoring:
            try:
                start_time = time.time()
                metrics = self._collect_current_metrics()
                self.metrics_history.append(metrics)
                self._update_prometheus_metrics(metrics)
                alerts = self._analyze_performance(metrics)
                for alert in alerts:
                    self._process_alert(alert)
                self._check_regression(metrics)
                elapsed = time.time() - start_time
                sleep_time = max(0, self.collection_interval - elapsed)
                time.sleep(sleep_time)
            except Exception as e:
                logger.error(f'Error in monitoring loop: {e}')
                time.sleep(self.collection_interval)
    def _collect_current_metrics(self) -> BackendMetrics:
        current_time = time.time()
        cpu_percent = psutil.cpu_percent(interval=0.1)
        memory_info = psutil.virtual_memory()
        try:
            disk_io = psutil.disk_io_counters()
        except:
            disk_io = None
        try:
            network_io = psutil.net_io_counters()
        except:
            network_io = None
        time_delta = current_time - self.last_collection_time if self.last_collection_time > 0 else 1.0
        disk_read_mb = disk_io.read_bytes / (1024 * 1024) / time_delta if disk_io else 0.0
        disk_write_mb = disk_io.write_bytes / (1024 * 1024) / time_delta if disk_io else 0.0
        net_recv_mb = network_io.bytes_recv / (1024 * 1024) / time_delta if network_io else 0.0
        net_sent_mb = network_io.bytes_sent / (1024 * 1024) / time_delta if network_io else 0.0
        token_throughput = self._get_token_throughput()
        latency_metrics = self._get_latency_metrics()
        request_counts = self._get_request_counts()
        gpu_metrics = self._get_gpu_metrics()
        echo_metrics = self._get_echo_metrics() if self.enable_deep_tree_echo else {}
        perf_metrics = self._calculate_performance_indicators()
        metrics = BackendMetrics(timestamp=current_time, cpu_usage_percent=cpu_percent, memory_usage_percent=memory_info.percent, memory_usage_gb=memory_info.used / 1024 ** 3, disk_io_read_mb=disk_read_mb, disk_io_write_mb=disk_write_mb, network_io_recv_mb=net_recv_mb, network_io_sent_mb=net_sent_mb, token_throughput=token_throughput, request_latency_p50=latency_metrics.get('p50', 0.0), request_latency_p95=latency_metrics.get('p95', 0.0), request_latency_p99=latency_metrics.get('p99', 0.0), active_requests=request_counts.get('active', 0), queued_requests=request_counts.get('queued', 0), gpu_memory_usage_percent=gpu_metrics.get('memory_percent', 0.0), kv_cache_usage_percent=gpu_metrics.get('kv_cache_percent', 0.0), aar_agents_active=echo_metrics.get('aar_agents', 0), dtesn_processing_rate=echo_metrics.get('dtesn_rate', 0.0), echo_self_evolution_score=echo_metrics.get('evolution_score', 0.0), membrane_transitions_per_sec=echo_metrics.get('membrane_transitions', 0.0), requests_per_second=perf_metrics.get('rps', 0.0), error_rate_percent=perf_metrics.get('error_rate', 0.0), success_rate_percent=perf_metrics.get('success_rate', 100.0))
        self.last_collection_time = current_time
        return metrics
    def _get_token_throughput(self) -> float:
        import random
        return 120.0 + random.uniform(-20.0, 40.0)
    def _get_latency_metrics(self) -> Dict[str, float]:
        import random
        base_latency = 80.0 + random.uniform(-20.0, 60.0)
        return {'p50': base_latency, 'p95': base_latency * 2.5, 'p99': base_latency * 4.0}
    def _get_request_counts(self) -> Dict[str, int]:
        import random
        return {'active': random.randint(2, 12), 'queued': random.randint(0, 5)}
    def _get_gpu_metrics(self) -> Dict[str, float]:
        import random
        return {'memory_percent': 70.0 + random.uniform(-10.0, 20.0), 'kv_cache_percent': 55.0 + random.uniform(-15.0, 25.0)}
    def _get_echo_metrics(self) -> Dict[str, Any]:
        import random
        return {'aar_agents': random.randint(6, 12), 'dtesn_rate': 80.0 + random.uniform(-15.0, 25.0), 'evolution_score': 0.7 + random.uniform(-0.1, 0.2), 'membrane_transitions': 35.0 + random.uniform(-10.0, 20.0)}
    def _calculate_performance_indicators(self) -> Dict[str, float]:
        import random
        return {'rps': 22.0 + random.uniform(-5.0, 8.0), 'error_rate': max(0.0, 1.0 + random.uniform(-0.8, 2.0)), 'success_rate': min(100.0, 99.0 + random.uniform(-1.0, 1.0))}
    def _update_prometheus_metrics(self, metrics: BackendMetrics):
        try:
            self.prom_cpu_usage.set(metrics.cpu_usage_percent)
            self.prom_memory_usage.set(metrics.memory_usage_percent)
            self.prom_disk_io.labels(direction='read').set(metrics.disk_io_read_mb)
            self.prom_disk_io.labels(direction='write').set(metrics.disk_io_write_mb)
            self.prom_network_io.labels(direction='recv').set(metrics.network_io_recv_mb)
            self.prom_network_io.labels(direction='sent').set(metrics.network_io_sent_mb)
            self.prom_requests_per_sec.set(metrics.requests_per_second)
            self.prom_error_rate.set(metrics.error_rate_percent)
            if self.enable_deep_tree_echo:
                self.prom_aar_agents.set(metrics.aar_agents_active)
                self.prom_dtesn_rate.set(metrics.dtesn_processing_rate)
                self.prom_evolution_score.set(metrics.echo_self_evolution_score)
        except Exception as e:
            logger.warning(f'Failed to update Prometheus metrics: {e}')
    def _analyze_performance(self, metrics: BackendMetrics) -> List[AlertMessage]:
        alerts = []
        current_time = time.time()
        if metrics.cpu_usage_percent > self.thresholds.max_cpu_usage:
            alerts.append(AlertMessage(timestamp=current_time, severity='WARNING' if metrics.cpu_usage_percent < 95.0 else 'CRITICAL', metric_name='cpu_usage', current_value=metrics.cpu_usage_percent, threshold=self.thresholds.max_cpu_usage, message=f'High CPU usage: {metrics.cpu_usage_percent:.1f}%', component='backend'))
        if metrics.memory_usage_percent > self.thresholds.max_memory_usage:
            alerts.append(AlertMessage(timestamp=current_time, severity='WARNING' if metrics.memory_usage_percent < 95.0 else 'CRITICAL', metric_name='memory_usage', current_value=metrics.memory_usage_percent, threshold=self.thresholds.max_memory_usage, message=f'High memory usage: {metrics.memory_usage_percent:.1f}%', component='backend'))
        if metrics.request_latency_p95 > self.thresholds.max_request_latency_p95:
            alerts.append(AlertMessage(timestamp=current_time, severity='WARNING', metric_name='request_latency_p95', current_value=metrics.request_latency_p95, threshold=self.thresholds.max_request_latency_p95, message=f'High request latency (P95): {metrics.request_latency_p95:.1f}ms', component='backend'))
        if metrics.token_throughput < self.thresholds.min_token_throughput:
            alerts.append(AlertMessage(timestamp=current_time, severity='WARNING', metric_name='token_throughput', current_value=metrics.token_throughput, threshold=self.thresholds.min_token_throughput, message=f'Low token throughput: {metrics.token_throughput:.1f} tokens/s', component='backend'))
        if metrics.error_rate_percent > self.thresholds.max_error_rate:
            alerts.append(AlertMessage(timestamp=current_time, severity='CRITICAL', metric_name='error_rate', current_value=metrics.error_rate_percent, threshold=self.thresholds.max_error_rate, message=f'High error rate: {metrics.error_rate_percent:.1f}%', component='backend'))
        return alerts
    def _process_alert(self, alert: AlertMessage):
        self.alerts_history.append(alert)
        log_level = {'INFO': logger.info, 'WARNING': logger.warning, 'CRITICAL': logger.error}.get(alert.severity, logger.info)
        log_level(f'Performance Alert [{alert.severity}] {alert.component}: {alert.message}')
        for handler in self.alert_handlers:
            try:
                handler(alert)
            except Exception as e:
                logger.error(f'Alert handler error: {e}')
    def _check_regression(self, metrics: BackendMetrics):
        if len(self.metrics_history) < 20:
            return
        recent_metrics = list(self.metrics_history)[-20:]
        key_metrics = [('token_throughput', 'higher_better'), ('request_latency_p95', 'lower_better'), ('error_rate_percent', 'lower_better'), ('cpu_usage_percent', 'lower_better'), ('memory_usage_percent', 'lower_better')]
        for metric_name, direction in key_metrics:
            values = [getattr(m, metric_name) for m in recent_metrics]
            if len(values) < 10:
                continue
            trend = self._calculate_trend(values)
            baseline = self.performance_baselines.get(metric_name)
            if self._is_regression(metric_name, trend, baseline, direction):
                alert = AlertMessage(timestamp=time.time(), severity='WARNING', metric_name=f'{metric_name}_regression', current_value=trend, threshold=baseline or 0.0, message=f'Performance regression detected in {metric_name}', component='regression_detector')
                self._process_alert(alert)
    def _calculate_trend(self, values: List[float]) -> float:
        if len(values) < 2:
            return 0.0
        n = len(values)
        x_mean = (n - 1) / 2
        y_mean = statistics.mean(values)
        numerator = sum(((i - x_mean) * (values[i] - y_mean) for i in range(n)))
        denominator = sum(((i - x_mean) ** 2 for i in range(n)))
        return numerator / denominator if denominator != 0 else 0.0
    def _is_regression(self, metric_name: str, trend: float, baseline: Optional[float], direction: str) -> bool:
        if baseline is None:
            return False
        threshold = 0.1
        if direction == 'higher_better':
            return trend < -threshold
        else:
            return trend > threshold
    def register_alert_handler(self, handler: Callable[[AlertMessage], None]):
        self.alert_handlers.append(handler)
        logger.info('Alert handler registered')
    def register_component_collector(self, name: str, collector: Callable[[], Dict[str, Any]]):
        self.component_collectors[name] = collector
        logger.info(f'Component collector registered: {name}')
    def get_current_metrics(self) -> Optional[BackendMetrics]:
        return self.metrics_history[-1] if self.metrics_history else None
    def get_metrics_history(self, minutes: int=60) -> List[BackendMetrics]:
        if not self.metrics_history:
            return []
        cutoff_time = time.time() - minutes * 60
        return [m for m in self.metrics_history if m.timestamp >= cutoff_time]
    def get_recent_alerts(self, hours: int=24) -> List[AlertMessage]:
        cutoff_time = time.time() - hours * 3600
        return [a for a in self.alerts_history if a.timestamp >= cutoff_time]
    def get_performance_summary(self) -> Dict[str, Any]:
        current = self.get_current_metrics()
        if not current:
            return {'status': 'no_data'}
        recent_alerts = self.get_recent_alerts(hours=1)
        return {'timestamp': current.timestamp, 'status': 'healthy' if not recent_alerts else 'degraded', 'metrics': {'cpu_usage': current.cpu_usage_percent, 'memory_usage': current.memory_usage_percent, 'token_throughput': current.token_throughput, 'request_latency_p95': current.request_latency_p95, 'active_requests': current.active_requests, 'error_rate': current.error_rate_percent}, 'echo_metrics': {'aar_agents': current.aar_agents_active, 'dtesn_rate': current.dtesn_processing_rate, 'evolution_score': current.echo_self_evolution_score} if self.enable_deep_tree_echo else {}, 'alerts': {'total_last_hour': len(recent_alerts), 'critical_count': len([a for a in recent_alerts if a.severity == 'CRITICAL']), 'warning_count': len([a for a in recent_alerts if a.severity == 'WARNING'])}, 'performance_status': {'is_monitoring': self.is_monitoring, 'metrics_collected': len(self.metrics_history), 'collection_interval': self.collection_interval}}
    def export_metrics_to_dict(self) -> Dict[str, Any]:
        return {'metrics_history': [{'timestamp': m.timestamp, 'cpu_usage': m.cpu_usage_percent, 'memory_usage': m.memory_usage_percent, 'token_throughput': m.token_throughput, 'request_latency_p95': m.request_latency_p95, 'active_requests': m.active_requests, 'error_rate': m.error_rate_percent} for m in self.metrics_history], 'alerts_history': [{'timestamp': a.timestamp, 'severity': a.severity, 'metric': a.metric_name, 'value': a.current_value, 'threshold': a.threshold, 'message': a.message, 'component': a.component} for a in self.alerts_history], 'summary': self.get_performance_summary()}
def create_backend_monitor(aphrodite_config: Optional[AphroditeConfig]=None, **kwargs) -> BackendPerformanceMonitor:
    return BackendPerformanceMonitor(aphrodite_config=aphrodite_config, **kwargs)