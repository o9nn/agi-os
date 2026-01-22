import asyncio
import json
import logging
import time
from collections import defaultdict, deque
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Callable
from dataclasses import dataclass, field
from enum import Enum
from .errors import DTESNError, ErrorSeverity, ErrorCategory, error_aggregator
from .error_recovery import error_recovery_service
logger = logging.getLogger(__name__)
class AlertLevel(Enum):
    INFO = 'info'
    WARNING = 'warning'
    CRITICAL = 'critical'
    EMERGENCY = 'emergency'
class MetricType(Enum):
    ERROR_RATE = 'error_rate'
    RESPONSE_TIME = 'response_time'
    AVAILABILITY = 'availability'
    THROUGHPUT = 'throughput'
    RECOVERY_RATE = 'recovery_rate'
    CIRCUIT_BREAKER_STATE = 'circuit_breaker_state'
@dataclass
class Alert:
    id: str
    level: AlertLevel
    metric_type: MetricType
    message: str
    timestamp: datetime
    threshold_value: float
    current_value: float
    context: Dict[str, Any] = field(default_factory=dict)
    resolved: bool = False
    resolution_time: Optional[datetime] = None
    def to_dict(self) -> Dict[str, Any]:
        return {'id': self.id, 'level': self.level.value, 'metric_type': self.metric_type.value, 'message': self.message, 'timestamp': self.timestamp.isoformat(), 'threshold_value': self.threshold_value, 'current_value': self.current_value, 'context': self.context, 'resolved': self.resolved, 'resolution_time': self.resolution_time.isoformat() if self.resolution_time else None}
@dataclass
class MonitoringMetrics:
    timestamp: datetime
    error_rate: float = 0.0
    avg_response_time_ms: float = 0.0
    availability_percent: float = 100.0
    throughput_rps: float = 0.0
    recovery_success_rate: float = 100.0
    active_requests: int = 0
    circuit_breaker_open: bool = False
    validation_errors: int = 0
    processing_errors: int = 0
    resource_errors: int = 0
    system_errors: int = 0
    successful_recoveries: int = 0
    failed_recoveries: int = 0
    fallback_activations: int = 0
    def to_dict(self) -> Dict[str, Any]:
        return {'timestamp': self.timestamp.isoformat(), 'error_rate': self.error_rate, 'avg_response_time_ms': self.avg_response_time_ms, 'availability_percent': self.availability_percent, 'throughput_rps': self.throughput_rps, 'recovery_success_rate': self.recovery_success_rate, 'active_requests': self.active_requests, 'circuit_breaker_open': self.circuit_breaker_open, 'error_breakdown': {'validation_errors': self.validation_errors, 'processing_errors': self.processing_errors, 'resource_errors': self.resource_errors, 'system_errors': self.system_errors}, 'recovery_metrics': {'successful_recoveries': self.successful_recoveries, 'failed_recoveries': self.failed_recoveries, 'fallback_activations': self.fallback_activations}}
class ThresholdConfig:
    def __init__(self):
        self.thresholds = {MetricType.ERROR_RATE: {AlertLevel.WARNING: 0.05, AlertLevel.CRITICAL: 0.1, AlertLevel.EMERGENCY: 0.2}, MetricType.RESPONSE_TIME: {AlertLevel.WARNING: 1000.0, AlertLevel.CRITICAL: 5000.0, AlertLevel.EMERGENCY: 10000.0}, MetricType.AVAILABILITY: {AlertLevel.WARNING: 99.5, AlertLevel.CRITICAL: 99.0, AlertLevel.EMERGENCY: 95.0}, MetricType.RECOVERY_RATE: {AlertLevel.WARNING: 95.0, AlertLevel.CRITICAL: 90.0, AlertLevel.EMERGENCY: 80.0}}
    def get_alert_level(self, metric_type: MetricType, value: float) -> Optional[AlertLevel]:
        if metric_type not in self.thresholds:
            return None
        thresholds = self.thresholds[metric_type]
        if metric_type in [MetricType.AVAILABILITY, MetricType.RECOVERY_RATE]:
            if value <= thresholds[AlertLevel.EMERGENCY]:
                return AlertLevel.EMERGENCY
            elif value <= thresholds[AlertLevel.CRITICAL]:
                return AlertLevel.CRITICAL
            elif value <= thresholds[AlertLevel.WARNING]:
                return AlertLevel.WARNING
        elif value >= thresholds[AlertLevel.EMERGENCY]:
            return AlertLevel.EMERGENCY
        elif value >= thresholds[AlertLevel.CRITICAL]:
            return AlertLevel.CRITICAL
        elif value >= thresholds[AlertLevel.WARNING]:
            return AlertLevel.WARNING
        return None
class MetricsCollector:
    def __init__(self, window_size: int=300):
        self.window_size = window_size
        self.request_times = deque(maxlen=1000)
        self.error_events = deque(maxlen=1000)
        self.success_events = deque(maxlen=1000)
        self.recovery_events = deque(maxlen=1000)
        self.active_requests = 0
        self.total_requests = 0
        self.total_errors = 0
        self.error_counts = defaultdict(int)
    def record_request_start(self) -> str:
        request_id = f'req_{int(time.time() * 1000000)}'
        self.active_requests += 1
        self.total_requests += 1
        return request_id
    def record_request_end(self, request_id: str, success: bool, response_time_ms: float):
        self.active_requests = max(0, self.active_requests - 1)
        timestamp = datetime.now()
        self.request_times.append((timestamp, response_time_ms))
        if success:
            self.success_events.append(timestamp)
        else:
            self.error_events.append(timestamp)
            self.total_errors += 1
    def record_error(self, error: DTESNError):
        self.error_counts[error.category.value] += 1
    def record_recovery(self, success: bool, recovery_mode: str):
        self.recovery_events.append({'timestamp': datetime.now(), 'success': success, 'mode': recovery_mode})
    def get_current_metrics(self) -> MonitoringMetrics:
        now = datetime.now()
        window_start = now - timedelta(seconds=self.window_size)
        recent_requests = [(ts, rt) for ts, rt in self.request_times if ts > window_start]
        recent_errors = [ts for ts in self.error_events if ts > window_start]
        recent_successes = [ts for ts in self.success_events if ts > window_start]
        recent_recoveries = [evt for evt in self.recovery_events if evt['timestamp'] > window_start]
        total_recent = len(recent_requests)
        error_count = len(recent_errors)
        success_count = len(recent_successes)
        error_rate = error_count / max(total_recent, 1)
        availability = success_count / max(total_recent, 1) * 100
        throughput = total_recent / self.window_size
        avg_response_time = 0.0
        if recent_requests:
            avg_response_time = sum((rt for _, rt in recent_requests)) / len(recent_requests)
        successful_recoveries = sum((1 for evt in recent_recoveries if evt['success']))
        total_recoveries = len(recent_recoveries)
        recovery_rate = successful_recoveries / max(total_recoveries, 1) * 100
        system_health = error_aggregator.get_system_health_status()
        return MonitoringMetrics(timestamp=now, error_rate=error_rate, avg_response_time_ms=avg_response_time, availability_percent=availability, throughput_rps=throughput, recovery_success_rate=recovery_rate, active_requests=self.active_requests, circuit_breaker_open=system_health.get('should_circuit_break', False), validation_errors=self.error_counts.get('validation', 0), processing_errors=self.error_counts.get('processing', 0), resource_errors=self.error_counts.get('resource', 0), system_errors=self.error_counts.get('system', 0), successful_recoveries=successful_recoveries, failed_recoveries=total_recoveries - successful_recoveries, fallback_activations=sum((1 for evt in recent_recoveries if 'fallback' in evt['mode'])))
class AlertManager:
    def __init__(self, threshold_config: Optional[ThresholdConfig]=None):
        self.threshold_config = threshold_config or ThresholdConfig()
        self.active_alerts: Dict[str, Alert] = {}
        self.alert_history: List[Alert] = []
        self.notification_handlers: List[Callable[[Alert], None]] = []
        self.alert_cooldown: Dict[str, datetime] = {}
        self.cooldown_period = timedelta(minutes=5)
    def add_notification_handler(self, handler: Callable[[Alert], None]):
        self.notification_handlers.append(handler)
    def check_metrics(self, metrics: MonitoringMetrics) -> List[Alert]:
        alerts = []
        metric_checks = [(MetricType.ERROR_RATE, metrics.error_rate), (MetricType.RESPONSE_TIME, metrics.avg_response_time_ms), (MetricType.AVAILABILITY, metrics.availability_percent), (MetricType.RECOVERY_RATE, metrics.recovery_success_rate)]
        for metric_type, value in metric_checks:
            alert_level = self.threshold_config.get_alert_level(metric_type, value)
            if alert_level:
                alert = self._create_alert(metric_type, alert_level, value, metrics)
                if alert and self._should_send_alert(alert):
                    alerts.append(alert)
                    self._process_alert(alert)
        if metrics.circuit_breaker_open:
            alert = Alert(id=f'circuit_breaker_{int(time.time())}', level=AlertLevel.CRITICAL, metric_type=MetricType.CIRCUIT_BREAKER_STATE, message='Circuit breaker is OPEN - service degraded', timestamp=metrics.timestamp, threshold_value=1.0, current_value=1.0, context={'circuit_breaker_open': True})
            if self._should_send_alert(alert):
                alerts.append(alert)
                self._process_alert(alert)
        return alerts
    def _create_alert(self, metric_type: MetricType, level: AlertLevel, value: float, metrics: MonitoringMetrics) -> Optional[Alert]:
        alert_id = f'{metric_type.value}_{level.value}_{int(time.time())}'
        threshold = self.threshold_config.thresholds[metric_type][level]
        messages = {MetricType.ERROR_RATE: f'Error rate is {value:.1%} (threshold: {threshold:.1%})', MetricType.RESPONSE_TIME: f'Response time is {value:.0f}ms (threshold: {threshold:.0f}ms)', MetricType.AVAILABILITY: f'Availability is {value:.1f}% (threshold: {threshold:.1f}%)', MetricType.RECOVERY_RATE: f'Recovery rate is {value:.1f}% (threshold: {threshold:.1f}%)'}
        return Alert(id=alert_id, level=level, metric_type=metric_type, message=messages.get(metric_type, f'{metric_type.value} threshold exceeded'), timestamp=metrics.timestamp, threshold_value=threshold, current_value=value, context=metrics.to_dict())
    def _should_send_alert(self, alert: Alert) -> bool:
        cooldown_key = f'{alert.metric_type.value}_{alert.level.value}'
        if cooldown_key in self.alert_cooldown:
            if datetime.now() - self.alert_cooldown[cooldown_key] < self.cooldown_period:
                return False
        self.alert_cooldown[cooldown_key] = datetime.now()
        return True
    def _process_alert(self, alert: Alert):
        self.active_alerts[alert.id] = alert
        self.alert_history.append(alert)
        if len(self.alert_history) > 1000:
            self.alert_history = self.alert_history[-1000:]
        for handler in self.notification_handlers:
            try:
                handler(alert)
            except Exception as e:
                logger.error(f'Alert notification handler failed: {e}')
        logger.warning(f'Alert generated: {alert.message}', extra={'alert': alert.to_dict()})
    def resolve_alert(self, alert_id: str):
        if alert_id in self.active_alerts:
            alert = self.active_alerts[alert_id]
            alert.resolved = True
            alert.resolution_time = datetime.now()
            del self.active_alerts[alert_id]
            logger.info(f'Alert resolved: {alert.message}')
    def get_active_alerts(self) -> List[Alert]:
        return list(self.active_alerts.values())
    def get_alert_history(self, limit: int=100) -> List[Alert]:
        return self.alert_history[-limit:]
class MonitoringDashboard:
    def __init__(self, metrics_collector: MetricsCollector, alert_manager: AlertManager):
        self.metrics_collector = metrics_collector
        self.alert_manager = alert_manager
        self.dashboard_data_cache = {}
        self.cache_ttl = 10
        self.last_cache_update = 0
    def get_dashboard_data(self) -> Dict[str, Any]:
        current_time = time.time()
        if current_time - self.last_cache_update < self.cache_ttl:
            return self.dashboard_data_cache
        metrics = self.metrics_collector.get_current_metrics()
        recovery_stats = error_recovery_service.get_recovery_stats()
        system_health = error_aggregator.get_system_health_status()
        dashboard_data = {'timestamp': datetime.now().isoformat(), 'system_status': self._get_system_status(metrics), 'metrics': metrics.to_dict(), 'alerts': {'active': [alert.to_dict() for alert in self.alert_manager.get_active_alerts()], 'recent': [alert.to_dict() for alert in self.alert_manager.get_alert_history(20)]}, 'recovery_stats': recovery_stats, 'system_health': system_health, 'uptime_info': self._get_uptime_info(metrics), 'performance_summary': self._get_performance_summary(metrics)}
        self.dashboard_data_cache = dashboard_data
        self.last_cache_update = current_time
        return dashboard_data
    def _get_system_status(self, metrics: MonitoringMetrics) -> str:
        if metrics.circuit_breaker_open:
            return 'degraded'
        elif metrics.error_rate > 0.1:
            return 'unhealthy'
        elif metrics.error_rate > 0.05:
            return 'warning'
        elif metrics.availability_percent < 99.0:
            return 'warning'
        else:
            return 'healthy'
    def _get_uptime_info(self, metrics: MonitoringMetrics) -> Dict[str, Any]:
        return {'availability_percent': metrics.availability_percent, 'uptime_target': 99.9, 'meets_sla': metrics.availability_percent >= 99.9, 'error_budget_remaining': max(0, 99.9 - metrics.error_rate * 100)}
    def _get_performance_summary(self, metrics: MonitoringMetrics) -> Dict[str, Any]:
        return {'avg_response_time_ms': metrics.avg_response_time_ms, 'throughput_rps': metrics.throughput_rps, 'active_requests': metrics.active_requests, 'error_rate': metrics.error_rate, 'recovery_success_rate': metrics.recovery_success_rate, 'performance_grade': self._calculate_performance_grade(metrics)}
    def _calculate_performance_grade(self, metrics: MonitoringMetrics) -> str:
        score = 0
        if metrics.availability_percent >= 99.9:
            score += 40
        elif metrics.availability_percent >= 99.5:
            score += 35
        elif metrics.availability_percent >= 99.0:
            score += 30
        elif metrics.availability_percent >= 95.0:
            score += 20
        if metrics.avg_response_time_ms <= 100:
            score += 30
        elif metrics.avg_response_time_ms <= 500:
            score += 25
        elif metrics.avg_response_time_ms <= 1000:
            score += 20
        elif metrics.avg_response_time_ms <= 2000:
            score += 15
        elif metrics.avg_response_time_ms <= 5000:
            score += 10
        if metrics.error_rate <= 0.01:
            score += 20
        elif metrics.error_rate <= 0.05:
            score += 15
        elif metrics.error_rate <= 0.1:
            score += 10
        elif metrics.error_rate <= 0.2:
            score += 5
        if metrics.recovery_success_rate >= 95:
            score += 10
        elif metrics.recovery_success_rate >= 90:
            score += 8
        elif metrics.recovery_success_rate >= 80:
            score += 6
        if score >= 90:
            return 'A+'
        elif score >= 85:
            return 'A'
        elif score >= 80:
            return 'A-'
        elif score >= 75:
            return 'B+'
        elif score >= 70:
            return 'B'
        elif score >= 65:
            return 'B-'
        elif score >= 60:
            return 'C+'
        elif score >= 55:
            return 'C'
        else:
            return 'F'
metrics_collector = MetricsCollector()
alert_manager = AlertManager()
monitoring_dashboard = MonitoringDashboard(metrics_collector, alert_manager)
def log_alert_handler(alert: Alert):
    logger.error(f'ALERT [{alert.level.value.upper()}]: {alert.message}', extra={'alert_data': alert.to_dict()})
def console_alert_handler(alert: Alert):
    print(f'\n🚨 ALERT [{alert.level.value.upper()}] 🚨')
    print(f'Type: {alert.metric_type.value}')
    print(f'Message: {alert.message}')
    print(f'Threshold: {alert.threshold_value}')
    print(f'Current: {alert.current_value}')
    print(f'Time: {alert.timestamp}')
    print('-' * 50)
alert_manager.add_notification_handler(log_alert_handler)
class MonitoringMiddleware:
    def __init__(self, metrics_collector: MetricsCollector, alert_manager: AlertManager):
        self.metrics_collector = metrics_collector
        self.alert_manager = alert_manager
        self.monitoring_task = None
    async def start_monitoring(self):
        if self.monitoring_task is None:
            self.monitoring_task = asyncio.create_task(self._monitoring_loop())
    async def stop_monitoring(self):
        if self.monitoring_task:
            self.monitoring_task.cancel()
            try:
                await self.monitoring_task
            except asyncio.CancelledError:
                pass
            self.monitoring_task = None
    async def _monitoring_loop(self):
        try:
            while True:
                metrics = self.metrics_collector.get_current_metrics()
                alerts = self.alert_manager.check_metrics(metrics)
                if len(alerts) > 0:
                    logger.info(f'Generated {len(alerts)} alerts in monitoring cycle')
                await asyncio.sleep(30)
        except asyncio.CancelledError:
            logger.info('Monitoring loop cancelled')
        except Exception as e:
            logger.error(f'Monitoring loop error: {e}', exc_info=True)
monitoring_middleware = MonitoringMiddleware(metrics_collector, alert_manager)
async def start_monitoring():
    await monitoring_middleware.start_monitoring()
    logger.info('DTESN monitoring system started')
async def stop_monitoring():
    await monitoring_middleware.stop_monitoring()
    logger.info('DTESN monitoring system stopped')