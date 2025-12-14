"""
Error monitoring and alerting system for Deep Tree Echo endpoints.

Implements real-time error monitoring, alerting, and dashboard capabilities
to support 99.9% uptime requirements and proactive incident response.
"""

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
    """Alert severity levels."""
    INFO = "info"
    WARNING = "warning"
    CRITICAL = "critical"
    EMERGENCY = "emergency"


class MetricType(Enum):
    """Types of metrics to monitor."""
    ERROR_RATE = "error_rate"
    RESPONSE_TIME = "response_time"
    AVAILABILITY = "availability"
    THROUGHPUT = "throughput"
    RECOVERY_RATE = "recovery_rate"
    CIRCUIT_BREAKER_STATE = "circuit_breaker_state"


@dataclass
class Alert:
    """Alert data structure."""
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
        """Convert alert to dictionary for JSON serialization."""
        return {
            "id": self.id,
            "level": self.level.value,
            "metric_type": self.metric_type.value,
            "message": self.message,
            "timestamp": self.timestamp.isoformat(),
            "threshold_value": self.threshold_value,
            "current_value": self.current_value,
            "context": self.context,
            "resolved": self.resolved,
            "resolution_time": self.resolution_time.isoformat() if self.resolution_time else None
        }


@dataclass
class MonitoringMetrics:
    """Current monitoring metrics."""
    timestamp: datetime
    error_rate: float = 0.0
    avg_response_time_ms: float = 0.0
    availability_percent: float = 100.0
    throughput_rps: float = 0.0
    recovery_success_rate: float = 100.0
    active_requests: int = 0
    circuit_breaker_open: bool = False
    
    # Error breakdown by category
    validation_errors: int = 0
    processing_errors: int = 0
    resource_errors: int = 0
    system_errors: int = 0
    
    # Recovery metrics
    successful_recoveries: int = 0
    failed_recoveries: int = 0
    fallback_activations: int = 0
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert metrics to dictionary."""
        return {
            "timestamp": self.timestamp.isoformat(),
            "error_rate": self.error_rate,
            "avg_response_time_ms": self.avg_response_time_ms,
            "availability_percent": self.availability_percent,
            "throughput_rps": self.throughput_rps,
            "recovery_success_rate": self.recovery_success_rate,
            "active_requests": self.active_requests,
            "circuit_breaker_open": self.circuit_breaker_open,
            "error_breakdown": {
                "validation_errors": self.validation_errors,
                "processing_errors": self.processing_errors,
                "resource_errors": self.resource_errors,
                "system_errors": self.system_errors
            },
            "recovery_metrics": {
                "successful_recoveries": self.successful_recoveries,
                "failed_recoveries": self.failed_recoveries,
                "fallback_activations": self.fallback_activations
            }
        }


class ThresholdConfig:
    """Configuration for monitoring thresholds."""
    
    def __init__(self):
        self.thresholds = {
            MetricType.ERROR_RATE: {
                AlertLevel.WARNING: 0.05,  # 5% error rate
                AlertLevel.CRITICAL: 0.10,  # 10% error rate
                AlertLevel.EMERGENCY: 0.20  # 20% error rate
            },
            MetricType.RESPONSE_TIME: {
                AlertLevel.WARNING: 1000.0,  # 1 second
                AlertLevel.CRITICAL: 5000.0,  # 5 seconds
                AlertLevel.EMERGENCY: 10000.0  # 10 seconds
            },
            MetricType.AVAILABILITY: {
                AlertLevel.WARNING: 99.5,  # Below 99.5%
                AlertLevel.CRITICAL: 99.0,  # Below 99%
                AlertLevel.EMERGENCY: 95.0  # Below 95%
            },
            MetricType.RECOVERY_RATE: {
                AlertLevel.WARNING: 95.0,  # Below 95% recovery success
                AlertLevel.CRITICAL: 90.0,  # Below 90% recovery success
                AlertLevel.EMERGENCY: 80.0  # Below 80% recovery success
            }
        }
    
    def get_alert_level(self, metric_type: MetricType, value: float) -> Optional[AlertLevel]:
        """Get alert level for metric value."""
        if metric_type not in self.thresholds:
            return None
        
        thresholds = self.thresholds[metric_type]
        
        # For availability and recovery rate, lower values are worse
        if metric_type in [MetricType.AVAILABILITY, MetricType.RECOVERY_RATE]:
            if value <= thresholds[AlertLevel.EMERGENCY]:
                return AlertLevel.EMERGENCY
            elif value <= thresholds[AlertLevel.CRITICAL]:
                return AlertLevel.CRITICAL
            elif value <= thresholds[AlertLevel.WARNING]:
                return AlertLevel.WARNING
        else:
            # For error rate and response time, higher values are worse
            if value >= thresholds[AlertLevel.EMERGENCY]:
                return AlertLevel.EMERGENCY
            elif value >= thresholds[AlertLevel.CRITICAL]:
                return AlertLevel.CRITICAL
            elif value >= thresholds[AlertLevel.WARNING]:
                return AlertLevel.WARNING
        
        return None


class MetricsCollector:
    """Collects and aggregates monitoring metrics."""
    
    def __init__(self, window_size: int = 300):  # 5 minute window
        self.window_size = window_size
        self.request_times = deque(maxlen=1000)
        self.error_events = deque(maxlen=1000)
        self.success_events = deque(maxlen=1000)
        self.recovery_events = deque(maxlen=1000)
        
        # Real-time counters
        self.active_requests = 0
        self.total_requests = 0
        self.total_errors = 0
        
        # Error breakdown
        self.error_counts = defaultdict(int)
        
    def record_request_start(self) -> str:
        """Record start of a request."""
        request_id = f"req_{int(time.time() * 1000000)}"
        self.active_requests += 1
        self.total_requests += 1
        return request_id
    
    def record_request_end(self, request_id: str, success: bool, response_time_ms: float):
        """Record end of a request."""
        self.active_requests = max(0, self.active_requests - 1)
        
        timestamp = datetime.now()
        self.request_times.append((timestamp, response_time_ms))
        
        if success:
            self.success_events.append(timestamp)
        else:
            self.error_events.append(timestamp)
            self.total_errors += 1
    
    def record_error(self, error: DTESNError):
        """Record an error event."""
        self.error_counts[error.category.value] += 1
        
    def record_recovery(self, success: bool, recovery_mode: str):
        """Record a recovery attempt."""
        self.recovery_events.append({
            "timestamp": datetime.now(),
            "success": success,
            "mode": recovery_mode
        })
    
    def get_current_metrics(self) -> MonitoringMetrics:
        """Get current monitoring metrics."""
        now = datetime.now()
        window_start = now - timedelta(seconds=self.window_size)
        
        # Filter events to current window
        recent_requests = [
            (ts, rt) for ts, rt in self.request_times
            if ts > window_start
        ]
        
        recent_errors = [ts for ts in self.error_events if ts > window_start]
        recent_successes = [ts for ts in self.success_events if ts > window_start]
        recent_recoveries = [
            evt for evt in self.recovery_events
            if evt["timestamp"] > window_start
        ]
        
        # Calculate metrics
        total_recent = len(recent_requests)
        error_count = len(recent_errors)
        success_count = len(recent_successes)
        
        error_rate = error_count / max(total_recent, 1)
        availability = (success_count / max(total_recent, 1)) * 100
        throughput = total_recent / self.window_size  # requests per second
        
        # Response time metrics
        avg_response_time = 0.0
        if recent_requests:
            avg_response_time = sum(rt for _, rt in recent_requests) / len(recent_requests)
        
        # Recovery metrics
        successful_recoveries = sum(1 for evt in recent_recoveries if evt["success"])
        total_recoveries = len(recent_recoveries)
        recovery_rate = (successful_recoveries / max(total_recoveries, 1)) * 100
        
        # Get system health from error aggregator
        system_health = error_aggregator.get_system_health_status()
        
        return MonitoringMetrics(
            timestamp=now,
            error_rate=error_rate,
            avg_response_time_ms=avg_response_time,
            availability_percent=availability,
            throughput_rps=throughput,
            recovery_success_rate=recovery_rate,
            active_requests=self.active_requests,
            circuit_breaker_open=system_health.get("should_circuit_break", False),
            validation_errors=self.error_counts.get("validation", 0),
            processing_errors=self.error_counts.get("processing", 0),
            resource_errors=self.error_counts.get("resource", 0),
            system_errors=self.error_counts.get("system", 0),
            successful_recoveries=successful_recoveries,
            failed_recoveries=total_recoveries - successful_recoveries,
            fallback_activations=sum(1 for evt in recent_recoveries if "fallback" in evt["mode"])
        )


class AlertManager:
    """Manages alerts and notifications."""
    
    def __init__(self, threshold_config: Optional[ThresholdConfig] = None):
        self.threshold_config = threshold_config or ThresholdConfig()
        self.active_alerts: Dict[str, Alert] = {}
        self.alert_history: List[Alert] = []
        self.notification_handlers: List[Callable[[Alert], None]] = []
        
        # Alert suppression to prevent spam
        self.alert_cooldown: Dict[str, datetime] = {}
        self.cooldown_period = timedelta(minutes=5)
        
    def add_notification_handler(self, handler: Callable[[Alert], None]):
        """Add a notification handler for alerts."""
        self.notification_handlers.append(handler)
    
    def check_metrics(self, metrics: MonitoringMetrics) -> List[Alert]:
        """Check metrics against thresholds and generate alerts."""
        alerts = []
        
        # Check each metric type
        metric_checks = [
            (MetricType.ERROR_RATE, metrics.error_rate),
            (MetricType.RESPONSE_TIME, metrics.avg_response_time_ms),
            (MetricType.AVAILABILITY, metrics.availability_percent),
            (MetricType.RECOVERY_RATE, metrics.recovery_success_rate)
        ]
        
        for metric_type, value in metric_checks:
            alert_level = self.threshold_config.get_alert_level(metric_type, value)
            
            if alert_level:
                alert = self._create_alert(metric_type, alert_level, value, metrics)
                if alert and self._should_send_alert(alert):
                    alerts.append(alert)
                    self._process_alert(alert)
        
        # Check for circuit breaker alerts
        if metrics.circuit_breaker_open:
            alert = Alert(
                id=f"circuit_breaker_{int(time.time())}",
                level=AlertLevel.CRITICAL,
                metric_type=MetricType.CIRCUIT_BREAKER_STATE,
                message="Circuit breaker is OPEN - service degraded",
                timestamp=metrics.timestamp,
                threshold_value=1.0,
                current_value=1.0,
                context={"circuit_breaker_open": True}
            )
            if self._should_send_alert(alert):
                alerts.append(alert)
                self._process_alert(alert)
        
        return alerts
    
    def _create_alert(
        self,
        metric_type: MetricType,
        level: AlertLevel,
        value: float,
        metrics: MonitoringMetrics
    ) -> Optional[Alert]:
        """Create an alert for the given metric."""
        alert_id = f"{metric_type.value}_{level.value}_{int(time.time())}"
        
        # Get threshold value
        threshold = self.threshold_config.thresholds[metric_type][level]
        
        # Create appropriate message
        messages = {
            MetricType.ERROR_RATE: f"Error rate is {value:.1%} (threshold: {threshold:.1%})",
            MetricType.RESPONSE_TIME: f"Response time is {value:.0f}ms (threshold: {threshold:.0f}ms)",
            MetricType.AVAILABILITY: f"Availability is {value:.1f}% (threshold: {threshold:.1f}%)",
            MetricType.RECOVERY_RATE: f"Recovery rate is {value:.1f}% (threshold: {threshold:.1f}%)"
        }
        
        return Alert(
            id=alert_id,
            level=level,
            metric_type=metric_type,
            message=messages.get(metric_type, f"{metric_type.value} threshold exceeded"),
            timestamp=metrics.timestamp,
            threshold_value=threshold,
            current_value=value,
            context=metrics.to_dict()
        )
    
    def _should_send_alert(self, alert: Alert) -> bool:
        """Check if alert should be sent (not in cooldown)."""
        cooldown_key = f"{alert.metric_type.value}_{alert.level.value}"
        
        if cooldown_key in self.alert_cooldown:
            if datetime.now() - self.alert_cooldown[cooldown_key] < self.cooldown_period:
                return False
        
        self.alert_cooldown[cooldown_key] = datetime.now()
        return True
    
    def _process_alert(self, alert: Alert):
        """Process and store an alert."""
        self.active_alerts[alert.id] = alert
        self.alert_history.append(alert)
        
        # Keep only last 1000 alerts in history
        if len(self.alert_history) > 1000:
            self.alert_history = self.alert_history[-1000:]
        
        # Send notifications
        for handler in self.notification_handlers:
            try:
                handler(alert)
            except Exception as e:
                logger.error(f"Alert notification handler failed: {e}")
        
        logger.warning(f"Alert generated: {alert.message}", extra={"alert": alert.to_dict()})
    
    def resolve_alert(self, alert_id: str):
        """Mark an alert as resolved."""
        if alert_id in self.active_alerts:
            alert = self.active_alerts[alert_id]
            alert.resolved = True
            alert.resolution_time = datetime.now()
            del self.active_alerts[alert_id]
            logger.info(f"Alert resolved: {alert.message}")
    
    def get_active_alerts(self) -> List[Alert]:
        """Get all active alerts."""
        return list(self.active_alerts.values())
    
    def get_alert_history(self, limit: int = 100) -> List[Alert]:
        """Get recent alert history."""
        return self.alert_history[-limit:]


class MonitoringDashboard:
    """Real-time monitoring dashboard data provider."""
    
    def __init__(self, metrics_collector: MetricsCollector, alert_manager: AlertManager):
        self.metrics_collector = metrics_collector
        self.alert_manager = alert_manager
        self.dashboard_data_cache = {}
        self.cache_ttl = 10  # seconds
        self.last_cache_update = 0
    
    def get_dashboard_data(self) -> Dict[str, Any]:
        """Get comprehensive dashboard data."""
        current_time = time.time()
        
        # Use cached data if recent enough
        if (current_time - self.last_cache_update) < self.cache_ttl:
            return self.dashboard_data_cache
        
        # Collect current metrics
        metrics = self.metrics_collector.get_current_metrics()
        
        # Get recovery service stats
        recovery_stats = error_recovery_service.get_recovery_stats()
        
        # Get system health
        system_health = error_aggregator.get_system_health_status()
        
        # Prepare dashboard data
        dashboard_data = {
            "timestamp": datetime.now().isoformat(),
            "system_status": self._get_system_status(metrics),
            "metrics": metrics.to_dict(),
            "alerts": {
                "active": [alert.to_dict() for alert in self.alert_manager.get_active_alerts()],
                "recent": [alert.to_dict() for alert in self.alert_manager.get_alert_history(20)]
            },
            "recovery_stats": recovery_stats,
            "system_health": system_health,
            "uptime_info": self._get_uptime_info(metrics),
            "performance_summary": self._get_performance_summary(metrics)
        }
        
        # Update cache
        self.dashboard_data_cache = dashboard_data
        self.last_cache_update = current_time
        
        return dashboard_data
    
    def _get_system_status(self, metrics: MonitoringMetrics) -> str:
        """Determine overall system status."""
        if metrics.circuit_breaker_open:
            return "degraded"
        elif metrics.error_rate > 0.10:  # 10% error rate
            return "unhealthy"
        elif metrics.error_rate > 0.05:  # 5% error rate
            return "warning"
        elif metrics.availability_percent < 99.0:
            return "warning"
        else:
            return "healthy"
    
    def _get_uptime_info(self, metrics: MonitoringMetrics) -> Dict[str, Any]:
        """Get uptime information."""
        return {
            "availability_percent": metrics.availability_percent,
            "uptime_target": 99.9,
            "meets_sla": metrics.availability_percent >= 99.9,
            "error_budget_remaining": max(0, 99.9 - metrics.error_rate * 100)
        }
    
    def _get_performance_summary(self, metrics: MonitoringMetrics) -> Dict[str, Any]:
        """Get performance summary."""
        return {
            "avg_response_time_ms": metrics.avg_response_time_ms,
            "throughput_rps": metrics.throughput_rps,
            "active_requests": metrics.active_requests,
            "error_rate": metrics.error_rate,
            "recovery_success_rate": metrics.recovery_success_rate,
            "performance_grade": self._calculate_performance_grade(metrics)
        }
    
    def _calculate_performance_grade(self, metrics: MonitoringMetrics) -> str:
        """Calculate overall performance grade."""
        score = 0
        
        # Availability score (40%)
        if metrics.availability_percent >= 99.9:
            score += 40
        elif metrics.availability_percent >= 99.5:
            score += 35
        elif metrics.availability_percent >= 99.0:
            score += 30
        elif metrics.availability_percent >= 95.0:
            score += 20
        
        # Response time score (30%)
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
        
        # Error rate score (20%)
        if metrics.error_rate <= 0.01:  # 1%
            score += 20
        elif metrics.error_rate <= 0.05:  # 5%
            score += 15
        elif metrics.error_rate <= 0.10:  # 10%
            score += 10
        elif metrics.error_rate <= 0.20:  # 20%
            score += 5
        
        # Recovery rate score (10%)
        if metrics.recovery_success_rate >= 95:
            score += 10
        elif metrics.recovery_success_rate >= 90:
            score += 8
        elif metrics.recovery_success_rate >= 80:
            score += 6
        
        # Grade mapping
        if score >= 90:
            return "A+"
        elif score >= 85:
            return "A"
        elif score >= 80:
            return "A-"
        elif score >= 75:
            return "B+"
        elif score >= 70:
            return "B"
        elif score >= 65:
            return "B-"
        elif score >= 60:
            return "C+"
        elif score >= 55:
            return "C"
        else:
            return "F"


# Global monitoring instances
metrics_collector = MetricsCollector()
alert_manager = AlertManager()
monitoring_dashboard = MonitoringDashboard(metrics_collector, alert_manager)


# Default notification handlers
def log_alert_handler(alert: Alert):
    """Default log-based alert handler."""
    logger.error(
        f"ALERT [{alert.level.value.upper()}]: {alert.message}",
        extra={"alert_data": alert.to_dict()}
    )


def console_alert_handler(alert: Alert):
    """Console-based alert handler for development."""
    print(f"\n🚨 ALERT [{alert.level.value.upper()}] 🚨")
    print(f"Type: {alert.metric_type.value}")
    print(f"Message: {alert.message}")
    print(f"Threshold: {alert.threshold_value}")
    print(f"Current: {alert.current_value}")
    print(f"Time: {alert.timestamp}")
    print("-" * 50)


# Register default handlers
alert_manager.add_notification_handler(log_alert_handler)


class MonitoringMiddleware:
    """Middleware integration for monitoring system."""
    
    def __init__(self, metrics_collector: MetricsCollector, alert_manager: AlertManager):
        self.metrics_collector = metrics_collector
        self.alert_manager = alert_manager
        self.monitoring_task = None
    
    async def start_monitoring(self):
        """Start background monitoring task."""
        if self.monitoring_task is None:
            self.monitoring_task = asyncio.create_task(self._monitoring_loop())
    
    async def stop_monitoring(self):
        """Stop background monitoring task."""
        if self.monitoring_task:
            self.monitoring_task.cancel()
            try:
                await self.monitoring_task
            except asyncio.CancelledError:
                pass
            self.monitoring_task = None
    
    async def _monitoring_loop(self):
        """Background monitoring loop."""
        try:
            while True:
                # Collect metrics and check for alerts
                metrics = self.metrics_collector.get_current_metrics()
                alerts = self.alert_manager.check_metrics(metrics)
                
                # Log monitoring status periodically
                if len(alerts) > 0:
                    logger.info(f"Generated {len(alerts)} alerts in monitoring cycle")
                
                # Wait before next check
                await asyncio.sleep(30)  # Check every 30 seconds
                
        except asyncio.CancelledError:
            logger.info("Monitoring loop cancelled")
        except Exception as e:
            logger.error(f"Monitoring loop error: {e}", exc_info=True)


# Global monitoring middleware instance
monitoring_middleware = MonitoringMiddleware(metrics_collector, alert_manager)


# Startup and shutdown hooks for monitoring
async def start_monitoring():
    """Start the monitoring system."""
    await monitoring_middleware.start_monitoring()
    logger.info("DTESN monitoring system started")


async def stop_monitoring():
    """Stop the monitoring system."""
    await monitoring_middleware.stop_monitoring()
    logger.info("DTESN monitoring system stopped")