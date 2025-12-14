"""
Performance Alerting System for Aphrodite Engine.

Comprehensive alerting system that integrates with backend monitoring
and provides real-time notifications for performance issues, regressions,
and system anomalies.
"""

import asyncio
import time
import json
from dataclasses import dataclass, asdict
from typing import Dict, List, Optional, Any, Callable, Set
from enum import Enum
import threading
from collections import defaultdict, deque

from loguru import logger

from .backend_monitor import AlertMessage
from .regression_detector import RegressionAlert, RegressionSeverity


class AlertChannel(Enum):
    """Alert delivery channels."""
    LOG = "log"
    WEBHOOK = "webhook"
    EMAIL = "email"
    SLACK = "slack" 
    DISCORD = "discord"
    FILE = "file"
    PROMETHEUS = "prometheus"


class AlertRule(Enum):
    """Alert rule types."""
    THRESHOLD = "threshold"
    TREND = "trend"
    REGRESSION = "regression"
    ANOMALY = "anomaly"
    CORRELATION = "correlation"


@dataclass
class AlertConfig:
    """Configuration for alert rules and channels."""
    enabled: bool = True
    severity_levels: List[str] = None
    channels: List[AlertChannel] = None
    rate_limit_minutes: int = 5
    escalation_minutes: int = 30
    auto_resolve_minutes: int = 60
    
    def __post_init__(self):
        if self.severity_levels is None:
            self.severity_levels = ["WARNING", "CRITICAL"]
        if self.channels is None:
            self.channels = [AlertChannel.LOG]


@dataclass
class AlertNotification:
    """Alert notification to be delivered."""
    alert_id: str
    timestamp: float
    alert_type: str  # "performance" or "regression"
    severity: str
    title: str
    message: str
    source_component: str
    channels: List[AlertChannel]
    metadata: Dict[str, Any]
    
    # Delivery tracking
    delivery_attempts: Dict[AlertChannel, int] = None
    delivered_channels: Set[AlertChannel] = None
    failed_channels: Set[AlertChannel] = None
    
    def __post_init__(self):
        if self.delivery_attempts is None:
            self.delivery_attempts = {}
        if self.delivered_channels is None:
            self.delivered_channels = set()
        if self.failed_channels is None:
            self.failed_channels = set()


class PerformanceAlertingSystem:
    """
    Comprehensive performance alerting system with multiple delivery channels
    and intelligent alert management.
    
    Features:
    - Multi-channel alert delivery (logs, webhooks, files, etc.)
    - Rate limiting and alert deduplication
    - Alert escalation and auto-resolution
    - Integration with regression detection
    - Custom alert rules and templates
    - Alert correlation and grouping
    """
    
    def __init__(
        self,
        default_config: Optional[AlertConfig] = None,
        enable_alert_correlation: bool = True,
        max_alert_history: int = 10000
    ):
        self.default_config = default_config or AlertConfig()
        self.enable_alert_correlation = enable_alert_correlation
        self.max_alert_history = max_alert_history
        
        # Alert management
        self.alert_notifications: List[AlertNotification] = []
        self.active_alerts: Dict[str, AlertNotification] = {}
        self.resolved_alerts: Dict[str, AlertNotification] = {}
        
        # Alert rules and configurations
        self.alert_configs: Dict[str, AlertConfig] = {}
        self.custom_rules: Dict[str, Callable[[Any], bool]] = {}
        
        # Delivery channels
        self.channel_handlers: Dict[AlertChannel, Callable] = {}
        self._setup_default_handlers()
        
        # Rate limiting
        self.rate_limits: Dict[str, deque] = defaultdict(lambda: deque(maxlen=100))
        
        # Alert correlation
        self.correlation_windows: Dict[str, List[AlertNotification]] = defaultdict(list)
        
        # Background processing
        self.is_active = False
        self.processor_thread: Optional[threading.Thread] = None
        
        logger.info("Performance Alerting System initialized")
    
    def _setup_default_handlers(self):
        """Setup default alert delivery handlers."""
        # Log handler
        def log_handler(notification: AlertNotification) -> bool:
            log_level = {
                "INFO": logger.info,
                "WARNING": logger.warning,
                "CRITICAL": logger.error
            }.get(notification.severity, logger.info)
            
            log_level(f"Alert [{notification.severity}] {notification.title}: {notification.message}")
            return True
        
        # File handler
        def file_handler(notification: AlertNotification) -> bool:
            try:
                alert_data = {
                    "id": notification.alert_id,
                    "timestamp": notification.timestamp,
                    "type": notification.alert_type,
                    "severity": notification.severity,
                    "title": notification.title,
                    "message": notification.message,
                    "component": notification.source_component,
                    "metadata": notification.metadata
                }
                
                # Write to alerts file (append mode)
                with open("/tmp/aphrodite_alerts.jsonl", "a") as f:
                    f.write(json.dumps(alert_data) + "\n")
                
                return True
                
            except Exception as e:
                logger.error(f"Failed to write alert to file: {e}")
                return False
        
        # Webhook handler (placeholder)
        def webhook_handler(notification: AlertNotification) -> bool:
            # TODO: Implement webhook delivery
            logger.debug(f"Webhook delivery not implemented for alert: {notification.alert_id}")
            return False
        
        # Register default handlers
        self.channel_handlers[AlertChannel.LOG] = log_handler
        self.channel_handlers[AlertChannel.FILE] = file_handler
        self.channel_handlers[AlertChannel.WEBHOOK] = webhook_handler
    
    def start(self):
        """Start the alerting system background processing."""
        if self.is_active:
            logger.warning("Alerting system already active")
            return
        
        self.is_active = True
        self.processor_thread = threading.Thread(
            target=self._processing_loop,
            name="AlertingProcessor",
            daemon=True
        )
        self.processor_thread.start()
        logger.info("Alerting system started")
    
    def stop(self):
        """Stop the alerting system."""
        if not self.is_active:
            return
        
        self.is_active = False
        if self.processor_thread and self.processor_thread.is_alive():
            self.processor_thread.join(timeout=5.0)
        logger.info("Alerting system stopped")
    
    def process_performance_alert(self, alert: AlertMessage):
        """Process a performance alert from the backend monitor."""
        if not self._should_process_alert(alert.metric_name, alert.severity):
            return
        
        # Create notification
        notification = AlertNotification(
            alert_id=f"perf_{alert.metric_name}_{int(alert.timestamp)}",
            timestamp=alert.timestamp,
            alert_type="performance",
            severity=alert.severity,
            title=f"Performance Alert: {alert.metric_name}",
            message=alert.message,
            source_component=alert.component,
            channels=self._get_alert_channels(alert.severity),
            metadata={
                "metric_name": alert.metric_name,
                "current_value": alert.current_value,
                "threshold": alert.threshold,
                "component": alert.component
            }
        )
        
        self._queue_notification(notification)
    
    def process_regression_alert(self, regression: RegressionAlert):
        """Process a regression alert from the regression detector."""
        severity = self._map_regression_severity(regression.severity)
        
        if not self._should_process_alert(regression.metric_name, severity):
            return
        
        # Create notification
        notification = AlertNotification(
            alert_id=f"regr_{regression.metric_name}_{int(regression.timestamp)}",
            timestamp=regression.timestamp,
            alert_type="regression",
            severity=severity,
            title=f"Performance Regression: {regression.metric_name}",
            message=regression.message,
            source_component="regression_detector",
            channels=self._get_alert_channels(severity),
            metadata={
                "metric_name": regression.metric_name,
                "current_value": regression.current_value,
                "baseline_value": regression.baseline_value,
                "regression_percent": regression.regression_percent,
                "confidence": regression.confidence,
                "trend_slope": regression.trend_slope,
                "context": regression.context
            }
        )
        
        self._queue_notification(notification)
    
    def _should_process_alert(self, metric_name: str, severity: str) -> bool:
        """Check if alert should be processed based on configuration and rate limits."""
        # Get configuration for this alert
        config = self.alert_configs.get(metric_name, self.default_config)
        
        if not config.enabled:
            return False
        
        if severity not in config.severity_levels:
            return False
        
        # Check rate limiting
        if self._is_rate_limited(metric_name, config.rate_limit_minutes):
            return False
        
        return True
    
    def _is_rate_limited(self, metric_name: str, rate_limit_minutes: int) -> bool:
        """Check if alert is rate limited."""
        if rate_limit_minutes <= 0:
            return False
        
        current_time = time.time()
        cutoff_time = current_time - (rate_limit_minutes * 60)
        
        # Get recent alerts for this metric
        recent_alerts = self.rate_limits[metric_name]
        
        # Remove old entries
        while recent_alerts and recent_alerts[0] < cutoff_time:
            recent_alerts.popleft()
        
        # Check if we've exceeded the rate limit (1 alert per period)
        if len(recent_alerts) > 0:
            return True
        
        # Add current alert timestamp
        recent_alerts.append(current_time)
        return False
    
    def _get_alert_channels(self, severity: str) -> List[AlertChannel]:
        """Get appropriate delivery channels based on severity."""
        if severity == "CRITICAL":
            return [AlertChannel.LOG, AlertChannel.FILE, AlertChannel.WEBHOOK]
        elif severity == "WARNING":
            return [AlertChannel.LOG, AlertChannel.FILE]
        else:
            return [AlertChannel.LOG]
    
    def _map_regression_severity(self, regression_severity: RegressionSeverity) -> str:
        """Map regression severity to alert severity."""
        mapping = {
            RegressionSeverity.MINOR: "INFO",
            RegressionSeverity.MODERATE: "WARNING", 
            RegressionSeverity.SEVERE: "WARNING",
            RegressionSeverity.CRITICAL: "CRITICAL"
        }
        return mapping.get(regression_severity, "WARNING")
    
    def _queue_notification(self, notification: AlertNotification):
        """Queue notification for processing."""
        # Check for correlation with existing alerts
        if self.enable_alert_correlation:
            correlated = self._find_correlated_alerts(notification)
            if correlated:
                # Update existing correlated alert instead of creating new one
                self._update_correlated_alert(notification, correlated)
                return
        
        # Add to notification queue
        self.alert_notifications.append(notification)
        
        # Maintain queue size
        if len(self.alert_notifications) > self.max_alert_history:
            self.alert_notifications = self.alert_notifications[-self.max_alert_history//2:]
        
        logger.debug(f"Queued alert notification: {notification.alert_id}")
    
    def _find_correlated_alerts(self, notification: AlertNotification) -> Optional[AlertNotification]:
        """Find existing correlated alerts that can be grouped."""
        correlation_window = 300  # 5 minutes
        current_time = notification.timestamp
        
        for existing_id, existing_alert in self.active_alerts.items():
            if current_time - existing_alert.timestamp > correlation_window:
                continue
            
            # Check correlation criteria
            if (existing_alert.source_component == notification.source_component and
                existing_alert.severity == notification.severity and
                self._are_alerts_correlated(existing_alert, notification)):
                return existing_alert
        
        return None
    
    def _are_alerts_correlated(self, alert1: AlertNotification, alert2: AlertNotification) -> bool:
        """Check if two alerts are correlated and should be grouped."""
        # Simple correlation based on component and metric similarity
        if alert1.alert_type != alert2.alert_type:
            return False
        
        # Check if metrics are related (e.g., both CPU/memory, both latency-related)
        related_groups = [
            {"cpu_usage", "memory_usage"},
            {"request_latency_p95", "request_latency_p99", "token_throughput"},
            {"aar_agents_active", "dtesn_processing_rate", "echo_self_evolution_score"}
        ]
        
        metric1 = alert1.metadata.get("metric_name", "")
        metric2 = alert2.metadata.get("metric_name", "")
        
        for group in related_groups:
            if metric1 in group and metric2 in group:
                return True
        
        return False
    
    def _update_correlated_alert(self, new_alert: AlertNotification, existing_alert: AlertNotification):
        """Update existing correlated alert with new information."""
        # Increment correlation count
        if "correlation_count" not in existing_alert.metadata:
            existing_alert.metadata["correlation_count"] = 1
        existing_alert.metadata["correlation_count"] += 1
        
        # Update message to reflect correlation
        count = existing_alert.metadata["correlation_count"]
        existing_alert.message += f" (correlated with {count} similar alerts)"
        
        # Update timestamp to latest
        existing_alert.timestamp = new_alert.timestamp
        
        logger.debug(f"Updated correlated alert: {existing_alert.alert_id} (count: {count})")
    
    def _processing_loop(self):
        """Main processing loop for alert delivery and management."""
        while self.is_active:
            try:
                current_time = time.time()
                
                # Process pending notifications
                self._process_pending_notifications()
                
                # Check for alert auto-resolution
                self._check_alert_resolution(current_time)
                
                # Check for alert escalation
                self._check_alert_escalation(current_time)
                
                # Sleep before next iteration
                time.sleep(5.0)  # Process every 5 seconds
                
            except Exception as e:
                logger.error(f"Error in alerting processing loop: {e}")
                time.sleep(5.0)
    
    def _process_pending_notifications(self):
        """Process all pending notifications."""
        # Process notifications that haven't been fully delivered
        for notification in list(self.alert_notifications):
            if len(notification.delivered_channels) < len(notification.channels):
                self._deliver_notification(notification)
            
            # Move to active alerts if delivered to at least one channel
            if notification.delivered_channels and notification.alert_id not in self.active_alerts:
                self.active_alerts[notification.alert_id] = notification
    
    def _deliver_notification(self, notification: AlertNotification):
        """Deliver notification to all configured channels."""
        for channel in notification.channels:
            if channel in notification.delivered_channels:
                continue  # Already delivered
            
            if channel in notification.failed_channels:
                # Retry failed deliveries (with backoff)
                attempts = notification.delivery_attempts.get(channel, 0)
                if attempts >= 3:  # Max 3 retry attempts
                    continue
            
            # Attempt delivery
            handler = self.channel_handlers.get(channel)
            if not handler:
                logger.warning(f"No handler registered for channel: {channel}")
                continue
            
            try:
                success = handler(notification)
                notification.delivery_attempts[channel] = notification.delivery_attempts.get(channel, 0) + 1
                
                if success:
                    notification.delivered_channels.add(channel)
                    notification.failed_channels.discard(channel)
                    logger.debug(f"Alert {notification.alert_id} delivered to {channel}")
                else:
                    notification.failed_channels.add(channel)
                    logger.warning(f"Failed to deliver alert {notification.alert_id} to {channel}")
                    
            except Exception as e:
                notification.failed_channels.add(channel)
                notification.delivery_attempts[channel] = notification.delivery_attempts.get(channel, 0) + 1
                logger.error(f"Error delivering alert {notification.alert_id} to {channel}: {e}")
    
    def _check_alert_resolution(self, current_time: float):
        """Check for alerts that should be auto-resolved."""
        auto_resolve_seconds = self.default_config.auto_resolve_minutes * 60
        
        for alert_id, alert in list(self.active_alerts.items()):
            if current_time - alert.timestamp > auto_resolve_seconds:
                self._resolve_alert(alert_id, "auto_resolved")
    
    def _check_alert_escalation(self, current_time: float):
        """Check for alerts that need escalation."""
        escalation_seconds = self.default_config.escalation_minutes * 60
        
        for alert_id, alert in self.active_alerts.items():
            if (current_time - alert.timestamp > escalation_seconds and 
                not alert.metadata.get("escalated", False)):
                self._escalate_alert(alert)
    
    def _escalate_alert(self, alert: AlertNotification):
        """Escalate an unresolved alert."""
        alert.metadata["escalated"] = True
        alert.metadata["escalation_timestamp"] = time.time()
        
        # Create escalation notification
        escalation = AlertNotification(
            alert_id=f"escalation_{alert.alert_id}",
            timestamp=time.time(),
            alert_type=alert.alert_type,
            severity="CRITICAL",  # Escalate to critical
            title=f"ESCALATED: {alert.title}",
            message=f"Alert has not been resolved: {alert.message}",
            source_component=alert.source_component,
            channels=[AlertChannel.LOG, AlertChannel.WEBHOOK],  # Use higher priority channels
            metadata={**alert.metadata, "escalated_from": alert.alert_id}
        )
        
        self._queue_notification(escalation)
        logger.warning(f"Alert escalated: {alert.alert_id}")
    
    def _resolve_alert(self, alert_id: str, resolution_type: str):
        """Resolve an active alert."""
        if alert_id not in self.active_alerts:
            return
        
        alert = self.active_alerts.pop(alert_id)
        alert.metadata["resolved"] = True
        alert.metadata["resolution_type"] = resolution_type
        alert.metadata["resolution_timestamp"] = time.time()
        
        self.resolved_alerts[alert_id] = alert
        
        logger.info(f"Alert resolved: {alert_id} ({resolution_type})")
    
    def register_channel_handler(self, channel: AlertChannel, handler: Callable[[AlertNotification], bool]):
        """Register a custom channel handler."""
        self.channel_handlers[channel] = handler
        logger.info(f"Channel handler registered: {channel}")
    
    def register_alert_config(self, metric_name: str, config: AlertConfig):
        """Register custom alert configuration for a metric."""
        self.alert_configs[metric_name] = config
        logger.info(f"Alert config registered for metric: {metric_name}")
    
    def get_alert_status(self) -> Dict[str, Any]:
        """Get comprehensive alerting system status."""
        current_time = time.time()
        
        # Calculate alert statistics
        recent_alerts = [
            a for a in self.alert_notifications 
            if current_time - a.timestamp <= 3600  # Last hour
        ]
        
        severity_counts = defaultdict(int)
        type_counts = defaultdict(int)
        
        for alert in recent_alerts:
            severity_counts[alert.severity] += 1
            type_counts[alert.alert_type] += 1
        
        return {
            "system_status": "active" if self.is_active else "inactive",
            "active_alerts": len(self.active_alerts),
            "total_notifications": len(self.alert_notifications),
            "recent_alerts_1h": {
                "total": len(recent_alerts),
                "by_severity": dict(severity_counts),
                "by_type": dict(type_counts)
            },
            "resolved_alerts": len(self.resolved_alerts),
            "channel_handlers": list(self.channel_handlers.keys()),
            "configurations": len(self.alert_configs),
            "rate_limit_entries": len(self.rate_limits)
        }
    
    def get_active_alerts(self) -> List[Dict[str, Any]]:
        """Get list of currently active alerts."""
        return [
            {
                "alert_id": alert.alert_id,
                "timestamp": alert.timestamp,
                "type": alert.alert_type,
                "severity": alert.severity,
                "title": alert.title,
                "message": alert.message,
                "component": alert.source_component,
                "delivered_channels": list(alert.delivered_channels),
                "failed_channels": list(alert.failed_channels),
                "metadata": alert.metadata
            }
            for alert in self.active_alerts.values()
        ]
    
    def resolve_alert_manually(self, alert_id: str) -> bool:
        """Manually resolve an active alert."""
        if alert_id in self.active_alerts:
            self._resolve_alert(alert_id, "manual")
            return True
        return False
    
    def clear_resolved_alerts(self, older_than_hours: int = 24):
        """Clear resolved alerts older than specified hours."""
        cutoff_time = time.time() - (older_than_hours * 3600)
        
        cleared_count = 0
        for alert_id, alert in list(self.resolved_alerts.items()):
            resolution_time = alert.metadata.get("resolution_timestamp", alert.timestamp)
            if resolution_time < cutoff_time:
                del self.resolved_alerts[alert_id]
                cleared_count += 1
        
        logger.info(f"Cleared {cleared_count} resolved alerts")


# Factory function for easy integration
def create_alerting_system(**kwargs) -> PerformanceAlertingSystem:
    """Create a PerformanceAlertingSystem with default configuration."""
    return PerformanceAlertingSystem(**kwargs)