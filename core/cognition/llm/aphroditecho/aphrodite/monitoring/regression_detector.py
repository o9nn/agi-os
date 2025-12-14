"""
Performance Regression Detector for Aphrodite Engine.

Advanced performance regression detection system that monitors performance 
trends and automatically detects degradations in model performance, especially
during model updates and Deep Tree Echo evolution cycles.
"""

import time
import statistics
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, Tuple, Callable
from enum import Enum
import threading
from collections import deque, defaultdict

import numpy as np
from loguru import logger

from .backend_monitor import BackendMetrics, AlertMessage


class RegressionSeverity(Enum):
    """Severity levels for performance regressions."""
    MINOR = "minor"
    MODERATE = "moderate"  
    SEVERE = "severe"
    CRITICAL = "critical"


@dataclass
class RegressionAlert:
    """Performance regression alert."""
    timestamp: float
    metric_name: str
    severity: RegressionSeverity
    current_value: float
    baseline_value: float
    regression_percent: float
    trend_slope: float
    confidence: float
    message: str
    context: Dict[str, Any] = field(default_factory=dict)


@dataclass  
class PerformanceBaseline:
    """Performance baseline for regression detection."""
    metric_name: str
    baseline_value: float
    window_size: int
    values: deque = field(default_factory=deque)
    last_updated: float = 0.0
    update_count: int = 0
    
    def update(self, value: float):
        """Update baseline with new value."""
        self.values.append(value)
        if len(self.values) > self.window_size:
            self.values.popleft()
        
        # Recalculate baseline
        if len(self.values) >= self.window_size // 2:
            self.baseline_value = statistics.median(self.values)
        
        self.last_updated = time.time()
        self.update_count += 1


class PerformanceRegressionDetector:
    """
    Advanced performance regression detection system.
    
    Features:
    - Statistical trend analysis for regression detection
    - Model update correlation tracking
    - Deep Tree Echo evolution impact analysis
    - Configurable sensitivity levels
    - Multi-metric correlation analysis
    - Automated baseline establishment
    """
    
    def __init__(
        self,
        baseline_window_size: int = 100,
        detection_window_size: int = 20,
        min_regression_threshold: float = 0.05,  # 5% regression
        confidence_threshold: float = 0.8,
        enable_correlation_analysis: bool = True
    ):
        self.baseline_window_size = baseline_window_size
        self.detection_window_size = detection_window_size
        self.min_regression_threshold = min_regression_threshold
        self.confidence_threshold = confidence_threshold
        self.enable_correlation_analysis = enable_correlation_analysis
        
        # Performance baselines for each metric
        self.baselines: Dict[str, PerformanceBaseline] = {}
        
        # Recent measurements for trend analysis
        self.recent_measurements: Dict[str, deque] = defaultdict(
            lambda: deque(maxlen=detection_window_size * 2)
        )
        
        # Regression detection history
        self.regression_history: List[RegressionAlert] = []
        
        # Model update tracking
        self.model_updates: List[Dict[str, Any]] = []
        
        # Alert callbacks
        self.alert_callbacks: List[Callable[[RegressionAlert], None]] = []
        
        # Detection state
        self.is_active = False
        self.detection_thread: Optional[threading.Thread] = None
        
        # Regression thresholds by severity
        self.severity_thresholds = {
            RegressionSeverity.MINOR: 0.05,      # 5%
            RegressionSeverity.MODERATE: 0.15,   # 15% 
            RegressionSeverity.SEVERE: 0.30,     # 30%
            RegressionSeverity.CRITICAL: 0.50    # 50%
        }
        
        logger.info("Performance Regression Detector initialized")
    
    def register_model_update(
        self, 
        model_name: str, 
        version: str, 
        update_type: str = "standard",
        metadata: Optional[Dict[str, Any]] = None
    ):
        """Register a model update event for correlation analysis."""
        update_event = {
            "timestamp": time.time(),
            "model_name": model_name,
            "version": version,
            "update_type": update_type,
            "metadata": metadata or {}
        }
        
        self.model_updates.append(update_event)
        
        # Keep only recent updates (last 24 hours)
        cutoff_time = time.time() - 86400
        self.model_updates = [
            u for u in self.model_updates if u["timestamp"] > cutoff_time
        ]
        
        logger.info(f"Model update registered: {model_name} v{version}")
    
    def update_metrics(self, metrics: BackendMetrics):
        """Update regression detector with new performance metrics."""
        current_time = time.time()
        
        # Key metrics to monitor for regressions
        key_metrics = {
            'token_throughput': metrics.token_throughput,
            'request_latency_p95': metrics.request_latency_p95,
            'error_rate_percent': metrics.error_rate_percent,
            'cpu_usage_percent': metrics.cpu_usage_percent,
            'memory_usage_percent': metrics.memory_usage_percent,
            'gpu_memory_usage_percent': metrics.gpu_memory_usage_percent,
        }
        
        # Add Deep Tree Echo metrics if available
        if metrics.aar_agents_active > 0:
            key_metrics.update({
                'aar_agents_active': metrics.aar_agents_active,
                'dtesn_processing_rate': metrics.dtesn_processing_rate,
                'echo_self_evolution_score': metrics.echo_self_evolution_score,
            })
        
        # Update baselines and detect regressions
        for metric_name, value in key_metrics.items():
            if value is not None and not np.isnan(value):
                self._update_baseline(metric_name, value)
                self._update_recent_measurements(metric_name, value)
                
                # Check for regressions
                regression = self._detect_regression(metric_name, current_time)
                if regression:
                    self._process_regression_alert(regression)
    
    def _update_baseline(self, metric_name: str, value: float):
        """Update performance baseline for a metric."""
        if metric_name not in self.baselines:
            self.baselines[metric_name] = PerformanceBaseline(
                metric_name=metric_name,
                baseline_value=value,
                window_size=self.baseline_window_size,
                values=deque([value], maxlen=self.baseline_window_size)
            )
        else:
            self.baselines[metric_name].update(value)
    
    def _update_recent_measurements(self, metric_name: str, value: float):
        """Update recent measurements for trend analysis."""
        self.recent_measurements[metric_name].append({
            'timestamp': time.time(),
            'value': value
        })
    
    def _detect_regression(self, metric_name: str, current_time: float) -> Optional[RegressionAlert]:
        """Detect performance regression for a specific metric."""
        if metric_name not in self.baselines:
            return None
        
        baseline = self.baselines[metric_name]
        recent_values = self.recent_measurements[metric_name]
        
        # Need sufficient data for analysis
        if len(recent_values) < self.detection_window_size:
            return None
        
        # Get recent window of values
        window_values = [m['value'] for m in list(recent_values)[-self.detection_window_size:]]
        
        if not window_values:
            return None
        
        # Calculate current performance vs baseline
        current_avg = statistics.mean(window_values)
        baseline_value = baseline.baseline_value
        
        if baseline_value == 0:
            return None  # Avoid division by zero
        
        # Calculate regression percentage
        regression_percent = self._calculate_regression_percent(
            metric_name, current_avg, baseline_value
        )
        
        # Check if regression exceeds threshold
        if abs(regression_percent) < self.min_regression_threshold:
            return None
        
        # Calculate trend slope for confidence
        trend_slope = self._calculate_trend_slope(window_values)
        confidence = self._calculate_confidence(window_values, trend_slope)
        
        # Check confidence threshold
        if confidence < self.confidence_threshold:
            return None
        
        # Determine severity
        severity = self._determine_severity(abs(regression_percent))
        
        # Check for recent model updates for context
        context = self._analyze_context(current_time)
        
        return RegressionAlert(
            timestamp=current_time,
            metric_name=metric_name,
            severity=severity,
            current_value=current_avg,
            baseline_value=baseline_value,
            regression_percent=regression_percent,
            trend_slope=trend_slope,
            confidence=confidence,
            message=self._create_regression_message(
                metric_name, regression_percent, severity, context
            ),
            context=context
        )
    
    def _calculate_regression_percent(
        self, metric_name: str, current_value: float, baseline_value: float
    ) -> float:
        """Calculate regression percentage for a metric."""
        # For metrics where higher is better (throughput, scores)
        higher_better_metrics = {
            'token_throughput', 'echo_self_evolution_score', 
            'dtesn_processing_rate', 'aar_agents_active'
        }
        
        if metric_name in higher_better_metrics:
            # Negative regression means performance decreased
            return (baseline_value - current_value) / baseline_value
        else:
            # For metrics where lower is better (latency, error rate, usage)
            # Positive regression means performance decreased
            return (current_value - baseline_value) / baseline_value
    
    def _calculate_trend_slope(self, values: List[float]) -> float:
        """Calculate trend slope using linear regression."""
        if len(values) < 2:
            return 0.0
        
        n = len(values)
        x_vals = list(range(n))
        
        # Linear regression slope calculation
        x_mean = statistics.mean(x_vals)
        y_mean = statistics.mean(values)
        
        numerator = sum((x_vals[i] - x_mean) * (values[i] - y_mean) for i in range(n))
        denominator = sum((x_vals[i] - x_mean) ** 2 for i in range(n))
        
        return numerator / denominator if denominator != 0 else 0.0
    
    def _calculate_confidence(self, values: List[float], trend_slope: float) -> float:
        """Calculate confidence in regression detection."""
        if len(values) < 3:
            return 0.0
        
        # Base confidence on data consistency
        std_dev = statistics.stdev(values)
        mean_val = statistics.mean(values)
        
        # Coefficient of variation (lower is more consistent)
        cv = std_dev / mean_val if mean_val != 0 else float('inf')
        
        # Trend strength contribution
        normalized_slope = abs(trend_slope) / (mean_val if mean_val != 0 else 1.0)
        
        # Calculate confidence (0.0 to 1.0)
        # Lower CV and higher normalized slope = higher confidence
        consistency_factor = max(0.0, 1.0 - min(cv, 1.0))
        trend_factor = min(normalized_slope * 10, 1.0)  # Scale trend factor
        
        confidence = (consistency_factor + trend_factor) / 2.0
        return min(confidence, 1.0)
    
    def _determine_severity(self, regression_percent: float) -> RegressionSeverity:
        """Determine severity level based on regression percentage."""
        for severity in reversed(list(RegressionSeverity)):
            if regression_percent >= self.severity_thresholds[severity]:
                return severity
        
        return RegressionSeverity.MINOR
    
    def _analyze_context(self, current_time: float) -> Dict[str, Any]:
        """Analyze context around regression detection."""
        context = {}
        
        # Check for recent model updates
        recent_updates = [
            u for u in self.model_updates 
            if current_time - u["timestamp"] <= 3600  # Last hour
        ]
        
        if recent_updates:
            context["recent_model_updates"] = recent_updates
            context["potential_cause"] = "model_update"
        
        # Check for correlation with other metrics
        if self.enable_correlation_analysis:
            correlations = self._analyze_metric_correlations(current_time)
            if correlations:
                context["correlated_metrics"] = correlations
        
        return context
    
    def _analyze_metric_correlations(self, current_time: float) -> List[str]:
        """Analyze correlations between metrics showing regression."""
        # Simple correlation analysis - could be enhanced with statistical methods
        correlated = []
        
        for metric_name in self.recent_measurements:
            if len(self.recent_measurements[metric_name]) >= self.detection_window_size:
                recent_alert = any(
                    alert.metric_name == metric_name and 
                    current_time - alert.timestamp <= 300  # Last 5 minutes
                    for alert in self.regression_history[-10:]  # Recent alerts
                )
                
                if recent_alert:
                    correlated.append(metric_name)
        
        return correlated
    
    def _create_regression_message(
        self, 
        metric_name: str, 
        regression_percent: float,
        severity: RegressionSeverity,
        context: Dict[str, Any]
    ) -> str:
        """Create human-readable regression alert message."""
        direction = "decreased" if regression_percent > 0 else "increased"
        
        message = (
            f"{severity.value.title()} performance regression detected: "
            f"{metric_name} has {direction} by {abs(regression_percent)*100:.1f}%"
        )
        
        # Add context information
        if "recent_model_updates" in context:
            updates = context["recent_model_updates"]
            message += f" (potential correlation with {len(updates)} recent model update(s))"
        
        if "correlated_metrics" in context:
            correlated = context["correlated_metrics"]
            if len(correlated) > 1:
                message += f" (correlated with {len(correlated)-1} other metrics)"
        
        return message
    
    def _process_regression_alert(self, alert: RegressionAlert):
        """Process and handle a regression alert."""
        # Add to history
        self.regression_history.append(alert)
        
        # Keep history manageable
        if len(self.regression_history) > 1000:
            self.regression_history = self.regression_history[-500:]
        
        # Log the alert
        log_level = {
            RegressionSeverity.MINOR: logger.info,
            RegressionSeverity.MODERATE: logger.warning,
            RegressionSeverity.SEVERE: logger.error,
            RegressionSeverity.CRITICAL: logger.critical
        }.get(alert.severity, logger.warning)
        
        log_level(f"Performance Regression [{alert.severity.value.upper()}]: {alert.message}")
        
        # Call registered callbacks
        for callback in self.alert_callbacks:
            try:
                callback(alert)
            except Exception as e:
                logger.error(f"Regression alert callback error: {e}")
    
    def register_alert_callback(self, callback: Callable[[RegressionAlert], None]):
        """Register a callback for regression alerts."""
        self.alert_callbacks.append(callback)
        logger.info("Regression alert callback registered")
    
    def get_recent_regressions(self, hours: int = 24) -> List[RegressionAlert]:
        """Get recent regression alerts within the specified time period."""
        cutoff_time = time.time() - (hours * 3600)
        return [r for r in self.regression_history if r.timestamp >= cutoff_time]
    
    def get_baseline_status(self) -> Dict[str, Any]:
        """Get status of performance baselines."""
        status = {}
        
        for metric_name, baseline in self.baselines.items():
            status[metric_name] = {
                "baseline_value": baseline.baseline_value,
                "data_points": len(baseline.values),
                "last_updated": baseline.last_updated,
                "update_count": baseline.update_count,
                "window_size": baseline.window_size,
                "is_stable": len(baseline.values) >= baseline.window_size // 2
            }
        
        return status
    
    def reset_baselines(self, metric_names: Optional[List[str]] = None):
        """Reset performance baselines for specified metrics or all metrics."""
        if metric_names is None:
            metric_names = list(self.baselines.keys())
        
        for metric_name in metric_names:
            if metric_name in self.baselines:
                del self.baselines[metric_name]
                if metric_name in self.recent_measurements:
                    self.recent_measurements[metric_name].clear()
        
        logger.info(f"Reset baselines for {len(metric_names)} metrics")
    
    def get_regression_summary(self) -> Dict[str, Any]:
        """Get comprehensive regression detection summary."""
        recent_regressions = self.get_recent_regressions(hours=1)
        
        severity_counts = defaultdict(int)
        for regression in recent_regressions:
            severity_counts[regression.severity.value] += 1
        
        return {
            "detection_status": "active" if self.is_active else "inactive",
            "recent_regressions": {
                "total_last_hour": len(recent_regressions),
                "by_severity": dict(severity_counts)
            },
            "baseline_status": {
                "total_metrics": len(self.baselines),
                "stable_baselines": len([
                    b for b in self.baselines.values() 
                    if len(b.values) >= b.window_size // 2
                ]),
            },
            "model_updates": {
                "total_recent": len(self.model_updates),
                "last_update": (
                    max(u["timestamp"] for u in self.model_updates) 
                    if self.model_updates else None
                )
            },
            "configuration": {
                "regression_threshold": self.min_regression_threshold,
                "confidence_threshold": self.confidence_threshold,
                "detection_window": self.detection_window_size,
                "baseline_window": self.baseline_window_size,
            }
        }


# Factory function for easy integration
def create_regression_detector(**kwargs) -> PerformanceRegressionDetector:
    """Create a PerformanceRegressionDetector with default configuration."""
    return PerformanceRegressionDetector(**kwargs)