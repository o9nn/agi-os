"""
Aphrodite Engine Backend Performance Monitoring System.

This module provides comprehensive real-time performance monitoring for all 
server operations, automated performance analysis, and regression detection
for the Aphrodite Engine with Deep Tree Echo integration.

Phase 8 - SSR-Focused MLOps & Production Observability Implementation
"""

from .backend_monitor import BackendPerformanceMonitor
from .metrics_collector import AphroditeMetricsCollector, create_metrics_collector
from .regression_detector import PerformanceRegressionDetector, create_regression_detector
from .alerting_system import PerformanceAlertingSystem, create_alerting_system

__all__ = [
    "BackendPerformanceMonitor",
    "AphroditeMetricsCollector", 
    "PerformanceRegressionDetector",
    "PerformanceAlertingSystem",
    "create_metrics_collector",
    "create_regression_detector", 
    "create_alerting_system",
]

__version__ = "1.0.0"