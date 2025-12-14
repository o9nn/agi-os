"""
Advanced middleware components for route optimization and observability.

This module provides caching, compression, preprocessing middleware
for the Aphrodite Engine API server to achieve sub-100ms response times,
plus comprehensive logging, performance monitoring, and security middleware
for full observability and protection.
"""

# Existing route optimization middleware
from .cache_middleware import CacheMiddleware, CacheConfig
from .compression_middleware import CompressionMiddleware, CompressionConfig  
from .preprocessing_middleware import (
    PreprocessingMiddleware, 
    PreprocessingConfig
)

# New comprehensive middleware stack
from .logging_middleware import (
    ComprehensiveLoggingMiddleware,
    LoggingConfig,
    RequestContext
)
from .performance_middleware import (
    EnhancedPerformanceMonitoringMiddleware,
    PerformanceProfiler,
    PerformanceMetrics,
    SystemMetrics
)
from .advanced_security_middleware import (
    AdvancedSecurityMiddleware,
    AdvancedSecurityConfig,
    SecurityThreat,
    DDoSProtector,
    AdvancedAnomalyDetector,
    ContentInspector
)
from .comprehensive_middleware import (
    MiddlewareOrchestrator,
    MiddlewareConfig,
    setup_comprehensive_middleware
)

__all__ = [
    # Existing route optimization middleware
    "CacheMiddleware",
    "CacheConfig", 
    "CompressionMiddleware",
    "CompressionConfig",
    "PreprocessingMiddleware", 
    "PreprocessingConfig",
    
    # New comprehensive middleware components
    'ComprehensiveLoggingMiddleware',
    'LoggingConfig', 
    'RequestContext',
    'EnhancedPerformanceMonitoringMiddleware',
    'PerformanceProfiler',
    'PerformanceMetrics',
    'SystemMetrics',
    'AdvancedSecurityMiddleware',
    'AdvancedSecurityConfig',
    'SecurityThreat',
    'DDoSProtector',
    'AdvancedAnomalyDetector', 
    'ContentInspector',
    'MiddlewareOrchestrator',
    'MiddlewareConfig',
    'setup_comprehensive_middleware'
]