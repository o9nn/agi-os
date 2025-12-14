"""
Comprehensive middleware stack integration for FastAPI applications.

Orchestrates logging, performance monitoring, and security middleware to provide
complete observability and protection for server-side endpoints.
"""

from typing import Any, Dict, Optional
from dataclasses import dataclass
from datetime import datetime

from fastapi import FastAPI
from loguru import logger

# Import middleware components
from .logging_middleware import (
    ComprehensiveLoggingMiddleware, 
    LoggingConfig
)
from .performance_middleware import (
    EnhancedPerformanceMonitoringMiddleware,
    PerformanceProfiler
)
from .advanced_security_middleware import (
    AdvancedSecurityMiddleware,
    AdvancedSecurityConfig
)

# Import existing middleware for compatibility
from aphrodite.endpoints.deep_tree_echo.middleware import (
    DTESNMiddleware
)
from aphrodite.endpoints.security.security_middleware import (
    SecurityMiddleware as BasicSecurityMiddleware,
    RateLimitMiddleware
)


@dataclass
class MiddlewareConfig:
    """Comprehensive configuration for all middleware components."""
    
    # Logging configuration
    enable_logging: bool = True
    log_requests: bool = True
    log_responses: bool = True
    log_request_body: bool = False
    log_response_body: bool = False
    log_level: str = "INFO"
    
    # Performance monitoring configuration
    enable_performance_monitoring: bool = True
    enable_profiling: bool = True
    slow_request_threshold_ms: float = 1000.0
    enable_detailed_metrics: bool = True
    
    # Security configuration
    enable_advanced_security: bool = True
    enable_ddos_protection: bool = True
    enable_anomaly_detection: bool = True
    enable_content_inspection: bool = True
    requests_per_minute: int = 100
    
    # DTESN integration
    enable_dtesn_integration: bool = True
    enable_dtesn_logging: bool = True
    
    # Health monitoring
    enable_health_endpoints: bool = True
    health_check_path: str = "/health"
    metrics_path: str = "/metrics"
    
    @classmethod
    def production(cls) -> 'MiddlewareConfig':
        """Production-ready configuration with full security and monitoring."""
        return cls(
            enable_logging=True,
            log_requests=True,
            log_responses=True,
            log_request_body=False,  # Don't log bodies in production
            log_response_body=False,
            log_level="INFO",
            enable_performance_monitoring=True,
            enable_profiling=True,
            slow_request_threshold_ms=500.0,
            enable_detailed_metrics=True,
            enable_advanced_security=True,
            enable_ddos_protection=True,
            enable_anomaly_detection=True,
            enable_content_inspection=True,
            requests_per_minute=60,  # Conservative rate limiting
            enable_dtesn_integration=True,
            enable_health_endpoints=True
        )
    
    @classmethod
    def development(cls) -> 'MiddlewareConfig':
        """Development configuration with detailed logging and relaxed security."""
        return cls(
            enable_logging=True,
            log_requests=True,
            log_responses=True,
            log_request_body=True,  # Log bodies for debugging
            log_response_body=True,
            log_level="DEBUG",
            enable_performance_monitoring=True,
            enable_profiling=True,
            slow_request_threshold_ms=2000.0,
            enable_detailed_metrics=True,
            enable_advanced_security=False,  # Use basic security only
            enable_ddos_protection=False,
            enable_anomaly_detection=False,
            enable_content_inspection=False,
            requests_per_minute=1000,  # Relaxed rate limiting
            enable_dtesn_integration=True,
            enable_health_endpoints=True
        )


class MiddlewareOrchestrator:
    """
    Orchestrates all middleware components and provides unified management,
    metrics collection, and health monitoring.
    """
    
    def __init__(self, config: MiddlewareConfig = None):
        """Initialize middleware orchestrator."""
        self.config = config or MiddlewareConfig()
        
        # Initialize shared components
        self.performance_profiler = PerformanceProfiler(max_history=1000)
        
        # Initialize middleware instances
        self.logging_middleware = None
        self.performance_middleware = None
        self.security_middleware = None
        self.dtesn_middleware = None
        
        # Metrics collection
        self.start_time = datetime.utcnow()
        self.request_count = 0
        self.error_count = 0
        
        logger.info("MiddlewareOrchestrator initialized")
    
    def setup_middleware_stack(self, app: FastAPI) -> FastAPI:
        """
        Set up the complete middleware stack on FastAPI application.
        Middleware is added in reverse order (last added = first executed).
        """
        
        # 1. DTESN Middleware (innermost - closest to route handlers)
        if self.config.enable_dtesn_integration:
            self.dtesn_middleware = DTESNMiddleware(app)
            app.add_middleware(
                DTESNMiddleware.__class__,
                **self._get_dtesn_middleware_kwargs()
            )
            logger.info("Added DTESN middleware to stack")
        
        # 2. Performance Monitoring Middleware
        if self.config.enable_performance_monitoring:
            self.performance_middleware = EnhancedPerformanceMonitoringMiddleware(
                app=app,
                profiler=self.performance_profiler,
                enable_profiling=self.config.enable_profiling,
                slow_request_threshold_ms=self.config.slow_request_threshold_ms,
                enable_detailed_metrics=self.config.enable_detailed_metrics
            )
            app.add_middleware(
                EnhancedPerformanceMonitoringMiddleware.__class__,
                profiler=self.performance_profiler,
                enable_profiling=self.config.enable_profiling,
                slow_request_threshold_ms=self.config.slow_request_threshold_ms,
                enable_detailed_metrics=self.config.enable_detailed_metrics
            )
            logger.info("Added enhanced performance monitoring middleware to stack")
        
        # 3. Security Middleware
        if self.config.enable_advanced_security:
            security_config = AdvancedSecurityConfig(
                enable_ddos_protection=self.config.enable_ddos_protection,
                enable_anomaly_detection=self.config.enable_anomaly_detection,
                enable_content_inspection=self.config.enable_content_inspection,
                requests_per_minute=self.config.requests_per_minute
            )
            
            self.security_middleware = AdvancedSecurityMiddleware(
                app=app,
                config=security_config
            )
            app.add_middleware(
                AdvancedSecurityMiddleware.__class__,
                config=security_config
            )
            logger.info("Added advanced security middleware to stack")
        else:
            # Use basic security middleware
            app.add_middleware(BasicSecurityMiddleware)
            app.add_middleware(RateLimitMiddleware)
            logger.info("Added basic security middleware to stack")
        
        # 4. Logging Middleware (outermost - first to see requests/responses)
        if self.config.enable_logging:
            logging_config = LoggingConfig(
                log_requests=self.config.log_requests,
                log_responses=self.config.log_responses,
                log_request_body=self.config.log_request_body,
                log_response_body=self.config.log_response_body,
                log_level=self.config.log_level,
                enable_dtesn_logging=self.config.enable_dtesn_logging
            )
            
            self.logging_middleware = ComprehensiveLoggingMiddleware(
                app=app,
                config=logging_config
            )
            app.add_middleware(
                ComprehensiveLoggingMiddleware.__class__,
                config=logging_config
            )
            logger.info("Added comprehensive logging middleware to stack")
        
        # 5. Add health and metrics endpoints
        if self.config.enable_health_endpoints:
            self._add_health_endpoints(app)
            logger.info("Added health and metrics endpoints")
        
        logger.info("Complete middleware stack configured successfully")
        return app
    
    def _get_dtesn_middleware_kwargs(self) -> Dict[str, Any]:
        """Get configuration for DTESN middleware."""
        return {
            # Add any specific DTESN configuration here
        }
    
    def _add_health_endpoints(self, app: FastAPI):
        """Add health check and metrics endpoints."""
        
        @app.get(self.config.health_check_path)
        async def health_check():
            """Health check endpoint."""
            health_status = {
                "status": "healthy",
                "timestamp": datetime.utcnow().isoformat(),
                "uptime_seconds": (datetime.utcnow() - self.start_time).total_seconds(),
                "version": "1.0.0",
                "middleware": {
                    "logging": self.logging_middleware is not None,
                    "performance": self.performance_middleware is not None,
                    "security": self.security_middleware is not None,
                    "dtesn": self.dtesn_middleware is not None
                }
            }
            
            # Add performance health if available
            if self.performance_middleware:
                performance_health = self.performance_middleware.get_health_status()
                health_status["system"] = performance_health.get("system", {})
                
                # Determine overall health based on system metrics
                if performance_health.get("status") == "critical":
                    health_status["status"] = "degraded"
            
            # Add security metrics if available
            if self.security_middleware:
                security_metrics = self.security_middleware.get_security_metrics()
                health_status["security"] = {
                    "total_requests": security_metrics.get("total_requests", 0),
                    "blocked_requests": security_metrics.get("blocked_requests", 0),
                    "block_rate": security_metrics.get("block_rate", 0.0),
                    "active_threats": security_metrics.get("threats_last_hour", 0)
                }
                
                # Mark as degraded if high block rate
                if security_metrics.get("block_rate", 0) > 0.1:  # More than 10% blocked
                    health_status["status"] = "degraded"
            
            return health_status
        
        @app.get(self.config.metrics_path)
        async def metrics_endpoint():
            """Comprehensive metrics endpoint."""
            metrics = {
                "timestamp": datetime.utcnow().isoformat(),
                "uptime_seconds": (datetime.utcnow() - self.start_time).total_seconds()
            }
            
            # Performance metrics
            if self.performance_middleware:
                metrics["performance"] = {
                    "statistics": self.performance_middleware.get_statistics(),
                    "endpoint_stats": self.performance_middleware.get_endpoint_statistics(),
                    "system_health": self.performance_middleware.get_health_status()
                }
            
            # Security metrics
            if self.security_middleware:
                metrics["security"] = {
                    "overview": self.security_middleware.get_security_metrics(),
                    "recent_threats": self.security_middleware.get_threat_history(hours=1)
                }
            
            # System metrics from profiler
            if self.performance_profiler:
                metrics["system_trends"] = self.performance_profiler.get_performance_trends(minutes=30)
            
            return metrics
        
        @app.get(f"{self.config.metrics_path}/performance")
        async def performance_metrics():
            """Detailed performance metrics endpoint."""
            if not self.performance_middleware:
                return {"error": "Performance monitoring not enabled"}
            
            return {
                "statistics": self.performance_middleware.get_statistics(),
                "endpoint_statistics": self.performance_middleware.get_endpoint_statistics(),
                "system_health": self.performance_middleware.get_health_status(),
                "trends": self.performance_profiler.get_performance_trends(minutes=60)
            }
        
        @app.get(f"{self.config.metrics_path}/security") 
        async def security_metrics():
            """Detailed security metrics endpoint."""
            if not self.security_middleware:
                return {"error": "Advanced security not enabled"}
            
            return {
                "overview": self.security_middleware.get_security_metrics(),
                "threat_history_24h": self.security_middleware.get_threat_history(hours=24),
                "threat_history_1h": self.security_middleware.get_threat_history(hours=1)
            }
    
    def get_comprehensive_status(self) -> Dict[str, Any]:
        """Get comprehensive status of all middleware components."""
        
        status = {
            "orchestrator": {
                "uptime_seconds": (datetime.utcnow() - self.start_time).total_seconds(),
                "middleware_count": sum([
                    self.logging_middleware is not None,
                    self.performance_middleware is not None,
                    self.security_middleware is not None,
                    self.dtesn_middleware is not None
                ])
            }
        }
        
        # Add component-specific status
        if self.performance_middleware:
            status["performance"] = self.performance_middleware.get_statistics()
        
        if self.security_middleware:
            status["security"] = self.security_middleware.get_security_metrics()
        
        return status
    
    def shutdown(self):
        """Gracefully shutdown all middleware components."""
        
        logger.info("Shutting down middleware orchestrator...")
        
        # Shutdown profiler
        if self.performance_profiler:
            self.performance_profiler.shutdown()
        
        logger.info("Middleware orchestrator shutdown complete")


# Convenience function for easy setup
def setup_comprehensive_middleware(
    app: FastAPI, 
    config: Optional[MiddlewareConfig] = None,
    environment: str = "production"
) -> MiddlewareOrchestrator:
    """
    Convenience function to set up comprehensive middleware stack.
    
    Args:
        app: FastAPI application instance
        config: Optional middleware configuration
        environment: Environment type ("production", "development", or custom config)
    
    Returns:
        MiddlewareOrchestrator instance for further management
    """
    
    # Use environment-specific config if none provided
    if config is None:
        if environment == "production":
            config = MiddlewareConfig.production()
        elif environment == "development":
            config = MiddlewareConfig.development()
        else:
            config = MiddlewareConfig()
    
    # Create and setup orchestrator
    orchestrator = MiddlewareOrchestrator(config)
    orchestrator.setup_middleware_stack(app)
    
    logger.info(f"Comprehensive middleware stack set up for {environment} environment")
    
    return orchestrator


# Export key components
__all__ = [
    'MiddlewareConfig',
    'MiddlewareOrchestrator', 
    'setup_comprehensive_middleware',
    'ComprehensiveLoggingMiddleware',
    'EnhancedPerformanceMonitoringMiddleware',
    'AdvancedSecurityMiddleware'
]