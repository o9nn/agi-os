"""
Advanced Route Optimization Manager for Aphrodite Engine API server.

Integrates caching, compression, and preprocessing middleware to achieve
sub-100ms API response times consistently.
"""

from typing import Optional, Dict, Any
from dataclasses import dataclass, field

from fastapi import FastAPI
from starlette.types import ASGIApp

from .middleware.cache_middleware import CacheMiddleware, CacheConfig
from .middleware.compression_middleware import CompressionMiddleware, CompressionConfig
from .middleware.preprocessing_middleware import PreprocessingMiddleware, PreprocessingConfig


@dataclass
class RouteOptimizationConfig:
    """Comprehensive configuration for route optimization."""
    
    # Enable/disable individual optimizations
    enable_caching: bool = True
    enable_compression: bool = True
    enable_preprocessing: bool = True
    
    # Individual middleware configurations
    cache_config: Optional[CacheConfig] = None
    compression_config: Optional[CompressionConfig] = None 
    preprocessing_config: Optional[PreprocessingConfig] = None
    
    # Performance targets
    target_response_time_ms: int = 100
    
    # Monitoring configuration
    enable_metrics: bool = True
    metrics_route: str = "/metrics"


class RouteOptimizer:
    """Route optimization manager that applies middleware in optimal order."""
    
    def __init__(self, config: RouteOptimizationConfig):
        self.config = config
        
        # Initialize configurations with defaults if not provided
        self.cache_config = config.cache_config or CacheConfig()
        self.compression_config = config.compression_config or CompressionConfig() 
        self.preprocessing_config = config.preprocessing_config or PreprocessingConfig()
    
    def apply_optimizations(self, app: FastAPI) -> FastAPI:
        """Apply all enabled optimizations to the FastAPI application."""
        
        # Add middleware in reverse order (last added = first executed)
        
        # 1. Compression (closest to response)
        if self.config.enable_compression:
            app.add_middleware(CompressionMiddleware, config=self.compression_config)
        
        # 2. Caching (after compression, before preprocessing)  
        if self.config.enable_caching:
            app.add_middleware(CacheMiddleware, config=self.cache_config)
        
        # 3. Preprocessing (closest to request)
        if self.config.enable_preprocessing:
            app.add_middleware(PreprocessingMiddleware, config=self.preprocessing_config)
        
        # Add performance monitoring if enabled
        if self.config.enable_metrics:
            self._add_performance_monitoring(app)
        
        return app
    
    def _add_performance_monitoring(self, app: FastAPI) -> None:
        """Add performance monitoring and metrics collection."""
        
        @app.middleware("http")
        async def performance_monitor(request, call_next):
            import time
            start_time = time.time()
            
            response = await call_next(request)
            
            process_time = (time.time() - start_time) * 1000  # Convert to ms
            response.headers["X-Process-Time"] = f"{process_time:.2f}ms"
            
            # Log slow requests
            if process_time > self.config.target_response_time_ms:
                from loguru import logger
                logger.warning(
                    f"Slow response: {request.url.path} took {process_time:.2f}ms "
                    f"(target: {self.config.target_response_time_ms}ms)"
                )
            
            return response
        
        # Add metrics endpoint if enabled
        if self.config.metrics_route:
            @app.get(self.config.metrics_route)
            async def metrics():
                """Endpoint for performance metrics."""
                return {
                    "optimization_status": {
                        "caching_enabled": self.config.enable_caching,
                        "compression_enabled": self.config.enable_compression, 
                        "preprocessing_enabled": self.config.enable_preprocessing
                    },
                    "target_response_time_ms": self.config.target_response_time_ms,
                    "cache_stats": self._get_cache_stats() if self.config.enable_caching else None
                }
    
    def _get_cache_stats(self) -> Dict[str, Any]:
        """Get cache performance statistics."""
        # This would be implemented to return cache hit/miss rates, etc.
        return {
            "cache_hits": 0,  # Placeholder
            "cache_misses": 0,  # Placeholder  
            "hit_rate": 0.0  # Placeholder
        }


def create_optimized_app(
    base_app: FastAPI,
    config: Optional[RouteOptimizationConfig] = None
) -> FastAPI:
    """
    Create an optimized FastAPI application with route optimizations applied.
    
    Args:
        base_app: The base FastAPI application
        config: Route optimization configuration (uses defaults if None)
    
    Returns:
        FastAPI application with optimizations applied
    """
    
    if config is None:
        config = RouteOptimizationConfig()
    
    optimizer = RouteOptimizer(config)
    return optimizer.apply_optimizations(base_app)


# Preset configurations for common use cases

def get_high_performance_config() -> RouteOptimizationConfig:
    """Configuration optimized for maximum performance."""
    
    cache_config = CacheConfig(
        backend="memory",
        max_cache_size=2000,
        default_ttl=600,
        cache_deterministic_posts=True
    )
    
    compression_config = CompressionConfig(
        min_size=200,  # Compress smaller responses
        compression_level=4,  # Balance compression vs CPU
        enable_streaming=True
    )
    
    preprocessing_config = PreprocessingConfig(
        enable_rate_limiting=True,
        rate_limit=RateLimitConfig(
            requests_per_minute=120,  # Higher limits
            burst_size=20
        ),
        request_timeout=15.0  # Shorter timeout
    )
    
    return RouteOptimizationConfig(
        enable_caching=True,
        enable_compression=True,
        enable_preprocessing=True,
        cache_config=cache_config,
        compression_config=compression_config,
        preprocessing_config=preprocessing_config,
        target_response_time_ms=50  # Very aggressive target
    )


def get_balanced_config() -> RouteOptimizationConfig:
    """Configuration with balanced performance and resource usage."""
    
    return RouteOptimizationConfig(
        enable_caching=True,
        enable_compression=True,
        enable_preprocessing=True,
        target_response_time_ms=100
    )


def get_minimal_config() -> RouteOptimizationConfig:
    """Minimal configuration with basic optimizations."""
    
    compression_config = CompressionConfig(
        min_size=1000,  # Only compress larger responses
        compression_level=1  # Minimal compression
    )
    
    preprocessing_config = PreprocessingConfig(
        enable_rate_limiting=False,  # Disable rate limiting
        enable_size_optimization=False
    )
    
    return RouteOptimizationConfig(
        enable_caching=False,  # Disable caching
        enable_compression=True,
        enable_preprocessing=True,
        compression_config=compression_config,
        preprocessing_config=preprocessing_config,
        target_response_time_ms=200  # More relaxed target
    )