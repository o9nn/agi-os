from typing import Optional, Dict, Any
from dataclasses import dataclass, field
from fastapi import FastAPI
from starlette.types import ASGIApp
from .middleware.cache_middleware import CacheMiddleware, CacheConfig
from .middleware.compression_middleware import CompressionMiddleware, CompressionConfig
from .middleware.preprocessing_middleware import PreprocessingMiddleware, PreprocessingConfig
@dataclass
class RouteOptimizationConfig:
    enable_caching: bool = True
    enable_compression: bool = True
    enable_preprocessing: bool = True
    cache_config: Optional[CacheConfig] = None
    compression_config: Optional[CompressionConfig] = None
    preprocessing_config: Optional[PreprocessingConfig] = None
    target_response_time_ms: int = 100
    enable_metrics: bool = True
    metrics_route: str = '/metrics'
class RouteOptimizer:
    def __init__(self, config: RouteOptimizationConfig):
        self.config = config
        self.cache_config = config.cache_config or CacheConfig()
        self.compression_config = config.compression_config or CompressionConfig()
        self.preprocessing_config = config.preprocessing_config or PreprocessingConfig()
    def apply_optimizations(self, app: FastAPI) -> FastAPI:
        if self.config.enable_compression:
            app.add_middleware(CompressionMiddleware, config=self.compression_config)
        if self.config.enable_caching:
            app.add_middleware(CacheMiddleware, config=self.cache_config)
        if self.config.enable_preprocessing:
            app.add_middleware(PreprocessingMiddleware, config=self.preprocessing_config)
        if self.config.enable_metrics:
            self._add_performance_monitoring(app)
        return app
    def _add_performance_monitoring(self, app: FastAPI) -> None:
        @app.middleware('http')
        async def performance_monitor(request, call_next):
            import time
            start_time = time.time()
            response = await call_next(request)
            process_time = (time.time() - start_time) * 1000
            response.headers['X-Process-Time'] = f'{process_time:.2f}ms'
            if process_time > self.config.target_response_time_ms:
                from loguru import logger
                logger.warning(f'Slow response: {request.url.path} took {process_time:.2f}ms (target: {self.config.target_response_time_ms}ms)')
            return response
        if self.config.metrics_route:
            @app.get(self.config.metrics_route)
            async def metrics():
                return {'optimization_status': {'caching_enabled': self.config.enable_caching, 'compression_enabled': self.config.enable_compression, 'preprocessing_enabled': self.config.enable_preprocessing}, 'target_response_time_ms': self.config.target_response_time_ms, 'cache_stats': self._get_cache_stats() if self.config.enable_caching else None}
    def _get_cache_stats(self) -> Dict[str, Any]:
        return {'cache_hits': 0, 'cache_misses': 0, 'hit_rate': 0.0}
def create_optimized_app(base_app: FastAPI, config: Optional[RouteOptimizationConfig]=None) -> FastAPI:
    if config is None:
        config = RouteOptimizationConfig()
    optimizer = RouteOptimizer(config)
    return optimizer.apply_optimizations(base_app)
def get_high_performance_config() -> RouteOptimizationConfig:
    cache_config = CacheConfig(backend='memory', max_cache_size=2000, default_ttl=600, cache_deterministic_posts=True)
    compression_config = CompressionConfig(min_size=200, compression_level=4, enable_streaming=True)
    preprocessing_config = PreprocessingConfig(enable_rate_limiting=True, rate_limit=RateLimitConfig(requests_per_minute=120, burst_size=20), request_timeout=15.0)
    return RouteOptimizationConfig(enable_caching=True, enable_compression=True, enable_preprocessing=True, cache_config=cache_config, compression_config=compression_config, preprocessing_config=preprocessing_config, target_response_time_ms=50)
def get_balanced_config() -> RouteOptimizationConfig:
    return RouteOptimizationConfig(enable_caching=True, enable_compression=True, enable_preprocessing=True, target_response_time_ms=100)
def get_minimal_config() -> RouteOptimizationConfig:
    compression_config = CompressionConfig(min_size=1000, compression_level=1)
    preprocessing_config = PreprocessingConfig(enable_rate_limiting=False, enable_size_optimization=False)
    return RouteOptimizationConfig(enable_caching=False, enable_compression=True, enable_preprocessing=True, compression_config=compression_config, preprocessing_config=preprocessing_config, target_response_time_ms=200)