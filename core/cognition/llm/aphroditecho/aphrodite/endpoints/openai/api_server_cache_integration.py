import logging
import os
from typing import Optional, Dict, Any
from aphrodite.endpoints.openai.dtesn_cache_manager import DTESNServerSideCacheManager, CacheStrategy, initialize_global_cache_manager, shutdown_global_cache_manager
logger = logging.getLogger(__name__)
def get_cache_config_from_env() -> Dict[str, Any]:
    cache_enabled = os.getenv('DTESN_CACHE_ENABLED', 'true').lower() == 'true'
    if not cache_enabled:
        return {'enabled': False}
    strategy_map = {'aggressive': CacheStrategy.AGGRESSIVE, 'balanced': CacheStrategy.BALANCED, 'conservative': CacheStrategy.CONSERVATIVE, 'dynamic': CacheStrategy.DYNAMIC}
    strategy_name = os.getenv('DTESN_CACHE_STRATEGY', 'balanced').lower()
    cache_strategy = strategy_map.get(strategy_name, CacheStrategy.BALANCED)
    config = {'enabled': True, 'max_memory_entries': int(os.getenv('DTESN_CACHE_MAX_MEMORY_ENTRIES', '1000')), 'max_compressed_entries': int(os.getenv('DTESN_CACHE_MAX_COMPRESSED_ENTRIES', '5000')), 'redis_url': os.getenv('DTESN_CACHE_REDIS_URL'), 'default_ttl_seconds': int(os.getenv('DTESN_CACHE_DEFAULT_TTL_SECONDS', '3600')), 'cache_strategy': cache_strategy, 'enable_compression': os.getenv('DTESN_CACHE_COMPRESSION_ENABLED', 'true').lower() == 'true', 'compression_threshold': int(os.getenv('DTESN_CACHE_COMPRESSION_THRESHOLD', '1024'))}
    return config
async def initialize_dtesn_cache(app_state: Optional[Any]=None) -> Optional[DTESNServerSideCacheManager]:
    try:
        config = get_cache_config_from_env()
        if not config.get('enabled', True):
            logger.info('DTESN caching disabled by configuration')
            return None
        cache_manager = initialize_global_cache_manager(**{k: v for k, v in config.items() if k != 'enabled'})
        await cache_manager.initialize()
        if app_state is not None:
            app_state.dtesn_cache_manager = cache_manager
        logger.info(f"✅ DTESN cache manager initialized with strategy: {config['cache_strategy'].value}")
        logger.info(f"   Memory entries: {config['max_memory_entries']}")
        logger.info(f"   Compressed entries: {config['max_compressed_entries']}")
        logger.info(f"   Redis URL: {config['redis_url'] or 'Not configured'}")
        logger.info(f"   Default TTL: {config['default_ttl_seconds']}s")
        return cache_manager
    except Exception as e:
        logger.error(f'Failed to initialize DTESN cache manager: {e}')
        return None
async def shutdown_dtesn_cache(app_state: Optional[Any]=None) -> None:
    try:
        if app_state is not None and hasattr(app_state, 'dtesn_cache_manager'):
            delattr(app_state, 'dtesn_cache_manager')
        await shutdown_global_cache_manager()
        logger.info('DTESN cache manager shut down successfully')
    except Exception as e:
        logger.error(f'Error shutting down DTESN cache manager: {e}')
def add_cache_middleware(app: Any) -> None:
    @app.middleware('http')
    async def cache_headers_middleware(request, call_next):
        response = await call_next(request)
        if request.url.path.startswith('/v1/dtesn/'):
            response.headers['X-DTESN-Cache-Available'] = 'true'
            from aphrodite.endpoints.openai.dtesn_cache_manager import get_cache_manager
            cache_manager = get_cache_manager()
            if cache_manager:
                try:
                    metrics = cache_manager.get_performance_metrics()
                    response.headers['X-Cache-Hit-Ratio'] = f"{metrics['hit_ratio']:.2%}"
                    response.headers['X-Cache-Performance-Improvement'] = f"{metrics['performance_improvement_percent']:.1f}%"
                except Exception as e:
                    logger.debug(f'Failed to add cache metrics headers: {e}')
        return response
    logger.info('Cache middleware added to FastAPI application')
def add_cache_routes(app: Any, router_prefix: str='') -> None:
    try:
        from aphrodite.endpoints.openai.dtesn_cached_routes import router as cache_router
        app.include_router(cache_router, prefix=router_prefix)
        logger.info(f'DTESN cache routes added with prefix: {router_prefix}')
    except ImportError as e:
        logger.warning(f'Could not add cache routes: {e}')
def create_cache_lifespan_handler():
    async def lifespan(app):
        logger.info('Initializing DTESN cache manager...')
        cache_manager = await initialize_dtesn_cache(app.state)
        if cache_manager:
            logger.info('DTESN cache manager startup complete')
        else:
            logger.info('DTESN cache manager not initialized (disabled or failed)')
        yield
        logger.info('Shutting down DTESN cache manager...')
        await shutdown_dtesn_cache(app.state)
        logger.info('DTESN cache manager shutdown complete')
    return lifespan
def validate_cache_config() -> bool:
    try:
        config = get_cache_config_from_env()
        if not config.get('enabled', True):
            return True
        if config['max_memory_entries'] <= 0:
            logger.error('DTESN_CACHE_MAX_MEMORY_ENTRIES must be > 0')
            return False
        if config['max_compressed_entries'] <= 0:
            logger.error('DTESN_CACHE_MAX_COMPRESSED_ENTRIES must be > 0')
            return False
        if config['default_ttl_seconds'] <= 0:
            logger.error('DTESN_CACHE_DEFAULT_TTL_SECONDS must be > 0')
            return False
        if config['compression_threshold'] < 0:
            logger.error('DTESN_CACHE_COMPRESSION_THRESHOLD must be >= 0')
            return False
        logger.info('DTESN cache configuration validation passed')
        return True
    except Exception as e:
        logger.error(f'Cache configuration validation failed: {e}')
        return False
def log_cache_config():
    try:
        config = get_cache_config_from_env()
        logger.info('DTESN Cache Configuration:')
        logger.info(f"  Enabled: {config.get('enabled', False)}")
        if config.get('enabled'):
            logger.info(f"  Strategy: {config['cache_strategy'].value}")
            logger.info(f"  Memory entries: {config['max_memory_entries']}")
            logger.info(f"  Compressed entries: {config['max_compressed_entries']}")
            logger.info(f"  Default TTL: {config['default_ttl_seconds']}s")
            logger.info(f"  Redis URL: {config['redis_url'] or 'Not configured'}")
            logger.info(f"  Compression enabled: {config['enable_compression']}")
            logger.info(f"  Compression threshold: {config['compression_threshold']} bytes")
    except Exception as e:
        logger.error(f'Failed to log cache configuration: {e}')
def integrate_dtesn_cache_with_api_server(app: Any, enable_routes: bool=True, enable_middleware: bool=True):
    if not validate_cache_config():
        logger.error('Invalid cache configuration - skipping cache integration')
        return
    log_cache_config()
    if enable_middleware:
        add_cache_middleware(app)
    if enable_routes:
        add_cache_routes(app)
    app.router.lifespan = create_cache_lifespan_handler()
    logger.info('DTESN cache integration complete')
__all__ = ['initialize_dtesn_cache', 'shutdown_dtesn_cache', 'add_cache_middleware', 'add_cache_routes', 'create_cache_lifespan_handler', 'validate_cache_config', 'log_cache_config', 'integrate_dtesn_cache_with_api_server', 'get_cache_config_from_env']