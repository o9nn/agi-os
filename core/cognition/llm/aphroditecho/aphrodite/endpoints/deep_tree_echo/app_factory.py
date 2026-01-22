import logging
from pathlib import Path
from typing import Optional
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.templating import Jinja2Templates
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
from aphrodite.endpoints.deep_tree_echo.middleware import DTESNMiddleware, PerformanceMonitoringMiddleware, AsyncResourceMiddleware
from aphrodite.endpoints.deep_tree_echo.async_manager import AsyncConnectionPool, ConcurrencyManager, ConnectionPoolConfig
from aphrodite.endpoints.deep_tree_echo.routes import router
from aphrodite.endpoints.deep_tree_echo.template_engine_advanced import AdvancedTemplateEngine
from aphrodite.endpoints.deep_tree_echo.template_cache_manager import DTESNTemplateCacheManager
from aphrodite.endpoints.security import InputValidationMiddleware, OutputSanitizationMiddleware, SecurityMiddleware, RateLimitMiddleware
from aphrodite.engine.async_aphrodite import AsyncAphrodite
logger = logging.getLogger(__name__)
TEMPLATES_DIR = Path(__file__).parent / 'templates'
def create_app(engine: Optional[AsyncAphrodite]=None, config: Optional[DTESNConfig]=None, enable_async_resources: bool=True) -> FastAPI:
    if config is None:
        config = DTESNConfig()
    app = FastAPI(title='Deep Tree Echo API', description='Enhanced server-side rendering API for Deep Tree Echo System Network (DTESN) processing with async resource management', version='1.0.0', docs_url='/docs' if config.enable_docs else None, redoc_url='/redoc' if config.enable_docs else None, openapi_url='/openapi.json' if config.enable_docs else None)
    templates = Jinja2Templates(directory=str(TEMPLATES_DIR))
    advanced_template_engine = AdvancedTemplateEngine(TEMPLATES_DIR)
    template_cache_manager = DTESNTemplateCacheManager(max_template_cache_size=100, max_rendered_cache_size=500, enable_compression=True)
    app.state.templates = templates
    app.state.templates_dir = TEMPLATES_DIR
    app.state.advanced_template_engine = advanced_template_engine
    app.state.template_cache_manager = template_cache_manager
    connection_pool = None
    concurrency_manager = None
    if enable_async_resources:
        pool_config = ConnectionPoolConfig(max_connections=500, min_connections=50, connection_timeout=15.0, idle_timeout=180.0, enable_keepalive=True, max_concurrent_creates=50)
        connection_pool = AsyncConnectionPool(pool_config)
        concurrency_manager = ConcurrencyManager(max_concurrent_requests=500, max_requests_per_second=1000.0, burst_limit=100, adaptive_scaling=True, scale_factor=1.2)
        logger.info('Async resource management enabled with connection pooling and concurrency control')
    app.add_middleware(CORSMiddleware, allow_origins=config.allowed_origins, allow_credentials=True, allow_methods=['GET', 'POST', 'PUT', 'DELETE'], allow_headers=['*'])
    app.add_middleware(OutputSanitizationMiddleware)
    app.add_middleware(SecurityMiddleware)
    app.add_middleware(RateLimitMiddleware)
    app.add_middleware(InputValidationMiddleware)
    if enable_async_resources and connection_pool:
        app.add_middleware(AsyncResourceMiddleware, connection_pool=connection_pool)
    app.add_middleware(PerformanceMonitoringMiddleware, concurrency_manager=concurrency_manager)
    app.add_middleware(DTESNMiddleware, connection_pool=connection_pool)
    app.state.engine = engine
    app.state.config = config
    app.state.connection_pool = connection_pool
    app.state.concurrency_manager = concurrency_manager
    from .model_serving_manager import ModelServingManager
    from .model_serving_routes import create_model_serving_routes
    model_serving_manager = ModelServingManager(engine=engine)
    app.state.model_serving_manager = model_serving_manager
    logger.info('Model Serving Manager initialized for Task 8.1.1')
    app.include_router(router, prefix='/deep_tree_echo')
    model_serving_router = create_model_serving_routes(model_serving_manager)
    app.include_router(model_serving_router, prefix='/api/v1', tags=['Model Serving'])
    logger.info('Model Serving routes integrated (Task 8.1.1)')
    try:
        from .config_routes import config_router
        app.include_router(config_router)
        logger.info('Configuration management endpoints enabled')
    except ImportError as e:
        logger.warning(f'Configuration management not available: {e}')
    @app.get('/health')
    async def health_check():
        health_data = {'status': 'healthy', 'service': 'Deep Tree Echo API', 'version': '1.0.0', 'server_rendered': True, 'templates_available': TEMPLATES_DIR.exists(), 'async_resources': {'connection_pool_enabled': connection_pool is not None, 'concurrency_management_enabled': concurrency_manager is not None}}
        if connection_pool:
            stats = connection_pool.get_stats()
            health_data['resource_stats'] = {'active_connections': stats.active_connections, 'idle_connections': stats.idle_connections, 'pool_utilization': stats.pool_utilization, 'avg_response_time': stats.avg_response_time}
        if concurrency_manager:
            load_stats = concurrency_manager.get_current_load()
            health_data['concurrency_stats'] = load_stats
        if model_serving_manager:
            serving_status = model_serving_manager.get_model_serving_status()
            health_data['model_serving'] = {'enabled': True, 'cached_models': serving_status['overview']['cached_models'], 'active_versions': serving_status['overview']['active_versions'], 'healthy_models': serving_status['health_summary']['healthy_models'], 'total_loads': serving_status['performance_metrics']['total_loads'], 'cache_hit_rate': serving_status['performance_metrics']['cache_hit_rate'], 'engine_integrated': serving_status['engine_integration']['engine_available']}
        return health_data
    @app.on_event('startup')
    async def startup_event():
        if connection_pool:
            await connection_pool.start()
            logger.info('Connection pool started successfully')
        await start_monitoring()
        if config.enable_docs:
            alert_manager.add_notification_handler(console_alert_handler)
            logger.info('Console alert handler enabled for development')
        logger.info('Deep Tree Echo FastAPI application started with enhanced async processing and monitoring')
    @app.on_event('shutdown')
    async def shutdown_event():
        await stop_monitoring()
        if connection_pool:
            await connection_pool.stop()
            logger.info('Connection pool stopped successfully')
        logger.info('Deep Tree Echo FastAPI application shutdown complete')
    if enable_async_resources:
        from aphrodite.endpoints.deep_tree_echo.middleware import AsyncPerformanceMiddleware, AsyncLoadBalancingMiddleware
        app.add_middleware(AsyncPerformanceMiddleware, enable_detailed_metrics=True, performance_threshold_ms=1000.0, slow_request_threshold_ms=5000.0)
        app.add_middleware(AsyncLoadBalancingMiddleware, enable_adaptive_throttling=True, max_queue_size=10000, load_balance_strategy='round_robin')
        logger.info('Enhanced async middleware stack added for optimal performance')
    app.state.connection_pool = connection_pool
    app.state.concurrency_manager = concurrency_manager
    return app
    logger.info(f'Deep Tree Echo FastAPI application created successfully with templates at {TEMPLATES_DIR}')
    return app