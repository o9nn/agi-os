#!/usr/bin/env python3
"""
API Server integration for DTESN server-side caching layer.

Provides integration points to add caching functionality to the main 
Aphrodite API server with minimal modifications.
"""

import logging
import os
from typing import Optional, Dict, Any

from aphrodite.endpoints.openai.dtesn_cache_manager import (
    DTESNServerSideCacheManager,
    CacheStrategy,
    initialize_global_cache_manager,
    shutdown_global_cache_manager
)

logger = logging.getLogger(__name__)


def get_cache_config_from_env() -> Dict[str, Any]:
    """
    Get cache configuration from environment variables.
    
    Environment variables:
    - DTESN_CACHE_ENABLED: Enable/disable caching (default: true)
    - DTESN_CACHE_STRATEGY: Caching strategy (default: balanced)
    - DTESN_CACHE_MAX_MEMORY_ENTRIES: Max memory cache entries (default: 1000)
    - DTESN_CACHE_MAX_COMPRESSED_ENTRIES: Max compressed cache entries (default: 5000)
    - DTESN_CACHE_DEFAULT_TTL_SECONDS: Default TTL (default: 3600)
    - DTESN_CACHE_REDIS_URL: Redis URL for distributed caching
    - DTESN_CACHE_COMPRESSION_ENABLED: Enable compression (default: true)
    - DTESN_CACHE_COMPRESSION_THRESHOLD: Compression threshold bytes (default: 1024)
    """
    
    # Check if caching is enabled
    cache_enabled = os.getenv('DTESN_CACHE_ENABLED', 'true').lower() == 'true'
    if not cache_enabled:
        return {'enabled': False}
    
    # Parse cache strategy
    strategy_map = {
        'aggressive': CacheStrategy.AGGRESSIVE,
        'balanced': CacheStrategy.BALANCED,
        'conservative': CacheStrategy.CONSERVATIVE,
        'dynamic': CacheStrategy.DYNAMIC
    }
    strategy_name = os.getenv('DTESN_CACHE_STRATEGY', 'balanced').lower()
    cache_strategy = strategy_map.get(strategy_name, CacheStrategy.BALANCED)
    
    config = {
        'enabled': True,
        'max_memory_entries': int(os.getenv('DTESN_CACHE_MAX_MEMORY_ENTRIES', '1000')),
        'max_compressed_entries': int(os.getenv('DTESN_CACHE_MAX_COMPRESSED_ENTRIES', '5000')),
        'redis_url': os.getenv('DTESN_CACHE_REDIS_URL'),
        'default_ttl_seconds': int(os.getenv('DTESN_CACHE_DEFAULT_TTL_SECONDS', '3600')),
        'cache_strategy': cache_strategy,
        'enable_compression': os.getenv('DTESN_CACHE_COMPRESSION_ENABLED', 'true').lower() == 'true',
        'compression_threshold': int(os.getenv('DTESN_CACHE_COMPRESSION_THRESHOLD', '1024'))
    }
    
    return config


async def initialize_dtesn_cache(app_state: Optional[Any] = None) -> Optional[DTESNServerSideCacheManager]:
    """
    Initialize the DTESN cache manager for the API server.
    
    Args:
        app_state: Optional FastAPI application state to store cache manager
        
    Returns:
        Initialized cache manager or None if disabled
    """
    try:
        config = get_cache_config_from_env()
        
        if not config.get('enabled', True):
            logger.info("DTESN caching disabled by configuration")
            return None
        
        # Create and initialize cache manager
        cache_manager = initialize_global_cache_manager(**{
            k: v for k, v in config.items() 
            if k != 'enabled'
        })
        
        await cache_manager.initialize()
        
        # Store in app state if provided
        if app_state is not None:
            app_state.dtesn_cache_manager = cache_manager
        
        logger.info(f"✅ DTESN cache manager initialized with strategy: {config['cache_strategy'].value}")
        logger.info(f"   Memory entries: {config['max_memory_entries']}")
        logger.info(f"   Compressed entries: {config['max_compressed_entries']}")
        logger.info(f"   Redis URL: {config['redis_url'] or 'Not configured'}")
        logger.info(f"   Default TTL: {config['default_ttl_seconds']}s")
        
        return cache_manager
        
    except Exception as e:
        logger.error(f"Failed to initialize DTESN cache manager: {e}")
        return None


async def shutdown_dtesn_cache(app_state: Optional[Any] = None) -> None:
    """
    Shutdown the DTESN cache manager gracefully.
    
    Args:
        app_state: Optional FastAPI application state containing cache manager
    """
    try:
        # Clear from app state if provided
        if app_state is not None and hasattr(app_state, 'dtesn_cache_manager'):
            delattr(app_state, 'dtesn_cache_manager')
        
        # Shutdown global cache manager
        await shutdown_global_cache_manager()
        
        logger.info("DTESN cache manager shut down successfully")
        
    except Exception as e:
        logger.error(f"Error shutting down DTESN cache manager: {e}")


def add_cache_middleware(app: Any) -> None:
    """
    Add caching-related middleware to the FastAPI application.
    
    Args:
        app: FastAPI application instance
    """
    
    @app.middleware("http")
    async def cache_headers_middleware(request, call_next):
        """Add cache-related headers to responses"""
        
        response = await call_next(request)
        
        # Add cache status headers for DTESN endpoints
        if request.url.path.startswith("/v1/dtesn/"):
            response.headers["X-DTESN-Cache-Available"] = "true"
            
            # Add performance metrics headers if available
            from aphrodite.endpoints.openai.dtesn_cache_manager import get_cache_manager
            cache_manager = get_cache_manager()
            
            if cache_manager:
                try:
                    metrics = cache_manager.get_performance_metrics()
                    response.headers["X-Cache-Hit-Ratio"] = f"{metrics['hit_ratio']:.2%}"
                    response.headers["X-Cache-Performance-Improvement"] = f"{metrics['performance_improvement_percent']:.1f}%"
                except Exception as e:
                    logger.debug(f"Failed to add cache metrics headers: {e}")
        
        return response
    
    logger.info("Cache middleware added to FastAPI application")


def add_cache_routes(app: Any, router_prefix: str = "") -> None:
    """
    Add cache management routes to the FastAPI application.
    
    Args:
        app: FastAPI application instance
        router_prefix: Prefix for cache routes (default: "")
    """
    try:
        from aphrodite.endpoints.openai.dtesn_cached_routes import router as cache_router
        
        # Include the cache router
        app.include_router(cache_router, prefix=router_prefix)
        
        logger.info(f"DTESN cache routes added with prefix: {router_prefix}")
        
    except ImportError as e:
        logger.warning(f"Could not add cache routes: {e}")


# Lifespan event handlers for FastAPI

def create_cache_lifespan_handler():
    """
    Create lifespan event handler for cache initialization and cleanup.
    
    Returns function that can be used as FastAPI lifespan parameter.
    """
    
    async def lifespan(app):
        """FastAPI lifespan handler for cache management"""
        # Startup
        logger.info("Initializing DTESN cache manager...")
        cache_manager = await initialize_dtesn_cache(app.state)
        
        if cache_manager:
            logger.info("DTESN cache manager startup complete")
        else:
            logger.info("DTESN cache manager not initialized (disabled or failed)")
        
        yield
        
        # Shutdown
        logger.info("Shutting down DTESN cache manager...")
        await shutdown_dtesn_cache(app.state)
        logger.info("DTESN cache manager shutdown complete")
    
    return lifespan


# Configuration utilities

def validate_cache_config() -> bool:
    """
    Validate cache configuration settings.
    
    Returns:
        True if configuration is valid, False otherwise
    """
    try:
        config = get_cache_config_from_env()
        
        if not config.get('enabled', True):
            return True  # Valid to be disabled
        
        # Validate numeric settings
        if config['max_memory_entries'] <= 0:
            logger.error("DTESN_CACHE_MAX_MEMORY_ENTRIES must be > 0")
            return False
        
        if config['max_compressed_entries'] <= 0:
            logger.error("DTESN_CACHE_MAX_COMPRESSED_ENTRIES must be > 0")
            return False
        
        if config['default_ttl_seconds'] <= 0:
            logger.error("DTESN_CACHE_DEFAULT_TTL_SECONDS must be > 0")
            return False
        
        if config['compression_threshold'] < 0:
            logger.error("DTESN_CACHE_COMPRESSION_THRESHOLD must be >= 0")
            return False
        
        logger.info("DTESN cache configuration validation passed")
        return True
        
    except Exception as e:
        logger.error(f"Cache configuration validation failed: {e}")
        return False


def log_cache_config():
    """Log the current cache configuration for debugging"""
    try:
        config = get_cache_config_from_env()
        
        logger.info("DTESN Cache Configuration:")
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
        logger.error(f"Failed to log cache configuration: {e}")


# Example integration for existing API server

def integrate_dtesn_cache_with_api_server(app: Any, enable_routes: bool = True, enable_middleware: bool = True):
    """
    Complete integration of DTESN caching with existing API server.
    
    Args:
        app: FastAPI application instance
        enable_routes: Whether to add cache management routes
        enable_middleware: Whether to add cache middleware
    """
    
    # Validate configuration first
    if not validate_cache_config():
        logger.error("Invalid cache configuration - skipping cache integration")
        return
    
    # Log configuration
    log_cache_config()
    
    # Add middleware if enabled
    if enable_middleware:
        add_cache_middleware(app)
    
    # Add routes if enabled
    if enable_routes:
        add_cache_routes(app)
    
    # Set up lifespan handler
    app.router.lifespan = create_cache_lifespan_handler()
    
    logger.info("DTESN cache integration complete")


__all__ = [
    'initialize_dtesn_cache',
    'shutdown_dtesn_cache', 
    'add_cache_middleware',
    'add_cache_routes',
    'create_cache_lifespan_handler',
    'validate_cache_config',
    'log_cache_config',
    'integrate_dtesn_cache_with_api_server',
    'get_cache_config_from_env'
]