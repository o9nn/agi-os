#!/usr/bin/env python3
"""
Cached DTESN routes for OpenAI-compatible API endpoints.

Provides FastAPI routes that integrate DTESN processing with server-side caching
for optimal performance improvements on repeated requests.
"""

import logging
import time
from typing import Any, Dict, Optional, Union
from collections.abc import AsyncGenerator

from fastapi import APIRouter, Depends, HTTPException, Request, BackgroundTasks
from fastapi.responses import JSONResponse, StreamingResponse
from pydantic import BaseModel, Field

from aphrodite.endpoints.openai.protocol import (
    ChatCompletionRequest,
    ChatCompletionResponse,
    CompletionRequest,
    CompletionResponse,
    ErrorResponse
)
from aphrodite.endpoints.openai.dtesn_integration import (
    DTESNEnhancedRequest,
    DTESNEnhancedOpenAIServingChat,
    DTESNEnhancedOpenAIServingCompletion,
    extract_dtesn_options,
    is_dtesn_request
)
from aphrodite.endpoints.openai.dtesn_cache_manager import (
    get_cache_manager,
    CacheStrategy
)

logger = logging.getLogger(__name__)

# Create router for cached DTESN endpoints
router = APIRouter(prefix="/v1/dtesn", tags=["DTESN Cached Processing"])


class CacheControlRequest(BaseModel):
    """Request model for cache control operations"""
    
    action: str = Field(..., description="Cache action: invalidate_tags, invalidate_model, get_metrics, clear_all")
    targets: Optional[Union[str, list]] = Field(None, description="Target tags, model IDs, or keys to act on")
    force: bool = Field(default=False, description="Force operation even if risky")


class CacheMetricsResponse(BaseModel):
    """Response model for cache performance metrics"""
    
    total_requests: int
    cache_hits: int
    cache_misses: int
    hit_ratio: float
    performance_improvement_percent: float
    memory_usage_bytes: int
    cache_levels: Dict[str, Any]
    strategy: str
    timestamp: float


# Dependency for getting DTESN serving instances
async def get_dtesn_chat_serving() -> DTESNEnhancedOpenAIServingChat:
    """Dependency to get DTESN-enhanced chat serving instance"""
    # This would normally be injected from the main API server
    # For now, we'll use a placeholder that would be initialized elsewhere
    chat_serving = getattr(router, '_dtesn_chat_serving', None)
    if not chat_serving:
        raise HTTPException(
            status_code=503,
            detail="DTESN chat serving not available - service not initialized"
        )
    return chat_serving


async def get_dtesn_completion_serving() -> DTESNEnhancedOpenAIServingCompletion:
    """Dependency to get DTESN-enhanced completion serving instance"""
    completion_serving = getattr(router, '_dtesn_completion_serving', None)
    if not completion_serving:
        raise HTTPException(
            status_code=503,
            detail="DTESN completion serving not available - service not initialized"
        )
    return completion_serving


# Main DTESN processing endpoints with caching

@router.post("/chat/completions")
async def create_cached_chat_completion(
    request: ChatCompletionRequest,
    raw_request: Request,
    background_tasks: BackgroundTasks,
    dtesn_serving: DTESNEnhancedOpenAIServingChat = Depends(get_dtesn_chat_serving)
) -> Union[ChatCompletionResponse, StreamingResponse]:
    """
    Create chat completion with DTESN processing and intelligent caching.
    
    This endpoint provides full OpenAI Chat Completions API compatibility
    while adding optional DTESN processing with server-side caching for
    significant performance improvements on repeated requests.
    """
    start_time = time.time()
    
    try:
        # Extract DTESN and caching options from request
        dtesn_options = None
        if hasattr(request, 'model_extra') and request.model_extra:
            dtesn_options = extract_dtesn_options(request.model_extra)
        
        # If no DTESN options in model_extra, check request body
        if not dtesn_options:
            request_dict = request.dict() if hasattr(request, 'dict') else {}
            dtesn_options = extract_dtesn_options(request_dict)
        
        # Process request with caching
        response = await dtesn_serving.create_chat_completion_with_dtesn(
            request=request,
            raw_request=raw_request,
            dtesn_options=dtesn_options
        )
        
        # Add performance headers
        processing_time = (time.time() - start_time) * 1000
        
        if isinstance(response, ChatCompletionResponse):
            # Add caching metadata to response headers
            cache_manager = get_cache_manager()
            if cache_manager and hasattr(response, 'dtesn_metadata'):
                dtesn_meta = response.dtesn_metadata
                headers = {
                    "X-DTESN-Processed": "true",
                    "X-Cache-Hit": str(dtesn_meta.get("cache_hit", False)).lower(),
                    "X-Performance-Improvement": f"{dtesn_meta.get('performance_improvement', 0):.1%}",
                    "X-Processing-Time-Ms": str(processing_time)
                }
                
                return JSONResponse(
                    content=response.dict(),
                    headers=headers
                )
        
        # Schedule cache maintenance in background
        if dtesn_options and dtesn_options.enable_caching:
            background_tasks.add_task(_schedule_cache_maintenance)
        
        return response
        
    except Exception as e:
        logger.error(f"Cached chat completion failed: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"DTESN cached chat completion failed: {str(e)}"
        )


@router.post("/completions") 
async def create_cached_completion(
    request: CompletionRequest,
    raw_request: Request,
    background_tasks: BackgroundTasks,
    dtesn_serving: DTESNEnhancedOpenAIServingCompletion = Depends(get_dtesn_completion_serving)
) -> Union[CompletionResponse, StreamingResponse]:
    """
    Create completion with DTESN processing and intelligent caching.
    
    This endpoint provides full OpenAI Completions API compatibility
    while adding optional DTESN processing with server-side caching.
    """
    start_time = time.time()
    
    try:
        # Extract DTESN and caching options
        request_dict = request.dict() if hasattr(request, 'dict') else {}
        dtesn_options = extract_dtesn_options(request_dict)
        
        # Process request with caching
        response = await dtesn_serving.create_completion_with_dtesn(
            request=request,
            raw_request=raw_request,
            dtesn_options=dtesn_options
        )
        
        # Add performance headers
        processing_time = (time.time() - start_time) * 1000
        
        if isinstance(response, CompletionResponse):
            # Add caching metadata to response headers
            cache_manager = get_cache_manager()
            if cache_manager and hasattr(response, 'dtesn_metadata'):
                dtesn_meta = response.dtesn_metadata
                headers = {
                    "X-DTESN-Processed": "true",
                    "X-Cache-Hit": str(dtesn_meta.get("cache_hit", False)).lower(),
                    "X-Performance-Improvement": f"{dtesn_meta.get('performance_improvement', 0):.1%}",
                    "X-Processing-Time-Ms": str(processing_time)
                }
                
                return JSONResponse(
                    content=response.dict(),
                    headers=headers
                )
        
        # Schedule cache maintenance
        if dtesn_options and dtesn_options.enable_caching:
            background_tasks.add_task(_schedule_cache_maintenance)
        
        return response
        
    except Exception as e:
        logger.error(f"Cached completion failed: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"DTESN cached completion failed: {str(e)}"
        )


# Cache management endpoints

@router.get("/cache/metrics")
async def get_cache_metrics() -> CacheMetricsResponse:
    """
    Get comprehensive cache performance metrics.
    
    Returns detailed information about cache performance, hit ratios,
    and performance improvements achieved through caching.
    """
    cache_manager = get_cache_manager()
    if not cache_manager:
        raise HTTPException(
            status_code=503,
            detail="Cache manager not available"
        )
    
    metrics = cache_manager.get_performance_metrics()
    
    return CacheMetricsResponse(
        total_requests=metrics["total_requests"],
        cache_hits=metrics["cache_hits"],
        cache_misses=metrics["cache_misses"],
        hit_ratio=metrics["hit_ratio"],
        performance_improvement_percent=metrics["performance_improvement_percent"],
        memory_usage_bytes=metrics["memory_usage_bytes"],
        cache_levels=metrics["cache_levels"],
        strategy=metrics["cache_strategy"],
        timestamp=time.time()
    )


@router.post("/cache/control")
async def cache_control(
    control_request: CacheControlRequest,
    background_tasks: BackgroundTasks
) -> JSONResponse:
    """
    Control cache operations like invalidation and metrics collection.
    
    Supports actions:
    - invalidate_tags: Invalidate cache entries by content tags
    - invalidate_model: Invalidate all entries for a model
    - clear_all: Clear entire cache (requires force=true)
    - get_metrics: Get detailed cache metrics
    """
    cache_manager = get_cache_manager()
    if not cache_manager:
        raise HTTPException(
            status_code=503,
            detail="Cache manager not available"
        )
    
    try:
        result = {"action": control_request.action, "success": False, "details": {}}
        
        if control_request.action == "invalidate_tags":
            if not control_request.targets:
                raise HTTPException(
                    status_code=400,
                    detail="Tags required for invalidate_tags action"
                )
            
            tags = set(control_request.targets if isinstance(control_request.targets, list) else [control_request.targets])
            invalidated_count = await cache_manager.invalidate_by_tags(tags)
            
            result.update({
                "success": True,
                "details": {
                    "invalidated_count": invalidated_count,
                    "tags": list(tags)
                }
            })
            
        elif control_request.action == "invalidate_model":
            if not control_request.targets:
                raise HTTPException(
                    status_code=400,
                    detail="Model ID required for invalidate_model action"
                )
            
            model_id = control_request.targets if isinstance(control_request.targets, str) else control_request.targets[0]
            invalidated_count = await cache_manager.invalidate_by_model(model_id)
            
            result.update({
                "success": True,
                "details": {
                    "invalidated_count": invalidated_count,
                    "model_id": model_id
                }
            })
            
        elif control_request.action == "clear_all":
            if not control_request.force:
                raise HTTPException(
                    status_code=400,
                    detail="clear_all action requires force=true"
                )
            
            # Schedule cache clearing in background
            background_tasks.add_task(_clear_all_caches)
            
            result.update({
                "success": True,
                "details": {"message": "Cache clearing scheduled in background"}
            })
            
        elif control_request.action == "get_metrics":
            metrics = cache_manager.get_performance_metrics()
            result.update({
                "success": True,
                "details": metrics
            })
            
        else:
            raise HTTPException(
                status_code=400,
                detail=f"Unknown action: {control_request.action}"
            )
        
        return JSONResponse(content=result)
        
    except Exception as e:
        logger.error(f"Cache control operation failed: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Cache control failed: {str(e)}"
        )


@router.get("/cache/status")
async def get_cache_status() -> JSONResponse:
    """
    Get basic cache status and health information.
    """
    cache_manager = get_cache_manager()
    
    status = {
        "cache_manager_available": cache_manager is not None,
        "timestamp": time.time()
    }
    
    if cache_manager:
        metrics = cache_manager.get_performance_metrics()
        status.update({
            "redis_enabled": metrics["cache_levels"]["redis_enabled"],
            "total_requests": metrics["total_requests"],
            "hit_ratio": metrics["hit_ratio"],
            "performance_improvement": metrics["performance_improvement_percent"],
            "memory_entries": metrics["cache_levels"]["memory_entries"],
            "compressed_entries": metrics["cache_levels"]["compressed_entries"]
        })
    
    return JSONResponse(content=status)


# Background task functions

async def _schedule_cache_maintenance():
    """Background task to perform cache maintenance"""
    cache_manager = get_cache_manager()
    if cache_manager:
        try:
            # This could trigger cleanup, metrics collection, etc.
            logger.debug("Cache maintenance task executed")
        except Exception as e:
            logger.error(f"Cache maintenance failed: {e}")


async def _clear_all_caches():
    """Background task to clear all cache levels"""
    cache_manager = get_cache_manager()
    if cache_manager:
        try:
            # Clear memory cache
            cache_manager.memory_cache.clear()
            cache_manager.compressed_cache.clear()
            cache_manager.cache_metadata.clear()
            
            # Clear Redis if available
            if cache_manager.redis_enabled and cache_manager.redis_client:
                await cache_manager.redis_client.flushdb()
            
            logger.info("All cache levels cleared")
        except Exception as e:
            logger.error(f"Cache clearing failed: {e}")


# Router configuration function for initialization

def configure_dtesn_cached_routes(
    chat_serving: DTESNEnhancedOpenAIServingChat,
    completion_serving: DTESNEnhancedOpenAIServingCompletion
):
    """
    Configure the router with DTESN serving instances.
    
    This should be called during API server initialization to inject
    the proper serving instances.
    """
    router._dtesn_chat_serving = chat_serving
    router._dtesn_completion_serving = completion_serving
    
    logger.info("DTESN cached routes configured successfully")


# Export the router for inclusion in main API server
__all__ = ["router", "configure_dtesn_cached_routes"]