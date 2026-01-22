import logging
import time
from typing import Any, Dict, Optional, Union
from collections.abc import AsyncGenerator
from fastapi import APIRouter, Depends, HTTPException, Request, BackgroundTasks
from fastapi.responses import JSONResponse, StreamingResponse
from pydantic import BaseModel, Field
from aphrodite.endpoints.openai.protocol import ChatCompletionRequest, ChatCompletionResponse, CompletionRequest, CompletionResponse, ErrorResponse
from aphrodite.endpoints.openai.dtesn_integration import DTESNEnhancedRequest, DTESNEnhancedOpenAIServingChat, DTESNEnhancedOpenAIServingCompletion, extract_dtesn_options, is_dtesn_request
from aphrodite.endpoints.openai.dtesn_cache_manager import get_cache_manager, CacheStrategy
logger = logging.getLogger(__name__)
router = APIRouter(prefix='/v1/dtesn', tags=['DTESN Cached Processing'])
class CacheControlRequest(BaseModel):
    action: str = Field(..., description='Cache action: invalidate_tags, invalidate_model, get_metrics, clear_all')
    targets: Optional[Union[str, list]] = Field(None, description='Target tags, model IDs, or keys to act on')
    force: bool = Field(default=False, description='Force operation even if risky')
class CacheMetricsResponse(BaseModel):
    total_requests: int
    cache_hits: int
    cache_misses: int
    hit_ratio: float
    performance_improvement_percent: float
    memory_usage_bytes: int
    cache_levels: Dict[str, Any]
    strategy: str
    timestamp: float
async def get_dtesn_chat_serving() -> DTESNEnhancedOpenAIServingChat:
    chat_serving = getattr(router, '_dtesn_chat_serving', None)
    if not chat_serving:
        raise HTTPException(status_code=503, detail='DTESN chat serving not available - service not initialized')
    return chat_serving
async def get_dtesn_completion_serving() -> DTESNEnhancedOpenAIServingCompletion:
    completion_serving = getattr(router, '_dtesn_completion_serving', None)
    if not completion_serving:
        raise HTTPException(status_code=503, detail='DTESN completion serving not available - service not initialized')
    return completion_serving
@router.post('/chat/completions')
async def create_cached_chat_completion(request: ChatCompletionRequest, raw_request: Request, background_tasks: BackgroundTasks, dtesn_serving: DTESNEnhancedOpenAIServingChat=Depends(get_dtesn_chat_serving)) -> Union[ChatCompletionResponse, StreamingResponse]:
    start_time = time.time()
    try:
        dtesn_options = None
        if hasattr(request, 'model_extra') and request.model_extra:
            dtesn_options = extract_dtesn_options(request.model_extra)
        if not dtesn_options:
            request_dict = request.dict() if hasattr(request, 'dict') else {}
            dtesn_options = extract_dtesn_options(request_dict)
        response = await dtesn_serving.create_chat_completion_with_dtesn(request=request, raw_request=raw_request, dtesn_options=dtesn_options)
        processing_time = (time.time() - start_time) * 1000
        if isinstance(response, ChatCompletionResponse):
            cache_manager = get_cache_manager()
            if cache_manager and hasattr(response, 'dtesn_metadata'):
                dtesn_meta = response.dtesn_metadata
                headers = {'X-DTESN-Processed': 'true', 'X-Cache-Hit': str(dtesn_meta.get('cache_hit', False)).lower(), 'X-Performance-Improvement': f"{dtesn_meta.get('performance_improvement', 0):.1%}", 'X-Processing-Time-Ms': str(processing_time)}
                return JSONResponse(content=response.dict(), headers=headers)
        if dtesn_options and dtesn_options.enable_caching:
            background_tasks.add_task(_schedule_cache_maintenance)
        return response
    except Exception as e:
        logger.error(f'Cached chat completion failed: {e}')
        raise HTTPException(status_code=500, detail=f'DTESN cached chat completion failed: {str(e)}')
@router.post('/completions')
async def create_cached_completion(request: CompletionRequest, raw_request: Request, background_tasks: BackgroundTasks, dtesn_serving: DTESNEnhancedOpenAIServingCompletion=Depends(get_dtesn_completion_serving)) -> Union[CompletionResponse, StreamingResponse]:
    start_time = time.time()
    try:
        request_dict = request.dict() if hasattr(request, 'dict') else {}
        dtesn_options = extract_dtesn_options(request_dict)
        response = await dtesn_serving.create_completion_with_dtesn(request=request, raw_request=raw_request, dtesn_options=dtesn_options)
        processing_time = (time.time() - start_time) * 1000
        if isinstance(response, CompletionResponse):
            cache_manager = get_cache_manager()
            if cache_manager and hasattr(response, 'dtesn_metadata'):
                dtesn_meta = response.dtesn_metadata
                headers = {'X-DTESN-Processed': 'true', 'X-Cache-Hit': str(dtesn_meta.get('cache_hit', False)).lower(), 'X-Performance-Improvement': f"{dtesn_meta.get('performance_improvement', 0):.1%}", 'X-Processing-Time-Ms': str(processing_time)}
                return JSONResponse(content=response.dict(), headers=headers)
        if dtesn_options and dtesn_options.enable_caching:
            background_tasks.add_task(_schedule_cache_maintenance)
        return response
    except Exception as e:
        logger.error(f'Cached completion failed: {e}')
        raise HTTPException(status_code=500, detail=f'DTESN cached completion failed: {str(e)}')
@router.get('/cache/metrics')
async def get_cache_metrics() -> CacheMetricsResponse:
    cache_manager = get_cache_manager()
    if not cache_manager:
        raise HTTPException(status_code=503, detail='Cache manager not available')
    metrics = cache_manager.get_performance_metrics()
    return CacheMetricsResponse(total_requests=metrics['total_requests'], cache_hits=metrics['cache_hits'], cache_misses=metrics['cache_misses'], hit_ratio=metrics['hit_ratio'], performance_improvement_percent=metrics['performance_improvement_percent'], memory_usage_bytes=metrics['memory_usage_bytes'], cache_levels=metrics['cache_levels'], strategy=metrics['cache_strategy'], timestamp=time.time())
@router.post('/cache/control')
async def cache_control(control_request: CacheControlRequest, background_tasks: BackgroundTasks) -> JSONResponse:
    cache_manager = get_cache_manager()
    if not cache_manager:
        raise HTTPException(status_code=503, detail='Cache manager not available')
    try:
        result = {'action': control_request.action, 'success': False, 'details': {}}
        if control_request.action == 'invalidate_tags':
            if not control_request.targets:
                raise HTTPException(status_code=400, detail='Tags required for invalidate_tags action')
            tags = set(control_request.targets if isinstance(control_request.targets, list) else [control_request.targets])
            invalidated_count = await cache_manager.invalidate_by_tags(tags)
            result.update({'success': True, 'details': {'invalidated_count': invalidated_count, 'tags': list(tags)}})
        elif control_request.action == 'invalidate_model':
            if not control_request.targets:
                raise HTTPException(status_code=400, detail='Model ID required for invalidate_model action')
            model_id = control_request.targets if isinstance(control_request.targets, str) else control_request.targets[0]
            invalidated_count = await cache_manager.invalidate_by_model(model_id)
            result.update({'success': True, 'details': {'invalidated_count': invalidated_count, 'model_id': model_id}})
        elif control_request.action == 'clear_all':
            if not control_request.force:
                raise HTTPException(status_code=400, detail='clear_all action requires force=true')
            background_tasks.add_task(_clear_all_caches)
            result.update({'success': True, 'details': {'message': 'Cache clearing scheduled in background'}})
        elif control_request.action == 'get_metrics':
            metrics = cache_manager.get_performance_metrics()
            result.update({'success': True, 'details': metrics})
        else:
            raise HTTPException(status_code=400, detail=f'Unknown action: {control_request.action}')
        return JSONResponse(content=result)
    except Exception as e:
        logger.error(f'Cache control operation failed: {e}')
        raise HTTPException(status_code=500, detail=f'Cache control failed: {str(e)}')
@router.get('/cache/status')
async def get_cache_status() -> JSONResponse:
    cache_manager = get_cache_manager()
    status = {'cache_manager_available': cache_manager is not None, 'timestamp': time.time()}
    if cache_manager:
        metrics = cache_manager.get_performance_metrics()
        status.update({'redis_enabled': metrics['cache_levels']['redis_enabled'], 'total_requests': metrics['total_requests'], 'hit_ratio': metrics['hit_ratio'], 'performance_improvement': metrics['performance_improvement_percent'], 'memory_entries': metrics['cache_levels']['memory_entries'], 'compressed_entries': metrics['cache_levels']['compressed_entries']})
    return JSONResponse(content=status)
async def _schedule_cache_maintenance():
    cache_manager = get_cache_manager()
    if cache_manager:
        try:
            logger.debug('Cache maintenance task executed')
        except Exception as e:
            logger.error(f'Cache maintenance failed: {e}')
async def _clear_all_caches():
    cache_manager = get_cache_manager()
    if cache_manager:
        try:
            cache_manager.memory_cache.clear()
            cache_manager.compressed_cache.clear()
            cache_manager.cache_metadata.clear()
            if cache_manager.redis_enabled and cache_manager.redis_client:
                await cache_manager.redis_client.flushdb()
            logger.info('All cache levels cleared')
        except Exception as e:
            logger.error(f'Cache clearing failed: {e}')
def configure_dtesn_cached_routes(chat_serving: DTESNEnhancedOpenAIServingChat, completion_serving: DTESNEnhancedOpenAIServingCompletion):
    router._dtesn_chat_serving = chat_serving
    router._dtesn_completion_serving = completion_serving
    logger.info('DTESN cached routes configured successfully')
__all__ = ['router', 'configure_dtesn_cached_routes']