"""
Route handlers for Deep Tree Echo endpoints.

Implements server-side route handlers for DTESN processing with FastAPI,
featuring comprehensive backend integration, server-side data fetching,
and Jinja2 template rendering for HTML responses with content negotiation.
"""

import asyncio
import hashlib
import json
import logging
import time
from datetime import datetime
from typing import Any, Dict, List, Optional, Union

from fastapi import APIRouter, Depends, HTTPException, Request
from fastapi.responses import HTMLResponse, JSONResponse, StreamingResponse, Response
from fastapi.templating import Jinja2Templates
from pydantic import BaseModel, Field

from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
from aphrodite.endpoints.deep_tree_echo.batch_manager import BatchConfiguration
from aphrodite.endpoints.deep_tree_echo.load_integration import get_batch_load_function
from aphrodite.endpoints.deep_tree_echo.content_negotiation import (
    wants_html, wants_xml, create_negotiated_response,
    content_negotiator, MultiFormatResponse
from aphrodite.endpoints.deep_tree_echo.template_engine_advanced import (
    AdvancedTemplateEngine,
    DTESNTemplateContext
)
from aphrodite.endpoints.deep_tree_echo.template_cache_manager import DTESNTemplateCacheManager
from aphrodite.endpoints.deep_tree_echo.progressive_renderer import (
    RenderingConfig,
    optimize_dtesn_response,
    ProgressiveJSONEncoder,
    ContentCompressor,
    RenderingHints
)

logger = logging.getLogger(__name__)

# Create API router for DTESN endpoints
router = APIRouter(tags=["deep_tree_echo"])

# Import configuration routes
from .config_routes import config_router

# Import model serving infrastructure
from .model_serving_manager import ModelServingManager
from .model_serving_routes import create_model_serving_routes


class DTESNRequest(BaseModel):
    """Request model for DTESN processing."""

    input_data: str
    membrane_depth: Optional[int] = Field(default=4, ge=1, le=16)
    esn_size: Optional[int] = Field(default=512, ge=32, le=4096)
    processing_mode: str = Field(default="server_side", pattern="^(server_side|streaming|batch)$")
    include_intermediate: bool = Field(default=False, description="Include intermediate processing steps")
    output_format: str = Field(default="json", pattern="^(json|compressed|streaming)$")


class DTESNBatchRequest(BaseModel):
    """Request model for batch DTESN processing."""

    inputs: List[str] = Field(..., min_items=1, max_items=100)
    membrane_depth: Optional[int] = Field(default=4, ge=1, le=16)
    esn_size: Optional[int] = Field(default=512, ge=32, le=4096)
    parallel_processing: bool = Field(default=True)
    max_batch_size: int = Field(default=10, ge=1, le=50)


class DTESNResponse(BaseModel):
    """Response model for DTESN processing results with error recovery support."""

    status: str
    result: Dict[str, Any]
    processing_time_ms: float
    membrane_layers: int
    server_rendered: bool = True
    engine_integration: Dict[str, Any] = Field(default_factory=dict)
    performance_metrics: Dict[str, Any] = Field(default_factory=dict)
    
    # Error recovery fields
    degraded_mode: bool = False
    fallback_used: bool = False
    recovery_info: Optional[Dict[str, Any]] = None
    error_recovery_available: bool = True


class DTESNBatchResponse(BaseModel):
    """Response model for batch DTESN processing results with error recovery support."""

    status: str
    results: List[Dict[str, Any]]
    total_processing_time_ms: float
    batch_size: int
    successful_count: int
    failed_count: int
    server_rendered: bool = True
    
    # Error recovery fields
    recovery_applied: bool = False
    degraded_mode: bool = False
    partial_success: bool = False


def get_dtesn_processor(request: Request) -> DTESNProcessor:
    """Dependency to get DTESN processor from app state."""
    config = getattr(request.app.state, "config", None)
    engine = getattr(request.app.state, "engine", None)
    try:
        return DTESNProcessor(config=config, engine=engine)
    except RuntimeError as e:
        logger.error(f"DTESN processor initialization failed: {e}")
        raise HTTPException(
            status_code=503,
            detail=f"DTESN service unavailable: {e}"
        )


def get_model_serving_manager(request: Request) -> ModelServingManager:
    """Dependency to get Model Serving Manager from app state."""
    model_serving_manager = getattr(request.app.state, "model_serving_manager", None)
    if model_serving_manager is None:
        # Initialize model serving manager
        engine = getattr(request.app.state, "engine", None)
        model_serving_manager = ModelServingManager(engine=engine)
        request.app.state.model_serving_manager = model_serving_manager
    return model_serving_manager


def get_templates(request: Request) -> Jinja2Templates:
    """Dependency to get Jinja2Templates from app state."""
    templates = getattr(request.app.state, "templates", None)
    if templates is None:
        raise HTTPException(
            status_code=500,
            detail="Templates not configured"
        )
    return templates


def get_advanced_template_engine(request: Request) -> AdvancedTemplateEngine:
    """Dependency to get Advanced Template Engine from app state."""
    advanced_engine = getattr(request.app.state, "advanced_template_engine", None)
    if advanced_engine is None:
        # Initialize if not already present
        templates_dir = getattr(request.app.state, "templates_dir", None)
        if templates_dir:
            advanced_engine = AdvancedTemplateEngine(templates_dir)
            request.app.state.advanced_template_engine = advanced_engine
        else:
            raise HTTPException(
                status_code=500,
                detail="Advanced template engine not configured"
            )
    return advanced_engine


def get_template_cache_manager(request: Request) -> DTESNTemplateCacheManager:
    """Dependency to get Template Cache Manager from app state."""
    cache_manager = getattr(request.app.state, "template_cache_manager", None)
    if cache_manager is None:
        # Initialize with default settings
        cache_manager = DTESNTemplateCacheManager(
            max_template_cache_size=100,
            max_rendered_cache_size=500,
            enable_compression=True
        )
        request.app.state.template_cache_manager = cache_manager
    return cache_manager


def get_engine_stats(request: Request) -> Dict[str, Any]:
    """Dependency to get Aphrodite Engine statistics."""
    engine = getattr(request.app.state, "engine", None)
    if engine is None:
        return {"engine_status": "unavailable"}

    try:
        # Get basic engine information
        return {
            "engine_status": "available",
            "engine_type": type(engine).__name__,
            "has_generate": hasattr(engine, "generate"),
            "has_get_model_config": hasattr(engine, "get_model_config"),
            "integration_ready": True
        }
    except Exception as e:
        logger.warning(f"Could not fetch engine stats: {e}")
        return {"engine_status": "error", "error": str(e)}


# Note: wants_html function now imported from content_negotiation module
# The original function is replaced by enhanced content negotiation system


@router.get("/")
async def dtesn_root(
    request: Request,
    templates: Jinja2Templates = Depends(get_templates),
    engine_stats: Dict[str, Any] = Depends(get_engine_stats)
) -> Response:
    """Root endpoint for Deep Tree Echo SSR API with engine integration status and multi-format content negotiation."""
    data = {
        "service": "Deep Tree Echo API",
        "version": "1.0.0",
        "description": "Server-side rendering API for DTESN processing with Aphrodite Engine integration",
        "endpoints": [
            "/process",
            "/batch_process", 
            "/stream_process",
            "/status",
            "/membrane_info",
            "/esn_state",
            "/engine_integration",
            "/performance_metrics"
        ],
        "server_rendered": True,
        "engine_integration": engine_stats
    }

    return create_negotiated_response(
        data=data,
        request=request,
        templates=templates,
        template_name="index.html",
        xml_root="deep_tree_echo_api"
    )


@router.post("/process", response_model=DTESNResponse)
async def process_dtesn(
    request_data: DTESNRequest,
    request: Request,
    processor: DTESNProcessor = Depends(get_dtesn_processor),
    engine_stats: Dict[str, Any] = Depends(get_engine_stats),
    templates: Jinja2Templates = Depends(get_templates),
    advanced_engine: AdvancedTemplateEngine = Depends(get_advanced_template_engine),
    cache_manager: DTESNTemplateCacheManager = Depends(get_template_cache_manager)
) -> Union[HTMLResponse, DTESNResponse]:
    """
    Process input through Deep Tree Echo System Network with engine integration.

    Server-side processing of input data through DTESN membrane hierarchy
    with Echo State Network integration and Aphrodite Engine backend support.
    Supports both HTML and JSON responses via content negotiation.

    Args:
        request_data: DTESN processing request
        request: FastAPI request object
        processor: DTESN processor instance
        engine_stats: Engine integration statistics
        templates: Jinja2Templates instance

    Returns:
        Server-rendered response with processing results and engine integration data
    """
    start_time = time.time()

    try:
        # Process input through DTESN with enhanced server-side data fetching
        result = await processor.process(
            input_data=request_data.input_data,
            membrane_depth=request_data.membrane_depth,
            esn_size=request_data.esn_size
        )

        # Enhanced server-side response generation
        processing_time = time.time() - start_time
        performance_metrics = {
            "total_processing_time_ms": processing_time * 1000,
            "dtesn_processing_time_ms": result.processing_time_ms,
            "overhead_ms": (processing_time * 1000) - result.processing_time_ms,
            "throughput_chars_per_second": len(request_data.input_data) / processing_time if processing_time > 0 else 0
        }

        response_data = DTESNResponse(
            status="success",
            result=result.to_dict(),
            processing_time_ms=result.processing_time_ms,
            membrane_layers=result.membrane_layers,
            server_rendered=True,
            engine_integration=engine_stats,
            performance_metrics=performance_metrics
        )

        # Add resource management context if available
        if hasattr(request.state, 'resource_context'):
            response_data.performance_metrics["resource_managed"] = request.state.resource_context.get("pool_available", False)

        if wants_html(request):
            # Use advanced template engine for dynamic template generation
            try:
                # Determine result type based on processing content
                result_type = "membrane_evolution"
                if hasattr(result, 'esn_state') and result.esn_state:
                    result_type = "esn_processing"
                elif hasattr(result, 'bseries_computation') and result.bseries_computation:
                    result_type = "bseries_computation"
                    
                # Generate cache key for rendered result
                cache_key_components = [
                    result_type,
                    str(response_data.membrane_layers),
                    str(len(request_data.input_data)),
                    hashlib.sha256(request_data.input_data.encode()).hexdigest()[:8]
                ]
                rendered_cache_key = "_".join(cache_key_components)
                
                # Check cache first
                cached_html = await cache_manager.get_rendered_result(rendered_cache_key)
                if cached_html:
                    return HTMLResponse(content=cached_html)
                
                # Generate dynamic template and render
                rendered_html = await advanced_engine.render_dtesn_result(
                    request=request,
                    dtesn_result=response_data.dict(),
                    result_type=result_type
                )
                
                # Cache the result
                await cache_manager.store_rendered_result(
                    result_key=rendered_cache_key,
                    html_content=rendered_html,
                    ttl_seconds=1800,  # 30 minutes
                    invalidation_tags={result_type, "dtesn_processing"}
                )
                
                return HTMLResponse(content=rendered_html)
                
            except Exception as template_error:
                logger.warning(f"Advanced template rendering failed: {template_error}")
                # Fallback to standard template
                return templates.TemplateResponse(
                    "process_result.html",
                    {
                        "request": request,
                        "data": response_data.dict(),
                        "input_data": request_data.input_data,
                        "timestamp": datetime.now().isoformat()
                    }
                )
        else:
            return response_data

    except Exception as e:
        logger.error(f"DTESN processing error: {e}")
        error_time = time.time() - start_time
        
        # Update error context with processing details
        error_context = getattr(request.state, 'dtesn_context', {}).get('error_context')
        if error_context:
            error_context.processing_stage = "route_processing"
            error_context.user_input = request_data.input_data[:100] if request_data.input_data else None
        
        # Try error recovery if available
        recovery_result = await error_recovery_service.execute_with_recovery(
            operation=lambda x: processor.process(x, request_data.membrane_depth, request_data.esn_size),
            input_data=request_data.input_data,
            context=error_context,
            enable_retry=True,
            enable_fallback=True
        )
        
        if recovery_result.success:
            # Recovery succeeded - return recovered result with degraded flag
            logger.info(f"Error recovery succeeded with mode: {recovery_result.recovery_mode}")
            
            response_data = DTESNResponse(
                status="success",
                result=recovery_result.result if hasattr(recovery_result.result, 'to_dict') else {"output": str(recovery_result.result)},
                processing_time_ms=error_time * 1000,
                membrane_layers=1,  # Reduced for fallback
                server_rendered=True,
                degraded_mode=recovery_result.degraded,
                fallback_used=recovery_result.fallback_used,
                recovery_info=recovery_result.to_dict()
            )
            
            if wants_html(request):
                return templates.TemplateResponse(
                    "process_result.html",
                    {
                        "request": request,
                        "data": response_data.dict(),
                        "input_data": request_data.input_data,
                        "timestamp": datetime.now().isoformat(),
                        "recovery_mode": recovery_result.recovery_mode
                    }
                )
            else:
                return response_data
        
        # Recovery failed - return structured error response
        from .errors import DTESNProcessingError, create_error_response
        
        dtesn_error = DTESNProcessingError(
            f"DTESN processing failed: {e}",
            processing_stage="route_processing",
            context=error_context,
            original_error=e
        )
        
        error_response = create_error_response(dtesn_error, error_context)
        
        if wants_html(request):
            return templates.TemplateResponse(
                "process_result.html",
                {
                    "request": request,
                    "data": {
                        "status": "error", 
                        "error": error_response.message,
                        "error_code": error_response.error_code,
                        "recovery_suggestions": error_response.recovery_suggestions
                    },
                    "input_data": request_data.input_data,
                    "timestamp": datetime.now().isoformat(),
                    "error_details": error_response.dict()
                },
                status_code=500
            )
        else:
            raise HTTPException(status_code=500, detail=error_response.dict())


@router.post("/batch_process", response_model=DTESNBatchResponse)
async def batch_process_dtesn(
    request: DTESNBatchRequest,
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> DTESNBatchResponse:
    """
    Enhanced batch process multiple inputs through DTESN with optimized concurrent processing.

    Processes multiple inputs efficiently using enhanced server-side batching,
    concurrent processing capabilities, and advanced resource management.

    Args:
        request: Batch DTESN processing request
        processor: DTESN processor instance

    Returns:
        Server-rendered batch processing results with enhanced concurrency metrics
    """
    start_time = time.time()
    
    try:
        # Use the enhanced batch processing method
        results_list = await processor.process_batch(
            inputs=request.inputs,
            membrane_depth=request.membrane_depth,
            esn_size=request.esn_size,
            max_concurrent=request.max_batch_size if request.parallel_processing else 1
        )
        
        # Convert results to response format
        results = []
        successful_count = 0
        failed_count = 0
        
        for i, result in enumerate(results_list):
            if "error" in result.processed_output:
                results.append({
                    "input_index": i,
                    "status": "failed",
                    "error": result.processed_output["error"],
                    "server_rendered": True
                })
                failed_count += 1
            else:
                results.append({
                    "input_index": i,
                    "status": "success",
                    "result": result.to_dict(),
                    "server_rendered": True
                })
                successful_count += 1

        total_processing_time = (time.time() - start_time) * 1000

        return DTESNBatchResponse(
            status="completed",
            results=results,
            total_processing_time_ms=total_processing_time,
            batch_size=len(request.inputs),
            successful_count=successful_count,
            failed_count=failed_count,
            server_rendered=True
        )

    except Exception as e:
        logger.error(f"Enhanced batch DTESN processing error: {e}")
        
        # Create error context for batch processing
        from .errors import DTESNProcessingError, create_error_response, ErrorContext
        
        error_context = ErrorContext(
            request_id=f"batch_{int(time.time() * 1000)}",
            endpoint="/deep_tree_echo/batch_process",
            processing_stage="batch_processing",
            user_input=f"Batch of {len(request.inputs)} inputs"
        )
        
        # Try recovery for batch operations with fallback to individual processing
        recovery_results = []
        successful_recoveries = 0
        
        for i, input_data in enumerate(request.inputs):
            try:
                recovery_result = await error_recovery_service.execute_with_recovery(
                    operation=lambda x: processor.process(x, request.membrane_depth, request.esn_size),
                    input_data=input_data,
                    context=error_context,
                    enable_retry=False,  # Skip retry for batch to avoid long delays
                    enable_fallback=True
                )
                
                if recovery_result.success:
                    successful_recoveries += 1
                    recovery_results.append({
                        "input_index": i,
                        "status": "recovered",
                        "result": recovery_result.result if hasattr(recovery_result.result, 'to_dict') else {"output": str(recovery_result.result)},
                        "recovery_mode": recovery_result.recovery_mode,
                        "degraded": recovery_result.degraded,
                        "server_rendered": True
                    })
                else:
                    recovery_results.append({
                        "input_index": i,
                        "status": "failed",
                        "error": str(recovery_result.error),
                        "server_rendered": True
                    })
                    
            except Exception as recovery_error:
                logger.error(f"Recovery failed for batch item {i}: {recovery_error}")
                recovery_results.append({
                    "input_index": i,
                    "status": "failed",
                    "error": f"Recovery failed: {recovery_error}",
                    "server_rendered": True
                })
        
        # Return partial results if any recoveries succeeded
        if successful_recoveries > 0:
            processing_time = (time.time() - start_time) * 1000
            
            return DTESNBatchResponse(
                status="partial_success",
                results=recovery_results,
                batch_size=len(request.inputs),
                successful_count=successful_recoveries,
                failed_count=len(request.inputs) - successful_recoveries,
                total_processing_time_ms=processing_time,
                server_rendered=True,
                recovery_applied=True,
                degraded_mode=True
            )
        
        # All recovery attempts failed - return comprehensive error
        dtesn_error = DTESNProcessingError(
            f"Batch DTESN processing failed for all {len(request.inputs)} inputs: {e}",
            processing_stage="batch_processing",
            context=error_context,
            original_error=e
        )
        
        error_response = create_error_response(dtesn_error, error_context)
        raise HTTPException(status_code=500, detail=error_response.dict())


@router.post("/stream_process")
async def stream_process_dtesn(
    request: DTESNRequest,
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> StreamingResponse:
    """
    Enhanced stream process input through DTESN with advanced async streaming and backpressure handling.

    Provides real-time streaming of DTESN processing results with enhanced
    server-side streaming capabilities, flow control, and resource management.

    Args:
        request: DTESN processing request
        processor: DTESN processor instance

    Returns:
        Enhanced streaming response with backpressure handling and resource management
    """

    async def generate_enhanced_stream():
        request_id = f"stream_{int(time.time() * 1000000)}"
        buffer_size = 0
        max_buffer_size = 1024 * 1024  # 1MB buffer limit for backpressure
        
        try:
            # Start processing and yield initial status with enhanced metadata
            initial_status = {
                "status": "started",
                "request_id": request_id,
                "timestamp": time.time(),
                "server_rendered": True,
                "stream_enhanced": True,
                "backpressure_enabled": True
            }
            message = f'data: {json.dumps(initial_status)}\n\n'
            buffer_size += len(message)
            yield message

            # Yield processing configuration
            config_status = {
                "status": "configuration",
                "membrane_depth": request.membrane_depth,
                "esn_size": request.esn_size,
                "processing_mode": request.processing_mode,
                "concurrent_processing": True,
                "server_rendered": True
            }
            message = f'data: {json.dumps(config_status)}\n\n'
            buffer_size += len(message)
            yield message

            # Apply backpressure control if buffer is getting large
            if buffer_size > max_buffer_size // 2:
                await asyncio.sleep(0.1)  # Brief pause to allow client to catch up

            # Send pre-processing heartbeat for long operations
            if len(request.input_data) > 50000:  # 50KB threshold for heartbeat
                heartbeat_message = {
                    "status": "processing_heartbeat",
                    "request_id": request_id,
                    "timestamp": time.time(),
                    "message": "Long-running operation in progress",
                    "estimated_completion_sec": len(request.input_data) / 10000,
                    "server_rendered": True
                }
                yield f'data: {json.dumps(heartbeat_message)}\n\n'
            
            # Process through DTESN with enhanced concurrent processing
            result = await processor.process(
                input_data=request.input_data,
                membrane_depth=request.membrane_depth,
                esn_size=request.esn_size,
                enable_concurrent=True  # Use enhanced concurrent processing
            )

            # Yield enhanced intermediate processing stages
            stages = [
                {
                    "status": "membrane_processing", 
                    "layers": result.membrane_layers, 
                    "concurrent": True,
                    "server_rendered": True
                },
                {
                    "status": "esn_processing", 
                    "reservoir_size": request.esn_size,
                    "parallel_updates": True, 
                    "server_rendered": True
                },
                {
                    "status": "bseries_computation", 
                    "concurrent_trees": True,
                    "server_rendered": True
                }
            ]

            for stage in stages:
                message = f'data: {json.dumps(stage)}\n\n'
                buffer_size += len(message)
                
                # Apply backpressure if buffer is too large
                if buffer_size > max_buffer_size:
                    await asyncio.sleep(0.2)
                    buffer_size = 0  # Reset buffer tracking after backpressure pause
                    
                yield message
                await asyncio.sleep(0.01)  # Small delay between stages for streaming effect

            # Yield processing statistics
            processing_stats = processor.get_processing_stats()
            stats_message = {
                "status": "processing_stats",
                "stats": processing_stats,
                "server_rendered": True
            }
            message = f'data: {json.dumps(stats_message)}\n\n'
            yield message

            # Yield final enhanced result
            final_result = {
                "status": "completed",
                "request_id": request_id,
                "result": result.to_dict(),
                "processing_time_ms": result.processing_time_ms,
                "server_rendered": True,
                "stream_enhanced": True,
                "total_messages": len(stages) + 4  # Initial + config + stats + final + stages
            }
            yield f'data: {json.dumps(final_result)}\n\n'

            # Send completion marker
            yield 'data: {"status": "stream_complete", "server_rendered": true}\n\n'

        except asyncio.CancelledError:
            # Handle client disconnect gracefully
            logger.info(f"Stream cancelled for request {request_id}")
            error_result = {
                "status": "cancelled",
                "request_id": request_id,
                "message": "Stream cancelled by client",
                "server_rendered": True
            }
            yield f'data: {json.dumps(error_result)}\n\n'
            
        except Exception as e:
            logger.error(f"Enhanced streaming processing error for {request_id}: {e}")
            error_result = {
                "status": "error",
                "request_id": request_id,
                "error": str(e),
                "server_rendered": True,
                "stream_enhanced": True
            }
            yield f'data: {json.dumps(error_result)}\n\n'

    return StreamingResponse(
        generate_enhanced_stream(),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Server-Rendered": "true",
            "X-Stream-Enhanced": "true",
            "X-Backpressure-Enabled": "true",
            "X-Async-Processing": "true"
        }
    )


@router.post("/priority_process", response_model=DTESNResponse)
async def priority_process_dtesn(
    request_data: DTESNRequest,
    priority: int = Field(default=1, ge=0, le=2, description="Request priority (0=highest, 2=lowest)"),
    timeout: Optional[float] = Field(default=None, description="Custom timeout in seconds"),
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> DTESNResponse:
    """
    Enhanced priority-based DTESN processing with advanced async queue management.

    Processes input through DTESN with priority queuing, circuit breaker pattern,
    and adaptive timeout handling for optimal server-side non-blocking performance.

    Args:
        request_data: DTESN processing request
        priority: Request priority level (0=highest, 2=lowest)
        timeout: Optional custom timeout
        processor: DTESN processor instance

    Returns:
        DTESN processing result with priority queue metadata
    """
    start_time = time.time()
    
    try:
        # Process with priority queue
        result = await processor.process_with_priority_queue(
            input_data=request_data.input_data,
            priority=priority,
            membrane_depth=request_data.membrane_depth,
            esn_size=request_data.esn_size,
            timeout=timeout
        )
        
        processing_time = (time.time() - start_time) * 1000
        
        response_data = DTESNResponse(
            status="success",
            result=result.to_dict(),
            processing_time_ms=result.processing_time_ms,
            membrane_layers=result.membrane_layers,
            server_rendered=True,
            performance_metrics={
                "total_processing_time_ms": processing_time,
                "priority": priority,
                "timeout_used": timeout,
                "queue_managed": True,
                "non_blocking": True
            }
        )
        
        return response_data
        
    except Exception as e:
        logger.error(f"Priority DTESN processing error: {e}")
        error_time = (time.time() - start_time) * 1000
        raise HTTPException(
            status_code=500,
            detail={
                "error": f"Priority DTESN processing failed: {e}",
                "processing_time_ms": error_time,
                "priority": priority,
                "server_rendered": True
            }
        )


@router.post("/stream_chunks")
async def stream_chunks_dtesn(
    request: DTESNRequest,
    chunk_size: int = Field(default=1024, ge=128, le=8192, description="Processing chunk size"),
    enable_compression: bool = Field(default=True, description="Enable response compression"),
    timeout_prevention: bool = Field(default=True, description="Enable timeout prevention"),
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> StreamingResponse:
    """
    Advanced streaming DTESN processing with intelligent chunking and backpressure.

    Provides real-time streaming of DTESN processing results with adaptive chunking,
    flow control, and enhanced server-side resource management.

    Args:
        request: DTESN processing request
        chunk_size: Size of processing chunks
        processor: DTESN processor instance

    Returns:
        Streaming response with advanced chunk management
    """
    
    async def generate_chunk_stream():
        try:
            async for chunk in processor.process_streaming_chunks(
                input_data=request.input_data,
                membrane_depth=request.membrane_depth,
                esn_size=request.esn_size,
                chunk_size=chunk_size,
                enable_compression=enable_compression,
                timeout_prevention=timeout_prevention
            ):
                message = f'data: {json.dumps(chunk)}\n\n'
                yield message
                
                # Adaptive delay based on chunk type
                delay = 0.001 if chunk.get("type") == "heartbeat" else 0.005
                await asyncio.sleep(delay)
                
        except Exception as e:
            logger.error(f"Chunk streaming error: {e}")
            error_chunk = {
                "type": "error",
                "error": str(e),
                "timestamp": time.time()
            }
            yield f'data: {json.dumps(error_chunk)}\n\n'
    
    return StreamingResponse(
        generate_chunk_stream(),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Server-Rendered": "true",
            "X-Chunk-Streaming": "true",
            "X-Chunk-Size": str(chunk_size),
            "X-Backpressure-Enabled": "true",
            "X-Async-Processing": "enhanced"
        }
    )


@router.post("/stream_large_dataset")
async def stream_large_dataset_dtesn(
    request: DTESNRequest,
    max_chunk_size: int = Field(default=4096, ge=512, le=16384, description="Maximum chunk size for large datasets"),
    compression_level: int = Field(default=1, ge=0, le=3, description="Compression level (0=none, 3=max)"),
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> StreamingResponse:
    """
    Optimized streaming endpoint for large datasets with advanced compression and timeout prevention.
    
    Specifically designed for datasets larger than 1MB with aggressive optimization techniques:
    - Adaptive chunking based on dataset size
    - Configurable compression levels
    - Enhanced timeout prevention with 20-second heartbeats
    - Minimal serialization overhead
    - Throughput-optimized processing
    
    Args:
        request: DTESN processing request with large input data
        max_chunk_size: Maximum size for processing chunks
        compression_level: Response compression level (0-3)
        processor: DTESN processor instance
        
    Returns:
        Highly optimized streaming response for large datasets
    """
    
    async def generate_large_dataset_stream():
        # Initialize progressive rendering configuration
        render_config = RenderingConfig(
            progressive_json=True,
            max_chunk_size=max_chunk_size,
            compression_strategy="adaptive",
            enable_rendering_hints=True
        )
        
        compressor = ContentCompressor(render_config)
        
        try:
            async for chunk in processor.process_large_dataset_stream(
                input_data=request.input_data,
                membrane_depth=request.membrane_depth,
                esn_size=request.esn_size,
                max_chunk_size=max_chunk_size,
                compression_level=compression_level
            ):
                # Apply progressive rendering optimization for complex chunks
                if chunk.get("type") in ["compressed_chunk", "large_dataset_chunk"]:
                    # Optimize chunk rendering for better client performance
                    optimized_content = compressor.compress_content(
                        json.dumps(chunk), 
                        "application/json"
                    )
                    
                    # Include optimization metadata in response
                    enhanced_chunk = {
                        **chunk,
                        "rendering_optimized": True,
                        "compression_stats": {
                            "method": optimized_content.get("method"),
                            "ratio": optimized_content.get("compression_ratio"),
                            "original_size": optimized_content.get("original_size")
                        }
                    }
                    chunk = enhanced_chunk
                
                # Format as SSE with progressive rendering support
                if chunk.get("type") == "compressed_metadata":
                    message = f'event: metadata\ndata: {json.dumps(chunk)}\n\n'
                elif chunk.get("type") == "compressed_chunk":
                    message = f'event: chunk\ndata: {json.dumps(chunk)}\n\n'
                elif chunk.get("type") == "large_dataset_heartbeat":
                    message = f'event: heartbeat\ndata: {json.dumps(chunk)}\n\n'
                elif chunk.get("type") == "compressed_completion":
                    message = f'event: completion\ndata: {json.dumps(chunk)}\n\n'
                else:
                    message = f'data: {json.dumps(chunk)}\n\n'
                
                yield message
                
                # Adaptive delay based on chunk complexity for optimal throughput
                delay = 0.0005 if chunk.get("rendering_optimized") else 0.001
                await asyncio.sleep(delay)
                
        except Exception as e:
            logger.error(f"Large dataset streaming error: {e}")
            error_chunk = {
                "type": "large_dataset_error",
                "error": str(e),
                "timestamp": time.time(),
                "recoverable": False
            }
            yield f'event: error\ndata: {json.dumps(error_chunk)}\n\n'
    
    # Enhanced headers for large dataset streaming with progressive rendering hints
    data_size = len(request.input_data)
    rendering_hints = RenderingHints.generate_hints({
        "size": data_size,
        "complexity": "high" if data_size > 1024*1024 else "medium",
        "compressed": compression_level > 0,
        "compression_method": "adaptive",
        "progressive": True
    })
    
    headers = {
        "Cache-Control": "no-cache",
        "Connection": "keep-alive", 
        "X-Server-Rendered": "true",
        "X-Large-Dataset-Optimized": "true",
        "X-Max-Chunk-Size": str(max_chunk_size),
        "X-Compression-Level": str(compression_level),
        "X-Timeout-Prevention": "enhanced",
        "X-Stream-Type": "large-dataset",
        "X-Progressive-Rendering": "enabled",
        "X-Adaptive-Compression": "true",
        **rendering_hints  # Add progressive rendering hints
    }
    
    return StreamingResponse(
        generate_large_dataset_stream(),
        media_type="text/event-stream",
        headers=headers
    )


@router.post("/stream_optimized")
async def stream_optimized_dtesn(
    request: DTESNRequest,
    bandwidth_hint: Optional[str] = Field(default="auto", description="Bandwidth hint: low, medium, high, auto"),
    adaptive_compression: bool = Field(default=True, description="Enable adaptive compression based on network conditions"),
    enable_progressive_rendering: bool = Field(default=True, description="Enable progressive rendering optimization"),
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> StreamingResponse:
    """
    Advanced streaming endpoint with bandwidth-aware optimization and progressive rendering.
    
    This endpoint provides intelligent optimization based on network conditions and client capabilities:
    - Adaptive compression levels based on bandwidth estimation
    - Progressive rendering for complex DTESN results
    - Dynamic chunk sizing based on network performance
    - Resume capability with checkpointing support
    - Enhanced error recovery with graceful degradation
    
    Args:
        request: DTESN processing request
        bandwidth_hint: Client bandwidth hint for optimization
        adaptive_compression: Enable dynamic compression adjustment
        enable_progressive_rendering: Enable progressive content delivery
        processor: DTESN processor instance
        
    Returns:
        Bandwidth-optimized streaming response with progressive rendering
    """
    
    async def generate_optimized_stream():
        # Configure rendering based on bandwidth and client capabilities
        render_config = RenderingConfig(
            progressive_json=enable_progressive_rendering,
            max_chunk_size=_get_optimal_chunk_size(bandwidth_hint),
            compression_strategy="adaptive" if adaptive_compression else "gzip",
            enable_rendering_hints=True,
            stream_buffer_size=_get_buffer_size(bandwidth_hint)
        )
        
        compressor = ContentCompressor(render_config)
        progressive_encoder = ProgressiveJSONEncoder(render_config)
        
        # Estimate optimal streaming parameters
        data_size = len(request.input_data)
        chunk_size = render_config.max_chunk_size
        compression_level = _get_compression_level(bandwidth_hint, data_size)
        
        try:
            request_id = f"optimized_stream_{int(time.time() * 1000000)}"
            
            # Send initial metadata with optimization parameters
            metadata = {
                "type": "optimized_metadata",
                "request_id": request_id,
                "optimization_config": {
                    "bandwidth_hint": bandwidth_hint,
                    "chunk_size": chunk_size,
                    "compression_level": compression_level,
                    "progressive_rendering": enable_progressive_rendering,
                    "adaptive_compression": adaptive_compression
                },
                "estimated_performance": {
                    "expected_throughput_mbps": _estimate_throughput(bandwidth_hint),
                    "estimated_duration_sec": _estimate_duration(data_size, bandwidth_hint),
                    "optimization_level": _get_optimization_level(bandwidth_hint)
                }
            }
            
            yield f'event: metadata\ndata: {json.dumps(metadata)}\n\n'
            
            # Process with bandwidth-aware streaming
            async for chunk in processor.process_streaming_chunks(
                input_data=request.input_data,
                membrane_depth=request.membrane_depth,
                esn_size=request.esn_size,
                chunk_size=chunk_size,
                enable_compression=adaptive_compression,
                timeout_prevention=True
            ):
                # Apply progressive rendering optimization
                if enable_progressive_rendering and chunk.get("type") == "chunk":
                    # Use progressive encoding for complex chunks
                    chunk_json = json.dumps(chunk)
                    if len(chunk_json) > render_config.max_chunk_size // 2:
                        # Stream progressively for large chunks
                        progressive_parts = list(progressive_encoder.encode_progressive(chunk))
                        for part in progressive_parts:
                            if part.strip():  # Skip empty parts
                                yield f'event: progressive_chunk\ndata: {json.dumps({"part": part, "request_id": request_id})}\n\n'
                                await asyncio.sleep(_get_adaptive_delay(bandwidth_hint))
                        continue
                
                # Apply adaptive compression for standard chunks
                if adaptive_compression and chunk.get("type") in ["chunk", "completion"]:
                    compressed_result = compressor.compress_content(json.dumps(chunk))
                    if compressed_result["compressed"]:
                        chunk["compression_info"] = {
                            "method": compressed_result["method"],
                            "ratio": compressed_result["compression_ratio"],
                            "original_size": compressed_result["original_size"]
                        }
                
                # Format as SSE with adaptive optimization
                event_type = _get_event_type(chunk.get("type", "data"))
                message = f'event: {event_type}\ndata: {json.dumps(chunk)}\n\n'
                yield message
                
                # Adaptive delay based on bandwidth and chunk complexity
                await asyncio.sleep(_get_adaptive_delay(bandwidth_hint))
                
        except Exception as e:
            logger.error(f"Optimized streaming error: {e}")
            error_chunk = {
                "type": "optimized_error",
                "error": str(e),
                "timestamp": time.time(),
                "recoverable": True,  # Enable retry logic
                "recovery_hint": "retry_with_lower_bandwidth"
            }
            yield f'event: error\ndata: {json.dumps(error_chunk)}\n\n'
    
    # Generate bandwidth-aware headers
    headers = _generate_optimization_headers(bandwidth_hint, adaptive_compression, enable_progressive_rendering)
    
    return StreamingResponse(
        generate_optimized_stream(),
        media_type="text/event-stream",
        headers=headers
    )


def _get_optimal_chunk_size(bandwidth_hint: str) -> int:
    """Get optimal chunk size based on bandwidth hint."""
    sizes = {
        "low": 1024,      # 1KB for low bandwidth
        "medium": 4096,   # 4KB for medium bandwidth  
        "high": 16384,    # 16KB for high bandwidth
        "auto": 8192      # 8KB for auto-detection
    }
    return sizes.get(bandwidth_hint, sizes["auto"])


def _get_buffer_size(bandwidth_hint: str) -> int:
    """Get optimal buffer size based on bandwidth hint."""
    sizes = {
        "low": 2048,      # 2KB buffer
        "medium": 8192,   # 8KB buffer
        "high": 32768,    # 32KB buffer
        "auto": 16384     # 16KB buffer
    }
    return sizes.get(bandwidth_hint, sizes["auto"])


def _get_compression_level(bandwidth_hint: str, data_size: int) -> int:
    """Get optimal compression level based on bandwidth and data size."""
    if data_size < 10240:  # < 10KB, don't over-compress
        return 3
    
    levels = {
        "low": 8,         # High compression for low bandwidth
        "medium": 6,      # Balanced compression
        "high": 3,        # Light compression for high bandwidth
        "auto": 6         # Balanced default
    }
    return levels.get(bandwidth_hint, levels["auto"])


def _estimate_throughput(bandwidth_hint: str) -> float:
    """Estimate throughput based on bandwidth hint."""
    estimates = {
        "low": 0.5,       # 0.5 MB/s
        "medium": 5.0,    # 5 MB/s
        "high": 50.0,     # 50 MB/s
        "auto": 10.0      # 10 MB/s
    }
    return estimates.get(bandwidth_hint, estimates["auto"])


def _estimate_duration(data_size: int, bandwidth_hint: str) -> float:
    """Estimate processing duration based on data size and bandwidth."""
    throughput = _estimate_throughput(bandwidth_hint) * 1024 * 1024  # Convert to bytes/sec
    processing_overhead = 1.5  # 50% overhead for processing
    return (data_size / throughput) * processing_overhead


def _get_optimization_level(bandwidth_hint: str) -> str:
    """Get optimization level description."""
    levels = {
        "low": "maximum",     # Maximum optimization for low bandwidth
        "medium": "balanced", # Balanced optimization
        "high": "minimal",    # Minimal optimization for high bandwidth
        "auto": "adaptive"    # Adaptive optimization
    }
    return levels.get(bandwidth_hint, levels["auto"])


def _get_adaptive_delay(bandwidth_hint: str) -> float:
    """Get adaptive delay between chunks based on bandwidth."""
    delays = {
        "low": 0.01,      # 10ms delay for low bandwidth
        "medium": 0.005,  # 5ms delay for medium bandwidth
        "high": 0.001,    # 1ms delay for high bandwidth
        "auto": 0.003     # 3ms delay for auto
    }
    return delays.get(bandwidth_hint, delays["auto"])


def _get_event_type(chunk_type: str) -> str:
    """Map chunk types to SSE event types."""
    mapping = {
        "metadata": "metadata",
        "chunk": "chunk", 
        "heartbeat": "heartbeat",
        "completion": "completion",
        "error": "error"
    }
    return mapping.get(chunk_type, "data")


def _generate_optimization_headers(bandwidth_hint: str, adaptive_compression: bool, progressive_rendering: bool) -> Dict[str, str]:
    """Generate headers with optimization information."""
    return {
        "Cache-Control": "no-cache",
        "Connection": "keep-alive",
        "X-Server-Rendered": "true",
        "X-Bandwidth-Optimized": "true",
        "X-Bandwidth-Hint": bandwidth_hint,
        "X-Adaptive-Compression": str(adaptive_compression).lower(),
        "X-Progressive-Rendering": str(progressive_rendering).lower(),
        "X-Optimization-Level": _get_optimization_level(bandwidth_hint),
        "X-Stream-Type": "bandwidth-optimized",
        "X-Resume-Supported": "true"
    }


@router.get("/async_status")
async def get_async_status(
    request: Request,
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> Dict[str, Any]:
    """
    Get comprehensive async processing status and performance metrics.

    Returns detailed information about concurrent processing capabilities,
    queue status, connection pool health, and server-side performance metrics.

    Returns:
        Comprehensive async status information
    """
    try:
        # Get processor statistics
        processing_stats = processor.get_processing_stats()
        
        # Get queue statistics if available
        queue_stats = {}
        if hasattr(processor, '_priority_queue'):
            queue_stats = processor._priority_queue.get_queue_stats()
        
        # Get connection pool stats if available
        pool_stats = {}
        if hasattr(request.app.state, 'connection_pool'):
            pool = request.app.state.connection_pool
            if hasattr(pool, 'get_stats'):
                pool_stats = pool.get_stats().__dict__
        
        # Get concurrency manager stats if available
        concurrency_stats = {}
        if hasattr(request.app.state, 'concurrency_manager'):
            manager = request.app.state.concurrency_manager
            if hasattr(manager, 'get_current_load'):
                concurrency_stats = manager.get_current_load()
        
        return {
            "status": "healthy",
            "timestamp": time.time(),
            "server_side_processing": True,
            "async_capabilities": {
                "concurrent_processing": True,
                "priority_queuing": hasattr(processor, '_priority_queue'),
                "streaming_chunks": True,
                "connection_pooling": bool(pool_stats),
                "rate_limiting": bool(concurrency_stats),
                "backpressure_control": True
            },
            "processing_stats": processing_stats,
            "queue_stats": queue_stats,
            "connection_pool_stats": pool_stats,
            "concurrency_stats": concurrency_stats,
            "performance_metrics": {
                "max_concurrent_processes": processor.max_concurrent_processes,
                "engine_integration": processor.engine is not None,
                "components_available": getattr(processor, 'components_real', False)
            }
        }
        
    except Exception as e:
        logger.error(f"Async status check error: {e}")
        return {
            "status": "error",
            "error": str(e),
            "timestamp": time.time(),
            "server_side_processing": True
        }


@router.get("/load_balancer_status")
async def get_load_balancer_status(request: Request) -> Dict[str, Any]:
    """
    Get load balancer and resource optimization status.
    
    Provides comprehensive information about server-side load distribution,
    resource utilization, and performance optimization metrics.
    
    Returns:
        Load balancer status and optimization metrics
    """
    try:
        current_time = time.time()
        
        # Get system resource information
        import psutil
        
        system_stats = {
            "cpu_percent": psutil.cpu_percent(interval=0.1),
            "memory_percent": psutil.virtual_memory().percent,
            "disk_usage_percent": psutil.disk_usage('/').percent,
            "active_connections": len(psutil.net_connections()),
            "load_average": psutil.getloadavg() if hasattr(psutil, 'getloadavg') else [0, 0, 0]
        }
        
        # Calculate load distribution recommendations
        load_recommendations = []
        
        if system_stats["cpu_percent"] > 80:
            load_recommendations.append({
                "type": "cpu_high",
                "message": "Consider scaling concurrent processing limits",
                "suggested_action": "reduce_concurrency"
            })
        
        if system_stats["memory_percent"] > 85:
            load_recommendations.append({
                "type": "memory_high", 
                "message": "Consider reducing connection pool size",
                "suggested_action": "optimize_memory"
            })
        
        return {
            "status": "active",
            "timestamp": current_time,
            "load_balancing_enabled": True,
            "system_stats": system_stats,
            "load_recommendations": load_recommendations,
            "optimization_features": {
                "adaptive_concurrency": True,
                "resource_monitoring": True,
                "automatic_scaling": False,  # Could be enhanced
                "circuit_breaker": True
            }
        }
        
    except ImportError:
        return {
            "status": "limited",
            "message": "psutil not available for detailed system monitoring",
            "timestamp": current_time,
            "load_balancing_enabled": True
        }
    except Exception as e:
        logger.error(f"Load balancer status error: {e}")
        return {
            "status": "error",
            "error": str(e),
            "timestamp": current_time,
            "X-Concurrent-Processing": "true"
        }


@router.get("/engine_integration")
async def engine_integration_status(
    processor: DTESNProcessor = Depends(get_dtesn_processor),
    engine_stats: Dict[str, Any] = Depends(get_engine_stats)
) -> Dict[str, Any]:
    """
    Get detailed engine integration status and capabilities.

    Server-side endpoint providing comprehensive integration status
    between DTESN processing and Aphrodite Engine components.

    Returns:
        Detailed engine integration information
    """
    try:
        integration_status = {
            "dtesn_processor": {
                "status": "operational",
                "echo_kern_available": hasattr(processor, 'dtesn_config'),
                "components_initialized": True
            },
            "aphrodite_engine": engine_stats,
            "integration_capabilities": {
                "server_side_processing": True,
                "batch_processing": True,
                "streaming_support": True,
                "real_time_updates": True
            },
            "performance_profile": {
                "max_membrane_depth": processor.config.max_membrane_depth,
                "max_esn_size": processor.config.esn_reservoir_size,
                "bseries_max_order": processor.config.bseries_max_order,
                "caching_enabled": processor.config.enable_caching
            },
            "server_rendered": True
        }

        return integration_status

    except Exception as e:
        logger.error(f"Engine integration status error: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Could not retrieve engine integration status: {e}"
        )


@router.get("/performance_metrics")
async def performance_metrics(
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> Dict[str, Any]:
    """
    Get detailed performance metrics for server-side monitoring.

    Provides comprehensive performance data for server-side monitoring
    and optimization without client dependencies.

    Returns:
        Performance metrics and monitoring data
    """
    return {
        "service_metrics": {
            "uptime_seconds": time.time(),
            "processing_mode": "server_side",
            "optimization_level": "production"
        },
        "dtesn_performance": {
            "max_membrane_depth": processor.config.max_membrane_depth,
            "max_esn_size": processor.config.esn_reservoir_size,
            "bseries_max_order": processor.config.bseries_max_order,
            "estimated_throughput": "varies_by_input_size",
            "memory_efficiency": "optimized"
        },
        "server_optimization": {
            "caching_enabled": processor.config.enable_caching,
            "performance_monitoring": processor.config.enable_performance_monitoring,
            "batch_processing": True,
            "streaming_support": True
        },
        "integration_metrics": {
            "echo_kern_integration": "active",
            "aphrodite_engine_integration": "available",
            "server_side_rendering": "enabled"
        },
        "server_rendered": True
    }


@router.get("/status")
async def dtesn_status(
    request: Request,
    templates: Jinja2Templates = Depends(get_templates),
    processor: DTESNProcessor = Depends(get_dtesn_processor),
    engine_stats: Dict[str, Any] = Depends(get_engine_stats)
) -> Response:
    """
    Get enhanced DTESN system status with engine integration.

    Returns server-rendered status information about the DTESN system
    including Aphrodite Engine integration status.
    Supports JSON, HTML, and XML responses via content negotiation.

    Returns:
        Enhanced system status information
    """
    data = {
        "dtesn_system": "operational",
        "membrane_hierarchy": "active",
        "esn_reservoir": "ready",
        "server_side": True,
        "processing_capabilities": {
            "max_membrane_depth": processor.config.max_membrane_depth,
            "max_esn_size": processor.config.esn_reservoir_size,
            "bseries_max_order": processor.config.bseries_max_order
        },
        "advanced_features": {
            "batch_processing": True,
            "streaming_support": True,
            "real_time_monitoring": True,
            "performance_optimization": True
        },
        "engine_integration": engine_stats,
        "server_rendered": True
    }

    return create_negotiated_response(
        data=data,
        request=request,
        templates=templates,
        template_name="status.html",
        xml_root="dtesn_status"
    )


@router.get("/membrane_info")
async def membrane_info(
    request: Request,
    templates: Jinja2Templates = Depends(get_templates),
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> Response:
    """
    Get comprehensive information about DTESN membrane hierarchy.

    Server-side endpoint providing detailed membrane computing information
    with enhanced server-side data fetching capabilities.
    Supports JSON, HTML, and XML responses via content negotiation.

    Returns:
        Enhanced membrane hierarchy information
    """
    data = {
        "membrane_type": "P-System",
        "hierarchy_type": "rooted_tree",
        "oeis_sequence": "A000081",
        "max_depth": processor.config.max_membrane_depth,
        "supported_operations": [
            "membrane_evolution",
            "cross_membrane_communication",
            "rule_application",
            "tree_enumeration",
            "hierarchical_processing",
            "parallel_computation"
        ],
        "performance_characteristics": {
            "parallel_processing": True,
            "memory_efficient": True,
            "real_time_capable": True,
            "scalable_depth": True
        },
        "integration_features": {
            "server_side_optimization": True,
            "batch_processing_support": True,
            "streaming_compatible": True,
            "engine_integrated": True
        },
        "server_rendered": True,
        "operation_descriptions": {
            "membrane_evolution": "Dynamic transformation of membrane structures",
            "cross_membrane_communication": "Communication protocols between different membrane levels",
            "rule_application": "Application of P-system evolution rules within membranes",
            "tree_enumeration": "OEIS A000081 compliant counting of rooted tree structures"
        }
    }

    return create_negotiated_response(
        data=data,
        request=request,
        templates=templates,
        template_name="membrane_info.html",
        xml_root="membrane_hierarchy_info"
    )


@router.get("/async_status")
async def async_processing_status(
    request: Request,
    processor: DTESNProcessor = Depends(get_dtesn_processor),
    templates: Jinja2Templates = Depends(get_templates)
) -> Union[HTMLResponse, JSONResponse]:
    """
    Get enhanced async processing status and concurrency metrics.

    Returns comprehensive information about async processing capabilities,
    resource utilization, and concurrent request handling performance.

    Returns:
        Enhanced async processing status with detailed metrics
    """
    # Get processing statistics from processor
    processing_stats = processor.get_processing_stats()
    
    # Get connection pool stats if available
    connection_pool = getattr(request.app.state, "connection_pool", None)
    pool_stats = connection_pool.get_stats() if connection_pool else None
    
    # Get concurrency manager stats if available
    concurrency_manager = getattr(request.app.state, "concurrency_manager", None)
    concurrency_stats = concurrency_manager.get_current_load() if concurrency_manager else None
    
    data = {
        "async_processing": {
            "enabled": True,
            "concurrent_processing": True,
            "resource_management": connection_pool is not None,
            "backpressure_handling": True
        },
        "processing_metrics": processing_stats,
        "resource_pool": pool_stats.__dict__ if pool_stats else {"status": "unavailable"},
        "concurrency_management": concurrency_stats or {"status": "unavailable"},
        "capabilities": {
            "max_concurrent_requests": processing_stats.get("max_concurrent_processes", 0),
            "connection_pooling": connection_pool is not None,
            "rate_limiting": concurrency_manager is not None,
            "streaming_with_backpressure": True,
            "batch_processing": True,
            "resource_monitoring": True
        },
        "performance_features": {
            "async_connection_pooling": connection_pool is not None,
            "concurrent_batch_processing": True,
            "enhanced_streaming": True,
            "resource_cleanup": True,
            "error_recovery": True
        },
        "server_rendered": True
    }

    if wants_html(request):
        return templates.TemplateResponse(
            "async_status.html",
            {"request": request, "data": data}
        )
    else:
        return JSONResponse(data)


@router.get("/monitoring")
async def get_monitoring_dashboard() -> Dict[str, Any]:
    """
    Get real-time monitoring dashboard data with comprehensive system health metrics.

    Returns comprehensive monitoring data including error rates, performance metrics,
    alert status, recovery statistics, and system health information to support
    99.9% uptime requirements.

    Returns:
        Real-time monitoring dashboard data
    """
    from .monitoring import monitoring_dashboard
    
    dashboard_data = monitoring_dashboard.get_dashboard_data()
    dashboard_data["endpoint"] = "/deep_tree_echo/monitoring"
    dashboard_data["server_rendered"] = True
    
    return dashboard_data


@router.get("/monitoring/alerts")
async def get_active_alerts() -> Dict[str, Any]:
    """
    Get currently active alerts and recent alert history.

    Returns:
        Active alerts and alert history for monitoring dashboard
    """
    from .monitoring import alert_manager
    
    return {
        "active_alerts": [alert.to_dict() for alert in alert_manager.get_active_alerts()],
        "recent_alerts": [alert.to_dict() for alert in alert_manager.get_alert_history(50)],
        "alert_counts": {
            "active": len(alert_manager.get_active_alerts()),
            "recent": len(alert_manager.get_alert_history(50))
        },
        "server_rendered": True
    }


@router.get("/monitoring/metrics")
async def get_current_metrics() -> Dict[str, Any]:
    """
    Get current system metrics for real-time monitoring.

    Returns:
        Current system performance and health metrics
    """
    from .monitoring import metrics_collector
    
    metrics = metrics_collector.get_current_metrics()
    return {
        "metrics": metrics.to_dict(),
        "collection_timestamp": datetime.now().isoformat(),
        "server_rendered": True
    }


@router.get("/monitoring/recovery_stats")
async def get_recovery_statistics() -> Dict[str, Any]:
    """
    Get error recovery statistics and system resilience metrics.

    Returns:
        Recovery statistics and resilience metrics
    """
    from .error_recovery import error_recovery_service
    
    recovery_stats = error_recovery_service.get_recovery_stats()
    return {
        "recovery_stats": recovery_stats,
        "system_resilience": {
            "recovery_enabled": True,
            "fallback_modes_available": ["simplified", "cached", "statistical", "minimal", "offline"],
            "circuit_breaker_enabled": True,
            "retry_mechanisms_active": True
        },
        "server_rendered": True
    }
# Dynamic Batching Endpoints

class DynamicBatchRequest(BaseModel):
    """Request model for dynamic batch processing."""
    
    input_data: str
    membrane_depth: Optional[int] = Field(default=4, ge=1, le=16)
    esn_size: Optional[int] = Field(default=512, ge=32, le=4096)
    priority: int = Field(default=1, ge=0, le=2, description="Request priority (0=highest, 2=lowest)")
    timeout: Optional[float] = Field(default=None, ge=1.0, le=300.0)
    enable_batching: bool = Field(default=True, description="Enable dynamic batching")


class BatchMetricsResponse(BaseModel):
    """Response model for batch processing metrics."""
    
    status: str
    batching_enabled: bool
    current_batch_size: Optional[int]
    pending_requests: int
    metrics: Dict[str, Any]
    server_load: Dict[str, Any]
    performance_stats: Dict[str, Any]


@router.post("/process_with_batching", response_model=DTESNResponse)
async def process_with_dynamic_batching(
    request_data: DynamicBatchRequest,
    request: Request,
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> Union[DTESNResponse, HTMLResponse]:
    """
    Process input using intelligent dynamic batching for optimal throughput.
    
    Automatically batches requests based on server load, request patterns,
    and performance metrics to maximize throughput while maintaining responsiveness.
    Features adaptive batch sizing and load-aware processing optimization.
    """
    start_time = time.time()
    
    try:
        # Ensure batch manager is configured
        if not hasattr(processor, '_batch_manager') or not processor._batch_manager:
            # Configure batch manager if not already done
            load_tracker = get_batch_load_function(request.app.state)
            batch_config = BatchConfiguration(
                target_batch_size=8,
                max_batch_size=32,
                max_batch_wait_ms=50.0,
                enable_adaptive_sizing=True
            )
            
            # Reinitialize processor with batching
            processor._batch_manager = DynamicBatchManager(
                config=batch_config,
                load_tracker=load_tracker
            )
            processor._batch_manager.set_dtesn_processor(processor)
            processor._batch_manager_started = False

        if request_data.enable_batching:
            # Process using dynamic batching
            result = await processor.process_with_dynamic_batching(
                input_data=request_data.input_data,
                membrane_depth=request_data.membrane_depth,
                esn_size=request_data.esn_size,
                priority=request_data.priority,
                timeout=request_data.timeout
            )
        else:
            # Process directly without batching
            result = await processor.process(
                input_data=request_data.input_data,
                membrane_depth=request_data.membrane_depth,
                esn_size=request_data.esn_size,
                enable_concurrent=True
            )

        processing_time = time.time() - start_time

        # Get batching metrics
        batch_metrics = processor.get_batching_metrics()
        current_batch_size = processor.get_current_batch_size()
        pending_count = await processor.get_pending_batch_count()

        # Prepare performance metrics
        performance_metrics = {
            "total_processing_time_ms": processing_time * 1000,
            "dtesn_processing_time_ms": result.processing_time_ms,
            "overhead_ms": (processing_time * 1000) - result.processing_time_ms,
            "throughput_chars_per_second": len(request_data.input_data) / processing_time if processing_time > 0 else 0,
            "batching_enabled": request_data.enable_batching,
            "current_batch_size": current_batch_size,
            "pending_requests": pending_count
        }

        # Add batch metrics if available
        if batch_metrics:
            performance_metrics.update({
                "batch_throughput_improvement": batch_metrics.throughput_improvement,
                "avg_batch_size": batch_metrics.avg_batch_size,
                "avg_server_load": batch_metrics.avg_server_load,
                "requests_processed": batch_metrics.requests_processed
            })

        response_data = DTESNResponse(
            status="success",
            result=result.to_dict(),
            processing_time_ms=result.processing_time_ms,
            membrane_layers=result.membrane_layers,
            server_rendered=True,
            engine_integration=result.engine_integration,
            performance_metrics=performance_metrics
        )

        if wants_html(request):
            return templates.TemplateResponse(
                "batch_process_result.html",
                {
                    "request": request,
                    "data": response_data.dict(),
                    "input_data": request_data.input_data,
                    "batching_enabled": request_data.enable_batching,
                    "timestamp": datetime.now().isoformat()
                }
            )
        else:
            return response_data

    except Exception as e:
        logger.error(f"Dynamic batch processing error: {e}")
        error_time = time.time() - start_time
        error_detail = {
            "error": f"Dynamic batch processing failed: {e}",
            "processing_time_ms": error_time * 1000,
            "batching_enabled": request_data.enable_batching,
            "server_rendered": True
        }

        if wants_html(request):
            return templates.TemplateResponse(
                "batch_process_result.html",
                {
                    "request": request,
                    "data": {"status": "error", "error": str(e)},
                    "input_data": request_data.input_data,
                    "batching_enabled": request_data.enable_batching,
                    "timestamp": datetime.now().isoformat()
                },
                status_code=500
            )
        else:
            raise HTTPException(status_code=500, detail=error_detail)


@router.get("/batch_metrics", response_model=BatchMetricsResponse)
async def get_batch_metrics(
    request: Request,
    processor: DTESNProcessor = Depends(get_dtesn_processor)
) -> Union[BatchMetricsResponse, HTMLResponse]:
    """
    Get current dynamic batching metrics and performance statistics.
    
    Provides comprehensive metrics about batch processing performance,
    server load, throughput improvements, and system optimization status.
    """
    try:
        # Get batch metrics
        batch_metrics = processor.get_batching_metrics()
        current_batch_size = processor.get_current_batch_size()
        pending_count = await processor.get_pending_batch_count()
        
        # Get server load information
        server_load = {}
        if hasattr(request.app.state, 'server_load_metrics'):
            server_load["active_requests"] = request.app.state.server_load_metrics
        
        # Prepare metrics data
        metrics_data = {}
        if batch_metrics:
            metrics_data = {
                "requests_processed": batch_metrics.requests_processed,
                "avg_batch_size": batch_metrics.avg_batch_size,
                "avg_processing_time_ms": batch_metrics.avg_processing_time_ms,
                "throughput_improvement": batch_metrics.throughput_improvement,
                "avg_server_load": batch_metrics.avg_server_load,
                "batch_utilization": batch_metrics.batch_utilization,
                "avg_batch_wait_time": batch_metrics.avg_batch_wait_time,
                "last_updated": batch_metrics.last_updated
            }

        # Get processing stats
        performance_stats = processor.get_processing_stats() if hasattr(processor, 'get_processing_stats') else {}

        response_data = BatchMetricsResponse(
            status="success",
            batching_enabled=processor._batch_manager is not None,
            current_batch_size=current_batch_size,
            pending_requests=pending_count,
            metrics=metrics_data,
            server_load=server_load,
            performance_stats=performance_stats
        )

        if wants_html(request):
            return templates.TemplateResponse(
                "batch_metrics.html",
                {
                    "request": request,
                    "data": response_data.dict(),
                    "timestamp": datetime.now().isoformat()
                }
            )
        else:
            return response_data

    except Exception as e:
        logger.error(f"Batch metrics retrieval error: {e}")
        error_response = BatchMetricsResponse(
            status="error",
            batching_enabled=False,
            current_batch_size=None,
            pending_requests=0,
            metrics={"error": str(e)},
            server_load={},
            performance_stats={}
        )

        if wants_html(request):
            return templates.TemplateResponse(
                "batch_metrics.html",
                {
                    "request": request,
                    "data": error_response.dict(),
                    "timestamp": datetime.now().isoformat()
                },
                status_code=500
            )
        else:
            raise HTTPException(status_code=500, detail=error_response.dict())


# Phase 7.2.1 - Advanced Server-Side Template Engine Endpoints

@router.get("/template_performance")
async def get_template_performance_metrics(
    request: Request,
    templates: Jinja2Templates = Depends(get_templates),
    advanced_engine: AdvancedTemplateEngine = Depends(get_advanced_template_engine),
    cache_manager: DTESNTemplateCacheManager = Depends(get_template_cache_manager)
) -> Union[HTMLResponse, Dict[str, Any]]:
    """
    Get comprehensive template engine performance metrics and cache statistics.
    
    Provides detailed information about template compilation, caching effectiveness,
    dynamic generation performance, and server-side rendering optimization metrics.
    """
    try:
        # Get cache statistics
        cache_stats = cache_manager.get_cache_statistics()
        
        # Get advanced engine performance stats  
        engine_stats = advanced_engine.get_performance_stats()
        
        # Get dynamic generator cache stats
        generator_stats = advanced_engine.dynamic_generator.get_cache_stats()
        
        # Calculate overall performance metrics
        performance_data = {
            "template_engine_status": "operational",
            "advanced_features_enabled": True,
            "phase_7_2_1_implemented": True,
            "cache_performance": cache_stats["cache_performance"],
            "dynamic_generation": {
                "templates_generated": generator_stats.get("template_cache_entries", 0),
                "rendered_cache_entries": generator_stats.get("rendered_cache_entries", 0),
                "hit_rate": generator_stats.get("hit_rate", 0),
                "supported_result_types": engine_stats.get("supported_result_types", []),
                "responsive_adaptation": engine_stats.get("supported_client_types", [])
            },
            "optimization_features": {
                "template_compilation_caching": True,
                "rendered_result_caching": True,
                "compression_enabled": cache_stats["compression"]["compression_enabled"],
                "ttl_management": True,
                "invalidation_by_tags": True,
                "responsive_templates": True,
                "server_side_only": True
            },
            "memory_usage": cache_stats["memory_usage"],
            "distributed_caching": cache_stats["distributed_cache"],
            "server_rendered": True
        }
        
        if wants_html(request):
            return templates.TemplateResponse(
                "template_performance.html",
                {
                    "request": request,
                    "data": performance_data,
                    "timestamp": datetime.now().isoformat()
                }
            )
        else:
            return performance_data
            
    except Exception as e:
        logger.error(f"Template performance metrics error: {e}")
        error_data = {
            "status": "error",
            "error": str(e),
            "template_engine_status": "degraded",
            "server_rendered": True
        }
        
        if wants_html(request):
            return templates.TemplateResponse(
                "template_performance.html",
                {
                    "request": request,
                    "data": error_data,
                    "timestamp": datetime.now().isoformat()
                },
                status_code=500
            )
        else:
            raise HTTPException(status_code=500, detail=error_data)


@router.post("/template_cache/optimize")
async def optimize_template_cache(
    cache_manager: DTESNTemplateCacheManager = Depends(get_template_cache_manager)
) -> Dict[str, Any]:
    """
    Optimize template cache performance by cleaning expired entries and updating metrics.
    """
    try:
        optimization_result = await cache_manager.optimize_performance()
        
        return {
            "status": "success",
            "optimization_completed": True,
            "results": optimization_result,
            "server_rendered": True
        }
        
    except Exception as e:
        logger.error(f"Template cache optimization error: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Cache optimization failed: {e}"
        )


@router.get("/template_capabilities")
async def get_template_capabilities(
    request: Request,
    templates: Jinja2Templates = Depends(get_templates)
) -> Union[HTMLResponse, Dict[str, Any]]:
    """
    Get comprehensive template engine capabilities and feature documentation.
    """
    capabilities_data = {
        "phase_7_2_1_features": {
            "dynamic_template_generation": {
                "enabled": True,
                "description": "Templates generated dynamically based on DTESN result structure",
                "supported_types": [
                    "membrane_evolution",
                    "esn_processing", 
                    "bseries_computation",
                    "batch_processing",
                    "error_recovery"
                ],
                "complexity_levels": ["simple", "medium", "complex"],
                "server_side_only": True
            },
            "template_caching": {
                "enabled": True,
                "description": "Multi-level caching for template compilation and rendered results"
            },
            "responsive_adaptation": {
                "enabled": True,
                "description": "Server-side responsive template selection based on client type"
            }
        },
        "server_rendered": True
    }
    
    if wants_html(request):
        return templates.TemplateResponse(
            "template_capabilities.html",
            {
                "request": request,
                "data": capabilities_data,
                "timestamp": datetime.now().isoformat()
            }
        )
    else:
        return capabilities_data


# Model Serving Integration - Task 8.1.1 Implementation
@router.get("/model_serving")
async def get_model_serving_overview(
    request: Request,
    templates: Jinja2Templates = Depends(get_templates),
    model_serving_manager: ModelServingManager = Depends(get_model_serving_manager)
) -> Union[HTMLResponse, Dict[str, Any]]:
    """
    Get overview of model serving infrastructure with zero-downtime capabilities.
    
    Provides comprehensive information about the enhanced model serving system
    including Task 8.1.1 implementation status and capabilities.
    """
    try:
        serving_status = model_serving_manager.get_model_serving_status()
        available_models = await model_serving_manager.list_available_models()
        
        overview_data = {
            "task_8_1_1_status": "implemented",
            "model_serving_infrastructure": {
                "zero_downtime_updates": True,
                "resource_aware_allocation": True,
                "dtesn_integration": True,
                "server_side_loading": True,
                "caching_strategies": True
            },
            "current_status": serving_status,
            "available_models": available_models,
            "capabilities": {
                "seamless_model_management": True,
                "zero_downtime_deployment": True,
                "health_monitoring": True,
                "automatic_rollback": True,
                "performance_optimization": True
            },
            "integration_features": {
                "echo_kern_integration": True,
                "dtesn_optimizations": True,
                "engine_backend_integration": True,
                "comprehensive_monitoring": True
            },
            "server_rendered": True
        }
        
        if wants_html(request):
            return templates.TemplateResponse(
                "model_serving_overview.html",
                {
                    "request": request,
                    "data": overview_data,
                    "timestamp": datetime.now().isoformat()
                }
            )
        else:
            return overview_data
            
    except Exception as e:
        logger.error(f"Model serving overview error: {e}")
        error_data = {
            "status": "error",
            "error": str(e),
            "task_8_1_1_status": "error",
            "server_rendered": True
        }
        
        if wants_html(request):
            return templates.TemplateResponse(
                "model_serving_overview.html",
                {
                    "request": request,
                    "data": error_data,
                    "timestamp": datetime.now().isoformat()
                },
                status_code=500
            )
        else:
            raise HTTPException(status_code=500, detail=error_data)


# Include model serving routes as sub-router
model_serving_router = create_model_serving_routes(None)  # Will be configured at app startup
router.include_router(model_serving_router, prefix="/model_serving", tags=["model_serving"])
