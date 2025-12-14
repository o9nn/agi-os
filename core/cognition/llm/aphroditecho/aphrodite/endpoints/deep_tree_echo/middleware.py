"""
Middleware for Deep Tree Echo FastAPI endpoints.

Provides request/response middleware for DTESN processing and performance monitoring
with enhanced async resource management, concurrency control, and comprehensive error handling.
"""

import time
import asyncio
import logging
from typing import Callable, Optional

from fastapi import Request, Response, HTTPException
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware

from aphrodite.endpoints.deep_tree_echo.async_manager import (
    AsyncConnectionPool,
    ConcurrencyManager
)
from .errors import (
    DTESNError, DTESNSystemError, DTESNResourceError, DTESNNetworkError,
    ErrorContext, create_error_response, error_aggregator,
    dtesn_circuit_breaker
)
from .error_recovery import error_recovery_service

logger = logging.getLogger(__name__)


class DTESNMiddleware(BaseHTTPMiddleware):
    """Enhanced middleware for DTESN-specific request processing with async resource management."""

    def __init__(self, app, connection_pool: Optional[AsyncConnectionPool] = None):
        """Initialize DTESN middleware with connection pool."""
        super().__init__(app)
        self.connection_pool = connection_pool

    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """
        Process DTESN-specific request handling with enhanced async resource management and error recovery.

        Args:
            request: FastAPI request object
            call_next: Next middleware or route handler

        Returns:
            Response with DTESN processing metadata and comprehensive error handling
        """
        # Create error context for comprehensive tracking
        request_id = request.headers.get("X-Request-ID", f"req_{int(time.time() * 1000000)}")
        error_context = ErrorContext(
            request_id=request_id,
            endpoint=str(request.url.path),
            user_input=None,  # Will be populated by route handlers
            processing_stage="middleware",
            trace_id=request.headers.get("X-Trace-ID")
        )

        # Add enhanced DTESN request context with resource management
        request.state.dtesn_context = {
            "request_id": request_id,
            "error_context": error_context,
            "membrane_depth": 0,
            "processing_mode": "server_side",
            "async_processing": True,
            "resource_managed": self.connection_pool is not None,
            "start_time": time.time(),
            "circuit_breaker_active": False
        }

        try:
            # Check circuit breaker status
            system_health = error_aggregator.get_system_health_status()
            if system_health.get("should_circuit_break", False):
                request.state.dtesn_context["circuit_breaker_active"] = True
                raise DTESNSystemError(
                    "Circuit breaker activated due to high error rate",
                    context=error_context
                )

            # Use connection pool if available for resource-managed processing
            if self.connection_pool and self._is_dtesn_request(request):
                async with self.connection_pool.get_connection() as connection_id:
                    request.state.dtesn_context["connection_id"] = connection_id
                    error_context.resource_state = {"connection_id": connection_id}
                    
                    # Execute with circuit breaker protection for DTESN requests
                    response = await dtesn_circuit_breaker.call(call_next, request)
            else:
                response = await call_next(request)

            # Add enhanced DTESN response headers
            response.headers["X-DTESN-Processed"] = "true"
            response.headers["X-Processing-Mode"] = "server-side"
            response.headers["X-Async-Managed"] = "true" if self.connection_pool else "false"
            response.headers["X-Request-ID"] = request_id
            response.headers["X-Error-Recovery-Available"] = "true"
            
            # Add system health indicators
            if system_health.get("degraded_mode_recommended", False):
                response.headers["X-System-Mode"] = "degraded"
            else:
                response.headers["X-System-Mode"] = "normal"

            return response

        except DTESNError as dtesn_error:
            # Handle DTESN-specific errors with structured response
            logger.error(f"DTESN error in middleware: {dtesn_error.error_code}")
            error_response = create_error_response(dtesn_error, error_context, include_debug_info=True)
            
            return JSONResponse(
                status_code=self._get_http_status_code(dtesn_error),
                content=error_response.dict(),
                headers={
                    "X-DTESN-Error": "true",
                    "X-Request-ID": request_id,
                    "X-Error-Code": dtesn_error.error_code,
                    "X-Error-Category": dtesn_error.category.value,
                    "X-Recovery-Available": "true" if dtesn_error.recovery_strategy.value != "abort" else "false"
                }
            )

        except Exception as e:
            # Handle unexpected errors
            logger.error(f"Unexpected middleware error for request {request_id}: {e}", exc_info=True)
            
            # Wrap in DTESN error for consistent handling
            system_error = DTESNSystemError(
                f"Middleware processing failed: {str(e)}",
                context=error_context,
                original_error=e
            )
            
            error_response = create_error_response(system_error, error_context)
            
            return JSONResponse(
                status_code=500,
                content=error_response.dict(),
                headers={
                    "X-DTESN-Error": "true",
                    "X-Request-ID": request_id,
                    "X-Error-Code": system_error.error_code,
                    "X-Unexpected-Error": "true"
                }
            )

    def _get_http_status_code(self, error: DTESNError) -> int:
        """Map DTESN error to appropriate HTTP status code."""
        severity_to_status = {
            "validation": 400,
            "processing": 422,
            "resource": 503,
            "network": 502,
            "configuration": 500,
            "system": 500,
            "dtesn": 500,
            "engine": 503
        }
        return severity_to_status.get(error.category.value, 500)

    def _is_dtesn_request(self, request: Request) -> bool:
        """Check if request is for DTESN processing endpoints."""
        return (
            request.url.path.startswith("/deep_tree_echo/") and
            request.url.path not in ["/deep_tree_echo/", "/deep_tree_echo/status"]
        )


class PerformanceMonitoringMiddleware(BaseHTTPMiddleware):
    """Enhanced middleware for performance monitoring and request timing with concurrency control."""

    def __init__(self, app, concurrency_manager: Optional[ConcurrencyManager] = None):
        """Initialize performance monitoring middleware with concurrency manager."""
        super().__init__(app)
        self.concurrency_manager = concurrency_manager

    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """
        Monitor request performance and timing with enhanced concurrency control.

        Args:
            request: FastAPI request object
            call_next: Next middleware or route handler

        Returns:
            Response with enhanced performance timing headers and concurrency metrics
        """
        start_time = time.time()

        try:
            # Apply concurrency control if available and needed
            if self.concurrency_manager and self._needs_throttling(request):
                async with self.concurrency_manager.throttle_request():
                    response = await call_next(request)
            else:
                response = await call_next(request)

            # Calculate processing time
            process_time = time.time() - start_time

            # Add enhanced performance headers
            response.headers["X-Process-Time"] = str(process_time)
            response.headers["X-Server-Timestamp"] = str(int(time.time()))
            response.headers["X-Async-Processing"] = "true"
            
            # Add concurrency metrics if available
            if self.concurrency_manager:
                load_stats = self.concurrency_manager.get_current_load()
                response.headers["X-Concurrency-Load"] = f"{load_stats['concurrency_utilization']:.2f}"
                response.headers["X-Rate-Limit-Load"] = f"{load_stats['rate_limit_utilization']:.2f}"

            # Log performance metrics with enhanced details
            logger.info(
                f"DTESN request processed: {request.url.path} in {process_time:.3f}s "
                f"[concurrent: {'yes' if self.concurrency_manager else 'no'}]"
            )

            return response

        except HTTPException:
            # Re-raise HTTP exceptions without modification
            raise
        except Exception as e:
            process_time = time.time() - start_time
            logger.error(f"Performance monitoring error: {e} (after {process_time:.3f}s)")
            
            # Return error response with timing information
            return Response(
                content=f"Performance monitoring error: {str(e)}",
                status_code=500,
                headers={
                    "X-Process-Time": str(process_time),
                    "X-Performance-Error": "true"
                }
            )

    def _needs_throttling(self, request: Request) -> bool:
        """Check if request needs concurrency throttling."""
        return (
            request.method in ["POST", "PUT"] and
            request.url.path.startswith("/deep_tree_echo/")
        )


class AsyncResourceMiddleware(BaseHTTPMiddleware):
    """
    Middleware for managing async resources and connection pooling.
    
    Provides centralized async resource management across all DTESN endpoints
    with proper cleanup and error handling.
    """
    
    def __init__(
        self,
        app,
        connection_pool: Optional[AsyncConnectionPool] = None,
        enable_resource_monitoring: bool = True
    ):
        """Initialize async resource middleware."""
        super().__init__(app)
        self.connection_pool = connection_pool
        self.enable_resource_monitoring = enable_resource_monitoring
        
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """
        Manage async resources for request processing.
        
        Args:
            request: FastAPI request object
            call_next: Next middleware or route handler
            
        Returns:
            Response with resource management headers and cleanup
        """
        resource_context = {
            "pool_available": self.connection_pool is not None,
            "monitoring_enabled": self.enable_resource_monitoring,
            "request_start": time.time()
        }
        
        # Store resource context in request state
        request.state.resource_context = resource_context
        
        try:
            # Process request with resource management
            response = await call_next(request)
            
            # Add resource management headers
            if self.enable_resource_monitoring and self.connection_pool:
                stats = self.connection_pool.get_stats()
                response.headers["X-Pool-Active"] = str(stats.active_connections)
                response.headers["X-Pool-Utilization"] = f"{stats.pool_utilization:.2f}"
                response.headers["X-Pool-Avg-Time"] = f"{stats.avg_response_time:.3f}"
            
            response.headers["X-Resource-Managed"] = "true"
            
            return response
            
        except asyncio.CancelledError:
            logger.warning(f"Request cancelled: {request.url.path}")
            raise
        except Exception as e:
            logger.error(f"Async resource management error: {e}")
            return Response(
                content=f"Resource management error: {str(e)}",
                status_code=503,
                headers={"X-Resource-Error": "true"}
            )


class AsyncPerformanceMiddleware(BaseHTTPMiddleware):
    """
    Enhanced middleware for async performance monitoring and optimization.
    
    Provides comprehensive monitoring of async request processing, including
    concurrency metrics, response time analysis, and adaptive performance tuning.
    """
    
    def __init__(
        self,
        app,
        enable_detailed_metrics: bool = True,
        performance_threshold_ms: float = 1000.0,
        slow_request_threshold_ms: float = 5000.0
    ):
        """Initialize async performance middleware."""
        super().__init__(app)
        self.enable_detailed_metrics = enable_detailed_metrics
        self.performance_threshold_ms = performance_threshold_ms
        self.slow_request_threshold_ms = slow_request_threshold_ms
        
        # Performance tracking
        self._request_times = []
        self._slow_requests = []
        self._concurrent_requests = 0
        self._lock = asyncio.Lock()
        
        logger.info("AsyncPerformanceMiddleware initialized with enhanced monitoring")
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """
        Monitor and optimize async request performance.
        
        Args:
            request: FastAPI request object
            call_next: Next middleware or route handler
            
        Returns:
            Response with enhanced performance headers and metrics
        """
        start_time = time.time()
        request_id = f"perf_{int(start_time * 1000000)}"
        
        # Track concurrent request count
        async with self._lock:
            self._concurrent_requests += 1
            peak_concurrent = self._concurrent_requests
        
        # Store performance context
        request.state.performance_context = {
            "request_id": request_id,
            "start_time": start_time,
            "concurrent_at_start": peak_concurrent
        }
        
        try:
            # Process request
            response = await call_next(request)
            
            # Calculate performance metrics
            processing_time_ms = (time.time() - start_time) * 1000
            
            # Update performance tracking
            await self._update_performance_metrics(
                request_id, processing_time_ms, True, request.url.path
            )
            
            # Add performance headers
            response.headers["X-Processing-Time-Ms"] = f"{processing_time_ms:.2f}"
            response.headers["X-Request-ID"] = request_id
            response.headers["X-Concurrent-Peak"] = str(peak_concurrent)
            
            if processing_time_ms > self.performance_threshold_ms:
                response.headers["X-Performance-Warning"] = "slow"
                
            if processing_time_ms > self.slow_request_threshold_ms:
                response.headers["X-Performance-Alert"] = "very-slow"
                logger.warning(
                    f"Very slow request {request_id}: {processing_time_ms:.2f}ms "
                    f"for {request.url.path}"
                )
            
            # Add detailed metrics if enabled
            if self.enable_detailed_metrics:
                avg_time = await self._get_average_response_time()
                response.headers["X-Avg-Response-Time-Ms"] = f"{avg_time:.2f}"
                response.headers["X-Performance-Optimized"] = "true"
            
            return response
            
        except Exception as e:
            # Track failed request
            processing_time_ms = (time.time() - start_time) * 1000
            await self._update_performance_metrics(
                request_id, processing_time_ms, False, request.url.path
            )
            
            logger.error(f"Request {request_id} failed after {processing_time_ms:.2f}ms: {e}")
            raise
            
        finally:
            # Update concurrent request count
            async with self._lock:
                self._concurrent_requests = max(0, self._concurrent_requests - 1)
    
    async def _update_performance_metrics(
        self,
        request_id: str,
        processing_time_ms: float,
        success: bool,
        path: str
    ):
        """Update internal performance metrics."""
        async with self._lock:
            # Update response times (keep last 1000)
            self._request_times.append(processing_time_ms)
            if len(self._request_times) > 1000:
                self._request_times = self._request_times[-1000:]
            
            # Track slow requests
            if processing_time_ms > self.slow_request_threshold_ms:
                slow_request = {
                    "request_id": request_id,
                    "processing_time_ms": processing_time_ms,
                    "path": path,
                    "timestamp": time.time(),
                    "success": success
                }
                self._slow_requests.append(slow_request)
                
                # Keep last 100 slow requests
                if len(self._slow_requests) > 100:
                    self._slow_requests = self._slow_requests[-100:]
    
    async def _get_average_response_time(self) -> float:
        """Get current average response time."""
        if not self._request_times:
            return 0.0
        return sum(self._request_times) / len(self._request_times)
    
    def get_performance_stats(self) -> Dict[str, Any]:
        """Get current performance statistics."""
        if not self._request_times:
            return {
                "total_requests": 0,
                "avg_response_time_ms": 0.0,
                "concurrent_requests": self._concurrent_requests,
                "slow_requests_count": 0
            }
        
        return {
            "total_requests": len(self._request_times),
            "avg_response_time_ms": sum(self._request_times) / len(self._request_times),
            "min_response_time_ms": min(self._request_times),
            "max_response_time_ms": max(self._request_times),
            "concurrent_requests": self._concurrent_requests,
            "slow_requests_count": len(self._slow_requests),
            "p95_response_time_ms": (
                sorted(self._request_times)[int(0.95 * len(self._request_times))]
                if len(self._request_times) > 0 else 0.0
            )
        }


class AsyncLoadBalancingMiddleware(BaseHTTPMiddleware):
    """
    Middleware for async load balancing and request distribution optimization.
    
    Implements intelligent request routing, adaptive throttling, and
    resource-aware load distribution for enhanced server-side performance.
    """
    
    def __init__(
        self,
        app,
        enable_adaptive_throttling: bool = True,
        max_queue_size: int = 10000,
        load_balance_strategy: str = "round_robin"
    ):
        """Initialize async load balancing middleware."""
        super().__init__(app)
        self.enable_adaptive_throttling = enable_adaptive_throttling
        self.max_queue_size = max_queue_size
        self.load_balance_strategy = load_balance_strategy
        
        # Load balancing state
        self._request_queue = asyncio.Queue(maxsize=max_queue_size)
        self._active_workers = 0
        self._max_workers = 50  # Could be configurable
        self._worker_semaphore = asyncio.Semaphore(self._max_workers)
        
        # Load distribution tracking
        self._endpoint_loads = {}
        self._lock = asyncio.Lock()
        
        logger.info(f"AsyncLoadBalancingMiddleware initialized with {load_balance_strategy} strategy")
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """
        Apply load balancing and request distribution optimization.
        
        Args:
            request: FastAPI request object  
            call_next: Next middleware or route handler
            
        Returns:
            Response with load balancing headers and optimization metadata
        """
        endpoint = request.url.path
        start_time = time.time()
        
        # Check if we should throttle this request
        if self.enable_adaptive_throttling:
            should_throttle = await self._should_throttle_request(endpoint)
            if should_throttle:
                return Response(
                    content="Request throttled due to high load",
                    status_code=429,
                    headers={
                        "X-Load-Balancing": "throttled",
                        "X-Endpoint": endpoint,
                        "Retry-After": "5"
                    }
                )
        
        # Acquire worker semaphore for load balancing
        async with self._worker_semaphore:
            try:
                # Track endpoint load
                await self._track_endpoint_load(endpoint, start=True)
                
                # Process request
                response = await call_next(request)
                
                # Add load balancing headers
                processing_time_ms = (time.time() - start_time) * 1000
                response.headers["X-Load-Balanced"] = "true"
                response.headers["X-Worker-ID"] = f"worker_{self._active_workers}"
                response.headers["X-Endpoint-Load"] = str(
                    self._endpoint_loads.get(endpoint, {}).get("current_load", 0)
                )
                response.headers["X-Processing-Time-Ms"] = f"{processing_time_ms:.2f}"
                
                return response
                
            finally:
                # Track endpoint load completion
                await self._track_endpoint_load(endpoint, start=False)
    
    async def _should_throttle_request(self, endpoint: str) -> bool:
        """Determine if request should be throttled based on current load."""
        async with self._lock:
            endpoint_info = self._endpoint_loads.get(endpoint, {})
            current_load = endpoint_info.get("current_load", 0)
            avg_response_time = endpoint_info.get("avg_response_time", 0)
            
            # Throttle if endpoint is heavily loaded or responding slowly
            return (
                current_load > 20 or  # More than 20 concurrent requests to this endpoint
                avg_response_time > 5000  # Average response time > 5 seconds
            )
    
    async def _track_endpoint_load(self, endpoint: str, start: bool):
        """Track load for specific endpoint."""
        async with self._lock:
            if endpoint not in self._endpoint_loads:
                self._endpoint_loads[endpoint] = {
                    "current_load": 0,
                    "total_requests": 0,
                    "total_response_time": 0,
                    "avg_response_time": 0
                }
            
            if start:
                self._endpoint_loads[endpoint]["current_load"] += 1
                self._endpoint_loads[endpoint]["total_requests"] += 1
                self._active_workers += 1
            else:
                self._endpoint_loads[endpoint]["current_load"] = max(
                    0, self._endpoint_loads[endpoint]["current_load"] - 1
                )
                self._active_workers = max(0, self._active_workers - 1)
    
    def get_load_balancing_stats(self) -> Dict[str, Any]:
        """Get current load balancing statistics."""
        return {
            "active_workers": self._active_workers,
            "max_workers": self._max_workers,
            "available_workers": self._max_workers - self._active_workers,
            "load_balance_strategy": self.load_balance_strategy,
            "endpoint_loads": self._endpoint_loads.copy(),
            "adaptive_throttling_enabled": self.enable_adaptive_throttling
        }