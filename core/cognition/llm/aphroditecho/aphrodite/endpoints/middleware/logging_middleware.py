"""
Comprehensive logging middleware for FastAPI applications.

Provides structured request/response logging with context tracking, security events,
and performance correlation for complete observability.
"""

import time
import uuid
from typing import Any, Callable, Dict, Optional
from datetime import datetime
import logging
import traceback

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from loguru import logger
import structlog

# Configure structured logging
structlog.configure(
    processors=[
        structlog.stdlib.filter_by_level,
        structlog.stdlib.add_logger_name,
        structlog.stdlib.add_log_level,
        structlog.stdlib.PositionalArgumentsFormatter(),
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.StackInfoRenderer(),
        structlog.processors.format_exc_info,
        structlog.processors.UnicodeDecoder(),
        structlog.processors.JSONRenderer()
    ],
    context_class=dict,
    logger_factory=structlog.stdlib.LoggerFactory(),
    wrapper_class=structlog.stdlib.BoundLogger,
    cache_logger_on_first_use=True,
)

class LoggingConfig:
    """Configuration for logging middleware."""
    
    def __init__(
        self,
        log_requests: bool = True,
        log_responses: bool = True,
        log_request_body: bool = False,
        log_response_body: bool = False,
        max_body_size: int = 1024,
        sensitive_headers: set = None,
        log_level: str = "INFO",
        enable_performance_logging: bool = True,
        enable_security_logging: bool = True,
        enable_dtesn_logging: bool = True
    ):
        self.log_requests = log_requests
        self.log_responses = log_responses
        self.log_request_body = log_request_body
        self.log_response_body = log_response_body
        self.max_body_size = max_body_size
        self.sensitive_headers = sensitive_headers or {
            'authorization', 'x-api-key', 'cookie', 'set-cookie'
        }
        self.log_level = log_level
        self.enable_performance_logging = enable_performance_logging
        self.enable_security_logging = enable_security_logging
        self.enable_dtesn_logging = enable_dtesn_logging


class RequestContext:
    """Request context for correlation and tracking."""
    
    def __init__(self, request: Request):
        self.request_id = self._get_or_create_request_id(request)
        self.trace_id = self._get_or_create_trace_id(request)
        self.user_id = self._extract_user_id(request)
        self.session_id = self._extract_session_id(request)
        self.client_ip = self._get_client_ip(request)
        self.user_agent = request.headers.get("user-agent", "Unknown")
        self.start_time = time.time()
        self.endpoint = request.url.path
        self.method = request.method
        self.query_params = dict(request.query_params)
        
    def _get_or_create_request_id(self, request: Request) -> str:
        """Get existing request ID or create a new one."""
        return request.headers.get("X-Request-ID", f"req_{uuid.uuid4().hex[:12]}")
    
    def _get_or_create_trace_id(self, request: Request) -> str:
        """Get existing trace ID or create a new one."""
        return request.headers.get("X-Trace-ID", f"trace_{uuid.uuid4().hex[:16]}")
    
    def _extract_user_id(self, request: Request) -> Optional[str]:
        """Extract user ID from various sources."""
        # Try different authentication patterns
        auth_header = request.headers.get("authorization", "")
        if auth_header.startswith("Bearer "):
            # Could decode JWT here if needed
            return "authenticated_user"
        return request.headers.get("X-User-ID")
    
    def _extract_session_id(self, request: Request) -> Optional[str]:
        """Extract session ID from headers or cookies."""
        return request.headers.get("X-Session-ID") or request.cookies.get("session_id")
    
    def _get_client_ip(self, request: Request) -> str:
        """Get client IP with proxy support."""
        forwarded_for = request.headers.get("X-Forwarded-For")
        if forwarded_for:
            return forwarded_for.split(",")[0].strip()
        
        real_ip = request.headers.get("X-Real-IP")
        if real_ip:
            return real_ip
        
        return request.client.host if request.client else "unknown"
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert context to dictionary for logging."""
        return {
            "request_id": self.request_id,
            "trace_id": self.trace_id,
            "user_id": self.user_id,
            "session_id": self.session_id,
            "client_ip": self.client_ip,
            "user_agent": self.user_agent,
            "endpoint": self.endpoint,
            "method": self.method,
            "query_params": self.query_params,
            "start_time": self.start_time
        }


class ComprehensiveLoggingMiddleware(BaseHTTPMiddleware):
    """
    Comprehensive logging middleware with structured logging, context tracking,
    and correlation across request/response lifecycle.
    """
    
    def __init__(self, app, config: LoggingConfig = None):
        """Initialize comprehensive logging middleware."""
        super().__init__(app)
        self.config = config or LoggingConfig()
        self.structured_logger = structlog.get_logger("middleware.logging")
        
        # Set up logging level
        logging.getLogger().setLevel(getattr(logging, self.config.log_level))
        
        logger.info("ComprehensiveLoggingMiddleware initialized with structured logging")
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Process request with comprehensive logging."""
        
        # Create request context
        context = RequestContext(request)
        
        # Attach context to request state
        request.state.logging_context = context
        request.state.request_id = context.request_id
        request.state.trace_id = context.trace_id
        
        # Log request start
        if self.config.log_requests:
            await self._log_request_start(request, context)
        
        response = None
        error_info = None
        
        try:
            # Process request through middleware chain
            response = await call_next(request)
            
            # Log successful response
            if self.config.log_responses:
                await self._log_response(request, response, context)
                
        except Exception as e:
            # Log error details
            error_info = {
                "error_type": type(e).__name__,
                "error_message": str(e),
                "error_traceback": traceback.format_exc()
            }
            
            await self._log_error(request, context, error_info)
            
            # Re-raise the exception
            raise
        
        finally:
            # Log request completion
            await self._log_request_completion(request, response, context, error_info)
        
        # Add correlation headers to response
        if response:
            response.headers["X-Request-ID"] = context.request_id
            response.headers["X-Trace-ID"] = context.trace_id
        
        return response
    
    async def _log_request_start(self, request: Request, context: RequestContext):
        """Log request start with full context."""
        
        # Prepare request data
        request_data = {
            "event": "request_start",
            "timestamp": datetime.utcnow().isoformat(),
            **context.to_dict(),
            "headers": self._filter_sensitive_headers(dict(request.headers)),
            "content_type": request.headers.get("content-type"),
            "content_length": request.headers.get("content-length")
        }
        
        # Add request body if configured
        if self.config.log_request_body:
            try:
                body = await self._read_request_body(request)
                if body and len(body) <= self.config.max_body_size:
                    request_data["request_body"] = body
                else:
                    request_data["request_body_size"] = len(body) if body else 0
            except Exception as e:
                request_data["body_read_error"] = str(e)
        
        # Log with structured logger
        self.structured_logger.info(
            "HTTP request started",
            **request_data
        )
    
    async def _log_response(self, request: Request, response: Response, context: RequestContext):
        """Log response details."""
        
        processing_time = time.time() - context.start_time
        
        response_data = {
            "event": "request_success",
            "request_id": context.request_id,
            "trace_id": context.trace_id,
            "status_code": response.status_code,
            "processing_time_ms": round(processing_time * 1000, 2),
            "response_headers": self._filter_sensitive_headers(dict(response.headers)),
            "content_type": response.headers.get("content-type"),
            "content_length": response.headers.get("content-length")
        }
        
        # Add DTESN specific logging if enabled
        if self.config.enable_dtesn_logging:
            dtesn_context = getattr(request.state, 'dtesn_context', None)
            if dtesn_context:
                response_data["dtesn_processed"] = dtesn_context.get("dtesn_processed", False)
                response_data["dtesn_processing_time_ms"] = dtesn_context.get("processing_time_ms", 0)
        
        # Add performance logging if enabled
        if self.config.enable_performance_logging:
            response_data.update({
                "performance_category": self._categorize_performance(processing_time),
                "slow_request": processing_time > 1.0,
                "memory_usage_mb": self._get_memory_usage()
            })
        
        # Add response body if configured
        if self.config.log_response_body and hasattr(response, 'body'):
            try:
                if len(response.body) <= self.config.max_body_size:
                    response_data["response_body"] = response.body.decode('utf-8')
                else:
                    response_data["response_body_size"] = len(response.body)
            except Exception as e:
                response_data["body_log_error"] = str(e)
        
        self.structured_logger.info(
            "HTTP request completed successfully",
            **response_data
        )
    
    async def _log_error(self, request: Request, context: RequestContext, error_info: Dict[str, Any]):
        """Log error details with full context."""
        
        processing_time = time.time() - context.start_time
        
        error_data = {
            "event": "request_error",
            "request_id": context.request_id,
            "trace_id": context.trace_id,
            "processing_time_ms": round(processing_time * 1000, 2),
            **error_info,
            "endpoint": context.endpoint,
            "method": context.method,
            "client_ip": context.client_ip
        }
        
        self.structured_logger.error(
            "HTTP request failed with error",
            **error_data
        )
    
    async def _log_request_completion(
        self, 
        request: Request, 
        response: Optional[Response], 
        context: RequestContext,
        error_info: Optional[Dict[str, Any]]
    ):
        """Log request completion summary."""
        
        processing_time = time.time() - context.start_time
        
        completion_data = {
            "event": "request_complete",
            "request_id": context.request_id,
            "trace_id": context.trace_id,
            "total_processing_time_ms": round(processing_time * 1000, 2),
            "success": error_info is None,
            "status_code": response.status_code if response else None
        }
        
        # Add security logging if enabled
        if self.config.enable_security_logging:
            completion_data.update({
                "security_processed": request.headers.get("X-Security-Processed") == "true",
                "rate_limited": response.status_code == 429 if response else False,
                "suspicious_activity": getattr(request.state, 'security_anomaly', False)
            })
        
        self.structured_logger.info(
            "Request processing completed",
            **completion_data
        )
    
    def _filter_sensitive_headers(self, headers: Dict[str, str]) -> Dict[str, str]:
        """Filter out sensitive headers for logging."""
        filtered = {}
        for key, value in headers.items():
            if key.lower() in self.config.sensitive_headers:
                filtered[key] = "[FILTERED]"
            else:
                filtered[key] = value
        return filtered
    
    async def _read_request_body(self, request: Request) -> Optional[str]:
        """Safely read request body for logging."""
        try:
            body = await request.body()
            return body.decode('utf-8') if body else None
        except Exception:
            return None
    
    def _categorize_performance(self, processing_time: float) -> str:
        """Categorize request performance."""
        if processing_time < 0.1:
            return "fast"
        elif processing_time < 0.5:
            return "normal"
        elif processing_time < 1.0:
            return "slow"
        else:
            return "very_slow"
    
    def _get_memory_usage(self) -> float:
        """Get current memory usage in MB."""
        try:
            import psutil
            process = psutil.Process()
            return round(process.memory_info().rss / 1024 / 1024, 2)
        except ImportError:
            return 0.0