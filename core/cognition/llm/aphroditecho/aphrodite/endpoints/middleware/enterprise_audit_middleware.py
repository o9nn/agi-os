"""
Enterprise audit middleware integrating comprehensive security logging,
privacy compliance, and incident response for Aphrodite Engine.

This middleware provides a unified security layer that orchestrates audit logging,
privacy tracking, and real-time threat detection with Echo systems integration.
"""

import time
import asyncio
from datetime import datetime
from typing import Any, Callable, Dict, Optional
import logging

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
import structlog

from ..security.audit_logger import (
    EnterpriseAuditLogger,
    AuditConfig,
    AuditEventType,
    AuditSeverity,
    get_audit_logger,
    configure_audit_logging
)
from ..security.privacy_compliance import (
    PrivacyComplianceManager,
    DataProcessingPurpose,
    DataCategory,
    PrivacyRegulation,
    get_privacy_manager
)
from ..security.incident_response import (
    IncidentResponseEngine,
    get_incident_engine,
    process_security_event
)

# Configure enterprise audit logger
enterprise_logger = structlog.get_logger("enterprise_audit")


class EnterpriseAuditConfig:
    """Configuration for enterprise audit middleware."""
    
    def __init__(
        self,
        # Audit Configuration
        audit_enabled: bool = True,
        audit_config: AuditConfig = None,
        
        # Privacy Configuration  
        privacy_enabled: bool = True,
        privacy_regulation: PrivacyRegulation = PrivacyRegulation.GDPR,
        
        # Security Configuration
        incident_response_enabled: bool = True,
        auto_threat_detection: bool = True,
        
        # Echo Systems Integration
        echo_integration_enabled: bool = True,
        dtesn_processing_tracking: bool = True,
        aar_orchestration_logging: bool = True,
        
        # Performance
        async_processing: bool = True,
        batch_processing: bool = True,
        
        # Compliance
        compliance_mode: str = "strict",  # strict, standard, minimal
        data_retention_days: int = 90,
        
        # Filtering
        exclude_health_checks: bool = True,
        exclude_static_assets: bool = True,
        log_only_errors: bool = False
    ):
        """Initialize enterprise audit configuration."""
        self.audit_enabled = audit_enabled
        self.audit_config = audit_config or AuditConfig()
        
        self.privacy_enabled = privacy_enabled  
        self.privacy_regulation = privacy_regulation
        
        self.incident_response_enabled = incident_response_enabled
        self.auto_threat_detection = auto_threat_detection
        
        self.echo_integration_enabled = echo_integration_enabled
        self.dtesn_processing_tracking = dtesn_processing_tracking
        self.aar_orchestration_logging = aar_orchestration_logging
        
        self.async_processing = async_processing
        self.batch_processing = batch_processing
        
        self.compliance_mode = compliance_mode
        self.data_retention_days = data_retention_days
        
        self.exclude_health_checks = exclude_health_checks
        self.exclude_static_assets = exclude_static_assets
        self.log_only_errors = log_only_errors


class EnterpriseAuditMiddleware(BaseHTTPMiddleware):
    """
    Comprehensive enterprise audit middleware.
    
    Provides unified security orchestration with:
    - Comprehensive audit logging with structured events
    - Privacy compliance tracking (GDPR, CCPA, etc.)
    - Real-time security incident detection and response
    - Echo systems integration for enhanced AI security
    - Performance monitoring and compliance reporting
    """
    
    def __init__(self, app, config: EnterpriseAuditConfig = None):
        """Initialize enterprise audit middleware."""
        super().__init__(app)
        
        # Configuration
        self.config = config or EnterpriseAuditConfig()
        
        # Initialize components
        self._initialize_components()
        
        # Request correlation
        self.active_requests = {}  # request_id -> request_context
        
        # Performance tracking
        self.performance_metrics = {
            'total_requests': 0,
            'audit_processing_time': 0.0,
            'privacy_processing_time': 0.0,
            'security_processing_time': 0.0
        }
        
        enterprise_logger.info("Enterprise audit middleware initialized")
    
    def _initialize_components(self):
        """Initialize audit, privacy, and security components."""
        
        # Initialize audit logger
        if self.config.audit_enabled:
            configure_audit_logging(self.config.audit_config)
            self.audit_logger = get_audit_logger()
        else:
            self.audit_logger = None
        
        # Initialize privacy manager
        if self.config.privacy_enabled:
            self.privacy_manager = get_privacy_manager(self.config.privacy_regulation)
        else:
            self.privacy_manager = None
        
        # Initialize incident response engine
        if self.config.incident_response_enabled:
            self.incident_engine = get_incident_engine()
        else:
            self.incident_engine = None
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Process request with comprehensive enterprise audit capabilities."""
        
        start_time = time.time()
        
        # Generate request correlation IDs
        request_id = self._generate_request_id()
        trace_id = self._generate_trace_id(request)
        
        # Set request context
        request.state.request_id = request_id
        request.state.trace_id = trace_id
        request.state.audit_start_time = start_time
        
        # Check if request should be excluded
        if self._should_exclude_request(request):
            return await call_next(request)
        
        # Extract request context
        context = await self._extract_request_context(request)
        
        try:
            # Log request start
            if self.audit_logger:
                await self._audit_request_start(request, context)
            
            # Track data processing for privacy compliance
            if self.privacy_manager:
                await self._track_data_processing(request, context)
            
            # Security event processing
            if self.incident_engine and self.config.auto_threat_detection:
                await self._process_security_event(request, context)
            
            # Process request through middleware chain
            response = await call_next(request)
            
            # Post-processing
            processing_time = time.time() - start_time
            
            # Log successful response
            if self.audit_logger:
                await self._audit_request_success(request, response, context, processing_time)
            
            # Check for security anomalies in response
            if self.incident_engine:
                await self._analyze_response_security(request, response, context)
            
            # Echo systems integration
            if self.config.echo_integration_enabled:
                await self._integrate_echo_systems(request, response, context)
            
            # Update performance metrics
            self._update_performance_metrics(processing_time)
            
            # Add audit headers to response
            response.headers["X-Audit-Request-ID"] = request_id
            response.headers["X-Audit-Trace-ID"] = trace_id
            response.headers["X-Audit-Processing-Time"] = f"{processing_time:.3f}"
            
            return response
            
        except Exception as e:
            # Handle request failure
            error_time = time.time() - start_time
            
            # Log error with full context
            if self.audit_logger:
                await self._audit_request_error(request, e, context, error_time)
            
            # Security incident for suspicious errors
            if self.incident_engine:
                await self._handle_error_security(request, e, context)
            
            # Re-raise exception
            raise
        
        finally:
            # Cleanup request context
            self._cleanup_request_context(request_id)
    
    def _generate_request_id(self) -> str:
        """Generate unique request ID."""
        import uuid
        return f"req_{uuid.uuid4().hex[:12]}"
    
    def _generate_trace_id(self, request: Request) -> str:
        """Generate or extract trace ID."""
        # Check for existing trace ID
        existing_trace = request.headers.get("X-Trace-ID")
        if existing_trace:
            return existing_trace
        
        # Generate new trace ID
        import uuid
        return f"trace_{uuid.uuid4().hex[:16]}"
    
    def _should_exclude_request(self, request: Request) -> bool:
        """Check if request should be excluded from audit logging."""
        
        path = request.url.path.lower()
        
        # Exclude health checks
        if self.config.exclude_health_checks:
            health_patterns = ["/health", "/healthz", "/ping", "/status"]
            if any(pattern in path for pattern in health_patterns):
                return True
        
        # Exclude static assets
        if self.config.exclude_static_assets:
            static_patterns = [".css", ".js", ".png", ".jpg", ".jpeg", ".gif", ".ico", ".woff", ".ttf"]
            if any(pattern in path for pattern in static_patterns):
                return True
        
        return False
    
    async def _extract_request_context(self, request: Request) -> Dict[str, Any]:
        """Extract comprehensive request context for audit logging."""
        
        # Get client IP with proxy support
        client_ip = request.client.host if request.client else "unknown"
        forwarded_for = request.headers.get('x-forwarded-for')
        if forwarded_for:
            client_ip = forwarded_for.split(',')[0].strip()
        
        # Extract user information
        user_id = (
            request.headers.get("X-User-ID") or
            getattr(request.state, 'user_id', None) or
            self._extract_user_from_auth(request)
        )
        
        session_id = (
            request.headers.get("X-Session-ID") or
            request.cookies.get("session_id") or
            getattr(request.state, 'session_id', None)
        )
        
        # Request details
        context = {
            "client_ip": client_ip,
            "user_agent": request.headers.get("user-agent"),
            "user_id": user_id,
            "session_id": session_id,
            "method": request.method,
            "endpoint": str(request.url.path),
            "query_params": dict(request.query_params),
            "content_type": request.headers.get("content-type"),
            "content_length": request.headers.get("content-length"),
            "referer": request.headers.get("referer"),
            "origin": request.headers.get("origin"),
            "host": request.headers.get("host"),
            "scheme": request.url.scheme
        }
        
        # Enhanced context for API requests
        if request.url.path.startswith('/v1/'):
            context["api_version"] = "v1"
            context["api_endpoint"] = True
        
        # Echo systems context
        if self.config.echo_integration_enabled:
            context["echo_context"] = {
                "dtesn_enabled": getattr(request.state, 'dtesn_enabled', False),
                "aar_session": getattr(request.state, 'aar_session', None),
                "evolution_active": getattr(request.state, 'evolution_active', False)
            }
        
        return context
    
    def _extract_user_from_auth(self, request: Request) -> Optional[str]:
        """Extract user ID from authentication headers."""
        
        auth_header = request.headers.get("Authorization", "")
        if auth_header.startswith("Bearer "):
            # Could decode JWT token here to extract user ID
            # For now, return a placeholder
            return "authenticated_user"
        
        return None
    
    async def _audit_request_start(self, request: Request, context: Dict[str, Any]):
        """Audit log request start with comprehensive context."""
        
        await self.audit_logger.log_request_start(
            request=request,
            request_id=context.get("request_id", request.state.request_id),
            trace_id=context.get("trace_id", request.state.trace_id)
        )
    
    async def _audit_request_success(
        self, 
        request: Request, 
        response: Response, 
        context: Dict[str, Any],
        processing_time: float
    ):
        """Audit log successful request completion."""
        
        await self.audit_logger.log_request_success(
            request=request,
            response=response,
            request_id=request.state.request_id,
            processing_time_ms=processing_time * 1000,
            trace_id=request.state.trace_id
        )
    
    async def _audit_request_error(
        self,
        request: Request,
        error: Exception,
        context: Dict[str, Any],
        processing_time: float
    ):
        """Audit log request error with full context."""
        
        await self.audit_logger.log_event(
            event_type=AuditEventType.API_REQUEST_FAILURE,
            message=f"Request failed: {str(error)}",
            severity=AuditSeverity.HIGH,
            request_id=request.state.request_id,
            trace_id=request.state.trace_id,
            client_ip=context.get("client_ip"),
            user_id=context.get("user_id"),
            endpoint=context.get("endpoint"),
            method=context.get("method"),
            processing_time_ms=processing_time * 1000,
            success=False,
            error_code=type(error).__name__,
            error_message=str(error),
            details=context
        )
    
    async def _track_data_processing(self, request: Request, context: Dict[str, Any]):
        """Track data processing for privacy compliance."""
        
        # Only track requests that process personal data
        if not self._involves_personal_data(request, context):
            return
        
        # Determine processing purpose
        purpose = self._determine_processing_purpose(request)
        
        # Determine data categories
        data_categories = self._identify_data_categories(request, context)
        
        # Record data processing
        await self.privacy_manager.record_data_processing(
            data_subject_id=context.get("user_id", "anonymous"),
            purpose=purpose,
            data_categories=data_categories,
            description=f"API request: {request.method} {request.url.path}",
            legal_basis="legitimate_interest",  # Configure based on use case
            request=request
        )
    
    def _involves_personal_data(self, request: Request, context: Dict[str, Any]) -> bool:
        """Check if request involves personal data processing."""
        
        # Check if user is identified
        if context.get("user_id"):
            return True
        
        # Check for personal data in request
        personal_data_indicators = [
            "email", "phone", "name", "address", "user", "profile", "account"
        ]
        
        path = request.url.path.lower()
        return any(indicator in path for indicator in personal_data_indicators)
    
    def _determine_processing_purpose(self, request: Request) -> DataProcessingPurpose:
        """Determine the purpose of data processing from request."""
        
        path = request.url.path.lower()
        
        if "/chat" in path or "/completions" in path:
            return DataProcessingPurpose.SERVICE_PROVISION
        elif "/analytics" in path or "/metrics" in path:
            return DataProcessingPurpose.ANALYTICS
        elif "/security" in path or "/audit" in path:
            return DataProcessingPurpose.SECURITY_MONITORING
        else:
            return DataProcessingPurpose.SERVICE_PROVISION
    
    def _identify_data_categories(self, request: Request, context: Dict[str, Any]) -> List[DataCategory]:
        """Identify categories of data being processed."""
        
        categories = []
        
        # Always include technical data for API requests
        categories.append(DataCategory.TECHNICAL_DATA)
        
        # Add usage data
        categories.append(DataCategory.USAGE_DATA)
        
        # Check for identity data
        if context.get("user_id"):
            categories.append(DataCategory.IDENTITY_DATA)
        
        # Check for behavioral data in AI interactions
        if "/chat" in request.url.path or "/completions" in request.url.path:
            categories.append(DataCategory.BEHAVIORAL_DATA)
        
        return categories
    
    async def _process_security_event(self, request: Request, context: Dict[str, Any]):
        """Process request as security event for threat detection."""
        
        # Prepare security event data
        security_data = {
            "headers": dict(request.headers),
            "query_params": context.get("query_params", {}),
            "content_type": context.get("content_type"),
            "content_length": context.get("content_length")
        }
        
        # Add request body for analysis if it's reasonably sized
        if request.method in ["POST", "PUT", "PATCH"]:
            content_length = context.get("content_length")
            if content_length and int(content_length) < 10240:  # 10KB limit
                try:
                    body = await request.body()
                    if body:
                        security_data["request_body"] = body.decode('utf-8', errors='ignore')
                except Exception:
                    pass  # Ignore body read errors
        
        # Process security event
        incident_id = await process_security_event(
            request=request,
            event_type="api_request",
            description=f"{request.method} {request.url.path}",
            raw_data=security_data,
            user_id=context.get("user_id"),
            session_id=context.get("session_id"),
            request_id=request.state.request_id
        )
        
        # Store incident ID in request state for later reference
        if incident_id:
            request.state.security_incident_id = incident_id
    
    async def _analyze_response_security(
        self, 
        request: Request, 
        response: Response, 
        context: Dict[str, Any]
    ):
        """Analyze response for security anomalies."""
        
        # Check for suspicious response patterns
        suspicious_indicators = []
        
        # Check response status
        if response.status_code >= 500:
            suspicious_indicators.append("server_error")
        elif response.status_code == 403:
            suspicious_indicators.append("access_denied")
        elif response.status_code == 429:
            suspicious_indicators.append("rate_limited")
        
        # Check response headers for security issues
        if not response.headers.get("X-Content-Type-Options"):
            suspicious_indicators.append("missing_security_headers")
        
        # Log security analysis if issues found
        if suspicious_indicators:
            await process_security_event(
                request=request,
                event_type="response_analysis",
                description=f"Response security analysis: {', '.join(suspicious_indicators)}",
                raw_data={
                    "status_code": response.status_code,
                    "indicators": suspicious_indicators,
                    "response_headers": dict(response.headers)
                }
            )
    
    async def _integrate_echo_systems(
        self, 
        request: Request, 
        response: Response, 
        context: Dict[str, Any]
    ):
        """Integrate with Echo systems for enhanced AI security."""
        
        # DTESN processing tracking
        if self.config.dtesn_processing_tracking:
            dtesn_context = getattr(request.state, 'dtesn_context', None)
            if dtesn_context:
                await self.audit_logger.log_echo_event(
                    echo_system="kern",
                    operation="dtesn_processing",
                    success=dtesn_context.get("success", True),
                    processing_time_ms=dtesn_context.get("processing_time", 0),
                    details=dtesn_context
                )
        
        # AAR orchestration logging
        if self.config.aar_orchestration_logging:
            aar_context = getattr(request.state, 'aar_context', None)
            if aar_context:
                await self.audit_logger.log_echo_event(
                    echo_system="dream",
                    operation="aar_orchestration",
                    success=aar_context.get("success", True),
                    agents_involved=aar_context.get("agent_count", 0),
                    details=aar_context
                )
        
        # Evolution engine events
        evolution_event = getattr(request.state, 'evolution_event', None)
        if evolution_event:
            await self.audit_logger.log_echo_event(
                echo_system="self",
                operation="evolution_event",
                success=evolution_event.get("success", True),
                evolution_type=evolution_event.get("type", "unknown"),
                details=evolution_event
            )
    
    def _handle_error_security(self, request: Request, error: Exception, context: Dict[str, Any]):
        """Handle security aspects of request errors."""
        
        # Check for suspicious error patterns
        error_str = str(error).lower()
        suspicious_patterns = [
            "injection", "xss", "script", "eval", "exec", 
            "union", "select", "drop", "insert", "update",
            "../", "..\\", "passwd", "shadow", "etc/hosts"
        ]
        
        if any(pattern in error_str for pattern in suspicious_patterns):
            # Log as potential attack
            asyncio.create_task(
                process_security_event(
                    request=request,
                    event_type="potential_attack",
                    description=f"Suspicious error pattern: {type(error).__name__}",
                    raw_data={
                        "error_type": type(error).__name__,
                        "error_message": str(error),
                        "suspicious_patterns": [p for p in suspicious_patterns if p in error_str]
                    }
                )
            )
    
    def _update_performance_metrics(self, processing_time: float):
        """Update performance metrics."""
        self.performance_metrics['total_requests'] += 1
        # Add more granular timing metrics as needed
    
    def _cleanup_request_context(self, request_id: str):
        """Clean up request context after processing."""
        self.active_requests.pop(request_id, None)
    
    async def get_audit_status(self) -> Dict[str, Any]:
        """Get current audit middleware status."""
        
        return {
            "middleware_status": "active",
            "configuration": {
                "audit_enabled": self.config.audit_enabled,
                "privacy_enabled": self.config.privacy_enabled,
                "incident_response_enabled": self.config.incident_response_enabled,
                "echo_integration_enabled": self.config.echo_integration_enabled
            },
            "performance_metrics": self.performance_metrics,
            "active_requests": len(self.active_requests),
            "components_status": {
                "audit_logger": self.audit_logger is not None,
                "privacy_manager": self.privacy_manager is not None,
                "incident_engine": self.incident_engine is not None
            }
        }
    
    async def generate_compliance_report(
        self,
        start_date: datetime = None,
        end_date: datetime = None
    ) -> Dict[str, Any]:
        """Generate comprehensive compliance report."""
        
        report = {
            "report_generated_at": datetime.utcnow().isoformat(),
            "middleware_config": {
                "compliance_mode": self.config.compliance_mode,
                "privacy_regulation": self.config.privacy_regulation.value,
                "data_retention_days": self.config.data_retention_days
            }
        }
        
        # Add audit report
        if self.audit_logger:
            audit_report = await self.audit_logger.generate_compliance_report(
                start_date or datetime.utcnow(),
                end_date or datetime.utcnow()
            )
            report["audit_compliance"] = audit_report
        
        # Add privacy report
        if self.privacy_manager:
            privacy_report = await self.privacy_manager.generate_privacy_report(
                start_date,
                end_date
            )
            report["privacy_compliance"] = privacy_report
        
        # Add security report
        if self.incident_engine:
            security_dashboard = await self.incident_engine.get_security_dashboard()
            report["security_status"] = security_dashboard
        
        return report