import time
import asyncio
from datetime import datetime
from typing import Any, Callable, Dict, Optional
import logging
from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
import structlog
from ..security.audit_logger import EnterpriseAuditLogger, AuditConfig, AuditEventType, AuditSeverity, get_audit_logger, configure_audit_logging
from ..security.privacy_compliance import PrivacyComplianceManager, DataProcessingPurpose, DataCategory, PrivacyRegulation, get_privacy_manager
from ..security.incident_response import IncidentResponseEngine, get_incident_engine, process_security_event
enterprise_logger = structlog.get_logger('enterprise_audit')
class EnterpriseAuditConfig:
    def __init__(self, audit_enabled: bool=True, audit_config: AuditConfig=None, privacy_enabled: bool=True, privacy_regulation: PrivacyRegulation=PrivacyRegulation.GDPR, incident_response_enabled: bool=True, auto_threat_detection: bool=True, echo_integration_enabled: bool=True, dtesn_processing_tracking: bool=True, aar_orchestration_logging: bool=True, async_processing: bool=True, batch_processing: bool=True, compliance_mode: str='strict', data_retention_days: int=90, exclude_health_checks: bool=True, exclude_static_assets: bool=True, log_only_errors: bool=False):
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
    def __init__(self, app, config: EnterpriseAuditConfig=None):
        super().__init__(app)
        self.config = config or EnterpriseAuditConfig()
        self._initialize_components()
        self.active_requests = {}
        self.performance_metrics = {'total_requests': 0, 'audit_processing_time': 0.0, 'privacy_processing_time': 0.0, 'security_processing_time': 0.0}
        enterprise_logger.info('Enterprise audit middleware initialized')
    def _initialize_components(self):
        if self.config.audit_enabled:
            configure_audit_logging(self.config.audit_config)
            self.audit_logger = get_audit_logger()
        else:
            self.audit_logger = None
        if self.config.privacy_enabled:
            self.privacy_manager = get_privacy_manager(self.config.privacy_regulation)
        else:
            self.privacy_manager = None
        if self.config.incident_response_enabled:
            self.incident_engine = get_incident_engine()
        else:
            self.incident_engine = None
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        start_time = time.time()
        request_id = self._generate_request_id()
        trace_id = self._generate_trace_id(request)
        request.state.request_id = request_id
        request.state.trace_id = trace_id
        request.state.audit_start_time = start_time
        if self._should_exclude_request(request):
            return await call_next(request)
        context = await self._extract_request_context(request)
        try:
            if self.audit_logger:
                await self._audit_request_start(request, context)
            if self.privacy_manager:
                await self._track_data_processing(request, context)
            if self.incident_engine and self.config.auto_threat_detection:
                await self._process_security_event(request, context)
            response = await call_next(request)
            processing_time = time.time() - start_time
            if self.audit_logger:
                await self._audit_request_success(request, response, context, processing_time)
            if self.incident_engine:
                await self._analyze_response_security(request, response, context)
            if self.config.echo_integration_enabled:
                await self._integrate_echo_systems(request, response, context)
            self._update_performance_metrics(processing_time)
            response.headers['X-Audit-Request-ID'] = request_id
            response.headers['X-Audit-Trace-ID'] = trace_id
            response.headers['X-Audit-Processing-Time'] = f'{processing_time:.3f}'
            return response
        except Exception as e:
            error_time = time.time() - start_time
            if self.audit_logger:
                await self._audit_request_error(request, e, context, error_time)
            if self.incident_engine:
                await self._handle_error_security(request, e, context)
            raise
        finally:
            self._cleanup_request_context(request_id)
    def _generate_request_id(self) -> str:
        import uuid
        return f'req_{uuid.uuid4().hex[:12]}'
    def _generate_trace_id(self, request: Request) -> str:
        existing_trace = request.headers.get('X-Trace-ID')
        if existing_trace:
            return existing_trace
        import uuid
        return f'trace_{uuid.uuid4().hex[:16]}'
    def _should_exclude_request(self, request: Request) -> bool:
        path = request.url.path.lower()
        if self.config.exclude_health_checks:
            health_patterns = ['/health', '/healthz', '/ping', '/status']
            if any((pattern in path for pattern in health_patterns)):
                return True
        if self.config.exclude_static_assets:
            static_patterns = ['.css', '.js', '.png', '.jpg', '.jpeg', '.gif', '.ico', '.woff', '.ttf']
            if any((pattern in path for pattern in static_patterns)):
                return True
        return False
    async def _extract_request_context(self, request: Request) -> Dict[str, Any]:
        client_ip = request.client.host if request.client else 'unknown'
        forwarded_for = request.headers.get('x-forwarded-for')
        if forwarded_for:
            client_ip = forwarded_for.split(',')[0].strip()
        user_id = request.headers.get('X-User-ID') or getattr(request.state, 'user_id', None) or self._extract_user_from_auth(request)
        session_id = request.headers.get('X-Session-ID') or request.cookies.get('session_id') or getattr(request.state, 'session_id', None)
        context = {'client_ip': client_ip, 'user_agent': request.headers.get('user-agent'), 'user_id': user_id, 'session_id': session_id, 'method': request.method, 'endpoint': str(request.url.path), 'query_params': dict(request.query_params), 'content_type': request.headers.get('content-type'), 'content_length': request.headers.get('content-length'), 'referer': request.headers.get('referer'), 'origin': request.headers.get('origin'), 'host': request.headers.get('host'), 'scheme': request.url.scheme}
        if request.url.path.startswith('/v1/'):
            context['api_version'] = 'v1'
            context['api_endpoint'] = True
        if self.config.echo_integration_enabled:
            context['echo_context'] = {'dtesn_enabled': getattr(request.state, 'dtesn_enabled', False), 'aar_session': getattr(request.state, 'aar_session', None), 'evolution_active': getattr(request.state, 'evolution_active', False)}
        return context
    def _extract_user_from_auth(self, request: Request) -> Optional[str]:
        auth_header = request.headers.get('Authorization', '')
        if auth_header.startswith('Bearer '):
            return 'authenticated_user'
        return None
    async def _audit_request_start(self, request: Request, context: Dict[str, Any]):
        await self.audit_logger.log_request_start(request=request, request_id=context.get('request_id', request.state.request_id), trace_id=context.get('trace_id', request.state.trace_id))
    async def _audit_request_success(self, request: Request, response: Response, context: Dict[str, Any], processing_time: float):
        await self.audit_logger.log_request_success(request=request, response=response, request_id=request.state.request_id, processing_time_ms=processing_time * 1000, trace_id=request.state.trace_id)
    async def _audit_request_error(self, request: Request, error: Exception, context: Dict[str, Any], processing_time: float):
        await self.audit_logger.log_event(event_type=AuditEventType.API_REQUEST_FAILURE, message=f'Request failed: {str(error)}', severity=AuditSeverity.HIGH, request_id=request.state.request_id, trace_id=request.state.trace_id, client_ip=context.get('client_ip'), user_id=context.get('user_id'), endpoint=context.get('endpoint'), method=context.get('method'), processing_time_ms=processing_time * 1000, success=False, error_code=type(error).__name__, error_message=str(error), details=context)
    async def _track_data_processing(self, request: Request, context: Dict[str, Any]):
        if not self._involves_personal_data(request, context):
            return
        purpose = self._determine_processing_purpose(request)
        data_categories = self._identify_data_categories(request, context)
        await self.privacy_manager.record_data_processing(data_subject_id=context.get('user_id', 'anonymous'), purpose=purpose, data_categories=data_categories, description=f'API request: {request.method} {request.url.path}', legal_basis='legitimate_interest', request=request)
    def _involves_personal_data(self, request: Request, context: Dict[str, Any]) -> bool:
        if context.get('user_id'):
            return True
        personal_data_indicators = ['email', 'phone', 'name', 'address', 'user', 'profile', 'account']
        path = request.url.path.lower()
        return any((indicator in path for indicator in personal_data_indicators))
    def _determine_processing_purpose(self, request: Request) -> DataProcessingPurpose:
        path = request.url.path.lower()
        if '/chat' in path or '/completions' in path:
            return DataProcessingPurpose.SERVICE_PROVISION
        elif '/analytics' in path or '/metrics' in path:
            return DataProcessingPurpose.ANALYTICS
        elif '/security' in path or '/audit' in path:
            return DataProcessingPurpose.SECURITY_MONITORING
        else:
            return DataProcessingPurpose.SERVICE_PROVISION
    def _identify_data_categories(self, request: Request, context: Dict[str, Any]) -> List[DataCategory]:
        categories = []
        categories.append(DataCategory.TECHNICAL_DATA)
        categories.append(DataCategory.USAGE_DATA)
        if context.get('user_id'):
            categories.append(DataCategory.IDENTITY_DATA)
        if '/chat' in request.url.path or '/completions' in request.url.path:
            categories.append(DataCategory.BEHAVIORAL_DATA)
        return categories
    async def _process_security_event(self, request: Request, context: Dict[str, Any]):
        security_data = {'headers': dict(request.headers), 'query_params': context.get('query_params', {}), 'content_type': context.get('content_type'), 'content_length': context.get('content_length')}
        if request.method in ['POST', 'PUT', 'PATCH']:
            content_length = context.get('content_length')
            if content_length and int(content_length) < 10240:
                try:
                    body = await request.body()
                    if body:
                        security_data['request_body'] = body.decode('utf-8', errors='ignore')
                except Exception:
                    pass
        incident_id = await process_security_event(request=request, event_type='api_request', description=f'{request.method} {request.url.path}', raw_data=security_data, user_id=context.get('user_id'), session_id=context.get('session_id'), request_id=request.state.request_id)
        if incident_id:
            request.state.security_incident_id = incident_id
    async def _analyze_response_security(self, request: Request, response: Response, context: Dict[str, Any]):
        suspicious_indicators = []
        if response.status_code >= 500:
            suspicious_indicators.append('server_error')
        elif response.status_code == 403:
            suspicious_indicators.append('access_denied')
        elif response.status_code == 429:
            suspicious_indicators.append('rate_limited')
        if not response.headers.get('X-Content-Type-Options'):
            suspicious_indicators.append('missing_security_headers')
        if suspicious_indicators:
            await process_security_event(request=request, event_type='response_analysis', description=f"Response security analysis: {', '.join(suspicious_indicators)}", raw_data={'status_code': response.status_code, 'indicators': suspicious_indicators, 'response_headers': dict(response.headers)})
    async def _integrate_echo_systems(self, request: Request, response: Response, context: Dict[str, Any]):
        if self.config.dtesn_processing_tracking:
            dtesn_context = getattr(request.state, 'dtesn_context', None)
            if dtesn_context:
                await self.audit_logger.log_echo_event(echo_system='kern', operation='dtesn_processing', success=dtesn_context.get('success', True), processing_time_ms=dtesn_context.get('processing_time', 0), details=dtesn_context)
        if self.config.aar_orchestration_logging:
            aar_context = getattr(request.state, 'aar_context', None)
            if aar_context:
                await self.audit_logger.log_echo_event(echo_system='dream', operation='aar_orchestration', success=aar_context.get('success', True), agents_involved=aar_context.get('agent_count', 0), details=aar_context)
        evolution_event = getattr(request.state, 'evolution_event', None)
        if evolution_event:
            await self.audit_logger.log_echo_event(echo_system='self', operation='evolution_event', success=evolution_event.get('success', True), evolution_type=evolution_event.get('type', 'unknown'), details=evolution_event)
    def _handle_error_security(self, request: Request, error: Exception, context: Dict[str, Any]):
        error_str = str(error).lower()
        suspicious_patterns = ['injection', 'xss', 'script', 'eval', 'exec', 'union', 'select', 'drop', 'insert', 'update', '../', '..\\', 'passwd', 'shadow', 'etc/hosts']
        if any((pattern in error_str for pattern in suspicious_patterns)):
            asyncio.create_task(process_security_event(request=request, event_type='potential_attack', description=f'Suspicious error pattern: {type(error).__name__}', raw_data={'error_type': type(error).__name__, 'error_message': str(error), 'suspicious_patterns': [p for p in suspicious_patterns if p in error_str]}))
    def _update_performance_metrics(self, processing_time: float):
        self.performance_metrics['total_requests'] += 1
    def _cleanup_request_context(self, request_id: str):
        self.active_requests.pop(request_id, None)
    async def get_audit_status(self) -> Dict[str, Any]:
        return {'middleware_status': 'active', 'configuration': {'audit_enabled': self.config.audit_enabled, 'privacy_enabled': self.config.privacy_enabled, 'incident_response_enabled': self.config.incident_response_enabled, 'echo_integration_enabled': self.config.echo_integration_enabled}, 'performance_metrics': self.performance_metrics, 'active_requests': len(self.active_requests), 'components_status': {'audit_logger': self.audit_logger is not None, 'privacy_manager': self.privacy_manager is not None, 'incident_engine': self.incident_engine is not None}}
    async def generate_compliance_report(self, start_date: datetime=None, end_date: datetime=None) -> Dict[str, Any]:
        report = {'report_generated_at': datetime.utcnow().isoformat(), 'middleware_config': {'compliance_mode': self.config.compliance_mode, 'privacy_regulation': self.config.privacy_regulation.value, 'data_retention_days': self.config.data_retention_days}}
        if self.audit_logger:
            audit_report = await self.audit_logger.generate_compliance_report(start_date or datetime.utcnow(), end_date or datetime.utcnow())
            report['audit_compliance'] = audit_report
        if self.privacy_manager:
            privacy_report = await self.privacy_manager.generate_privacy_report(start_date, end_date)
            report['privacy_compliance'] = privacy_report
        if self.incident_engine:
            security_dashboard = await self.incident_engine.get_security_dashboard()
            report['security_status'] = security_dashboard
        return report