import time
import uuid
import json
import asyncio
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from enum import Enum
from dataclasses import dataclass
from pathlib import Path
import hashlib
import os
from fastapi import Request, Response
import structlog
from pydantic import BaseModel
audit_logger = structlog.get_logger('audit')
class AuditEventType(str, Enum):
    AUTH_LOGIN_SUCCESS = 'auth.login.success'
    AUTH_LOGIN_FAILURE = 'auth.login.failure'
    AUTH_LOGOUT = 'auth.logout'
    AUTH_TOKEN_ISSUED = 'auth.token.issued'
    AUTH_TOKEN_REVOKED = 'auth.token.revoked'
    AUTH_ACCESS_DENIED = 'auth.access.denied'
    API_REQUEST_START = 'api.request.start'
    API_REQUEST_SUCCESS = 'api.request.success'
    API_REQUEST_FAILURE = 'api.request.failure'
    API_REQUEST_TIMEOUT = 'api.request.timeout'
    API_RATE_LIMITED = 'api.rate_limited'
    MODEL_INFERENCE_START = 'model.inference.start'
    MODEL_INFERENCE_SUCCESS = 'model.inference.success'
    MODEL_INFERENCE_FAILURE = 'model.inference.failure'
    MODEL_LOAD = 'model.load'
    MODEL_UNLOAD = 'model.unload'
    MODEL_CONFIG_CHANGE = 'model.config.change'
    SECURITY_ANOMALY_DETECTED = 'security.anomaly.detected'
    SECURITY_IP_BLOCKED = 'security.ip.blocked'
    SECURITY_SUSPICIOUS_ACTIVITY = 'security.suspicious.activity'
    SECURITY_ATTACK_ATTEMPT = 'security.attack.attempt'
    SECURITY_POLICY_VIOLATION = 'security.policy.violation'
    DATA_ACCESS = 'data.access'
    DATA_EXPORT = 'data.export'
    DATA_DELETION = 'data.deletion'
    DATA_RETENTION_POLICY_APPLIED = 'data.retention.applied'
    PII_DETECTED = 'data.pii.detected'
    SYSTEM_START = 'system.start'
    SYSTEM_STOP = 'system.stop'
    SYSTEM_CONFIG_CHANGE = 'system.config.change'
    SYSTEM_ERROR = 'system.error'
    SYSTEM_HEALTH_CHECK = 'system.health.check'
    ECHO_SYSTEM_INTERACTION = 'echo.system.interaction'
    DTESN_PROCESSING = 'echo.dtesn.processing'
    AAR_ORCHESTRATION = 'echo.aar.orchestration'
    EVOLUTION_ENGINE_EVENT = 'echo.evolution.event'
class AuditSeverity(str, Enum):
    LOW = 'low'
    MEDIUM = 'medium'
    HIGH = 'high'
    CRITICAL = 'critical'
@dataclass
class AuditEvent:
    event_id: str
    event_type: AuditEventType
    timestamp: datetime
    severity: AuditSeverity
    user_id: Optional[str] = None
    session_id: Optional[str] = None
    client_ip: str = 'unknown'
    user_agent: Optional[str] = None
    request_id: Optional[str] = None
    trace_id: Optional[str] = None
    endpoint: Optional[str] = None
    method: Optional[str] = None
    message: str = ''
    details: Dict[str, Any] = None
    security_context: Dict[str, Any] = None
    echo_context: Dict[str, Any] = None
    success: bool = True
    error_code: Optional[str] = None
    error_message: Optional[str] = None
    processing_time_ms: Optional[float] = None
    resource_usage: Dict[str, Any] = None
    def __post_init__(self):
        if self.details is None:
            self.details = {}
        if self.security_context is None:
            self.security_context = {}
        if self.echo_context is None:
            self.echo_context = {}
        if self.resource_usage is None:
            self.resource_usage = {}
    def to_dict(self) -> Dict[str, Any]:
        return {'event_id': self.event_id, 'event_type': self.event_type.value, 'timestamp': self.timestamp.isoformat(), 'severity': self.severity.value, 'user_id': self.user_id, 'session_id': self.session_id, 'client_ip': self.client_ip, 'user_agent': self.user_agent, 'request_id': self.request_id, 'trace_id': self.trace_id, 'endpoint': self.endpoint, 'method': self.method, 'message': self.message, 'details': self.details, 'security_context': self.security_context, 'echo_context': self.echo_context, 'success': self.success, 'error_code': self.error_code, 'error_message': self.error_message, 'processing_time_ms': self.processing_time_ms, 'resource_usage': self.resource_usage}
class AuditConfig(BaseModel):
    enabled: bool = True
    log_level: str = 'INFO'
    log_authentication_events: bool = True
    log_api_operations: bool = True
    log_model_operations: bool = True
    log_security_events: bool = True
    log_data_privacy_events: bool = True
    log_system_events: bool = True
    log_echo_events: bool = True
    include_request_bodies: bool = False
    include_response_bodies: bool = False
    max_body_size: int = 1024
    include_headers: bool = True
    include_performance_metrics: bool = True
    storage_backend: str = 'file'
    file_storage_path: str = '/var/log/aphrodite/audit'
    max_file_size_mb: int = 100
    max_files: int = 50
    retention_days: int = 90
    auto_purge_enabled: bool = True
    encrypt_audit_logs: bool = False
    hash_sensitive_data: bool = True
    pii_detection_enabled: bool = True
    async_logging: bool = True
    batch_size: int = 100
    flush_interval_seconds: int = 5
    compliance_mode: str = 'standard'
    include_compliance_fields: bool = True
class PIIDetector:
    def __init__(self):
        self.patterns = {'email': '\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b', 'phone': '\\b\\d{3}[-.]?\\d{3}[-.]?\\d{4}\\b', 'ssn': '\\b\\d{3}-\\d{2}-\\d{4}\\b', 'credit_card': '\\b\\d{4}[-\\s]?\\d{4}[-\\s]?\\d{4}[-\\s]?\\d{4}\\b', 'ip_address': '\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b'}
    def detect_pii(self, text: str) -> List[Dict[str, str]]:
        import re
        findings = []
        for pii_type, pattern in self.patterns.items():
            matches = re.finditer(pattern, text)
            for match in matches:
                findings.append({'type': pii_type, 'value': match.group(), 'position': match.span()})
        return findings
    def sanitize_pii(self, text: str, hash_data: bool=True) -> str:
        import re
        sanitized = text
        for pii_type, pattern in self.patterns.items():
            if hash_data:
                def hash_match(match):
                    hashed = hashlib.sha256(match.group().encode()).hexdigest()[:8]
                    return f'[{pii_type.upper()}:{hashed}]'
                sanitized = re.sub(pattern, hash_match, sanitized)
            else:
                sanitized = re.sub(pattern, f'[{pii_type.upper()}_REDACTED]', sanitized)
        return sanitized
class AuditStorage:
    async def store_event(self, event: AuditEvent) -> bool:
        raise NotImplementedError
    async def store_events(self, events: List[AuditEvent]) -> bool:
        raise NotImplementedError
    async def query_events(self, start_time: datetime, end_time: datetime, event_types: List[AuditEventType]=None, user_id: str=None, limit: int=1000) -> List[AuditEvent]:
        raise NotImplementedError
    async def purge_old_events(self, older_than: datetime) -> int:
        raise NotImplementedError
class FileAuditStorage(AuditStorage):
    def __init__(self, config: AuditConfig):
        self.config = config
        self.storage_path = Path(config.file_storage_path)
        self.storage_path.mkdir(parents=True, exist_ok=True)
        self.current_file = None
        self.current_file_size = 0
        self.events_buffer = []
        self.max_size_bytes = config.max_file_size_mb * 1024 * 1024
    async def store_event(self, event: AuditEvent) -> bool:
        return await self.store_events([event])
    async def store_events(self, events: List[AuditEvent]) -> bool:
        try:
            if not self.current_file or self._should_rotate():
                await self._rotate_file()
            for event in events:
                event_json = json.dumps(event.to_dict(), separators=(',', ':'))
                line = f'{event_json}\n'
                self.current_file.write(line)
                self.current_file_size += len(line.encode('utf-8'))
            self.current_file.flush()
            os.fsync(self.current_file.fileno())
            return True
        except Exception as e:
            audit_logger.error(f'Failed to store audit events: {e}')
            return False
    def _should_rotate(self) -> bool:
        return self.current_file is None or self.current_file_size >= self.max_size_bytes
    async def _rotate_file(self):
        if self.current_file:
            self.current_file.close()
        timestamp = datetime.utcnow().strftime('%Y%m%d_%H%M%S')
        filename = f'audit_{timestamp}.jsonl'
        filepath = self.storage_path / filename
        self.current_file = open(filepath, 'w')
        self.current_file_size = 0
        await self._cleanup_old_files()
    async def _cleanup_old_files(self):
        audit_files = sorted(self.storage_path.glob('audit_*.jsonl'), key=lambda p: p.stat().st_mtime, reverse=True)
        for old_file in audit_files[self.config.max_files:]:
            try:
                old_file.unlink()
                audit_logger.info(f'Removed old audit file: {old_file}')
            except Exception as e:
                audit_logger.error(f'Failed to remove old audit file {old_file}: {e}')
    async def query_events(self, start_time: datetime, end_time: datetime, event_types: List[AuditEventType]=None, user_id: str=None, limit: int=1000) -> List[AuditEvent]:
        events = []
        try:
            audit_files = sorted(self.storage_path.glob('audit_*.jsonl'))
            for file_path in audit_files:
                if len(events) >= limit:
                    break
                with open(file_path, 'r') as f:
                    for line in f:
                        if len(events) >= limit:
                            break
                        try:
                            event_data = json.loads(line.strip())
                            event_time = datetime.fromisoformat(event_data['timestamp'])
                            if event_time < start_time or event_time > end_time:
                                continue
                            if event_types and event_data['event_type'] not in [t.value for t in event_types]:
                                continue
                            if user_id and event_data.get('user_id') != user_id:
                                continue
                            events.append(event_data)
                        except json.JSONDecodeError:
                            continue
        except Exception as e:
            audit_logger.error(f'Failed to query audit events: {e}')
        return events
    async def purge_old_events(self, older_than: datetime) -> int:
        purged_count = 0
        try:
            audit_files = list(self.storage_path.glob('audit_*.jsonl'))
            for file_path in audit_files:
                file_time = datetime.fromtimestamp(file_path.stat().st_mtime)
                if file_time < older_than:
                    try:
                        file_path.unlink()
                        purged_count += 1
                        audit_logger.info(f'Purged old audit file: {file_path}')
                    except Exception as e:
                        audit_logger.error(f'Failed to purge audit file {file_path}: {e}')
        except Exception as e:
            audit_logger.error(f'Failed to purge old audit events: {e}')
        return purged_count
class EnterpriseAuditLogger:
    def __init__(self, config: AuditConfig=None):
        self.config = config or AuditConfig()
        self.pii_detector = PIIDetector()
        if self.config.storage_backend == 'file':
            self.storage = FileAuditStorage(self.config)
        else:
            self.storage = FileAuditStorage(self.config)
        self.event_buffer = []
        self.buffer_lock = asyncio.Lock()
        self.last_flush = time.time()
        if self.config.async_logging:
            asyncio.create_task(self._flush_events_periodically())
        audit_logger.info('Enterprise audit logger initialized')
    async def log_event(self, event_type: AuditEventType, message: str='', severity: AuditSeverity=AuditSeverity.MEDIUM, **kwargs) -> str:
        event_id = f'audit_{uuid.uuid4().hex[:12]}'
        event = AuditEvent(event_id=event_id, event_type=event_type, timestamp=datetime.utcnow(), severity=severity, message=message, **{k: v for k, v in kwargs.items() if v is not None})
        if self.config.pii_detection_enabled:
            pii_findings = self.pii_detector.detect_pii(message)
            if pii_findings:
                pii_event = AuditEvent(event_id=f'pii_{uuid.uuid4().hex[:12]}', event_type=AuditEventType.PII_DETECTED, timestamp=datetime.utcnow(), severity=AuditSeverity.HIGH, message=f'PII detected in audit event {event_id}', details={'pii_types': [f['type'] for f in pii_findings]}, security_context={'original_event_id': event_id})
                await self._store_single_event(pii_event)
                if self.config.hash_sensitive_data:
                    event.message = self.pii_detector.sanitize_pii(message, hash_data=True)
        await self._store_event(event)
        return event_id
    async def log_request_start(self, request: Request, request_id: str, trace_id: str=None) -> str:
        context = self._extract_request_context(request)
        return await self.log_event(event_type=AuditEventType.API_REQUEST_START, message=f'API request started: {request.method} {request.url.path}', severity=AuditSeverity.LOW, request_id=request_id, trace_id=trace_id, endpoint=str(request.url.path), method=request.method, **context)
    async def log_request_success(self, request: Request, response: Response, request_id: str, processing_time_ms: float=None, trace_id: str=None) -> str:
        context = self._extract_request_context(request)
        return await self.log_event(event_type=AuditEventType.API_REQUEST_SUCCESS, message=f'API request completed: {request.method} {request.url.path} -> {response.status_code}', severity=AuditSeverity.LOW, request_id=request_id, trace_id=trace_id, endpoint=str(request.url.path), method=request.method, processing_time_ms=processing_time_ms, details={'status_code': response.status_code}, **context)
    async def log_security_event(self, event_type: AuditEventType, message: str, client_ip: str, severity: AuditSeverity=AuditSeverity.HIGH, **kwargs) -> str:
        return await self.log_event(event_type=event_type, message=message, severity=severity, client_ip=client_ip, security_context=kwargs.get('security_context', {}), **kwargs)
    async def log_echo_event(self, echo_system: str, operation: str, success: bool=True, processing_time_ms: float=None, **kwargs) -> str:
        return await self.log_event(event_type=AuditEventType.ECHO_SYSTEM_INTERACTION, message=f'Echo.{echo_system} operation: {operation}', severity=AuditSeverity.MEDIUM, success=success, processing_time_ms=processing_time_ms, echo_context={'system': echo_system, 'operation': operation, **kwargs})
    def _extract_request_context(self, request: Request) -> Dict[str, Any]:
        client_ip = request.client.host if request.client else 'unknown'
        forwarded_for = request.headers.get('x-forwarded-for')
        if forwarded_for:
            client_ip = forwarded_for.split(',')[0].strip()
        return {'client_ip': client_ip, 'user_agent': request.headers.get('user-agent'), 'user_id': request.headers.get('x-user-id') or getattr(request.state, 'user_id', None), 'session_id': request.headers.get('x-session-id') or request.cookies.get('session_id'), 'details': {'query_params': dict(request.query_params), 'content_type': request.headers.get('content-type'), 'content_length': request.headers.get('content-length')}}
    async def _store_event(self, event: AuditEvent):
        if self.config.async_logging:
            async with self.buffer_lock:
                self.event_buffer.append(event)
                if len(self.event_buffer) >= self.config.batch_size:
                    await self._flush_events()
        else:
            await self._store_single_event(event)
    async def _store_single_event(self, event: AuditEvent):
        try:
            success = await self.storage.store_event(event)
            if not success:
                import sys
                print(f'AUDIT_FALLBACK: {json.dumps(event.to_dict())}', file=sys.stderr)
        except Exception as e:
            audit_logger.error(f'Failed to store audit event: {e}')
    async def _flush_events(self):
        async with self.buffer_lock:
            if self.event_buffer:
                events_to_store = self.event_buffer.copy()
                self.event_buffer.clear()
                self.last_flush = time.time()
                try:
                    await self.storage.store_events(events_to_store)
                    audit_logger.debug(f'Flushed {len(events_to_store)} audit events')
                except Exception as e:
                    audit_logger.error(f'Failed to flush audit events: {e}')
                    self.event_buffer.extend(events_to_store)
    async def _flush_events_periodically(self):
        while True:
            try:
                await asyncio.sleep(self.config.flush_interval_seconds)
                if time.time() - self.last_flush >= self.config.flush_interval_seconds:
                    await self._flush_events()
            except Exception as e:
                audit_logger.error(f'Error in periodic flush task: {e}')
    async def query_events(self, start_time: datetime=None, end_time: datetime=None, event_types: List[AuditEventType]=None, user_id: str=None, limit: int=1000) -> List[Dict[str, Any]]:
        if start_time is None:
            start_time = datetime.utcnow() - timedelta(days=1)
        if end_time is None:
            end_time = datetime.utcnow()
        return await self.storage.query_events(start_time=start_time, end_time=end_time, event_types=event_types, user_id=user_id, limit=limit)
    async def generate_compliance_report(self, start_date: datetime, end_date: datetime, report_type: str='standard') -> Dict[str, Any]:
        events = await self.query_events(start_time=start_date, end_time=end_date, limit=10000)
        event_counts = {}
        security_events = []
        data_privacy_events = []
        for event in events:
            event_type = event.get('event_type', 'unknown')
            event_counts[event_type] = event_counts.get(event_type, 0) + 1
            if 'security' in event_type:
                security_events.append(event)
            elif 'data' in event_type or 'pii' in event_type:
                data_privacy_events.append(event)
        return {'report_type': report_type, 'period': {'start': start_date.isoformat(), 'end': end_date.isoformat()}, 'summary': {'total_events': len(events), 'event_type_counts': event_counts, 'security_events_count': len(security_events), 'data_privacy_events_count': len(data_privacy_events)}, 'security_summary': {'total_security_events': len(security_events), 'high_severity_events': len([e for e in security_events if e.get('severity') == 'high']), 'critical_events': len([e for e in security_events if e.get('severity') == 'critical'])}, 'data_privacy_summary': {'total_data_events': len(data_privacy_events), 'pii_detections': len([e for e in data_privacy_events if 'pii' in e.get('event_type', '')])}}
    async def cleanup_old_events(self, retention_days: int=None) -> int:
        if retention_days is None:
            retention_days = self.config.retention_days
        cutoff_date = datetime.utcnow() - timedelta(days=retention_days)
        try:
            purged_count = await self.storage.purge_old_events(cutoff_date)
            await self.log_event(event_type=AuditEventType.DATA_RETENTION_POLICY_APPLIED, message=f'Purged {purged_count} audit events older than {retention_days} days', severity=AuditSeverity.MEDIUM, details={'purged_count': purged_count, 'retention_days': retention_days})
            return purged_count
        except Exception as e:
            audit_logger.error(f'Failed to cleanup old audit events: {e}')
            return 0
_global_audit_logger: Optional[EnterpriseAuditLogger] = None
def get_audit_logger(config: AuditConfig=None) -> EnterpriseAuditLogger:
    global _global_audit_logger
    if _global_audit_logger is None:
        _global_audit_logger = EnterpriseAuditLogger(config)
    return _global_audit_logger
def configure_audit_logging(config: AuditConfig):
    global _global_audit_logger
    _global_audit_logger = EnterpriseAuditLogger(config)
async def audit_log(event_type: AuditEventType, message: str='', severity: AuditSeverity=AuditSeverity.MEDIUM, **kwargs) -> str:
    logger = get_audit_logger()
    return await logger.log_event(event_type, message, severity, **kwargs)