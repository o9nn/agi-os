"""
Enterprise-grade audit logging system for Aphrodite Engine.

Provides comprehensive audit logging for all server operations, security events,
and compliance tracking with structured event schemas and retention policies.
"""

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


# Configure structured audit logging
audit_logger = structlog.get_logger("audit")


class AuditEventType(str, Enum):
    """Types of audit events tracked by the system."""
    
    # Authentication & Authorization
    AUTH_LOGIN_SUCCESS = "auth.login.success"
    AUTH_LOGIN_FAILURE = "auth.login.failure"
    AUTH_LOGOUT = "auth.logout"
    AUTH_TOKEN_ISSUED = "auth.token.issued"
    AUTH_TOKEN_REVOKED = "auth.token.revoked"
    AUTH_ACCESS_DENIED = "auth.access.denied"
    
    # API Operations  
    API_REQUEST_START = "api.request.start"
    API_REQUEST_SUCCESS = "api.request.success"
    API_REQUEST_FAILURE = "api.request.failure"
    API_REQUEST_TIMEOUT = "api.request.timeout"
    API_RATE_LIMITED = "api.rate_limited"
    
    # Model Operations
    MODEL_INFERENCE_START = "model.inference.start"
    MODEL_INFERENCE_SUCCESS = "model.inference.success"
    MODEL_INFERENCE_FAILURE = "model.inference.failure"
    MODEL_LOAD = "model.load"
    MODEL_UNLOAD = "model.unload"
    MODEL_CONFIG_CHANGE = "model.config.change"
    
    # Security Events
    SECURITY_ANOMALY_DETECTED = "security.anomaly.detected"
    SECURITY_IP_BLOCKED = "security.ip.blocked"
    SECURITY_SUSPICIOUS_ACTIVITY = "security.suspicious.activity"
    SECURITY_ATTACK_ATTEMPT = "security.attack.attempt"
    SECURITY_POLICY_VIOLATION = "security.policy.violation"
    
    # Data Privacy & Compliance
    DATA_ACCESS = "data.access"
    DATA_EXPORT = "data.export"
    DATA_DELETION = "data.deletion"
    DATA_RETENTION_POLICY_APPLIED = "data.retention.applied"
    PII_DETECTED = "data.pii.detected"
    
    # System Operations
    SYSTEM_START = "system.start"
    SYSTEM_STOP = "system.stop"
    SYSTEM_CONFIG_CHANGE = "system.config.change"
    SYSTEM_ERROR = "system.error"
    SYSTEM_HEALTH_CHECK = "system.health.check"
    
    # Echo Systems Integration
    ECHO_SYSTEM_INTERACTION = "echo.system.interaction"
    DTESN_PROCESSING = "echo.dtesn.processing"
    AAR_ORCHESTRATION = "echo.aar.orchestration"
    EVOLUTION_ENGINE_EVENT = "echo.evolution.event"


class AuditSeverity(str, Enum):
    """Audit event severity levels."""
    
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class AuditEvent:
    """Structured audit event with complete metadata."""
    
    # Core Event Properties
    event_id: str
    event_type: AuditEventType
    timestamp: datetime
    severity: AuditSeverity
    
    # Actor Information
    user_id: Optional[str] = None
    session_id: Optional[str] = None
    client_ip: str = "unknown"
    user_agent: Optional[str] = None
    
    # Request Context
    request_id: Optional[str] = None
    trace_id: Optional[str] = None
    endpoint: Optional[str] = None
    method: Optional[str] = None
    
    # Event Details
    message: str = ""
    details: Dict[str, Any] = None
    
    # Security Context
    security_context: Dict[str, Any] = None
    
    # Echo Systems Context
    echo_context: Dict[str, Any] = None
    
    # Result Information
    success: bool = True
    error_code: Optional[str] = None
    error_message: Optional[str] = None
    
    # Performance Metrics
    processing_time_ms: Optional[float] = None
    resource_usage: Dict[str, Any] = None
    
    def __post_init__(self):
        """Initialize default values after creation."""
        if self.details is None:
            self.details = {}
        if self.security_context is None:
            self.security_context = {}
        if self.echo_context is None:
            self.echo_context = {}
        if self.resource_usage is None:
            self.resource_usage = {}
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert audit event to dictionary for logging."""
        return {
            "event_id": self.event_id,
            "event_type": self.event_type.value,
            "timestamp": self.timestamp.isoformat(),
            "severity": self.severity.value,
            "user_id": self.user_id,
            "session_id": self.session_id,
            "client_ip": self.client_ip,
            "user_agent": self.user_agent,
            "request_id": self.request_id,
            "trace_id": self.trace_id,
            "endpoint": self.endpoint,
            "method": self.method,
            "message": self.message,
            "details": self.details,
            "security_context": self.security_context,
            "echo_context": self.echo_context,
            "success": self.success,
            "error_code": self.error_code,
            "error_message": self.error_message,
            "processing_time_ms": self.processing_time_ms,
            "resource_usage": self.resource_usage
        }


class AuditConfig(BaseModel):
    """Configuration for audit logging system."""
    
    enabled: bool = True
    log_level: str = "INFO"
    
    # Event Filtering
    log_authentication_events: bool = True
    log_api_operations: bool = True
    log_model_operations: bool = True
    log_security_events: bool = True
    log_data_privacy_events: bool = True
    log_system_events: bool = True
    log_echo_events: bool = True
    
    # Event Details
    include_request_bodies: bool = False
    include_response_bodies: bool = False
    max_body_size: int = 1024
    include_headers: bool = True
    include_performance_metrics: bool = True
    
    # Storage Configuration
    storage_backend: str = "file"  # file, database, elasticsearch
    file_storage_path: str = "/var/log/aphrodite/audit"
    max_file_size_mb: int = 100
    max_files: int = 50
    
    # Retention Policy
    retention_days: int = 90
    auto_purge_enabled: bool = True
    
    # Security
    encrypt_audit_logs: bool = False
    hash_sensitive_data: bool = True
    pii_detection_enabled: bool = True
    
    # Performance
    async_logging: bool = True
    batch_size: int = 100
    flush_interval_seconds: int = 5
    
    # Compliance
    compliance_mode: str = "standard"  # standard, gdpr, hipaa, sox
    include_compliance_fields: bool = True


class PIIDetector:
    """Detect and handle personally identifiable information in audit logs."""
    
    def __init__(self):
        """Initialize PII detection patterns."""
        self.patterns = {
            'email': r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b',
            'phone': r'\b\d{3}[-.]?\d{3}[-.]?\d{4}\b',
            'ssn': r'\b\d{3}-\d{2}-\d{4}\b',
            'credit_card': r'\b\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}\b',
            'ip_address': r'\b(?:\d{1,3}\.){3}\d{1,3}\b'
        }
    
    def detect_pii(self, text: str) -> List[Dict[str, str]]:
        """Detect PII in text and return findings."""
        import re
        
        findings = []
        for pii_type, pattern in self.patterns.items():
            matches = re.finditer(pattern, text)
            for match in matches:
                findings.append({
                    'type': pii_type,
                    'value': match.group(),
                    'position': match.span()
                })
        
        return findings
    
    def sanitize_pii(self, text: str, hash_data: bool = True) -> str:
        """Remove or hash PII from text."""
        import re
        
        sanitized = text
        for pii_type, pattern in self.patterns.items():
            if hash_data:
                # Replace with hash
                def hash_match(match):
                    hashed = hashlib.sha256(match.group().encode()).hexdigest()[:8]
                    return f"[{pii_type.upper()}:{hashed}]"
                sanitized = re.sub(pattern, hash_match, sanitized)
            else:
                # Replace with placeholder
                sanitized = re.sub(pattern, f"[{pii_type.upper()}_REDACTED]", sanitized)
        
        return sanitized


class AuditStorage:
    """Base class for audit event storage backends."""
    
    async def store_event(self, event: AuditEvent) -> bool:
        """Store a single audit event."""
        raise NotImplementedError
    
    async def store_events(self, events: List[AuditEvent]) -> bool:
        """Store multiple audit events in batch."""
        raise NotImplementedError
    
    async def query_events(
        self, 
        start_time: datetime, 
        end_time: datetime, 
        event_types: List[AuditEventType] = None,
        user_id: str = None,
        limit: int = 1000
    ) -> List[AuditEvent]:
        """Query audit events with filters."""
        raise NotImplementedError
    
    async def purge_old_events(self, older_than: datetime) -> int:
        """Purge events older than specified date."""
        raise NotImplementedError


class FileAuditStorage(AuditStorage):
    """File-based audit storage with rotation and compression."""
    
    def __init__(self, config: AuditConfig):
        """Initialize file storage."""
        self.config = config
        self.storage_path = Path(config.file_storage_path)
        self.storage_path.mkdir(parents=True, exist_ok=True)
        
        self.current_file = None
        self.current_file_size = 0
        self.events_buffer = []
        
        # Setup rotation
        self.max_size_bytes = config.max_file_size_mb * 1024 * 1024
        
    async def store_event(self, event: AuditEvent) -> bool:
        """Store single audit event to file."""
        return await self.store_events([event])
    
    async def store_events(self, events: List[AuditEvent]) -> bool:
        """Store multiple events to file with rotation."""
        try:
            # Prepare file if needed
            if not self.current_file or self._should_rotate():
                await self._rotate_file()
            
            # Write events
            for event in events:
                event_json = json.dumps(event.to_dict(), separators=(',', ':'))
                line = f"{event_json}\n"
                
                self.current_file.write(line)
                self.current_file_size += len(line.encode('utf-8'))
            
            # Flush to disk
            self.current_file.flush()
            os.fsync(self.current_file.fileno())
            
            return True
            
        except Exception as e:
            audit_logger.error(f"Failed to store audit events: {e}")
            return False
    
    def _should_rotate(self) -> bool:
        """Check if file should be rotated."""
        return (
            self.current_file is None or 
            self.current_file_size >= self.max_size_bytes
        )
    
    async def _rotate_file(self):
        """Rotate to new audit file."""
        if self.current_file:
            self.current_file.close()
        
        # Generate new filename
        timestamp = datetime.utcnow().strftime("%Y%m%d_%H%M%S")
        filename = f"audit_{timestamp}.jsonl"
        filepath = self.storage_path / filename
        
        self.current_file = open(filepath, 'w')
        self.current_file_size = 0
        
        # Clean up old files
        await self._cleanup_old_files()
    
    async def _cleanup_old_files(self):
        """Remove old audit files beyond retention limit."""
        audit_files = sorted(
            self.storage_path.glob("audit_*.jsonl"),
            key=lambda p: p.stat().st_mtime,
            reverse=True
        )
        
        # Remove files beyond max count
        for old_file in audit_files[self.config.max_files:]:
            try:
                old_file.unlink()
                audit_logger.info(f"Removed old audit file: {old_file}")
            except Exception as e:
                audit_logger.error(f"Failed to remove old audit file {old_file}: {e}")
    
    async def query_events(
        self,
        start_time: datetime,
        end_time: datetime,
        event_types: List[AuditEventType] = None,
        user_id: str = None,
        limit: int = 1000
    ) -> List[AuditEvent]:
        """Query events from files (basic implementation)."""
        events = []
        
        try:
            # Read from all audit files
            audit_files = sorted(self.storage_path.glob("audit_*.jsonl"))
            
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
                            
                            # Apply filters
                            if event_time < start_time or event_time > end_time:
                                continue
                            
                            if event_types and event_data['event_type'] not in [t.value for t in event_types]:
                                continue
                            
                            if user_id and event_data.get('user_id') != user_id:
                                continue
                            
                            # Convert back to AuditEvent (simplified)
                            events.append(event_data)
                            
                        except json.JSONDecodeError:
                            continue
        
        except Exception as e:
            audit_logger.error(f"Failed to query audit events: {e}")
        
        return events
    
    async def purge_old_events(self, older_than: datetime) -> int:
        """Purge old audit files."""
        purged_count = 0
        
        try:
            audit_files = list(self.storage_path.glob("audit_*.jsonl"))
            
            for file_path in audit_files:
                file_time = datetime.fromtimestamp(file_path.stat().st_mtime)
                
                if file_time < older_than:
                    try:
                        file_path.unlink()
                        purged_count += 1
                        audit_logger.info(f"Purged old audit file: {file_path}")
                    except Exception as e:
                        audit_logger.error(f"Failed to purge audit file {file_path}: {e}")
        
        except Exception as e:
            audit_logger.error(f"Failed to purge old audit events: {e}")
        
        return purged_count


class EnterpriseAuditLogger:
    """
    Enterprise-grade audit logging system with comprehensive event tracking,
    compliance features, and integration with Echo systems.
    """
    
    def __init__(self, config: AuditConfig = None):
        """Initialize enterprise audit logger."""
        self.config = config or AuditConfig()
        self.pii_detector = PIIDetector()
        
        # Initialize storage backend
        if self.config.storage_backend == "file":
            self.storage = FileAuditStorage(self.config)
        else:
            # Future: Add database, elasticsearch backends
            self.storage = FileAuditStorage(self.config)
        
        # Event buffer for batch processing
        self.event_buffer = []
        self.buffer_lock = asyncio.Lock()
        self.last_flush = time.time()
        
        # Start background tasks
        if self.config.async_logging:
            asyncio.create_task(self._flush_events_periodically())
        
        audit_logger.info("Enterprise audit logger initialized")
    
    async def log_event(
        self,
        event_type: AuditEventType,
        message: str = "",
        severity: AuditSeverity = AuditSeverity.MEDIUM,
        **kwargs
    ) -> str:
        """
        Log an audit event with comprehensive context.
        
        Args:
            event_type: Type of audit event
            message: Human-readable event description
            severity: Event severity level
            **kwargs: Additional context fields
            
        Returns:
            Event ID for correlation
        """
        
        # Generate event ID
        event_id = f"audit_{uuid.uuid4().hex[:12]}"
        
        # Create audit event
        event = AuditEvent(
            event_id=event_id,
            event_type=event_type,
            timestamp=datetime.utcnow(),
            severity=severity,
            message=message,
            **{k: v for k, v in kwargs.items() if v is not None}
        )
        
        # PII Detection and sanitization
        if self.config.pii_detection_enabled:
            pii_findings = self.pii_detector.detect_pii(message)
            if pii_findings:
                # Log PII detection event
                pii_event = AuditEvent(
                    event_id=f"pii_{uuid.uuid4().hex[:12]}",
                    event_type=AuditEventType.PII_DETECTED,
                    timestamp=datetime.utcnow(),
                    severity=AuditSeverity.HIGH,
                    message=f"PII detected in audit event {event_id}",
                    details={"pii_types": [f['type'] for f in pii_findings]},
                    security_context={"original_event_id": event_id}
                )
                
                # Store PII event immediately
                await self._store_single_event(pii_event)
                
                # Sanitize original message
                if self.config.hash_sensitive_data:
                    event.message = self.pii_detector.sanitize_pii(message, hash_data=True)
        
        # Store event
        await self._store_event(event)
        
        return event_id
    
    async def log_request_start(
        self,
        request: Request,
        request_id: str,
        trace_id: str = None
    ) -> str:
        """Log API request start event."""
        
        # Extract context from request
        context = self._extract_request_context(request)
        
        return await self.log_event(
            event_type=AuditEventType.API_REQUEST_START,
            message=f"API request started: {request.method} {request.url.path}",
            severity=AuditSeverity.LOW,
            request_id=request_id,
            trace_id=trace_id,
            endpoint=str(request.url.path),
            method=request.method,
            **context
        )
    
    async def log_request_success(
        self,
        request: Request,
        response: Response,
        request_id: str,
        processing_time_ms: float = None,
        trace_id: str = None
    ) -> str:
        """Log successful API request completion."""
        
        context = self._extract_request_context(request)
        
        return await self.log_event(
            event_type=AuditEventType.API_REQUEST_SUCCESS,
            message=f"API request completed: {request.method} {request.url.path} -> {response.status_code}",
            severity=AuditSeverity.LOW,
            request_id=request_id,
            trace_id=trace_id,
            endpoint=str(request.url.path),
            method=request.method,
            processing_time_ms=processing_time_ms,
            details={"status_code": response.status_code},
            **context
        )
    
    async def log_security_event(
        self,
        event_type: AuditEventType,
        message: str,
        client_ip: str,
        severity: AuditSeverity = AuditSeverity.HIGH,
        **kwargs
    ) -> str:
        """Log security-related event."""
        
        return await self.log_event(
            event_type=event_type,
            message=message,
            severity=severity,
            client_ip=client_ip,
            security_context=kwargs.get('security_context', {}),
            **kwargs
        )
    
    async def log_echo_event(
        self,
        echo_system: str,
        operation: str,
        success: bool = True,
        processing_time_ms: float = None,
        **kwargs
    ) -> str:
        """Log Echo system interaction event."""
        
        return await self.log_event(
            event_type=AuditEventType.ECHO_SYSTEM_INTERACTION,
            message=f"Echo.{echo_system} operation: {operation}",
            severity=AuditSeverity.MEDIUM,
            success=success,
            processing_time_ms=processing_time_ms,
            echo_context={
                "system": echo_system,
                "operation": operation,
                **kwargs
            }
        )
    
    def _extract_request_context(self, request: Request) -> Dict[str, Any]:
        """Extract context information from HTTP request."""
        
        # Get client IP with proxy support
        client_ip = request.client.host if request.client else "unknown"
        forwarded_for = request.headers.get('x-forwarded-for')
        if forwarded_for:
            client_ip = forwarded_for.split(',')[0].strip()
        
        return {
            "client_ip": client_ip,
            "user_agent": request.headers.get("user-agent"),
            "user_id": request.headers.get("x-user-id") or 
                      getattr(request.state, 'user_id', None),
            "session_id": request.headers.get("x-session-id") or 
                         request.cookies.get("session_id"),
            "details": {
                "query_params": dict(request.query_params),
                "content_type": request.headers.get("content-type"),
                "content_length": request.headers.get("content-length")
            }
        }
    
    async def _store_event(self, event: AuditEvent):
        """Store event using configured method."""
        
        if self.config.async_logging:
            # Add to buffer for batch processing
            async with self.buffer_lock:
                self.event_buffer.append(event)
                
                # Flush if buffer is full
                if len(self.event_buffer) >= self.config.batch_size:
                    await self._flush_events()
        else:
            # Store immediately
            await self._store_single_event(event)
    
    async def _store_single_event(self, event: AuditEvent):
        """Store single event immediately."""
        try:
            success = await self.storage.store_event(event)
            if not success:
                # Fallback to stderr if storage fails
                import sys
                print(f"AUDIT_FALLBACK: {json.dumps(event.to_dict())}", file=sys.stderr)
                
        except Exception as e:
            audit_logger.error(f"Failed to store audit event: {e}")
    
    async def _flush_events(self):
        """Flush buffered events to storage."""
        async with self.buffer_lock:
            if self.event_buffer:
                events_to_store = self.event_buffer.copy()
                self.event_buffer.clear()
                self.last_flush = time.time()
                
                try:
                    await self.storage.store_events(events_to_store)
                    audit_logger.debug(f"Flushed {len(events_to_store)} audit events")
                except Exception as e:
                    audit_logger.error(f"Failed to flush audit events: {e}")
                    # Re-add to buffer for retry
                    self.event_buffer.extend(events_to_store)
    
    async def _flush_events_periodically(self):
        """Background task to flush events periodically."""
        while True:
            try:
                await asyncio.sleep(self.config.flush_interval_seconds)
                
                # Check if we need to flush
                if (time.time() - self.last_flush) >= self.config.flush_interval_seconds:
                    await self._flush_events()
                    
            except Exception as e:
                audit_logger.error(f"Error in periodic flush task: {e}")
    
    async def query_events(
        self,
        start_time: datetime = None,
        end_time: datetime = None,
        event_types: List[AuditEventType] = None,
        user_id: str = None,
        limit: int = 1000
    ) -> List[Dict[str, Any]]:
        """Query audit events with filters."""
        
        if start_time is None:
            start_time = datetime.utcnow() - timedelta(days=1)
        if end_time is None:
            end_time = datetime.utcnow()
        
        return await self.storage.query_events(
            start_time=start_time,
            end_time=end_time,
            event_types=event_types,
            user_id=user_id,
            limit=limit
        )
    
    async def generate_compliance_report(
        self,
        start_date: datetime,
        end_date: datetime,
        report_type: str = "standard"
    ) -> Dict[str, Any]:
        """Generate compliance report for specified period."""
        
        # Query all events in period
        events = await self.query_events(
            start_time=start_date,
            end_time=end_date,
            limit=10000
        )
        
        # Aggregate by event type
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
        
        return {
            "report_type": report_type,
            "period": {
                "start": start_date.isoformat(),
                "end": end_date.isoformat()
            },
            "summary": {
                "total_events": len(events),
                "event_type_counts": event_counts,
                "security_events_count": len(security_events),
                "data_privacy_events_count": len(data_privacy_events)
            },
            "security_summary": {
                "total_security_events": len(security_events),
                "high_severity_events": len([e for e in security_events if e.get('severity') == 'high']),
                "critical_events": len([e for e in security_events if e.get('severity') == 'critical'])
            },
            "data_privacy_summary": {
                "total_data_events": len(data_privacy_events),
                "pii_detections": len([e for e in data_privacy_events if 'pii' in e.get('event_type', '')])
            }
        }
    
    async def cleanup_old_events(self, retention_days: int = None) -> int:
        """Clean up audit events older than retention period."""
        
        if retention_days is None:
            retention_days = self.config.retention_days
        
        cutoff_date = datetime.utcnow() - timedelta(days=retention_days)
        
        try:
            purged_count = await self.storage.purge_old_events(cutoff_date)
            
            await self.log_event(
                event_type=AuditEventType.DATA_RETENTION_POLICY_APPLIED,
                message=f"Purged {purged_count} audit events older than {retention_days} days",
                severity=AuditSeverity.MEDIUM,
                details={"purged_count": purged_count, "retention_days": retention_days}
            )
            
            return purged_count
            
        except Exception as e:
            audit_logger.error(f"Failed to cleanup old audit events: {e}")
            return 0


# Global audit logger instance
_global_audit_logger: Optional[EnterpriseAuditLogger] = None


def get_audit_logger(config: AuditConfig = None) -> EnterpriseAuditLogger:
    """Get global audit logger instance."""
    global _global_audit_logger
    
    if _global_audit_logger is None:
        _global_audit_logger = EnterpriseAuditLogger(config)
    
    return _global_audit_logger


def configure_audit_logging(config: AuditConfig):
    """Configure global audit logging."""
    global _global_audit_logger
    _global_audit_logger = EnterpriseAuditLogger(config)


async def audit_log(
    event_type: AuditEventType,
    message: str = "",
    severity: AuditSeverity = AuditSeverity.MEDIUM,
    **kwargs
) -> str:
    """Convenience function for audit logging."""
    logger = get_audit_logger()
    return await logger.log_event(event_type, message, severity, **kwargs)