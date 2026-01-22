import time
import uuid
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from enum import Enum
from dataclasses import dataclass
from collections import defaultdict, deque
import statistics
from fastapi import Request
import structlog
from .audit_logger import AuditEventType, AuditSeverity, get_audit_logger
incident_logger = structlog.get_logger('incident_response')
class ThreatType(str, Enum):
    BRUTE_FORCE_ATTACK = 'brute_force_attack'
    CREDENTIAL_STUFFING = 'credential_stuffing'
    ACCOUNT_TAKEOVER = 'account_takeover'
    DOS_ATTACK = 'dos_attack'
    DDOS_ATTACK = 'ddos_attack'
    RATE_LIMIT_ABUSE = 'rate_limit_abuse'
    SQL_INJECTION = 'sql_injection'
    XSS_ATTACK = 'xss_attack'
    COMMAND_INJECTION = 'command_injection'
    PATH_TRAVERSAL = 'path_traversal'
    API_ABUSE = 'api_abuse'
    UNAUTHORIZED_ACCESS = 'unauthorized_access'
    DATA_EXFILTRATION = 'data_exfiltration'
    ANOMALOUS_BEHAVIOR = 'anomalous_behavior'
    SUSPICIOUS_PATTERNS = 'suspicious_patterns'
    RECONNAISSANCE = 'reconnaissance'
    MALWARE_DETECTION = 'malware_detection'
    PRIVILEGE_ESCALATION = 'privilege_escalation'
    INSIDER_THREAT = 'insider_threat'
    MODEL_POISONING = 'model_poisoning'
    ADVERSARIAL_INPUT = 'adversarial_input'
    PROMPT_INJECTION = 'prompt_injection'
    DATA_POISONING = 'data_poisoning'
class IncidentSeverity(str, Enum):
    LOW = 'low'
    MEDIUM = 'medium'
    HIGH = 'high'
    CRITICAL = 'critical'
class IncidentStatus(str, Enum):
    DETECTED = 'detected'
    INVESTIGATING = 'investigating'
    CONFIRMED = 'confirmed'
    MITIGATING = 'mitigating'
    RESOLVED = 'resolved'
    CLOSED = 'closed'
class ResponseAction(str, Enum):
    LOG_EVENT = 'log_event'
    INCREASE_MONITORING = 'increase_monitoring'
    ALERT_SECURITY_TEAM = 'alert_security_team'
    BLOCK_IP = 'block_ip'
    BLOCK_USER = 'block_user'
    RATE_LIMIT = 'rate_limit'
    ISOLATE_COMPONENT = 'isolate_component'
    RESTART_SERVICE = 'restart_service'
    SWITCH_TO_BACKUP = 'switch_to_backup'
    ENCRYPT_SENSITIVE_DATA = 'encrypt_sensitive_data'
    BACKUP_DATA = 'backup_data'
    QUARANTINE_DATA = 'quarantine_data'
    FIREWALL_RULE_UPDATE = 'firewall_rule_update'
    NETWORK_SEGMENTATION = 'network_segmentation'
    TRAFFIC_REDIRECTION = 'traffic_redirection'
@dataclass
class SecurityEvent:
    event_id: str
    timestamp: datetime
    source_ip: str
    user_agent: Optional[str]
    endpoint: Optional[str]
    method: Optional[str]
    event_type: str
    description: str
    raw_data: Dict[str, Any]
    user_id: Optional[str] = None
    session_id: Optional[str] = None
    request_id: Optional[str] = None
    anomaly_score: float = 0.0
    threat_indicators: List[str] = None
    confidence: float = 0.0
    def __post_init__(self):
        if self.threat_indicators is None:
            self.threat_indicators = []
@dataclass
class SecurityIncident:
    incident_id: str
    detected_at: datetime
    threat_type: ThreatType
    severity: IncidentSeverity
    status: IncidentStatus
    title: str
    description: str
    affected_assets: List[str]
    events: List[SecurityEvent]
    indicators_of_compromise: List[str]
    estimated_impact: str
    affected_users: int = 0
    data_at_risk: bool = False
    response_actions: List[ResponseAction] = None
    assigned_to: Optional[str] = None
    resolution_notes: Optional[str] = None
    last_updated: datetime = None
    resolved_at: Optional[datetime] = None
    def __post_init__(self):
        if self.response_actions is None:
            self.response_actions = []
        if self.last_updated is None:
            self.last_updated = self.detected_at
class ThreatDetector:
    def __init__(self, name: str):
        self.name = name
        self.enabled = True
        self.detection_count = 0
        self.false_positive_rate = 0.0
    async def analyze_event(self, event: SecurityEvent) -> Optional[ThreatType]:
        raise NotImplementedError
    def get_confidence(self, event: SecurityEvent) -> float:
        return 0.5
class BruteForceDetector(ThreatDetector):
    def __init__(self):
        super().__init__('brute_force_detector')
        self.failed_attempts = defaultdict(list)
        self.threshold = 10
        self.time_window = 300
    async def analyze_event(self, event: SecurityEvent) -> Optional[ThreatType]:
        if 'login' not in event.event_type and 'auth' not in event.event_type:
            return None
        if 'fail' in event.description.lower() or 'error' in event.description.lower():
            now = time.time()
            self.failed_attempts[event.source_ip].append(now)
            cutoff = now - self.time_window
            self.failed_attempts[event.source_ip] = [timestamp for timestamp in self.failed_attempts[event.source_ip] if timestamp > cutoff]
            if len(self.failed_attempts[event.source_ip]) >= self.threshold:
                return ThreatType.BRUTE_FORCE_ATTACK
        return None
    def get_confidence(self, event: SecurityEvent) -> float:
        attempts = len(self.failed_attempts.get(event.source_ip, []))
        return min(attempts / self.threshold, 1.0)
class RateLimitAbuseDetector(ThreatDetector):
    def __init__(self):
        super().__init__('rate_limit_abuse_detector')
        self.request_counts = defaultdict(list)
        self.threshold = 100
        self.time_window = 60
    async def analyze_event(self, event: SecurityEvent) -> Optional[ThreatType]:
        now = time.time()
        self.request_counts[event.source_ip].append(now)
        cutoff = now - self.time_window
        self.request_counts[event.source_ip] = [timestamp for timestamp in self.request_counts[event.source_ip] if timestamp > cutoff]
        request_count = len(self.request_counts[event.source_ip])
        if request_count >= self.threshold:
            return ThreatType.RATE_LIMIT_ABUSE
        return None
    def get_confidence(self, event: SecurityEvent) -> float:
        count = len(self.request_counts.get(event.source_ip, []))
        return min(count / self.threshold, 1.0)
class AnomalyDetector(ThreatDetector):
    def __init__(self):
        super().__init__('anomaly_detector')
        self.baseline_metrics = {}
        self.current_metrics = {}
        self.lookback_window = 3600
    async def analyze_event(self, event: SecurityEvent) -> Optional[ThreatType]:
        metrics = self._extract_metrics(event)
        now = time.time()
        for metric, value in metrics.items():
            if metric not in self.current_metrics:
                self.current_metrics[metric] = []
            self.current_metrics[metric].append((now, value))
            cutoff = now - self.lookback_window
            self.current_metrics[metric] = [(timestamp, val) for timestamp, val in self.current_metrics[metric] if timestamp > cutoff]
        for metric, values in self.current_metrics.items():
            if len(values) >= 10:
                recent_values = [val for _, val in values]
                if self._is_anomalous(metric, recent_values):
                    return ThreatType.ANOMALOUS_BEHAVIOR
        return None
    def _extract_metrics(self, event: SecurityEvent) -> Dict[str, float]:
        metrics = {}
        if 'processing_time' in event.raw_data:
            metrics['processing_time'] = float(event.raw_data['processing_time'])
        if 'content_length' in event.raw_data:
            try:
                metrics['request_size'] = float(event.raw_data['content_length'])
            except (ValueError, TypeError):
                pass
        if event.endpoint:
            metrics['path_length'] = len(event.endpoint)
        if event.user_agent:
            metrics['user_agent_length'] = len(event.user_agent)
        return metrics
    def _is_anomalous(self, metric: str, values: List[float]) -> bool:
        if len(values) < 5:
            return False
        mean_val = statistics.mean(values[:-3])
        std_val = statistics.stdev(values[:-3]) if len(values) > 5 else 1.0
        recent = values[-3:]
        for value in recent:
            if std_val > 0:
                z_score = abs(value - mean_val) / std_val
                if z_score > 3.0:
                    return True
        return False
class PromptInjectionDetector(ThreatDetector):
    def __init__(self):
        super().__init__('prompt_injection_detector')
        self.injection_patterns = ['ignore previous instructions', 'disregard the above', 'forget everything above', 'new instructions:', 'system:', 'override instructions', 'jailbreak', 'act as', 'pretend to be', 'roleplay as', '\\n\\nHuman:', '\\n\\nAssistant:']
        self.encoding_patterns = ['\\x', '\\u', '%', '&#', '&lt;', '&gt;', 'base64', 'hex', 'rot13', 'caesar']
    async def analyze_event(self, event: SecurityEvent) -> Optional[ThreatType]:
        text_content = ''
        if 'prompt' in event.raw_data:
            text_content = str(event.raw_data['prompt']).lower()
        elif 'message' in event.raw_data:
            text_content = str(event.raw_data['message']).lower()
        elif 'input' in event.raw_data:
            text_content = str(event.raw_data['input']).lower()
        if not text_content:
            return None
        injection_score = 0
        detected_patterns = []
        for pattern in self.injection_patterns:
            if pattern in text_content:
                injection_score += 2
                detected_patterns.append(pattern)
        for pattern in self.encoding_patterns:
            if pattern in text_content:
                injection_score += 1
                detected_patterns.append(f'encoding:{pattern}')
        if len(text_content) > 10000:
            injection_score += 1
            detected_patterns.append('excessive_length')
        if any((char * 100 in text_content for char in 'aA123!@#')):
            injection_score += 2
            detected_patterns.append('repeated_characters')
        event.threat_indicators.extend(detected_patterns)
        event.anomaly_score = max(event.anomaly_score, injection_score / 5.0)
        if injection_score >= 3:
            return ThreatType.PROMPT_INJECTION
        return None
    def get_confidence(self, event: SecurityEvent) -> float:
        return min(event.anomaly_score, 1.0)
class IncidentResponseEngine:
    def __init__(self):
        self.audit_logger = get_audit_logger()
        self.detectors = [BruteForceDetector(), RateLimitAbuseDetector(), AnomalyDetector(), PromptInjectionDetector()]
        self.active_incidents = {}
        self.event_buffer = deque(maxlen=10000)
        self.incident_counter = 0
        self.auto_response_enabled = True
        self.response_thresholds = {IncidentSeverity.LOW: 0.3, IncidentSeverity.MEDIUM: 0.6, IncidentSeverity.HIGH: 0.8, IncidentSeverity.CRITICAL: 0.9}
        self.response_handlers = {ResponseAction.LOG_EVENT: self._log_event_action, ResponseAction.BLOCK_IP: self._block_ip_action, ResponseAction.RATE_LIMIT: self._rate_limit_action, ResponseAction.ALERT_SECURITY_TEAM: self._alert_security_team_action, ResponseAction.INCREASE_MONITORING: self._increase_monitoring_action}
        self.echo_integration_enabled = True
        incident_logger.info('Incident response engine initialized')
    async def process_security_event(self, request: Request=None, event_type: str='api_request', description: str='', raw_data: Dict[str, Any]=None, **kwargs) -> Optional[str]:
        event = SecurityEvent(event_id=f'evt_{uuid.uuid4().hex[:12]}', timestamp=datetime.utcnow(), source_ip=self._get_client_ip(request) if request else 'unknown', user_agent=request.headers.get('user-agent') if request else None, endpoint=str(request.url.path) if request else None, method=request.method if request else None, event_type=event_type, description=description, raw_data=raw_data or {}, **kwargs)
        self.event_buffer.append(event)
        detected_threats = []
        max_confidence = 0.0
        for detector in self.detectors:
            if not detector.enabled:
                continue
            try:
                threat_type = await detector.analyze_event(event)
                if threat_type:
                    confidence = detector.get_confidence(event)
                    detected_threats.append((threat_type, confidence, detector.name))
                    max_confidence = max(max_confidence, confidence)
            except Exception as e:
                incident_logger.error(f'Error in detector {detector.name}: {e}')
        incident_id = None
        if detected_threats and max_confidence > 0.3:
            incident_id = await self._create_incident(event, detected_threats, max_confidence)
        if self.echo_integration_enabled and detected_threats:
            await self._notify_echo_systems(event, detected_threats)
        return incident_id
    async def _create_incident(self, trigger_event: SecurityEvent, detected_threats: List[tuple], confidence: float) -> str:
        self.incident_counter += 1
        incident_id = f"inc_{datetime.utcnow().strftime('%Y%m%d')}_{self.incident_counter:04d}"
        primary_threat = detected_threats[0][0]
        severity = self._calculate_severity(primary_threat, confidence)
        related_events = self._find_related_events(trigger_event)
        incident = SecurityIncident(incident_id=incident_id, detected_at=datetime.utcnow(), threat_type=primary_threat, severity=severity, status=IncidentStatus.DETECTED, title=f"{primary_threat.value.replace('_', ' ').title()} - {trigger_event.source_ip}", description=f'Detected {primary_threat.value} with {confidence:.1%} confidence', affected_assets=[trigger_event.endpoint or 'unknown'], events=[trigger_event] + related_events, indicators_of_compromise=[f'Source IP: {trigger_event.source_ip}', f"User Agent: {trigger_event.user_agent or 'Unknown'}", f"Detection: {', '.join([t[2] for t in detected_threats])}"], estimated_impact=self._estimate_impact(primary_threat, len(related_events)))
        self.active_incidents[incident_id] = incident
        await self.audit_logger.log_event(event_type=AuditEventType.SECURITY_ANOMALY_DETECTED, message=f'Security incident created: {incident.title}', severity=AuditSeverity.HIGH if severity in [IncidentSeverity.HIGH, IncidentSeverity.CRITICAL] else AuditSeverity.MEDIUM, client_ip=trigger_event.source_ip, details={'incident_id': incident_id, 'threat_type': primary_threat.value, 'confidence': confidence, 'affected_assets': incident.affected_assets})
        if self.auto_response_enabled:
            await self._execute_automated_response(incident)
        incident_logger.warning(f'Security incident created: {incident_id}', threat_type=primary_threat.value, severity=severity.value, confidence=confidence, source_ip=trigger_event.source_ip)
        return incident_id
    def _calculate_severity(self, threat_type: ThreatType, confidence: float) -> IncidentSeverity:
        threat_severity = {ThreatType.BRUTE_FORCE_ATTACK: IncidentSeverity.MEDIUM, ThreatType.DOS_ATTACK: IncidentSeverity.HIGH, ThreatType.DDOS_ATTACK: IncidentSeverity.CRITICAL, ThreatType.SQL_INJECTION: IncidentSeverity.HIGH, ThreatType.XSS_ATTACK: IncidentSeverity.MEDIUM, ThreatType.UNAUTHORIZED_ACCESS: IncidentSeverity.HIGH, ThreatType.DATA_EXFILTRATION: IncidentSeverity.CRITICAL, ThreatType.PROMPT_INJECTION: IncidentSeverity.MEDIUM, ThreatType.MODEL_POISONING: IncidentSeverity.HIGH, ThreatType.ANOMALOUS_BEHAVIOR: IncidentSeverity.LOW}
        base_severity = threat_severity.get(threat_type, IncidentSeverity.MEDIUM)
        if confidence >= 0.9:
            if base_severity == IncidentSeverity.LOW:
                return IncidentSeverity.MEDIUM
            elif base_severity == IncidentSeverity.MEDIUM:
                return IncidentSeverity.HIGH
            elif base_severity == IncidentSeverity.HIGH:
                return IncidentSeverity.CRITICAL
        elif confidence < 0.5:
            if base_severity == IncidentSeverity.CRITICAL:
                return IncidentSeverity.HIGH
            elif base_severity == IncidentSeverity.HIGH:
                return IncidentSeverity.MEDIUM
            elif base_severity == IncidentSeverity.MEDIUM:
                return IncidentSeverity.LOW
        return base_severity
    def _find_related_events(self, trigger_event: SecurityEvent) -> List[SecurityEvent]:
        related_events = []
        cutoff_time = trigger_event.timestamp - timedelta(minutes=10)
        for event in reversed(self.event_buffer):
            if event.event_id == trigger_event.event_id:
                continue
            if event.timestamp < cutoff_time:
                break
            if event.source_ip == trigger_event.source_ip:
                related_events.append(event)
            elif event.user_id and event.user_id == trigger_event.user_id or (event.session_id and event.session_id == trigger_event.session_id):
                related_events.append(event)
            if len(related_events) >= 20:
                break
        return related_events
    def _estimate_impact(self, threat_type: ThreatType, related_events_count: int) -> str:
        impact_levels = {'minimal': 'Single user/session affected, no data at risk', 'limited': 'Multiple users affected, limited data exposure risk', 'significant': 'System performance impacted, potential data exposure', 'severe': 'Service disruption, confirmed or high risk of data breach'}
        if threat_type in [ThreatType.DATA_EXFILTRATION, ThreatType.UNAUTHORIZED_ACCESS]:
            base_impact = 'severe'
        elif threat_type in [ThreatType.DOS_ATTACK, ThreatType.DDOS_ATTACK, ThreatType.SQL_INJECTION]:
            base_impact = 'significant'
        elif threat_type in [ThreatType.BRUTE_FORCE_ATTACK, ThreatType.RATE_LIMIT_ABUSE]:
            base_impact = 'limited'
        else:
            base_impact = 'minimal'
        if related_events_count > 50:
            if base_impact in ['minimal', 'limited']:
                base_impact = 'significant'
        elif related_events_count > 10:
            if base_impact == 'minimal':
                base_impact = 'limited'
        return impact_levels[base_impact]
    async def _execute_automated_response(self, incident: SecurityIncident):
        response_actions = self._get_response_actions(incident)
        for action in response_actions:
            try:
                if action in self.response_handlers:
                    await self.response_handlers[action](incident)
                    incident.response_actions.append(action)
                    await self.audit_logger.log_event(event_type=AuditEventType.SECURITY_POLICY_VIOLATION, message=f'Automated response executed: {action.value}', severity=AuditSeverity.HIGH, details={'incident_id': incident.incident_id, 'action': action.value, 'threat_type': incident.threat_type.value})
            except Exception as e:
                incident_logger.error(f'Failed to execute response action {action}: {e}')
        incident.status = IncidentStatus.MITIGATING
        incident.last_updated = datetime.utcnow()
    def _get_response_actions(self, incident: SecurityIncident) -> List[ResponseAction]:
        actions = [ResponseAction.LOG_EVENT]
        if incident.severity in [IncidentSeverity.MEDIUM, IncidentSeverity.HIGH, IncidentSeverity.CRITICAL]:
            actions.append(ResponseAction.ALERT_SECURITY_TEAM)
        if incident.severity in [IncidentSeverity.HIGH, IncidentSeverity.CRITICAL]:
            actions.append(ResponseAction.INCREASE_MONITORING)
        if incident.threat_type in [ThreatType.BRUTE_FORCE_ATTACK, ThreatType.RATE_LIMIT_ABUSE, ThreatType.DOS_ATTACK]:
            actions.append(ResponseAction.RATE_LIMIT)
        if incident.threat_type in [ThreatType.DOS_ATTACK, ThreatType.DDOS_ATTACK, ThreatType.UNAUTHORIZED_ACCESS]:
            actions.append(ResponseAction.BLOCK_IP)
        return actions
    async def _log_event_action(self, incident: SecurityIncident):
        incident_logger.warning('Security incident response: LOG_EVENT', incident_id=incident.incident_id, threat_type=incident.threat_type.value)
    async def _block_ip_action(self, incident: SecurityIncident):
        source_ips = list(set((event.source_ip for event in incident.events)))
        for ip in source_ips:
            incident_logger.warning(f'BLOCKING IP: {ip} due to incident {incident.incident_id}')
            await self.audit_logger.log_event(event_type=AuditEventType.SECURITY_IP_BLOCKED, message=f'IP address blocked: {ip}', severity=AuditSeverity.HIGH, client_ip=ip, details={'incident_id': incident.incident_id, 'reason': incident.threat_type.value})
    async def _rate_limit_action(self, incident: SecurityIncident):
        source_ips = list(set((event.source_ip for event in incident.events)))
        for ip in source_ips:
            incident_logger.warning(f'RATE LIMITING IP: {ip} due to incident {incident.incident_id}')
    async def _alert_security_team_action(self, incident: SecurityIncident):
        incident_logger.critical(f'SECURITY ALERT: {incident.title}', incident_id=incident.incident_id, severity=incident.severity.value, threat_type=incident.threat_type.value, affected_assets=incident.affected_assets)
    async def _increase_monitoring_action(self, incident: SecurityIncident):
        incident_logger.info(f'INCREASING MONITORING for incident {incident.incident_id}')
    async def _notify_echo_systems(self, event: SecurityEvent, detected_threats: List[tuple]):
        echo_notification = {'event_id': event.event_id, 'timestamp': event.timestamp.isoformat(), 'threat_types': [threat[0].value for threat in detected_threats], 'confidence_scores': [threat[1] for threat in detected_threats], 'source_ip': event.source_ip, 'affected_endpoint': event.endpoint}
        await self.audit_logger.log_echo_event(echo_system='security', operation='threat_notification', success=True, threat_types=echo_notification['threat_types'], confidence_scores=echo_notification['confidence_scores'])
        incident_logger.info('Notified Echo systems of security threat', echo_notification=echo_notification)
    def _get_client_ip(self, request: Request) -> str:
        forwarded_for = request.headers.get('x-forwarded-for')
        if forwarded_for:
            return forwarded_for.split(',')[0].strip()
        real_ip = request.headers.get('x-real-ip')
        if real_ip:
            return real_ip
        return request.client.host if request.client else 'unknown'
    async def get_incident_status(self, incident_id: str) -> Optional[Dict[str, Any]]:
        if incident_id not in self.active_incidents:
            return None
        incident = self.active_incidents[incident_id]
        return {'incident_id': incident.incident_id, 'status': incident.status.value, 'severity': incident.severity.value, 'threat_type': incident.threat_type.value, 'detected_at': incident.detected_at.isoformat(), 'last_updated': incident.last_updated.isoformat(), 'events_count': len(incident.events), 'response_actions': [action.value for action in incident.response_actions], 'estimated_impact': incident.estimated_impact}
    async def update_incident_status(self, incident_id: str, new_status: IncidentStatus, resolution_notes: str=None) -> bool:
        if incident_id not in self.active_incidents:
            return False
        incident = self.active_incidents[incident_id]
        old_status = incident.status
        incident.status = new_status
        incident.last_updated = datetime.utcnow()
        if resolution_notes:
            incident.resolution_notes = resolution_notes
        if new_status == IncidentStatus.RESOLVED:
            incident.resolved_at = datetime.utcnow()
        await self.audit_logger.log_event(event_type=AuditEventType.SYSTEM_CONFIG_CHANGE, message=f'Incident status updated: {old_status.value} -> {new_status.value}', severity=AuditSeverity.MEDIUM, details={'incident_id': incident_id, 'old_status': old_status.value, 'new_status': new_status.value, 'resolution_notes': resolution_notes})
        return True
    async def get_security_dashboard(self) -> Dict[str, Any]:
        now = datetime.utcnow()
        last_24h = now - timedelta(hours=24)
        status_counts = defaultdict(int)
        severity_counts = defaultdict(int)
        threat_type_counts = defaultdict(int)
        recent_incidents = []
        for incident in self.active_incidents.values():
            status_counts[incident.status.value] += 1
            severity_counts[incident.severity.value] += 1
            threat_type_counts[incident.threat_type.value] += 1
            if incident.detected_at >= last_24h:
                recent_incidents.append({'incident_id': incident.incident_id, 'title': incident.title, 'severity': incident.severity.value, 'status': incident.status.value, 'detected_at': incident.detected_at.isoformat()})
        detector_stats = {}
        for detector in self.detectors:
            detector_stats[detector.name] = {'enabled': detector.enabled, 'detection_count': detector.detection_count, 'false_positive_rate': detector.false_positive_rate}
        return {'dashboard_generated_at': now.isoformat(), 'summary': {'total_active_incidents': len(self.active_incidents), 'incidents_last_24h': len(recent_incidents), 'critical_incidents': severity_counts.get('critical', 0), 'auto_response_enabled': self.auto_response_enabled}, 'incident_statistics': {'by_status': dict(status_counts), 'by_severity': dict(severity_counts), 'by_threat_type': dict(threat_type_counts)}, 'recent_incidents': recent_incidents[-10:], 'detector_performance': detector_stats, 'system_status': {'echo_integration': self.echo_integration_enabled, 'total_detectors': len(self.detectors), 'enabled_detectors': sum((1 for d in self.detectors if d.enabled))}}
_global_incident_engine: Optional[IncidentResponseEngine] = None
def get_incident_engine() -> IncidentResponseEngine:
    global _global_incident_engine
    if _global_incident_engine is None:
        _global_incident_engine = IncidentResponseEngine()
    return _global_incident_engine
async def process_security_event(**kwargs) -> Optional[str]:
    engine = get_incident_engine()
    return await engine.process_security_event(**kwargs)