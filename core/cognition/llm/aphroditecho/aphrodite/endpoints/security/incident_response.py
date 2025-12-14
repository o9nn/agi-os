"""
Security incident detection and automated response system for Aphrodite Engine.

Implements real-time threat detection, automated incident response,
and integration with Echo systems for enhanced security awareness.
"""

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

from .audit_logger import (
    AuditEventType, 
    AuditSeverity, 
    get_audit_logger
)

# Configure incident response logger
incident_logger = structlog.get_logger("incident_response")


class ThreatType(str, Enum):
    """Types of security threats detected."""
    
    # Authentication Threats
    BRUTE_FORCE_ATTACK = "brute_force_attack"
    CREDENTIAL_STUFFING = "credential_stuffing"
    ACCOUNT_TAKEOVER = "account_takeover"
    
    # Network Threats
    DOS_ATTACK = "dos_attack"
    DDOS_ATTACK = "ddos_attack"
    RATE_LIMIT_ABUSE = "rate_limit_abuse"
    
    # Application Threats
    SQL_INJECTION = "sql_injection"
    XSS_ATTACK = "xss_attack"
    COMMAND_INJECTION = "command_injection"
    PATH_TRAVERSAL = "path_traversal"
    
    # API Threats
    API_ABUSE = "api_abuse"
    UNAUTHORIZED_ACCESS = "unauthorized_access"
    DATA_EXFILTRATION = "data_exfiltration"
    
    # Behavioral Threats
    ANOMALOUS_BEHAVIOR = "anomalous_behavior"
    SUSPICIOUS_PATTERNS = "suspicious_patterns"
    RECONNAISSANCE = "reconnaissance"
    
    # System Threats
    MALWARE_DETECTION = "malware_detection"
    PRIVILEGE_ESCALATION = "privilege_escalation"
    INSIDER_THREAT = "insider_threat"
    
    # AI/ML Specific Threats
    MODEL_POISONING = "model_poisoning"
    ADVERSARIAL_INPUT = "adversarial_input"
    PROMPT_INJECTION = "prompt_injection"
    DATA_POISONING = "data_poisoning"


class IncidentSeverity(str, Enum):
    """Incident severity levels for response prioritization."""
    
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


class IncidentStatus(str, Enum):
    """Incident lifecycle status."""
    
    DETECTED = "detected"
    INVESTIGATING = "investigating"
    CONFIRMED = "confirmed"
    MITIGATING = "mitigating"
    RESOLVED = "resolved"
    CLOSED = "closed"


class ResponseAction(str, Enum):
    """Automated response actions."""
    
    # Monitoring Actions
    LOG_EVENT = "log_event"
    INCREASE_MONITORING = "increase_monitoring"
    ALERT_SECURITY_TEAM = "alert_security_team"
    
    # Blocking Actions
    BLOCK_IP = "block_ip"
    BLOCK_USER = "block_user"
    RATE_LIMIT = "rate_limit"
    
    # System Actions
    ISOLATE_COMPONENT = "isolate_component"
    RESTART_SERVICE = "restart_service"
    SWITCH_TO_BACKUP = "switch_to_backup"
    
    # Data Protection
    ENCRYPT_SENSITIVE_DATA = "encrypt_sensitive_data"
    BACKUP_DATA = "backup_data"
    QUARANTINE_DATA = "quarantine_data"
    
    # Network Actions
    FIREWALL_RULE_UPDATE = "firewall_rule_update"
    NETWORK_SEGMENTATION = "network_segmentation"
    TRAFFIC_REDIRECTION = "traffic_redirection"


@dataclass
class SecurityEvent:
    """Individual security event for analysis."""
    
    event_id: str
    timestamp: datetime
    source_ip: str
    user_agent: Optional[str]
    endpoint: Optional[str]
    method: Optional[str]
    
    # Event Details
    event_type: str
    description: str
    raw_data: Dict[str, Any]
    
    # Context
    user_id: Optional[str] = None
    session_id: Optional[str] = None
    request_id: Optional[str] = None
    
    # Analysis Results
    anomaly_score: float = 0.0
    threat_indicators: List[str] = None
    confidence: float = 0.0
    
    def __post_init__(self):
        """Initialize default values."""
        if self.threat_indicators is None:
            self.threat_indicators = []


@dataclass
class SecurityIncident:
    """Security incident aggregating multiple related events."""
    
    incident_id: str
    detected_at: datetime
    threat_type: ThreatType
    severity: IncidentSeverity
    status: IncidentStatus
    
    # Incident Details
    title: str
    description: str
    affected_assets: List[str]
    
    # Events and Evidence
    events: List[SecurityEvent]
    indicators_of_compromise: List[str]
    
    # Impact Assessment
    estimated_impact: str
    affected_users: int = 0
    data_at_risk: bool = False
    
    # Response Information
    response_actions: List[ResponseAction] = None
    assigned_to: Optional[str] = None
    resolution_notes: Optional[str] = None
    
    # Timeline
    last_updated: datetime = None
    resolved_at: Optional[datetime] = None
    
    def __post_init__(self):
        """Initialize default values."""
        if self.response_actions is None:
            self.response_actions = []
        if self.last_updated is None:
            self.last_updated = self.detected_at


class ThreatDetector:
    """Base class for threat detection algorithms."""
    
    def __init__(self, name: str):
        """Initialize threat detector."""
        self.name = name
        self.enabled = True
        self.detection_count = 0
        self.false_positive_rate = 0.0
    
    async def analyze_event(self, event: SecurityEvent) -> Optional[ThreatType]:
        """Analyze security event and return threat type if detected."""
        raise NotImplementedError
    
    def get_confidence(self, event: SecurityEvent) -> float:
        """Get confidence score for threat detection."""
        return 0.5  # Base implementation


class BruteForceDetector(ThreatDetector):
    """Detect brute force authentication attacks."""
    
    def __init__(self):
        """Initialize brute force detector."""
        super().__init__("brute_force_detector")
        self.failed_attempts = defaultdict(list)  # ip -> [timestamps]
        self.threshold = 10  # attempts
        self.time_window = 300  # 5 minutes
    
    async def analyze_event(self, event: SecurityEvent) -> Optional[ThreatType]:
        """Detect brute force patterns."""
        
        # Only analyze authentication-related events
        if "login" not in event.event_type and "auth" not in event.event_type:
            return None
        
        # Check for failed authentication
        if "fail" in event.description.lower() or "error" in event.description.lower():
            now = time.time()
            
            # Add to failed attempts
            self.failed_attempts[event.source_ip].append(now)
            
            # Clean old attempts
            cutoff = now - self.time_window
            self.failed_attempts[event.source_ip] = [
                timestamp for timestamp in self.failed_attempts[event.source_ip]
                if timestamp > cutoff
            ]
            
            # Check if threshold exceeded
            if len(self.failed_attempts[event.source_ip]) >= self.threshold:
                return ThreatType.BRUTE_FORCE_ATTACK
        
        return None
    
    def get_confidence(self, event: SecurityEvent) -> float:
        """Calculate confidence based on attempt frequency."""
        attempts = len(self.failed_attempts.get(event.source_ip, []))
        return min(attempts / self.threshold, 1.0)


class RateLimitAbuseDetector(ThreatDetector):
    """Detect rate limit abuse and DoS attempts."""
    
    def __init__(self):
        """Initialize rate limit abuse detector."""
        super().__init__("rate_limit_abuse_detector")
        self.request_counts = defaultdict(list)  # ip -> [timestamps]
        self.threshold = 100  # requests per minute
        self.time_window = 60  # 1 minute
    
    async def analyze_event(self, event: SecurityEvent) -> Optional[ThreatType]:
        """Detect excessive request patterns."""
        
        now = time.time()
        
        # Track all requests from this IP
        self.request_counts[event.source_ip].append(now)
        
        # Clean old requests
        cutoff = now - self.time_window
        self.request_counts[event.source_ip] = [
            timestamp for timestamp in self.request_counts[event.source_ip]
            if timestamp > cutoff
        ]
        
        # Check rate
        request_count = len(self.request_counts[event.source_ip])
        if request_count >= self.threshold:
            return ThreatType.RATE_LIMIT_ABUSE
        
        return None
    
    def get_confidence(self, event: SecurityEvent) -> float:
        """Calculate confidence based on request rate."""
        count = len(self.request_counts.get(event.source_ip, []))
        return min(count / self.threshold, 1.0)


class AnomalyDetector(ThreatDetector):
    """Detect anomalous behavior patterns using statistical analysis."""
    
    def __init__(self):
        """Initialize anomaly detector."""
        super().__init__("anomaly_detector")
        self.baseline_metrics = {}  # metric -> historical_values
        self.current_metrics = {}
        self.lookback_window = 3600  # 1 hour
        
    async def analyze_event(self, event: SecurityEvent) -> Optional[ThreatType]:
        """Detect statistical anomalies."""
        
        # Calculate metrics from event
        metrics = self._extract_metrics(event)
        
        # Update current metrics
        now = time.time()
        for metric, value in metrics.items():
            if metric not in self.current_metrics:
                self.current_metrics[metric] = []
            self.current_metrics[metric].append((now, value))
            
            # Maintain window
            cutoff = now - self.lookback_window
            self.current_metrics[metric] = [
                (timestamp, val) for timestamp, val in self.current_metrics[metric]
                if timestamp > cutoff
            ]
        
        # Check for anomalies
        for metric, values in self.current_metrics.items():
            if len(values) >= 10:  # Need sufficient samples
                recent_values = [val for _, val in values]
                
                if self._is_anomalous(metric, recent_values):
                    return ThreatType.ANOMALOUS_BEHAVIOR
        
        return None
    
    def _extract_metrics(self, event: SecurityEvent) -> Dict[str, float]:
        """Extract numerical metrics from event."""
        metrics = {}
        
        # Request timing
        if 'processing_time' in event.raw_data:
            metrics['processing_time'] = float(event.raw_data['processing_time'])
        
        # Request size
        if 'content_length' in event.raw_data:
            try:
                metrics['request_size'] = float(event.raw_data['content_length'])
            except (ValueError, TypeError):
                pass
        
        # Path length (indicator of traversal attempts)
        if event.endpoint:
            metrics['path_length'] = len(event.endpoint)
        
        # User agent length (bots often have short/long UAs)
        if event.user_agent:
            metrics['user_agent_length'] = len(event.user_agent)
        
        return metrics
    
    def _is_anomalous(self, metric: str, values: List[float]) -> bool:
        """Check if current values are anomalous."""
        if len(values) < 5:
            return False
        
        # Calculate z-score for recent values
        mean_val = statistics.mean(values[:-3])  # Exclude recent values for baseline
        std_val = statistics.stdev(values[:-3]) if len(values) > 5 else 1.0
        
        # Check recent values
        recent = values[-3:]
        for value in recent:
            if std_val > 0:
                z_score = abs(value - mean_val) / std_val
                if z_score > 3.0:  # 3 standard deviations
                    return True
        
        return False


class PromptInjectionDetector(ThreatDetector):
    """Detect prompt injection attempts in AI/ML inputs."""
    
    def __init__(self):
        """Initialize prompt injection detector."""
        super().__init__("prompt_injection_detector")
        
        # Suspicious patterns in prompts
        self.injection_patterns = [
            "ignore previous instructions",
            "disregard the above",
            "forget everything above",
            "new instructions:",
            "system:",
            "override instructions",
            "jailbreak",
            "act as",
            "pretend to be",
            "roleplay as",
            "\\n\\nHuman:",
            "\\n\\nAssistant:",
        ]
        
        # Encoding bypass attempts
        self.encoding_patterns = [
            "\\x", "\\u", "%", "&#", "&lt;", "&gt;",
            "base64", "hex", "rot13", "caesar"
        ]
    
    async def analyze_event(self, event: SecurityEvent) -> Optional[ThreatType]:
        """Detect prompt injection patterns."""
        
        # Only analyze events with text input
        text_content = ""
        
        if 'prompt' in event.raw_data:
            text_content = str(event.raw_data['prompt']).lower()
        elif 'message' in event.raw_data:
            text_content = str(event.raw_data['message']).lower()
        elif 'input' in event.raw_data:
            text_content = str(event.raw_data['input']).lower()
        
        if not text_content:
            return None
        
        # Check for injection patterns
        injection_score = 0
        detected_patterns = []
        
        for pattern in self.injection_patterns:
            if pattern in text_content:
                injection_score += 2
                detected_patterns.append(pattern)
        
        for pattern in self.encoding_patterns:
            if pattern in text_content:
                injection_score += 1
                detected_patterns.append(f"encoding:{pattern}")
        
        # Check for suspicious length (very long prompts might be attempts to overflow)
        if len(text_content) > 10000:
            injection_score += 1
            detected_patterns.append("excessive_length")
        
        # Check for repeated characters (buffer overflow attempts)
        if any(char * 100 in text_content for char in "aA123!@#"):
            injection_score += 2
            detected_patterns.append("repeated_characters")
        
        # Update event with detection details
        event.threat_indicators.extend(detected_patterns)
        event.anomaly_score = max(event.anomaly_score, injection_score / 5.0)
        
        if injection_score >= 3:
            return ThreatType.PROMPT_INJECTION
        
        return None
    
    def get_confidence(self, event: SecurityEvent) -> float:
        """Calculate confidence based on pattern matches."""
        return min(event.anomaly_score, 1.0)


class IncidentResponseEngine:
    """
    Automated incident detection and response engine.
    
    Coordinates threat detectors, manages incidents, and executes
    automated response actions with integration to Echo systems.
    """
    
    def __init__(self):
        """Initialize incident response engine."""
        self.audit_logger = get_audit_logger()
        
        # Threat Detection
        self.detectors = [
            BruteForceDetector(),
            RateLimitAbuseDetector(), 
            AnomalyDetector(),
            PromptInjectionDetector()
        ]
        
        # Incident Management
        self.active_incidents = {}  # incident_id -> SecurityIncident
        self.event_buffer = deque(maxlen=10000)  # Recent events for correlation
        self.incident_counter = 0
        
        # Response Configuration
        self.auto_response_enabled = True
        self.response_thresholds = {
            IncidentSeverity.LOW: 0.3,
            IncidentSeverity.MEDIUM: 0.6,
            IncidentSeverity.HIGH: 0.8,
            IncidentSeverity.CRITICAL: 0.9
        }
        
        # Response Actions Registry
        self.response_handlers = {
            ResponseAction.LOG_EVENT: self._log_event_action,
            ResponseAction.BLOCK_IP: self._block_ip_action,
            ResponseAction.RATE_LIMIT: self._rate_limit_action,
            ResponseAction.ALERT_SECURITY_TEAM: self._alert_security_team_action,
            ResponseAction.INCREASE_MONITORING: self._increase_monitoring_action
        }
        
        # Echo Systems Integration
        self.echo_integration_enabled = True
        
        incident_logger.info("Incident response engine initialized")
    
    async def process_security_event(
        self,
        request: Request = None,
        event_type: str = "api_request",
        description: str = "",
        raw_data: Dict[str, Any] = None,
        **kwargs
    ) -> Optional[str]:
        """
        Process security event and detect potential threats.
        
        Returns:
            Incident ID if incident created, None otherwise
        """
        
        # Create security event
        event = SecurityEvent(
            event_id=f"evt_{uuid.uuid4().hex[:12]}",
            timestamp=datetime.utcnow(),
            source_ip=self._get_client_ip(request) if request else "unknown",
            user_agent=request.headers.get("user-agent") if request else None,
            endpoint=str(request.url.path) if request else None,
            method=request.method if request else None,
            event_type=event_type,
            description=description,
            raw_data=raw_data or {},
            **kwargs
        )
        
        # Add to event buffer for correlation
        self.event_buffer.append(event)
        
        # Run threat detection
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
                incident_logger.error(f"Error in detector {detector.name}: {e}")
        
        # Create incident if threats detected
        incident_id = None
        if detected_threats and max_confidence > 0.3:
            incident_id = await self._create_incident(event, detected_threats, max_confidence)
        
        # Echo systems integration
        if self.echo_integration_enabled and detected_threats:
            await self._notify_echo_systems(event, detected_threats)
        
        return incident_id
    
    async def _create_incident(
        self,
        trigger_event: SecurityEvent,
        detected_threats: List[tuple],
        confidence: float
    ) -> str:
        """Create security incident from detected threats."""
        
        self.incident_counter += 1
        incident_id = f"inc_{datetime.utcnow().strftime('%Y%m%d')}_{self.incident_counter:04d}"
        
        # Determine primary threat and severity
        primary_threat = detected_threats[0][0]  # Highest confidence threat
        severity = self._calculate_severity(primary_threat, confidence)
        
        # Find related events
        related_events = self._find_related_events(trigger_event)
        
        # Create incident
        incident = SecurityIncident(
            incident_id=incident_id,
            detected_at=datetime.utcnow(),
            threat_type=primary_threat,
            severity=severity,
            status=IncidentStatus.DETECTED,
            title=f"{primary_threat.value.replace('_', ' ').title()} - {trigger_event.source_ip}",
            description=f"Detected {primary_threat.value} with {confidence:.1%} confidence",
            affected_assets=[trigger_event.endpoint or "unknown"],
            events=[trigger_event] + related_events,
            indicators_of_compromise=[
                f"Source IP: {trigger_event.source_ip}",
                f"User Agent: {trigger_event.user_agent or 'Unknown'}",
                f"Detection: {', '.join([t[2] for t in detected_threats])}"
            ],
            estimated_impact=self._estimate_impact(primary_threat, len(related_events))
        )
        
        # Store incident
        self.active_incidents[incident_id] = incident
        
        # Audit log incident creation
        await self.audit_logger.log_event(
            event_type=AuditEventType.SECURITY_ANOMALY_DETECTED,
            message=f"Security incident created: {incident.title}",
            severity=AuditSeverity.HIGH if severity in [IncidentSeverity.HIGH, IncidentSeverity.CRITICAL] else AuditSeverity.MEDIUM,
            client_ip=trigger_event.source_ip,
            details={
                "incident_id": incident_id,
                "threat_type": primary_threat.value,
                "confidence": confidence,
                "affected_assets": incident.affected_assets
            }
        )
        
        # Execute automated response
        if self.auto_response_enabled:
            await self._execute_automated_response(incident)
        
        incident_logger.warning(
            f"Security incident created: {incident_id}",
            threat_type=primary_threat.value,
            severity=severity.value,
            confidence=confidence,
            source_ip=trigger_event.source_ip
        )
        
        return incident_id
    
    def _calculate_severity(self, threat_type: ThreatType, confidence: float) -> IncidentSeverity:
        """Calculate incident severity based on threat type and confidence."""
        
        # Base severity by threat type
        threat_severity = {
            ThreatType.BRUTE_FORCE_ATTACK: IncidentSeverity.MEDIUM,
            ThreatType.DOS_ATTACK: IncidentSeverity.HIGH,
            ThreatType.DDOS_ATTACK: IncidentSeverity.CRITICAL,
            ThreatType.SQL_INJECTION: IncidentSeverity.HIGH,
            ThreatType.XSS_ATTACK: IncidentSeverity.MEDIUM,
            ThreatType.UNAUTHORIZED_ACCESS: IncidentSeverity.HIGH,
            ThreatType.DATA_EXFILTRATION: IncidentSeverity.CRITICAL,
            ThreatType.PROMPT_INJECTION: IncidentSeverity.MEDIUM,
            ThreatType.MODEL_POISONING: IncidentSeverity.HIGH,
            ThreatType.ANOMALOUS_BEHAVIOR: IncidentSeverity.LOW
        }
        
        base_severity = threat_severity.get(threat_type, IncidentSeverity.MEDIUM)
        
        # Adjust based on confidence
        if confidence >= 0.9:
            # High confidence - escalate severity
            if base_severity == IncidentSeverity.LOW:
                return IncidentSeverity.MEDIUM
            elif base_severity == IncidentSeverity.MEDIUM:
                return IncidentSeverity.HIGH
            elif base_severity == IncidentSeverity.HIGH:
                return IncidentSeverity.CRITICAL
        elif confidence < 0.5:
            # Low confidence - reduce severity
            if base_severity == IncidentSeverity.CRITICAL:
                return IncidentSeverity.HIGH
            elif base_severity == IncidentSeverity.HIGH:
                return IncidentSeverity.MEDIUM
            elif base_severity == IncidentSeverity.MEDIUM:
                return IncidentSeverity.LOW
        
        return base_severity
    
    def _find_related_events(self, trigger_event: SecurityEvent) -> List[SecurityEvent]:
        """Find events related to the trigger event for correlation."""
        
        related_events = []
        
        # Look for events from same IP in last 10 minutes
        cutoff_time = trigger_event.timestamp - timedelta(minutes=10)
        
        for event in reversed(self.event_buffer):
            if event.event_id == trigger_event.event_id:
                continue
            
            if event.timestamp < cutoff_time:
                break
            
            # Same source IP
            if event.source_ip == trigger_event.source_ip:
                related_events.append(event)
            
            # Same user/session
            elif (event.user_id and event.user_id == trigger_event.user_id) or \
                 (event.session_id and event.session_id == trigger_event.session_id):
                related_events.append(event)
            
            # Stop at reasonable limit
            if len(related_events) >= 20:
                break
        
        return related_events
    
    def _estimate_impact(self, threat_type: ThreatType, related_events_count: int) -> str:
        """Estimate incident impact."""
        
        impact_levels = {
            "minimal": "Single user/session affected, no data at risk",
            "limited": "Multiple users affected, limited data exposure risk",
            "significant": "System performance impacted, potential data exposure",
            "severe": "Service disruption, confirmed or high risk of data breach"
        }
        
        # Base impact on threat type
        if threat_type in [ThreatType.DATA_EXFILTRATION, ThreatType.UNAUTHORIZED_ACCESS]:
            base_impact = "severe"
        elif threat_type in [ThreatType.DOS_ATTACK, ThreatType.DDOS_ATTACK, ThreatType.SQL_INJECTION]:
            base_impact = "significant"
        elif threat_type in [ThreatType.BRUTE_FORCE_ATTACK, ThreatType.RATE_LIMIT_ABUSE]:
            base_impact = "limited"
        else:
            base_impact = "minimal"
        
        # Escalate based on related events
        if related_events_count > 50:
            if base_impact in ["minimal", "limited"]:
                base_impact = "significant"
        elif related_events_count > 10:
            if base_impact == "minimal":
                base_impact = "limited"
        
        return impact_levels[base_impact]
    
    async def _execute_automated_response(self, incident: SecurityIncident):
        """Execute automated response actions for incident."""
        
        # Determine appropriate response actions
        response_actions = self._get_response_actions(incident)
        
        # Execute each action
        for action in response_actions:
            try:
                if action in self.response_handlers:
                    await self.response_handlers[action](incident)
                    incident.response_actions.append(action)
                    
                    # Audit log the response action
                    await self.audit_logger.log_event(
                        event_type=AuditEventType.SECURITY_POLICY_VIOLATION,
                        message=f"Automated response executed: {action.value}",
                        severity=AuditSeverity.HIGH,
                        details={
                            "incident_id": incident.incident_id,
                            "action": action.value,
                            "threat_type": incident.threat_type.value
                        }
                    )
                    
            except Exception as e:
                incident_logger.error(f"Failed to execute response action {action}: {e}")
        
        # Update incident status
        incident.status = IncidentStatus.MITIGATING
        incident.last_updated = datetime.utcnow()
    
    def _get_response_actions(self, incident: SecurityIncident) -> List[ResponseAction]:
        """Determine appropriate response actions for incident."""
        
        actions = [ResponseAction.LOG_EVENT]  # Always log
        
        # Severity-based actions
        if incident.severity in [IncidentSeverity.MEDIUM, IncidentSeverity.HIGH, IncidentSeverity.CRITICAL]:
            actions.append(ResponseAction.ALERT_SECURITY_TEAM)
        
        if incident.severity in [IncidentSeverity.HIGH, IncidentSeverity.CRITICAL]:
            actions.append(ResponseAction.INCREASE_MONITORING)
        
        # Threat-specific actions
        if incident.threat_type in [
            ThreatType.BRUTE_FORCE_ATTACK,
            ThreatType.RATE_LIMIT_ABUSE,
            ThreatType.DOS_ATTACK
        ]:
            actions.append(ResponseAction.RATE_LIMIT)
        
        if incident.threat_type in [
            ThreatType.DOS_ATTACK,
            ThreatType.DDOS_ATTACK,
            ThreatType.UNAUTHORIZED_ACCESS
        ]:
            actions.append(ResponseAction.BLOCK_IP)
        
        return actions
    
    async def _log_event_action(self, incident: SecurityIncident):
        """Log incident event action."""
        incident_logger.warning(
            "Security incident response: LOG_EVENT",
            incident_id=incident.incident_id,
            threat_type=incident.threat_type.value
        )
    
    async def _block_ip_action(self, incident: SecurityIncident):
        """Block IP address action."""
        # Extract source IPs from incident events
        source_ips = list(set(event.source_ip for event in incident.events))
        
        for ip in source_ips:
            # This would integrate with actual blocking mechanisms
            incident_logger.warning(f"BLOCKING IP: {ip} due to incident {incident.incident_id}")
            
            # Audit log the IP block
            await self.audit_logger.log_event(
                event_type=AuditEventType.SECURITY_IP_BLOCKED,
                message=f"IP address blocked: {ip}",
                severity=AuditSeverity.HIGH,
                client_ip=ip,
                details={
                    "incident_id": incident.incident_id,
                    "reason": incident.threat_type.value
                }
            )
    
    async def _rate_limit_action(self, incident: SecurityIncident):
        """Apply rate limiting action."""
        source_ips = list(set(event.source_ip for event in incident.events))
        
        for ip in source_ips:
            incident_logger.warning(f"RATE LIMITING IP: {ip} due to incident {incident.incident_id}")
    
    async def _alert_security_team_action(self, incident: SecurityIncident):
        """Alert security team action."""
        # This would integrate with alerting systems (email, Slack, PagerDuty, etc.)
        incident_logger.critical(
            f"SECURITY ALERT: {incident.title}",
            incident_id=incident.incident_id,
            severity=incident.severity.value,
            threat_type=incident.threat_type.value,
            affected_assets=incident.affected_assets
        )
    
    async def _increase_monitoring_action(self, incident: SecurityIncident):
        """Increase monitoring level action."""
        incident_logger.info(f"INCREASING MONITORING for incident {incident.incident_id}")
        # This would integrate with monitoring systems to increase sensitivity
    
    async def _notify_echo_systems(
        self,
        event: SecurityEvent, 
        detected_threats: List[tuple]
    ):
        """Notify Echo systems of security threats for enhanced awareness."""
        
        echo_notification = {
            "event_id": event.event_id,
            "timestamp": event.timestamp.isoformat(),
            "threat_types": [threat[0].value for threat in detected_threats],
            "confidence_scores": [threat[1] for threat in detected_threats],
            "source_ip": event.source_ip,
            "affected_endpoint": event.endpoint
        }
        
        # Log Echo system notification
        await self.audit_logger.log_echo_event(
            echo_system="security",
            operation="threat_notification",
            success=True,
            threat_types=echo_notification["threat_types"],
            confidence_scores=echo_notification["confidence_scores"]
        )
        
        incident_logger.info(
            "Notified Echo systems of security threat",
            echo_notification=echo_notification
        )
    
    def _get_client_ip(self, request: Request) -> str:
        """Extract client IP address from request."""
        # Check for proxy headers first
        forwarded_for = request.headers.get('x-forwarded-for')
        if forwarded_for:
            return forwarded_for.split(',')[0].strip()
        
        real_ip = request.headers.get('x-real-ip')
        if real_ip:
            return real_ip
        
        return request.client.host if request.client else "unknown"
    
    async def get_incident_status(self, incident_id: str) -> Optional[Dict[str, Any]]:
        """Get current status of security incident."""
        
        if incident_id not in self.active_incidents:
            return None
        
        incident = self.active_incidents[incident_id]
        
        return {
            "incident_id": incident.incident_id,
            "status": incident.status.value,
            "severity": incident.severity.value,
            "threat_type": incident.threat_type.value,
            "detected_at": incident.detected_at.isoformat(),
            "last_updated": incident.last_updated.isoformat(),
            "events_count": len(incident.events),
            "response_actions": [action.value for action in incident.response_actions],
            "estimated_impact": incident.estimated_impact
        }
    
    async def update_incident_status(
        self,
        incident_id: str,
        new_status: IncidentStatus,
        resolution_notes: str = None
    ) -> bool:
        """Update incident status."""
        
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
        
        # Audit log status change
        await self.audit_logger.log_event(
            event_type=AuditEventType.SYSTEM_CONFIG_CHANGE,
            message=f"Incident status updated: {old_status.value} -> {new_status.value}",
            severity=AuditSeverity.MEDIUM,
            details={
                "incident_id": incident_id,
                "old_status": old_status.value,
                "new_status": new_status.value,
                "resolution_notes": resolution_notes
            }
        )
        
        return True
    
    async def get_security_dashboard(self) -> Dict[str, Any]:
        """Get security dashboard data."""
        
        now = datetime.utcnow()
        last_24h = now - timedelta(hours=24)
        
        # Count incidents by status and severity
        status_counts = defaultdict(int)
        severity_counts = defaultdict(int)
        threat_type_counts = defaultdict(int)
        
        recent_incidents = []
        
        for incident in self.active_incidents.values():
            status_counts[incident.status.value] += 1
            severity_counts[incident.severity.value] += 1
            threat_type_counts[incident.threat_type.value] += 1
            
            if incident.detected_at >= last_24h:
                recent_incidents.append({
                    "incident_id": incident.incident_id,
                    "title": incident.title,
                    "severity": incident.severity.value,
                    "status": incident.status.value,
                    "detected_at": incident.detected_at.isoformat()
                })
        
        # Detector performance
        detector_stats = {}
        for detector in self.detectors:
            detector_stats[detector.name] = {
                "enabled": detector.enabled,
                "detection_count": detector.detection_count,
                "false_positive_rate": detector.false_positive_rate
            }
        
        return {
            "dashboard_generated_at": now.isoformat(),
            "summary": {
                "total_active_incidents": len(self.active_incidents),
                "incidents_last_24h": len(recent_incidents),
                "critical_incidents": severity_counts.get("critical", 0),
                "auto_response_enabled": self.auto_response_enabled
            },
            "incident_statistics": {
                "by_status": dict(status_counts),
                "by_severity": dict(severity_counts),
                "by_threat_type": dict(threat_type_counts)
            },
            "recent_incidents": recent_incidents[-10:],  # Last 10 incidents
            "detector_performance": detector_stats,
            "system_status": {
                "echo_integration": self.echo_integration_enabled,
                "total_detectors": len(self.detectors),
                "enabled_detectors": sum(1 for d in self.detectors if d.enabled)
            }
        }


# Global incident response engine
_global_incident_engine: Optional[IncidentResponseEngine] = None


def get_incident_engine() -> IncidentResponseEngine:
    """Get global incident response engine."""
    global _global_incident_engine
    
    if _global_incident_engine is None:
        _global_incident_engine = IncidentResponseEngine()
    
    return _global_incident_engine


async def process_security_event(**kwargs) -> Optional[str]:
    """Convenience function for processing security events."""
    engine = get_incident_engine()
    return await engine.process_security_event(**kwargs)