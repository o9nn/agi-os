"""
Advanced security middleware for FastAPI applications.

Provides comprehensive security protection including DDoS protection, 
rate limiting, anomaly detection, and advanced threat protection 
for server-side endpoints.
"""

import time
import json
import re
from collections import defaultdict, deque
from dataclasses import dataclass, asdict
from typing import Any, Callable, Dict, List, Optional, Set
from datetime import datetime, timedelta
import asyncio

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from loguru import logger

# Import existing security components
from aphrodite.endpoints.security.security_middleware import (
    SecurityConfig as BaseSecurityConfig,
    RateLimiter,
    get_client_identifier
)


@dataclass
class SecurityThreat:
    """Represents a detected security threat."""
    threat_type: str
    severity: str  # low, medium, high, critical
    client_ip: str
    endpoint: str
    details: Dict[str, Any]
    timestamp: datetime
    action_taken: str
    
    def to_dict(self) -> Dict[str, Any]:
        result = asdict(self)
        result['timestamp'] = self.timestamp.isoformat()
        return result


class AdvancedSecurityConfig(BaseSecurityConfig):
    """Extended security configuration with advanced features."""
    
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        
        # DDoS Protection
        self.enable_ddos_protection: bool = True
        self.ddos_detection_window: int = 60  # seconds
        self.ddos_threshold_requests: int = 1000
        self.ddos_ban_duration: int = 3600  # 1 hour
        
        # Advanced Rate Limiting
        self.enable_adaptive_rate_limiting: bool = True
        self.rate_limit_per_endpoint: Dict[str, int] = {}
        self.burst_detection_threshold: float = 5.0  # 5x normal rate
        
        # Anomaly Detection
        self.enable_anomaly_detection: bool = True
        self.anomaly_threshold: float = 0.8
        self.behavioral_analysis: bool = True
        
        # Geolocation Security
        self.enable_geo_blocking: bool = False
        self.blocked_countries: Set[str] = set()
        self.allowed_countries: Set[str] = set()
        
        # Content Security
        self.enable_content_inspection: bool = True
        self.max_request_size: int = 10 * 1024 * 1024  # 10MB
        self.blocked_user_agents: Set[str] = {
            'sqlmap', 'nmap', 'masscan', 'nikto', 'w3af'
        }
        
        # API Security
        self.require_api_key: bool = False
        self.api_key_header: str = "X-API-Key"
        self.valid_api_keys: Set[str] = set()
        
        # Update from kwargs
        for key, value in kwargs.items():
            if hasattr(self, key):
                setattr(self, key, value)


class DDoSProtector:
    """Advanced DDoS protection with multiple detection methods."""
    
    def __init__(self, config: AdvancedSecurityConfig):
        self.config = config
        self.request_counts = defaultdict(deque)
        self.banned_ips = {}  # IP -> ban_expiry_time
        self.ip_patterns = defaultdict(list)  # Track request patterns
        self.lock = asyncio.Lock()
    
    async def check_ddos(
        self, client_ip: str, request: Request
    ) -> Optional[SecurityThreat]:
        """Check for DDoS patterns and return threat if detected."""
        
        async with self.lock:
            current_time = time.time()
            window_start = current_time - self.config.ddos_detection_window
            
            # Clean old entries
            self.request_counts[client_ip] = deque([
                timestamp for timestamp in self.request_counts[client_ip]
                if timestamp > window_start
            ])
            
            # Add current request
            self.request_counts[client_ip].append(current_time)
            
            # Check if IP is already banned
            if client_ip in self.banned_ips:
                if current_time < self.banned_ips[client_ip]:
                    return SecurityThreat(
                        threat_type="ddos_banned_ip",
                        severity="critical",
                        client_ip=client_ip,
                        endpoint=request.url.path,
                        details={"ban_expires": self.banned_ips[client_ip]},
                        timestamp=datetime.utcnow(),
                        action_taken="blocked"
                    )
                else:
                    # Ban expired, remove it
                    del self.banned_ips[client_ip]
            
            # Check request rate
            request_count = len(self.request_counts[client_ip])
            
            if request_count > self.config.ddos_threshold_requests:
                # Ban the IP
                ban_expiry = current_time + self.config.ddos_ban_duration
                self.banned_ips[client_ip] = ban_expiry
                
                return SecurityThreat(
                    threat_type="ddos_attack",
                    severity="critical",
                    client_ip=client_ip,
                    endpoint=request.url.path,
                    details={
                        "requests_in_window": request_count,
                        "threshold": self.config.ddos_threshold_requests,
                        "ban_duration": self.config.ddos_ban_duration
                    },
                    timestamp=datetime.utcnow(),
                    action_taken="ip_banned"
                )
            
            # Check for burst patterns
            if len(self.request_counts[client_ip]) >= 10:
                recent_requests = list(self.request_counts[client_ip])[-10:]
                time_span = recent_requests[-1] - recent_requests[0]
                
                if time_span < 1.0:  # 10 requests in less than 1 second
                    return SecurityThreat(
                        threat_type="burst_attack",
                        severity="high",
                        client_ip=client_ip,
                        endpoint=request.url.path,
                        details={
                            "requests_per_second": 10 / time_span,
                            "time_span": time_span
                        },
                        timestamp=datetime.utcnow(),
                        action_taken="rate_limited"
                    )
            
            return None


class AdvancedAnomalyDetector:
    """Advanced anomaly detection using behavioral analysis."""
    
    def __init__(self, config: AdvancedSecurityConfig):
        self.config = config
        self.client_behaviors = defaultdict(lambda: {
            'endpoints': defaultdict(int),
            'user_agents': defaultdict(int), 
            'request_sizes': deque(maxlen=100),
            'request_timing': deque(maxlen=100),
            'first_seen': time.time(),
            'total_requests': 0
        })
        self.global_baselines = {
            'avg_request_size': 1024,
            'avg_request_interval': 5.0,
            'common_endpoints': defaultdict(int),
            'common_user_agents': defaultdict(int)
        }
    
    async def analyze_request(self, client_ip: str, request: Request) -> Dict[str, Any]:
        """Analyze request for anomalies and return analysis results."""
        
        current_time = time.time()
        behavior = self.client_behaviors[client_ip]
        
        # Update behavior tracking
        behavior['endpoints'][request.url.path] += 1
        behavior['user_agents'][request.headers.get('user-agent', 'unknown')] += 1
        behavior['total_requests'] += 1
        
        # Track request size
        content_length = int(request.headers.get('content-length', '0'))
        behavior['request_sizes'].append(content_length)
        
        # Track request timing
        if len(behavior['request_timing']) > 0:
            interval = current_time - behavior['request_timing'][-1]
            behavior['request_timing'].append(current_time)
        else:
            behavior['request_timing'].append(current_time)
        
        # Calculate anomaly scores
        anomaly_score = 0.0
        anomaly_details = {}
        
        # 1. Unusual endpoint access patterns
        total_endpoints = len(behavior['endpoints'])
        if total_endpoints > 20:  # Accessing many different endpoints
            anomaly_score += 0.2
            anomaly_details['high_endpoint_diversity'] = total_endpoints
        
        # 2. Unusual request size patterns
        if len(behavior['request_sizes']) >= 10:
            avg_size = sum(behavior['request_sizes']) / len(behavior['request_sizes'])
            if avg_size > self.global_baselines['avg_request_size'] * 10:
                anomaly_score += 0.3
                anomaly_details['large_request_size'] = avg_size
        
        # 3. Unusual timing patterns
        if len(behavior['request_timing']) >= 10:
            intervals = [
                behavior['request_timing'][i] - behavior['request_timing'][i-1]
                for i in range(1, len(behavior['request_timing']))
            ]
            avg_interval = sum(intervals) / len(intervals)
            
            if avg_interval < 0.1:  # Very fast requests
                anomaly_score += 0.4
                anomaly_details['rapid_requests'] = avg_interval
        
        # 4. Suspicious user agent
        user_agent = request.headers.get('user-agent', '').lower()
        for blocked_agent in self.config.blocked_user_agents:
            if blocked_agent in user_agent:
                anomaly_score += 0.8
                anomaly_details['suspicious_user_agent'] = user_agent
                break
        
        # 5. Scanner-like behavior
        scanning_patterns = [
            r'/admin', r'/wp-admin', r'/phpmyadmin', r'/.env', 
            r'/config', r'/backup', r'/test', r'/debug'
        ]
        
        for pattern in scanning_patterns:
            if re.search(pattern, request.url.path, re.IGNORECASE):
                anomaly_score += 0.3
                anomaly_details['scanning_behavior'] = request.url.path
                break
        
        # 6. Unusual HTTP methods
        if request.method not in ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS']:
            anomaly_score += 0.2
            anomaly_details['unusual_method'] = request.method
        
        return {
            'anomaly_score': min(anomaly_score, 1.0),
            'client_profile': {
                'total_requests': behavior['total_requests'],
                'unique_endpoints': len(behavior['endpoints']),
                'unique_user_agents': len(behavior['user_agents']),
                'session_duration': current_time - behavior['first_seen']
            },
            'anomaly_details': anomaly_details
        }
    
    async def should_block(self, analysis: Dict[str, Any]) -> bool:
        """Determine if request should be blocked based on analysis."""
        return analysis['anomaly_score'] >= self.config.anomaly_threshold


class ContentInspector:
    """Inspect request content for malicious patterns."""
    
    def __init__(self):
        # SQL injection patterns
        self.sql_patterns = [
            r"(?i)(\bUNION\b.*\bSELECT\b)",
            r"(?i)(\bDROP\b.*\bTABLE\b)",
            r"(?i)(\bINSERT\b.*\bINTO\b)",
            r"(?i)(\bDELETE\b.*\bFROM\b)",
            r"(?i)('.*OR.*'.*=.*')",
            r"(?i)(';.*--)",
        ]
        
        # XSS patterns
        self.xss_patterns = [
            r"(?i)(<script[^>]*>.*</script>)",
            r"(?i)(javascript:)",
            r"(?i)(on\w+\s*=)",
            r"(?i)(<iframe[^>]*>)",
        ]
        
        # Path traversal patterns
        self.path_traversal_patterns = [
            r"(\.\.\/)+",
            r"(\.\.\\)+", 
            r"%2e%2e%2f",
            r"%2e%2e\\",
        ]
    
    async def inspect_request(self, request: Request) -> Optional[SecurityThreat]:
        """Inspect request for malicious content."""
        
        # Check URL path
        path = request.url.path
        query = str(request.query_params)
        
        # Check for path traversal
        for pattern in self.path_traversal_patterns:
            if re.search(pattern, path + query, re.IGNORECASE):
                return SecurityThreat(
                    threat_type="path_traversal",
                    severity="high",
                    client_ip=request.client.host if request.client else "unknown",
                    endpoint=path,
                    details={"pattern": pattern, "matched_content": path + query},
                    timestamp=datetime.utcnow(),
                    action_taken="blocked"
                )
        
        # Check for SQL injection in query parameters
        for pattern in self.sql_patterns:
            if re.search(pattern, query, re.IGNORECASE):
                return SecurityThreat(
                    threat_type="sql_injection",
                    severity="critical",
                    client_ip=request.client.host if request.client else "unknown", 
                    endpoint=path,
                    details={"pattern": pattern, "query": query},
                    timestamp=datetime.utcnow(),
                    action_taken="blocked"
                )
        
        # Check for XSS in query parameters
        for pattern in self.xss_patterns:
            if re.search(pattern, query, re.IGNORECASE):
                return SecurityThreat(
                    threat_type="xss_attempt",
                    severity="high",
                    client_ip=request.client.host if request.client else "unknown",
                    endpoint=path,
                    details={"pattern": pattern, "query": query},
                    timestamp=datetime.utcnow(),
                    action_taken="blocked"
                )
        
        # Check request body if present
        try:
            body = await request.body()
            if body:
                body_str = body.decode('utf-8', errors='ignore')
                
                # Check body for SQL injection
                for pattern in self.sql_patterns:
                    if re.search(pattern, body_str, re.IGNORECASE):
                        return SecurityThreat(
                            threat_type="sql_injection_body",
                            severity="critical",
                            client_ip=request.client.host if request.client else "unknown",
                            endpoint=path,
                            details={"pattern": pattern},
                            timestamp=datetime.utcnow(),
                            action_taken="blocked"
                        )
                
                # Check body for XSS
                for pattern in self.xss_patterns:
                    if re.search(pattern, body_str, re.IGNORECASE):
                        return SecurityThreat(
                            threat_type="xss_attempt_body",
                            severity="high",
                            client_ip=request.client.host if request.client else "unknown",
                            endpoint=path,
                            details={"pattern": pattern},
                            timestamp=datetime.utcnow(),
                            action_taken="blocked"
                        )
        
        except Exception as e:
            logger.warning(f"Failed to inspect request body: {e}")
        
        return None


class AdvancedSecurityMiddleware(BaseHTTPMiddleware):
    """
    Advanced comprehensive security middleware providing DDoS protection,
    anomaly detection, content inspection, and threat mitigation.
    """
    
    def __init__(self, app, config: AdvancedSecurityConfig = None):
        """Initialize advanced security middleware."""
        super().__init__(app)
        self.config = config or AdvancedSecurityConfig()
        
        # Initialize security components
        self.ddos_protector = DDoSProtector(self.config)
        self.anomaly_detector = AdvancedAnomalyDetector(self.config)
        self.content_inspector = ContentInspector()
        self.rate_limiter = RateLimiter(
            requests_per_minute=self.config.requests_per_minute,
            burst_threshold=self.config.burst_threshold
        )
        
        # Security metrics
        self.threat_history = deque(maxlen=1000)
        self.blocked_requests = 0
        self.total_requests = 0
        
        logger.info("AdvancedSecurityMiddleware initialized with comprehensive protection")
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Apply comprehensive security protection to requests."""
        
        start_time = time.time()
        self.total_requests += 1
        client_ip = request.client.host if request.client else "unknown"
        
        try:
            # 1. DDoS Protection
            if self.config.enable_ddos_protection:
                ddos_threat = await self.ddos_protector.check_ddos(client_ip, request)
                if ddos_threat:
                    self.threat_history.append(ddos_threat)
                    self.blocked_requests += 1
                    
                    return await self._create_security_response(
                        ddos_threat, 
                        429 if ddos_threat.threat_type == "burst_attack" else 403
                    )
            
            # 2. Rate Limiting
            if self.config.enable_rate_limiting:
                client_id = get_client_identifier(request)
                if not self.rate_limiter.is_allowed(client_id):
                    threat = SecurityThreat(
                        threat_type="rate_limit_exceeded",
                        severity="medium",
                        client_ip=client_ip,
                        endpoint=request.url.path,
                        details={"rate_limit": self.config.requests_per_minute},
                        timestamp=datetime.utcnow(),
                        action_taken="rate_limited"
                    )
                    
                    self.threat_history.append(threat)
                    self.blocked_requests += 1
                    
                    return await self._create_security_response(threat, 429)
            
            # 3. Content Inspection
            if self.config.enable_content_inspection:
                content_threat = await self.content_inspector.inspect_request(request)
                if content_threat:
                    self.threat_history.append(content_threat)
                    self.blocked_requests += 1
                    
                    return await self._create_security_response(content_threat, 403)
            
            # 4. Anomaly Detection
            if self.config.enable_anomaly_detection:
                analysis = await self.anomaly_detector.analyze_request(client_ip, request)
                
                if await self.anomaly_detector.should_block(analysis):
                    threat = SecurityThreat(
                        threat_type="anomaly_detected",
                        severity="high",
                        client_ip=client_ip,
                        endpoint=request.url.path,
                        details=analysis,
                        timestamp=datetime.utcnow(),
                        action_taken="blocked"
                    )
                    
                    self.threat_history.append(threat)
                    self.blocked_requests += 1
                    
                    return await self._create_security_response(threat, 403)
                
                # Store analysis in request state for logging
                request.state.security_analysis = analysis
            
            # 5. API Key Validation (if required)
            if self.config.require_api_key:
                api_key = request.headers.get(self.config.api_key_header)
                if not api_key or api_key not in self.config.valid_api_keys:
                    threat = SecurityThreat(
                        threat_type="invalid_api_key",
                        severity="medium",
                        client_ip=client_ip,
                        endpoint=request.url.path,
                        details={"provided_key": api_key is not None},
                        timestamp=datetime.utcnow(),
                        action_taken="unauthorized"
                    )
                    
                    self.threat_history.append(threat)
                    self.blocked_requests += 1
                    
                    return await self._create_security_response(threat, 401)
            
            # Process request through middleware chain
            response = await call_next(request)
            
            # Add security headers
            security_time = time.time() - start_time
            response.headers["X-Advanced-Security-Processed"] = "true"
            response.headers["X-Security-Processing-Time"] = f"{security_time:.3f}"
            response.headers["X-Client-Threat-Score"] = str(
                getattr(request.state, 'security_analysis', {}).get('anomaly_score', 0.0)
            )
            
            return response
            
        except Exception as e:
            logger.error(f"Advanced security middleware error: {str(e)}")
            
            # Log as security incident
            threat = SecurityThreat(
                threat_type="middleware_error",
                severity="medium",
                client_ip=client_ip,
                endpoint=request.url.path,
                details={"error": str(e)},
                timestamp=datetime.utcnow(),
                action_taken="logged"
            )
            
            self.threat_history.append(threat)
            
            # Don't block on middleware errors, just log
            return await call_next(request)
    
    async def _create_security_response(self, threat: SecurityThreat, status_code: int) -> Response:
        """Create standardized security response."""
        
        logger.warning(
            f"Security threat blocked: {threat.threat_type} from {threat.client_ip} "
            f"on {threat.endpoint} (severity: {threat.severity})"
        )
        
        content = {
            "error": "Security validation failed",
            "type": threat.threat_type,
            "message": self._get_user_friendly_message(threat.threat_type),
            "request_id": threat.timestamp.isoformat()
        }
        
        headers = {
            "X-Security-Block": threat.threat_type,
            "X-Threat-Severity": threat.severity,
            "X-Request-ID": threat.timestamp.isoformat()
        }
        
        if threat.threat_type in ["rate_limit_exceeded", "burst_attack"]:
            headers["Retry-After"] = "60"
        
        return Response(
            content=json.dumps(content),
            status_code=status_code,
            headers=headers,
            media_type="application/json"
        )
    
    def _get_user_friendly_message(self, threat_type: str) -> str:
        """Get user-friendly message for threat type."""
        messages = {
            "ddos_attack": "Too many requests. Please slow down.",
            "burst_attack": "Request rate too high. Please wait before retrying.",
            "rate_limit_exceeded": "Rate limit exceeded. Please try again later.",
            "sql_injection": "Invalid request format detected.",
            "xss_attempt": "Invalid request content detected.",
            "path_traversal": "Invalid path access attempted.",
            "anomaly_detected": "Unusual request pattern detected.",
            "invalid_api_key": "Authentication required.",
            "ddos_banned_ip": "Access temporarily restricted."
        }
        
        return messages.get(threat_type, "Request blocked due to security policy.")
    
    def get_security_metrics(self) -> Dict[str, Any]:
        """Get security metrics and statistics."""
        
        recent_threats = [
            threat for threat in self.threat_history
            if (datetime.utcnow() - threat.timestamp).total_seconds() < 3600
        ]
        
        threat_counts = defaultdict(int)
        for threat in recent_threats:
            threat_counts[threat.threat_type] += 1
        
        return {
            "total_requests": self.total_requests,
            "blocked_requests": self.blocked_requests,
            "block_rate": self.blocked_requests / max(self.total_requests, 1),
            "threats_last_hour": len(recent_threats),
            "threat_types": dict(threat_counts),
            "active_bans": len(self.ddos_protector.banned_ips),
            "monitored_clients": len(self.anomaly_detector.client_behaviors)
        }
    
    def get_threat_history(self, hours: int = 24) -> List[Dict[str, Any]]:
        """Get threat history for specified time period."""
        
        cutoff = datetime.utcnow() - timedelta(hours=hours)
        return [
            threat.to_dict() for threat in self.threat_history
            if threat.timestamp > cutoff
        ]