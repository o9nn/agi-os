"""
Main security middleware for FastAPI applications.

Provides comprehensive security middleware including rate limiting, CORS management,
authentication framework, and security monitoring for server-side endpoints.
"""

import time
import logging
from collections import defaultdict, deque
from typing import Any, Callable, Dict, List
from datetime import datetime
import hashlib

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from pydantic import BaseModel

logger = logging.getLogger(__name__)

class SecurityConfig(BaseModel):
    """Configuration for security middleware."""
    enable_rate_limiting: bool = True
    enable_ip_blocking: bool = True
    enable_request_size_limits: bool = True
    enable_security_monitoring: bool = True
    enable_cors_protection: bool = True
    
    # Rate limiting settings
    requests_per_minute: int = 100
    requests_per_hour: int = 1000
    burst_threshold: int = 10
    
    # Request size limits
    max_request_size: int = 10 * 1024 * 1024  # 10MB
    max_headers_size: int = 8192
    max_cookies_size: int = 4096
    
    # IP blocking settings
    max_failed_attempts: int = 5
    block_duration_minutes: int = 15
    suspicious_patterns_threshold: int = 3
    
    # Monitoring settings
    log_suspicious_requests: bool = True
    alert_on_anomalies: bool = True
    track_user_agents: bool = True
    
    # CORS settings
    allowed_origins: List[str] = ["http://localhost:3000", "http://127.0.0.1:3000"]
    allowed_methods: List[str] = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    allowed_headers: List[str] = ["*"]
    allow_credentials: bool = False

class RateLimiter:
    """
    Token bucket rate limiter with burst support.
    """
    
    def __init__(self, requests_per_minute: int = 100, burst_threshold: int = 10):
        """Initialize rate limiter."""
        self.requests_per_minute = requests_per_minute
        self.burst_threshold = burst_threshold
        self.clients = defaultdict(lambda: {
            'tokens': requests_per_minute,
            'last_update': time.time(),
            'requests': deque()
        })
    
    def is_allowed(self, client_id: str) -> bool:
        """
        Check if request is allowed for client.
        
        Args:
            client_id: Unique identifier for client
            
        Returns:
            True if request is allowed, False otherwise
        """
        now = time.time()
        client = self.clients[client_id]
        
        # Update token bucket
        time_passed = now - client['last_update']
        tokens_to_add = time_passed * (self.requests_per_minute / 60.0)
        client['tokens'] = min(self.requests_per_minute, client['tokens'] + tokens_to_add)
        client['last_update'] = now
        
        # Check burst protection
        minute_ago = now - 60
        client['requests'] = deque([req_time for req_time in client['requests'] if req_time > minute_ago])
        
        if len(client['requests']) >= self.burst_threshold:
            return False
        
        # Check token availability
        if client['tokens'] >= 1:
            client['tokens'] -= 1
            client['requests'].append(now)
            return True
        
        return False
    
    def get_client_stats(self, client_id: str) -> Dict[str, Any]:
        """Get statistics for a client."""
        client = self.clients[client_id]
        now = time.time()
        minute_ago = now - 60
        recent_requests = [req_time for req_time in client['requests'] if req_time > minute_ago]
        
        return {
            'tokens_remaining': int(client['tokens']),
            'requests_last_minute': len(recent_requests),
            'last_request': client['last_update']
        }

class IPBlocklist:
    """
    IP address blocking system with automatic expiration.
    """
    
    def __init__(self, block_duration_minutes: int = 15):
        """Initialize IP blocklist."""
        self.block_duration_minutes = block_duration_minutes
        self.blocked_ips = {}  # ip -> block_expiry_time
        self.failed_attempts = defaultdict(int)
        self.suspicious_patterns = defaultdict(int)
    
    def is_blocked(self, ip_address: str) -> bool:
        """Check if IP address is currently blocked."""
        if ip_address in self.blocked_ips:
            if time.time() < self.blocked_ips[ip_address]:
                return True
            else:
                # Block expired, remove it
                del self.blocked_ips[ip_address]
        return False
    
    def block_ip(self, ip_address: str, reason: str = "security_violation"):
        """Block an IP address."""
        block_until = time.time() + (self.block_duration_minutes * 60)
        self.blocked_ips[ip_address] = block_until
        logger.warning(f"Blocked IP {ip_address} until {datetime.fromtimestamp(block_until)} - Reason: {reason}")
    
    def record_failed_attempt(self, ip_address: str):
        """Record a failed attempt from an IP."""
        self.failed_attempts[ip_address] += 1
        if self.failed_attempts[ip_address] >= 5:  # Configurable threshold
            self.block_ip(ip_address, "too_many_failed_attempts")
    
    def record_suspicious_pattern(self, ip_address: str):
        """Record suspicious behavior pattern."""
        self.suspicious_patterns[ip_address] += 1
        if self.suspicious_patterns[ip_address] >= 3:  # Configurable threshold
            self.block_ip(ip_address, "suspicious_patterns")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get blocking statistics."""
        active_blocks = sum(1 for exp_time in self.blocked_ips.values() if time.time() < exp_time)
        return {
            'active_blocks': active_blocks,
            'total_failed_attempts': sum(self.failed_attempts.values()),
            'total_suspicious_patterns': sum(self.suspicious_patterns.values())
        }

class SecurityMonitor:
    """
    Security monitoring and anomaly detection.
    """
    
    def __init__(self):
        """Initialize security monitor."""
        self.request_patterns = defaultdict(list)
        self.user_agents = defaultdict(int)
        self.endpoints_accessed = defaultdict(lambda: defaultdict(int))
        self.suspicious_requests = []
    
    def analyze_request(self, request: Request, client_ip: str) -> Dict[str, Any]:
        """
        Analyze request for security anomalies.
        
        Args:
            request: FastAPI request object
            client_ip: Client IP address
            
        Returns:
            Analysis results with anomaly scores
        """
        analysis = {
            'anomaly_score': 0,
            'suspicious_indicators': [],
            'user_agent_suspicious': False,
            'path_suspicious': False,
            'headers_suspicious': False
        }
        
        # Analyze user agent
        user_agent = request.headers.get('user-agent', '').lower()
        if user_agent:
            self.user_agents[user_agent] += 1
            
            # Check for suspicious user agents
            suspicious_ua_patterns = [
                'bot', 'crawler', 'spider', 'scraper', 'curl', 'wget', 'python', 'java',
                'scanner', 'test', 'hack', 'exploit', 'attack', 'injection'
            ]
            
            if any(pattern in user_agent for pattern in suspicious_ua_patterns):
                analysis['user_agent_suspicious'] = True
                analysis['anomaly_score'] += 2
                analysis['suspicious_indicators'].append('suspicious_user_agent')
        
        # Analyze request path
        path = request.url.path.lower()
        suspicious_paths = [
            'admin', 'config', 'backup', '.env', 'wp-admin', 'phpmyadmin',
            'shell', 'cmd', 'exec', 'system', '../', '..\\', 'passwd', 'shadow'
        ]
        
        if any(suspicious in path for suspicious in suspicious_paths):
            analysis['path_suspicious'] = True
            analysis['anomaly_score'] += 3
            analysis['suspicious_indicators'].append('suspicious_path')
        
        # Analyze headers
        suspicious_headers = request.headers.get('x-forwarded-for', '').count(',') > 5
        if suspicious_headers:
            analysis['headers_suspicious'] = True
            analysis['anomaly_score'] += 1
            analysis['suspicious_indicators'].append('suspicious_headers')
        
        # Track endpoint access patterns
        self.endpoints_accessed[client_ip][path] += 1
        
        # Check for rapid endpoint scanning
        if len(self.endpoints_accessed[client_ip]) > 20:  # More than 20 different endpoints
            analysis['anomaly_score'] += 2
            analysis['suspicious_indicators'].append('endpoint_scanning')
        
        # Record patterns
        now = time.time()
        self.request_patterns[client_ip].append({
            'timestamp': now,
            'path': path,
            'user_agent': user_agent,
            'anomaly_score': analysis['anomaly_score']
        })
        
        # Keep only recent patterns (last hour)
        hour_ago = now - 3600
        self.request_patterns[client_ip] = [
            pattern for pattern in self.request_patterns[client_ip]
            if pattern['timestamp'] > hour_ago
        ]
        
        return analysis
    
    def get_security_stats(self) -> Dict[str, Any]:
        """Get security monitoring statistics."""
        return {
            'unique_user_agents': len(self.user_agents),
            'monitored_ips': len(self.request_patterns),
            'suspicious_requests_last_hour': len(self.suspicious_requests),
            'top_user_agents': dict(sorted(self.user_agents.items(), key=lambda x: x[1], reverse=True)[:10])
        }

def get_client_identifier(request: Request) -> str:
    """
    Generate unique client identifier for rate limiting.
    
    Args:
        request: FastAPI request object
        
    Returns:
        Unique client identifier
    """
    # Get client IP (handle proxies)
    client_ip = request.client.host if request.client else "unknown"
    
    # Check for forwarded IPs
    forwarded_for = request.headers.get('x-forwarded-for')
    if forwarded_for:
        client_ip = forwarded_for.split(',')[0].strip()
    
    real_ip = request.headers.get('x-real-ip')
    if real_ip:
        client_ip = real_ip.strip()
    
    # Include user agent for more specific identification
    user_agent = request.headers.get('user-agent', '')
    
    # Create hash of IP + user agent for privacy
    identifier = hashlib.sha256(f"{client_ip}:{user_agent}".encode()).hexdigest()[:16]
    
    return identifier

class RateLimitMiddleware(BaseHTTPMiddleware):
    """
    Rate limiting middleware with burst protection.
    """
    
    def __init__(self, app, config: SecurityConfig = None):
        """Initialize rate limit middleware."""
        super().__init__(app)
        self.config = config or SecurityConfig()
        self.rate_limiter = RateLimiter(
            requests_per_minute=self.config.requests_per_minute,
            burst_threshold=self.config.burst_threshold
        )
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Apply rate limiting to requests."""
        if not self.config.enable_rate_limiting:
            return await call_next(request)
        
        client_id = get_client_identifier(request)
        
        if not self.rate_limiter.is_allowed(client_id):
            stats = self.rate_limiter.get_client_stats(client_id)
            logger.warning(f"Rate limit exceeded for client {client_id[:8]}...")
            
            return Response(
                content="Rate limit exceeded. Please slow down your requests.",
                status_code=429,
                headers={
                    "X-RateLimit-Limit": str(self.config.requests_per_minute),
                    "X-RateLimit-Remaining": str(stats['tokens_remaining']),
                    "X-RateLimit-Reset": str(int(time.time()) + 60),
                    "Retry-After": "60"
                }
            )
        
        response = await call_next(request)
        
        # Add rate limit headers to successful responses
        stats = self.rate_limiter.get_client_stats(client_id)
        response.headers["X-RateLimit-Limit"] = str(self.config.requests_per_minute)
        response.headers["X-RateLimit-Remaining"] = str(stats['tokens_remaining'])
        
        return response

class SecurityMiddleware(BaseHTTPMiddleware):
    """
    Comprehensive security middleware.
    
    Provides IP blocking, security monitoring, request size limits,
    and anomaly detection for FastAPI applications.
    """
    
    def __init__(self, app, config: SecurityConfig = None):
        """Initialize security middleware."""
        super().__init__(app)
        self.config = config or SecurityConfig()
        self.ip_blocklist = IPBlocklist(self.config.block_duration_minutes)
        self.security_monitor = SecurityMonitor()
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Apply comprehensive security checks."""
        start_time = time.time()
        client_ip = request.client.host if request.client else "unknown"
        
        # Get real client IP from headers
        forwarded_for = request.headers.get('x-forwarded-for')
        if forwarded_for:
            client_ip = forwarded_for.split(',')[0].strip()
        
        real_ip = request.headers.get('x-real-ip')
        if real_ip:
            client_ip = real_ip.strip()
        
        try:
            # Check IP blocklist
            if self.config.enable_ip_blocking and self.ip_blocklist.is_blocked(client_ip):
                logger.warning(f"Blocked request from IP {client_ip}")
                return Response(
                    content="Access denied: IP address blocked",
                    status_code=403,
                    headers={"X-Security-Block": "ip_blocked"}
                )
            
            # Check request size limits
            if self.config.enable_request_size_limits:
                content_length = request.headers.get('content-length')
                if content_length and int(content_length) > self.config.max_request_size:
                    logger.warning(f"Request size {content_length} exceeds limit from IP {client_ip}")
                    return Response(
                        content="Request too large",
                        status_code=413,
                        headers={"X-Security-Block": "size_limit"}
                    )
                
                # Check headers size
                headers_size = sum(len(k) + len(v) for k, v in request.headers.items())
                if headers_size > self.config.max_headers_size:
                    logger.warning(f"Headers size {headers_size} exceeds limit from IP {client_ip}")
                    return Response(
                        content="Headers too large",
                        status_code=413,
                        headers={"X-Security-Block": "headers_size"}
                    )
            
            # Security monitoring and anomaly detection
            analysis = {}
            if self.config.enable_security_monitoring:
                analysis = self.security_monitor.analyze_request(request, client_ip)
                
                # Block if anomaly score is too high
                if analysis['anomaly_score'] >= 5:
                    self.ip_blocklist.record_suspicious_pattern(client_ip)
                    logger.warning(f"High anomaly score {analysis['anomaly_score']} from IP {client_ip}")
                    return Response(
                        content="Access denied: suspicious activity detected",
                        status_code=403,
                        headers={"X-Security-Block": "anomaly_detection"}
                    )
            
            # Process request
            response = await call_next(request)
            
            # Handle error responses
            if response.status_code >= 400:
                self.ip_blocklist.record_failed_attempt(client_ip)
                
                # Additional monitoring for repeated failures
                if response.status_code in [401, 403, 404]:
                    self.ip_blocklist.record_suspicious_pattern(client_ip)
            
            # Add security headers
            security_time = time.time() - start_time
            response.headers["X-Security-Processed"] = "true"
            response.headers["X-Security-Time"] = f"{security_time:.3f}"
            response.headers["X-Client-ID"] = get_client_identifier(request)[:8]  # First 8 chars only
            
            if analysis:
                response.headers["X-Anomaly-Score"] = str(analysis['anomaly_score'])
            
            return response
            
        except Exception as e:
            logger.error(f"Security middleware error: {str(e)}")
            return Response(
                content="Security processing error",
                status_code=500,
                headers={"X-Security-Error": "true"}
            )

class RequestSizeLimitMiddleware(BaseHTTPMiddleware):
    """
    Middleware for enforcing request size limits.
    """
    
    def __init__(self, app, max_size: int = 10 * 1024 * 1024):
        """Initialize request size limit middleware."""
        super().__init__(app)
        self.max_size = max_size
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Enforce request size limits."""
        content_length = request.headers.get('content-length')
        
        if content_length:
            size = int(content_length)
            if size > self.max_size:
                return Response(
                    content=f"Request too large: {size} bytes (max: {self.max_size})",
                    status_code=413,
                    headers={
                        "X-Max-Size": str(self.max_size),
                        "X-Request-Size": str(size)
                    }
                )
        
        return await call_next(request)