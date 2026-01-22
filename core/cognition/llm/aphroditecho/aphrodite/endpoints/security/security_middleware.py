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
    enable_rate_limiting: bool = True
    enable_ip_blocking: bool = True
    enable_request_size_limits: bool = True
    enable_security_monitoring: bool = True
    enable_cors_protection: bool = True
    requests_per_minute: int = 100
    requests_per_hour: int = 1000
    burst_threshold: int = 10
    max_request_size: int = 10 * 1024 * 1024
    max_headers_size: int = 8192
    max_cookies_size: int = 4096
    max_failed_attempts: int = 5
    block_duration_minutes: int = 15
    suspicious_patterns_threshold: int = 3
    log_suspicious_requests: bool = True
    alert_on_anomalies: bool = True
    track_user_agents: bool = True
    allowed_origins: List[str] = ['http://localhost:3000', 'http://127.0.0.1:3000']
    allowed_methods: List[str] = ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS']
    allowed_headers: List[str] = ['*']
    allow_credentials: bool = False
class RateLimiter:
    def __init__(self, requests_per_minute: int=100, burst_threshold: int=10):
        self.requests_per_minute = requests_per_minute
        self.burst_threshold = burst_threshold
        self.clients = defaultdict(lambda: {'tokens': requests_per_minute, 'last_update': time.time(), 'requests': deque()})
    def is_allowed(self, client_id: str) -> bool:
        now = time.time()
        client = self.clients[client_id]
        time_passed = now - client['last_update']
        tokens_to_add = time_passed * (self.requests_per_minute / 60.0)
        client['tokens'] = min(self.requests_per_minute, client['tokens'] + tokens_to_add)
        client['last_update'] = now
        minute_ago = now - 60
        client['requests'] = deque([req_time for req_time in client['requests'] if req_time > minute_ago])
        if len(client['requests']) >= self.burst_threshold:
            return False
        if client['tokens'] >= 1:
            client['tokens'] -= 1
            client['requests'].append(now)
            return True
        return False
    def get_client_stats(self, client_id: str) -> Dict[str, Any]:
        client = self.clients[client_id]
        now = time.time()
        minute_ago = now - 60
        recent_requests = [req_time for req_time in client['requests'] if req_time > minute_ago]
        return {'tokens_remaining': int(client['tokens']), 'requests_last_minute': len(recent_requests), 'last_request': client['last_update']}
class IPBlocklist:
    def __init__(self, block_duration_minutes: int=15):
        self.block_duration_minutes = block_duration_minutes
        self.blocked_ips = {}
        self.failed_attempts = defaultdict(int)
        self.suspicious_patterns = defaultdict(int)
    def is_blocked(self, ip_address: str) -> bool:
        if ip_address in self.blocked_ips:
            if time.time() < self.blocked_ips[ip_address]:
                return True
            else:
                del self.blocked_ips[ip_address]
        return False
    def block_ip(self, ip_address: str, reason: str='security_violation'):
        block_until = time.time() + self.block_duration_minutes * 60
        self.blocked_ips[ip_address] = block_until
        logger.warning(f'Blocked IP {ip_address} until {datetime.fromtimestamp(block_until)} - Reason: {reason}')
    def record_failed_attempt(self, ip_address: str):
        self.failed_attempts[ip_address] += 1
        if self.failed_attempts[ip_address] >= 5:
            self.block_ip(ip_address, 'too_many_failed_attempts')
    def record_suspicious_pattern(self, ip_address: str):
        self.suspicious_patterns[ip_address] += 1
        if self.suspicious_patterns[ip_address] >= 3:
            self.block_ip(ip_address, 'suspicious_patterns')
    def get_stats(self) -> Dict[str, Any]:
        active_blocks = sum((1 for exp_time in self.blocked_ips.values() if time.time() < exp_time))
        return {'active_blocks': active_blocks, 'total_failed_attempts': sum(self.failed_attempts.values()), 'total_suspicious_patterns': sum(self.suspicious_patterns.values())}
class SecurityMonitor:
    def __init__(self):
        self.request_patterns = defaultdict(list)
        self.user_agents = defaultdict(int)
        self.endpoints_accessed = defaultdict(lambda: defaultdict(int))
        self.suspicious_requests = []
    def analyze_request(self, request: Request, client_ip: str) -> Dict[str, Any]:
        analysis = {'anomaly_score': 0, 'suspicious_indicators': [], 'user_agent_suspicious': False, 'path_suspicious': False, 'headers_suspicious': False}
        user_agent = request.headers.get('user-agent', '').lower()
        if user_agent:
            self.user_agents[user_agent] += 1
            suspicious_ua_patterns = ['bot', 'crawler', 'spider', 'scraper', 'curl', 'wget', 'python', 'java', 'scanner', 'test', 'hack', 'exploit', 'attack', 'injection']
            if any((pattern in user_agent for pattern in suspicious_ua_patterns)):
                analysis['user_agent_suspicious'] = True
                analysis['anomaly_score'] += 2
                analysis['suspicious_indicators'].append('suspicious_user_agent')
        path = request.url.path.lower()
        suspicious_paths = ['admin', 'config', 'backup', '.env', 'wp-admin', 'phpmyadmin', 'shell', 'cmd', 'exec', 'system', '../', '..\\', 'passwd', 'shadow']
        if any((suspicious in path for suspicious in suspicious_paths)):
            analysis['path_suspicious'] = True
            analysis['anomaly_score'] += 3
            analysis['suspicious_indicators'].append('suspicious_path')
        suspicious_headers = request.headers.get('x-forwarded-for', '').count(',') > 5
        if suspicious_headers:
            analysis['headers_suspicious'] = True
            analysis['anomaly_score'] += 1
            analysis['suspicious_indicators'].append('suspicious_headers')
        self.endpoints_accessed[client_ip][path] += 1
        if len(self.endpoints_accessed[client_ip]) > 20:
            analysis['anomaly_score'] += 2
            analysis['suspicious_indicators'].append('endpoint_scanning')
        now = time.time()
        self.request_patterns[client_ip].append({'timestamp': now, 'path': path, 'user_agent': user_agent, 'anomaly_score': analysis['anomaly_score']})
        hour_ago = now - 3600
        self.request_patterns[client_ip] = [pattern for pattern in self.request_patterns[client_ip] if pattern['timestamp'] > hour_ago]
        return analysis
    def get_security_stats(self) -> Dict[str, Any]:
        return {'unique_user_agents': len(self.user_agents), 'monitored_ips': len(self.request_patterns), 'suspicious_requests_last_hour': len(self.suspicious_requests), 'top_user_agents': dict(sorted(self.user_agents.items(), key=lambda x: x[1], reverse=True)[:10])}
def get_client_identifier(request: Request) -> str:
    client_ip = request.client.host if request.client else 'unknown'
    forwarded_for = request.headers.get('x-forwarded-for')
    if forwarded_for:
        client_ip = forwarded_for.split(',')[0].strip()
    real_ip = request.headers.get('x-real-ip')
    if real_ip:
        client_ip = real_ip.strip()
    user_agent = request.headers.get('user-agent', '')
    identifier = hashlib.sha256(f'{client_ip}:{user_agent}'.encode()).hexdigest()[:16]
    return identifier
class RateLimitMiddleware(BaseHTTPMiddleware):
    def __init__(self, app, config: SecurityConfig=None):
        super().__init__(app)
        self.config = config or SecurityConfig()
        self.rate_limiter = RateLimiter(requests_per_minute=self.config.requests_per_minute, burst_threshold=self.config.burst_threshold)
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        if not self.config.enable_rate_limiting:
            return await call_next(request)
        client_id = get_client_identifier(request)
        if not self.rate_limiter.is_allowed(client_id):
            stats = self.rate_limiter.get_client_stats(client_id)
            logger.warning(f'Rate limit exceeded for client {client_id[:8]}...')
            return Response(content='Rate limit exceeded. Please slow down your requests.', status_code=429, headers={'X-RateLimit-Limit': str(self.config.requests_per_minute), 'X-RateLimit-Remaining': str(stats['tokens_remaining']), 'X-RateLimit-Reset': str(int(time.time()) + 60), 'Retry-After': '60'})
        response = await call_next(request)
        stats = self.rate_limiter.get_client_stats(client_id)
        response.headers['X-RateLimit-Limit'] = str(self.config.requests_per_minute)
        response.headers['X-RateLimit-Remaining'] = str(stats['tokens_remaining'])
        return response
class SecurityMiddleware(BaseHTTPMiddleware):
    def __init__(self, app, config: SecurityConfig=None):
        super().__init__(app)
        self.config = config or SecurityConfig()
        self.ip_blocklist = IPBlocklist(self.config.block_duration_minutes)
        self.security_monitor = SecurityMonitor()
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        start_time = time.time()
        client_ip = request.client.host if request.client else 'unknown'
        forwarded_for = request.headers.get('x-forwarded-for')
        if forwarded_for:
            client_ip = forwarded_for.split(',')[0].strip()
        real_ip = request.headers.get('x-real-ip')
        if real_ip:
            client_ip = real_ip.strip()
        try:
            if self.config.enable_ip_blocking and self.ip_blocklist.is_blocked(client_ip):
                logger.warning(f'Blocked request from IP {client_ip}')
                return Response(content='Access denied: IP address blocked', status_code=403, headers={'X-Security-Block': 'ip_blocked'})
            if self.config.enable_request_size_limits:
                content_length = request.headers.get('content-length')
                if content_length and int(content_length) > self.config.max_request_size:
                    logger.warning(f'Request size {content_length} exceeds limit from IP {client_ip}')
                    return Response(content='Request too large', status_code=413, headers={'X-Security-Block': 'size_limit'})
                headers_size = sum((len(k) + len(v) for k, v in request.headers.items()))
                if headers_size > self.config.max_headers_size:
                    logger.warning(f'Headers size {headers_size} exceeds limit from IP {client_ip}')
                    return Response(content='Headers too large', status_code=413, headers={'X-Security-Block': 'headers_size'})
            analysis = {}
            if self.config.enable_security_monitoring:
                analysis = self.security_monitor.analyze_request(request, client_ip)
                if analysis['anomaly_score'] >= 5:
                    self.ip_blocklist.record_suspicious_pattern(client_ip)
                    logger.warning(f"High anomaly score {analysis['anomaly_score']} from IP {client_ip}")
                    return Response(content='Access denied: suspicious activity detected', status_code=403, headers={'X-Security-Block': 'anomaly_detection'})
            response = await call_next(request)
            if response.status_code >= 400:
                self.ip_blocklist.record_failed_attempt(client_ip)
                if response.status_code in [401, 403, 404]:
                    self.ip_blocklist.record_suspicious_pattern(client_ip)
            security_time = time.time() - start_time
            response.headers['X-Security-Processed'] = 'true'
            response.headers['X-Security-Time'] = f'{security_time:.3f}'
            response.headers['X-Client-ID'] = get_client_identifier(request)[:8]
            if analysis:
                response.headers['X-Anomaly-Score'] = str(analysis['anomaly_score'])
            return response
        except Exception as e:
            logger.error(f'Security middleware error: {str(e)}')
            return Response(content='Security processing error', status_code=500, headers={'X-Security-Error': 'true'})
class RequestSizeLimitMiddleware(BaseHTTPMiddleware):
    def __init__(self, app, max_size: int=10 * 1024 * 1024):
        super().__init__(app)
        self.max_size = max_size
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        content_length = request.headers.get('content-length')
        if content_length:
            size = int(content_length)
            if size > self.max_size:
                return Response(content=f'Request too large: {size} bytes (max: {self.max_size})', status_code=413, headers={'X-Max-Size': str(self.max_size), 'X-Request-Size': str(size)})
        return await call_next(request)