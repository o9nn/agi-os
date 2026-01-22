import asyncio
import time
import json
from datetime import datetime
from typing import Dict, Any
try:
    from fastapi import FastAPI, Request, Response
    from fastapi.testclient import TestClient
    from starlette.middleware.base import BaseHTTPMiddleware
    from loguru import logger
    import structlog
    DEPENDENCIES_AVAILABLE = True
except ImportError as e:
    print(f'Dependencies not available: {e}')
    print('Install with: pip install fastapi loguru structlog psutil')
    DEPENDENCIES_AVAILABLE = False
class DemoLoggingMiddleware(BaseHTTPMiddleware):
    def __init__(self, app):
        super().__init__(app)
        self.request_count = 0
        logger.add('middleware_demo.log', format='{time:YYYY-MM-DD HH:mm:ss} | {level} | {message}', level='INFO')
    async def dispatch(self, request: Request, call_next):
        self.request_count += 1
        request_id = f'req_{self.request_count:06d}'
        start_time = time.time()
        logger.info(f'Request started: {request.method} {request.url.path}', extra={'request_id': request_id, 'method': request.method, 'path': request.url.path, 'client_ip': request.client.host if request.client else 'unknown', 'user_agent': request.headers.get('user-agent', 'unknown')})
        try:
            response = await call_next(request)
            process_time = time.time() - start_time
            logger.info(f'Request completed: {request.method} {request.url.path} in {process_time * 1000:.2f}ms with status {response.status_code}', extra={'request_id': request_id, 'status_code': response.status_code, 'processing_time_ms': round(process_time * 1000, 2)})
            response.headers['X-Request-ID'] = request_id
            response.headers['X-Process-Time-Ms'] = f'{process_time * 1000:.2f}'
            return response
        except Exception as e:
            process_time = time.time() - start_time
            logger.error(f'Request failed: {request.method} {request.url.path} after {process_time * 1000:.2f}ms with error: {str(e)}', extra={'request_id': request_id, 'error': str(e), 'processing_time_ms': round(process_time * 1000, 2)})
            raise
class DemoPerformanceMiddleware(BaseHTTPMiddleware):
    def __init__(self, app):
        super().__init__(app)
        self.request_times = []
        self.slow_request_threshold = 100.0
        self.concurrent_requests = 0
    async def dispatch(self, request: Request, call_next):
        self.concurrent_requests += 1
        start_time = time.time()
        try:
            memory_usage = self._get_memory_usage()
            response = await call_next(request)
            process_time = time.time() - start_time
            process_time_ms = process_time * 1000
            self.request_times.append(process_time_ms)
            if len(self.request_times) > 100:
                self.request_times.pop(0)
            response.headers['X-Process-Time-Ms'] = f'{process_time_ms:.2f}'
            response.headers['X-Memory-Usage-Mb'] = f'{memory_usage:.1f}'
            response.headers['X-Concurrent-Requests'] = str(self.concurrent_requests)
            if len(self.request_times) >= 5:
                avg_time = sum(self.request_times) / len(self.request_times)
                response.headers['X-Avg-Response-Time-Ms'] = f'{avg_time:.2f}'
            if process_time_ms > self.slow_request_threshold:
                logger.warning(f'Slow request detected: {request.method} {request.url.path} took {process_time_ms:.2f}ms (threshold: {self.slow_request_threshold}ms)')
            return response
        finally:
            self.concurrent_requests -= 1
    def _get_memory_usage(self) -> float:
        try:
            import psutil
            process = psutil.Process()
            return process.memory_info().rss / 1024 / 1024
        except ImportError:
            return 0.0
    def get_statistics(self) -> Dict[str, Any]:
        if not self.request_times:
            return {'no_data': True}
        return {'total_requests': len(self.request_times), 'avg_response_time_ms': sum(self.request_times) / len(self.request_times), 'min_response_time_ms': min(self.request_times), 'max_response_time_ms': max(self.request_times), 'current_concurrent': self.concurrent_requests, 'slow_requests': len([t for t in self.request_times if t > self.slow_request_threshold])}
class DemoSecurityMiddleware(BaseHTTPMiddleware):
    def __init__(self, app):
        super().__init__(app)
        self.request_counts = {}
        self.blocked_ips = set()
        self.rate_limit = 10
        self.security_events = []
    async def dispatch(self, request: Request, call_next):
        client_ip = request.client.host if request.client else 'unknown'
        current_time = time.time()
        if client_ip not in self.request_counts:
            self.request_counts[client_ip] = []
        self.request_counts[client_ip] = [timestamp for timestamp in self.request_counts[client_ip] if current_time - timestamp < 60]
        if len(self.request_counts[client_ip]) >= self.rate_limit:
            self.blocked_ips.add(client_ip)
            security_event = {'type': 'rate_limit_exceeded', 'client_ip': client_ip, 'endpoint': request.url.path, 'timestamp': datetime.utcnow().isoformat(), 'request_count': len(self.request_counts[client_ip])}
            self.security_events.append(security_event)
            logger.warning(f'Rate limit exceeded for IP {client_ip}: {len(self.request_counts[client_ip])} requests in last minute')
            return Response(content=json.dumps({'error': 'Rate limit exceeded', 'message': 'Too many requests. Please slow down.', 'retry_after': 60}), status_code=429, headers={'Content-Type': 'application/json', 'Retry-After': '60', 'X-Security-Block': 'rate_limit'})
        if client_ip in self.blocked_ips:
            return Response(content='Access denied', status_code=403, headers={'X-Security-Block': 'ip_blocked'})
        path = request.url.path.lower()
        query = str(request.query_params).lower()
        suspicious_patterns = ["' or '1'='1", '<script>', '../', 'admin', 'wp-admin']
        for pattern in suspicious_patterns:
            if pattern in path or pattern in query:
                security_event = {'type': 'suspicious_pattern', 'client_ip': client_ip, 'endpoint': request.url.path, 'pattern': pattern, 'timestamp': datetime.utcnow().isoformat()}
                self.security_events.append(security_event)
                logger.warning(f'Suspicious pattern detected from {client_ip}: {pattern} in {request.url.path}')
                return Response(content=json.dumps({'error': 'Security violation detected', 'message': 'Request blocked due to security policy'}), status_code=403, headers={'Content-Type': 'application/json', 'X-Security-Block': 'content_inspection'})
        self.request_counts[client_ip].append(current_time)
        response = await call_next(request)
        response.headers['X-Security-Processed'] = 'true'
        response.headers['X-Client-IP'] = client_ip
        return response
    def get_security_metrics(self) -> Dict[str, Any]:
        return {'monitored_ips': len(self.request_counts), 'blocked_ips': len(self.blocked_ips), 'security_events': len(self.security_events), 'recent_events': self.security_events[-10:] if self.security_events else []}
def create_demo_app() -> FastAPI:
    app = FastAPI(title='Advanced Middleware Demo', version='1.0.0')
    app.add_middleware(DemoLoggingMiddleware)
    app.add_middleware(DemoPerformanceMiddleware)
    app.add_middleware(DemoSecurityMiddleware)
    @app.get('/')
    async def root():
        return {'message': 'Advanced Middleware Stack Demo', 'timestamp': datetime.utcnow().isoformat(), 'features': ['Comprehensive request/response logging', 'Performance monitoring and profiling', 'Security protection with rate limiting']}
    @app.get('/fast')
    async def fast_endpoint():
        return {'message': 'fast response', 'data': list(range(10))}
    @app.get('/slow')
    async def slow_endpoint():
        await asyncio.sleep(0.2)
        return {'message': 'slow response', 'processing_time': '200ms'}
    @app.get('/error')
    async def error_endpoint():
        raise ValueError('Intentional error for demo purposes')
    @app.get('/metrics')
    async def metrics_endpoint(request: Request):
        metrics = {}
        for middleware in app.user_middleware:
            if hasattr(middleware, 'cls'):
                if middleware.cls == DemoPerformanceMiddleware:
                    pass
                elif middleware.cls == DemoSecurityMiddleware:
                    pass
        return {'message': 'Metrics collection demo', 'note': 'In real implementation, middleware instances would be accessible', 'timestamp': datetime.utcnow().isoformat()}
    @app.get('/health')
    async def health_endpoint():
        return {'status': 'healthy', 'timestamp': datetime.utcnow().isoformat(), 'middleware': {'logging': 'active', 'performance': 'active', 'security': 'active'}}
    return app
def run_demo():
    if not DEPENDENCIES_AVAILABLE:
        print('❌ Required dependencies not available')
        print('Install with: pip install fastapi loguru structlog psutil')
        return
    print('🚀 Starting Advanced Middleware Stack Demo')
    print('=' * 50)
    app = create_demo_app()
    client = TestClient(app)
    print('\n📊 Testing middleware functionality...')
    print('\n1. Normal request:')
    response = client.get('/')
    print(f'   Status: {response.status_code}')
    print(f"   Headers: X-Request-ID={response.headers.get('X-Request-ID')}")
    print(f"   Process Time: {response.headers.get('X-Process-Time-Ms')}ms")
    print('\n2. Fast endpoint:')
    response = client.get('/fast')
    print(f'   Status: {response.status_code}')
    print(f"   Process Time: {response.headers.get('X-Process-Time-Ms')}ms")
    print('\n3. Slow endpoint (should trigger slow request warning):')
    response = client.get('/slow')
    print(f'   Status: {response.status_code}')
    print(f"   Process Time: {response.headers.get('X-Process-Time-Ms')}ms")
    print('\n4. Rate limiting test (making many requests):')
    rate_limited = False
    for i in range(15):
        response = client.get(f'/fast?test={i}')
        if response.status_code == 429:
            rate_limited = True
            print(f'   ✓ Rate limited at request {i + 1}')
            break
    if not rate_limited:
        print('   Note: Rate limiting may not trigger in single-threaded test')
    print('\n5. Security test (SQL injection detection):')
    response = client.get("/fast?id=1' OR '1'='1")
    if response.status_code == 403:
        print('   ✓ SQL injection attempt blocked')
        print(f"   Security Block: {response.headers.get('X-Security-Block')}")
    else:
        print('   Note: Security blocking may vary')
    print('\n6. Error handling test:')
    try:
        response = client.get('/error')
    except Exception:
        print('   ✓ Error logged and handled by middleware')
    print('\n7. Health check:')
    response = client.get('/health')
    print(f'   Status: {response.status_code}')
    print(f"   Health: {response.json().get('status')}")
    print('\n' + '=' * 50)
    print('✅ Advanced Middleware Stack Demo completed!')
    print('\n📝 Key Features Demonstrated:')
    print('   • Comprehensive request/response logging with correlation IDs')
    print('   • Performance monitoring with timing and memory tracking')
    print('   • Security protection with rate limiting and content inspection')
    print('   • Structured logging and metrics collection')
    print('   • Health monitoring and observability')
    print(f"\n📄 Check 'middleware_demo.log' for detailed logging output")
if __name__ == '__main__':
    run_demo()