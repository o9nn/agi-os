import asyncio
import time
from typing import Any, Callable, Dict, Set
from dataclasses import dataclass, field
from collections import defaultdict
from fastapi import Request, Response, HTTPException
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.types import ASGIApp
@dataclass
class RateLimitConfig:
    requests_per_minute: int = 60
    burst_size: int = 10
    route_limits: Dict[str, int] = field(default_factory=lambda: {'/v1/chat/completions': 30, '/v1/completions': 30, '/v1/embeddings': 120, '/health': 300})
@dataclass
class PreprocessingConfig:
    enable_validation: bool = True
    enable_rate_limiting: bool = True
    rate_limit: RateLimitConfig = field(default_factory=RateLimitConfig)
    max_body_size: int = 1024 * 1024
    request_timeout: float = 30.0
    exclude_routes: Set[str] = field(default_factory=lambda: {'/health', '/metrics'})
    enable_size_optimization: bool = True
class RateLimiter:
    def __init__(self, config: RateLimitConfig):
        self.config = config
        self._buckets: Dict[str, Dict[str, Any]] = defaultdict(lambda: {'tokens': config.burst_size, 'last_refill': time.time()})
    async def is_allowed(self, client_id: str, route: str=None) -> bool:
        limit = self.config.route_limits.get(route, self.config.requests_per_minute)
        bucket = self._buckets[client_id]
        now = time.time()
        time_passed = now - bucket['last_refill']
        bucket['last_refill'] = now
        tokens_to_add = time_passed * (limit / 60.0)
        bucket['tokens'] = min(self.config.burst_size, bucket['tokens'] + tokens_to_add)
        if bucket['tokens'] >= 1.0:
            bucket['tokens'] -= 1.0
            return True
        return False
    def cleanup_old_entries(self, max_age: float=3600.0):
        now = time.time()
        to_remove = []
        for client_id, bucket in self._buckets.items():
            if now - bucket['last_refill'] > max_age:
                to_remove.append(client_id)
        for client_id in to_remove:
            del self._buckets[client_id]
class PreprocessingMiddleware(BaseHTTPMiddleware):
    def __init__(self, app: ASGIApp, config: PreprocessingConfig):
        super().__init__(app)
        self.config = config
        if config.enable_rate_limiting:
            self.rate_limiter = RateLimiter(config.rate_limit)
        else:
            self.rate_limiter = None
        if self.rate_limiter:
            asyncio.create_task(self._cleanup_task())
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        if request.url.path in self.config.exclude_routes:
            return await call_next(request)
        try:
            if self.config.enable_validation:
                await self._validate_request_size(request)
            if self.config.enable_rate_limiting and self.rate_limiter:
                await self._check_rate_limit(request)
            if self.config.enable_size_optimization:
                await self._optimize_request(request)
            return await asyncio.wait_for(call_next(request), timeout=self.config.request_timeout)
        except asyncio.TimeoutError:
            raise HTTPException(status_code=408, detail='Request timeout')
        except HTTPException:
            raise
        except Exception as e:
            raise HTTPException(status_code=500, detail=f'Request preprocessing error: {str(e)}')
    async def _validate_request_size(self, request: Request) -> None:
        content_length = request.headers.get('content-length')
        if content_length:
            size = int(content_length)
            if size > self.config.max_body_size:
                raise HTTPException(status_code=413, detail=f'Request body too large: {size} bytes > {self.config.max_body_size} bytes')
    async def _check_rate_limit(self, request: Request) -> None:
        client_ip = self._get_client_ip(request)
        is_allowed = await self.rate_limiter.is_allowed(client_ip, request.url.path)
        if not is_allowed:
            raise HTTPException(status_code=429, detail='Rate limit exceeded', headers={'Retry-After': '60'})
    async def _optimize_request(self, request: Request) -> None:
        if request.method == 'POST':
            content_type = request.headers.get('content-type', '')
            if 'application/json' in content_type:
                try:
                    body = await request.body()
                    if body:
                        import json
                        json.loads(body.decode())
                except (json.JSONDecodeError, UnicodeDecodeError):
                    raise HTTPException(status_code=400, detail='Invalid JSON in request body')
    def _get_client_ip(self, request: Request) -> str:
        forwarded_for = request.headers.get('x-forwarded-for')
        if forwarded_for:
            return forwarded_for.split(',')[0].strip()
        real_ip = request.headers.get('x-real-ip')
        if real_ip:
            return real_ip
        client = getattr(request, 'client', None)
        if client:
            return client.host
        return 'unknown'
    async def _cleanup_task(self) -> None:
        while True:
            try:
                await asyncio.sleep(3600)
                if self.rate_limiter:
                    self.rate_limiter.cleanup_old_entries()
            except Exception:
                pass