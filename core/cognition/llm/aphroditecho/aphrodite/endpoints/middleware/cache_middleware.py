"""
Route-specific caching middleware for Aphrodite Engine API server.

Implements intelligent caching strategies for different endpoint types
to minimize response latency and improve throughput.
"""

import hashlib
import json
import time
from typing import Any, Callable, Dict, Optional
from dataclasses import dataclass, field

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.types import ASGIApp


@dataclass
class CacheConfig:
    """Configuration for caching middleware."""
    
    # Cache backend type: 'memory' or 'redis'
    backend: str = "memory"
    
    # Default TTL in seconds
    default_ttl: int = 300
    
    # Maximum cache size for memory backend
    max_cache_size: int = 1000
    
    # Route-specific TTL configuration
    route_ttl: Dict[str, int] = field(default_factory=lambda: {
        "/v1/models": 3600,          # Model list rarely changes
        "/v1/chat/completions": 60,   # Short TTL for dynamic responses
        "/v1/completions": 60,        # Short TTL for dynamic responses
        "/v1/embeddings": 300,        # Medium TTL for embeddings
        "/health": 30                 # Short TTL for health checks
    })
    
    # Routes to exclude from caching
    exclude_routes: set = field(default_factory=lambda: {
        "/v1/chat/completions",  # Dynamic content, but can cache certain patterns
        "/v1/completions"        # Dynamic content
    })
    
    # Cache only GET requests by default
    cache_methods: set = field(default_factory=lambda: {"GET"})
    
    # Enable cache for POST with deterministic inputs
    cache_deterministic_posts: bool = True


class MemoryCache:
    """In-memory cache implementation with LRU eviction."""
    
    def __init__(self, max_size: int = 1000):
        self.max_size = max_size
        self._cache: Dict[str, Dict[str, Any]] = {}
        self._access_times: Dict[str, float] = {}
    
    def get(self, key: str) -> Optional[Any]:
        """Get value from cache."""
        if key in self._cache:
            entry = self._cache[key]
            
            # Check if expired
            if time.time() > entry["expires_at"]:
                self._remove(key)
                return None
            
            # Update access time
            self._access_times[key] = time.time()
            return entry["value"]
        return None
    
    def set(self, key: str, value: Any, ttl: int) -> None:
        """Set value in cache with TTL."""
        # Evict if at max size
        if len(self._cache) >= self.max_size and key not in self._cache:
            self._evict_lru()
        
        expires_at = time.time() + ttl
        self._cache[key] = {
            "value": value,
            "expires_at": expires_at
        }
        self._access_times[key] = time.time()
    
    def delete(self, key: str) -> None:
        """Remove key from cache."""
        self._remove(key)
    
    def clear(self) -> None:
        """Clear all cached entries."""
        self._cache.clear()
        self._access_times.clear()
    
    def _remove(self, key: str) -> None:
        """Remove key from internal structures."""
        self._cache.pop(key, None)
        self._access_times.pop(key, None)
    
    def _evict_lru(self) -> None:
        """Evict least recently used entry."""
        if not self._access_times:
            return
        
        lru_key = min(self._access_times.keys(), 
                     key=lambda k: self._access_times[k])
        self._remove(lru_key)


class CacheMiddleware(BaseHTTPMiddleware):
    """Route-specific caching middleware."""
    
    def __init__(self, app: ASGIApp, config: CacheConfig):
        super().__init__(app)
        self.config = config
        
        # Initialize cache backend
        if config.backend == "memory":
            self.cache = MemoryCache(config.max_cache_size)
        else:
            raise ValueError(f"Unsupported cache backend: {config.backend}")
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Process request through caching layer."""
        
        # Skip caching for certain conditions
        if not self._should_cache_request(request):
            return await call_next(request)
        
        # Generate cache key
        cache_key = await self._generate_cache_key(request)
        
        # Try to get from cache
        cached_response = self.cache.get(cache_key)
        if cached_response is not None:
            return self._create_response_from_cache(cached_response)
        
        # Execute request
        response = await call_next(request)
        
        # Cache successful responses
        if self._should_cache_response(response):
            ttl = self._get_ttl_for_route(request.url.path)
            cached_data = await self._serialize_response(response)
            self.cache.set(cache_key, cached_data, ttl)
        
        return response
    
    def _should_cache_request(self, request: Request) -> bool:
        """Determine if request should be cached."""
        
        # Check HTTP method
        if request.method not in self.config.cache_methods:
            # Special handling for deterministic POST requests
            if (request.method == "POST" and 
                self.config.cache_deterministic_posts and
                self._is_deterministic_request(request)):
                return True
            return False
        
        # Check excluded routes
        if request.url.path in self.config.exclude_routes:
            return False
        
        return True
    
    def _is_deterministic_request(self, request: Request) -> bool:
        """Check if POST request has deterministic parameters."""
        path = request.url.path
        
        # For embeddings, cache if no streaming and same model
        if path == "/v1/embeddings":
            return True
        
        # For completions, only cache if temperature=0 and deterministic
        if path in ["/v1/chat/completions", "/v1/completions"]:
            # This would need to inspect request body, but for now be conservative
            return False
        
        return False
    
    async def _generate_cache_key(self, request: Request) -> str:
        """Generate cache key for request."""
        key_components = [
            request.method,
            request.url.path,
            str(sorted(request.query_params.items()))
        ]
        
        # Include request body for POST requests
        if request.method == "POST":
            body = await request.body()
            if body:
                # Parse JSON to normalize format
                try:
                    body_json = json.loads(body.decode())
                    # Sort keys for consistent hashing
                    normalized_body = json.dumps(body_json, sort_keys=True)
                    key_components.append(normalized_body)
                except (json.JSONDecodeError, UnicodeDecodeError):
                    # Fall back to raw body hash
                    key_components.append(hashlib.md5(body).hexdigest())
        
        # Include relevant headers
        auth_header = request.headers.get("Authorization")
        if auth_header:
            key_components.append(f"auth:{hashlib.md5(auth_header.encode()).hexdigest()}")
        
        key_string = "|".join(key_components)
        return hashlib.sha256(key_string.encode()).hexdigest()
    
    def _should_cache_response(self, response: Response) -> bool:
        """Determine if response should be cached."""
        # Only cache successful responses
        if response.status_code != 200:
            return False
        
        # Don't cache streaming responses
        if response.headers.get("content-type", "").startswith("text/event-stream"):
            return False
        
        return True
    
    def _get_ttl_for_route(self, path: str) -> int:
        """Get TTL for specific route."""
        return self.config.route_ttl.get(path, self.config.default_ttl)
    
    async def _serialize_response(self, response: Response) -> Dict[str, Any]:
        """Serialize response for caching."""
        # Read response body
        body = b""
        if hasattr(response, 'body_iterator'):
            chunks = []
            async for chunk in response.body_iterator:
                chunks.append(chunk)
            body = b"".join(chunks)
        elif hasattr(response, 'body'):
            body = response.body
        
        return {
            "status_code": response.status_code,
            "headers": dict(response.headers),
            "body": body,
            "media_type": response.media_type
        }
    
    def _create_response_from_cache(self, cached_data: Dict[str, Any]) -> Response:
        """Create Response object from cached data."""
        return Response(
            content=cached_data["body"],
            status_code=cached_data["status_code"],
            headers=cached_data["headers"],
            media_type=cached_data.get("media_type")
        )