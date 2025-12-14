"""
Response compression middleware for Aphrodite Engine API server.

Provides intelligent compression strategies to reduce response sizes
and improve network transfer times.
"""

import gzip
import io
from typing import Callable, Optional, Set
from dataclasses import dataclass, field

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.types import ASGIApp


@dataclass
class CompressionConfig:
    """Configuration for compression middleware."""
    
    # Minimum response size to compress (bytes)
    min_size: int = 500
    
    # Compression level (1-9, 9 is highest compression)
    compression_level: int = 6
    
    # Supported compression algorithms in order of preference
    algorithms: list = field(default_factory=lambda: ["gzip", "deflate"])
    
    # Content types to compress
    compressible_types: Set[str] = field(default_factory=lambda: {
        "application/json",
        "text/plain", 
        "text/html",
        "application/javascript",
        "text/css",
        "application/xml",
        "text/xml",
        "text/event-stream"  # Added for SSE streaming support
    })
    
    # Routes to exclude from compression
    exclude_routes: Set[str] = field(default_factory=set)
    
    # Enable streaming compression
    enable_streaming: bool = True
    
    # Enhanced compression settings for large datasets
    large_dataset_threshold: int = 1024 * 1024  # 1MB
    adaptive_compression: bool = True
    
    # Content-aware compression levels
    json_compression_level: int = 7  # Higher for JSON (better compression)
    text_compression_level: int = 6  # Balanced for text
    binary_compression_level: int = 3  # Lower for binary (faster)


class CompressionMiddleware(BaseHTTPMiddleware):
    """Response compression middleware with intelligent algorithm selection."""
    
    def __init__(self, app: ASGIApp, config: CompressionConfig):
        super().__init__(app)
        self.config = config
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Process response through compression layer."""
        
        # Skip compression for certain conditions
        if not self._should_compress_request(request):
            return await call_next(request)
        
        # Get client's accepted encodings
        accepted_encodings = self._get_accepted_encodings(request)
        
        # Select compression algorithm
        compression_algo = self._select_compression_algorithm(accepted_encodings)
        
        if not compression_algo:
            return await call_next(request)
        
        # Execute request
        response = await call_next(request)
        
        # Compress response if eligible
        if self._should_compress_response(response):
            return await self._compress_response(response, compression_algo)
        
        return response
    
    def _should_compress_request(self, request: Request) -> bool:
        """Determine if request should have compressed response."""
        
        # Check excluded routes
        if request.url.path in self.config.exclude_routes:
            return False
        
        # Check if client accepts compressed responses
        accept_encoding = request.headers.get("accept-encoding", "")
        if not any(algo in accept_encoding.lower() 
                  for algo in self.config.algorithms):
            return False
        
        return True
    
    def _get_accepted_encodings(self, request: Request) -> Set[str]:
        """Parse client's accepted encodings."""
        accept_encoding = request.headers.get("accept-encoding", "")
        
        encodings = set()
        for encoding in accept_encoding.lower().split(","):
            encoding = encoding.strip()
            # Handle quality values (e.g., "gzip;q=0.8")
            if ";" in encoding:
                encoding = encoding.split(";")[0].strip()
            encodings.add(encoding)
        
        return encodings
    
    def _select_compression_algorithm(self, accepted_encodings: Set[str]) -> Optional[str]:
        """Select best compression algorithm based on client support."""
        for algo in self.config.algorithms:
            if algo.lower() in accepted_encodings:
                return algo
        return None
    
    def _should_compress_response(self, response: Response) -> bool:
        """Determine if response should be compressed."""
        
        # Check status code (only compress successful responses)
        if response.status_code < 200 or response.status_code >= 300:
            return False
        
        # Check if already compressed
        if response.headers.get("content-encoding"):
            return False
        
        # Check content type
        content_type = response.headers.get("content-type", "")
        if content_type:
            # Extract media type (ignore charset)
            media_type = content_type.split(";")[0].strip()
            if media_type not in self.config.compressible_types:
                return False
        
        # Check if it's a streaming response
        if response.headers.get("transfer-encoding") == "chunked":
            return self.config.enable_streaming
        
        return True
    
    async def _compress_response(self, response: Response, algorithm: str) -> Response:
        """Compress response using specified algorithm with content-aware optimization."""
        
        # Read response body
        body = await self._read_response_body(response)
        
        # Check minimum size
        if len(body) < self.config.min_size:
            return response
        
        # Determine optimal compression level based on content type and size
        compression_level = self._get_optimal_compression_level(response, len(body))
        
        # Compress the body with optimized settings
        if algorithm == "gzip":
            compressed_body = self._gzip_compress(body, compression_level)
        elif algorithm == "deflate":
            compressed_body = self._deflate_compress(body, compression_level)
        else:
            return response  # Unsupported algorithm
        
        # Calculate compression ratio for monitoring
        compression_ratio = len(compressed_body) / len(body)
        
        # Create new response with compressed body
        new_response = Response(
            content=compressed_body,
            status_code=response.status_code,
            headers=dict(response.headers),
            media_type=response.media_type
        )
        
        # Update headers with enhanced information
        new_response.headers["content-encoding"] = algorithm
        new_response.headers["content-length"] = str(len(compressed_body))
        new_response.headers["x-compression-ratio"] = f"{compression_ratio:.3f}"
        new_response.headers["x-original-size"] = str(len(body))
        new_response.headers["x-compression-level"] = str(compression_level)
        
        # Add vary header to indicate compression varies by encoding
        vary_header = response.headers.get("vary", "")
        if "accept-encoding" not in vary_header.lower():
            if vary_header:
                vary_header += ", Accept-Encoding"
            else:
                vary_header = "Accept-Encoding"
            new_response.headers["vary"] = vary_header
        
        return new_response
    
    def _get_optimal_compression_level(self, response: Response, size: int) -> int:
        """Determine optimal compression level based on content type and size."""
        if not self.config.adaptive_compression:
            return self.config.compression_level
        
        content_type = response.headers.get("content-type", "")
        
        # For large datasets, use more aggressive compression
        if size >= self.config.large_dataset_threshold:
            if "json" in content_type.lower():
                return min(self.config.json_compression_level + 1, 9)
            else:
                return min(self.config.compression_level + 1, 9)
        
        # Content-type specific optimization
        if "json" in content_type.lower():
            return self.config.json_compression_level
        elif "text/" in content_type.lower():
            return self.config.text_compression_level
        elif "event-stream" in content_type.lower():
            # Streaming responses need faster compression
            return max(self.config.compression_level - 2, 1)
        else:
            return self.config.compression_level
    
    async def _read_response_body(self, response: Response) -> bytes:
        """Read response body as bytes."""
        if hasattr(response, 'body_iterator'):
            # For streaming responses
            chunks = []
            async for chunk in response.body_iterator:
                chunks.append(chunk)
            return b"".join(chunks)
        elif hasattr(response, 'body'):
            # For regular responses
            if isinstance(response.body, bytes):
                return response.body
            elif isinstance(response.body, str):
                return response.body.encode('utf-8')
        
        return b""
    
    def _gzip_compress(self, data: bytes, compression_level: Optional[int] = None) -> bytes:
        """Compress data using gzip with optimized settings."""
        level = compression_level or self.config.compression_level
        buffer = io.BytesIO()
        with gzip.GzipFile(fileobj=buffer, mode='wb', compresslevel=level) as gz_file:
            gz_file.write(data)
        return buffer.getvalue()
    
    def _deflate_compress(self, data: bytes, compression_level: Optional[int] = None) -> bytes:
        """Compress data using deflate (zlib) with optimized settings."""
        import zlib
        level = compression_level or self.config.compression_level
        return zlib.compress(data, level=level)
        
    def get_compression_stats(self) -> Dict[str, Any]:
        """Get compression statistics for monitoring."""
        return {
            "config": {
                "min_size": self.config.min_size,
                "default_level": self.config.compression_level,
                "adaptive": self.config.adaptive_compression,
                "large_threshold": self.config.large_dataset_threshold
            }
        }