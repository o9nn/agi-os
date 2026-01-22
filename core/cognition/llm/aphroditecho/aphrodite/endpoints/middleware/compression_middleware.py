import gzip
import io
from typing import Callable, Optional, Set
from dataclasses import dataclass, field
from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.types import ASGIApp
@dataclass
class CompressionConfig:
    min_size: int = 500
    compression_level: int = 6
    algorithms: list = field(default_factory=lambda: ['gzip', 'deflate'])
    compressible_types: Set[str] = field(default_factory=lambda: {'application/json', 'text/plain', 'text/html', 'application/javascript', 'text/css', 'application/xml', 'text/xml', 'text/event-stream'})
    exclude_routes: Set[str] = field(default_factory=set)
    enable_streaming: bool = True
    large_dataset_threshold: int = 1024 * 1024
    adaptive_compression: bool = True
    json_compression_level: int = 7
    text_compression_level: int = 6
    binary_compression_level: int = 3
class CompressionMiddleware(BaseHTTPMiddleware):
    def __init__(self, app: ASGIApp, config: CompressionConfig):
        super().__init__(app)
        self.config = config
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        if not self._should_compress_request(request):
            return await call_next(request)
        accepted_encodings = self._get_accepted_encodings(request)
        compression_algo = self._select_compression_algorithm(accepted_encodings)
        if not compression_algo:
            return await call_next(request)
        response = await call_next(request)
        if self._should_compress_response(response):
            return await self._compress_response(response, compression_algo)
        return response
    def _should_compress_request(self, request: Request) -> bool:
        if request.url.path in self.config.exclude_routes:
            return False
        accept_encoding = request.headers.get('accept-encoding', '')
        if not any((algo in accept_encoding.lower() for algo in self.config.algorithms)):
            return False
        return True
    def _get_accepted_encodings(self, request: Request) -> Set[str]:
        accept_encoding = request.headers.get('accept-encoding', '')
        encodings = set()
        for encoding in accept_encoding.lower().split(','):
            encoding = encoding.strip()
            if ';' in encoding:
                encoding = encoding.split(';')[0].strip()
            encodings.add(encoding)
        return encodings
    def _select_compression_algorithm(self, accepted_encodings: Set[str]) -> Optional[str]:
        for algo in self.config.algorithms:
            if algo.lower() in accepted_encodings:
                return algo
        return None
    def _should_compress_response(self, response: Response) -> bool:
        if response.status_code < 200 or response.status_code >= 300:
            return False
        if response.headers.get('content-encoding'):
            return False
        content_type = response.headers.get('content-type', '')
        if content_type:
            media_type = content_type.split(';')[0].strip()
            if media_type not in self.config.compressible_types:
                return False
        if response.headers.get('transfer-encoding') == 'chunked':
            return self.config.enable_streaming
        return True
    async def _compress_response(self, response: Response, algorithm: str) -> Response:
        body = await self._read_response_body(response)
        if len(body) < self.config.min_size:
            return response
        compression_level = self._get_optimal_compression_level(response, len(body))
        if algorithm == 'gzip':
            compressed_body = self._gzip_compress(body, compression_level)
        elif algorithm == 'deflate':
            compressed_body = self._deflate_compress(body, compression_level)
        else:
            return response
        compression_ratio = len(compressed_body) / len(body)
        new_response = Response(content=compressed_body, status_code=response.status_code, headers=dict(response.headers), media_type=response.media_type)
        new_response.headers['content-encoding'] = algorithm
        new_response.headers['content-length'] = str(len(compressed_body))
        new_response.headers['x-compression-ratio'] = f'{compression_ratio:.3f}'
        new_response.headers['x-original-size'] = str(len(body))
        new_response.headers['x-compression-level'] = str(compression_level)
        vary_header = response.headers.get('vary', '')
        if 'accept-encoding' not in vary_header.lower():
            if vary_header:
                vary_header += ', Accept-Encoding'
            else:
                vary_header = 'Accept-Encoding'
            new_response.headers['vary'] = vary_header
        return new_response
    def _get_optimal_compression_level(self, response: Response, size: int) -> int:
        if not self.config.adaptive_compression:
            return self.config.compression_level
        content_type = response.headers.get('content-type', '')
        if size >= self.config.large_dataset_threshold:
            if 'json' in content_type.lower():
                return min(self.config.json_compression_level + 1, 9)
            else:
                return min(self.config.compression_level + 1, 9)
        if 'json' in content_type.lower():
            return self.config.json_compression_level
        elif 'text/' in content_type.lower():
            return self.config.text_compression_level
        elif 'event-stream' in content_type.lower():
            return max(self.config.compression_level - 2, 1)
        else:
            return self.config.compression_level
    async def _read_response_body(self, response: Response) -> bytes:
        if hasattr(response, 'body_iterator'):
            chunks = []
            async for chunk in response.body_iterator:
                chunks.append(chunk)
            return b''.join(chunks)
        elif hasattr(response, 'body'):
            if isinstance(response.body, bytes):
                return response.body
            elif isinstance(response.body, str):
                return response.body.encode('utf-8')
        return b''
    def _gzip_compress(self, data: bytes, compression_level: Optional[int]=None) -> bytes:
        level = compression_level or self.config.compression_level
        buffer = io.BytesIO()
        with gzip.GzipFile(fileobj=buffer, mode='wb', compresslevel=level) as gz_file:
            gz_file.write(data)
        return buffer.getvalue()
    def _deflate_compress(self, data: bytes, compression_level: Optional[int]=None) -> bytes:
        import zlib
        level = compression_level or self.config.compression_level
        return zlib.compress(data, level=level)
    def get_compression_stats(self) -> Dict[str, Any]:
        return {'config': {'min_size': self.config.min_size, 'default_level': self.config.compression_level, 'adaptive': self.config.adaptive_compression, 'large_threshold': self.config.large_dataset_threshold}}