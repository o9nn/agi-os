"""
Progressive rendering utilities for optimizing server-side response generation.

Provides specialized encoders and streaming optimizations for complex DTESN results
to minimize serialization overhead and enable progressive content delivery.
"""

import json
import gzip
import zlib
from typing import Any, Dict, Iterator, Optional, Union
from dataclasses import dataclass
from io import StringIO


@dataclass
class RenderingConfig:
    """Configuration for progressive rendering optimization."""
    
    # Enable progressive JSON streaming
    progressive_json: bool = True
    
    # Maximum chunk size for progressive rendering
    max_chunk_size: int = 8192
    
    # Compression strategy for progressive content
    compression_strategy: str = "adaptive"  # adaptive, gzip, zlib, none
    
    # Enable client-side rendering hints
    enable_rendering_hints: bool = True
    
    # Buffer size for streaming operations
    stream_buffer_size: int = 4096


class ProgressiveJSONEncoder:
    """Optimized JSON encoder for progressive streaming of complex data."""
    
    def __init__(self, config: RenderingConfig):
        self.config = config
        self._buffer = StringIO()
    
    def encode_progressive(self, data: Dict[str, Any], chunk_callback=None) -> Iterator[str]:
        """
        Encode JSON progressively, yielding chunks as they're ready.
        
        Args:
            data: Data to encode
            chunk_callback: Optional callback for chunk processing
            
        Yields:
            JSON chunks ready for streaming
        """
        if not self.config.progressive_json:
            # Fallback to standard encoding
            yield json.dumps(data)
            return
        
        # Start JSON object
        yield "{"
        
        items = list(data.items())
        for i, (key, value) in enumerate(items):
            # Encode key
            key_json = json.dumps(key)
            yield f'"{key}":'
            
            # Handle different value types progressively
            if isinstance(value, (dict, list)) and self._is_complex(value):
                # Stream complex objects in chunks
                yield from self._stream_complex_value(value)
            else:
                # Simple values can be encoded directly
                yield json.dumps(value)
            
            # Add comma if not the last item
            if i < len(items) - 1:
                yield ","
            
            # Yield chunks when buffer is full
            if self._buffer.tell() > self.config.stream_buffer_size:
                chunk = self._flush_buffer()
                if chunk and chunk_callback:
                    chunk_callback(chunk)
                yield chunk
        
        # End JSON object
        yield "}"
    
    def _is_complex(self, value: Any) -> bool:
        """Check if value is complex enough to warrant progressive streaming."""
        if isinstance(value, dict):
            return len(value) > 10 or any(isinstance(v, (dict, list)) for v in value.values())
        elif isinstance(value, list):
            return len(value) > 50 or any(isinstance(item, (dict, list)) for item in value)
        return False
    
    def _stream_complex_value(self, value: Union[Dict, list]) -> Iterator[str]:
        """Stream complex values (dicts/lists) progressively."""
        if isinstance(value, dict):
            yield from self._stream_dict(value)
        elif isinstance(value, list):
            yield from self._stream_list(value)
    
    def _stream_dict(self, data: Dict[str, Any]) -> Iterator[str]:
        """Stream dictionary progressively."""
        yield "{"
        items = list(data.items())
        for i, (key, val) in enumerate(items):
            yield f'{json.dumps(key)}:{json.dumps(val)}'
            if i < len(items) - 1:
                yield ","
        yield "}"
    
    def _stream_list(self, data: list) -> Iterator[str]:
        """Stream list progressively."""
        yield "["
        for i, item in enumerate(data):
            yield json.dumps(item)
            if i < len(data) - 1:
                yield ","
        yield "]"
    
    def _flush_buffer(self) -> str:
        """Flush the internal buffer and return content."""
        content = self._buffer.getvalue()
        self._buffer.seek(0)
        self._buffer.truncate(0)
        return content


class ContentCompressor:
    """Adaptive content compression for optimal delivery performance."""
    
    def __init__(self, config: RenderingConfig):
        self.config = config
    
    def compress_content(self, content: str, content_type: str = "application/json") -> Dict[str, Any]:
        """
        Compress content using the optimal algorithm based on content characteristics.
        
        Args:
            content: Content to compress
            content_type: MIME type of content
            
        Returns:
            Dictionary with compressed data and metadata
        """
        content_bytes = content.encode('utf-8')
        original_size = len(content_bytes)
        
        if original_size < 512:  # Don't compress very small content
            return {
                "data": content,
                "compressed": False,
                "original_size": original_size,
                "method": "none"
            }
        
        # Choose compression method based on strategy and content
        method = self._select_compression_method(content_bytes, content_type)
        
        if method == "none":
            return {
                "data": content,
                "compressed": False,
                "original_size": original_size,
                "method": "none"
            }
        
        # Apply compression
        compressed_data = self._apply_compression(content_bytes, method)
        compression_ratio = len(compressed_data) / original_size
        
        # Return metadata with compressed content
        return {
            "data": compressed_data.hex() if method != "none" else content,
            "compressed": True,
            "original_size": original_size,
            "compressed_size": len(compressed_data),
            "compression_ratio": round(compression_ratio, 3),
            "method": method,
            "encoding": "hex" if method != "none" else "utf-8"
        }
    
    def _select_compression_method(self, content: bytes, content_type: str) -> str:
        """Select optimal compression method based on content characteristics."""
        if self.config.compression_strategy == "none":
            return "none"
        
        content_size = len(content)
        
        if self.config.compression_strategy == "adaptive":
            # For JSON and text, gzip usually works better
            if "json" in content_type.lower() or "text/" in content_type:
                return "gzip" if content_size > 2048 else "zlib"
            # For event streams, use faster compression
            elif "event-stream" in content_type:
                return "zlib"
            else:
                return "gzip"
        
        return self.config.compression_strategy
    
    def _apply_compression(self, content: bytes, method: str) -> bytes:
        """Apply the specified compression method."""
        if method == "gzip":
            return gzip.compress(content, compresslevel=6)
        elif method == "zlib":
            return zlib.compress(content, level=6)
        else:
            return content


class RenderingHints:
    """Generate client-side rendering hints to optimize delivery."""
    
    @staticmethod
    def generate_hints(data_info: Dict[str, Any]) -> Dict[str, str]:
        """
        Generate rendering hints based on data characteristics.
        
        Args:
            data_info: Information about the data being rendered
            
        Returns:
            Dictionary of HTTP headers with rendering hints
        """
        hints = {}
        
        # Content size hints
        if "size" in data_info:
            size = data_info["size"]
            if size > 1024 * 1024:  # > 1MB
                hints["X-Content-Hint"] = "large-dataset"
                hints["X-Progressive-Rendering"] = "recommended"
            elif size > 10240:  # > 10KB
                hints["X-Content-Hint"] = "medium-dataset" 
                hints["X-Progressive-Rendering"] = "optional"
            else:
                hints["X-Content-Hint"] = "small-dataset"
        
        # Complexity hints
        if "complexity" in data_info:
            complexity = data_info["complexity"]
            if complexity == "high":
                hints["X-Parsing-Hint"] = "incremental"
                hints["X-Buffer-Size"] = "8192"
            elif complexity == "medium":
                hints["X-Parsing-Hint"] = "buffered"
                hints["X-Buffer-Size"] = "4096"
            else:
                hints["X-Parsing-Hint"] = "standard"
        
        # Compression hints
        if data_info.get("compressed"):
            hints["X-Compression-Method"] = data_info.get("compression_method", "unknown")
            hints["X-Original-Size"] = str(data_info.get("original_size", 0))
        
        # Progressive delivery hints
        if data_info.get("progressive"):
            hints["X-Progressive-Delivery"] = "true"
            hints["X-Chunk-Boundary"] = data_info.get("chunk_boundary", "\\n")
        
        return hints


def optimize_dtesn_response(dtesn_result: Dict[str, Any], config: RenderingConfig) -> Dict[str, Any]:
    """
    Optimize a DTESN response for efficient server-side delivery.
    
    Args:
        dtesn_result: Raw DTESN processing result
        config: Rendering configuration
        
    Returns:
        Optimized response with compression and rendering hints
    """
    # Analyze data characteristics
    data_size = len(json.dumps(dtesn_result))
    complexity = _analyze_complexity(dtesn_result)
    
    # Apply progressive encoding if beneficial
    if config.progressive_json and data_size > config.max_chunk_size:
        encoder = ProgressiveJSONEncoder(config)
        json_chunks = list(encoder.encode_progressive(dtesn_result))
        progressive_json = "".join(json_chunks)
    else:
        progressive_json = json.dumps(dtesn_result)
    
    # Apply optimal compression
    compressor = ContentCompressor(config)
    compressed_result = compressor.compress_content(progressive_json)
    
    # Generate rendering hints
    data_info = {
        "size": data_size,
        "complexity": complexity,
        "compressed": compressed_result["compressed"],
        "compression_method": compressed_result.get("method"),
        "original_size": compressed_result.get("original_size"),
        "progressive": config.progressive_json and data_size > config.max_chunk_size
    }
    
    rendering_hints = RenderingHints.generate_hints(data_info)
    
    return {
        "content": compressed_result,
        "hints": rendering_hints,
        "metadata": {
            "optimized": True,
            "original_size": data_size,
            "optimization_method": "progressive_rendering_v1"
        }
    }


def _analyze_complexity(data: Any, depth: int = 0) -> str:
    """Analyze data complexity for rendering optimization."""
    if depth > 5:
        return "high"
    
    if isinstance(data, dict):
        if len(data) > 50:
            return "high"
        elif len(data) > 10:
            # Check nested complexity
            for value in data.values():
                if _analyze_complexity(value, depth + 1) == "high":
                    return "high"
            return "medium"
        else:
            return "low"
    
    elif isinstance(data, list):
        if len(data) > 100:
            return "high"
        elif len(data) > 25:
            return "medium"
        else:
            return "low"
    
    return "low"