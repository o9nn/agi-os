"""
Tests for progressive rendering and streaming response optimization.

Validates the enhanced server-side streaming capabilities for large datasets
with progressive rendering, adaptive compression, and bandwidth optimization.
"""

import json
import time
import pytest
from typing import Dict, Any
from unittest.mock import AsyncMock, MagicMock

from aphrodite.endpoints.deep_tree_echo.progressive_renderer import (
    ProgressiveJSONEncoder,
    ContentCompressor,
    RenderingConfig,
    RenderingHints,
    optimize_dtesn_response,
    _analyze_complexity
)


class TestProgressiveJSONEncoder:
    """Test progressive JSON encoding functionality."""
    
    def test_simple_data_encoding(self):
        """Test encoding simple data structures."""
        config = RenderingConfig(progressive_json=True, max_chunk_size=1024)
        encoder = ProgressiveJSONEncoder(config)
        
        simple_data = {"key": "value", "number": 42}
        chunks = list(encoder.encode_progressive(simple_data))
        
        # Reconstruct JSON
        result = "".join(chunks)
        parsed = json.loads(result)
        
        assert parsed == simple_data
        assert len(chunks) >= 3  # Should have multiple chunks
    
    def test_complex_data_progressive_encoding(self):
        """Test progressive encoding of complex data structures."""
        config = RenderingConfig(progressive_json=True, max_chunk_size=512)
        encoder = ProgressiveJSONEncoder(config)
        
        complex_data = {
            "metadata": {"id": 1, "timestamp": "2024-01-01"},
            "results": [{"value": i, "processed": True} for i in range(20)],
            "summary": {"total": 20, "success": True}
        }
        
        chunks = list(encoder.encode_progressive(complex_data))
        result = "".join(chunks)
        parsed = json.loads(result)
        
        assert parsed == complex_data
        assert len(chunks) > 5  # Complex data should generate more chunks
    
    def test_fallback_to_standard_encoding(self):
        """Test fallback when progressive JSON is disabled."""
        config = RenderingConfig(progressive_json=False)
        encoder = ProgressiveJSONEncoder(config)
        
        data = {"test": "data"}
        chunks = list(encoder.encode_progressive(data))
        
        assert len(chunks) == 1
        assert json.loads(chunks[0]) == data


class TestContentCompressor:
    """Test adaptive content compression functionality."""
    
    def test_small_content_no_compression(self):
        """Test that small content is not compressed."""
        config = RenderingConfig(compression_strategy="adaptive")
        compressor = ContentCompressor(config)
        
        small_content = "small"
        result = compressor.compress_content(small_content)
        
        assert result["compressed"] == False
        assert result["method"] == "none"
        assert result["data"] == small_content
    
    def test_json_content_compression(self):
        """Test compression of JSON content."""
        config = RenderingConfig(compression_strategy="adaptive")
        compressor = ContentCompressor(config)
        
        json_content = json.dumps({"data": "x" * 1000})  # Large enough to compress
        result = compressor.compress_content(json_content, "application/json")
        
        assert result["compressed"] == True
        assert result["method"] in ["gzip", "zlib"]
        assert result["compression_ratio"] < 1.0
        assert result["original_size"] == len(json_content.encode('utf-8'))
    
    def test_compression_strategy_selection(self):
        """Test compression method selection based on content type and size."""
        config = RenderingConfig(compression_strategy="adaptive")
        compressor = ContentCompressor(config)
        
        # Large JSON should use gzip
        large_json = json.dumps({"data": "x" * 5000})
        result = compressor.compress_content(large_json, "application/json")
        assert result["method"] == "gzip"
        
        # Event stream should use zlib
        stream_data = "data: " + "x" * 1000
        result = compressor.compress_content(stream_data, "text/event-stream")
        assert result["method"] == "zlib"
    
    def test_no_compression_strategy(self):
        """Test when compression is disabled."""
        config = RenderingConfig(compression_strategy="none")
        compressor = ContentCompressor(config)
        
        content = "x" * 1000
        result = compressor.compress_content(content)
        
        assert result["compressed"] == False
        assert result["method"] == "none"


class TestRenderingHints:
    """Test rendering hints generation."""
    
    def test_size_based_hints(self):
        """Test hint generation based on content size."""
        # Large dataset
        large_hints = RenderingHints.generate_hints({"size": 2 * 1024 * 1024})  # 2MB
        assert large_hints["X-Content-Hint"] == "large-dataset"
        assert large_hints["X-Progressive-Rendering"] == "recommended"
        
        # Medium dataset
        medium_hints = RenderingHints.generate_hints({"size": 50 * 1024})  # 50KB
        assert medium_hints["X-Content-Hint"] == "medium-dataset"
        assert medium_hints["X-Progressive-Rendering"] == "optional"
        
        # Small dataset
        small_hints = RenderingHints.generate_hints({"size": 1024})  # 1KB
        assert small_hints["X-Content-Hint"] == "small-dataset"
    
    def test_complexity_based_hints(self):
        """Test hint generation based on data complexity."""
        high_complexity_hints = RenderingHints.generate_hints({"complexity": "high"})
        assert high_complexity_hints["X-Parsing-Hint"] == "incremental"
        assert high_complexity_hints["X-Buffer-Size"] == "8192"
        
        medium_complexity_hints = RenderingHints.generate_hints({"complexity": "medium"})
        assert medium_complexity_hints["X-Parsing-Hint"] == "buffered"
        assert medium_complexity_hints["X-Buffer-Size"] == "4096"
    
    def test_compression_hints(self):
        """Test hints for compressed content."""
        compressed_hints = RenderingHints.generate_hints({
            "compressed": True,
            "compression_method": "gzip",
            "original_size": 1024
        })
        
        assert compressed_hints["X-Compression-Method"] == "gzip"
        assert compressed_hints["X-Original-Size"] == "1024"
    
    def test_progressive_delivery_hints(self):
        """Test hints for progressive delivery."""
        progressive_hints = RenderingHints.generate_hints({
            "progressive": True,
            "chunk_boundary": "\n\n"
        })
        
        assert progressive_hints["X-Progressive-Delivery"] == "true"
        assert progressive_hints["X-Chunk-Boundary"] == "\n\n"


class TestComplexityAnalysis:
    """Test data complexity analysis for optimization decisions."""
    
    def test_simple_data_complexity(self):
        """Test complexity analysis of simple data."""
        simple_dict = {"a": 1, "b": 2}
        simple_list = [1, 2, 3]
        
        assert _analyze_complexity(simple_dict) == "low"
        assert _analyze_complexity(simple_list) == "low"
    
    def test_medium_complexity_data(self):
        """Test complexity analysis of medium complexity data."""
        medium_dict = {f"key_{i}": i for i in range(20)}
        medium_list = [{"item": i} for i in range(30)]
        
        assert _analyze_complexity(medium_dict) == "medium"
        assert _analyze_complexity(medium_list) == "medium"
    
    def test_high_complexity_data(self):
        """Test complexity analysis of high complexity data."""
        high_dict = {f"key_{i}": {"nested": {"deep": i}} for i in range(100)}
        high_list = [{"item": i, "data": [j for j in range(10)]} for i in range(200)]
        
        assert _analyze_complexity(high_dict) == "high"
        assert _analyze_complexity(high_list) == "high"
    
    def test_deep_nesting_complexity(self):
        """Test complexity analysis with deep nesting."""
        deep_data = {"level1": {"level2": {"level3": {"level4": {"level5": {"level6": "deep"}}}}}}
        
        assert _analyze_complexity(deep_data) == "high"


class TestDTESNResponseOptimization:
    """Test end-to-end DTESN response optimization."""
    
    def test_small_response_optimization(self):
        """Test optimization of small DTESN responses."""
        config = RenderingConfig(progressive_json=True, max_chunk_size=4096)
        
        small_result = {
            "result": {"output": "test", "status": "success"},
            "metadata": {"processed": True}
        }
        
        optimized = optimize_dtesn_response(small_result, config)
        
        assert optimized["metadata"]["optimized"] == True
        assert optimized["content"]["compressed"] == False  # Too small to compress
        assert "hints" in optimized
    
    def test_large_response_optimization(self):
        """Test optimization of large DTESN responses."""
        config = RenderingConfig(
            progressive_json=True,
            max_chunk_size=2048,
            compression_strategy="adaptive"
        )
        
        # Create large response
        large_result = {
            "result": {
                "output": "x" * 5000,
                "details": [{"item": i, "data": "y" * 100} for i in range(100)]
            },
            "metadata": {"processed": True, "chunks": 100}
        }
        
        optimized = optimize_dtesn_response(large_result, config)
        
        assert optimized["metadata"]["optimized"] == True
        assert optimized["content"]["compressed"] == True
        assert optimized["content"]["compression_ratio"] < 1.0
        assert optimized["hints"]["X-Content-Hint"] == "large-dataset"
        assert optimized["hints"]["X-Progressive-Rendering"] == "recommended"
    
    def test_complex_response_optimization(self):
        """Test optimization of complex nested DTESN responses."""
        config = RenderingConfig(progressive_json=True, compression_strategy="gzip")
        
        complex_result = {
            "dtesn_output": {
                "membrane_results": [
                    {
                        "depth": i,
                        "computations": [{"op": j, "result": f"output_{i}_{j}"} for j in range(10)],
                        "metadata": {"processing_time": i * 0.1}
                    } for i in range(20)
                ]
            },
            "performance": {"total_time": 2.5, "memory_usage": 1024},
            "configuration": {"membrane_depth": 5, "esn_size": 256}
        }
        
        optimized = optimize_dtesn_response(complex_result, config)
        
        assert optimized["metadata"]["optimized"] == True
        assert optimized["hints"]["X-Parsing-Hint"] == "incremental"
        assert "X-Progressive-Delivery" in optimized["hints"]
        
        # Verify content is properly compressed
        if optimized["content"]["compressed"]:
            assert optimized["content"]["method"] == "gzip"
            assert optimized["content"]["compression_ratio"] < 0.8  # Should achieve good compression


@pytest.mark.asyncio
class TestStreamingPerformanceIntegration:
    """Integration tests for streaming performance optimization."""
    
    async def test_bandwidth_aware_streaming_configuration(self):
        """Test streaming configuration adapts to bandwidth hints."""
        # Test different bandwidth configurations
        bandwidth_configs = ["low", "medium", "high", "auto"]
        
        for bandwidth in bandwidth_configs:
            # Import functions from routes module
            from aphrodite.endpoints.deep_tree_echo.routes import (
                _get_optimal_chunk_size,
                _get_compression_level,
                _estimate_throughput
            )
            
            chunk_size = _get_optimal_chunk_size(bandwidth)
            compression_level = _get_compression_level(bandwidth, 1024*1024)  # 1MB
            throughput = _estimate_throughput(bandwidth)
            
            # Verify configurations make sense
            assert chunk_size > 0
            assert 1 <= compression_level <= 9
            assert throughput > 0
            
            # Low bandwidth should have smaller chunks and higher compression
            if bandwidth == "low":
                assert chunk_size <= 1024
                assert compression_level >= 6
            # High bandwidth should have larger chunks and lower compression
            elif bandwidth == "high":
                assert chunk_size >= 8192
                assert compression_level <= 4
    
    async def test_progressive_rendering_integration(self):
        """Test integration of progressive rendering with streaming."""
        config = RenderingConfig(
            progressive_json=True,
            max_chunk_size=1024,
            compression_strategy="adaptive",
            enable_rendering_hints=True
        )
        
        encoder = ProgressiveJSONEncoder(config)
        
        # Simulate streaming a complex DTESN result
        dtesn_result = {
            "request_id": "test_123",
            "results": [{"chunk": i, "data": f"result_{i}" * 50} for i in range(10)],
            "metadata": {"total_chunks": 10, "processing_complete": True}
        }
        
        # Progressive encoding should work
        chunks = list(encoder.encode_progressive(dtesn_result))
        assert len(chunks) > 1
        
        # Reconstructed data should match original
        reconstructed = json.loads("".join(chunks))
        assert reconstructed == dtesn_result
    
    async def test_compression_efficiency_measurement(self):
        """Test measurement of compression efficiency for different content types."""
        config = RenderingConfig(compression_strategy="adaptive")
        compressor = ContentCompressor(config)
        
        # Test various content types
        test_cases = [
            ("application/json", json.dumps({"data": "x" * 1000})),
            ("text/plain", "This is plain text content. " * 100),
            ("text/event-stream", "data: " + json.dumps({"event": "test"}) + "\n\n" * 50)
        ]
        
        for content_type, content in test_cases:
            result = compressor.compress_content(content, content_type)
            
            if result["compressed"]:
                # Verify compression actually reduces size
                assert result["compressed_size"] < result["original_size"]
                assert 0.1 < result["compression_ratio"] < 0.9  # Reasonable compression ratio
                assert result["method"] in ["gzip", "zlib"]


if __name__ == "__main__":
    pytest.main([__file__])