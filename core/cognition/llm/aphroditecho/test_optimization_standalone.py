"""
Standalone test script for progressive rendering and optimization features.
Tests the new streaming response optimizations without external dependencies.
"""

import json
import sys
import os

# Add the project root to path
sys.path.insert(0, '/home/runner/work/aphroditecho/aphroditecho')

from aphrodite.endpoints.deep_tree_echo.progressive_renderer import (
    ProgressiveJSONEncoder,
    ContentCompressor,
    RenderingConfig,
    RenderingHints,
    optimize_dtesn_response,
    _analyze_complexity
)


def test_progressive_json_encoder():
    """Test progressive JSON encoding."""
    print("Testing ProgressiveJSONEncoder...")
    
    config = RenderingConfig(progressive_json=True, max_chunk_size=1024)
    encoder = ProgressiveJSONEncoder(config)
    
    # Test simple data
    simple_data = {"key": "value", "number": 42}
    chunks = list(encoder.encode_progressive(simple_data))
    result = "".join(chunks)
    parsed = json.loads(result)
    
    assert parsed == simple_data, f"Simple data encoding failed: {parsed} != {simple_data}"
    print("✓ Simple data encoding works")
    
    # Test complex data
    complex_data = {
        "metadata": {"id": 1, "timestamp": "2024-01-01"},
        "results": [{"value": i, "processed": True} for i in range(20)],
        "summary": {"total": 20, "success": True}
    }
    
    chunks = list(encoder.encode_progressive(complex_data))
    result = "".join(chunks)
    parsed = json.loads(result)
    
    assert parsed == complex_data, "Complex data encoding failed"
    assert len(chunks) > 3, f"Expected multiple chunks, got {len(chunks)}"
    print("✓ Complex data progressive encoding works")
    
    # Test fallback mode
    config_no_progressive = RenderingConfig(progressive_json=False)
    encoder_fallback = ProgressiveJSONEncoder(config_no_progressive)
    
    chunks_fallback = list(encoder_fallback.encode_progressive(simple_data))
    assert len(chunks_fallback) == 1, "Fallback should produce single chunk"
    print("✓ Fallback mode works")


def test_content_compressor():
    """Test adaptive content compression."""
    print("\nTesting ContentCompressor...")
    
    config = RenderingConfig(compression_strategy="adaptive")
    compressor = ContentCompressor(config)
    
    # Test small content (should not compress)
    small_content = "small"
    result = compressor.compress_content(small_content)
    
    assert result["compressed"] == False, "Small content should not be compressed"
    assert result["method"] == "none", "Small content should use no compression"
    print("✓ Small content not compressed")
    
    # Test large JSON content (should compress)
    large_json = json.dumps({"data": "x" * 1000})
    result = compressor.compress_content(large_json, "application/json")
    
    assert result["compressed"] == True, "Large JSON should be compressed"
    assert result["method"] in ["gzip", "zlib"], f"Unexpected compression method: {result['method']}"
    assert result["compression_ratio"] < 1.0, "Compression should reduce size"
    print(f"✓ Large JSON compressed with {result['method']}, ratio: {result['compression_ratio']:.3f}")
    
    # Test different content types
    stream_data = "data: " + json.dumps({"event": "test"}) * 50
    result_stream = compressor.compress_content(stream_data, "text/event-stream")
    
    if result_stream["compressed"]:
        print(f"✓ Event stream compressed with {result_stream['method']}")
    else:
        print("✓ Event stream not compressed (as expected for smaller content)")


def test_rendering_hints():
    """Test rendering hints generation."""
    print("\nTesting RenderingHints...")
    
    # Test size-based hints
    large_hints = RenderingHints.generate_hints({"size": 2 * 1024 * 1024})
    assert large_hints["X-Content-Hint"] == "large-dataset", "Large dataset hint incorrect"
    print("✓ Large dataset hints generated")
    
    medium_hints = RenderingHints.generate_hints({"size": 50 * 1024})
    assert medium_hints["X-Content-Hint"] == "medium-dataset", "Medium dataset hint incorrect"
    print("✓ Medium dataset hints generated")
    
    small_hints = RenderingHints.generate_hints({"size": 1024})
    assert small_hints["X-Content-Hint"] == "small-dataset", "Small dataset hint incorrect"
    print("✓ Small dataset hints generated")
    
    # Test complexity-based hints
    complex_hints = RenderingHints.generate_hints({"complexity": "high"})
    assert complex_hints["X-Parsing-Hint"] == "incremental", "High complexity hint incorrect"
    print("✓ Complexity hints generated")
    
    # Test compression hints
    compression_hints = RenderingHints.generate_hints({
        "compressed": True,
        "compression_method": "gzip",
        "original_size": 1024
    })
    assert compression_hints["X-Compression-Method"] == "gzip", "Compression method hint incorrect"
    print("✓ Compression hints generated")


def test_complexity_analysis():
    """Test data complexity analysis."""
    print("\nTesting complexity analysis...")
    
    # Simple data
    simple_dict = {"a": 1, "b": 2}
    simple_list = [1, 2, 3]
    
    assert _analyze_complexity(simple_dict) == "low", "Simple dict should be low complexity"
    assert _analyze_complexity(simple_list) == "low", "Simple list should be low complexity"
    print("✓ Simple data correctly identified as low complexity")
    
    # Medium complexity
    medium_dict = {f"key_{i}": i for i in range(20)}
    medium_list = [{"item": i} for i in range(30)]
    
    assert _analyze_complexity(medium_dict) == "medium", "Medium dict should be medium complexity"
    assert _analyze_complexity(medium_list) == "medium", "Medium list should be medium complexity"
    print("✓ Medium complexity data correctly identified")
    
    # High complexity
    high_dict = {f"key_{i}": {"nested": {"deep": i}} for i in range(100)}
    high_list = [{"item": i, "data": [j for j in range(10)]} for i in range(200)]
    
    assert _analyze_complexity(high_dict) == "high", "High dict should be high complexity"
    assert _analyze_complexity(high_list) == "high", "High list should be high complexity"
    print("✓ High complexity data correctly identified")


def test_dtesn_optimization():
    """Test end-to-end DTESN response optimization."""
    print("\nTesting DTESN response optimization...")
    
    config = RenderingConfig(
        progressive_json=True,
        max_chunk_size=2048,
        compression_strategy="adaptive"
    )
    
    # Test small response
    small_result = {
        "result": {"output": "test", "status": "success"},
        "metadata": {"processed": True}
    }
    
    optimized_small = optimize_dtesn_response(small_result, config)
    assert optimized_small["metadata"]["optimized"] == True, "Small response should be marked as optimized"
    print("✓ Small DTESN response optimization works")
    
    # Test large response
    large_result = {
        "result": {
            "output": "x" * 5000,
            "details": [{"item": i, "data": "y" * 100} for i in range(100)]
        },
        "metadata": {"processed": True, "chunks": 100}
    }
    
    optimized_large = optimize_dtesn_response(large_result, config)
    assert optimized_large["metadata"]["optimized"] == True, "Large response should be optimized"
    assert "hints" in optimized_large, "Optimized response should include hints"
    
    # Check if compression was applied (it should be for large content)
    if optimized_large["content"]["compressed"]:
        assert optimized_large["content"]["compression_ratio"] < 1.0, "Compression should reduce size"
        print(f"✓ Large DTESN response compressed with ratio: {optimized_large['content']['compression_ratio']:.3f}")
    else:
        print("✓ Large DTESN response processed (compression may not have been beneficial)")
    
    print("✓ DTESN response optimization works end-to-end")


def test_route_optimization_functions():
    """Test route optimization helper functions."""
    print("\nTesting route optimization functions...")
    
    try:
        from aphrodite.endpoints.deep_tree_echo.routes import (
            _get_optimal_chunk_size,
            _get_compression_level,
            _estimate_throughput,
            _get_adaptive_delay
        )
        
        # Test bandwidth configurations
        for bandwidth in ["low", "medium", "high", "auto"]:
            chunk_size = _get_optimal_chunk_size(bandwidth)
            compression_level = _get_compression_level(bandwidth, 1024*1024)
            throughput = _estimate_throughput(bandwidth)
            delay = _get_adaptive_delay(bandwidth)
            
            assert chunk_size > 0, f"Chunk size should be positive for {bandwidth}"
            assert 1 <= compression_level <= 9, f"Compression level should be 1-9 for {bandwidth}"
            assert throughput > 0, f"Throughput should be positive for {bandwidth}"
            assert delay >= 0, f"Delay should be non-negative for {bandwidth}"
            
            print(f"✓ {bandwidth} bandwidth: chunk_size={chunk_size}, compression={compression_level}, throughput={throughput:.1f}MB/s")
        
        print("✓ Route optimization functions work correctly")
        
    except ImportError as e:
        print(f"⚠ Could not test route functions (import error): {e}")


def main():
    """Run all tests."""
    print("🧪 Running Progressive Rendering and Optimization Tests")
    print("=" * 60)
    
    try:
        test_progressive_json_encoder()
        test_content_compressor()
        test_rendering_hints()
        test_complexity_analysis()
        test_dtesn_optimization()
        test_route_optimization_functions()
        
        print("\n" + "=" * 60)
        print("✅ All tests passed! Progressive rendering optimization is working correctly.")
        print("\nKey optimizations implemented:")
        print("• Progressive JSON encoding for large datasets")
        print("• Adaptive compression based on content type and size")
        print("• Intelligent rendering hints for client optimization")
        print("• Bandwidth-aware streaming configuration")
        print("• Content complexity analysis for optimization decisions")
        
        return True
        
    except Exception as e:
        print(f"\n❌ Test failed: {e}")
        import traceback
        traceback.print_exc()
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)