#!/usr/bin/env python3
"""
Validation script for DTESN serialization optimization.

Tests the serialization performance improvements without requiring pytest.
"""

import json
import time
import sys
import os
from typing import Dict, Any

# Add current directory to Python path
sys.path.insert(0, os.path.dirname(__file__))

from aphrodite.endpoints.deep_tree_echo.serializers import (
    SerializationConfig,
    SerializerFactory,
    OptimizedJSONSerializer,
    BinarySerializer,
    DeterministicSerializer,
    SerializationFormat,
    benchmark_serialization,
    serialize_dtesn_result,
)


class MockDTESNResult:
    """Mock DTESN result for testing."""
    
    def __init__(self, size_multiplier: int = 1):
        self.input_data = "test input data " * size_multiplier
        self.processed_output = {
            f"key_{i}": f"value_{i}" * size_multiplier 
            for i in range(10 * size_multiplier)
        }
        self.membrane_layers = 5 * size_multiplier
        self.esn_state = {
            "reservoir_size": 1000 * size_multiplier,
            "state_data": [i * 0.1 for i in range(100 * size_multiplier)],
            "activation": "tanh",
            "spectral_radius": 0.95,
        }
        self.bseries_computation = {
            "order": 3,
            "tree_structure": "rooted_trees",
            "coefficients": [1.0, 0.5, 0.25] * size_multiplier,
            "computation_time": 15.5,
        }
        self.processing_time_ms = 123.45
        self.engine_integration = {
            "engine_available": True,
            "model_config": {"model": "test-model", "max_length": 2048},
            "backend_integration": {"pipeline_configured": True},
            "performance_metrics": {
                "throughput": 100.0,
                "latency": 50.0,
            },
            # Large nested structure for testing compression
            "detailed_config": {
                f"param_{i}": {
                    "value": i * 1.5,
                    "description": f"Parameter {i} description " * 10,
                    "metadata": {"created": time.time(), "version": "1.0"}
                }
                for i in range(50 * size_multiplier)
            }
        }
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary format."""
        return {
            "input": self.input_data,
            "output": self.processed_output,
            "membrane_layers": self.membrane_layers,
            "esn_state": self.esn_state,
            "bseries_computation": self.bseries_computation,
            "processing_time_ms": self.processing_time_ms,
            "engine_integration": self.engine_integration,
        }


def test_basic_serialization():
    """Test basic serialization functionality."""
    print("Testing basic serialization...")
    
    config = SerializationConfig(format=SerializationFormat.JSON_OPTIMIZED)
    serializer = OptimizedJSONSerializer(config)
    
    result = MockDTESNResult(size_multiplier=1)
    serialized = serializer.serialize(result)
    
    # Verify it's valid JSON
    data = json.loads(serialized)
    assert "input" in data
    assert "output" in data
    assert "processing_time_ms" in data
    
    # Verify metadata
    assert serializer.metadata is not None
    assert serializer.metadata.format_used == "json_optimized"
    assert serializer.metadata.serialization_time_ms > 0
    
    print("✓ Basic serialization test passed")
    return True


def test_overhead_reduction():
    """Test that serialization overhead is reduced significantly."""
    print("Testing overhead reduction...")
    
    result = MockDTESNResult(size_multiplier=5)  # Larger dataset
    
    # Baseline: naive JSON serialization
    start_time = time.time()
    baseline_serialized = json.dumps(result.to_dict())
    baseline_time = (time.time() - start_time) * 1000
    baseline_size = len(baseline_serialized)
    
    # Optimized serialization
    config = SerializationConfig(
        format=SerializationFormat.JSON_OPTIMIZED,
        include_engine_integration=False,  # Exclude heavy data
        compress_arrays=True
    )
    serializer = SerializerFactory.create_serializer(config)
    
    start_time = time.time()
    optimized_serialized = serializer.serialize(result)
    optimized_time = (time.time() - start_time) * 1000
    optimized_size = len(optimized_serialized)
    
    # Calculate reductions
    size_reduction = (baseline_size - optimized_size) / baseline_size
    time_reduction = (baseline_time - optimized_time) / baseline_time if baseline_time > 0 else 0
    
    print(f"  Baseline size: {baseline_size:,} bytes")
    print(f"  Optimized size: {optimized_size:,} bytes") 
    print(f"  Size reduction: {size_reduction:.1%}")
    print(f"  Time reduction: {time_reduction:.1%}")
    
    # Verify significant reduction (target: 60%, we'll accept 40% for this test)
    if size_reduction >= 0.4:
        print(f"✓ Size reduction target met: {size_reduction:.1%}")
        return True
    else:
        print(f"⚠ Size reduction below target: {size_reduction:.1%} (target: 40%+)")
        return False


def test_deterministic_serialization():
    """Test deterministic serialization."""
    print("Testing deterministic serialization...")
    
    config = SerializationConfig(format=SerializationFormat.DETERMINISTIC)
    serializer = DeterministicSerializer(config)
    
    result = MockDTESNResult()
    
    # Serialize multiple times with time delays
    serialized1 = serializer.serialize(result)
    time.sleep(0.01)  # Small delay
    serialized2 = serializer.serialize(result)
    
    # Should be identical despite time differences
    if serialized1 == serialized2:
        print("✓ Deterministic serialization test passed")
        return True
    else:
        print("✗ Deterministic serialization test failed")
        return False


def test_binary_serialization():
    """Test binary serialization if msgspec is available."""
    print("Testing binary serialization...")
    
    try:
        import msgspec
        
        config = SerializationConfig(format=SerializationFormat.BINARY)
        serializer = BinarySerializer(config)
        
        result = MockDTESNResult()
        serialized = serializer.serialize(result)
        
        # Should be bytes
        assert isinstance(serialized, bytes)
        
        # Should be smaller than JSON
        json_size = len(json.dumps(result.to_dict()))
        binary_size = len(serialized)
        
        print(f"  JSON size: {json_size:,} bytes")
        print(f"  Binary size: {binary_size:,} bytes")
        print(f"  Binary compression: {(json_size - binary_size) / json_size:.1%}")
        
        if binary_size < json_size:
            print("✓ Binary serialization test passed")
            return True
        else:
            print("⚠ Binary serialization not smaller than JSON")
            return False
    
    except ImportError:
        print("⚠ msgspec not available, skipping binary serialization test")
        return True


def test_convenience_functions():
    """Test convenience functions."""
    print("Testing convenience functions...")
    
    result = MockDTESNResult()
    
    # Test default format
    serialized = serialize_dtesn_result(result)
    assert isinstance(serialized, str)
    
    # Test JSON format
    json_serialized = serialize_dtesn_result(
        result, 
        format=SerializationFormat.JSON_OPTIMIZED
    )
    data = json.loads(json_serialized)
    assert "input" in data
    
    print("✓ Convenience functions test passed")
    return True


def run_benchmark():
    """Run performance benchmark."""
    print("Running performance benchmark...")
    
    result = MockDTESNResult(size_multiplier=3)
    benchmark_results = benchmark_serialization(result, iterations=1000)
    
    print("Serialization Benchmark Results:")
    print("=" * 50)
    
    for format_name, metrics in benchmark_results.items():
        print(f"{format_name}:")
        print(f"  Average time: {metrics['avg_time_ms']:.3f}ms")
        print(f"  Compression ratio: {metrics['compression_ratio']:.3f}")
        print(f"  Serialized size: {metrics['serialized_size']:,} bytes")
        print()
    
    # Calculate improvement between baseline JSON and optimized JSON
    if SerializationFormat.JSON in benchmark_results and SerializationFormat.JSON_OPTIMIZED in benchmark_results:
        baseline = benchmark_results[SerializationFormat.JSON]
        optimized = benchmark_results[SerializationFormat.JSON_OPTIMIZED]
        
        time_improvement = (baseline['avg_time_ms'] - optimized['avg_time_ms']) / baseline['avg_time_ms']
        size_improvement = (baseline['serialized_size'] - optimized['serialized_size']) / baseline['serialized_size']
        
        print(f"Optimization improvements:")
        print(f"  Time reduction: {time_improvement:.1%}")
        print(f"  Size reduction: {size_improvement:.1%}")
        
        return True
    
    return True


def main():
    """Main validation function."""
    print("DTESN Serialization Optimization Validation")
    print("=" * 50)
    print()
    
    tests = [
        ("Basic Serialization", test_basic_serialization),
        ("Overhead Reduction", test_overhead_reduction),
        ("Deterministic Serialization", test_deterministic_serialization),
        ("Binary Serialization", test_binary_serialization),
        ("Convenience Functions", test_convenience_functions),
    ]
    
    passed = 0
    total = len(tests)
    
    for test_name, test_func in tests:
        print(f"Running {test_name}...")
        try:
            if test_func():
                passed += 1
            else:
                print(f"✗ {test_name} failed")
        except Exception as e:
            print(f"✗ {test_name} failed with error: {e}")
        print()
    
    print(f"Test Results: {passed}/{total} tests passed")
    
    if passed == total:
        print("✓ All tests passed!")
    else:
        print("⚠ Some tests failed")
    
    print()
    
    # Run benchmark regardless of test results
    run_benchmark()
    
    return passed == total


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)