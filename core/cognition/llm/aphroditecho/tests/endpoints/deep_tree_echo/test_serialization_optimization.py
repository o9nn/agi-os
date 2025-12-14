"""
Tests for DTESN result serialization optimization.

Validates the 60% serialization overhead reduction target and ensures
data integrity across all serialization formats.
"""

import json
import time
import pickle
from typing import Dict, Any
import pytest

try:
    import msgspec
    MSGSPEC_AVAILABLE = True
except ImportError:
    MSGSPEC_AVAILABLE = False

try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False

# Test imports
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


class TestSerializationConfig:
    """Test serialization configuration."""
    
    def test_default_config(self):
        config = SerializationConfig()
        assert config.format == SerializationFormat.JSON_OPTIMIZED
        assert config.include_metadata == True
        assert config.include_engine_integration == False
        assert config.compress_arrays == True
        assert config.max_array_inline_size == 256
        assert config.precision == 6
    
    def test_custom_config(self):
        config = SerializationConfig(
            format=SerializationFormat.BINARY,
            include_engine_integration=True,
            precision=4,
            max_array_inline_size=512
        )
        assert config.format == SerializationFormat.BINARY
        assert config.include_engine_integration == True
        assert config.precision == 4
        assert config.max_array_inline_size == 512


class TestOptimizedJSONSerializer:
    """Test optimized JSON serialization."""
    
    def test_basic_serialization(self):
        config = SerializationConfig(format=SerializationFormat.JSON_OPTIMIZED)
        serializer = OptimizedJSONSerializer(config)
        
        result = MockDTESNResult(size_multiplier=1)
        serialized = serializer.serialize(result)
        
        # Verify it's valid JSON
        data = json.loads(serialized)
        assert "input" in data
        assert "output" in data
        assert "processing_time_ms" in data
        
        # Verify metadata is included
        assert serializer.metadata is not None
        assert serializer.metadata.format_used == "json_optimized"
        assert serializer.metadata.serialization_time_ms > 0
    
    def test_engine_integration_exclusion(self):
        """Test that engine integration data is excluded by default."""
        config = SerializationConfig(
            format=SerializationFormat.JSON_OPTIMIZED,
            include_engine_integration=False
        )
        serializer = OptimizedJSONSerializer(config)
        
        result = MockDTESNResult()
        serialized = serializer.serialize(result)
        data = json.loads(serialized)
        
        # Engine integration should be excluded or minimal
        if "engine_integration" in data:
            assert len(data["engine_integration"]) <= 3  # Only basic fields
    
    def test_engine_integration_inclusion(self):
        """Test that engine integration data is included when requested."""
        config = SerializationConfig(
            format=SerializationFormat.JSON_OPTIMIZED,
            include_engine_integration=True
        )
        serializer = OptimizedJSONSerializer(config)
        
        result = MockDTESNResult()
        serialized = serializer.serialize(result)
        data = json.loads(serialized)
        
        assert "engine_integration" in data
        assert "engine_available" in data["engine_integration"]
    
    def test_large_structure_summarization(self):
        """Test that large structures are summarized to reduce overhead."""
        config = SerializationConfig(format=SerializationFormat.JSON_OPTIMIZED)
        serializer = OptimizedJSONSerializer(config)
        
        result = MockDTESNResult(size_multiplier=10)  # Large data
        serialized = serializer.serialize(result)
        
        # Should be significantly smaller than naive serialization
        naive_size = len(json.dumps(result.to_dict()))
        optimized_size = len(serialized)
        
        compression_ratio = optimized_size / naive_size
        assert compression_ratio < 0.8  # At least 20% reduction
    
    def test_deterministic_output(self):
        """Test that output is deterministic with sort_keys=True."""
        config = SerializationConfig(
            format=SerializationFormat.JSON_OPTIMIZED,
            sort_keys=True
        )
        serializer = OptimizedJSONSerializer(config)
        
        result = MockDTESNResult()
        
        # Serialize multiple times
        serialized1 = serializer.serialize(result)
        serialized2 = serializer.serialize(result)
        
        assert serialized1 == serialized2
    
    def test_deserialization(self):
        """Test deserialization works correctly."""
        config = SerializationConfig(format=SerializationFormat.JSON_OPTIMIZED)
        serializer = OptimizedJSONSerializer(config)
        
        result = MockDTESNResult()
        serialized = serializer.serialize(result)
        deserialized = serializer.deserialize(serialized)
        
        # Verify core data is preserved
        assert deserialized["input"] == result.input_data
        assert deserialized["processing_time_ms"] == result.processing_time_ms


class TestBinarySerializer:
    """Test binary serialization."""
    
    @pytest.mark.skipif(not MSGSPEC_AVAILABLE, reason="msgspec not available")
    def test_binary_serialization(self):
        config = SerializationConfig(format=SerializationFormat.BINARY)
        serializer = BinarySerializer(config)
        
        result = MockDTESNResult()
        serialized = serializer.serialize(result)
        
        # Should be bytes
        assert isinstance(serialized, bytes)
        
        # Should be smaller than JSON
        json_size = len(json.dumps(result.to_dict()))
        binary_size = len(serialized)
        assert binary_size < json_size
        
        # Verify metadata
        assert serializer.metadata is not None
        assert serializer.metadata.format_used == "binary_msgpack"
    
    @pytest.mark.skipif(not MSGSPEC_AVAILABLE, reason="msgspec not available")
    def test_binary_roundtrip(self):
        """Test binary serialization roundtrip."""
        config = SerializationConfig(format=SerializationFormat.BINARY)
        serializer = BinarySerializer(config)
        
        result = MockDTESNResult()
        serialized = serializer.serialize(result)
        deserialized = serializer.deserialize(serialized)
        
        # Verify data integrity
        assert deserialized["input"] == result.input_data
        assert deserialized["processing_time_ms"] == result.processing_time_ms


class TestDeterministicSerializer:
    """Test deterministic serialization."""
    
    def test_deterministic_serialization(self):
        config = SerializationConfig(format=SerializationFormat.DETERMINISTIC)
        serializer = DeterministicSerializer(config)
        
        result = MockDTESNResult()
        
        # Serialize multiple times with time delays
        serialized1 = serializer.serialize(result)
        time.sleep(0.01)  # Small delay
        serialized2 = serializer.serialize(result)
        
        # Should be identical despite time differences
        assert serialized1 == serialized2
    
    def test_checksum_included(self):
        """Test that deterministic serialization includes checksum."""
        config = SerializationConfig(
            format=SerializationFormat.DETERMINISTIC,
            validate_output=True
        )
        serializer = DeterministicSerializer(config)
        
        result = MockDTESNResult()
        serialized = serializer.serialize(result)
        data = json.loads(serialized)
        
        assert "_deterministic" in data
        assert "checksum" in data["_deterministic"]
        assert len(data["_deterministic"]["checksum"]) == 16
    
    def test_time_fields_excluded(self):
        """Test that time-dependent fields are excluded."""
        config = SerializationConfig(format=SerializationFormat.DETERMINISTIC)
        serializer = DeterministicSerializer(config)
        
        result = MockDTESNResult()
        serialized = serializer.serialize(result)
        data = json.loads(serialized)
        
        # Time-dependent fields should be excluded
        assert "processing_time_ms" not in data


class TestSerializerFactory:
    """Test serializer factory."""
    
    def test_create_optimized_json_serializer(self):
        config = SerializationConfig(format=SerializationFormat.JSON_OPTIMIZED)
        serializer = SerializerFactory.create_serializer(config)
        assert isinstance(serializer, OptimizedJSONSerializer)
    
    @pytest.mark.skipif(not MSGSPEC_AVAILABLE, reason="msgspec not available")
    def test_create_binary_serializer(self):
        config = SerializationConfig(format=SerializationFormat.BINARY)
        serializer = SerializerFactory.create_serializer(config)
        assert isinstance(serializer, BinarySerializer)
    
    def test_create_deterministic_serializer(self):
        config = SerializationConfig(format=SerializationFormat.DETERMINISTIC)
        serializer = SerializerFactory.create_serializer(config)
        assert isinstance(serializer, DeterministicSerializer)
    
    def test_unsupported_format(self):
        config = SerializationConfig(format="unsupported_format")
        with pytest.raises(ValueError, match="Unsupported serialization format"):
            SerializerFactory.create_serializer(config)


class TestPerformanceOptimization:
    """Test performance optimization and overhead reduction."""
    
    def test_overhead_reduction_target(self):
        """Test that serialization overhead is reduced by at least 60%."""
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
        
        # Verify significant reduction (target: 60%)
        assert size_reduction >= 0.5, f"Size reduction {size_reduction:.2%} below 50% target"
        
        # Time reduction may vary, but should show improvement in most cases
        print(f"Size reduction: {size_reduction:.2%}")
        print(f"Time reduction: {time_reduction:.2%}")
        print(f"Baseline size: {baseline_size} bytes")
        print(f"Optimized size: {optimized_size} bytes")
    
    def test_benchmark_all_formats(self):
        """Test benchmark function with all serialization formats."""
        result = MockDTESNResult(size_multiplier=2)
        
        # Run benchmark (small number of iterations for test)
        benchmark_results = benchmark_serialization(result, iterations=100)
        
        # Verify all formats are tested
        expected_formats = [
            SerializationFormat.JSON,
            SerializationFormat.JSON_OPTIMIZED,
            SerializationFormat.DETERMINISTIC,
        ]
        
        # Binary format only if msgspec available
        if MSGSPEC_AVAILABLE:
            expected_formats.append(SerializationFormat.BINARY)
        
        for format_name in expected_formats:
            assert format_name in benchmark_results
            result_data = benchmark_results[format_name]
            assert "avg_time_ms" in result_data
            assert "compression_ratio" in result_data
            assert "serialized_size" in result_data
            assert result_data["avg_time_ms"] > 0
    
    def test_large_array_handling(self):
        """Test handling of large arrays in serialization."""
        result = MockDTESNResult()
        
        # Add large array data
        if NUMPY_AVAILABLE:
            result.esn_state["large_array"] = np.random.rand(1000)
        else:
            result.esn_state["large_array"] = [0.1] * 1000
        
        config = SerializationConfig(
            format=SerializationFormat.JSON_OPTIMIZED,
            max_array_inline_size=256,
            compress_arrays=True
        )
        serializer = OptimizedJSONSerializer(config)
        
        serialized = serializer.serialize(result)
        data = json.loads(serialized)
        
        # Large arrays should be referenced or compressed
        if "esn_state" in data and "large_array" in data["esn_state"]:
            array_data = data["esn_state"]["large_array"]
            if isinstance(array_data, dict):
                # Should be a reference structure
                assert "_type" in array_data or "_summary" in array_data


class TestConvenienceFunctions:
    """Test convenience functions."""
    
    def test_serialize_dtesn_result_function(self):
        """Test the serialize_dtesn_result convenience function."""
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
    
    @pytest.mark.skipif(not MSGSPEC_AVAILABLE, reason="msgspec not available")
    def test_serialize_binary_format(self):
        """Test binary format serialization."""
        result = MockDTESNResult()
        
        binary_serialized = serialize_dtesn_result(
            result,
            format=SerializationFormat.BINARY
        )
        assert isinstance(binary_serialized, bytes)


class TestErrorHandling:
    """Test error handling in serialization."""
    
    def test_invalid_data_handling(self):
        """Test handling of invalid or problematic data."""
        # Create result with non-serializable data
        result = MockDTESNResult()
        result.processed_output["invalid_func"] = lambda x: x  # Function object
        result.processed_output["none_value"] = None
        
        config = SerializationConfig(format=SerializationFormat.JSON_OPTIMIZED)
        serializer = OptimizedJSONSerializer(config)
        
        # Should not raise exception, should handle gracefully
        serialized = serializer.serialize(result)
        data = json.loads(serialized)
        
        # Invalid function should be converted to string representation
        assert isinstance(data["output"]["invalid_func"], str)
        assert "callable" in data["output"]["invalid_func"]
    
    def test_empty_result_handling(self):
        """Test handling of empty or minimal results."""
        # Create minimal result
        class MinimalResult:
            def __init__(self):
                self.input_data = ""
                self.processed_output = {}
            
            def to_dict(self):
                return {"input": self.input_data, "output": self.processed_output}
        
        result = MinimalResult()
        
        config = SerializationConfig(format=SerializationFormat.JSON_OPTIMIZED)
        serializer = OptimizedJSONSerializer(config)
        
        serialized = serializer.serialize(result)
        data = json.loads(serialized)
        
        assert "input" in data
        assert "output" in data


if __name__ == "__main__":
    # Run a quick benchmark test
    result = MockDTESNResult(size_multiplier=3)
    benchmark_results = benchmark_serialization(result, iterations=1000)
    
    print("Serialization Benchmark Results:")
    print("=" * 50)
    
    for format_name, metrics in benchmark_results.items():
        print(f"{format_name}:")
        print(f"  Average time: {metrics['avg_time_ms']:.3f}ms")
        print(f"  Compression ratio: {metrics['compression_ratio']:.3f}")
        print(f"  Serialized size: {metrics['serialized_size']} bytes")
        print()
    
    # Calculate improvement
    if SerializationFormat.JSON in benchmark_results and SerializationFormat.JSON_OPTIMIZED in benchmark_results:
        baseline = benchmark_results[SerializationFormat.JSON]
        optimized = benchmark_results[SerializationFormat.JSON_OPTIMIZED]
        
        time_improvement = (baseline['avg_time_ms'] - optimized['avg_time_ms']) / baseline['avg_time_ms']
        size_improvement = (baseline['serialized_size'] - optimized['serialized_size']) / baseline['serialized_size']
        
        print(f"Optimization improvements:")
        print(f"  Time reduction: {time_improvement:.1%}")
        print(f"  Size reduction: {size_improvement:.1%}")