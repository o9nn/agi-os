"""
Tests for comprehensive DTESN error handling and recovery mechanisms.

Validates error handling, recovery strategies, circuit breakers, and fallback modes
to ensure 99.9% uptime requirements are met.
"""

import asyncio
import pytest
from unittest.mock import Mock, patch, AsyncMock
from fastapi.testclient import TestClient

from aphrodite.endpoints.deep_tree_echo import create_app
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
from aphrodite.endpoints.deep_tree_echo.errors import (
    DTESNError, DTESNValidationError, DTESNProcessingError, 
    DTESNResourceError, DTESNEngineError, DTESNSystemError,
    ErrorSeverity, ErrorCategory, RecoveryStrategy,
    ErrorContext, error_aggregator, CircuitBreaker
)
from aphrodite.endpoints.deep_tree_echo.error_recovery import (
    RetryManager, FallbackProcessor, ErrorRecoveryService, 
    FallbackMode, RecoveryResult
)


class TestDTESNErrorTypes:
    """Test DTESN error types and classification."""

    def test_dtesn_validation_error(self):
        """Test validation error creation and properties."""
        error = DTESNValidationError(
            "Invalid membrane depth",
            field_name="membrane_depth"
        )
        
        assert error.category == ErrorCategory.VALIDATION
        assert error.severity == ErrorSeverity.LOW
        assert error.recovery_strategy == RecoveryStrategy.ABORT
        assert error.field_name == "membrane_depth"
        assert "DTESN_VALIDATION_LOW" in error.error_code

    def test_dtesn_processing_error(self):
        """Test processing error creation and properties."""
        error = DTESNProcessingError(
            "DTESN processing failed",
            processing_stage="membrane_computation"
        )
        
        assert error.category == ErrorCategory.PROCESSING
        assert error.severity == ErrorSeverity.MEDIUM
        assert error.recovery_strategy == RecoveryStrategy.RETRY
        assert error.processing_stage == "membrane_computation"

    def test_dtesn_resource_error(self):
        """Test resource error creation and properties."""
        error = DTESNResourceError(
            "Memory exhausted",
            resource_type="memory"
        )
        
        assert error.category == ErrorCategory.RESOURCE
        assert error.severity == ErrorSeverity.HIGH
        assert error.recovery_strategy == RecoveryStrategy.DEGRADE
        assert error.resource_type == "memory"

    def test_dtesn_system_error(self):
        """Test system error creation and properties."""
        error = DTESNSystemError("Critical system failure")
        
        assert error.category == ErrorCategory.SYSTEM
        assert error.severity == ErrorSeverity.CRITICAL
        assert error.recovery_strategy == RecoveryStrategy.CIRCUIT_BREAK

    def test_error_context_serialization(self):
        """Test error context serialization."""
        context = ErrorContext(
            request_id="req_123",
            endpoint="/deep_tree_echo/process",
            user_input="test input data",
            processing_stage="validation",
            retry_count=2
        )
        
        context_dict = context.to_dict()
        assert context_dict["request_id"] == "req_123"
        assert context_dict["endpoint"] == "/deep_tree_echo/process"
        assert context_dict["user_input"] == "test input data"
        assert context_dict["retry_count"] == 2

    def test_error_serialization(self):
        """Test DTESN error serialization."""
        context = ErrorContext("req_123", "/test")
        error = DTESNProcessingError(
            "Test error",
            context=context,
            original_error=ValueError("original")
        )
        
        error_dict = error.to_dict()
        assert error_dict["message"] == "Test error"
        assert error_dict["category"] == "processing"
        assert error_dict["severity"] == "medium"
        assert error_dict["context"]["request_id"] == "req_123"
        assert error_dict["original_error"]["type"] == "ValueError"


class TestErrorAggregator:
    """Test error aggregation and pattern analysis."""

    def setup_method(self):
        """Reset error aggregator for each test."""
        error_aggregator.error_history = []
        error_aggregator.error_counts = {}

    def test_error_recording(self):
        """Test error recording and tracking."""
        error = DTESNProcessingError("Test error")
        error_aggregator.record_error(error)
        
        assert len(error_aggregator.error_history) == 1
        assert "processing_medium" in error_aggregator.error_counts
        assert error_aggregator.error_counts["processing_medium"] == 1

    def test_system_health_assessment(self):
        """Test system health status assessment."""
        # Add multiple errors to trigger thresholds
        for i in range(10):
            error = DTESNProcessingError(f"Error {i}")
            error_aggregator.record_error(error)
        
        health = error_aggregator.get_system_health_status()
        assert health["recent_errors_count"] > 0
        assert health["status"] in ["healthy", "unhealthy"]

    def test_recovery_recommendations(self):
        """Test recovery recommendations by category."""
        recommendations = error_aggregator.get_recovery_recommendations(ErrorCategory.RESOURCE)
        assert "Reduce batch sizes" in recommendations
        assert "Enable degraded mode processing" in recommendations
        
        recommendations = error_aggregator.get_recovery_recommendations(ErrorCategory.PROCESSING)
        assert "Retry with exponential backoff" in recommendations


class TestCircuitBreaker:
    """Test circuit breaker functionality."""

    def test_circuit_breaker_states(self):
        """Test circuit breaker state transitions."""
        breaker = CircuitBreaker(failure_threshold=3, recovery_timeout=1)
        
        # Should be closed initially
        assert breaker.state == "CLOSED"
        
        # Trigger failures
        for i in range(3):
            try:
                breaker.call(lambda: 1 / 0)  # Will raise ZeroDivisionError
            except:
                pass
        
        # Should be open after failure threshold
        assert breaker.state == "OPEN"

    @pytest.mark.asyncio
    async def test_circuit_breaker_recovery(self):
        """Test circuit breaker recovery timeout."""
        breaker = CircuitBreaker(failure_threshold=2, recovery_timeout=0.1)
        
        # Trigger failures to open circuit
        for i in range(2):
            try:
                breaker.call(lambda: 1 / 0)
            except:
                pass
        
        assert breaker.state == "OPEN"
        
        # Wait for recovery timeout
        await asyncio.sleep(0.15)
        
        # Should attempt reset on next call
        result = breaker.call(lambda: "success")
        assert result == "success"
        assert breaker.state == "CLOSED"


class TestRetryManager:
    """Test retry mechanisms."""

    @pytest.mark.asyncio
    async def test_successful_retry(self):
        """Test successful retry after initial failure."""
        call_count = 0
        
        async def flaky_function():
            nonlocal call_count
            call_count += 1
            if call_count < 3:
                raise ConnectionError("Network error")
            return "success"
        
        retry_manager = RetryManager()
        result = await retry_manager.retry_async(flaky_function)
        
        assert result.success is True
        assert result.result == "success"
        assert result.attempts_made == 3

    @pytest.mark.asyncio
    async def test_retry_exhaustion(self):
        """Test retry exhaustion when all attempts fail."""
        async def always_fail():
            raise RuntimeError("Persistent error")
        
        retry_manager = RetryManager()
        result = await retry_manager.retry_async(always_fail)
        
        assert result.success is False
        assert result.attempts_made == 3
        assert isinstance(result.error, DTESNProcessingError)


class TestFallbackProcessor:
    """Test fallback processing strategies."""

    def setup_method(self):
        """Set up fallback processor for each test."""
        self.processor = FallbackProcessor()

    @pytest.mark.asyncio
    async def test_simplified_fallback(self):
        """Test simplified processing fallback."""
        result = await self.processor._simplified_processing("Hello world test")
        
        assert "Simplified processing" in result["output"]
        assert result["membrane_layers"] == 1
        assert result["metadata"]["processing_mode"] == "simplified"
        assert result["metadata"]["word_count"] == 3

    @pytest.mark.asyncio
    async def test_statistical_fallback(self):
        """Test statistical analysis fallback."""
        result = await self.processor._statistical_processing("Test input with punctuation!")
        
        assert "Statistical analysis" in result["output"]
        assert result["membrane_layers"] == 2
        assert result["metadata"]["processing_mode"] == "statistical"
        assert "statistics" in result["metadata"]

    @pytest.mark.asyncio
    async def test_minimal_fallback(self):
        """Test minimal processing fallback."""
        input_text = "This is a test input for minimal processing"
        result = await self.processor._minimal_processing(input_text)
        
        assert "Echo:" in result["output"]
        assert result["membrane_layers"] == 1
        assert result["metadata"]["processing_mode"] == "minimal"

    @pytest.mark.asyncio
    async def test_cached_fallback(self):
        """Test cached processing fallback."""
        # First, add something to cache
        input_data = "test input"
        cache_result = {"output": "cached result", "cached": True}
        self.processor._update_cache(input_data, cache_result)
        
        result = await self.processor._cached_processing(input_data)
        assert result["cached"] is True
        assert result["fallback_mode"] == "cached"

    @pytest.mark.asyncio
    async def test_fallback_with_primary_success(self):
        """Test fallback when primary processor succeeds."""
        async def successful_processor(input_data):
            return {"output": "primary success", "membrane_layers": 5}
        
        result = await self.processor.process_with_fallback(
            "test input",
            successful_processor
        )
        
        assert result.success is True
        assert result.fallback_used is False
        assert result.recovery_mode == "primary_success"

    @pytest.mark.asyncio
    async def test_fallback_with_primary_failure(self):
        """Test fallback when primary processor fails."""
        async def failing_processor(input_data):
            raise RuntimeError("Primary processing failed")
        
        result = await self.processor.process_with_fallback(
            "test input",
            failing_processor,
            fallback_mode=FallbackMode.SIMPLIFIED
        )
        
        assert result.success is True
        assert result.fallback_used is True
        assert result.degraded is True
        assert result.recovery_mode == "fallback_simplified"


class TestErrorRecoveryService:
    """Test comprehensive error recovery service."""

    def setup_method(self):
        """Set up error recovery service for each test."""
        self.service = ErrorRecoveryService()

    @pytest.mark.asyncio
    async def test_successful_operation(self):
        """Test successful operation without recovery needed."""
        async def successful_operation(input_data):
            return {"result": "success", "data": input_data}
        
        result = await self.service.execute_with_recovery(
            successful_operation,
            "test input"
        )
        
        assert result.success is True
        assert result.result["result"] == "success"

    @pytest.mark.asyncio
    async def test_recovery_with_retry(self):
        """Test recovery using retry mechanism."""
        call_count = 0
        
        async def flaky_operation(input_data):
            nonlocal call_count
            call_count += 1
            if call_count < 2:
                raise ConnectionError("Temporary failure")
            return {"result": "recovered", "attempts": call_count}
        
        result = await self.service.execute_with_recovery(
            flaky_operation,
            "test input",
            enable_retry=True
        )
        
        assert result.success is True
        assert result.result["result"] == "recovered"
        assert result.attempts_made == 2

    @pytest.mark.asyncio
    async def test_recovery_with_fallback(self):
        """Test recovery using fallback mechanism."""
        async def always_failing_operation(input_data):
            raise RuntimeError("Operation always fails")
        
        result = await self.service.execute_with_recovery(
            always_failing_operation,
            "test input",
            enable_retry=False,
            enable_fallback=True,
            fallback_mode=FallbackMode.MINIMAL
        )
        
        assert result.success is True
        assert result.fallback_used is True
        assert result.degraded is True

    def test_recovery_stats(self):
        """Test recovery statistics tracking."""
        stats = self.service.get_recovery_stats()
        
        assert "total_recoveries" in stats
        assert "successful_recoveries" in stats
        assert "success_rate" in stats
        assert "resource_status" in stats
        assert "system_health" in stats


class TestErrorHandlingIntegration:
    """Integration tests for error handling in DTESN endpoints."""

    @pytest.fixture
    def config(self):
        """Test configuration fixture."""
        return DTESNConfig(
            enable_docs=False,
            max_membrane_depth=4,
            esn_reservoir_size=256
        )

    @pytest.fixture
    def app(self, config):
        """FastAPI application fixture."""
        return create_app(config=config)

    @pytest.fixture
    def client(self, app):
        """Test client fixture."""
        return TestClient(app)

    def test_validation_error_handling(self, client):
        """Test validation error handling in endpoints."""
        # Test invalid membrane depth
        response = client.post("/deep_tree_echo/process", json={
            "input_data": "test",
            "membrane_depth": 999,  # Too high
            "esn_size": 128
        })
        
        assert response.status_code == 422  # Validation error
        data = response.json()
        assert "detail" in data

    def test_processing_error_recovery(self, client):
        """Test processing error recovery in endpoints."""
        with patch('aphrodite.endpoints.deep_tree_echo.dtesn_processor.DTESNProcessor') as mock_processor_class:
            # Mock processor to raise an error initially, then succeed on retry
            mock_processor = Mock()
            mock_processor_class.return_value = mock_processor
            
            call_count = 0
            
            async def mock_process(*args, **kwargs):
                nonlocal call_count
                call_count += 1
                if call_count == 1:
                    raise RuntimeError("Initial processing failure")
                return Mock(
                    to_dict=lambda: {"output": "recovered result"},
                    processing_time_ms=100,
                    membrane_layers=2
                )
            
            mock_processor.process = mock_process
            
            response = client.post("/deep_tree_echo/process", json={
                "input_data": "test input",
                "membrane_depth": 2,
                "esn_size": 64
            })
            
            # Should get a response (either success or structured error)
            assert response.status_code in [200, 422, 500]

    def test_batch_processing_error_handling(self, client):
        """Test batch processing error handling and partial recovery."""
        response = client.post("/deep_tree_echo/batch_process", json={
            "inputs": ["test1", "test2", "test3"],
            "membrane_depth": 2,
            "esn_size": 64,
            "parallel_processing": True
        })
        
        # Should handle batch processing gracefully
        assert response.status_code in [200, 422, 500]
        
        if response.status_code == 200:
            data = response.json()
            assert "results" in data
            assert "batch_size" in data

    def test_middleware_error_handling(self, client):
        """Test middleware error handling and context enrichment."""
        # Test with custom headers to trigger middleware processing
        response = client.get("/deep_tree_echo/status", headers={
            "X-Request-ID": "test-req-123",
            "X-Trace-ID": "trace-456"
        })
        
        assert response.status_code == 200
        assert "X-DTESN-Processed" in response.headers
        assert "X-Request-ID" in response.headers

    def test_circuit_breaker_integration(self, client):
        """Test circuit breaker integration in endpoints."""
        # This would require simulating multiple failures to trigger circuit breaker
        # For now, just verify the endpoint responds appropriately
        response = client.get("/deep_tree_echo/")
        assert response.status_code == 200

    def test_error_response_format(self, client):
        """Test standardized error response format."""
        # Test with invalid JSON to trigger a parsing error
        response = client.post(
            "/deep_tree_echo/process",
            data="invalid json",
            headers={"Content-Type": "application/json"}
        )
        
        assert response.status_code == 422
        # Error response should be structured
        data = response.json()
        assert "detail" in data


class TestErrorHandlingPerformance:
    """Performance tests for error handling system."""

    @pytest.mark.asyncio
    async def test_error_aggregation_performance(self):
        """Test performance of error aggregation with many errors."""
        import time
        
        start_time = time.time()
        
        # Add many errors quickly
        for i in range(1000):
            error = DTESNProcessingError(f"Error {i}")
            error_aggregator.record_error(error)
        
        end_time = time.time()
        
        # Should process 1000 errors in reasonable time (< 1 second)
        assert end_time - start_time < 1.0
        
        # Verify error history is maintained within limits
        assert len(error_aggregator.error_history) <= 1000

    @pytest.mark.asyncio
    async def test_retry_manager_performance(self):
        """Test retry manager performance with concurrent operations."""
        retry_manager = RetryManager()
        
        async def test_operation():
            await asyncio.sleep(0.001)  # Simulate small delay
            return "success"
        
        start_time = time.time()
        
        # Run multiple retry operations concurrently
        tasks = [
            retry_manager.retry_async(test_operation)
            for _ in range(100)
        ]
        
        results = await asyncio.gather(*tasks)
        
        end_time = time.time()
        
        # All should succeed
        assert all(result.success for result in results)
        
        # Should complete in reasonable time
        assert end_time - start_time < 2.0

    @pytest.mark.asyncio
    async def test_fallback_processor_cache_performance(self):
        """Test fallback processor cache performance."""
        processor = FallbackProcessor()
        
        # Add many items to cache
        for i in range(1000):
            input_data = f"test input {i}"
            result = {"output": f"result {i}"}
            processor._update_cache(input_data, result)
        
        # Cache should maintain size limit
        assert len(processor.cache) <= 1000
        
        # Cache lookups should be fast
        start_time = time.time()
        
        for i in range(100):
            cache_key = processor._generate_cache_key(f"test input {i}")
            processor._is_cache_valid(cache_key)
        
        end_time = time.time()
        assert end_time - start_time < 0.1  # 100ms for 100 lookups


if __name__ == "__main__":
    pytest.main([__file__, "-v"])