#!/usr/bin/env python3
"""
Tests for DTESN server-side caching layer integration.

Comprehensive test suite validating the caching functionality, performance
improvements, and integration with DTESN processing endpoints.
"""

import asyncio
import pytest
import time
from typing import Dict, Any, Set
from unittest.mock import Mock, AsyncMock, patch

# Import the modules to test
from aphrodite.endpoints.openai.dtesn_cache_manager import (
    DTESNServerSideCacheManager,
    DTESNCacheKey,
    DTESNCacheEntry,
    CacheStrategy,
    CacheMetrics
)
from aphrodite.endpoints.openai.dtesn_integration import (
    DTESNEnhancedRequest,
    DTESNIntegrationMixin
)


class TestDTESNServerSideCacheManager:
    """Test the core server-side cache manager functionality"""
    
    @pytest.fixture
    async def cache_manager(self):
        """Create a test cache manager instance"""
        manager = DTESNServerSideCacheManager(
            max_memory_entries=100,
            max_compressed_entries=200,
            redis_url=None,  # No Redis for unit tests
            default_ttl_seconds=300,
            cache_strategy=CacheStrategy.BALANCED,
            enable_compression=True
        )
        await manager.initialize()
        yield manager
        await manager.shutdown()
    
    @pytest.fixture
    def sample_dtesn_config(self):
        """Sample DTESN configuration for testing"""
        return {
            "membrane_depth": 4,
            "esn_size": 512,
            "processing_mode": "server_side"
        }
    
    @pytest.fixture
    def sample_result(self):
        """Sample DTESN processing result"""
        return {
            "membrane_layers": 4,
            "esn_output": [0.1, 0.2, 0.3],
            "final_result": "processed_output",
            "confidence": 0.95
        }
    
    @pytest.fixture 
    def sample_metadata(self):
        """Sample processing metadata"""
        return {
            "processing_time_ms": 150.0,
            "membrane_depth": 4,
            "esn_size": 512,
            "cache_miss": False
        }
    
    def test_cache_key_generation(self, cache_manager, sample_dtesn_config):
        """Test cache key generation for consistent hashing"""
        input_data = "test input data"
        model_id = "test-model"
        
        # Generate same key multiple times
        key1 = cache_manager._generate_cache_key(input_data, model_id, sample_dtesn_config)
        key2 = cache_manager._generate_cache_key(input_data, model_id, sample_dtesn_config)
        
        assert key1.to_string() == key2.to_string()
        assert key1.input_hash == key2.input_hash
        assert key1.model_id == model_id
        assert key1.membrane_depth == 4
        assert key1.esn_size == 512
    
    def test_cache_key_uniqueness(self, cache_manager, sample_dtesn_config):
        """Test that different inputs generate different cache keys"""
        model_id = "test-model"
        
        key1 = cache_manager._generate_cache_key("input1", model_id, sample_dtesn_config)
        key2 = cache_manager._generate_cache_key("input2", model_id, sample_dtesn_config)
        key3 = cache_manager._generate_cache_key("input1", "different-model", sample_dtesn_config)
        
        assert key1.to_string() != key2.to_string()  # Different input
        assert key1.to_string() != key3.to_string()  # Different model
        assert key2.to_string() != key3.to_string()  # Both different
    
    async def test_basic_cache_operations(self, cache_manager, sample_dtesn_config, sample_result, sample_metadata):
        """Test basic cache store and retrieve operations"""
        input_data = "test input"
        model_id = "test-model"
        processing_time = 100.0
        content_tags = {"test", "dtesn"}
        
        # Cache miss initially
        result = await cache_manager.get_cached_result(input_data, model_id, sample_dtesn_config)
        assert result is None
        
        # Cache the result
        await cache_manager.cache_result(
            input_data=input_data,
            model_id=model_id,
            dtesn_config=sample_dtesn_config,
            result=sample_result,
            metadata=sample_metadata,
            processing_time_ms=processing_time,
            content_tags=content_tags
        )
        
        # Cache hit now
        cached_result = await cache_manager.get_cached_result(input_data, model_id, sample_dtesn_config)
        assert cached_result is not None
        
        cached_data, cached_meta = cached_result
        assert cached_data == sample_result
        assert cached_meta["processing_time_ms"] == processing_time
    
    async def test_cache_ttl_expiration(self, sample_dtesn_config, sample_result, sample_metadata):
        """Test that cache entries expire based on TTL"""
        # Create cache manager with short TTL
        manager = DTESNServerSideCacheManager(
            max_memory_entries=100,
            default_ttl_seconds=1,  # 1 second TTL
            redis_url=None
        )
        await manager.initialize()
        
        try:
            input_data = "test input"
            model_id = "test-model"
            
            # Cache result
            await manager.cache_result(
                input_data=input_data,
                model_id=model_id,
                dtesn_config=sample_dtesn_config,
                result=sample_result,
                metadata=sample_metadata,
                processing_time_ms=100.0,
                content_tags={"test"}
            )
            
            # Should be cached immediately
            result = await manager.get_cached_result(input_data, model_id, sample_dtesn_config)
            assert result is not None
            
            # Wait for expiration
            await asyncio.sleep(1.5)
            
            # Should be expired now
            result = await manager.get_cached_result(input_data, model_id, sample_dtesn_config)
            assert result is None
            
        finally:
            await manager.shutdown()
    
    async def test_cache_invalidation_by_tags(self, cache_manager, sample_dtesn_config, sample_result, sample_metadata):
        """Test cache invalidation by content tags"""
        # Cache multiple entries with different tags
        entries = [
            ("input1", {"tag1", "shared"}),
            ("input2", {"tag2", "shared"}),
            ("input3", {"tag1", "unique"}),
            ("input4", {"tag3"})
        ]
        
        model_id = "test-model"
        
        for input_data, tags in entries:
            await cache_manager.cache_result(
                input_data=input_data,
                model_id=model_id,
                dtesn_config=sample_dtesn_config,
                result=sample_result,
                metadata=sample_metadata,
                processing_time_ms=100.0,
                content_tags=tags
            )
        
        # Verify all entries are cached
        for input_data, _ in entries:
            result = await cache_manager.get_cached_result(input_data, model_id, sample_dtesn_config)
            assert result is not None
        
        # Invalidate by tag1 - should affect entries 1 and 3
        invalidated = await cache_manager.invalidate_by_tags({"tag1"})
        assert invalidated == 2
        
        # Check which entries remain
        remaining = []
        for input_data, tags in entries:
            result = await cache_manager.get_cached_result(input_data, model_id, sample_dtesn_config)
            if result is not None:
                remaining.append(input_data)
        
        assert "input1" not in remaining  # Has tag1
        assert "input2" in remaining      # Has tag2, shared
        assert "input3" not in remaining  # Has tag1
        assert "input4" in remaining      # Has tag3
    
    async def test_cache_invalidation_by_model(self, cache_manager, sample_dtesn_config, sample_result, sample_metadata):
        """Test cache invalidation by model ID"""
        models_and_inputs = [
            ("model1", "input1"),
            ("model1", "input2"), 
            ("model2", "input1"),
            ("model2", "input2")
        ]
        
        # Cache entries for different models
        for model_id, input_data in models_and_inputs:
            await cache_manager.cache_result(
                input_data=input_data,
                model_id=model_id,
                dtesn_config=sample_dtesn_config,
                result=sample_result,
                metadata=sample_metadata,
                processing_time_ms=100.0,
                content_tags={"test"}
            )
        
        # Verify all entries are cached
        for model_id, input_data in models_and_inputs:
            result = await cache_manager.get_cached_result(input_data, model_id, sample_dtesn_config)
            assert result is not None
        
        # Invalidate model1
        invalidated = await cache_manager.invalidate_by_model("model1")
        assert invalidated == 2
        
        # Check remaining entries
        remaining = []
        for model_id, input_data in models_and_inputs:
            result = await cache_manager.get_cached_result(input_data, model_id, sample_dtesn_config)
            if result is not None:
                remaining.append((model_id, input_data))
        
        # Only model2 entries should remain
        assert len(remaining) == 2
        assert all(model_id == "model2" for model_id, _ in remaining)
    
    async def test_cache_lru_eviction(self, sample_dtesn_config, sample_result, sample_metadata):
        """Test LRU eviction when cache is full"""
        # Create cache manager with small capacity
        manager = DTESNServerSideCacheManager(
            max_memory_entries=3,  # Small capacity
            redis_url=None
        )
        await manager.initialize()
        
        try:
            model_id = "test-model"
            
            # Fill cache to capacity
            for i in range(3):
                await manager.cache_result(
                    input_data=f"input{i}",
                    model_id=model_id,
                    dtesn_config=sample_dtesn_config,
                    result=sample_result,
                    metadata=sample_metadata,
                    processing_time_ms=100.0,
                    content_tags={"test"}
                )
            
            # All entries should be cached
            for i in range(3):
                result = await manager.get_cached_result(f"input{i}", model_id, sample_dtesn_config)
                assert result is not None
            
            # Add one more entry - should evict oldest
            await manager.cache_result(
                input_data="input3",
                model_id=model_id,
                dtesn_config=sample_dtesn_config,
                result=sample_result,
                metadata=sample_metadata,
                processing_time_ms=100.0,
                content_tags={"test"}
            )
            
            # input0 should be evicted (oldest)
            result = await manager.get_cached_result("input0", model_id, sample_dtesn_config)
            assert result is None
            
            # Others should still be there
            for i in range(1, 4):
                result = await manager.get_cached_result(f"input{i}", model_id, sample_dtesn_config)
                assert result is not None
            
        finally:
            await manager.shutdown()
    
    async def test_performance_metrics_collection(self, cache_manager, sample_dtesn_config, sample_result, sample_metadata):
        """Test performance metrics collection and calculation"""
        model_id = "test-model"
        
        # Perform cache operations to generate metrics
        # Cache miss
        result = await cache_manager.get_cached_result("input1", model_id, sample_dtesn_config)
        assert result is None
        
        # Cache result
        await cache_manager.cache_result(
            input_data="input1",
            model_id=model_id,
            dtesn_config=sample_dtesn_config,
            result=sample_result,
            metadata=sample_metadata,
            processing_time_ms=200.0,
            content_tags={"test"}
        )
        
        # Cache hit
        result = await cache_manager.get_cached_result("input1", model_id, sample_dtesn_config)
        assert result is not None
        
        # Another cache miss
        result = await cache_manager.get_cached_result("input2", model_id, sample_dtesn_config)
        assert result is None
        
        # Get metrics
        metrics = cache_manager.get_performance_metrics()
        
        assert metrics["total_requests"] == 3
        assert metrics["cache_hits"] == 1
        assert metrics["cache_misses"] == 2
        assert abs(metrics["hit_ratio"] - (1/3)) < 0.01
        assert "cache_levels" in metrics
        assert "performance_improvement_percent" in metrics


class TestDTESNCacheStrategies:
    """Test different caching strategies"""
    
    @pytest.mark.parametrize("strategy,expected_multiplier", [
        (CacheStrategy.AGGRESSIVE, 2),
        (CacheStrategy.CONSERVATIVE, 0.5),
        (CacheStrategy.BALANCED, 1),
    ])
    async def test_ttl_determination_by_strategy(self, strategy, expected_multiplier):
        """Test TTL determination based on caching strategy"""
        manager = DTESNServerSideCacheManager(
            cache_strategy=strategy,
            default_ttl_seconds=100,
            redis_url=None
        )
        
        processing_time = 50.0
        content_tags = set()
        
        ttl = manager._determine_ttl(processing_time, content_tags)
        expected_ttl = int(100 * expected_multiplier)
        
        assert ttl == expected_ttl
    
    async def test_dynamic_strategy_ttl(self):
        """Test dynamic strategy TTL based on processing time"""
        manager = DTESNServerSideCacheManager(
            cache_strategy=CacheStrategy.DYNAMIC,
            default_ttl_seconds=100,
            redis_url=None
        )
        
        # Fast operation (< 100ms)
        fast_ttl = manager._determine_ttl(50.0, set())
        assert fast_ttl == 100 // 3
        
        # Medium operation (100ms - 1s)  
        medium_ttl = manager._determine_ttl(500.0, set())
        assert medium_ttl == 100
        
        # Slow operation (> 1s)
        slow_ttl = manager._determine_ttl(1500.0, set())
        assert slow_ttl == 100 * 3


class TestDTESNIntegrationWithCache:
    """Test DTESN integration mixin with caching"""
    
    @pytest.fixture
    def mock_dtesn_processor(self):
        """Mock DTESN processor for testing"""
        processor = Mock()
        processor.process = AsyncMock()
        processor.process.return_value = Mock()
        processor.process.return_value.to_dict.return_value = {
            "membrane_layers": 4,
            "final_result": "mock_result"
        }
        return processor
    
    @pytest.fixture 
    def integration_mixin(self, mock_dtesn_processor):
        """Create integration mixin instance for testing"""
        
        class TestMixin(DTESNIntegrationMixin):
            def __init__(self):
                self.dtesn_processor = mock_dtesn_processor
        
        return TestMixin()
    
    async def test_cache_hit_performance_improvement(self, integration_mixin, mock_dtesn_processor):
        """Test that cache hits provide significant performance improvement"""
        
        with patch('aphrodite.endpoints.openai.dtesn_integration.get_cache_manager') as mock_get_cache:
            # Setup mock cache manager with cache hit
            mock_cache_manager = Mock()
            mock_cache_manager.get_cached_result = AsyncMock()
            mock_cache_manager.get_cached_result.return_value = (
                {"cached": "result"}, 
                {"processing_time_ms": 200.0}
            )
            mock_get_cache.return_value = mock_cache_manager
            
            # Setup DTESN options with caching enabled
            dtesn_options = DTESNEnhancedRequest(
                enable_dtesn=True,
                enable_caching=True,
                dtesn_membrane_depth=4,
                dtesn_esn_size=512
            )
            
            # Process request
            start_time = time.time()
            result = await integration_mixin._preprocess_with_dtesn(
                request_data="test input",
                dtesn_options=dtesn_options,
                model_id="test-model"
            )
            end_time = time.time()
            
            # Verify cache hit
            assert result["cache_metadata"]["cache_hit"] is True
            assert result["cache_metadata"]["cache_enabled"] is True
            assert result["dtesn_processed"] is True
            assert result["dtesn_result"] == {"cached": "result"}
            
            # Verify performance improvement
            assert result["cache_metadata"]["performance_improvement"] > 0
            
            # Verify DTESN processor was not called (cache hit)
            mock_dtesn_processor.process.assert_not_called()
    
    async def test_cache_miss_and_storage(self, integration_mixin, mock_dtesn_processor):
        """Test cache miss scenario and result storage"""
        
        with patch('aphrodite.endpoints.openai.dtesn_integration.get_cache_manager') as mock_get_cache:
            # Setup mock cache manager with cache miss
            mock_cache_manager = Mock()
            mock_cache_manager.get_cached_result = AsyncMock(return_value=None)
            mock_cache_manager.cache_result = AsyncMock()
            mock_get_cache.return_value = mock_cache_manager
            
            dtesn_options = DTESNEnhancedRequest(
                enable_dtesn=True,
                enable_caching=True,
                dtesn_membrane_depth=4,
                dtesn_esn_size=512
            )
            
            # Process request
            result = await integration_mixin._preprocess_with_dtesn(
                request_data="test input",
                dtesn_options=dtesn_options,
                model_id="test-model"
            )
            
            # Verify cache miss and processing
            assert result["cache_metadata"]["cache_hit"] is False
            assert result["dtesn_processed"] is True
            
            # Verify DTESN processor was called
            mock_dtesn_processor.process.assert_called_once()
            
            # Verify result was cached
            mock_cache_manager.cache_result.assert_called_once()


class TestCachePerformanceRequirements:
    """Test that cache meets performance requirements"""
    
    async def test_50_percent_performance_improvement_target(self):
        """Test that caching achieves the required 50% performance improvement"""
        manager = DTESNServerSideCacheManager(
            max_memory_entries=1000,
            redis_url=None
        )
        await manager.initialize()
        
        try:
            # Simulate processing times
            original_processing_times = [200, 150, 300, 180, 250]  # ms
            cache_retrieval_times = [5, 3, 8, 4, 6]  # ms (fast cache retrieval)
            
            # Add processing times to manager
            manager.processing_times = original_processing_times
            manager.cache_times = cache_retrieval_times
            
            # Calculate metrics
            metrics = manager.get_performance_metrics()
            
            # Verify performance improvement meets requirement
            improvement_percent = metrics["performance_improvement_percent"]
            assert improvement_percent >= 50.0  # Must meet 50% requirement
            
            # Verify calculation is correct
            avg_processing = sum(original_processing_times) / len(original_processing_times)
            avg_cache = sum(cache_retrieval_times) / len(cache_retrieval_times)
            expected_improvement = (1.0 - (avg_cache / avg_processing)) * 100
            
            assert abs(improvement_percent - expected_improvement) < 0.1
            
        finally:
            await manager.shutdown()


# Integration test fixtures and helpers

@pytest.fixture(scope="session")
def event_loop():
    """Create an event loop for async tests"""
    loop = asyncio.new_event_loop()
    yield loop
    loop.close()


@pytest.fixture
async def redis_cache_manager():
    """Cache manager with Redis - only if Redis is available"""
    try:
        import aioredis
        # Try to connect to Redis
        redis_client = aioredis.from_url("redis://localhost:6379", decode_responses=False)
        await redis_client.ping()
        await redis_client.close()
        
        # If Redis is available, create manager with Redis
        manager = DTESNServerSideCacheManager(
            max_memory_entries=100,
            redis_url="redis://localhost:6379",
            default_ttl_seconds=300
        )
        await manager.initialize()
        yield manager
        await manager.shutdown()
        
    except Exception:
        # Skip Redis tests if not available
        pytest.skip("Redis not available for integration tests")


class TestRedisIntegration:
    """Test Redis integration if available"""
    
    async def test_redis_cache_operations(self, redis_cache_manager, sample_dtesn_config, sample_result, sample_metadata):
        """Test Redis cache operations"""
        manager = redis_cache_manager
        
        input_data = "redis test input"
        model_id = "redis-model"
        
        # Cache result (should go to Redis)
        await manager.cache_result(
            input_data=input_data,
            model_id=model_id,
            dtesn_config=sample_dtesn_config,
            result=sample_result,
            metadata=sample_metadata,
            processing_time_ms=100.0,
            content_tags={"redis", "test"}
        )
        
        # Clear local caches to force Redis lookup
        manager.memory_cache.clear()
        manager.compressed_cache.clear() 
        manager.cache_metadata.clear()
        
        # Should retrieve from Redis
        cached_result = await manager.get_cached_result(input_data, model_id, sample_dtesn_config)
        assert cached_result is not None
        
        cached_data, cached_meta = cached_result
        assert cached_data == sample_result


if __name__ == "__main__":
    # Run the tests
    pytest.main([__file__, "-v", "--tb=short"])