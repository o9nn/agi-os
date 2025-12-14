"""
Tests for Model Serving Integration - Task 8.1.1 Implementation.

Tests the comprehensive model serving infrastructure including:
- Server-side model loading and caching strategies
- Model versioning with zero-downtime updates
- Resource-aware model allocation for DTESN operations
- Integration with existing DTESN components
"""

import asyncio
import pytest
import time
from unittest.mock import AsyncMock, Mock, patch

from aphrodite.endpoints.deep_tree_echo.model_serving_manager import ModelServingManager
from aphrodite.endpoints.deep_tree_echo.model_serving_routes import create_model_serving_routes


class TestModelServingManager:
    """Test the core model serving manager functionality."""
    
    @pytest.fixture
    def mock_engine(self):
        """Create a mock AsyncAphrodite engine."""
        engine = AsyncMock()
        
        # Mock model config
        mock_config = Mock()
        mock_config.model = "test-model-7b"
        mock_config.max_model_len = 4096
        mock_config.dtype = "float16"
        mock_config.vocab_size = 32000
        mock_config.hidden_size = 4096
        
        engine.get_model_config = AsyncMock(return_value=mock_config)
        engine.get_tokenizer = Mock(return_value=Mock())
        
        return engine
    
    @pytest.fixture
    def model_serving_manager(self, mock_engine):
        """Create a model serving manager with mock engine."""
        return ModelServingManager(engine=mock_engine)
    
    @pytest.mark.asyncio
    async def test_load_model_async_basic(self, model_serving_manager):
        """Test basic model loading functionality."""
        model_config = await model_serving_manager.load_model_async("test-model", "v1.0")
        
        assert model_config["model_id"] == "test-model"
        assert model_config["version"] == "v1.0"
        assert model_config["dtesn_optimizations"]["membrane_depth_optimization"] is True
        assert model_config["engine_integration"]["available"] is True
        assert model_config["serving_config"]["zero_downtime_capable"] is True
    
    @pytest.mark.asyncio
    async def test_load_model_caching(self, model_serving_manager):
        """Test model caching functionality."""
        # First load
        start_time = time.time()
        config1 = await model_serving_manager.load_model_async("test-model", "v1.0")
        first_load_time = time.time() - start_time
        
        # Second load (should be cached)
        start_time = time.time()
        config2 = await model_serving_manager.load_model_async("test-model", "v1.0")
        cached_load_time = time.time() - start_time
        
        # Verify caching worked
        assert cached_load_time < first_load_time
        assert config1 == config2
        assert model_serving_manager.performance_metrics["cache_hit_rate"] > 0
    
    @pytest.mark.asyncio
    async def test_dtesn_optimizations(self, model_serving_manager):
        """Test DTESN-specific optimizations are applied."""
        model_config = await model_serving_manager.load_model_async("test-model-7b", "v1.0")
        
        optimizations = model_config["dtesn_optimizations"]
        
        # Check core DTESN optimizations
        assert optimizations["membrane_depth_optimization"] is True
        assert optimizations["esn_reservoir_integration"] is True
        assert optimizations["b_series_computation"] is True
        assert optimizations["p_system_acceleration"] is True
        
        # Check model-specific optimizations
        assert optimizations["small_model_optimizations"] is True
        assert "recommended_membrane_depth" in optimizations
        assert "recommended_reservoir_size" in optimizations
    
    @pytest.mark.asyncio
    async def test_resource_aware_allocation(self, model_serving_manager):
        """Test resource-aware memory allocation calculations."""
        model_config = await model_serving_manager.load_model_async("test-model-7b", "v1.0")
        
        memory_usage = model_config["memory_usage"]
        
        # Check memory allocation structure
        assert "model_memory_gb" in memory_usage
        assert "cache_memory_gb" in memory_usage
        assert "dtesn_memory_gb" in memory_usage
        assert "total_estimated_gb" in memory_usage
        
        # Check resource awareness
        assert memory_usage["resource_aware"] is True
        assert memory_usage["engine_integrated"] is True
        assert memory_usage["allocation_strategy"] is not None
    
    @pytest.mark.asyncio
    async def test_zero_downtime_update_success(self, model_serving_manager):
        """Test successful zero-downtime model update."""
        # Load initial model
        await model_serving_manager.load_model_async("test-model", "v1.0")
        
        # Mock health check to pass
        with patch.object(model_serving_manager, '_health_check_model', return_value=True):
            # Perform zero-downtime update
            success = await model_serving_manager.update_model_zero_downtime("test-model", "v2.0")
            
            assert success is True
            assert model_serving_manager.model_versions["test-model"] == "v2.0"
            assert model_serving_manager.performance_metrics["zero_downtime_updates"] == 1
    
    @pytest.mark.asyncio
    async def test_zero_downtime_update_failure_rollback(self, model_serving_manager):
        """Test zero-downtime update failure and automatic rollback."""
        # Load initial model
        await model_serving_manager.load_model_async("test-model", "v1.0")
        original_version = model_serving_manager.model_versions["test-model"]
        
        # Mock health check to fail
        with patch.object(model_serving_manager, '_health_check_model', return_value=False):
            # Perform zero-downtime update (should fail)
            success = await model_serving_manager.update_model_zero_downtime("test-model", "v2.0")
            
            assert success is False
            # Should rollback to original version
            assert model_serving_manager.model_versions["test-model"] == original_version
            assert "rollback_completed_at" in model_serving_manager.health_status["test-model"]
    
    @pytest.mark.asyncio
    async def test_health_check_comprehensive(self, model_serving_manager):
        """Test comprehensive model health checking."""
        # Load a model
        await model_serving_manager.load_model_async("test-model", "v1.0")
        
        # Perform health check
        health_result = await model_serving_manager._health_check_model("test-model", "v1.0")
        
        assert health_result is True
        
        # Check health status was updated
        health_status = model_serving_manager.health_status["test-model"]
        assert health_status["status"] == "healthy"
        assert health_status["checks_passed"] == 5
        assert health_status["health_score"] == 100
    
    @pytest.mark.asyncio
    async def test_performance_metrics_tracking(self, model_serving_manager):
        """Test performance metrics are properly tracked."""
        # Load multiple models to generate metrics
        await model_serving_manager.load_model_async("model1", "v1.0")
        await model_serving_manager.load_model_async("model2", "v1.0")
        await model_serving_manager.load_model_async("model1", "v1.0")  # Cache hit
        
        metrics = model_serving_manager.performance_metrics
        
        assert metrics["total_loads"] == 3
        assert metrics["successful_loads"] == 2  # Two unique models
        assert metrics["cache_hits"] == 1
        assert metrics["cache_hit_rate"] > 0
        assert metrics["average_load_time"] > 0
    
    @pytest.mark.asyncio
    async def test_model_removal(self, model_serving_manager):
        """Test model removal functionality."""
        # Load a model
        await model_serving_manager.load_model_async("test-model", "v1.0")
        assert "test-model:v1.0" in model_serving_manager.model_cache
        
        # Remove the model
        success = await model_serving_manager.remove_model("test-model", "v1.0")
        
        assert success is True
        assert "test-model:v1.0" not in model_serving_manager.model_cache
        assert model_serving_manager.health_status["test-model"]["status"] == "removed"
    
    def test_serving_status_comprehensive(self, model_serving_manager):
        """Test comprehensive serving status reporting."""
        status = model_serving_manager.get_model_serving_status()
        
        # Check status structure
        assert "overview" in status
        assert "performance_metrics" in status
        assert "health_summary" in status
        assert "load_balancer_status" in status
        assert "engine_integration" in status
        
        # Check overview data
        overview = status["overview"]
        assert "cached_models" in overview
        assert "active_versions" in overview
        assert "health_tracked_models" in overview
    
    @pytest.mark.asyncio
    async def test_model_list_functionality(self, model_serving_manager):
        """Test model listing functionality."""
        # Load some models
        await model_serving_manager.load_model_async("model1", "v1.0")
        await model_serving_manager.load_model_async("model2", "v2.0")
        
        # Get model list
        models = await model_serving_manager.list_available_models()
        
        assert len(models) == 2
        
        # Check model data structure
        model = models[0]
        assert "model_id" in model
        assert "version" in model
        assert "status" in model
        assert "memory_usage_gb" in model
        assert "dtesn_optimized" in model
        assert "engine_integrated" in model
        assert "zero_downtime_capable" in model


class TestModelServingRoutes:
    """Test the FastAPI routes for model serving."""
    
    @pytest.fixture
    def model_serving_manager_mock(self):
        """Create a mock model serving manager."""
        manager = Mock(spec=ModelServingManager)
        
        # Mock methods
        manager.get_model_serving_status.return_value = {
            "overview": {"cached_models": 2},
            "performance_metrics": {"total_loads": 5},
            "health_summary": {"healthy_models": 2}
        }
        
        manager.load_model_async = AsyncMock(return_value={
            "model_id": "test-model",
            "version": "v1.0",
            "dtesn_optimizations": {"membrane_depth_optimization": True},
            "engine_integration": {"available": True}
        })
        
        manager.list_available_models = AsyncMock(return_value=[
            {
                "model_id": "test-model",
                "version": "v1.0",
                "status": "healthy",
                "dtesn_optimized": True,
                "engine_integrated": True
            }
        ])
        
        return manager
    
    @pytest.fixture
    def routes(self, model_serving_manager_mock):
        """Create model serving routes with mock manager."""
        return create_model_serving_routes(model_serving_manager_mock)
    
    def test_routes_creation(self, routes):
        """Test that routes are created successfully."""
        assert routes is not None
        
        # Check that routes exist
        route_paths = [route.path for route in routes.routes]
        expected_paths = [
            "/model_serving/status",
            "/model_serving/load", 
            "/model_serving/update",
            "/model_serving/models",
            "/model_serving/models/{model_id}",
            "/model_serving/health/{model_id}",
            "/model_serving/metrics"
        ]
        
        for expected_path in expected_paths:
            assert any(expected_path in path for path in route_paths)


class TestIntegrationWithDTESN:
    """Test integration between model serving and DTESN processing."""
    
    @pytest.mark.asyncio
    async def test_dtesn_processor_with_model_serving(self):
        """Test DTESN processor integration with model serving manager."""
        from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
        from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
        
        # Create mock engine
        mock_engine = AsyncMock()
        mock_config = Mock()
        mock_config.model = "test-model-7b"
        mock_config.max_model_len = 4096
        mock_engine.get_model_config = AsyncMock(return_value=mock_config)
        
        # Create DTESN config
        config = DTESNConfig()
        
        # Create processor with model serving manager
        try:
            processor = DTESNProcessor(config=config, engine=mock_engine)
            
            # Verify model serving manager is initialized
            assert hasattr(processor, 'model_serving_manager')
            assert processor.model_serving_manager.engine == mock_engine
            
        except RuntimeError:
            # Expected if echo.kern components are not available
            pytest.skip("Echo.kern components not available for integration test")
    
    @pytest.mark.asyncio 
    async def test_model_serving_performance_impact(self):
        """Test that model serving integration doesn't negatively impact DTESN performance."""
        mock_engine = AsyncMock()
        model_serving_manager = ModelServingManager(engine=mock_engine)
        
        # Load a model
        start_time = time.time()
        await model_serving_manager.load_model_async("performance-test-model", "v1.0")
        load_time = time.time() - start_time
        
        # Verify reasonable load time (< 1 second for mock)
        assert load_time < 1.0
        
        # Verify performance metrics are tracked
        metrics = model_serving_manager.performance_metrics
        assert metrics["average_load_time"] > 0
        assert metrics["total_loads"] == 1


class TestProductionReadiness:
    """Test production readiness aspects of model serving infrastructure."""
    
    @pytest.mark.asyncio
    async def test_error_handling_robustness(self):
        """Test robust error handling in model serving operations."""
        # Create manager with engine that raises exceptions
        faulty_engine = AsyncMock()
        faulty_engine.get_model_config.side_effect = Exception("Engine failure")
        
        model_serving_manager = ModelServingManager(engine=faulty_engine)
        
        # Should handle engine errors gracefully
        model_config = await model_serving_manager.load_model_async("test-model", "v1.0")
        
        # Should still create configuration without engine integration
        assert model_config["model_id"] == "test-model"
        assert model_config["engine_integration"]["available"] is True  # Basic availability
        assert model_config["dtesn_optimizations"]["membrane_depth_optimization"] is True
    
    @pytest.mark.asyncio
    async def test_concurrent_model_operations(self):
        """Test concurrent model operations don't interfere with each other."""
        mock_engine = AsyncMock()
        model_serving_manager = ModelServingManager(engine=mock_engine)
        
        # Start multiple concurrent model loads
        load_tasks = [
            model_serving_manager.load_model_async(f"model-{i}", "v1.0")
            for i in range(5)
        ]
        
        # Wait for all to complete
        results = await asyncio.gather(*load_tasks)
        
        # Verify all succeeded with correct data
        assert len(results) == 5
        for i, result in enumerate(results):
            assert result["model_id"] == f"model-{i}"
            assert result["version"] == "v1.0"
    
    @pytest.mark.asyncio
    async def test_memory_usage_tracking(self):
        """Test memory usage is properly tracked and reported.""" 
        mock_engine = AsyncMock()
        model_serving_manager = ModelServingManager(engine=mock_engine)
        
        # Load models and check memory tracking
        await model_serving_manager.load_model_async("small-model-7b", "v1.0")
        await model_serving_manager.load_model_async("large-model-70b", "v1.0")
        
        # Check resource allocation
        small_allocation = await model_serving_manager.get_model_resource_allocation("small-model-7b")
        large_allocation = await model_serving_manager.get_model_resource_allocation("large-model-70b")
        
        # Large model should have higher memory allocation
        small_memory = small_allocation["memory_usage"]["total_estimated_gb"]
        large_memory = large_allocation["memory_usage"]["total_estimated_gb"]
        
        assert large_memory > small_memory
        assert small_memory > 0
    
    def test_health_monitoring_comprehensive(self):
        """Test comprehensive health monitoring capabilities."""
        mock_engine = AsyncMock()
        model_serving_manager = ModelServingManager(engine=mock_engine)
        
        # Check initial health status
        status = model_serving_manager.get_model_serving_status()
        health_summary = status["health_summary"]
        
        assert "healthy_models" in health_summary
        assert "total_models" in health_summary
        assert "models_with_issues" in health_summary
        
        # Check engine integration status
        engine_integration = status["engine_integration"]
        assert "engine_available" in engine_integration
        assert "integrated_models" in engine_integration


if __name__ == "__main__":
    pytest.main([__file__, "-v"])