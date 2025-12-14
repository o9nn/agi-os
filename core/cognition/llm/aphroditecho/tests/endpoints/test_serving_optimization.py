"""
Tests for server-side model optimization serving endpoints.

Tests the FastAPI endpoints and integration with the optimization system
for comprehensive server-side model optimization functionality.
"""

import asyncio
import pytest
import time
from unittest.mock import Mock, AsyncMock, patch, MagicMock
from typing import Dict, Any
import json

from fastapi import FastAPI
from fastapi.testclient import TestClient
import torch

from aphrodite.endpoints.openai.serving_optimization import (
    OptimizationServingMixin,
    OptimizationRequest,
    OptimizationResponse,
    LoadMetricsResponse,
    EnsembleStatusResponse,
    PerformanceReportResponse,
    OptimizationMiddleware,
    setup_optimization_serving,
    create_optimized_request_context
)
from aphrodite.engine.server_side_optimizer import (
    ServerSideOptimizer,
    OptimizationConfig,
    ServerLoadMetrics
)


class MockAsyncAphrodite:
    """Mock AsyncAphrodite engine for testing."""
    
    def __init__(self):
        self.engine = Mock()
        self.engine.model_config = Mock()
        self.engine.model_config.model = "test-model"


@pytest.fixture
def mock_engine():
    """Create mock AsyncAphrodite engine."""
    return MockAsyncAphrodite()


@pytest.fixture 
def mock_optimizer():
    """Create mock ServerSideOptimizer."""
    optimizer = Mock(spec=ServerSideOptimizer)
    optimizer.config = OptimizationConfig()
    optimizer.start_optimization = AsyncMock()
    optimizer.stop_optimization = AsyncMock()
    optimizer.get_optimization_status = Mock(return_value={
        "running": True,
        "configuration": {
            "torch_compile_enabled": True,
            "dynamic_tuning_enabled": True,
            "ensemble_enabled": True,
            "auto_scaling_enabled": False
        },
        "current_strategy": {
            "strategy": "balanced",
            "load_score": 0.5
        },
        "compilation_stats": {},
        "ensemble_status": {
            "enabled": True,
            "model_count": 2,
            "models": []
        },
        "performance_metrics_count": 100,
        "model_manager_status": {"status": "active"}
    })
    
    # Mock parameter tuner
    optimizer.parameter_tuner = Mock()
    optimizer.parameter_tuner.collect_load_metrics = Mock(return_value=ServerLoadMetrics(
        timestamp=time.time(),
        cpu_usage_percent=65.0,
        memory_usage_percent=70.0,
        gpu_utilization=80.0,
        active_requests=15,
        queue_depth=3,
        throughput_tokens_per_sec=120.0,
        avg_latency_ms=150.0,
        error_rate=0.02
    ))
    
    # Mock ensemble manager
    optimizer.ensemble_manager = Mock()
    optimizer.ensemble_manager.get_ensemble_status = Mock(return_value={
        "enabled": True,
        "strategy": "weighted_voting",
        "model_count": 2,
        "max_size": 3,
        "models": [
            {
                "model_id": "model1",
                "weight": 1.0,
                "request_count": 100,
                "avg_latency_ms": 120.0,
                "error_rate": 0.01,
                "last_used": time.time(),
                "added_timestamp": time.time() - 3600
            },
            {
                "model_id": "model2", 
                "weight": 0.8,
                "request_count": 80,
                "avg_latency_ms": 140.0,
                "error_rate": 0.02,
                "last_used": time.time() - 60,
                "added_timestamp": time.time() - 1800
            }
        ]
    })
    optimizer.ensemble_manager.add_model_to_ensemble = Mock()
    
    # Mock performance reporting
    optimizer.export_performance_report = Mock(return_value={
        "report_timestamp": time.time(),
        "optimization_config": optimizer.config.__dict__,
        "status": optimizer.get_optimization_status(),
        "performance_history": [],
        "model_versions": []
    })
    
    return optimizer


@pytest.fixture
def app_with_optimization(mock_engine):
    """Create FastAPI app with optimization endpoints."""
    app = FastAPI()
    
    # Set up optimization serving
    optimization_mixin = OptimizationServingMixin()
    optimization_mixin._setup_optimization_routes(app, mock_engine)
    
    return app, optimization_mixin


@pytest.fixture
def client(app_with_optimization):
    """Create test client."""
    app, mixin = app_with_optimization
    return TestClient(app), mixin


class TestOptimizationServingMixin:
    """Test OptimizationServingMixin functionality."""
    
    @pytest.mark.asyncio
    async def test_initialize_optimizer(self, mock_engine):
        """Test optimizer initialization."""
        mixin = OptimizationServingMixin()
        
        with patch('aphrodite.endpoints.openai.serving_optimization.ServerSideOptimizer') as mock_optimizer_class:
            mock_optimizer_class.return_value = Mock()
            
            await mixin._initialize_optimizer(mock_engine)
        
        mock_optimizer_class.assert_called_once()
        assert mixin.optimizer is not None
    
    @pytest.mark.asyncio
    async def test_optimize_request_processing_disabled(self):
        """Test optimization when disabled."""
        mixin = OptimizationServingMixin()
        mixin.optimizer = None
        mixin.optimization_enabled = False
        
        model = torch.nn.Linear(10, 5)
        context = {"model_id": "test"}
        
        result = await mixin.optimize_request_processing(model, context)
        assert result is model
    
    @pytest.mark.asyncio
    async def test_optimize_request_processing_enabled(self, mock_optimizer):
        """Test optimization when enabled."""
        mixin = OptimizationServingMixin()
        mixin.optimizer = mock_optimizer
        mixin.optimization_enabled = True
        
        # Mock optimal model selection
        optimal_model = torch.nn.Linear(5, 3)
        mock_optimizer.select_optimal_model = Mock(return_value=optimal_model)
        
        original_model = torch.nn.Linear(10, 5) 
        context = {"model_id": "test"}
        
        result = await mixin.optimize_request_processing(original_model, context)
        assert result is optimal_model
        mock_optimizer.select_optimal_model.assert_called_once_with(context)
    
    def test_record_request_metrics_disabled(self):
        """Test metrics recording when disabled."""
        mixin = OptimizationServingMixin()
        mixin.optimizer = None
        mixin.optimization_enabled = False
        
        # Should not raise any errors
        mixin.record_request_metrics("test", time.time(), time.time() + 0.1, True)
    
    def test_record_request_metrics_enabled(self, mock_optimizer):
        """Test metrics recording when enabled."""
        mixin = OptimizationServingMixin()
        mixin.optimizer = mock_optimizer
        mixin.optimization_enabled = True
        
        start_time = time.time()
        end_time = start_time + 0.15  # 150ms
        
        mixin.record_request_metrics("test_model", start_time, end_time, True, {"extra": "data"})
        
        mock_optimizer.record_request_performance.assert_called_once()
        args = mock_optimizer.record_request_performance.call_args
        assert args[1]["model_id"] == "test_model"
        assert args[1]["latency_ms"] == 150.0
        assert args[1]["success"] is True
        assert args[1]["additional_metrics"]["extra"] == "data"


class TestOptimizationEndpoints:
    """Test optimization API endpoints."""
    
    def test_configure_optimization_success(self, client, mock_optimizer):
        """Test successful optimization configuration."""
        test_client, mixin = client
        mixin.optimizer = mock_optimizer
        
        request_data = {
            "enable_torch_compile": False,
            "compile_mode": "max-autotune",
            "tuning_interval_sec": 60.0,
            "enable_ensemble": True,
            "max_ensemble_size": 5
        }
        
        response = test_client.post("/v1/optimization/configure", json=request_data)
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "optimization_status" in data
        assert "Updated 5 optimization settings" in data["message"]
    
    def test_configure_optimization_invalid_data(self, client):
        """Test optimization configuration with invalid data."""
        test_client, _ = client
        
        request_data = {
            "compile_mode": "invalid_mode",  # Invalid enum value
            "tuning_interval_sec": -1.0,    # Invalid range
            "max_ensemble_size": 0          # Invalid range
        }
        
        response = test_client.post("/v1/optimization/configure", json=request_data)
        assert response.status_code == 422  # Validation error
    
    def test_start_optimization_success(self, client, mock_optimizer):
        """Test starting optimization."""
        test_client, mixin = client
        mixin.optimizer = mock_optimizer
        mixin.optimization_enabled = False
        
        response = test_client.post("/v1/optimization/start")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "started" in data["message"]
        assert mixin.optimization_enabled is True
        mock_optimizer.start_optimization.assert_called_once()
    
    def test_start_optimization_already_running(self, client, mock_optimizer):
        """Test starting optimization when already running."""
        test_client, mixin = client
        mixin.optimizer = mock_optimizer
        mixin.optimization_enabled = True
        
        response = test_client.post("/v1/optimization/start")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "already running" in data["message"]
        # Should not call start again
        mock_optimizer.start_optimization.assert_not_called()
    
    def test_stop_optimization_success(self, client, mock_optimizer):
        """Test stopping optimization."""
        test_client, mixin = client
        mixin.optimizer = mock_optimizer
        mixin.optimization_enabled = True
        
        response = test_client.post("/v1/optimization/stop")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "stopped" in data["message"]
        assert mixin.optimization_enabled is False
        mock_optimizer.stop_optimization.assert_called_once()
    
    def test_stop_optimization_not_running(self, client):
        """Test stopping optimization when not running."""
        test_client, mixin = client
        mixin.optimizer = None
        mixin.optimization_enabled = False
        
        response = test_client.post("/v1/optimization/stop")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "not running" in data["message"]
    
    def test_get_optimization_status_with_optimizer(self, client, mock_optimizer):
        """Test getting optimization status with active optimizer."""
        test_client, mixin = client
        mixin.optimizer = mock_optimizer
        mixin.optimization_enabled = True
        
        response = test_client.get("/v1/optimization/status")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["optimization_status"]["enabled"] is True
        assert data["optimization_status"]["running"] is True
        assert "configuration" in data["optimization_status"]
    
    def test_get_optimization_status_no_optimizer(self, client):
        """Test getting optimization status without optimizer."""
        test_client, mixin = client
        mixin.optimizer = None
        
        response = test_client.get("/v1/optimization/status")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["optimization_status"]["enabled"] is False
    
    def test_get_load_metrics_success(self, client, mock_optimizer):
        """Test getting load metrics."""
        test_client, mixin = client
        mixin.optimizer = mock_optimizer
        
        response = test_client.get("/v1/optimization/metrics")
        
        assert response.status_code == 200
        data = response.json()
        assert "cpu_usage_percent" in data
        assert "memory_usage_percent" in data
        assert "gpu_utilization" in data
        assert "overall_load_score" in data
        assert data["cpu_usage_percent"] == 65.0
        assert data["active_requests"] == 15
    
    def test_get_load_metrics_no_optimizer(self, client):
        """Test getting load metrics without optimizer."""
        test_client, mixin = client
        mixin.optimizer = None
        
        response = test_client.get("/v1/optimization/metrics")
        assert response.status_code == 400
    
    def test_get_ensemble_status_success(self, client, mock_optimizer):
        """Test getting ensemble status."""
        test_client, mixin = client
        mixin.optimizer = mock_optimizer
        
        response = test_client.get("/v1/optimization/ensemble")
        
        assert response.status_code == 200
        data = response.json()
        assert data["enabled"] is True
        assert data["strategy"] == "weighted_voting"
        assert data["model_count"] == 2
        assert data["max_size"] == 3
        assert len(data["models"]) == 2
        
        # Check model details
        model1 = data["models"][0]
        assert model1["model_id"] == "model1"
        assert model1["weight"] == 1.0
        assert model1["request_count"] == 100
    
    def test_add_model_to_ensemble_success(self, client, mock_optimizer):
        """Test adding model to ensemble."""
        test_client, mixin = client
        mixin.optimizer = mock_optimizer
        
        response = test_client.post(
            "/v1/optimization/ensemble/add_model",
            params={"model_id": "new_model", "initial_weight": 0.9}
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "new_model added to ensemble" in data["message"]
        
        mock_optimizer.ensemble_manager.add_model_to_ensemble.assert_called_once()
        args = mock_optimizer.ensemble_manager.add_model_to_ensemble.call_args[1]
        assert args["model_id"] == "new_model"
        assert args["initial_weight"] == 0.9
    
    def test_get_performance_report_success(self, client, mock_optimizer):
        """Test getting performance report."""
        test_client, mixin = client
        mixin.optimizer = mock_optimizer
        
        response = test_client.get("/v1/optimization/report?export_file=false")
        
        assert response.status_code == 200
        data = response.json()
        assert "report_timestamp" in data
        assert "summary" in data
        assert "performance_grade" in data
        
        summary = data["summary"]
        assert "current_strategy" in summary
        assert "ensemble_models" in summary
        assert "compilation_enabled" in summary
        assert "success_rate" in summary
        
        # Should have a valid grade
        assert data["performance_grade"] in ["A+", "A", "A-", "B+", "B", "B-", "C+", "C", "D"]
    
    def test_force_recompile_success(self, client, mock_optimizer):
        """Test forcing model recompilation."""
        test_client, mixin = client
        mixin.optimizer = mock_optimizer
        
        # Mock compiled models
        mock_optimizer.model_compiler = Mock()
        mock_optimizer.model_compiler.compiled_models = {"test_model": Mock()}
        
        response = test_client.post(
            "/v1/optimization/force_recompile",
            params={"model_id": "test_model"}
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "marked for recompilation" in data["message"]
        
        # Should have removed from compiled models
        assert "test_model" not in mock_optimizer.model_compiler.compiled_models
    
    def test_endpoint_error_handling(self, client):
        """Test error handling in endpoints."""
        test_client, mixin = client
        
        # Cause an error by not initializing optimizer properly
        mixin.optimizer = Mock()
        mixin.optimizer.get_optimization_status.side_effect = Exception("Test error")
        
        response = test_client.get("/v1/optimization/status")
        assert response.status_code == 500


class TestOptimizationMiddleware:
    """Test OptimizationMiddleware functionality."""
    
    @pytest.fixture
    def mock_optimization_mixin(self):
        """Create mock optimization mixin."""
        mixin = Mock(spec=OptimizationServingMixin)
        mixin.optimize_request_processing = AsyncMock()
        mixin.record_request_metrics = Mock()
        return mixin
    
    @pytest.fixture
    def middleware(self, mock_optimization_mixin):
        """Create optimization middleware."""
        return OptimizationMiddleware(mock_optimization_mixin)
    
    @pytest.mark.asyncio
    async def test_process_request_success(self, middleware, mock_optimization_mixin):
        """Test successful request processing with optimization."""
        # Set up mocks
        original_model = torch.nn.Linear(10, 5)
        optimized_model = torch.nn.Linear(10, 5)
        mock_optimization_mixin.optimize_request_processing.return_value = optimized_model
        
        async def mock_process_func(model, request_data):
            return {"result": "success", "model_used": model is optimized_model}
        
        request_data = {
            "model": "test_model",
            "request_id": "req_123",
            "input": "test input"
        }
        
        result = await middleware.process_request(
            original_model, 
            request_data, 
            mock_process_func
        )
        
        # Should have called optimization
        mock_optimization_mixin.optimize_request_processing.assert_called_once()
        
        # Should have recorded metrics
        mock_optimization_mixin.record_request_metrics.assert_called_once()
        
        # Check recorded metrics
        call_args = mock_optimization_mixin.record_request_metrics.call_args[1]
        assert call_args["model_id"] == "test_model"
        assert call_args["success"] is True
        assert "request_id" in call_args["additional_metrics"]
        
        # Should return processing result
        assert result["result"] == "success"
        assert result["model_used"] is True  # Used optimized model
    
    @pytest.mark.asyncio
    async def test_process_request_error(self, middleware, mock_optimization_mixin):
        """Test request processing with error."""
        # Set up mocks
        original_model = torch.nn.Linear(10, 5)
        mock_optimization_mixin.optimize_request_processing.return_value = original_model
        
        async def mock_process_func(model, request_data):
            raise ValueError("Processing error")
        
        request_data = {"model": "test_model"}
        
        # Should raise the original error
        with pytest.raises(ValueError, match="Processing error"):
            await middleware.process_request(
                original_model,
                request_data,
                mock_process_func
            )
        
        # Should still record metrics for the failed request
        mock_optimization_mixin.record_request_metrics.assert_called_once()
        call_args = mock_optimization_mixin.record_request_metrics.call_args[1]
        assert call_args["success"] is False
        assert "error" in call_args["additional_metrics"]
        assert call_args["additional_metrics"]["error"] == "Processing error"


class TestUtilityFunctions:
    """Test utility functions."""
    
    def test_create_optimized_request_context(self):
        """Test creating optimization request context."""
        context = create_optimized_request_context(
            model_name="test-model",
            request_id="req_123",
            user_preferences={"quality": "high"},
            load_info={"cpu": 65.0}
        )
        
        assert context["model_id"] == "test-model"
        assert context["request_id"] == "req_123"
        assert context["user_preferences"]["quality"] == "high"
        assert context["load_info"]["cpu"] == 65.0
        assert "timestamp" in context
    
    def test_create_optimized_request_context_minimal(self):
        """Test creating context with minimal parameters."""
        context = create_optimized_request_context(
            model_name="test-model",
            request_id="req_123"
        )
        
        assert context["model_id"] == "test-model"
        assert context["request_id"] == "req_123"
        assert "timestamp" in context
        assert "user_preferences" not in context
        assert "load_info" not in context
    
    @pytest.mark.asyncio
    async def test_setup_optimization_serving(self, mock_engine):
        """Test setting up optimization serving."""
        app = FastAPI()
        
        mixin = await setup_optimization_serving(app, mock_engine)
        
        assert isinstance(mixin, OptimizationServingMixin)
        
        # Test that routes were added
        route_paths = [route.path for route in app.routes]
        expected_paths = [
            "/v1/optimization/configure",
            "/v1/optimization/start", 
            "/v1/optimization/stop",
            "/v1/optimization/status",
            "/v1/optimization/metrics",
            "/v1/optimization/ensemble",
            "/v1/optimization/ensemble/add_model",
            "/v1/optimization/report",
            "/v1/optimization/force_recompile"
        ]
        
        for expected_path in expected_paths:
            assert expected_path in route_paths


class TestModelRequestValidation:
    """Test request/response model validation."""
    
    def test_optimization_request_validation(self):
        """Test OptimizationRequest validation."""
        # Valid request
        valid_request = OptimizationRequest(
            enable_torch_compile=True,
            compile_mode="max-autotune",
            tuning_interval_sec=45.0,
            load_threshold_high=0.85,
            max_ensemble_size=4
        )
        assert valid_request.enable_torch_compile is True
        assert valid_request.compile_mode == "max-autotune"
        assert valid_request.tuning_interval_sec == 45.0
        
        # Test validation errors
        with pytest.raises(ValueError):
            OptimizationRequest(compile_mode="invalid_mode")
        
        with pytest.raises(ValueError):
            OptimizationRequest(tuning_interval_sec=0.5)  # Below minimum
        
        with pytest.raises(ValueError):
            OptimizationRequest(load_threshold_high=1.5)  # Above maximum
    
    def test_load_metrics_response_structure(self):
        """Test LoadMetricsResponse structure."""
        response = LoadMetricsResponse(
            timestamp=time.time(),
            cpu_usage_percent=75.0,
            memory_usage_percent=60.0,
            gpu_utilization=85.0,
            active_requests=20,
            queue_depth=5,
            throughput_tokens_per_sec=150.0,
            avg_latency_ms=120.0,
            error_rate=0.02,
            overall_load_score=0.7
        )
        
        assert response.cpu_usage_percent == 75.0
        assert response.overall_load_score == 0.7
        
        # Test serialization
        response_dict = response.dict()
        assert "timestamp" in response_dict
        assert "cpu_usage_percent" in response_dict
        assert "overall_load_score" in response_dict
    
    def test_ensemble_status_response_structure(self):
        """Test EnsembleStatusResponse structure."""
        models_data = [
            {
                "model_id": "model1",
                "weight": 1.0,
                "request_count": 100,
                "avg_latency_ms": 120.0,
                "error_rate": 0.01
            }
        ]
        
        response = EnsembleStatusResponse(
            enabled=True,
            strategy="weighted_voting",
            model_count=1,
            max_size=3,
            models=models_data
        )
        
        assert response.enabled is True
        assert response.strategy == "weighted_voting"
        assert len(response.models) == 1
        assert response.models[0]["model_id"] == "model1"


if __name__ == "__main__":
    pytest.main([__file__])