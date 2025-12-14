"""
Comprehensive Test Suite for A/B Testing Framework
Phase 8 - SSR-Focused MLOps & Production Observability

Tests for server-side A/B testing functionality including:
- Traffic splitting and performance comparison  
- Automated rollback mechanisms
- Model variant management
"""
import asyncio
import json
import pytest
import time
from unittest.mock import Mock, AsyncMock, patch
from fastapi.testclient import TestClient
from fastapi import FastAPI, Request
from starlette.responses import JSONResponse

from aphrodite.endpoints.middleware.ab_testing_middleware import (
    ABTestingManager, ABTestConfig, ABTestMetrics, ABTestResult, 
    ABTestingMiddleware
)
from aphrodite.endpoints.ab_testing_monitor import (
    ABTestMonitor, AlertLevel, MonitoringAlert, get_auto_rollback_system
)
from aphrodite.endpoints.model_variant_manager import ModelVariantManager, ModelVariant
from aphrodite.endpoints.openai.serving_ab_testing import router as ab_testing_router


class TestABTestingManager:
    """Test cases for A/B testing manager"""
    
    def setup_method(self):
        """Setup test fixtures"""
        self.config = ABTestConfig(
            enabled=False,
            traffic_split_percent=10.0,
            model_a_name="stable-model",
            model_b_name="canary-model",
            test_duration_minutes=60,
            auto_rollback=True
        )
        self.ab_manager = ABTestingManager()
        self.ab_manager.config = self.config
    
    def test_ab_test_config_initialization(self):
        """Test A/B test configuration initialization"""
        config = ABTestConfig()
        assert config.enabled is False
        assert config.traffic_split_percent == 10.0
        assert config.auto_rollback is True
        assert "max_error_rate_increase_percent" in config.success_criteria
        assert "max_error_rate_percent" in config.failure_criteria
    
    def test_traffic_splitting_logic(self):
        """Test traffic splitting based on user identifier"""
        # Mock request
        request = Mock()
        request.headers = {"x-session-id": "user123"}
        request.client = None
        
        # Enable A/B testing
        self.ab_manager.config.enabled = True
        self.ab_manager.active_test = {"test_id": "test123"}
        self.ab_manager.config.traffic_split_percent = 50.0
        
        # Test consistent splitting
        results = []
        for i in range(100):
            request.headers = {"x-session-id": f"user{i}"}
            results.append(self.ab_manager.should_use_variant_b(request))
        
        # Should be roughly 50% split (allow for some variation)
        variant_b_count = sum(results)
        assert 35 <= variant_b_count <= 65, f"Got {variant_b_count}% variant B, expected ~50%"
    
    @pytest.mark.asyncio
    async def test_start_ab_test(self):
        """Test starting an A/B test"""
        test_id = await self.ab_manager.start_ab_test("model-a", "model-b")
        
        assert test_id.startswith("ab_test_")
        assert self.ab_manager.active_test is not None
        assert self.ab_manager.active_test["model_a"] == "model-a"
        assert self.ab_manager.active_test["model_b"] == "model-b"
        assert self.ab_manager.config.enabled is True
    
    @pytest.mark.asyncio
    async def test_stop_ab_test(self):
        """Test stopping an A/B test"""
        # Start test first
        await self.ab_manager.start_ab_test("model-a", "model-b")
        
        # Add some metrics
        self.ab_manager.record_request_metrics("a", 100.0, True)
        self.ab_manager.record_request_metrics("b", 120.0, True)
        
        result = await self.ab_manager.stop_ab_test("test_complete")
        
        assert isinstance(result, ABTestResult)
        assert result.status == "completed"
        assert result.metrics_a.request_count == 1
        assert result.metrics_b.request_count == 1
        assert self.ab_manager.active_test is None
        assert self.ab_manager.config.enabled is False
    
    def test_metrics_recording(self):
        """Test request metrics recording"""
        # Record successful requests
        self.ab_manager.record_request_metrics("a", 100.0, True)
        self.ab_manager.record_request_metrics("a", 150.0, True)
        self.ab_manager.record_request_metrics("b", 120.0, True)
        
        # Record failed request
        self.ab_manager.record_request_metrics("b", 200.0, False)
        
        # Check metrics
        assert self.ab_manager.metrics_a.request_count == 2
        assert self.ab_manager.metrics_a.successful_requests == 2
        assert self.ab_manager.metrics_a.error_count == 0
        assert self.ab_manager.metrics_a.avg_latency_ms == 125.0
        
        assert self.ab_manager.metrics_b.request_count == 2
        assert self.ab_manager.metrics_b.successful_requests == 1
        assert self.ab_manager.metrics_b.error_count == 1
        assert self.ab_manager.metrics_b.error_rate == 50.0
    
    def test_success_criteria_evaluation(self):
        """Test success criteria evaluation logic"""
        # Setup metrics for variant A (baseline)
        self.ab_manager.metrics_a.request_count = 100
        self.ab_manager.metrics_a.successful_requests = 95
        self.ab_manager.metrics_a.error_count = 5
        self.ab_manager.metrics_a.total_latency_ms = 10000.0
        
        # Setup metrics for variant B (better performance)
        self.ab_manager.metrics_b.request_count = 100
        self.ab_manager.metrics_b.successful_requests = 98
        self.ab_manager.metrics_b.error_count = 2
        self.ab_manager.metrics_b.total_latency_ms = 9000.0
        
        # Start a test to enable evaluation
        self.ab_manager.active_test = {"test_id": "test123", "start_time": "2025-01-01T00:00:00Z"}
        
        # Test success criteria
        assert self.ab_manager._meets_success_criteria() is True
        
        # Test failure criteria (make variant B worse)
        self.ab_manager.metrics_b.error_count = 15
        self.ab_manager.metrics_b.successful_requests = 85
        
        assert self.ab_manager._variant_a_significantly_better() is True


class TestABTestingMiddleware:
    """Test cases for A/B testing middleware"""
    
    def setup_method(self):
        """Setup test fixtures"""
        self.ab_manager = Mock(spec=ABTestingManager)
        self.app = FastAPI()
        
        @self.app.get("/v1/chat/completions")
        async def mock_completion():
            return JSONResponse({"choices": [{"message": {"content": "test"}}]})
        
        @self.app.get("/health") 
        async def mock_health():
            return JSONResponse({"status": "ok"})
        
        # Add middleware
        self.app.add_middleware(ABTestingMiddleware, ab_manager=self.ab_manager)
        self.client = TestClient(self.app)
    
    def test_inference_request_detection(self):
        """Test middleware correctly identifies inference requests"""
        middleware = ABTestingMiddleware(None, self.ab_manager)
        
        # Mock inference requests
        inference_request = Mock()
        inference_request.url.path = "/v1/chat/completions"
        assert middleware._is_inference_request(inference_request) is True
        
        # Mock non-inference request
        health_request = Mock()
        health_request.url.path = "/health"
        assert middleware._is_inference_request(health_request) is False
    
    def test_ab_testing_headers(self):
        """Test A/B testing headers are added to responses"""
        # Configure mock
        self.ab_manager.should_use_variant_b.return_value = True
        self.ab_manager.config.enabled = True
        self.ab_manager.active_test = {"test_id": "test123"}
        
        response = self.client.get("/v1/chat/completions")
        
        assert response.status_code == 200
        assert response.headers.get("X-AB-Test-Variant") == "b"
        assert response.headers.get("X-AB-Test-ID") == "test123"


class TestABTestMonitor:
    """Test cases for A/B testing monitoring system"""
    
    def setup_method(self):
        """Setup test fixtures"""
        self.ab_manager = Mock(spec=ABTestingManager)
        self.ab_manager.active_test = {"test_id": "test123"}
        self.ab_manager.config = ABTestConfig()
        
        self.alert_callback = Mock()
        self.monitor = ABTestMonitor(
            self.ab_manager, 
            check_interval_seconds=0.1,  # Fast for testing
            alert_callback=self.alert_callback
        )
    
    @pytest.mark.asyncio
    async def test_monitoring_lifecycle(self):
        """Test monitoring start and stop"""
        assert self.monitor.is_monitoring is False
        
        await self.monitor.start_monitoring()
        assert self.monitor.is_monitoring is True
        assert self.monitor.monitoring_task is not None
        
        await self.monitor.stop_monitoring()
        assert self.monitor.is_monitoring is False
        assert self.monitor.monitoring_task is None
    
    @pytest.mark.asyncio
    async def test_rollback_condition_detection(self):
        """Test rollback condition detection"""
        # Mock test status with critical error rate
        test_status = {
            "test_id": "test123",
            "metrics": {
                "variant_a": {"error_rate": 1.0, "avg_latency_ms": 100.0, "request_count": 100},
                "variant_b": {"error_rate": 15.0, "avg_latency_ms": 110.0, "request_count": 100}
            }
        }
        
        # Test critical error rate detection
        rollback_needed = await self.monitor._check_rollback_conditions(
            test_status["metrics"]["variant_a"],
            test_status["metrics"]["variant_b"], 
            test_status
        )
        
        assert rollback_needed is True
        
        # Verify alert was generated
        self.alert_callback.assert_called()
        alert_call = self.alert_callback.call_args[0][0]
        assert alert_call.level == AlertLevel.EMERGENCY
        assert "error rate" in alert_call.message.lower()
    
    @pytest.mark.asyncio 
    async def test_warning_condition_detection(self):
        """Test warning condition detection"""
        # Mock test status with warning-level issues
        test_status = {
            "test_id": "test123",
            "metrics": {
                "variant_a": {"error_rate": 1.0, "avg_latency_ms": 100.0, "request_count": 100},
                "variant_b": {"error_rate": 6.0, "avg_latency_ms": 1600.0, "request_count": 100}
            }
        }
        
        await self.monitor._check_warning_conditions(
            test_status["metrics"]["variant_a"],
            test_status["metrics"]["variant_b"],
            test_status
        )
        
        # Should generate 2 warnings (error rate + latency)
        assert self.alert_callback.call_count >= 2
    
    def test_alert_generation(self):
        """Test alert generation and storage"""
        initial_count = len(self.monitor.alerts)
        
        # Generate test alert
        asyncio.run(self.monitor._generate_alert(
            AlertLevel.WARNING,
            "Test alert message",
            {"test_id": "test123", "metrics": {}},
            "test_action"
        ))
        
        assert len(self.monitor.alerts) == initial_count + 1
        alert = self.monitor.alerts[-1]
        assert alert.level == AlertLevel.WARNING
        assert alert.message == "Test alert message"
        assert alert.test_id == "test123"
        assert alert.action_taken == "test_action"


class TestModelVariantManager:
    """Test cases for model variant management"""
    
    def setup_method(self):
        """Setup test fixtures"""
        # Mock engine args
        self.mock_engine_args = Mock()
        self.mock_engine_args.model = "base-model"
        self.mock_engine_args.tokenizer = None
        self.mock_engine_args.trust_remote_code = True
        
        self.manager = ModelVariantManager(self.mock_engine_args, max_concurrent_models=2)
    
    @pytest.mark.asyncio
    async def test_variant_registration(self):
        """Test model variant registration"""
        success = await self.manager.register_variant("variant-a", "path/to/model-a")
        
        assert success is True
        assert "variant-a" in self.manager.variants
        assert self.manager.variants["variant-a"].name == "variant-a"
        assert self.manager.variants["variant-a"].model_path == "path/to/model-a"
        assert self.manager.variants["variant-a"].is_loaded is False
    
    def test_variant_stats(self):
        """Test variant statistics collection"""
        # Register and add metrics to variants
        asyncio.run(self.manager.register_variant("variant-a", "path/to/model-a"))
        asyncio.run(self.manager.register_variant("variant-b", "path/to/model-b"))
        
        variant_a = self.manager.variants["variant-a"]
        variant_a.request_count = 100
        variant_a.error_count = 5
        variant_a.update_health_score()
        
        stats = self.manager.get_variant_stats()
        
        assert "variant-a" in stats
        assert "variant-b" in stats
        assert stats["variant-a"]["request_count"] == 100
        assert stats["variant-a"]["error_count"] == 5
        assert stats["variant-a"]["error_rate"] == 5.0
        assert 0.0 <= stats["variant-a"]["health_score"] <= 1.0


class TestABTestingAPI:
    """Test cases for A/B testing API endpoints"""
    
    def setup_method(self):
        """Setup test fixtures"""
        self.app = FastAPI()
        self.app.include_router(ab_testing_router)
        self.client = TestClient(self.app)
        
        # Mock dependencies
        self.mock_ab_manager = Mock(spec=ABTestingManager)
        self.app.dependency_overrides[get_ab_testing_manager] = lambda: self.mock_ab_manager
    
    def test_start_ab_test_endpoint(self):
        """Test start A/B test API endpoint"""
        # Configure mock
        self.mock_ab_manager.active_test = None
        self.mock_ab_manager.start_ab_test.return_value = asyncio.Future()
        self.mock_ab_manager.start_ab_test.return_value.set_result("test123")
        
        request_data = {
            "model_a": "stable-model",
            "model_b": "canary-model",
            "traffic_split_percent": 20.0,
            "test_duration_minutes": 30
        }
        
        response = self.client.post("/v1/ab-testing/start", json=request_data)
        
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "started"
        assert "test_id" in data
    
    def test_get_ab_test_status(self):
        """Test get A/B test status endpoint"""
        # Configure mock
        mock_status = {
            "test_id": "test123",
            "status": "running", 
            "elapsed_minutes": 15.5,
            "traffic_split_percent": 10.0,
            "metrics": {
                "variant_a": {"error_rate": 2.0, "avg_latency_ms": 100.0, "request_count": 500},
                "variant_b": {"error_rate": 1.8, "avg_latency_ms": 95.0, "request_count": 50}
            },
            "models": {
                "variant_a": "stable-model",
                "variant_b": "canary-model"
            }
        }
        self.mock_ab_manager.get_test_status.return_value = mock_status
        
        response = self.client.get("/v1/ab-testing/status")
        
        assert response.status_code == 200
        data = response.json()
        assert data["test_id"] == "test123"
        assert data["status"] == "running"
        assert data["elapsed_minutes"] == 15.5
    
    def test_stop_ab_test_endpoint(self):
        """Test stop A/B test endpoint"""
        # Configure mock
        self.mock_ab_manager.active_test = {"test_id": "test123"}
        
        mock_result = ABTestResult(
            test_id="test123",
            status="completed",
            decision="promote_b",
            metrics_a=ABTestMetrics(request_count=500, error_count=10),
            metrics_b=ABTestMetrics(request_count=50, error_count=1),
            start_time="2025-01-01T00:00:00Z",
            end_time="2025-01-01T01:00:00Z",
            reason="Test completed successfully"
        )
        
        async def mock_stop_test(_):
            return mock_result
        
        self.mock_ab_manager.stop_ab_test = mock_stop_test
        
        response = self.client.post("/v1/ab-testing/stop")
        
        assert response.status_code == 200
        data = response.json()
        assert data["test_id"] == "test123"
        assert data["decision"] == "promote_b"
        assert data["status"] == "completed"
    
    def test_emergency_rollback_endpoint(self):
        """Test emergency rollback endpoint"""
        # Configure mock
        self.mock_ab_manager.active_test = {"test_id": "test123"}
        
        mock_result = ABTestResult(
            test_id="test123",
            status="completed", 
            decision="rollback",
            metrics_a=ABTestMetrics(),
            metrics_b=ABTestMetrics(),
            start_time="2025-01-01T00:00:00Z",
            reason="Emergency rollback initiated"
        )
        
        async def mock_stop_test(_):
            return mock_result
        
        self.mock_ab_manager.stop_ab_test = mock_stop_test
        
        response = self.client.post("/v1/ab-testing/rollback")
        
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "rolled_back"
        assert data["test_id"] == "test123"


class TestIntegrationScenarios:
    """Integration test scenarios for A/B testing framework"""
    
    @pytest.mark.asyncio
    async def test_end_to_end_ab_test_flow(self):
        """Test complete A/B test lifecycle"""
        # Initialize components
        ab_manager = ABTestingManager()
        monitor = ABTestMonitor(ab_manager, check_interval_seconds=0.1)
        
        # Start A/B test
        test_id = await ab_manager.start_ab_test("model-a", "model-b")
        assert ab_manager.active_test is not None
        
        # Simulate some traffic with metrics
        for i in range(100):
            variant = "b" if i < 10 else "a"  # 10% traffic to B
            latency = 120.0 if variant == "b" else 100.0
            success = True
            ab_manager.record_request_metrics(variant, latency, success)
        
        # Check test status
        status = ab_manager.get_test_status()
        assert status["metrics"]["variant_a"]["request_count"] == 90
        assert status["metrics"]["variant_b"]["request_count"] == 10
        
        # Stop test and get results
        result = await ab_manager.stop_ab_test("test_complete")
        assert result.test_id == test_id
        assert result.status == "completed"
        assert result.decision in ["promote_b", "keep_a", "inconclusive"]
    
    @pytest.mark.asyncio
    async def test_automated_rollback_scenario(self):
        """Test automated rollback on performance degradation"""
        ab_manager = ABTestingManager()
        
        # Configure aggressive failure criteria for testing
        ab_manager.config.failure_criteria["max_error_rate_percent"] = 5.0
        
        monitor = ABTestMonitor(ab_manager, check_interval_seconds=0.1)
        alerts = []
        
        def capture_alerts(alert):
            alerts.append(alert)
        
        monitor.alert_callback = capture_alerts
        
        # Start test and monitoring
        test_id = await ab_manager.start_ab_test("stable-model", "problematic-model")
        await monitor.start_monitoring()
        
        # Simulate high error rate in variant B
        for i in range(60):  # More than min_sample_size
            ab_manager.record_request_metrics("a", 100.0, True)
            ab_manager.record_request_metrics("b", 150.0, i > 50)  # 83% failure rate
        
        # Wait for monitoring to detect issue
        await asyncio.sleep(0.2)
        
        # Check that rollback was triggered
        await monitor.stop_monitoring()
        
        # Should have generated emergency alerts
        emergency_alerts = [a for a in alerts if a.level == AlertLevel.EMERGENCY]
        assert len(emergency_alerts) > 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])