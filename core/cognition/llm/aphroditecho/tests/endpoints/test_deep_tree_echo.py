"""
Tests for Deep Tree Echo FastAPI endpoints.

Validates the server-side rendering functionality of DTESN endpoints
with enhanced route handlers and engine integration.
"""

import pytest
from fastapi.testclient import TestClient

from aphrodite.endpoints.deep_tree_echo import create_app
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig


class TestDeepTreeEchoEndpoints:
    """Test suite for Deep Tree Echo FastAPI endpoints with enhanced functionality."""

    @pytest.fixture
    def config(self):
        """Test configuration fixture."""
        return DTESNConfig(
            enable_docs=True,
            max_membrane_depth=4,
            esn_reservoir_size=256,
            bseries_max_order=8
        )

    @pytest.fixture
    def app(self, config):
        """FastAPI application fixture."""
        return create_app(config=config)

    @pytest.fixture
    def client(self, app):
        """Test client fixture."""
        return TestClient(app)

    def test_health_endpoint(self, client):
        """Test health check endpoint."""
        response = client.get("/health")
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "healthy"
        assert data["service"] == "deep_tree_echo"
        assert data["version"] == "1.0.0"

    def test_dtesn_root_endpoint_enhanced(self, client):
        """Test enhanced DTESN root endpoint with engine integration."""
        response = client.get("/deep_tree_echo/")
        assert response.status_code == 200
        data = response.json()
        assert data["service"] == "Deep Tree Echo API"
        assert data["server_rendered"] is True
        assert "endpoints" in data
        assert "/batch_process" in data["endpoints"]
        assert "/stream_process" in data["endpoints"]
        assert "/engine_integration" in data["endpoints"]
        assert "engine_integration" in data

    def test_dtesn_status_endpoint_enhanced(self, client):
        """Test enhanced DTESN status endpoint."""
        response = client.get("/deep_tree_echo/status")
        assert response.status_code == 200
        data = response.json()
        assert data["dtesn_system"] == "operational"
        assert data["server_side"] is True
        assert "processing_capabilities" in data
        assert "advanced_features" in data
        assert data["advanced_features"]["batch_processing"] is True
        assert data["advanced_features"]["streaming_support"] is True
        assert "engine_integration" in data

    def test_membrane_info_endpoint_enhanced(self, client):
        """Test enhanced membrane information endpoint."""
        response = client.get("/deep_tree_echo/membrane_info")
        assert response.status_code == 200
        data = response.json()
        assert data["membrane_type"] == "P-System"
        assert data["oeis_sequence"] == "A000081"
        assert data["server_rendered"] is True
        assert "performance_characteristics" in data
        assert "integration_features" in data
        assert data["integration_features"]["server_side_optimization"] is True

    def test_esn_state_endpoint_enhanced(self, client):
        """Test enhanced ESN state endpoint."""
        response = client.get("/deep_tree_echo/esn_state")
        assert response.status_code == 200
        data = response.json()
        assert data["reservoir_type"] == "echo_state_network"
        assert data["state"] == "ready"
        assert data["server_rendered"] is True
        assert "performance_profile" in data
        assert "integration_capabilities" in data
        assert "optimization_features" in data
        assert data["integration_capabilities"]["server_side_processing"] is True

    def test_engine_integration_endpoint(self, client):
        """Test engine integration status endpoint."""
        response = client.get("/deep_tree_echo/engine_integration")
        assert response.status_code == 200
        data = response.json()
        assert "dtesn_processor" in data
        assert "aphrodite_engine" in data
        assert "integration_capabilities" in data
        assert data["integration_capabilities"]["server_side_processing"] is True
        assert data["server_rendered"] is True

    def test_performance_metrics_endpoint(self, client):
        """Test performance metrics endpoint."""
        response = client.get("/deep_tree_echo/performance_metrics")
        assert response.status_code == 200
        data = response.json()
        assert "service_metrics" in data
        assert "dtesn_performance" in data
        assert "server_optimization" in data
        assert "integration_metrics" in data
        assert data["server_rendered"] is True

    def test_dtesn_process_endpoint_enhanced(self, client):
        """Test enhanced DTESN processing endpoint."""
        request_data = {
            "input_data": "test input",
            "membrane_depth": 3,
            "esn_size": 128,
            "processing_mode": "server_side",
            "include_intermediate": False,
            "output_format": "json"
        }
        
        response = client.post("/deep_tree_echo/process", json=request_data)
        assert response.status_code == 200
        data = response.json()
        
        assert data["status"] == "success"
        assert data["server_rendered"] is True
        assert data["membrane_layers"] == 3
        assert "result" in data
        assert "processing_time_ms" in data
        assert "engine_integration" in data
        assert "performance_metrics" in data

    def test_batch_process_endpoint(self, client):
        """Test batch DTESN processing endpoint."""
        request_data = {
            "inputs": ["test input 1", "test input 2", "test input 3"],
            "membrane_depth": 2,
            "esn_size": 64,
            "parallel_processing": True,
            "max_batch_size": 5
        }
        
        response = client.post("/deep_tree_echo/batch_process", json=request_data)
        assert response.status_code == 200
        data = response.json()
        
        assert data["status"] == "completed"
        assert data["server_rendered"] is True
        assert data["batch_size"] == 3
        assert "results" in data
        assert len(data["results"]) == 3
        assert "successful_count" in data
        assert "failed_count" in data
        assert "total_processing_time_ms" in data

    def test_stream_process_endpoint(self, client):
        """Test streaming DTESN processing endpoint."""
        request_data = {
            "input_data": "test streaming input",
            "membrane_depth": 2,
            "esn_size": 64,
            "processing_mode": "streaming"
        }
        
        response = client.post("/deep_tree_echo/stream_process", json=request_data)
        assert response.status_code == 200
        assert response.headers["content-type"] == "text/event-stream; charset=utf-8"
        assert "X-Server-Rendered" in response.headers
        
        # Check that we get streaming content
        content = response.text
        assert "data:" in content
        assert "server_rendered" in content

    def test_performance_headers_enhanced(self, client):
        """Test enhanced performance monitoring headers are present."""
        response = client.get("/deep_tree_echo/status")
        assert response.status_code == 200
        
        # Check performance headers from middleware
        assert "X-Process-Time" in response.headers
        assert "X-Server-Timestamp" in response.headers
        assert "X-DTESN-Processed" in response.headers
        assert response.headers["X-Processing-Mode"] == "server-side"

    def test_input_validation(self, client):
        """Test input validation for enhanced endpoints."""
        # Test invalid membrane depth
        request_data = {
            "input_data": "test",
            "membrane_depth": 20,  # Too high
            "esn_size": 128
        }
        
        response = client.post("/deep_tree_echo/process", json=request_data)
        assert response.status_code == 422  # Validation error
        
        # Test invalid ESN size  
        request_data = {
            "input_data": "test",
            "membrane_depth": 4,
            "esn_size": 10000  # Too high
        }
        
        response = client.post("/deep_tree_echo/process", json=request_data)
        assert response.status_code == 422  # Validation error

    def test_config_from_env(self):
        """Test configuration from environment variables."""
        import os
        
        # Set test environment variables
        os.environ["DTESN_MAX_MEMBRANE_DEPTH"] = "6"
        os.environ["DTESN_ESN_RESERVOIR_SIZE"] = "512"
        os.environ["DTESN_ENABLE_CACHING"] = "false"
        
        config = DTESNConfig.from_env()
        assert config.max_membrane_depth == 6
        assert config.esn_reservoir_size == 512
        assert config.enable_caching is False
        
        # Cleanup
        del os.environ["DTESN_MAX_MEMBRANE_DEPTH"]
        del os.environ["DTESN_ESN_RESERVOIR_SIZE"]  
        del os.environ["DTESN_ENABLE_CACHING"]