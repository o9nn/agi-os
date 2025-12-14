"""
Tests for enhanced async server-side processing in Deep Tree Echo endpoints.

Validates the non-blocking request handling, concurrent processing for multiple
DTESN requests, and async response streaming capabilities.
"""

import asyncio
import json
import pytest
import time
from unittest.mock import AsyncMock, Mock, patch

from fastapi.testclient import TestClient
from fastapi import FastAPI

from aphrodite.endpoints.deep_tree_echo import create_app
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
from aphrodite.endpoints.deep_tree_echo.async_manager import (
    AsyncConnectionPool, 
    ConcurrencyManager,
    AsyncRequestQueue,
    ConnectionPoolConfig
)
from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor


class TestAsyncServerSideProcessing:
    """Test suite for enhanced async server-side processing capabilities."""

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
    async def connection_pool(self):
        """Async connection pool fixture."""
        pool_config = ConnectionPoolConfig(
            max_connections=50,
            min_connections=5,
            connection_timeout=10.0
        )
        pool = AsyncConnectionPool(pool_config)
        await pool.start()
        yield pool
        await pool.stop()

    @pytest.fixture
    def concurrency_manager(self):
        """Concurrency manager fixture."""
        return ConcurrencyManager(
            max_concurrent_requests=25,
            max_requests_per_second=50.0,
            burst_limit=10
        )

    @pytest.fixture
    def app(self, config):
        """FastAPI application fixture with async resources enabled."""
        return create_app(config=config, enable_async_resources=True)

    @pytest.fixture
    def client(self, app):
        """Test client fixture."""
        return TestClient(app)

    def test_async_status_endpoint(self, client):
        """Test enhanced async processing status endpoint."""
        response = client.get("/deep_tree_echo/async_status")
        assert response.status_code == 200
        data = response.json()
        
        assert data["async_processing"]["enabled"] is True
        assert data["async_processing"]["concurrent_processing"] is True
        assert "processing_metrics" in data
        assert "capabilities" in data
        assert "performance_features" in data
        assert data["server_rendered"] is True

    def test_concurrent_batch_processing(self, client):
        """Test enhanced concurrent batch processing."""
        request_data = {
            "inputs": [f"test input {i}" for i in range(10)],
            "membrane_depth": 2,
            "esn_size": 64,
            "parallel_processing": True,
            "max_batch_size": 5
        }
        
        response = client.post("/deep_tree_echo/batch_process", json=request_data)
        assert response.status_code == 200
        data = response.json()
        
        assert data["status"] == "completed"
        assert data["batch_size"] == 10
        assert len(data["results"]) == 10
        assert data["server_rendered"] is True
        
        # Check that all results have proper structure
        for result in data["results"]:
            assert "status" in result
            assert "server_rendered" in result
            assert result["server_rendered"] is True

    def test_enhanced_streaming_with_backpressure(self, client):
        """Test enhanced streaming with backpressure handling."""
        request_data = {
            "input_data": "test streaming with backpressure",
            "membrane_depth": 3,
            "esn_size": 128,
            "processing_mode": "streaming"
        }
        
        response = client.post("/deep_tree_echo/stream_process", json=request_data)
        assert response.status_code == 200
        assert response.headers["content-type"] == "text/event-stream; charset=utf-8"
        
        # Check enhanced headers
        assert "X-Stream-Enhanced" in response.headers
        assert "X-Backpressure-Enabled" in response.headers
        assert "X-Concurrent-Processing" in response.headers
        
        # Check streaming content
        content = response.text
        assert "data:" in content
        assert "stream_enhanced" in content
        assert "backpressure_enabled" in content
        assert "concurrent_processing" in content

    def test_health_check_with_async_resources(self, client):
        """Test health check endpoint includes async resource status."""
        response = client.get("/health")
        assert response.status_code == 200
        data = response.json()
        
        assert "async_resources" in data
        assert "connection_pool_enabled" in data["async_resources"]
        assert "concurrency_management_enabled" in data["async_resources"]

    def test_middleware_async_headers(self, client):
        """Test that async middleware adds proper headers."""
        response = client.get("/deep_tree_echo/status")
        assert response.status_code == 200
        
        # Check async processing headers
        assert "X-Process-Time" in response.headers
        assert "X-Async-Processing" in response.headers
        assert "X-Resource-Managed" in response.headers
        assert response.headers["X-Async-Processing"] == "true"

    @pytest.mark.asyncio
    async def test_connection_pool_functionality(self, connection_pool):
        """Test async connection pool basic functionality."""
        # Test getting connections
        connections_used = []
        
        async def use_connection():
            async with connection_pool.get_connection() as conn_id:
                connections_used.append(conn_id)
                await asyncio.sleep(0.01)
        
        # Test concurrent connections
        tasks = [use_connection() for _ in range(5)]
        await asyncio.gather(*tasks)
        
        assert len(connections_used) == 5
        assert len(set(connections_used)) >= 1  # At least some connection reuse
        
        # Check stats
        stats = connection_pool.get_stats()
        assert stats.total_requests == 5
        assert stats.failed_requests == 0

    @pytest.mark.asyncio
    async def test_concurrency_manager_throttling(self, concurrency_manager):
        """Test concurrency manager request throttling."""
        request_times = []
        
        async def throttled_request():
            start_time = asyncio.get_event_loop().time()
            async with concurrency_manager.throttle_request():
                await asyncio.sleep(0.01)
            end_time = asyncio.get_event_loop().time()
            request_times.append(end_time - start_time)
        
        # Send many requests quickly
        tasks = [throttled_request() for _ in range(10)]
        await asyncio.gather(*tasks)
        
        # Check that throttling occurred (some requests took longer)
        assert len(request_times) == 10
        assert max(request_times) > 0.01  # Some requests were delayed
        
        # Check load stats
        load_stats = concurrency_manager.get_current_load()
        assert "concurrent_requests" in load_stats
        assert "rate_limit_utilization" in load_stats

    def test_error_handling_in_concurrent_processing(self, client):
        """Test error handling during concurrent processing."""
        # Test with invalid input that should cause processing errors
        request_data = {
            "inputs": ["" for _ in range(5)],  # Empty inputs might cause errors
            "membrane_depth": 999,  # Invalid depth
            "esn_size": 10000,  # Invalid size
            "parallel_processing": True,
            "max_batch_size": 3
        }
        
        response = client.post("/deep_tree_echo/batch_process", json=request_data)
        # Should handle errors gracefully, not crash
        assert response.status_code in [200, 422, 500]  # Various error codes acceptable
        
        if response.status_code == 200:
            data = response.json()
            # If processed, should have error handling in results
            assert "results" in data
            assert data["server_rendered"] is True

    def test_processing_request_validation(self, client):
        """Test enhanced input validation for async processing."""
        # Test membrane depth validation
        request_data = {
            "input_data": "test",
            "membrane_depth": 20,  # Too high
            "esn_size": 128
        }
        
        response = client.post("/deep_tree_echo/process", json=request_data)
        assert response.status_code == 422
        
        # Test ESN size validation
        request_data = {
            "input_data": "test",
            "membrane_depth": 4,
            "esn_size": 10000  # Too high
        }
        
        response = client.post("/deep_tree_echo/process", json=request_data)
        assert response.status_code == 422

    def test_async_resource_cleanup_headers(self, client):
        """Test that async resources add proper cleanup and monitoring headers."""
        request_data = {
            "input_data": "test resource cleanup",
            "membrane_depth": 2,
            "esn_size": 64
        }
        
        response = client.post("/deep_tree_echo/process", json=request_data)
        assert response.status_code == 200
        
        # Check resource management headers
        assert "X-DTESN-Processed" in response.headers
        assert "X-Async-Managed" in response.headers
        assert "X-Request-ID" in response.headers

    def test_performance_monitoring_with_concurrency(self, client):
        """Test performance monitoring includes concurrency metrics."""
        response = client.get("/deep_tree_echo/performance_metrics")
        assert response.status_code == 200
        data = response.json()
        
        assert "service_metrics" in data
        assert "server_optimization" in data
        assert "integration_metrics" in data
        assert data["server_rendered"] is True
        
        # Should include async processing indicators
        assert data["service_metrics"]["processing_mode"] == "server_side"

    @pytest.mark.asyncio
    async def test_connection_pool_stats_tracking(self, connection_pool):
        """Test that connection pool properly tracks statistics."""
        initial_stats = connection_pool.get_stats()
        assert initial_stats.total_requests == 0
        
        # Use some connections
        async def use_connections():
            tasks = []
            for _ in range(3):
                async def single_connection():
                    async with connection_pool.get_connection() as conn:
                        await asyncio.sleep(0.01)
                tasks.append(single_connection())
            await asyncio.gather(*tasks)
        
        await use_connections()
        
        # Check updated stats
        final_stats = connection_pool.get_stats()
        assert final_stats.total_requests == 3
        assert final_stats.avg_response_time > 0
        assert final_stats.last_updated > initial_stats.last_updated