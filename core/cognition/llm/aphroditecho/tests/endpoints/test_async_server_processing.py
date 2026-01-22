import asyncio
import json
import pytest
import time
from unittest.mock import AsyncMock, Mock, patch
from fastapi.testclient import TestClient
from fastapi import FastAPI
from aphrodite.endpoints.deep_tree_echo import create_app
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
from aphrodite.endpoints.deep_tree_echo.async_manager import AsyncConnectionPool, ConcurrencyManager, AsyncRequestQueue, ConnectionPoolConfig
from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
class TestAsyncServerSideProcessing:
    @pytest.fixture
    def config(self):
        return DTESNConfig(enable_docs=True, max_membrane_depth=4, esn_reservoir_size=256, bseries_max_order=8)
    @pytest.fixture
    async def connection_pool(self):
        pool_config = ConnectionPoolConfig(max_connections=50, min_connections=5, connection_timeout=10.0)
        pool = AsyncConnectionPool(pool_config)
        await pool.start()
        yield pool
        await pool.stop()
    @pytest.fixture
    def concurrency_manager(self):
        return ConcurrencyManager(max_concurrent_requests=25, max_requests_per_second=50.0, burst_limit=10)
    @pytest.fixture
    def app(self, config):
        return create_app(config=config, enable_async_resources=True)
    @pytest.fixture
    def client(self, app):
        return TestClient(app)
    def test_async_status_endpoint(self, client):
        response = client.get('/deep_tree_echo/async_status')
        assert response.status_code == 200
        data = response.json()
        assert data['async_processing']['enabled'] is True
        assert data['async_processing']['concurrent_processing'] is True
        assert 'processing_metrics' in data
        assert 'capabilities' in data
        assert 'performance_features' in data
        assert data['server_rendered'] is True
    def test_concurrent_batch_processing(self, client):
        request_data = {'inputs': [f'test input {i}' for i in range(10)], 'membrane_depth': 2, 'esn_size': 64, 'parallel_processing': True, 'max_batch_size': 5}
        response = client.post('/deep_tree_echo/batch_process', json=request_data)
        assert response.status_code == 200
        data = response.json()
        assert data['status'] == 'completed'
        assert data['batch_size'] == 10
        assert len(data['results']) == 10
        assert data['server_rendered'] is True
        for result in data['results']:
            assert 'status' in result
            assert 'server_rendered' in result
            assert result['server_rendered'] is True
    def test_enhanced_streaming_with_backpressure(self, client):
        request_data = {'input_data': 'test streaming with backpressure', 'membrane_depth': 3, 'esn_size': 128, 'processing_mode': 'streaming'}
        response = client.post('/deep_tree_echo/stream_process', json=request_data)
        assert response.status_code == 200
        assert response.headers['content-type'] == 'text/event-stream; charset=utf-8'
        assert 'X-Stream-Enhanced' in response.headers
        assert 'X-Backpressure-Enabled' in response.headers
        assert 'X-Concurrent-Processing' in response.headers
        content = response.text
        assert 'data:' in content
        assert 'stream_enhanced' in content
        assert 'backpressure_enabled' in content
        assert 'concurrent_processing' in content
    def test_health_check_with_async_resources(self, client):
        response = client.get('/health')
        assert response.status_code == 200
        data = response.json()
        assert 'async_resources' in data
        assert 'connection_pool_enabled' in data['async_resources']
        assert 'concurrency_management_enabled' in data['async_resources']
    def test_middleware_async_headers(self, client):
        response = client.get('/deep_tree_echo/status')
        assert response.status_code == 200
        assert 'X-Process-Time' in response.headers
        assert 'X-Async-Processing' in response.headers
        assert 'X-Resource-Managed' in response.headers
        assert response.headers['X-Async-Processing'] == 'true'
    @pytest.mark.asyncio
    async def test_connection_pool_functionality(self, connection_pool):
        connections_used = []
        async def use_connection():
            async with connection_pool.get_connection() as conn_id:
                connections_used.append(conn_id)
                await asyncio.sleep(0.01)
        tasks = [use_connection() for _ in range(5)]
        await asyncio.gather(*tasks)
        assert len(connections_used) == 5
        assert len(set(connections_used)) >= 1
        stats = connection_pool.get_stats()
        assert stats.total_requests == 5
        assert stats.failed_requests == 0
    @pytest.mark.asyncio
    async def test_concurrency_manager_throttling(self, concurrency_manager):
        request_times = []
        async def throttled_request():
            start_time = asyncio.get_event_loop().time()
            async with concurrency_manager.throttle_request():
                await asyncio.sleep(0.01)
            end_time = asyncio.get_event_loop().time()
            request_times.append(end_time - start_time)
        tasks = [throttled_request() for _ in range(10)]
        await asyncio.gather(*tasks)
        assert len(request_times) == 10
        assert max(request_times) > 0.01
        load_stats = concurrency_manager.get_current_load()
        assert 'concurrent_requests' in load_stats
        assert 'rate_limit_utilization' in load_stats
    def test_error_handling_in_concurrent_processing(self, client):
        request_data = {'inputs': ['' for _ in range(5)], 'membrane_depth': 999, 'esn_size': 10000, 'parallel_processing': True, 'max_batch_size': 3}
        response = client.post('/deep_tree_echo/batch_process', json=request_data)
        assert response.status_code in [200, 422, 500]
        if response.status_code == 200:
            data = response.json()
            assert 'results' in data
            assert data['server_rendered'] is True
    def test_processing_request_validation(self, client):
        request_data = {'input_data': 'test', 'membrane_depth': 20, 'esn_size': 128}
        response = client.post('/deep_tree_echo/process', json=request_data)
        assert response.status_code == 422
        request_data = {'input_data': 'test', 'membrane_depth': 4, 'esn_size': 10000}
        response = client.post('/deep_tree_echo/process', json=request_data)
        assert response.status_code == 422
    def test_async_resource_cleanup_headers(self, client):
        request_data = {'input_data': 'test resource cleanup', 'membrane_depth': 2, 'esn_size': 64}
        response = client.post('/deep_tree_echo/process', json=request_data)
        assert response.status_code == 200
        assert 'X-DTESN-Processed' in response.headers
        assert 'X-Async-Managed' in response.headers
        assert 'X-Request-ID' in response.headers
    def test_performance_monitoring_with_concurrency(self, client):
        response = client.get('/deep_tree_echo/performance_metrics')
        assert response.status_code == 200
        data = response.json()
        assert 'service_metrics' in data
        assert 'server_optimization' in data
        assert 'integration_metrics' in data
        assert data['server_rendered'] is True
        assert data['service_metrics']['processing_mode'] == 'server_side'
    @pytest.mark.asyncio
    async def test_connection_pool_stats_tracking(self, connection_pool):
        initial_stats = connection_pool.get_stats()
        assert initial_stats.total_requests == 0
        async def use_connections():
            tasks = []
            for _ in range(3):
                async def single_connection():
                    async with connection_pool.get_connection() as conn:
                        await asyncio.sleep(0.01)
                tasks.append(single_connection())
            await asyncio.gather(*tasks)
        await use_connections()
        final_stats = connection_pool.get_stats()
        assert final_stats.total_requests == 3
        assert final_stats.avg_response_time > 0
        assert final_stats.last_updated > initial_stats.last_updated