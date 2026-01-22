import pytest
import time
import json
from unittest.mock import AsyncMock, MagicMock
from fastapi.testclient import TestClient
from aphrodite.endpoints.deep_tree_echo import create_app
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
class TestBackendIntegration:
    @pytest.fixture
    def mock_engine(self):
        engine = AsyncMock()
        engine.get_model_config.return_value = MagicMock(model='test-model', max_model_len=4096, vocab_size=32000)
        engine.get_aphrodite_config.return_value = MagicMock(model_config=MagicMock(model='test-model'), parallel_config=MagicMock(tensor_parallel_size=1), scheduler_config=MagicMock(max_num_seqs=256))
        return engine
    @pytest.fixture
    def backend_config(self):
        return DTESNConfig(enable_docs=True, max_membrane_depth=4, esn_reservoir_size=256, bseries_max_order=8, enable_caching=True, enable_performance_monitoring=True)
    @pytest.fixture
    def backend_app(self, mock_engine, backend_config):
        return create_app(engine=mock_engine, config=backend_config, enable_async_resources=True)
    @pytest.fixture
    def backend_client(self, backend_app):
        return TestClient(backend_app)
    def test_fastapi_engine_integration_creation(self, mock_engine, backend_config):
        app = create_app(engine=mock_engine, config=backend_config)
        assert app.state.engine is mock_engine
        assert app.state.config is backend_config
        assert hasattr(app.state, 'connection_pool')
        assert hasattr(app.state, 'concurrency_manager')
        middleware_types = [type(middleware).__name__ for middleware in app.user_middleware]
        expected_middleware = ['DTESNMiddleware', 'PerformanceMonitoringMiddleware', 'AsyncResourceMiddleware', 'InputValidationMiddleware', 'RateLimitMiddleware', 'SecurityMiddleware', 'OutputSanitizationMiddleware']
        for expected in expected_middleware:
            assert any((expected in mw_type for mw_type in middleware_types)), f'Missing {expected}'
    def test_server_side_response_generation(self, backend_client):
        endpoints_to_test = ['/health', '/deep_tree_echo/', '/deep_tree_echo/status', '/deep_tree_echo/membrane_info', '/deep_tree_echo/esn_state', '/deep_tree_echo/engine_integration', '/deep_tree_echo/performance_metrics']
        for endpoint in endpoints_to_test:
            response = backend_client.get(endpoint)
            assert response.status_code == 200, f'Failed for endpoint: {endpoint}'
            data = response.json()
            assert data.get('server_rendered') is True, f'Not server-rendered: {endpoint}'
            response_str = json.dumps(data)
            client_indicators = ['window.', 'document.', 'React.', 'Vue.', 'Angular.', 'useEffect', 'useState', 'componentDidMount']
            for indicator in client_indicators:
                assert indicator not in response_str, f'Client dependency found: {indicator}'
    @pytest.mark.asyncio
    async def test_dtesn_processing_endpoint_integration(self, backend_client):
        request_data = {'input_data': 'backend integration test input', 'membrane_depth': 3, 'esn_size': 128, 'processing_mode': 'server_side', 'include_intermediate': True, 'output_format': 'json'}
        response = backend_client.post('/deep_tree_echo/process', json=request_data)
        assert response.status_code == 200
        data = response.json()
        assert data['status'] == 'success'
        assert data['server_rendered'] is True
        assert data['membrane_layers'] == 3
        assert 'processing_time_ms' in data
        assert 'engine_integration' in data
        assert 'performance_metrics' in data
        assert 'result' in data
        processing_time = data['processing_time_ms']
        assert isinstance(processing_time, (int, float))
        assert processing_time >= 0
    def test_batch_processing_backend_performance(self, backend_client):
        batch_request = {'inputs': [f'batch test input {i}' for i in range(5)], 'membrane_depth': 2, 'esn_size': 64, 'parallel_processing': True, 'max_batch_size': 5}
        start_time = time.time()
        response = backend_client.post('/deep_tree_echo/batch_process', json=batch_request)
        total_time = time.time() - start_time
        assert response.status_code == 200
        data = response.json()
        assert data['status'] == 'completed'
        assert data['server_rendered'] is True
        assert data['batch_size'] == 5
        assert len(data['results']) == 5
        assert 'total_processing_time_ms' in data
        backend_processing_time = data['total_processing_time_ms']
        assert backend_processing_time < 5000, 'Backend processing too slow'
        assert total_time < 10.0, 'Total request time too slow'
    def test_streaming_response_backend_integration(self, backend_client):
        stream_request = {'input_data': 'streaming backend test', 'membrane_depth': 2, 'esn_size': 64, 'processing_mode': 'streaming'}
        response = backend_client.post('/deep_tree_echo/stream_process', json=stream_request)
        assert response.status_code == 200
        assert response.headers['content-type'] == 'text/event-stream; charset=utf-8'
        assert 'X-Server-Rendered' in response.headers
        content = response.text
        assert 'data:' in content
        assert 'server_rendered' in content
        lines = content.strip().split('\n')
        data_lines = [line for line in lines if line.startswith('data:')]
        assert len(data_lines) > 0, 'No SSE data lines found'
    def test_concurrent_request_handling(self, backend_client):
        import concurrent.futures
        def make_request(request_id: int):
            response = backend_client.get('/deep_tree_echo/status')
            return {'id': request_id, 'status_code': response.status_code, 'response_time': time.time(), 'has_engine_integration': 'engine_integration' in response.json()}
        concurrent_requests = 10
        start_time = time.time()
        with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
            futures = [executor.submit(make_request, i) for i in range(concurrent_requests)]
            results = [future.result() for future in concurrent.futures.as_completed(futures)]
        total_time = time.time() - start_time
        assert len(results) == concurrent_requests
        assert all((r['status_code'] == 200 for r in results))
        assert all((r['has_engine_integration'] for r in results))
        assert total_time < 5.0, 'Concurrent requests too slow'
    def test_performance_monitoring_headers(self, backend_client):
        response = backend_client.get('/deep_tree_echo/performance_metrics')
        assert response.status_code == 200
        required_headers = ['X-Process-Time', 'X-Server-Timestamp', 'X-DTESN-Processed', 'X-Processing-Mode']
        for header in required_headers:
            assert header in response.headers, f'Missing performance header: {header}'
        assert response.headers['X-Processing-Mode'] == 'server-side'
        process_time = response.headers['X-Process-Time']
        assert float(process_time) >= 0
    def test_engine_integration_status_endpoint(self, backend_client):
        response = backend_client.get('/deep_tree_echo/engine_integration')
        assert response.status_code == 200
        data = response.json()
        assert 'dtesn_processor' in data
        assert 'aphrodite_engine' in data
        assert 'integration_capabilities' in data
        capabilities = data['integration_capabilities']
        assert capabilities['server_side_processing'] is True
        engine_info = data['aphrodite_engine']
        assert 'status' in engine_info
        assert 'model_info' in engine_info
    @pytest.mark.asyncio
    async def test_backend_processing_pipeline_performance(self, backend_client):
        test_cases = [{'membrane_depth': 2, 'esn_size': 64, 'expected_max_time': 1000}, {'membrane_depth': 4, 'esn_size': 128, 'expected_max_time': 2000}, {'membrane_depth': 6, 'esn_size': 256, 'expected_max_time': 3000}]
        performance_results = []
        for case in test_cases:
            request_data = {'input_data': f"performance test depth={case['membrane_depth']}", 'membrane_depth': case['membrane_depth'], 'esn_size': case['esn_size'], 'processing_mode': 'server_side'}
            start_time = time.time()
            response = backend_client.post('/deep_tree_echo/process', json=request_data)
            end_time = time.time()
            assert response.status_code == 200
            data = response.json()
            processing_time = data['processing_time_ms']
            total_time = (end_time - start_time) * 1000
            performance_results.append({'case': case, 'backend_time': processing_time, 'total_time': total_time, 'within_expected': processing_time <= case['expected_max_time']})
            assert processing_time <= case['expected_max_time'], f"Backend processing too slow: {processing_time}ms > {case['expected_max_time']}ms"
        times = [r['backend_time'] for r in performance_results]
        assert times[0] <= times[1] <= times[2], 'Performance should scale with complexity'
    def test_error_handling_and_resilience(self, backend_client):
        invalid_requests = [{'input_data': 'test', 'membrane_depth': 50, 'esn_size': 128}, {'input_data': 'test', 'membrane_depth': 4, 'esn_size': 10000}, {'input_data': '', 'membrane_depth': 4, 'esn_size': 128}]
        for invalid_request in invalid_requests:
            response = backend_client.post('/deep_tree_echo/process', json=invalid_request)
            assert response.status_code in [400, 422], f'Invalid request not handled: {invalid_request}'
    def test_memory_and_resource_management(self, backend_app):
        assert hasattr(backend_app.state, 'connection_pool')
        assert hasattr(backend_app.state, 'concurrency_manager')
        with TestClient(backend_app) as client:
            response = client.get('/health')
            assert response.status_code == 200
            data = response.json()
            assert 'async_resources' in data
            assert 'resource_stats' in data or 'concurrency_stats' in data
    def test_openai_compatibility_integration(self, backend_client):
        response = backend_client.get('/deep_tree_echo/')
        assert response.status_code == 200
        data = response.json()
        assert data['service'] == 'Deep Tree Echo API'
        assert 'endpoints' in data
        assert 'engine_integration' in data