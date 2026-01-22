import asyncio
import json
import pytest
import time
from unittest.mock import Mock, patch, AsyncMock
from datetime import datetime, timedelta
from fastapi import FastAPI, Request, Response
from fastapi.testclient import TestClient
from starlette.responses import JSONResponse
from aphrodite.endpoints.middleware import ComprehensiveLoggingMiddleware, LoggingConfig, RequestContext, EnhancedPerformanceMonitoringMiddleware, PerformanceProfiler, AdvancedSecurityMiddleware, AdvancedSecurityConfig, MiddlewareOrchestrator, MiddlewareConfig, setup_comprehensive_middleware
@pytest.fixture
def app_basic():
    app = FastAPI()
    @app.get('/test')
    async def test_endpoint():
        return {'message': 'test successful'}
    @app.get('/slow')
    async def slow_endpoint():
        await asyncio.sleep(0.1)
        return {'message': 'slow endpoint'}
    @app.get('/error')
    async def error_endpoint():
        raise Exception('Test error')
    @app.post('/data')
    async def data_endpoint(request: Request):
        body = await request.body()
        return {'received': len(body)}
    return app
@pytest.fixture
def app_with_logging(app_basic):
    config = LoggingConfig(log_requests=True, log_responses=True, log_request_body=True, log_response_body=True, log_level='DEBUG')
    app_basic.add_middleware(ComprehensiveLoggingMiddleware, config=config)
    return app_basic
@pytest.fixture
def app_with_performance(app_basic):
    profiler = PerformanceProfiler(max_history=100)
    app_basic.add_middleware(EnhancedPerformanceMonitoringMiddleware, profiler=profiler, slow_request_threshold_ms=50.0, enable_detailed_metrics=True)
    return app_basic
@pytest.fixture
def app_with_security(app_basic):
    config = AdvancedSecurityConfig(enable_ddos_protection=True, enable_anomaly_detection=True, enable_content_inspection=True, requests_per_minute=10)
    app_basic.add_middleware(AdvancedSecurityMiddleware, config=config)
    return app_basic
@pytest.fixture
def app_comprehensive():
    app = FastAPI()
    @app.get('/test')
    async def test_endpoint():
        return {'message': 'test successful'}
    @app.get('/health')
    async def health_endpoint():
        return {'status': 'ok'}
    config = MiddlewareConfig.development()
    config.enable_advanced_security = True
    orchestrator = setup_comprehensive_middleware(app, config)
    return (app, orchestrator)
class TestLoggingMiddleware:
    def test_basic_request_logging(self, app_with_logging):
        client = TestClient(app_with_logging)
        with patch('aphrodite.endpoints.middleware.logging_middleware.logger') as mock_logger:
            response = client.get('/test')
            assert response.status_code == 200
            assert 'X-Request-ID' in response.headers
            assert 'X-Trace-ID' in response.headers
            assert mock_logger.info.called
    def test_request_context_creation(self):
        mock_request = Mock()
        mock_request.url.path = '/test'
        mock_request.method = 'GET'
        mock_request.query_params = {'param': 'value'}
        mock_request.headers = {'user-agent': 'test-agent', 'x-forwarded-for': '192.168.1.1'}
        mock_request.client.host = '127.0.0.1'
        mock_request.cookies = {}
        context = RequestContext(mock_request)
        assert context.endpoint == '/test'
        assert context.method == 'GET'
        assert context.client_ip == '192.168.1.1'
        assert context.user_agent == 'test-agent'
        assert 'req_' in context.request_id
        assert 'trace_' in context.trace_id
    def test_error_logging(self, app_with_logging):
        client = TestClient(app_with_logging)
        with patch('aphrodite.endpoints.middleware.logging_middleware.logger') as mock_logger:
            with pytest.raises(Exception):
                client.get('/error')
            assert any((call for call in mock_logger.error.call_args_list if 'HTTP request failed with error' in str(call)))
class TestPerformanceMiddleware:
    def test_performance_headers(self, app_with_performance):
        client = TestClient(app_with_performance)
        response = client.get('/test')
        assert response.status_code == 200
        assert 'X-Process-Time-Ms' in response.headers
        assert 'X-Memory-Usage-Mb' in response.headers
        assert 'X-Concurrent-Requests' in response.headers
        assert 'X-Cache-Hit' in response.headers
        process_time = float(response.headers['X-Process-Time-Ms'])
        assert 0 <= process_time <= 1000
    def test_slow_request_detection(self, app_with_performance):
        client = TestClient(app_with_performance)
        with patch('aphrodite.endpoints.middleware.performance_middleware.logger') as mock_logger:
            response = client.get('/slow')
            assert response.status_code == 200
            assert any((call for call in mock_logger.warning.call_args_list if 'Slow request detected' in str(call)))
class TestSecurityMiddleware:
    def test_normal_request_passes(self, app_with_security):
        client = TestClient(app_with_security)
        response = client.get('/test')
        assert response.status_code == 200
        assert 'X-Advanced-Security-Processed' in response.headers
        assert response.headers['X-Advanced-Security-Processed'] == 'true'
    def test_content_inspection_sql_injection(self, app_with_security):
        client = TestClient(app_with_security)
        response = client.get("/test?id=1' OR '1'='1")
        assert response.status_code == 403
        response_data = response.json()
        assert response_data['type'] == 'sql_injection'
class TestMiddlewareOrchestrator:
    def test_orchestrator_initialization(self):
        config = MiddlewareConfig()
        orchestrator = MiddlewareOrchestrator(config)
        assert orchestrator.config == config
        assert orchestrator.performance_profiler is not None
        assert orchestrator.start_time is not None
    def test_production_vs_development_config(self):
        prod_config = MiddlewareConfig.production()
        dev_config = MiddlewareConfig.development()
        assert not prod_config.log_request_body
        assert not prod_config.log_response_body
        assert prod_config.requests_per_minute < dev_config.requests_per_minute
        assert prod_config.enable_advanced_security
        assert dev_config.log_request_body
        assert dev_config.log_response_body
        assert not dev_config.enable_advanced_security
if __name__ == '__main__':
    pytest.main([__file__, '-v'])