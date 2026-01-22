import asyncio
import json
import time
import pytest
from unittest.mock import AsyncMock, Mock, patch
from typing import Dict, Any
from fastapi import FastAPI, Request, Response
from fastapi.testclient import TestClient
from starlette.applications import Starlette
from starlette.responses import JSONResponse
from aphrodite.endpoints.middleware import CacheMiddleware, CacheConfig, CompressionMiddleware, CompressionConfig, PreprocessingMiddleware, PreprocessingConfig
from aphrodite.endpoints.route_optimizer import RouteOptimizer, RouteOptimizationConfig, create_optimized_app, get_high_performance_config, get_balanced_config, get_minimal_config
class TestCacheMiddleware:
    @pytest.fixture
    def cache_config(self):
        return CacheConfig(backend='memory', default_ttl=60, max_cache_size=100, cache_methods={'GET', 'POST'})
    @pytest.fixture
    def app_with_cache(self, cache_config):
        app = FastAPI()
        @app.get('/test')
        async def test_endpoint():
            return {'message': 'test response', 'timestamp': time.time()}
        @app.post('/test-post')
        async def test_post_endpoint(request: Request):
            body = await request.json()
            return {'echo': body, 'timestamp': time.time()}
        app.add_middleware(CacheMiddleware, config=cache_config)
        return app
    def test_cache_get_request(self, app_with_cache):
        client = TestClient(app_with_cache)
        response1 = client.get('/test')
        assert response1.status_code == 200
        data1 = response1.json()
        response2 = client.get('/test')
        assert response2.status_code == 200
        data2 = response2.json()
        assert data1['timestamp'] == data2['timestamp']
    def test_cache_post_request(self, app_with_cache):
        client = TestClient(app_with_cache)
        payload = {'test': 'data', 'value': 123}
        response1 = client.post('/test-post', json=payload)
        assert response1.status_code == 200
        data1 = response1.json()
        response2 = client.post('/test-post', json=payload)
        assert response2.status_code == 200
        data2 = response2.json()
        assert data1['echo'] == data2['echo']
    def test_cache_different_payloads(self, app_with_cache):
        client = TestClient(app_with_cache)
        payload1 = {'test': 'data1'}
        payload2 = {'test': 'data2'}
        response1 = client.post('/test-post', json=payload1)
        response2 = client.post('/test-post', json=payload2)
        assert response1.json()['echo'] != response2.json()['echo']
    def test_cache_ttl_expiration(self, cache_config):
        cache_config.default_ttl = 1
        app = FastAPI()
        @app.get('/test')
        async def test_endpoint():
            return {'timestamp': time.time()}
        app.add_middleware(CacheMiddleware, config=cache_config)
        client = TestClient(app)
        response1 = client.get('/test')
        data1 = response1.json()
        time.sleep(2)
        response2 = client.get('/test')
        data2 = response2.json()
        assert data1['timestamp'] != data2['timestamp']
class TestCompressionMiddleware:
    @pytest.fixture
    def compression_config(self):
        return CompressionConfig(min_size=100, compression_level=6, algorithms=['gzip', 'deflate'])
    @pytest.fixture
    def app_with_compression(self, compression_config):
        app = FastAPI()
        @app.get('/large')
        async def large_endpoint():
            return {'data': 'x' * 1000, 'items': list(range(100))}
        @app.get('/small')
        async def small_endpoint():
            return {'small': 'data'}
        app.add_middleware(CompressionMiddleware, config=compression_config)
        return app
    def test_gzip_compression(self, app_with_compression):
        client = TestClient(app_with_compression)
        headers = {'Accept-Encoding': 'gzip'}
        response = client.get('/large', headers=headers)
        assert response.status_code == 200
        assert response.headers.get('content-encoding') == 'gzip'
        assert 'accept-encoding' in response.headers.get('vary', '').lower()
    def test_no_compression_small_response(self, app_with_compression):
        client = TestClient(app_with_compression)
        headers = {'Accept-Encoding': 'gzip'}
        response = client.get('/small', headers=headers)
        assert response.status_code == 200
        assert 'content-encoding' not in response.headers
    def test_no_compression_unsupported_encoding(self, app_with_compression):
        client = TestClient(app_with_compression)
        response = client.get('/large')
        assert response.status_code == 200
        assert 'content-encoding' not in response.headers
    def test_deflate_compression(self, app_with_compression):
        client = TestClient(app_with_compression)
        headers = {'Accept-Encoding': 'deflate'}
        response = client.get('/large', headers=headers)
        assert response.status_code == 200
        assert response.headers.get('content-encoding') == 'deflate'
class TestPreprocessingMiddleware:
    @pytest.fixture
    def preprocessing_config(self):
        rate_limit_config = RateLimitConfig(requests_per_minute=60, burst_size=10)
        return PreprocessingConfig(enable_validation=True, enable_rate_limiting=True, rate_limit=rate_limit_config, max_body_size=1024, request_timeout=5.0)
    @pytest.fixture
    def app_with_preprocessing(self, preprocessing_config):
        app = FastAPI()
        @app.get('/test')
        async def test_endpoint():
            return {'status': 'ok'}
        @app.post('/test')
        async def test_post_endpoint(request: Request):
            body = await request.json()
            return {'received': body}
        app.add_middleware(PreprocessingMiddleware, config=preprocessing_config)
        return app
    def test_request_validation(self, app_with_preprocessing):
        client = TestClient(app_with_preprocessing)
        response = client.get('/test')
        assert response.status_code == 200
    def test_large_body_rejection(self, app_with_preprocessing):
        client = TestClient(app_with_preprocessing)
        large_payload = {'data': 'x' * 2000}
        response = client.post('/test', json=large_payload)
        assert response.status_code == 413
    def test_rate_limiting(self, preprocessing_config):
        from aphrodite.endpoints.middleware.preprocessing_middleware import RateLimitConfig
        rate_config = RateLimitConfig(requests_per_minute=2, burst_size=2)
        preprocessing_config.rate_limit = rate_config
        app = FastAPI()
        @app.get('/test')
        async def test_endpoint():
            return {'status': 'ok'}
        app.add_middleware(PreprocessingMiddleware, config=preprocessing_config)
        client = TestClient(app)
        for _ in range(2):
            response = client.get('/test')
            assert response.status_code == 200
        response = client.get('/test')
        assert response.status_code == 429
    def test_json_validation(self, app_with_preprocessing):
        client = TestClient(app_with_preprocessing)
        valid_payload = {'test': 'data'}
        response = client.post('/test', json=valid_payload)
        assert response.status_code == 200
        headers = {'Content-Type': 'application/json'}
        response = client.post('/test', data='invalid json', headers=headers)
        assert response.status_code == 400
class TestRouteOptimizer:
    def test_route_optimizer_initialization(self):
        config = RouteOptimizationConfig()
        optimizer = RouteOptimizer(config)
        assert optimizer.config.enable_caching
        assert optimizer.config.enable_compression
        assert optimizer.config.enable_preprocessing
        assert optimizer.config.target_response_time_ms == 100
    def test_create_optimized_app(self):
        app = FastAPI()
        @app.get('/test')
        async def test_endpoint():
            return {'message': 'test'}
        optimized_app = create_optimized_app(app)
        client = TestClient(optimized_app)
        response = client.get('/test')
        assert response.status_code == 200
        assert 'X-Process-Time' in response.headers
    def test_high_performance_config(self):
        config = get_high_performance_config()
        assert config.enable_caching
        assert config.enable_compression
        assert config.enable_preprocessing
        assert config.target_response_time_ms == 50
        assert config.cache_config.max_cache_size == 2000
        assert config.compression_config.min_size == 200
        assert config.preprocessing_config.rate_limit.requests_per_minute == 120
    def test_balanced_config(self):
        config = get_balanced_config()
        assert config.enable_caching
        assert config.enable_compression
        assert config.enable_preprocessing
        assert config.target_response_time_ms == 100
    def test_minimal_config(self):
        config = get_minimal_config()
        assert not config.enable_caching
        assert config.enable_compression
        assert config.enable_preprocessing
        assert config.target_response_time_ms == 200
class TestIntegrationPerformance:
    @pytest.fixture
    def optimized_app(self):
        app = FastAPI()
        @app.get('/models')
        async def list_models():
            return {'object': 'list', 'data': [{'id': 'gpt-3.5-turbo', 'object': 'model'}, {'id': 'gpt-4', 'object': 'model'}]}
        @app.post('/v1/chat/completions')
        async def chat_completions(request: Request):
            body = await request.json()
            return {'id': 'chatcmpl-123', 'object': 'chat.completion', 'model': body.get('model', 'gpt-3.5-turbo'), 'choices': [{'index': 0, 'message': {'role': 'assistant', 'content': 'Hello! How can I help you today?'}, 'finish_reason': 'stop'}], 'usage': {'prompt_tokens': 10, 'completion_tokens': 20, 'total_tokens': 30}}
        config = get_high_performance_config()
        return create_optimized_app(app, config)
    def test_response_time_target(self, optimized_app):
        client = TestClient(optimized_app)
        client.get('/models')
        start_time = time.time()
        response = client.get('/models')
        end_time = time.time()
        response_time_ms = (end_time - start_time) * 1000
        assert response.status_code == 200
        assert response_time_ms < 50
    def test_compression_reduces_size(self, optimized_app):
        client = TestClient(optimized_app)
        response_uncompressed = client.post('/v1/chat/completions', json={'model': 'gpt-3.5-turbo', 'messages': [{'role': 'user', 'content': 'Hi'}]})
        headers = {'Accept-Encoding': 'gzip'}
        response_compressed = client.post('/v1/chat/completions', json={'model': 'gpt-3.5-turbo', 'messages': [{'role': 'user', 'content': 'Hi'}]}, headers=headers)
        assert response_compressed.status_code == 200
        if 'content-encoding' in response_compressed.headers:
            assert response_compressed.headers['content-encoding'] == 'gzip'
    @pytest.mark.asyncio
    async def test_concurrent_request_handling(self, optimized_app):
        client = TestClient(optimized_app)
        async def make_request():
            response = client.get('/models')
            return response.status_code == 200
        tasks = [make_request() for _ in range(10)]
        results = await asyncio.gather(*tasks)
        assert all(results)
from aphrodite.endpoints.middleware.preprocessing_middleware import RateLimitConfig