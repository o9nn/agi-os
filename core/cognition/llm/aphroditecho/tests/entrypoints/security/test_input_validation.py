import pytest
from unittest.mock import Mock
from fastapi import FastAPI, Request, HTTPException
from fastapi.testclient import TestClient
from aphrodite.endpoints.security.input_validation import InputValidationMiddleware, ValidationConfig, validate_string_content, validate_json_structure, validate_file_upload, validate_request_input
@pytest.fixture
def app():
    app = FastAPI()
    config = ValidationConfig()
    app.add_middleware(InputValidationMiddleware, config=config)
    @app.get('/test')
    async def test_endpoint():
        return {'message': 'test successful'}
    @app.post('/test')
    async def test_post_endpoint(request: Request):
        return {'message': 'post successful', 'validated': hasattr(request.state, 'input_validated')}
    return app
@pytest.fixture
def client(app):
    return TestClient(app)
class TestStringValidation:
    def test_valid_string(self):
        result = validate_string_content('Hello, world!', 'test_field')
        assert result == 'Hello, world!'
    def test_html_escaping(self):
        result = validate_string_content("<script>alert('xss')</script>", 'test_field')
        assert '&lt;script&gt;' in result
        assert '&lt;/script&gt;' in result
    def test_sql_injection_detection(self):
        with pytest.raises(HTTPException) as exc_info:
            validate_string_content("'; DROP TABLE users; --", 'test_field')
        assert exc_info.value.status_code == 400
        assert 'sql injection' in exc_info.value.detail.lower()
    def test_xss_pattern_detection(self):
        with pytest.raises(HTTPException) as exc_info:
            validate_string_content("<script>alert('xss')</script>", 'test_field')
        assert exc_info.value.status_code == 400
        assert 'xss' in exc_info.value.detail.lower()
    def test_path_traversal_detection(self):
        with pytest.raises(HTTPException) as exc_info:
            validate_string_content('../../etc/passwd', 'test_field')
        assert exc_info.value.status_code == 400
        assert 'path traversal' in exc_info.value.detail.lower()
    def test_command_injection_detection(self):
        with pytest.raises(HTTPException) as exc_info:
            validate_string_content('test; cat /etc/passwd', 'test_field')
        assert exc_info.value.status_code == 400
        assert 'command injection' in exc_info.value.detail.lower()
    def test_size_limit_enforcement(self):
        large_string = 'x' * (1024 * 1024 + 1)
        with pytest.raises(HTTPException) as exc_info:
            validate_string_content(large_string, 'test_field')
        assert exc_info.value.status_code == 413
        assert 'size limit' in exc_info.value.detail.lower()
class TestJSONValidation:
    def test_valid_json(self):
        data = {'key': 'value', 'number': 123, 'list': [1, 2, 3]}
        result = validate_json_structure(data)
        assert isinstance(result, dict)
        assert result['key'] == 'value'
        assert result['number'] == 123
        assert result['list'] == [1, 2, 3]
    def test_json_depth_limit(self):
        deep_json = {}
        current = deep_json
        for i in range(15):
            current['level'] = {}
            current = current['level']
        with pytest.raises(HTTPException) as exc_info:
            validate_json_structure(deep_json, max_depth=10)
        assert exc_info.value.status_code == 400
        assert 'depth' in exc_info.value.detail.lower()
    def test_json_array_size_limit(self):
        large_array = list(range(10001))
        with pytest.raises(HTTPException) as exc_info:
            validate_json_structure(large_array)
        assert exc_info.value.status_code == 400
        assert 'array size' in exc_info.value.detail.lower()
    def test_json_string_validation(self):
        data = {'safe': 'hello', 'dangerous': "<script>alert('xss')</script>"}
        with pytest.raises(HTTPException) as exc_info:
            validate_json_structure(data)
        assert exc_info.value.status_code == 400
        assert 'xss' in exc_info.value.detail.lower()
class TestFileUploadValidation:
    def test_valid_file(self):
        validate_file_upload('document.txt', 'text/plain', 1024)
    def test_filename_too_long(self):
        long_filename = 'x' * 300
        with pytest.raises(HTTPException) as exc_info:
            validate_file_upload(long_filename, 'text/plain', 1024)
        assert exc_info.value.status_code == 400
        assert 'filename too long' in exc_info.value.detail.lower()
    def test_path_traversal_in_filename(self):
        with pytest.raises(HTTPException) as exc_info:
            validate_file_upload('../../evil.txt', 'text/plain', 1024)
        assert exc_info.value.status_code == 400
        assert 'path traversal' in exc_info.value.detail.lower()
    def test_invalid_filename_characters(self):
        with pytest.raises(HTTPException) as exc_info:
            validate_file_upload('file<script>.txt', 'text/plain', 1024)
        assert exc_info.value.status_code == 400
        assert 'forbidden characters' in exc_info.value.detail.lower()
    def test_file_size_limit(self):
        large_size = 11 * 1024 * 1024
        with pytest.raises(HTTPException) as exc_info:
            validate_file_upload('file.txt', 'text/plain', large_size)
        assert exc_info.value.status_code == 413
        assert 'size exceeds' in exc_info.value.detail.lower()
    def test_dangerous_content_type(self):
        with pytest.raises(HTTPException) as exc_info:
            validate_file_upload('script.exe', 'application/x-executable', 1024)
        assert exc_info.value.status_code == 400
        assert 'dangerous file type' in exc_info.value.detail.lower()
class TestInputValidationMiddleware:
    def test_get_request_passes(self, client):
        response = client.get('/test')
        assert response.status_code == 200
        assert response.headers.get('X-Input-Validated') == 'true'
    def test_post_request_validation(self, client):
        response = client.post('/test', json={'message': 'hello world'}, headers={'Content-Type': 'application/json'})
        assert response.status_code == 200
        assert response.headers.get('X-Input-Validated') == 'true'
    def test_malicious_json_blocked(self, client):
        response = client.post('/test', json={'message': "<script>alert('xss')</script>"}, headers={'Content-Type': 'application/json'})
        assert response.status_code == 400
        assert 'xss' in response.text.lower()
    def test_sql_injection_blocked(self, client):
        response = client.post('/test', json={'query': "'; DROP TABLE users; --"}, headers={'Content-Type': 'application/json'})
        assert response.status_code == 400
        assert 'sql injection' in response.text.lower()
    def test_unsupported_content_type_blocked(self, client):
        response = client.post('/test', data='test data', headers={'Content-Type': 'application/octet-stream'})
        assert response.status_code == 415
        assert 'unsupported content type' in response.text.lower()
    def test_oversized_request_blocked(self, client):
        large_data = {'data': 'x' * (2 * 1024 * 1024)}
        response = client.post('/test', json=large_data, headers={'Content-Type': 'application/json'})
        assert response.status_code in [400, 413]
    def test_health_check_bypass(self, client):
        app = FastAPI()
        app.add_middleware(InputValidationMiddleware)
        @app.get('/health')
        async def health():
            return {'status': 'healthy'}
        client = TestClient(app)
        response = client.get('/health')
        assert response.status_code == 200
        assert 'X-Input-Validated' not in response.headers
@pytest.mark.asyncio
class TestAsyncValidation:
    async def test_validate_request_input(self):
        request = Mock(spec=Request)
        request.url = Mock()
        request.url.path = '/test'
        request.headers = {'content-type': 'application/json'}
        request.query_params = {'param': 'value'}
        request.path_params = {'id': '123'}
        request.method = 'GET'
        str_url = 'http://example.com/test'
        request.url.__str__ = Mock(return_value=str_url)
        result = await validate_request_input(request)
        assert result is not None
        assert 'headers' in result
        assert 'query_params' in result
        assert 'path_params' in result
        assert result['query_params']['param'] == 'value'
        assert result['path_params']['id'] == '123'