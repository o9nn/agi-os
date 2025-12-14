"""
Tests for input validation middleware.
"""

import pytest
from unittest.mock import Mock
from fastapi import FastAPI, Request, HTTPException
from fastapi.testclient import TestClient

from aphrodite.endpoints.security.input_validation import (
    InputValidationMiddleware,
    ValidationConfig,
    validate_string_content,
    validate_json_structure,
    validate_file_upload,
    validate_request_input
)

@pytest.fixture
def app():
    """Create FastAPI app with input validation middleware."""
    app = FastAPI()
    config = ValidationConfig()
    app.add_middleware(InputValidationMiddleware, config=config)
    
    @app.get("/test")
    async def test_endpoint():
        return {"message": "test successful"}
    
    @app.post("/test")
    async def test_post_endpoint(request: Request):
        return {"message": "post successful", "validated": hasattr(request.state, 'input_validated')}
    
    return app

@pytest.fixture
def client(app):
    """Create test client."""
    return TestClient(app)

class TestStringValidation:
    """Test string content validation."""
    
    def test_valid_string(self):
        """Test validation of safe string content."""
        result = validate_string_content("Hello, world!", "test_field")
        assert result == "Hello, world!"
    
    def test_html_escaping(self):
        """Test HTML escaping in string validation."""
        result = validate_string_content("<script>alert('xss')</script>", "test_field")
        assert "&lt;script&gt;" in result
        assert "&lt;/script&gt;" in result
    
    def test_sql_injection_detection(self):
        """Test SQL injection pattern detection."""
        with pytest.raises(HTTPException) as exc_info:
            validate_string_content("'; DROP TABLE users; --", "test_field")
        
        assert exc_info.value.status_code == 400
        assert "sql injection" in exc_info.value.detail.lower()
    
    def test_xss_pattern_detection(self):
        """Test XSS pattern detection."""
        with pytest.raises(HTTPException) as exc_info:
            validate_string_content("<script>alert('xss')</script>", "test_field")
        
        assert exc_info.value.status_code == 400
        assert "xss" in exc_info.value.detail.lower()
    
    def test_path_traversal_detection(self):
        """Test path traversal pattern detection."""
        with pytest.raises(HTTPException) as exc_info:
            validate_string_content("../../etc/passwd", "test_field")
        
        assert exc_info.value.status_code == 400
        assert "path traversal" in exc_info.value.detail.lower()
    
    def test_command_injection_detection(self):
        """Test command injection pattern detection."""
        with pytest.raises(HTTPException) as exc_info:
            validate_string_content("test; cat /etc/passwd", "test_field")
        
        assert exc_info.value.status_code == 400
        assert "command injection" in exc_info.value.detail.lower()
    
    def test_size_limit_enforcement(self):
        """Test string size limit enforcement."""
        large_string = "x" * (1024 * 1024 + 1)  # > 1MB
        
        with pytest.raises(HTTPException) as exc_info:
            validate_string_content(large_string, "test_field")
        
        assert exc_info.value.status_code == 413
        assert "size limit" in exc_info.value.detail.lower()

class TestJSONValidation:
    """Test JSON structure validation."""
    
    def test_valid_json(self):
        """Test validation of safe JSON structure."""
        data = {"key": "value", "number": 123, "list": [1, 2, 3]}
        result = validate_json_structure(data)
        
        assert isinstance(result, dict)
        assert result["key"] == "value"
        assert result["number"] == 123
        assert result["list"] == [1, 2, 3]
    
    def test_json_depth_limit(self):
        """Test JSON depth limit enforcement."""
        # Create deeply nested JSON
        deep_json = {}
        current = deep_json
        for i in range(15):  # Exceed max depth
            current["level"] = {}
            current = current["level"]
        
        with pytest.raises(HTTPException) as exc_info:
            validate_json_structure(deep_json, max_depth=10)
        
        assert exc_info.value.status_code == 400
        assert "depth" in exc_info.value.detail.lower()
    
    def test_json_array_size_limit(self):
        """Test JSON array size limits."""
        large_array = list(range(10001))  # > 10000 items
        
        with pytest.raises(HTTPException) as exc_info:
            validate_json_structure(large_array)
        
        assert exc_info.value.status_code == 400
        assert "array size" in exc_info.value.detail.lower()
    
    def test_json_string_validation(self):
        """Test string validation within JSON structures."""
        data = {"safe": "hello", "dangerous": "<script>alert('xss')</script>"}
        
        with pytest.raises(HTTPException) as exc_info:
            validate_json_structure(data)
        
        assert exc_info.value.status_code == 400
        assert "xss" in exc_info.value.detail.lower()

class TestFileUploadValidation:
    """Test file upload validation."""
    
    def test_valid_file(self):
        """Test validation of safe file upload."""
        # Should not raise exception
        validate_file_upload("document.txt", "text/plain", 1024)
    
    def test_filename_too_long(self):
        """Test filename length limit."""
        long_filename = "x" * 300  # > 255 chars
        
        with pytest.raises(HTTPException) as exc_info:
            validate_file_upload(long_filename, "text/plain", 1024)
        
        assert exc_info.value.status_code == 400
        assert "filename too long" in exc_info.value.detail.lower()
    
    def test_path_traversal_in_filename(self):
        """Test path traversal detection in filename."""
        with pytest.raises(HTTPException) as exc_info:
            validate_file_upload("../../evil.txt", "text/plain", 1024)
        
        assert exc_info.value.status_code == 400
        assert "path traversal" in exc_info.value.detail.lower()
    
    def test_invalid_filename_characters(self):
        """Test invalid character detection in filename."""
        with pytest.raises(HTTPException) as exc_info:
            validate_file_upload("file<script>.txt", "text/plain", 1024)
        
        assert exc_info.value.status_code == 400
        assert "forbidden characters" in exc_info.value.detail.lower()
    
    def test_file_size_limit(self):
        """Test file size limit enforcement."""
        large_size = 11 * 1024 * 1024  # > 10MB
        
        with pytest.raises(HTTPException) as exc_info:
            validate_file_upload("file.txt", "text/plain", large_size)
        
        assert exc_info.value.status_code == 413
        assert "size exceeds" in exc_info.value.detail.lower()
    
    def test_dangerous_content_type(self):
        """Test dangerous content type detection."""
        with pytest.raises(HTTPException) as exc_info:
            validate_file_upload("script.exe", "application/x-executable", 1024)
        
        assert exc_info.value.status_code == 400
        assert "dangerous file type" in exc_info.value.detail.lower()

class TestInputValidationMiddleware:
    """Test input validation middleware integration."""
    
    def test_get_request_passes(self, client):
        """Test that safe GET requests pass validation."""
        response = client.get("/test")
        assert response.status_code == 200
        assert response.headers.get("X-Input-Validated") == "true"
    
    def test_post_request_validation(self, client):
        """Test POST request validation."""
        response = client.post(
            "/test",
            json={"message": "hello world"},
            headers={"Content-Type": "application/json"}
        )
        assert response.status_code == 200
        assert response.headers.get("X-Input-Validated") == "true"
    
    def test_malicious_json_blocked(self, client):
        """Test that malicious JSON is blocked."""
        response = client.post(
            "/test",
            json={"message": "<script>alert('xss')</script>"},
            headers={"Content-Type": "application/json"}
        )
        assert response.status_code == 400
        assert "xss" in response.text.lower()
    
    def test_sql_injection_blocked(self, client):
        """Test that SQL injection attempts are blocked."""
        response = client.post(
            "/test",
            json={"query": "'; DROP TABLE users; --"},
            headers={"Content-Type": "application/json"}
        )
        assert response.status_code == 400
        assert "sql injection" in response.text.lower()
    
    def test_unsupported_content_type_blocked(self, client):
        """Test that unsupported content types are blocked."""
        response = client.post(
            "/test",
            data="test data",
            headers={"Content-Type": "application/octet-stream"}
        )
        assert response.status_code == 415
        assert "unsupported content type" in response.text.lower()
    
    def test_oversized_request_blocked(self, client):
        """Test that oversized requests are blocked."""
        large_data = {"data": "x" * (2 * 1024 * 1024)}  # 2MB of data
        
        response = client.post(
            "/test",
            json=large_data,
            headers={"Content-Type": "application/json"}
        )
        # This might be blocked by the size check or JSON validation
        assert response.status_code in [400, 413]
    
    def test_health_check_bypass(self, client):
        """Test that health checks bypass validation."""
        app = FastAPI()
        app.add_middleware(InputValidationMiddleware)
        
        @app.get("/health")
        async def health():
            return {"status": "healthy"}
        
        client = TestClient(app)
        response = client.get("/health")
        assert response.status_code == 200
        # Health checks should not have validation headers
        assert "X-Input-Validated" not in response.headers

@pytest.mark.asyncio
class TestAsyncValidation:
    """Test async validation functions."""
    
    async def test_validate_request_input(self):
        """Test async request input validation."""
        # Mock request object
        request = Mock(spec=Request)
        request.url = Mock()
        request.url.path = "/test"
        request.headers = {"content-type": "application/json"}
        request.query_params = {"param": "value"}
        request.path_params = {"id": "123"}
        request.method = "GET"
        
        # Mock URL length
        str_url = "http://example.com/test"
        request.url.__str__ = Mock(return_value=str_url)
        
        result = await validate_request_input(request)
        
        assert result is not None
        assert "headers" in result
        assert "query_params" in result
        assert "path_params" in result
        assert result["query_params"]["param"] == "value"
        assert result["path_params"]["id"] == "123"