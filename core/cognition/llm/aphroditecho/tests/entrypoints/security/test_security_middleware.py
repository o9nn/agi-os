"""
Tests for security middleware functionality.
"""

import pytest
import time
from unittest.mock import Mock
from fastapi import FastAPI, Request
from fastapi.testclient import TestClient

from aphrodite.endpoints.security.security_middleware import (
    SecurityMiddleware,
    RateLimitMiddleware,
    RequestSizeLimitMiddleware,
    SecurityConfig,
    RateLimiter,
    IPBlocklist,
    SecurityMonitor,
    get_client_identifier
)

@pytest.fixture
def app_with_security():
    """Create FastAPI app with security middleware."""
    app = FastAPI()
    
    # Add security middleware
    app.add_middleware(SecurityMiddleware)
    app.add_middleware(RateLimitMiddleware)
    
    @app.get("/test")
    async def test_endpoint():
        return {"message": "test successful"}
    
    @app.post("/test")
    async def test_post_endpoint():
        return {"message": "post successful"}
        
    return app

@pytest.fixture
def client_with_security(app_with_security):
    """Create test client with security middleware."""
    return TestClient(app_with_security)

class TestRateLimiter:
    """Test rate limiter functionality."""
    
    def test_rate_limiter_allows_normal_requests(self):
        """Test that normal request rates are allowed."""
        limiter = RateLimiter(requests_per_minute=60, burst_threshold=10)
        
        # Should allow several requests
        for i in range(5):
            assert limiter.is_allowed("client1") == True
    
    def test_rate_limiter_blocks_excessive_requests(self):
        """Test that excessive requests are blocked."""
        limiter = RateLimiter(requests_per_minute=2, burst_threshold=2)
        
        # First 2 should be allowed
        assert limiter.is_allowed("client1") == True
        assert limiter.is_allowed("client1") == True
        
        # Third should be blocked (burst threshold)
        assert limiter.is_allowed("client1") == False
    
    def test_rate_limiter_per_client_isolation(self):
        """Test that rate limiting is per-client."""
        limiter = RateLimiter(requests_per_minute=2, burst_threshold=2)
        
        # Client 1 uses up their quota
        assert limiter.is_allowed("client1") == True
        assert limiter.is_allowed("client1") == True
        assert limiter.is_allowed("client1") == False
        
        # Client 2 should still be allowed
        assert limiter.is_allowed("client2") == True
        assert limiter.is_allowed("client2") == True
    
    def test_rate_limiter_token_refill(self):
        """Test that tokens are refilled over time."""
        limiter = RateLimiter(requests_per_minute=60, burst_threshold=5)
        
        # Use up tokens
        for i in range(5):
            limiter.is_allowed("client1")
        
        # Should be blocked now
        assert limiter.is_allowed("client1") == False
        
        # Simulate time passing
        client = limiter.clients["client1"]
        client['last_update'] = time.time() - 60  # 1 minute ago
        
        # Should be allowed again
        assert limiter.is_allowed("client1") == True

class TestIPBlocklist:
    """Test IP blocking functionality."""
    
    def test_ip_not_blocked_initially(self):
        """Test that IPs are not blocked initially."""
        blocklist = IPBlocklist()
        assert blocklist.is_blocked("192.168.1.1") == False
    
    def test_manual_ip_blocking(self):
        """Test manual IP blocking."""
        blocklist = IPBlocklist()
        blocklist.block_ip("192.168.1.1", "test_reason")
        assert blocklist.is_blocked("192.168.1.1") == True
    
    def test_ip_block_expiration(self):
        """Test that IP blocks expire."""
        blocklist = IPBlocklist(block_duration_minutes=0.01)  # Very short duration
        blocklist.block_ip("192.168.1.1", "test_reason")
        
        assert blocklist.is_blocked("192.168.1.1") == True
        
        # Wait for expiration
        time.sleep(0.7)  # 0.01 minutes = 0.6 seconds
        assert blocklist.is_blocked("192.168.1.1") == False
    
    def test_failed_attempts_tracking(self):
        """Test that failed attempts are tracked and lead to blocking."""
        blocklist = IPBlocklist()
        
        # Record failed attempts
        for i in range(4):
            blocklist.record_failed_attempt("192.168.1.1")
            assert blocklist.is_blocked("192.168.1.1") == False
        
        # Fifth attempt should trigger blocking
        blocklist.record_failed_attempt("192.168.1.1")
        assert blocklist.is_blocked("192.168.1.1") == True
    
    def test_suspicious_pattern_tracking(self):
        """Test that suspicious patterns lead to blocking."""
        blocklist = IPBlocklist()
        
        # Record suspicious patterns
        for i in range(2):
            blocklist.record_suspicious_pattern("192.168.1.1")
            assert blocklist.is_blocked("192.168.1.1") == False
        
        # Third pattern should trigger blocking
        blocklist.record_suspicious_pattern("192.168.1.1")
        assert blocklist.is_blocked("192.168.1.1") == True

class TestSecurityMonitor:
    """Test security monitoring functionality."""
    
    def test_user_agent_analysis(self):
        """Test user agent analysis."""
        monitor = SecurityMonitor()
        
        # Mock request with suspicious user agent
        request = Mock(spec=Request)
        request.headers = {"user-agent": "curl/7.68.0"}
        request.url = Mock()
        request.url.path = "/test"
        
        analysis = monitor.analyze_request(request, "192.168.1.1")
        
        assert analysis['user_agent_suspicious'] == True
        assert analysis['anomaly_score'] > 0
        assert 'suspicious_user_agent' in analysis['suspicious_indicators']
    
    def test_path_analysis(self):
        """Test suspicious path analysis."""
        monitor = SecurityMonitor()
        
        # Mock request with suspicious path
        request = Mock(spec=Request)
        request.headers = {"user-agent": "Mozilla/5.0"}
        request.url = Mock()
        request.url.path = "/admin/config"
        
        analysis = monitor.analyze_request(request, "192.168.1.1")
        
        assert analysis['path_suspicious'] == True
        assert analysis['anomaly_score'] > 0
        assert 'suspicious_path' in analysis['suspicious_indicators']
    
    def test_endpoint_scanning_detection(self):
        """Test endpoint scanning detection."""
        monitor = SecurityMonitor()
        
        # Mock request
        request = Mock(spec=Request)
        request.headers = {"user-agent": "Mozilla/5.0"}
        request.url = Mock()
        
        # Simulate scanning many different endpoints
        for i in range(25):
            request.url.path = f"/endpoint{i}"
            analysis = monitor.analyze_request(request, "192.168.1.1")
        
        # Should detect endpoint scanning
        assert 'endpoint_scanning' in analysis['suspicious_indicators']
        assert analysis['anomaly_score'] > 0

class TestSecurityMiddleware:
    """Test complete security middleware integration."""
    
    def test_normal_request_passes(self, client_with_security):
        """Test that normal requests pass through security middleware."""
        response = client_with_security.get("/test")
        assert response.status_code == 200
        
        # Should have security headers
        assert "X-Security-Processed" in response.headers
        assert response.headers["X-Security-Processed"] == "true"
    
    def test_rate_limiting_blocks_excessive_requests(self, client_with_security):
        """Test that rate limiting blocks excessive requests."""
        # Make many requests quickly
        responses = []
        for i in range(150):  # Exceed rate limit
            response = client_with_security.get("/test")
            responses.append(response)
            if response.status_code == 429:
                break
        
        # Should eventually get rate limited
        assert any(r.status_code == 429 for r in responses)
        
        # Rate limited response should have proper headers
        rate_limited = next(r for r in responses if r.status_code == 429)
        assert "X-RateLimit-Limit" in rate_limited.headers
        assert "Retry-After" in rate_limited.headers
    
    def test_large_request_blocked(self, client_with_security):
        """Test that oversized requests are blocked."""
        large_headers = {"X-Large-Header": "x" * 10000}  # Very large header
        
        response = client_with_security.get("/test", headers=large_headers)
        assert response.status_code == 413
        assert "Headers too large" in response.text
    
    def test_client_identifier_generation(self):
        """Test client identifier generation."""
        # Mock request
        request = Mock(spec=Request)
        request.client = Mock()
        request.client.host = "192.168.1.1"
        request.headers = {"user-agent": "Mozilla/5.0"}
        
        identifier = get_client_identifier(request)
        
        assert isinstance(identifier, str)
        assert len(identifier) == 16  # Should be truncated hash
        
        # Same request should produce same identifier
        identifier2 = get_client_identifier(request)
        assert identifier == identifier2
        
        # Different IP should produce different identifier
        request.client.host = "192.168.1.2"
        identifier3 = get_client_identifier(request)
        assert identifier != identifier3

class TestRequestSizeLimitMiddleware:
    """Test request size limiting middleware."""
    
    def test_normal_request_passes(self):
        """Test that normal-sized requests pass."""
        app = FastAPI()
        app.add_middleware(RequestSizeLimitMiddleware, max_size=1024)
        
        @app.post("/test")
        async def test_endpoint():
            return {"message": "success"}
        
        client = TestClient(app)
        response = client.post("/test", json={"data": "small"})
        assert response.status_code == 200
    
    def test_large_request_blocked(self):
        """Test that large requests are blocked."""
        app = FastAPI()
        app.add_middleware(RequestSizeLimitMiddleware, max_size=100)  # Very small limit
        
        @app.post("/test")
        async def test_endpoint():
            return {"message": "success"}
        
        client = TestClient(app)
        
        # This should be blocked due to size
        large_data = {"data": "x" * 1000}
        response = client.post("/test", json=large_data)
        assert response.status_code == 413
        assert "Request too large" in response.text
        assert "X-Max-Size" in response.headers
        assert "X-Request-Size" in response.headers

@pytest.mark.asyncio
class TestAsyncSecurityFeatures:
    """Test async security functionality."""
    
    async def test_security_config_validation(self):
        """Test security configuration validation."""
        config = SecurityConfig(
            enable_rate_limiting=True,
            requests_per_minute=100,
            max_request_size=10 * 1024 * 1024
        )
        
        assert config.enable_rate_limiting == True
        assert config.requests_per_minute == 100
        assert config.max_request_size == 10 * 1024 * 1024
    
    async def test_security_stats_collection(self):
        """Test security statistics collection."""
        monitor = SecurityMonitor()
        
        # Generate some activity
        request = Mock(spec=Request)
        request.headers = {"user-agent": "test-agent"}
        request.url = Mock()
        request.url.path = "/test"
        
        for i in range(5):
            monitor.analyze_request(request, f"192.168.1.{i}")
        
        stats = monitor.get_security_stats()
        
        assert stats['unique_user_agents'] >= 1
        assert stats['monitored_ips'] >= 1
        assert 'top_user_agents' in stats