#!/usr/bin/env python3
"""
Test content negotiation functionality for Deep Tree Echo endpoints.

Tests the enhanced multi-format response generation (JSON, HTML, XML)
and server-side content adaptation based on request headers.
"""

import pytest
from unittest.mock import Mock, MagicMock
from fastapi import Request
from fastapi.templating import Jinja2Templates

try:
    # Test imports to verify module structure
    from aphrodite.endpoints.deep_tree_echo.content_negotiation import (
        ContentNegotiator, AcceptedType, XMLResponseGenerator,
        MultiFormatResponse, wants_html, wants_xml, create_negotiated_response
    )
    IMPORTS_AVAILABLE = True
except ImportError as e:
    print(f"Import test failed: {e}")
    IMPORTS_AVAILABLE = False


class TestContentNegotiation:
    """Test the content negotiation system components."""
    
    def setup_method(self):
        """Set up test fixtures."""
        if not IMPORTS_AVAILABLE:
            pytest.skip("Content negotiation imports not available")
        
        self.negotiator = ContentNegotiator()
        self.xml_generator = XMLResponseGenerator()
        self.multi_format = MultiFormatResponse(self.negotiator)
    
    def create_mock_request(self, accept_header: str = "application/json") -> Request:
        """Create a mock FastAPI Request object."""
        request = Mock(spec=Request)
        request.headers = {"accept": accept_header} if accept_header else {}
        return request
    
    def test_accepted_type_parsing(self):
        """Test AcceptedType parsing from Accept header entries."""
        # Basic media type
        accepted = AcceptedType.from_accept_entry("application/json")
        assert accepted.media_type == "application/json"
        assert accepted.quality == 1.0
        assert accepted.params == {}
        
        # With quality factor
        accepted = AcceptedType.from_accept_entry("application/xml;q=0.8")
        assert accepted.media_type == "application/xml"
        assert accepted.quality == 0.8
        
        # With parameters
        accepted = AcceptedType.from_accept_entry("text/html;charset=utf-8;q=0.9")
        assert accepted.media_type == "text/html"
        assert accepted.quality == 0.9
        assert "charset" in accepted.params
    
    def test_accept_header_parsing(self):
        """Test parsing complete Accept headers."""
        # Simple case
        accept_header = "application/json"
        parsed = self.negotiator.parse_accept_header(accept_header)
        assert len(parsed) == 1
        assert parsed[0].media_type == "application/json"
        assert parsed[0].quality == 1.0
        
        # Multiple types with quality factors
        accept_header = "text/html,application/xml;q=0.9,application/json;q=0.8"
        parsed = self.negotiator.parse_accept_header(accept_header)
        assert len(parsed) == 3
        
        # Should be sorted by quality (descending)
        qualities = [p.quality for p in parsed]
        assert qualities == sorted(qualities, reverse=True)
    
    def test_content_type_negotiation(self):
        """Test content type negotiation logic."""
        # JSON preference
        request = self.create_mock_request("application/json")
        result = self.negotiator.negotiate_content_type(request)
        assert result == "json"
        
        # HTML preference
        request = self.create_mock_request("text/html")
        result = self.negotiator.negotiate_content_type(request)
        assert result == "html"
        
        # XML preference
        request = self.create_mock_request("application/xml")
        result = self.negotiator.negotiate_content_type(request)
        assert result == "xml"
        
        # Quality factor negotiation
        request = self.create_mock_request("application/xml;q=0.9,application/json;q=0.8")
        result = self.negotiator.negotiate_content_type(request)
        assert result == "xml"
        
        # Fallback to JSON for unsupported types
        request = self.create_mock_request("application/pdf")
        result = self.negotiator.negotiate_content_type(request)
        assert result == "json"
    
    def test_xml_generation(self):
        """Test XML generation from Python data structures."""
        # Simple data
        data = {"status": "success", "value": 42}
        xml = self.xml_generator.dict_to_xml(data, "test_root")
        
        assert "<test_root>" in xml
        assert "</test_root>" in xml
        assert "<status>success</status>" in xml
        assert "<value>42</value>" in xml
        
        # Nested data
        data = {
            "service": "API",
            "stats": {
                "users": 100,
                "active": True
            },
            "endpoints": ["/api/v1", "/api/v2"]
        }
        xml = self.xml_generator.dict_to_xml(data, "api_info")
        
        assert "<api_info>" in xml
        assert "<service>API</service>" in xml
        assert "<stats>" in xml
        assert "<users>100</users>" in xml
        assert "<active>True</active>" in xml
        assert "<endpoints>" in xml
        assert "<item_0>/api/v1</item_0>" in xml
    
    def test_backward_compatibility_functions(self):
        """Test backward compatibility wrapper functions."""
        # HTML request
        request = self.create_mock_request("text/html")
        assert wants_html(request) == True
        assert wants_xml(request) == False
        
        # XML request  
        request = self.create_mock_request("application/xml")
        assert wants_html(request) == False
        assert wants_xml(request) == True
        
        # JSON request
        request = self.create_mock_request("application/json") 
        assert wants_html(request) == False
        assert wants_xml(request) == False
    
    def test_multi_format_response_json(self):
        """Test MultiFormatResponse JSON generation."""
        data = {"test": "data"}
        request = self.create_mock_request("application/json")
        
        response = self.multi_format.create_response(data, request)
        
        # Should be JSONResponse
        assert hasattr(response, 'body')
        # Response content should be JSON
        content = response.body.decode()
        assert '"test": "data"' in content or '"test":"data"' in content
    
    def test_multi_format_response_xml(self):
        """Test MultiFormatResponse XML generation."""
        data = {"service": "test", "active": True}
        request = self.create_mock_request("application/xml")
        
        response = self.multi_format.create_response(
            data, request, xml_root="test_response"
        )
        
        # Should be XML Response
        assert response.media_type == "application/xml"
        content = response.body.decode()
        assert "<?xml version=" in content
        assert "<test_response>" in content
        assert "<service>test</service>" in content
        assert "<active>True</active>" in content
    
    def test_create_negotiated_response_function(self):
        """Test the main create_negotiated_response function."""
        data = {"api": "Deep Tree Echo", "version": "1.0"}
        
        # JSON request
        request = self.create_mock_request("application/json")
        response = create_negotiated_response(data, request)
        assert hasattr(response, 'body')
        
        # XML request
        request = self.create_mock_request("application/xml")
        response = create_negotiated_response(
            data, request, xml_root="api_response"
        )
        assert response.media_type == "application/xml"
        content = response.body.decode()
        assert "<api_response>" in content


if __name__ == "__main__":
    pytest.main([__file__, "-v"])