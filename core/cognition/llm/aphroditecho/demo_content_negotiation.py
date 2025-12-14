#!/usr/bin/env python3
"""
Demonstration script for the enhanced content negotiation system.

Shows multi-format response generation (JSON, HTML, XML) with proper
HTTP Accept header parsing and quality factor support.
"""

import json
from typing import Dict, Any


def demo_content_negotiation():
    """Demonstrate the content negotiation functionality."""
    
    print("🔄 Deep Tree Echo Content Negotiation System Demo")
    print("=" * 60)
    
    # Import the content negotiation module
    try:
        import sys
        import os
        sys.path.insert(0, os.path.abspath('.'))
        
        # Direct import for demo
        exec(open('aphrodite/endpoints/deep_tree_echo/content_negotiation.py').read())
        
        # Create instances
        negotiator = ContentNegotiator()
        xml_generator = XMLResponseGenerator()
        
        print("✅ Content negotiation system loaded successfully!\n")
        
    except Exception as e:
        print(f"❌ Error loading content negotiation system: {e}")
        return
    
    # Demo data
    sample_data = {
        "service": "Deep Tree Echo API",
        "version": "1.0.0", 
        "status": "operational",
        "features": {
            "membrane_computing": True,
            "echo_state_networks": True,
            "batch_processing": True
        },
        "endpoints": ["/process", "/status", "/membrane_info"],
        "performance_metrics": {
            "response_time_ms": 45.2,
            "throughput_rps": 1250.8,
            "memory_usage_mb": 128.5
        }
    }
    
    # Test different Accept headers
    test_cases = [
        ("application/json", "Standard JSON request"),
        ("application/xml", "XML API consumption"),
        ("text/html", "Browser request"),
        ("application/xml;q=0.9,application/json;q=0.8", "Prefer XML over JSON"),
        ("text/html;q=1.0,application/xml;q=0.9,*/*;q=0.1", "Browser with fallbacks"),
        ("application/pdf,image/jpeg", "Unsupported types (fallback to JSON)"),
        ("*/*", "Accept anything (default to JSON)")
    ]
    
    for accept_header, description in test_cases:
        print(f"📨 Test Case: {description}")
        print(f"   Accept: {accept_header}")
        
        # Parse accept header
        accepted_types = negotiator.parse_accept_header(accept_header)
        print(f"   Parsed types: {[(t.media_type, t.quality) for t in accepted_types]}")
        
        # Negotiate content type
        # Create mock request object
        class MockRequest:
            def __init__(self, accept_header):
                self.headers = {"accept": accept_header}
        
        request = MockRequest(accept_header)
        chosen_type = negotiator.negotiate_content_type(request)
        print(f"   Negotiated type: {chosen_type}")
        
        # Generate sample response based on chosen type
        if chosen_type == "json":
            response_preview = json.dumps(sample_data, indent=2)[:200] + "..."
            print(f"   Response preview (JSON): {response_preview}")
        elif chosen_type == "xml":
            xml_content = xml_generator.dict_to_xml(sample_data, "api_response")
            response_preview = xml_content[:300] + "..."
            print(f"   Response preview (XML): {xml_content[:200]}...")
        elif chosen_type == "html":
            print(f"   Response: HTML template would be rendered with data")
        
        print()
    
    print("🎯 Advanced Features Demo")
    print("-" * 30)
    
    # Demo XML generation with complex data
    print("📄 XML Generation Example:")
    complex_data = {
        "dtesn_status": {
            "membrane_hierarchy": "active",
            "processing_stats": {
                "current_jobs": 15,
                "completed_today": 1247,
                "error_rate": 0.002
            },
            "supported_operations": ["evolution", "communication", "enumeration"],
            "server_info": {
                "uptime_hours": 168.5,
                "cpu_usage": 23.4,
                "memory_gb": 16.2
            }
        }
    }
    
    xml_output = xml_generator.dict_to_xml(complex_data, "dtesn_system_status")
    print(f"Generated XML ({len(xml_output)} chars):")
    print("<?xml version='1.0' encoding='UTF-8'?>")
    print(xml_output[:500] + ("..." if len(xml_output) > 500 else ""))
    
    print("\n✅ Content negotiation demo completed successfully!")
    print("🚀 The system now supports efficient multi-format responses:")
    print("   • JSON (default) - Fast API consumption")  
    print("   • HTML - Browser-friendly rendering")
    print("   • XML - Standards-compliant data exchange")
    print("   • Quality factor negotiation (q-values)")
    print("   • Backward compatibility with existing code")


if __name__ == "__main__":
    demo_content_negotiation()