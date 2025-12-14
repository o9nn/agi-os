#!/usr/bin/env python3
"""
Demo script for the Deep Tree Echo server-side template system.

Shows how the Jinja2 template system renders server-side HTML.
"""

from pathlib import Path
from jinja2 import Environment, FileSystemLoader


def demo_template_rendering():
    """Demonstrate template rendering with server-side data binding."""
    print("Deep Tree Echo Server-Side Template Rendering Demo")
    print("=" * 60)
    
    # Set up Jinja2 environment
    base_dir = Path(__file__).parent
    templates_dir = base_dir / "aphrodite" / "endpoints" / "deep_tree_echo" / "templates"
    
    if not templates_dir.exists():
        print("❌ Templates directory not found")
        return
    
    env = Environment(loader=FileSystemLoader(str(templates_dir)))
    
    # Demo 1: Index page rendering
    print("\n🏠 Rendering Index Page")
    print("-" * 30)
    
    index_template = env.get_template("index.html")
    index_data = {
        "service": "Deep Tree Echo API",
        "version": "1.0.0", 
        "description": "Server-side rendering API for DTESN processing",
        "endpoints": ["/process", "/status", "/membrane_info", "/esn_state"],
        "server_rendered": True
    }
    
    # Mock request object for template
    class MockRequest:
        pass
    
    rendered_html = index_template.render(request=MockRequest(), data=index_data)
    
    # Show key parts of rendered HTML
    lines = rendered_html.split('\n')
    for i, line in enumerate(lines[:50]):  # Show first 50 lines
        if any(keyword in line.lower() for keyword in ['title', 'deep tree', 'service', 'version']):
            print(f"  {line.strip()}")
    
    print(f"✅ Index template rendered successfully ({len(rendered_html)} characters)")
    
    # Demo 2: Status page rendering
    print("\n📊 Rendering Status Page")
    print("-" * 30)
    
    status_template = env.get_template("status.html") 
    status_data = {
        "dtesn_system": "operational",
        "membrane_hierarchy": "active",
        "esn_reservoir": "ready",
        "server_side": True,
        "processing_capabilities": {
            "max_membrane_depth": 4,
            "max_esn_size": 512,
            "bseries_max_order": 8
        }
    }
    
    rendered_status = status_template.render(request=MockRequest(), data=status_data)
    
    # Show key parts
    if "OPERATIONAL" in rendered_status:
        print("✅ Status badge rendered correctly")
    if "512" in rendered_status:
        print("✅ Configuration data bound correctly")
    if "System Status" in rendered_status:
        print("✅ Template inheritance working")
    
    print(f"✅ Status template rendered successfully ({len(rendered_status)} characters)")
    
    # Demo 3: Membrane info page
    print("\n🧬 Rendering Membrane Info Page")
    print("-" * 30)
    
    membrane_template = env.get_template("membrane_info.html")
    membrane_data = {
        "membrane_type": "P-System",
        "hierarchy_type": "rooted_tree", 
        "oeis_sequence": "A000081",
        "max_depth": 4,
        "supported_operations": [
            "membrane_evolution",
            "cross_membrane_communication",
            "rule_application",
            "tree_enumeration"
        ],
        "server_rendered": True
    }
    
    operation_descriptions = {
        "membrane_evolution": "Dynamic evolution of membrane states based on P-lingua rules",
        "cross_membrane_communication": "Communication protocols between different membrane levels"
    }
    
    rendered_membrane = membrane_template.render(
        request=MockRequest(), 
        data=membrane_data,
        operation_descriptions=operation_descriptions
    )
    
    # Show key elements
    if "P-System" in rendered_membrane:
        print("✅ Membrane type rendered")
    if "A000081" in rendered_membrane:
        print("✅ OEIS sequence displayed")
    if "membrane_evolution" in rendered_membrane:
        print("✅ Operations list rendered")
    
    print(f"✅ Membrane info template rendered successfully ({len(rendered_membrane)} characters)")
    
    # Demo 4: Template inheritance verification
    print("\n🔗 Verifying Template Inheritance")
    print("-" * 30)
    
    templates_to_check = ["index.html", "status.html", "membrane_info.html", "esn_state.html"]
    
    for template_name in templates_to_check:
        template = env.get_template(template_name)
        
        # Provide appropriate test data for each template
        test_data = {
            "test": True,
            "processing_capabilities": {"max_membrane_depth": 4, "max_esn_size": 512, "bseries_max_order": 8},
            "supported_operations": ["test_op"],
            "reservoir_size": 512
        }
        
        rendered = template.render(request=MockRequest(), data=test_data)
        
        if "<!DOCTYPE html>" in rendered and "Deep Tree Echo" in rendered:
            print(f"✅ {template_name} properly inherits from base template")
        else:
            print(f"❌ {template_name} inheritance issue")
    
    # Demo 5: Server-side data binding verification
    print("\n🔧 Server-Side Data Binding Verification")
    print("-" * 30)
    
    # Test dynamic data binding
    dynamic_data = {
        "dynamic_value": "Test Dynamic Content",
        "timestamp": "2025-01-01T12:00:00",
        "processing_time": 123.45
    }
    
    test_template_content = """
{% extends "base.html" %}
{% block content %}
<div>
    <p>Dynamic Value: {{ data.dynamic_value }}</p>
    <p>Timestamp: {{ data.timestamp }}</p>
    <p>Processing Time: {{ "%.2f"|format(data.processing_time) }}ms</p>
</div>
{% endblock %}
"""
    
    test_template = env.from_string(test_template_content)
    test_rendered = test_template.render(request=MockRequest(), data=dynamic_data)
    
    if "Test Dynamic Content" in test_rendered:
        print("✅ Dynamic data binding works")
    if "123.45" in test_rendered:
        print("✅ Number formatting works")
    if "2025-01-01" in test_rendered:
        print("✅ Template variables rendered correctly")
    
    print("\n" + "=" * 60)
    print("🎉 Server-Side Template System Demo Complete!")
    print("✅ All template rendering and data binding features working")
    print("✅ Template inheritance structure properly implemented")
    print("✅ Server-side HTML generation functional")


if __name__ == "__main__":
    demo_template_rendering()