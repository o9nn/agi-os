"""
Usage example for Deep Tree Echo FastAPI endpoints.

Demonstrates how to use the server-side DTESN processing endpoints
with the Aphrodite Engine FastAPI application.
"""

import json


class DTESNUsageExample:
    """Example usage of Deep Tree Echo endpoints."""
    
    def __init__(self):
        """Initialize the usage example."""
        self.base_url = "http://localhost:8000"
        self.endpoints = {
            "health": "/health",
            "root": "/deep_tree_echo/",
            "process": "/deep_tree_echo/process",
            "status": "/deep_tree_echo/status",
            "membrane_info": "/deep_tree_echo/membrane_info",
            "esn_state": "/deep_tree_echo/esn_state"
        }
    
    def show_endpoint_documentation(self):
        """Show documentation for available endpoints."""
        print("📚 Deep Tree Echo FastAPI Endpoints Documentation")
        print("=" * 60)
        print()
        
        endpoints_info = {
            "GET /health": {
                "description": "Health check for the Deep Tree Echo service",
                "response_example": {
                    "status": "healthy",
                    "service": "deep_tree_echo",
                    "version": "1.0.0"
                }
            },
            "GET /deep_tree_echo/": {
                "description": "Root endpoint with service information",
                "response_example": {
                    "service": "Deep Tree Echo API",
                    "version": "1.0.0",
                    "server_rendered": True,
                    "endpoints": ["process", "status", "membrane_info", "esn_state"]
                }
            },
            "POST /deep_tree_echo/process": {
                "description": "Process input through DTESN system",
                "request_example": {
                    "input_data": "Hello Deep Tree Echo",
                    "membrane_depth": 4,
                    "esn_size": 512,
                    "processing_mode": "server_side"
                },
                "response_example": {
                    "status": "success",
                    "result": {"processed_output": "dtesn_result"},
                    "membrane_layers": 4,
                    "processing_time_ms": 15.2,
                    "server_rendered": True
                }
            },
            "GET /deep_tree_echo/status": {
                "description": "Get DTESN system operational status",
                "response_example": {
                    "dtesn_system": "operational",
                    "server_side": True,
                    "processing_capabilities": {
                        "max_membrane_depth": 8,
                        "max_esn_size": 1024
                    }
                }
            },
            "GET /deep_tree_echo/membrane_info": {
                "description": "Information about P-System membrane hierarchy",
                "response_example": {
                    "membrane_type": "P-System",
                    "hierarchy_type": "rooted_tree",
                    "oeis_sequence": "A000081",
                    "server_rendered": True
                }
            },
            "GET /deep_tree_echo/esn_state": {
                "description": "Echo State Network reservoir information",
                "response_example": {
                    "reservoir_type": "echo_state_network",
                    "state": "ready",
                    "spectral_radius": 0.95,
                    "server_rendered": True
                }
            }
        }
        
        for endpoint, info in endpoints_info.items():
            print(f"🔗 **{endpoint}**")
            print(f"   Description: {info['description']}")
            
            if 'request_example' in info:
                print("   Request Example:")
                print("   ```json")
                print(f"   {json.dumps(info['request_example'], indent=4)}")
                print("   ```")
            
            print("   Response Example:")
            print("   ```json")
            print(f"   {json.dumps(info['response_example'], indent=4)}")
            print("   ```")
            print()
    
    def show_curl_examples(self):
        """Show curl command examples for testing endpoints."""
        print("💻 cURL Command Examples")
        print("=" * 30)
        print()
        
        curl_commands = [
            {
                "description": "Health check",
                "command": "curl -X GET http://localhost:8000/health"
            },
            {
                "description": "Get service information",
                "command": "curl -X GET http://localhost:8000/deep_tree_echo/"
            },
            {
                "description": "Process text through DTESN",
                "command": '''curl -X POST http://localhost:8000/deep_tree_echo/process \\
  -H "Content-Type: application/json" \\
  -d '{
    "input_data": "Hello Deep Tree Echo",
    "membrane_depth": 3,
    "esn_size": 256,
    "processing_mode": "server_side"
  }'
                '''.strip()
            },
            {
                "description": "Get system status",
                "command": "curl -X GET http://localhost:8000/deep_tree_echo/status"
            },
            {
                "description": "Get membrane information",
                "command": "curl -X GET http://localhost:8000/deep_tree_echo/membrane_info"
            },
            {
                "description": "Get ESN state",
                "command": "curl -X GET http://localhost:8000/deep_tree_echo/esn_state"
            }
        ]
        
        for i, cmd_info in enumerate(curl_commands, 1):
            print(f"{i}. **{cmd_info['description']}**")
            print("   ```bash")
            print(f"   {cmd_info['command']}")
            print("   ```")
            print()
    
    def show_python_client_example(self):
        """Show Python client usage example."""
        print("🐍 Python Client Example")
        print("=" * 30)
        print()
        
        python_code = '''
import asyncio
import aiohttp
import json

class DTESNClient:
    def __init__(self, base_url="http://localhost:8000"):
        self.base_url = base_url
    
    async def health_check(self):
        """Check service health."""
        async with aiohttp.ClientSession() as session:
            async with session.get(f"{self.base_url}/health") as response:
                return await response.json()
    
    async def get_status(self):
        """Get DTESN system status."""
        async with aiohttp.ClientSession() as session:
            async with session.get(f"{self.base_url}/deep_tree_echo/status") as response:
                return await response.json()
    
    async def process_text(self, text, membrane_depth=4, esn_size=512):
        """Process text through DTESN system."""
        data = {
            "input_data": text,
            "membrane_depth": membrane_depth,
            "esn_size": esn_size,
            "processing_mode": "server_side"
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.post(
                f"{self.base_url}/deep_tree_echo/process",
                json=data
            ) as response:
                return await response.json()
    
    async def get_membrane_info(self):
        """Get membrane hierarchy information."""
        async with aiohttp.ClientSession() as session:
            async with session.get(f"{self.base_url}/deep_tree_echo/membrane_info") as response:
                return await response.json()

# Usage example
async def main():
    client = DTESNClient()
    
    # Health check
    health = await client.health_check()
    print(f"Health: {health}")
    
    # Process some text
    result = await client.process_text("Deep Tree Echo test input")
    print(f"Processing result: {json.dumps(result, indent=2)}")
    
    # Get system status
    status = await client.get_status()
    print(f"Status: {json.dumps(status, indent=2)}")

if __name__ == "__main__":
    asyncio.run(main())
        '''.strip()
        
        print("```python")
        print(python_code)
        print("```")
        print()
    
    def show_fastapi_integration(self):
        """Show FastAPI integration example."""
        print("⚡ FastAPI Integration Example")
        print("=" * 40)
        print()
        
        integration_code = '''
from fastapi import FastAPI
from aphrodite.endpoints.deep_tree_echo import create_app
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
from aphrodite.engine.async_aphrodite import AsyncAphrodite

# Create Aphrodite engine (optional)
# engine = AsyncAphrodite.from_engine_args(engine_args)

# Create DTESN configuration
config = DTESNConfig(
    max_membrane_depth=6,
    esn_reservoir_size=1024,
    bseries_max_order=12,
    enable_caching=True,
    enable_performance_monitoring=True
)

# Create FastAPI app with Deep Tree Echo endpoints
app = create_app(engine=None, config=config)

# Run with uvicorn
if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
        '''.strip()
        
        print("```python")
        print(integration_code)
        print("```")
        print()
        
        print("🚀 **Running the server:**")
        print("```bash")
        print("python your_server.py")
        print("```")
        print()
        print("📖 **View API documentation:**")
        print("- Swagger UI: http://localhost:8000/docs")
        print("- ReDoc: http://localhost:8000/redoc")
        print()

def main():
    """Main function to show all usage examples."""
    print("🧠 Deep Tree Echo FastAPI Usage Guide")
    print("=" * 50)
    print()
    print("This guide shows how to use the Deep Tree Echo FastAPI endpoints")
    print("for server-side DTESN (Deep Tree Echo System Network) processing.")
    print()
    
    example = DTESNUsageExample()
    
    # Show documentation
    example.show_endpoint_documentation()
    print("─" * 60)
    
    # Show curl examples
    example.show_curl_examples()
    print("─" * 60)
    
    # Show Python client example
    example.show_python_client_example()
    print("─" * 60)
    
    # Show FastAPI integration
    example.show_fastapi_integration()
    
    print("✨ **Summary**")
    print()
    print("The Deep Tree Echo FastAPI endpoints provide:")
    print("• ✅ Server-side rendering with no client dependencies")
    print("• ✅ Integration with Aphrodite Engine architecture")
    print("• ✅ DTESN processing (P-Systems + ESN + B-Series)")
    print("• ✅ Performance monitoring and caching")
    print("• ✅ Comprehensive API documentation")
    print("• ✅ Production-ready configuration management")
    print()
    print("🎯 Ready for production deployment!")

if __name__ == "__main__":
    main()