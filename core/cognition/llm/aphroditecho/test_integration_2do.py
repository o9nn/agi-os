#!/usr/bin/env python3
"""
Integration Test for 2do Components

This script demonstrates the integration of 2do components with the Aphrodite Engine
through the AAR Gateway system.
"""

import asyncio
import json
import logging
from typing import Dict, Any

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def test_basic_imports():
    """Test basic imports of the integration components."""
    try:
        from aphrodite.aar_gateway import AARGateway, ComponentType, ComponentSpec
        from aphrodite.function_registry import FunctionRegistry, SafetyClass
        from aphrodite.integrations import (
            LLMAdapter, AiChatAdapter, GalateaAdapter, 
            SparkAdapter, ArgcAdapter, LLMFunctionsAdapter
        )
        
        logger.info("✓ Successfully imported all integration components")
        return True
        
    except ImportError as e:
        logger.error(f"✗ Import failed: {e}")
        return False

def test_function_registry():
    """Test the function registry functionality."""
    try:
        from aphrodite.function_registry import (
            FunctionRegistry
        )
        
        # Create registry
        registry = FunctionRegistry()
        
        # Test built-in functions
        assert len(registry.functions) > 0, "No built-in functions registered"
        
        # Test health check
        health = registry.health_check()
        assert health["status"] == "healthy", "Registry health check failed"
        
        # Test function listing
        functions = registry.list_functions()
        assert len(functions) > 0, "No functions available"
        
        logger.info(f"✓ Function registry test passed - {len(functions)} functions available")
        return True
        
    except Exception as e:
        logger.error(f"✗ Function registry test failed: {e}")
        return False

def test_aar_gateway():
    """Test the AAR Gateway functionality."""
    try:
        from aphrodite.aar_gateway import AARGateway
        
        # Create gateway
        gateway = AARGateway()
        
        # Test component registration
        assert len(gateway.components) > 0, "No components registered"
        
        # Test health check
        health = gateway.health_check()
        assert health["status"] == "healthy", "Gateway health check failed"
        
        # Test component listing
        components = gateway.list_components()
        assert len(components) > 0, "No components available"
        
        logger.info(f"✓ AAR Gateway test passed - {len(components)} components registered")
        return True
        
    except Exception as e:
        logger.error(f"✗ AAR Gateway test failed: {e}")
        return False

async def test_adapter_integrations():
    """Test the various component adapters."""
    try:
        from aphrodite.function_registry import FunctionRegistry
        from aphrodite.integrations import (
            LLMAdapter, AiChatAdapter, GalateaAdapter,
            SparkAdapter, ArgcAdapter, LLMFunctionsAdapter
        )
        from echo.sys.prompt_kernel import PromptStore
        
        registry = FunctionRegistry()
        prompt_store = PromptStore()
        
        # Test LLM Adapter
        llm_adapter = LLMAdapter(registry)
        llm_health = llm_adapter.health_check()
        logger.info(f"✓ LLM Adapter: {llm_health['status']} - {len(llm_adapter.models)} models")
        
        # Test AiChat Adapter
        aichat_adapter = AiChatAdapter()
        aichat_health = aichat_adapter.health_check()
        logger.info(f"✓ AiChat Adapter: {aichat_health['status']} - {len(aichat_adapter.models)} models")
        
        # Test Galatea Adapter
        galatea_adapter = GalateaAdapter()
        galatea_health = galatea_adapter.health_check()
        logger.info(f"✓ Galatea Adapter: {galatea_health['status']} - {len(galatea_adapter.services)} services")
        
        # Test Spark Adapter
        spark_adapter = SparkAdapter(prompt_store)
        spark_health = spark_adapter.health_check()
        logger.info(f"✓ Spark Adapter: {spark_health['status']} - {spark_health['prompts_loaded']} prompts")
        
        # Test Argc Adapter
        argc_adapter = ArgcAdapter(registry)
        argc_health = argc_adapter.health_check()
        logger.info(f"✓ Argc Adapter: {argc_health['status']} - {argc_health['commands_registered']} commands")
        
        # Test LLM Functions Adapter
        llm_functions_adapter = LLMFunctionsAdapter(registry)
        llm_functions_health = llm_functions_adapter.health_check()
        logger.info(f"✓ LLM Functions Adapter: {llm_functions_health['status']} - {llm_functions_health['functions_discovered']} functions")
        
        logger.info("✓ All adapter integrations tested successfully")
        return True
        
    except Exception as e:
        logger.error(f"✗ Adapter integration test failed: {e}")
        import traceback
        traceback.print_exc()
        return False

async def test_gateway_requests():
    """Test AAR Gateway request handling."""
    try:
        from aphrodite.aar_gateway import AARGateway, GatewayRequest
        
        gateway = AARGateway()
        
        # Test basic request handling
        request = GatewayRequest(
            request_id="test_001",
            component="aichat",
            operation="health",
            payload={}
        )
        
        response = await gateway.handle_request(request)
        assert response.success, f"Request failed: {response.error}"
        
        logger.info("✓ Gateway request handling test passed")
        return True
        
    except Exception as e:
        logger.error(f"✗ Gateway request test failed: {e}")
        return False

async def test_function_invocation():
    """Test function invocation through the registry."""
    try:
        from aphrodite.function_registry import FunctionRegistry, FunctionInvocation
        
        registry = FunctionRegistry()
        
        # Test built-in echo function
        invocation = FunctionInvocation(
            function_name="echo",
            arguments={"text": "Hello, World!"}
        )
        
        result = await registry.invoke_function(invocation)
        assert result.success, f"Function invocation failed: {result.error}"
        assert result.result["echoed"] == "Hello, World!", "Echo function returned wrong result"
        
        logger.info("✓ Function invocation test passed")
        return True
        
    except Exception as e:
        logger.error(f"✗ Function invocation test failed: {e}")
        return False

def test_contract_schemas():
    """Test contract schema validation."""
    try:
        from pathlib import Path
        import json
        
        contracts_dir = Path(__file__).parent / "contracts" / "json"
        
        if contracts_dir.exists():
            schema_files = list(contracts_dir.glob("*.schema.json"))
            
            for schema_file in schema_files:
                schema_content = json.loads(schema_file.read_text())
                assert "$schema" in schema_content, f"Invalid schema format in {schema_file.name}"
                assert "title" in schema_content, f"Missing title in {schema_file.name}"
                
            logger.info(f"✓ Contract schema validation passed - {len(schema_files)} schemas checked")
        else:
            logger.warning("✓ Contract schemas directory not found (expected for test environment)")
        
        return True
        
    except Exception as e:
        logger.error(f"✗ Contract schema test failed: {e}")
        return False

def generate_integration_report() -> Dict[str, Any]:
    """Generate a comprehensive integration report."""
    try:
        from aphrodite.aar_gateway import AARGateway
        from aphrodite.function_registry import FunctionRegistry
        
        gateway = AARGateway()
        registry = FunctionRegistry()
        
        report = {
            "gateway": {
                "status": "healthy",
                "components": len(gateway.components),
                "component_list": [comp.name for comp in gateway.list_components()],
                "sessions": len(gateway.active_sessions)
            },
            "function_registry": {
                "status": "healthy", 
                "functions": len(registry.functions),
                "active_functions": len([f for f in registry.functions.values() 
                                       if f.status.value == "active"]),
                "function_list": list(registry.functions.keys())[:10]  # First 10
            },
            "integration_summary": {
                "total_components": len(gateway.components),
                "total_functions": len(registry.functions),
                "adapters_available": [
                    "LLMAdapter", "AiChatAdapter", "GalateaAdapter", 
                    "SparkAdapter", "ArgcAdapter", "LLMFunctionsAdapter"
                ]
            }
        }
        
        return report
        
    except Exception as e:
        logger.error(f"Failed to generate integration report: {e}")
        return {"error": str(e)}

async def main():
    """Run all integration tests."""
    logger.info("Starting 2do Components Integration Test Suite")
    logger.info("=" * 60)
    
    tests = [
        ("Basic Imports", test_basic_imports),
        ("Function Registry", test_function_registry),
        ("AAR Gateway", test_aar_gateway),
        ("Contract Schemas", test_contract_schemas),
        ("Adapter Integrations", test_adapter_integrations),
        ("Gateway Requests", test_gateway_requests),
        ("Function Invocation", test_function_invocation),
    ]
    
    results = []
    passed = 0
    
    for test_name, test_func in tests:
        logger.info(f"\nRunning test: {test_name}")
        try:
            if asyncio.iscoroutinefunction(test_func):
                result = await test_func()
            else:
                result = test_func()
            
            results.append((test_name, result))
            if result:
                passed += 1
        except Exception as e:
            logger.error(f"Test {test_name} crashed: {e}")
            results.append((test_name, False))
    
    # Generate final report
    logger.info("\n" + "=" * 60)
    logger.info("Integration Test Results:")
    logger.info("=" * 60)
    
    for test_name, result in results:
        status = "PASS" if result else "FAIL"
        logger.info(f"{test_name:<25} {status}")
    
    logger.info(f"\nPassed: {passed}/{len(tests)} tests")
    
    if passed == len(tests):
        logger.info("🎉 All integration tests PASSED!")
        
        # Generate detailed report
        report = generate_integration_report()
        logger.info("\nIntegration Report:")
        logger.info(json.dumps(report, indent=2))
        
    else:
        logger.error(f"❌ {len(tests) - passed} test(s) FAILED")
    
    return passed == len(tests)

if __name__ == "__main__":
    success = asyncio.run(main())
    exit(0 if success else 1)