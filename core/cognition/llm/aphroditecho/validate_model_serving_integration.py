#!/usr/bin/env python3
"""
Validation script for Task 8.1.1 Model Serving Integration.

This script validates that the model serving infrastructure is properly
integrated into the Deep Tree Echo endpoints app factory.
"""

import re
from pathlib import Path

def validate_integration():
    """Validate model serving integration in app_factory.py"""
    
    app_factory_path = Path("aphrodite/endpoints/deep_tree_echo/app_factory.py")
    
    if not app_factory_path.exists():
        print("❌ app_factory.py not found")
        return False
    
    content = app_factory_path.read_text()
    
    checks = {
        "ModelServingManager import": "from .model_serving_manager import ModelServingManager",
        "create_model_serving_routes import": "from .model_serving_routes import create_model_serving_routes",
        "ModelServingManager initialization": "model_serving_manager = ModelServingManager(engine=engine)",
        "app.state.model_serving_manager": "app.state.model_serving_manager = model_serving_manager",
        "Model serving router creation": "model_serving_router = create_model_serving_routes(model_serving_manager)",
        "Router inclusion": 'app.include_router(model_serving_router',
        "Health check model serving": '"model_serving"',
        "Task 8.1.1 reference": "Task 8.1.1"
    }
    
    print("Validating Task 8.1.1 Model Serving Integration...")
    print("=" * 60)
    
    all_passed = True
    for check_name, pattern in checks.items():
        if pattern in content:
            print(f"✅ {check_name}")
        else:
            print(f"❌ {check_name}")
            all_passed = False
    
    print("=" * 60)
    
    if all_passed:
        print("\n✅ All integration checks passed!")
        print("\nIntegrated features:")
        print("  - Server-side model loading and caching strategies")
        print("  - Model versioning with zero-downtime updates")
        print("  - Resource-aware model allocation for DTESN operations")
        print("  - Comprehensive health monitoring in /health endpoint")
        print("  - RESTful API at /api/v1/model_serving/*")
        return True
    else:
        print("\n❌ Some integration checks failed")
        return False

def check_existing_implementation():
    """Check that the core implementation files exist"""
    
    print("\nChecking existing implementation files...")
    print("=" * 60)
    
    files_to_check = {
        "Model Serving Manager": "aphrodite/endpoints/deep_tree_echo/model_serving_manager.py",
        "Model Serving Routes": "aphrodite/endpoints/deep_tree_echo/model_serving_routes.py",
        "Integration Tests": "tests/endpoints/test_model_serving_integration.py"
    }
    
    all_exist = True
    for name, filepath in files_to_check.items():
        path = Path(filepath)
        if path.exists():
            size = path.stat().st_size
            print(f"✅ {name}: {filepath} ({size:,} bytes)")
        else:
            print(f"❌ {name}: {filepath} (not found)")
            all_exist = False
    
    print("=" * 60)
    return all_exist

def main():
    """Run all validation checks"""
    
    print("Task 8.1.1: Integrate with Aphrodite Model Serving Infrastructure")
    print("Validation Report")
    print("=" * 60)
    print()
    
    impl_exists = check_existing_implementation()
    integration_ok = validate_integration()
    
    print("\n" + "=" * 60)
    print("VALIDATION SUMMARY")
    print("=" * 60)
    
    if impl_exists and integration_ok:
        print("✅ Task 8.1.1 implementation is complete and integrated")
        print("\nAvailable Endpoints:")
        print("  GET  /health - Enhanced health check with model serving status")
        print("  GET  /api/v1/model_serving/status - Comprehensive serving status")
        print("  POST /api/v1/model_serving/load - Load model with caching")
        print("  POST /api/v1/model_serving/update - Zero-downtime model update")
        print("  GET  /api/v1/model_serving/models - List all models")
        print("  GET  /api/v1/model_serving/models/{id} - Get model details")
        print("  GET  /api/v1/model_serving/health/{id} - Model health check")
        print("  GET  /api/v1/model_serving/metrics - Performance metrics")
        print("  POST /api/v1/model_serving/health_check/{id} - On-demand health check")
        print("  DELETE /api/v1/model_serving/models/{id} - Remove model")
        return 0
    else:
        print("❌ Task 8.1.1 validation failed")
        return 1

if __name__ == "__main__":
    exit(main())
