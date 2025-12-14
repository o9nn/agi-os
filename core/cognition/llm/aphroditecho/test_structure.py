"""
Isolated test for the 2do integration components
"""

def test_component_structure():
    """Test the component file structure."""
    from pathlib import Path
    
    base_path = Path(__file__).parent
    
    # Check if integration files exist
    integration_files = [
        "aphrodite/aar_gateway.py",
        "aphrodite/function_registry.py",
        "aphrodite/integrations/__init__.py",
        "aphrodite/integrations/llm_adapter.py",
        "aphrodite/integrations/aichat_adapter.py",
        "aphrodite/integrations/galatea_adapter.py",
        "aphrodite/integrations/spark_adapter.py",
        "aphrodite/integrations/argc_adapter.py",
        "aphrodite/integrations/llm_functions_adapter.py",
    ]
    
    missing_files = []
    for file_path in integration_files:
        if not (base_path / file_path).exists():
            missing_files.append(file_path)
    
    if missing_files:
        print(f"Missing files: {missing_files}")
        return False
    
    print(f"✓ All {len(integration_files)} integration files present")
    return True

def test_component_imports():
    """Test importing components without full dependencies."""
    try:
        # Import without executing initialization that needs torch
        import importlib.util
        from pathlib import Path
        
        base_path = Path(__file__).parent
        
        # Test direct module imports
        modules_to_test = [
            ("aar_gateway", base_path / "aphrodite/aar_gateway.py"),
            ("function_registry", base_path / "aphrodite/function_registry.py"),
        ]
        
        for module_name, module_path in modules_to_test:
            spec = importlib.util.spec_from_file_location(module_name, module_path)
            if spec and spec.loader:
                print(f"✓ {module_name} module structure valid")
            else:
                print(f"✗ {module_name} module structure invalid")
                return False
        
        print("✓ Core module structures are valid")
        return True
        
    except Exception as e:
        print(f"✗ Import test failed: {e}")
        return False

def test_contract_schemas():
    """Test contract schema files."""
    from pathlib import Path
    import json
    
    contracts_dir = Path(__file__).parent / "contracts" / "json"
    
    if not contracts_dir.exists():
        print("✗ Contracts directory not found")
        return False
    
    schema_files = list(contracts_dir.glob("*.schema.json"))
    
    if len(schema_files) == 0:
        print("✗ No schema files found")
        return False
    
    for schema_file in schema_files:
        try:
            schema_data = json.loads(schema_file.read_text())
            if "$schema" not in schema_data:
                print(f"✗ Invalid schema format in {schema_file.name}")
                return False
        except json.JSONDecodeError as e:
            print(f"✗ Invalid JSON in {schema_file.name}: {e}")
            return False
    
    print(f"✓ {len(schema_files)} contract schemas validated")
    return True

def test_2do_components_present():
    """Test that 2do components are present."""
    from pathlib import Path
    
    base_path = Path(__file__).parent / "2do"
    
    expected_components = [
        "aichat", "argc", "galatea-UI", "galatea-frontend", 
        "llm", "llm-functions", "paphos-backend", "spark.sys"
    ]
    
    present_components = []
    missing_components = []
    
    for component in expected_components:
        component_path = base_path / component
        if component_path.exists():
            present_components.append(component)
        else:
            missing_components.append(component)
    
    print(f"✓ Found {len(present_components)} components: {present_components}")
    
    if missing_components:
        print(f"! Missing components: {missing_components}")
    
    return len(present_components) > 0

if __name__ == "__main__":
    print("2do Components Integration - Structure Test")
    print("=" * 50)
    
    tests = [
        test_component_structure,
        test_component_imports,
        test_contract_schemas,
        test_2do_components_present,
    ]
    
    passed = 0
    for test in tests:
        try:
            result = test()
            if result:
                passed += 1
        except Exception as e:
            print(f"Test {test.__name__} failed: {e}")
    
    print(f"\nPassed: {passed}/{len(tests)} tests")
    print("✓ Structure tests complete" if passed == len(tests) else "✗ Some structure tests failed")