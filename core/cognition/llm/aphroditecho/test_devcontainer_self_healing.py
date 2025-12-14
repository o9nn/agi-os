#!/usr/bin/env python3
"""
Integration test for DevContainer and Self-Healing System
Tests all components to ensure they work together correctly
"""

import sys
import json
import subprocess
from pathlib import Path


def test_devcontainer_config():
    """Test DevContainer configuration"""
    print("🔍 Testing DevContainer configuration...")
    
    devcontainer_path = Path('.devcontainer/devcontainer.json')
    if not devcontainer_path.exists():
        print("❌ DevContainer configuration not found")
        return False
    
    try:
        with open(devcontainer_path, 'r') as f:
            config = json.load(f)
        
        required_keys = ['name', 'dockerFile', 'workspaceFolder', 'forwardPorts']
        for key in required_keys:
            if key not in config:
                print(f"❌ Missing required key: {key}")
                return False
        
        print("✅ DevContainer configuration valid")
        print(f"   - Name: {config['name']}")
        print(f"   - Ports: {len(config['forwardPorts'])} forwarded")
        print(f"   - Extensions: {len(config['customizations']['vscode']['extensions'])}")
        return True
        
    except Exception as e:
        print(f"❌ Error reading DevContainer config: {e}")
        return False


def test_self_healing_workflow():
    """Test self-healing workflow configuration"""
    print("\n🔍 Testing Self-Healing workflow...")
    
    workflow_path = Path('.github/workflows/self-healing-workflow.yml')
    if not workflow_path.exists():
        print("❌ Self-healing workflow not found")
        return False
    
    try:
        with open(workflow_path, 'r') as f:
            content = f.read()
        
        required_sections = [
            'name:', 'on:', 'jobs:', 'permissions:',
            'detect-errors', 'create-healing-issues'
        ]
        
        missing_sections = []
        for section in required_sections:
            if section not in content:
                missing_sections.append(section)
        
        if missing_sections:
            print(f"❌ Missing workflow sections: {missing_sections}")
            return False
        
        print("✅ Self-healing workflow valid")
        print(f"   - Size: {len(content)} bytes")
        print("   - Contains error detection and issue creation")
        return True
        
    except Exception as e:
        print(f"❌ Error reading workflow: {e}")
        return False


def test_self_healing_script():
    """Test self-healing Python script"""
    print("\n🔍 Testing Self-Healing script...")
    
    script_path = Path('.github/scripts/self-healing-system.py')
    if not script_path.exists():
        print("❌ Self-healing script not found")
        return False
    
    # Test script can be imported/executed
    try:
        result = subprocess.run([
            sys.executable, str(script_path), '--dry-run'
        ], capture_output=True, text=True, timeout=30)
        
        if result.returncode in [0, 1]:  # 0 = no errors, 1 = errors detected (expected)
            print("✅ Self-healing script runs successfully")
            if "🚨 Detected errors" in result.stdout:
                print("   - Detected errors in dry-run mode (expected)")
            else:
                print("   - No errors detected in current environment")
            return True
        else:
            print(f"❌ Script failed with return code: {result.returncode}")
            print(f"   - stderr: {result.stderr[:200]}...")
            return False
            
    except subprocess.TimeoutExpired:
        print("❌ Script timed out")
        return False
    except Exception as e:
        print(f"❌ Error running script: {e}")
        return False


def main():
    """Run integration tests"""
    print("🧪 Aphrodite Engine DevContainer & Self-Healing System Integration Test")
    print("=" * 80)
    
    tests = [
        ("DevContainer Config", test_devcontainer_config),
        ("Self-Healing Workflow", test_self_healing_workflow),
        ("Self-Healing Script", test_self_healing_script)
    ]
    
    results = []
    for test_name, test_func in tests:
        try:
            result = test_func()
            results.append((test_name, result))
        except Exception as e:
            print(f"❌ Test {test_name} failed with exception: {e}")
            results.append((test_name, False))
    
    print("\n" + "=" * 80)
    print("📊 Test Results Summary:")
    print("-" * 40)
    
    passed = 0
    failed = 0
    
    for test_name, result in results:
        status = "✅ PASS" if result else "❌ FAIL"
        print(f"{status} - {test_name}")
        if result:
            passed += 1
        else:
            failed += 1
    
    print("-" * 40)
    print(f"Total: {len(results)} tests")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    
    if failed == 0:
        print("\n🎉 All integration tests passed!")
        print("The DevContainer and Self-Healing System are ready for use.")
        return 0
    else:
        print(f"\n⚠️  {failed} test(s) failed. Please check the issues above.")
        return 1


if __name__ == '__main__':
    sys.exit(main())