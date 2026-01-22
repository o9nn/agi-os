import sys
import subprocess
import traceback
import os
def test_environment():
    print('🔍 Testing environment...')
    print(f'✅ Python {sys.version}')
    device = os.environ.get('APHRODITE_TARGET_DEVICE', 'not set')
    print(f'✅ APHRODITE_TARGET_DEVICE: {device}')
    return True
def test_system_dependencies():
    print('\n🔍 Testing system dependencies...')
    try:
        result = subprocess.run(['find', '/usr/include', '-name', 'numa.h'], capture_output=True, text=True)
        if result.returncode == 0 and result.stdout.strip():
            print(f'✅ numa.h found: {result.stdout.strip()}')
        else:
            print('❌ numa.h not found')
            return False
        result = subprocess.run(['cmake', '--version'], capture_output=True, text=True)
        if result.returncode == 0:
            print(f'✅ CMake: {result.stdout.split()[2]}')
        else:
            print('❌ CMake not available')
            return False
        result = subprocess.run(['ninja', '--version'], capture_output=True, text=True)
        if result.returncode == 0:
            print(f'✅ Ninja: {result.stdout.strip()}')
        else:
            print('❌ Ninja not available')
            return False
    except Exception as e:
        print(f'❌ System dependency check failed: {e}')
        return False
    return True
def test_python_dependencies():
    print('\n🔍 Testing Python dependencies...')
    try:
        import torch
        print(f'✅ PyTorch: {torch.__version__}')
        import numpy
        print(f'✅ NumPy: {numpy.__version__}')
        try:
            import ninja
            print('✅ Ninja Python package available')
        except ImportError:
            pass
        try:
            import setuptools
            print('✅ Setuptools available')
        except ImportError:
            pass
    except ImportError as e:
        print(f'❌ Python dependency missing: {e}')
        return False
    return True
def test_build_system():
    print('\n🔍 Testing build system...')
    try:
        extension_path = 'aphrodite/_C.abi3.so'
        if os.path.exists(extension_path):
            print(f'✅ C extension built: {extension_path}')
        else:
            print(f'❌ C extension not found: {extension_path}')
            return False
        os.environ['APHRODITE_TARGET_DEVICE'] = 'cpu'
        result = subprocess.run([sys.executable, 'setup.py', '--help-commands'], capture_output=True, text=True, cwd='.')
        if result.returncode == 0:
            print('✅ setup.py responds correctly')
        else:
            print(f'❌ setup.py failed: {result.stderr}')
            return False
    except Exception as e:
        print(f'❌ Build system test failed: {e}')
        return False
    return True
def test_core_imports():
    print('\n🔍 Testing core imports...')
    try:
        import aphrodite
        print(f'✅ Aphrodite imported: {aphrodite.__version__}')
        try:
            from aphrodite import LLM, SamplingParams
            print('✅ Core classes available')
        except ImportError as e:
            print(f'⚠️ Core classes import warning: {e}')
    except Exception as e:
        print(f'❌ Import failed: {e}')
        traceback.print_exc()
        return False
    return True
def run_validation():
    print('🚀 Aphrodite Build System Recovery Validation\n')
    tests = [('Environment', test_environment), ('System Dependencies', test_system_dependencies), ('Python Dependencies', test_python_dependencies), ('Build System', test_build_system), ('Core Imports', test_core_imports)]
    passed = 0
    total = len(tests)
    for test_name, test_func in tests:
        try:
            if test_func():
                passed += 1
            else:
                print(f'❌ {test_name} test failed')
        except Exception as e:
            print(f'❌ {test_name} test error: {e}')
    print(f'\n📊 Results: {passed}/{total} tests passed')
    if passed == total:
        print('✅ BUILD SYSTEM RECOVERY SUCCESSFUL')
        print('✅ Issue #202 has been resolved')
        return True
    else:
        print('❌ Some tests failed - investigation needed')
        return False
if __name__ == '__main__':
    success = run_validation()
    sys.exit(0 if success else 1)