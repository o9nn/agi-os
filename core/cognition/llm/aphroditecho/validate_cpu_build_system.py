import os
import sys
import subprocess
from pathlib import Path
def test_numa_headers():
    numa_path = Path('/usr/include/numa.h')
    if numa_path.exists():
        print('✅ NUMA headers available at', numa_path)
        return True
    else:
        print('❌ NUMA headers missing')
        return False
def test_c_extension():
    extension_path = Path('aphrodite/_C.abi3.so')
    if extension_path.exists():
        size_mb = extension_path.stat().st_size / (1024 * 1024)
        print(f'✅ C++ extension built successfully ({size_mb:.1f}MB)')
        return True
    else:
        print('❌ C++ extension missing')
        return False
def test_core_imports():
    try:
        os.environ['APHRODITE_TARGET_DEVICE'] = 'cpu'
        result = subprocess.run([sys.executable, '-c', "from aphrodite import LLM, SamplingParams; print('Core imports OK')"], capture_output=True, text=True)
        if result.returncode == 0:
            print('✅ Core Aphrodite imports working')
            return True
        else:
            print(f'❌ Core imports failed: {result.stderr}')
            return False
    except Exception as e:
        print(f'❌ Import test failed: {e}')
        return False
def test_build_system():
    try:
        os.environ['APHRODITE_TARGET_DEVICE'] = 'cpu'
        result = subprocess.run([sys.executable, 'test_build_system.py'], capture_output=True, text=True)
        if result.returncode == 0 and 'BUILD SYSTEM TEST PASSED' in result.stdout:
            print('✅ Build system validation passed')
            return True
        else:
            print(f'❌ Build system test failed: {result.stderr}')
            return False
    except Exception as e:
        print(f'❌ Build system test failed: {e}')
        return False
def main():
    print('🔧 Final CPU Build System Fix Validation')
    print('=' * 50)
    os.chdir('/home/runner/work/aphroditecho/aphroditecho')
    tests = [('NUMA Headers', test_numa_headers), ('C++ Extension', test_c_extension), ('Core Imports', test_core_imports), ('Build System', test_build_system)]
    results = []
    for name, test_func in tests:
        print(f'\n🔍 Testing {name}...')
        success = test_func()
        results.append((name, success))
    print('\n' + '=' * 50)
    print('📊 Final Validation Results:')
    print('-' * 30)
    all_passed = True
    for name, success in results:
        status = 'PASS' if success else 'FAIL'
        print(f"{('✅' if success else '❌')} {status} - {name}")
        if not success:
            all_passed = False
    print('-' * 30)
    print(f'Total: {len(results)} tests')
    print(f'Passed: {sum((1 for _, success in results if success))}')
    print(f'Failed: {sum((1 for _, success in results if not success))}')
    if all_passed:
        print('\n🎉 ALL VALIDATION TESTS PASSED!')
        print('The CPU build system failure has been completely resolved.')
        return 0
    else:
        print('\n❌ Some validation tests failed!')
        return 1
if __name__ == '__main__':
    exit(main())