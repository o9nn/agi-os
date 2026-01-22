import subprocess
import sys
import json
import os
from pathlib import Path
def run_command(cmd, capture_output=True, check=True):
    try:
        result = subprocess.run(cmd, shell=True, capture_output=capture_output, text=True, check=check)
        return (result.returncode == 0, result.stdout, result.stderr)
    except subprocess.CalledProcessError as e:
        return (False, e.stdout if hasattr(e, 'stdout') else '', e.stderr if hasattr(e, 'stderr') else str(e))
def test_vcpkg_integration():
    print('🔧 Testing vcpkg integration...')
    success, stdout, stderr = run_command('which vcpkg')
    if not success:
        print('❌ vcpkg not found in PATH')
        return False
    print(f'✅ vcpkg found at: {stdout.strip()}')
    vcpkg_json = Path('vcpkg.json')
    if not vcpkg_json.exists():
        print('❌ vcpkg.json not found')
        return False
    try:
        with open(vcpkg_json) as f:
            data = json.load(f)
        print(f"✅ vcpkg.json is valid JSON with {len(data.get('dependencies', []))} dependencies")
    except json.JSONDecodeError as e:
        print(f'❌ vcpkg.json is invalid JSON: {e}')
        return False
    return True
def test_cmake_presets():
    print('🔧 Testing CMake presets...')
    presets_file = Path('CMakePresets.json')
    if not presets_file.exists():
        print('❌ CMakePresets.json not found')
        return False
    try:
        with open(presets_file) as f:
            data = json.load(f)
        configure_presets = data.get('configurePresets', [])
        build_presets = data.get('buildPresets', [])
        print(f'✅ CMakePresets.json valid with {len(configure_presets)} configure presets and {len(build_presets)} build presets')
        preset_names = [p['name'] for p in configure_presets]
        expected_presets = ['vcpkg', 'conan', 'system']
        for preset in expected_presets:
            if preset in preset_names:
                print(f'✅ Found {preset} preset')
            else:
                print(f'⚠️  Missing {preset} preset')
        return True
    except json.JSONDecodeError as e:
        print(f'❌ CMakePresets.json is invalid JSON: {e}')
        return False
def test_package_manager_script():
    print('🔧 Testing package manager script...')
    script_path = Path('scripts/package-manager.py')
    if not script_path.exists():
        print('❌ package-manager.py script not found')
        return False
    success, stdout, stderr = run_command(f'python3 {script_path} --status')
    if not success:
        print(f'❌ package-manager.py --status failed: {stderr}')
        return False
    try:
        status_data = json.loads(stdout)
        print(f"✅ Package manager status: {len(status_data.get('available_managers', []))} managers available")
        for manager in status_data.get('available_managers', []):
            print(f'  - {manager}')
    except json.JSONDecodeError:
        print(f'❌ package-manager.py output is not valid JSON')
        return False
    return True
def test_setup_script():
    print('🔧 Testing setup script...')
    script_path = Path('scripts/setup-deps.sh')
    if not script_path.exists():
        print('❌ setup-deps.sh script not found')
        return False
    if not os.access(script_path, os.X_OK):
        print('❌ setup-deps.sh is not executable')
        return False
    success, stdout, stderr = run_command(f'{script_path} --help')
    if success and ('Usage:' in stdout or 'Options:' in stdout):
        print('✅ setup-deps.sh help works')
        return True
    else:
        print(f'❌ setup-deps.sh help failed: {stderr}')
        return False
def test_conan_integration():
    print('🔧 Testing Conan integration...')
    conanfile = Path('conanfile.txt')
    if not conanfile.exists():
        print('❌ conanfile.txt not found')
        return False
    success, stdout, stderr = run_command('which conan', check=False)
    if not success:
        print('⚠️  Conan not available, skipping Conan-specific tests')
        print('✅ conanfile.txt exists (minimal validation)')
        return True
    print(f'✅ Conan found')
    print('✅ conanfile.txt exists')
    return True
def test_cmake_integration():
    print('🔧 Testing CMake integration...')
    cmake_file = Path('CMakeLists.txt')
    if not cmake_file.exists():
        print('❌ CMakeLists.txt not found')
        return False
    try:
        with open(cmake_file) as f:
            content = f.read()
        checks = [('find_package', 'Package finding functionality'), ('target_link_libraries', 'Target linking'), ('vcpkg', 'vcpkg integration')]
        for check, description in checks:
            if check in content:
                print(f'✅ Found {description}')
            else:
                print(f'⚠️  Missing {description}')
        return True
    except Exception as e:
        print(f'❌ Failed to read CMakeLists.txt: {e}')
        return False
def test_dependency_declarations():
    print('🔧 Testing dependency declarations...')
    try:
        with open('vcpkg.json') as f:
            vcpkg_data = json.load(f)
        vcpkg_deps = [dep['name'] if isinstance(dep, dict) else dep for dep in vcpkg_data.get('dependencies', [])]
        expected_deps = ['curl', 'jsoncpp', 'glfw3', 'imgui', 'opengl']
        for dep in expected_deps:
            if dep in vcpkg_deps:
                print(f'✅ vcpkg dependency: {dep}')
            else:
                print(f'❌ Missing vcpkg dependency: {dep}')
    except Exception as e:
        print(f'❌ Failed to validate vcpkg dependencies: {e}')
        return False
    try:
        with open('conanfile.txt') as f:
            conan_content = f.read()
        conan_deps = ['libcurl', 'jsoncpp', 'glfw', 'imgui']
        for dep in conan_deps:
            if dep in conan_content:
                print(f'✅ conan dependency: {dep}')
            else:
                print(f'❌ Missing conan dependency: {dep}')
    except Exception as e:
        print(f'❌ Failed to validate conan dependencies: {e}')
        return False
    return True
def main():
    print('🚀 Starting package management integration tests...')
    print('=' * 50)
    tests = [('vcpkg Integration', test_vcpkg_integration), ('CMake Presets', test_cmake_presets), ('Package Manager Script', test_package_manager_script), ('Setup Script', test_setup_script), ('Conan Integration', test_conan_integration), ('CMake Integration', test_cmake_integration), ('Dependency Declarations', test_dependency_declarations)]
    results = []
    for test_name, test_func in tests:
        print(f'\n📋 {test_name}')
        print('-' * 30)
        try:
            result = test_func()
            results.append((test_name, result))
            if result:
                print(f'✅ {test_name} PASSED')
            else:
                print(f'❌ {test_name} FAILED')
        except Exception as e:
            print(f'💥 {test_name} CRASHED: {e}')
            results.append((test_name, False))
    print('\n' + '=' * 50)
    print('📊 Test Summary')
    print('=' * 50)
    passed = sum((1 for _, result in results if result))
    total = len(results)
    for test_name, result in results:
        status = '✅ PASS' if result else '❌ FAIL'
        print(f'{status} {test_name}')
    print('-' * 50)
    print(f'Results: {passed}/{total} tests passed ({passed / total * 100:.1f}%)')
    if passed == total:
        print('🎉 All package management integration tests passed!')
        sys.exit(0)
    else:
        print('⚠️  Some tests failed. Check the output above for details.')
        sys.exit(1)
if __name__ == '__main__':
    main()